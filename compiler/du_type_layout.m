%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: du_type_layout.m.
% Main author: zs.
%
% The task of this module is decide the representation of each type.
% Once that is done, it will also invoke add_special_pred.m to declare
% and (if necessary) define the unify and compare predicates for each type.
%
% XXX TYPE_REPN
%
% At the moment, the algorithms we use to decide type representations
% follow the algorithms we used to use when type representations were decided
% piecemeal. We created an initial representation when we say the type
% definition, revised it when we saw foreign_type and/or foreign_enum
% pragmas for the type, and then possibly revised it again to pack terms
% into as little memory as possible.
%
% This structure was the best one could do when type representations were
% decided as part of make_hlds.m. Now that du_type_layout has its own pass
% *after* make_hlds, executed at a time when all the information we need
% to decide the representation of any given type is available up front,
% we can do better.
%
% We should restructure this algorithm code to group related activities
% together as much as possible. It should be possible to do just two passes:
%
% - pass 1 to decide the du_type_kind and the possible the sub-word-size nature
%   of each type, and
% - pass 2 to decide the representation of each type, including
%
%   - deciding foreign type overrides,
%   - allocating tags to functors,
%   - packing args, and
%   - deciding direct args.
%
% The two passes are needed because
%
% - e.g. decisions about the representation of a notag type require
%   knowing whether the type it is equivalent to is a dummy type or not, and
%
% - e.g. deciding whether (and if yes, how) the arguments of a function symbol
%   can be packed require knowing the maximum number of bits that an argument
%   type may occupy.
%
% Switching to this scheme will require moving all of make_tags.m
% and at least half of add_foreign_enum.m to this module. However,
% the simpler pass structure should allow us eliminate much code
% that does redundant work.
%
%---------------------------------------------------------------------------%
%
% Originally, I (zs) intended to run the du_type_layout pass
% after the whole semantic analysis phase of the compiler, i.e. after type,
% mode, and determinism checking and simplification. My reason for this,
% as discussed on m-dev in late october 2017, was to try to ensure that
% semantic analysis does not have access to, and thus cannot depend on,
% type representations, which are implementation-level details, and not
% a user-visible aspect of the program's semantics.
% 
% I found out the hard way that this won't work, at least not without
% a significant amount of otherwise-unnecessary code duplication.
% 
% The ultimate reason for this is that the code we want to generate
% for a type's unify and compare predicates depends on the type's
% representation. Consider a type such as
% 
%     :- type t
%         --->    f(bool, bool, bool bool).
% 
% One future packing optimization should allow us to represent values
% of this type as four bits in a register (no heap cell needed),
% If we can do that, then unifying two values X and Y of this type
% should be done by unsafe-casting both to int, and comparing the ints.
% If we cannot, then we should generate the usual code deconstructing
% both X and Y, and comparing the arguments (one by one or all together,
% depending on well we can pack the heap cell's contents).
% 
% Deciding type representations after semantic analysis would therefore
% require us to generate the unify and compare (and maybe index) predicates
% of each type late as well, but then (since the clauses we generate are NOT
% guaranteed to include all the right types, modes and other annotations)
% we would have to repeat semantic analysis just on these late-auto-generated
% predicates. This would not be too hard with some semantic analysis passes
% (e.g. determinism analysis) that are already effectively done
% predicate-by-predicate, but *would* require large amounts of new work
% for the polymorphism pass, which currently works on the whole module at once.
% 
% To minimize this problem, we execute the du_type_layout pass directly after
% the make_hlds pass, i.e. before all the semantic analysis passes.
% 
%---------------------------------------------------------------------------%

:- module hlds.du_type_layout.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

:- pred decide_type_repns(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred update_repn_of_ctor(type_ctor::in, cons_id_to_tag_map::in,
    constructor_repn::in, constructor_repn::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.add_foreign_enum.
:- import_module hlds.add_special_pred.
:- import_module hlds.make_tags.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module require.

%---------------------------------------------------------------------------%

decide_type_repns(!ModuleInfo, !Specs) :-
    module_info_get_type_repn_dec(!.ModuleInfo, TypeRepnDec),
    TypeRepnDec = type_repn_decision_data(TypeRepns, DirectArgMap,
        ForeignEnums, ForeignExportEnums),

    % XXX TYPE_REPN The compiler does not yet generate type_repn items,
    % so for now, TypeRepns will be the empty list, which makes _TypeRepnMap
    % not yet useful.
    build_type_repn_map(TypeRepns, map.init, _TypeRepnMap),

    list.foldl2(add_pragma_foreign_enum(!.ModuleInfo), ForeignEnums,
        map.init, TypeCtorToForeignEnumMap, !Specs),

    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    foldl_over_type_ctor_defns(
        decide_unpacked_du_type_layout(!.ModuleInfo,
            DirectArgMap, TypeCtorToForeignEnumMap),
        TypeTable0, TypeTable0, TypeTable1),
    module_info_set_type_table(TypeTable1, !ModuleInfo),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_int_option(Globals, arg_pack_bits, ArgPackBits),
    module_info_get_no_tag_types(!.ModuleInfo, NoTagTypesMap0),
    foldl2_over_type_ctor_defns(
        decide_packed_du_type_layout(!.ModuleInfo, ArgPackBits),
        TypeTable1, TypeTable1, TypeTable2, NoTagTypesMap0, NoTagTypesMap),
    module_info_set_no_tag_types(NoTagTypesMap, !ModuleInfo),
    module_info_set_type_table(TypeTable2, !ModuleInfo),

    post_process_types_direct_args(!ModuleInfo, PostTypeSpecs),
    !:Specs = PostTypeSpecs ++ !.Specs,

    list.foldl2(add_pragma_foreign_export_enum, ForeignExportEnums,
        !ModuleInfo, !Specs),

    module_info_get_type_table(!.ModuleInfo, TypeTable),
    foldl_over_type_ctor_defns(
        add_special_pred_decl_defns_for_type_maybe_lazily,
        TypeTable, !ModuleInfo).

%---------------------------------------------------------------------------%

:- type type_repn_map == multi_map(type_ctor, item_type_repn_info).

:- pred build_type_repn_map(list(item_type_repn_info)::in,
    type_repn_map::in, type_repn_map::out) is det.

build_type_repn_map([], !TypeRepnMap).
build_type_repn_map([TypeRepn | TypeRepns], !TypeRepnMap) :-
    TypeRepn = item_type_repn_info(TypeCtorSymName, ArgTVars, _, _, _, _),
    list.length(ArgTVars, Arity),
    TypeCtor = type_ctor(TypeCtorSymName, Arity),
    multi_map.add(TypeCtor, TypeRepn, !TypeRepnMap),
    build_type_repn_map(TypeRepns, !TypeRepnMap).

%---------------------------------------------------------------------------%

:- pred decide_unpacked_du_type_layout(module_info::in,
    map(type_ctor, list(sym_name_and_arity))::in,
    type_ctor_to_foreign_enums_map::in,
    type_ctor::in, hlds_type_defn::in, type_table::in, type_table::out) is det.

decide_unpacked_du_type_layout(ModuleInfo, DirectArgMap,
        TypeCtorToForeignEnumMap, TypeCtor, TypeDefn, !TypeTable) :-
    get_type_defn_body(TypeDefn, Body0),
    (
        Body0 = hlds_du_type(Ctors, MaybeUserEqComp, MaybeRepn0,
            MaybeForeign),
        expect(unify(MaybeRepn0, no), $pred, "MaybeRepn0 != no"),
        module_info_get_globals(ModuleInfo, Globals),
        assign_constructor_tags(Globals, TypeCtor, MaybeUserEqComp,
            Ctors, ConsTagMap0, ReservedAddr, DuKind0),
        ( if
            map.search(TypeCtorToForeignEnumMap, TypeCtor, TCFE),
            TCFE = type_ctor_foreign_enums(_LangContextMap,
                MaybeForeignEnumTagMap),
            MaybeForeignEnumTagMap = yes({ForeignEnumTagMap, Lang})
        then
            ConsTagMap = ForeignEnumTagMap,
            DuKind = du_type_kind_foreign_enum(Lang)
        else
            ConsTagMap = ConsTagMap0,
            DuKind = DuKind0
        ),
        list.map_foldl(add_repn_to_ctor(TypeCtor, ConsTagMap),
            Ctors, CtorRepns, map.init, CtorRepnMap),
        (
            ReservedAddr = does_not_use_reserved_address,
            compute_cheaper_tag_test(ConsTagMap, MaybeCheaperTagTest)
        ;
            ReservedAddr = uses_reserved_address,
            MaybeCheaperTagTest = no_cheaper_tag_test
        ),
        % The type_ctors in the keys of DirectArgMap should always be
        % fully module qualified, just like TypeCtor.
        ( if map.search(DirectArgMap, TypeCtor, DirectArgFunctors) then
            MaybeDirectArgFunctors = yes(DirectArgFunctors)
        else
            MaybeDirectArgFunctors = no
        ),
        Repn = du_type_repn(ConsTagMap, CtorRepns, CtorRepnMap,
            MaybeCheaperTagTest, DuKind, MaybeDirectArgFunctors, ReservedAddr),
        Body = hlds_du_type(Ctors, MaybeUserEqComp, yes(Repn), MaybeForeign),
        set_type_defn_body(Body, TypeDefn, UnpackedTypeDefn),
        replace_type_ctor_defn(TypeCtor, UnpackedTypeDefn, !TypeTable)
    ;
        ( Body0 = hlds_eqv_type(_)
        ; Body0 = hlds_foreign_type(_)
        ; Body0 = hlds_solver_type(_)
        ; Body0 = hlds_abstract_type(_)
        )
        % Leave these types alone.
    ).

update_repn_of_ctor(TypeCtor, ConsTagMap, CtorRepn0, CtorRepn, !CtorRepnMap) :-
    % update_repn_of_ctor and add_repn_to_ctor do very similar jobs.
    CtorRepn0 = ctor_repn(_MaybeExistConstraints, SymName, _ConsTag0,
        _ArgRepns, Arity, _Context),
    ConsId = cons(SymName, Arity, TypeCtor),
    map.lookup(ConsTagMap, ConsId, ConsTag),
    CtorRepn = CtorRepn0 ^ cr_tag := ConsTag,
    insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap).

:- pred add_repn_to_ctor(type_ctor::in, cons_id_to_tag_map::in,
    constructor::in, constructor_repn::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

add_repn_to_ctor(TypeCtor, ConsTagMap, Ctor, CtorRepn, !CtorRepnMap) :-
    % update_repn_of_ctor and add_repn_to_ctor do very similar jobs.
    Ctor = ctor(MaybeExistConstraints, SymName, Args, Arity, Context),
    ConsId = cons(SymName, Arity, TypeCtor),
    map.lookup(ConsTagMap, ConsId, ConsTag),
    ArgRepns = list.map(add_default_repn_to_ctor_arg, Args),
    CtorRepn = ctor_repn(MaybeExistConstraints, SymName, ConsTag, ArgRepns,
        Arity, Context),
    insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap).

:- pred insert_ctor_repn_into_map(constructor_repn::in,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap) :-
    SymName = CtorRepn ^ cr_name,
    Name = unqualify_name(SymName),
    ( if map.search(!.CtorRepnMap, Name, OldCtorRepns) then
        OldCtorRepns = one_or_more(FirstOldCtorRepn, LaterOldCtorRepns),
        CtorRepns = one_or_more(CtorRepn,
            [FirstOldCtorRepn | LaterOldCtorRepns]),
        map.det_update(Name, CtorRepns, !CtorRepnMap)
    else
        map.det_insert(Name, one_or_more(CtorRepn, []), !CtorRepnMap)
    ).

:- func add_default_repn_to_ctor_arg(constructor_arg) = constructor_arg_repn.

add_default_repn_to_ctor_arg(ConsArg) = ConsArgRepn :-
    ConsArg = ctor_arg(MaybeFieldName, Type, Context),
    ConsArgRepn = ctor_arg_repn(MaybeFieldName, Type, full_word, Context).

%---------------------------------------------------------------------------%

:- pred decide_packed_du_type_layout(module_info::in, int::in,
    type_ctor::in, hlds_type_defn::in, type_table::in, type_table::out,
    no_tag_type_table::in, no_tag_type_table::out) is det.

decide_packed_du_type_layout(ModuleInfo, ArgPackBits, TypeCtor, TypeDefn,
        !TypeTable, !NoTagTypesMap) :-
    get_type_defn_body(TypeDefn, Body0),
    (
        Body0 = hlds_du_type(Ctors, MaybeUserEqComp, MaybeRepn0, MaybeForeign),
        (
            MaybeRepn0 = no,
            unexpected($pred, "MaybeRepn0 = no")
        ;
            MaybeRepn0 = yes(Repn0)
        ),
        DuKind = Repn0 ^ dur_kind,
        CtorRepns0 = Repn0 ^ dur_ctor_repns,
        list.map_foldl(layout_du_ctor_args(ModuleInfo, DuKind, ArgPackBits),
            CtorRepns0, CtorRepns, map.init, CtorRepnMap),
        Repn1 = Repn0 ^ dur_ctor_repns := CtorRepns,
        Repn = Repn1 ^ dur_ctor_map := CtorRepnMap,
        Body = hlds_du_type(Ctors, MaybeUserEqComp, yes(Repn), MaybeForeign),
        set_type_defn_body(Body, TypeDefn, PackedTypeDefn),
        replace_type_ctor_defn(TypeCtor, PackedTypeDefn, !TypeTable),

        module_info_get_globals(ModuleInfo, Globals),
        ( if
            type_ctor_should_be_notag(Globals, TypeCtor, Ctors,
                MaybeUserEqComp,
                NoTagCtorName, NoTagCtorArgType, _NoTagCtorFieldName)
        then
            get_type_defn_tparams(TypeDefn, TypeParams),
            NoTagType =
                no_tag_type(TypeParams, NoTagCtorName, NoTagCtorArgType),
            % XXX TYPE_REPN Should this be det_insert?
            map.set(TypeCtor, NoTagType, !NoTagTypesMap)
        else
            true
        )
    ;
        ( Body0 = hlds_eqv_type(_)
        ; Body0 = hlds_foreign_type(_)
        ; Body0 = hlds_solver_type(_)
        ; Body0 = hlds_abstract_type(_)
        )
        % Leave these types alone.
    ).

:- pred layout_du_ctor_args(module_info::in, du_type_kind::in, int::in,
    constructor_repn::in, constructor_repn::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

layout_du_ctor_args(ModuleInfo, DuKind, ArgPackBits, CtorRepn0, CtorRepn,
        !CtorRepnMap) :-
    % ArgRepns1 is ArgRepns0 with any float arg marked as needing a double word
    % if the representation of floats is a double word.
    %
    % ArgRepns2 is ArgRepns1 with consecutive sub-word-sized arguments packed
    % into single words as much as possible.
    % XXX TYPE_REPN For now, we only recognize enums as sub-word-sized.
    % We should extend this to also cover whichever of int8, int16, int32
    % (and their unsigned versions) are smaller than a word.
    ArgRepns0 = CtorRepn0 ^ cr_args,
    module_info_get_globals(ModuleInfo, Globals),
    (
        ( DuKind = du_type_kind_mercury_enum
        ; DuKind = du_type_kind_foreign_enum(_)
        ; DuKind = du_type_kind_direct_dummy
        ; DuKind = du_type_kind_notag(_, _, _)
        ),
        ArgRepns1 = ArgRepns0
    ;
        DuKind = du_type_kind_general,
        % A functor with a single float argument can have a double-width word
        % if it is not a no-tag functor. An example is `poly_type.f(float)'.
        use_double_word_floats(Globals, UseDoubleWordFloats),
        (
            UseDoubleWordFloats = yes,
            set_double_word_floats(ModuleInfo, ArgRepns0, ArgRepns1)
        ;
            UseDoubleWordFloats = no,
            ArgRepns1 = ArgRepns0
        )
    ),
    ( if ArgPackBits > 0 then
        pack_du_ctor_args(ModuleInfo, ArgPackBits, 0, ArgRepns1, ArgRepns2, _),
        WorthPacking = worth_arg_packing(ArgRepns1, ArgRepns2),
        (
            WorthPacking = yes,
            ArgRepns = ArgRepns2
        ;
            WorthPacking = no,
            ArgRepns = ArgRepns1
        )
    else
        ArgRepns = ArgRepns1
    ),
    % The individual args may have changed, but the *number* of args
    % can't change.
    CtorRepn = CtorRepn0 ^ cr_args := ArgRepns,
    insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap).

:- pred use_double_word_floats(globals::in, bool::out) is det.

use_double_word_floats(Globals, DoubleWordFloats) :-
    globals.lookup_bool_option(Globals, allow_double_word_fields,
        AllowDoubleWords),
    (
        AllowDoubleWords = yes,
        globals.lookup_int_option(Globals, bits_per_word, TargetWordBits),
        globals.lookup_bool_option(Globals, single_prec_float, SinglePrec),
        ( if
            TargetWordBits = 32,
            SinglePrec = no
        then
            DoubleWordFloats = yes
        else
            DoubleWordFloats = no
        )
    ;
        AllowDoubleWords = no,
        DoubleWordFloats = no
    ).

:- pred set_double_word_floats(module_info::in,
    list(constructor_arg_repn)::in, list(constructor_arg_repn)::out) is det.

set_double_word_floats(_ModuleInfo, [], []).
set_double_word_floats(ModuleInfo, [Arg0 | Args0], [Arg | Args]) :-
    Arg0 = ctor_arg_repn(Name, Type, _, Context),
    ( if type_is_float_eqv(ModuleInfo, Type) then
        Arg = ctor_arg_repn(Name, Type, double_word, Context)
    else
        Arg = Arg0
    ),
    set_double_word_floats(ModuleInfo, Args0, Args).

:- pred pack_du_ctor_args(module_info::in, int::in, int::in,
    list(constructor_arg_repn)::in, list(constructor_arg_repn)::out,
    arg_width::out) is det.

pack_du_ctor_args(_ModuleInfo, _TargetWordBits, _Shift, [], [], full_word).
pack_du_ctor_args(ModuleInfo, TargetWordBits, Shift,
        [Arg0 | Args0], [Arg | Args], ArgWidth) :-
    Arg0 = ctor_arg_repn(Name, Type, ArgWidth0, Context),
    ( if type_fits_in_n_bits(ModuleInfo, Type, NumBits) then
        Mask = int.pow(2, NumBits) - 1,
        % Try to place the argument in the current word, otherwise move on to
        % the next word.
        % XXX The code does things in the opposite order.
        % XXX We don't generate an error if NumBits > TargetWordBits.
        ( if Shift + NumBits > TargetWordBits then
            ArgWidth1 = partial_word_first(Mask),
            NextShift = NumBits
        else if Shift = 0 then
            ArgWidth1 = partial_word_first(Mask),
            NextShift = NumBits
        else
            ArgWidth1 = partial_word_shifted(Shift, Mask),
            NextShift = Shift + NumBits
        ),
        pack_du_ctor_args(ModuleInfo, TargetWordBits, NextShift, Args0, Args,
            NextArgWidth),
        % If this argument starts a word but the next argument is not packed
        % with it, then this argument is not packed.
        ( if
            ArgWidth1 = partial_word_first(_),
            NextArgWidth \= partial_word_shifted(_, _)
        then
            ArgWidth = full_word
        else
            ArgWidth = ArgWidth1
        ),
        Arg = ctor_arg_repn(Name, Type, ArgWidth, Context)
    else
        Arg = Arg0,
        ArgWidth = ArgWidth0,
        NextShift = 0,
        pack_du_ctor_args(ModuleInfo, TargetWordBits, NextShift, Args0, Args,
            _)
    ).

:- pred type_fits_in_n_bits(module_info::in, mer_type::in, int::out)
    is semidet.

type_fits_in_n_bits(ModuleInfo, Type, NumBits) :-
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    % XXX Instead of invoking classify_type_defn_body, we should switch on
    % the possible shapes of TypeBody ourselves.
    TypeCategory = classify_type_defn_body(TypeBody),
    (
        TypeCategory = ctor_cat_enum(cat_enum_mercury),
        TypeBody = hlds_du_type(_, _, MaybeRepn, _),
        MaybeRepn = yes(Repn),
        NumBits = cons_tags_bits(Repn ^ dur_cons_id_to_tag_map)
    ;
        TypeCategory = ctor_cat_user(cat_user_general),
        TypeBody = hlds_abstract_type(abstract_type_fits_in_n_bits(NumBits))
        % XXX TYPE_REPN is this combination actually possible?
    ).

:- func cons_tags_bits(cons_id_to_tag_map) = int.

cons_tags_bits(ConsIdToTagMap) = NumBits :-
    map.foldl_values(max_int_tag, ConsIdToTagMap, 0, MaxFunctor),
    int.log2(MaxFunctor + 1, NumBits).

:- pred max_int_tag(cons_tag::in, int::in, int::out) is det.

max_int_tag(ConsTag, !Max) :-
    ( if ConsTag = int_tag(int_tag_int(Int)) then
        int.max(Int, !Max)
    else
        unexpected($pred, "non-integer value for enumeration")
    ).

:- func worth_arg_packing(
    list(constructor_arg_repn), list(constructor_arg_repn)) = bool.

worth_arg_packing(UnpackedArgs, PackedArgs) = Worthwhile :-
    count_words(UnpackedArgs, 0, UnpackedLength),
    count_words(PackedArgs, 0, PackedLength),
    expect(PackedLength =< UnpackedLength, $pred,
        "packed length exceeds unpacked length"),
    % Boehm GC will round up allocations (at least) to the next even number
    % of words. There is no point saving a single word if that word will be
    % allocated anyway.
    ( if round_to_even(PackedLength) < round_to_even(UnpackedLength) then
        Worthwhile = yes
    else
        Worthwhile = no
    ).

:- pred count_words(list(constructor_arg_repn)::in, int::in, int::out) is det.

count_words([], !Count).
count_words([Arg | Args], !Count) :-
    ArgWidth = Arg ^ car_width,
    (
        ArgWidth = full_word,
        !:Count = !.Count + 1
    ;
        ArgWidth = double_word,
        !:Count = !.Count + 2
    ;
        ArgWidth = partial_word_first(_),
        !:Count = !.Count + 1
    ;
        ArgWidth = partial_word_shifted(_Shift, _Mask)
    ),
    count_words(Args, !Count).

:- func round_to_even(int) = int.

round_to_even(I) = E :-
    ( if int.even(I) then
        E = I
    else
        E = I + 1
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.du_type_layout.
%---------------------------------------------------------------------------%
