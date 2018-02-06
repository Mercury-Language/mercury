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

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

:- pred decide_type_repns(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.add_foreign_enum.
:- import_module hlds.add_special_pred.
:- import_module hlds.hlds_data.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.      % undesirable dependency
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
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
            Ctors, ConsTagMap0, DuKind0),
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
        compute_cheaper_tag_test(ConsTagMap, MaybeCheaperTagTest),
        % The type_ctors in the keys of DirectArgMap should always be
        % fully module qualified, just like TypeCtor.
        ( if map.search(DirectArgMap, TypeCtor, DirectArgFunctors) then
            MaybeDirectArgFunctors = yes(DirectArgFunctors)
        else
            MaybeDirectArgFunctors = no
        ),
        Repn = du_type_repn(ConsTagMap, CtorRepns, CtorRepnMap,
            MaybeCheaperTagTest, DuKind, MaybeDirectArgFunctors),
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

%---------------------------------------------------------------------------%

    % assign_constructor_tags(Globals, TypeCtor, Constructors, MaybeUserEq,
    %   TagValues, DuTypeKinds):
    %
    % Assign a tag to each constructor of a type.
    %
    % Each d.u. type is represented as a word. In the case of functors
    % with arguments, we allocate the arguments on the heap, and the
    % word contains a pointer to those arguments.
    %
    % For types which are just enumerations (all the constructors are
    % constants), we just assign a different value for each constructor.
    %
    % For types which have only one functor of arity one, there is no need
    % to store the functor, and we just store the argument value directly;
    % construction and deconstruction unifications on these types (which
    % we call no_tag types) are no-ops.
    %
    % For other types, we use a two or three bits of the word as a tag.
    % We split the constructors into constants and functors, and assign
    % tag zero to the constants (if any). If there is more than one constant,
    % we distinguish between the different constants by the value of the
    % rest of the word. Then we assign one tag bit each to the first few
    % functors. The remaining functors all get the last remaining two- or
    % three-bit tag. These functors are distinguished by a secondary tag
    % which is the first word of the argument vector for those functors.
    %
:- pred assign_constructor_tags(globals::in, type_ctor::in,
    maybe_canonical::in, list(constructor)::in, cons_id_to_tag_map::out,
    du_type_kind::out) is det.

assign_constructor_tags(Globals, TypeCtor, UserEqCmp, Ctors,
        !:CtorTags, DuTypeKind) :-
    % Work out how many tag bits we have available.
    globals.lookup_int_option(Globals, num_tag_bits, NumTagBits),

    % Now assign them.
    InitTag = 0,
    map.init(!:CtorTags),
    ( if
        % Try representing the type as an enumeration: all the constructors
        % must be constant, and we must be allowed to make unboxed enums.
        ctors_are_all_constants(Ctors)
    then
        ( if Ctors = [_] then
            DuTypeKind = du_type_kind_direct_dummy
        else
            DuTypeKind = du_type_kind_mercury_enum
        ),
        assign_enum_constants(TypeCtor, InitTag, Ctors, !CtorTags)
    else if
        % Try representing it as a no-tag type.
        type_ctor_should_be_notag(Globals, TypeCtor, Ctors, UserEqCmp,
            SingleFunctorName, SingleArgType, MaybeSingleArgName)
    then
        SingleConsId = cons(SingleFunctorName, 1, TypeCtor),
        map.det_insert(SingleConsId, no_tag, !CtorTags),
        % XXX What if SingleArgType uses reserved addresses?
        DuTypeKind = du_type_kind_notag(SingleFunctorName, SingleArgType,
            MaybeSingleArgName)
    else
        DuTypeKind = du_type_kind_general,
        ( if NumTagBits = 0 then
            % Assign reserved addresses to the constants, if possible.
            assign_unshared_tags(TypeCtor, Ctors, 0, 0, !CtorTags)
        else
            MaxTag = max_num_tags(NumTagBits) - 1,
            separate_out_constants(Ctors, Constants, Functors),
            assign_constant_tags(TypeCtor, Constants, InitTag, NextTag,
                !CtorTags),
            assign_unshared_tags(TypeCtor, Functors, NextTag, MaxTag,
                !CtorTags)
        )
    ).

:- pred assign_enum_constants(type_ctor::in, int::in, list(constructor)::in,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_enum_constants(_, _, [], !CtorTags).
assign_enum_constants(TypeCtor, Val, [Ctor | Ctors], !CtorTags) :-
    Ctor = ctor(_MaybeExistConstraints, SymName, _Args, Arity, _Ctxt),
    ConsId = cons(SymName, Arity, TypeCtor),
    Tag = int_tag(int_tag_int(Val)),
    % We call set instead of det_insert because we don't want types
    % that erroneously contain more than one copy of a cons_id to crash
    % the compiler.
    map.set(ConsId, Tag, !CtorTags),
    assign_enum_constants(TypeCtor, Val + 1, Ctors, !CtorTags).

:- pred assign_constant_tags(type_ctor::in, list(constructor)::in,
    int::in, int::out, cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_constant_tags(TypeCtor, Constants, InitTag, NextTag, !CtorTags) :-
    % If there are no constants, don't do anything. Otherwise, allocate the
    % first tag for the constants, and give them all shared local tags
    % with that tag as the primary tag, and different secondary tags
    % starting from zero.
    %
    % Note that if there is a single constant, we still give it a
    % shared_local_tag rather than a unshared_tag. That is because
    % deconstruction of the shared_local_tag is more efficient.
    (
        Constants = [],
        NextTag = InitTag
    ;
        Constants = [_ | _],
        NextTag = InitTag + 1,
        assign_shared_local_tags(TypeCtor, Constants, InitTag, 0, !CtorTags)
    ).

:- pred assign_unshared_tags(type_ctor::in, list(constructor)::in,
    int::in, int::in,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_unshared_tags(_, [], _, _, !CtorTags).
assign_unshared_tags(TypeCtor, [Ctor | Ctors], Val, MaxTag, !CtorTags) :-
    Ctor = ctor(_MaybeExistConstraints, SymName, _Args, Arity, _Ctxt),
    ConsId = cons(SymName, Arity, TypeCtor),
    ( if
        % If there is only one functor, give it the "single_functor" (untagged)
        % representation, rather than giving it unshared_tag(0).
        Val = 0,
        Ctors = []
    then
        Tag = single_functor_tag,
        % We call set instead of det_insert because we don't want types
        % that erroneously contain more than one copy of a cons_id to crash
        % the compiler.
        map.set(ConsId, Tag, !CtorTags)
    else if
        % If we are about to run out of unshared tags, start assigning
        % shared remote tags instead.
        Val = MaxTag,
        Ctors = [_ | _]
    then
        assign_shared_remote_tags(TypeCtor, [Ctor | Ctors], MaxTag, 0,
            !CtorTags)
    else if
        Val =< MaxTag
    then
        Tag = unshared_tag(Val),
        % We call set instead of det_insert because we don't want types
        % that erroneously contain more than one copy of a cons_id to crash
        % the compiler.
        map.set(ConsId, Tag, !CtorTags),
        assign_unshared_tags(TypeCtor, Ctors, Val + 1, MaxTag, !CtorTags)
    else
        unexpected($module, $pred, "exceeded max tag")
    ).

:- pred assign_shared_remote_tags(type_ctor::in, list(constructor)::in,
    int::in, int::in,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_shared_remote_tags(_, [], _, _, !CtorTags).
assign_shared_remote_tags(TypeCtor, [Ctor | Ctors], PrimaryVal, SecondaryVal,
        !CtorTags) :-
    Ctor = ctor(_MaybeExistConstraints, SymName, _Args, Arity, _Ctxt),
    ConsId = cons(SymName, Arity, TypeCtor),
    Tag = shared_remote_tag(PrimaryVal, SecondaryVal),
    % We call set instead of det_insert because we don't want types
    % that erroneously contain more than one copy of a cons_id to crash
    % the compiler.
    map.set(ConsId, Tag, !CtorTags),
    SecondaryVal1 = SecondaryVal + 1,
    assign_shared_remote_tags(TypeCtor, Ctors, PrimaryVal, SecondaryVal1,
        !CtorTags).

:- pred assign_shared_local_tags(type_ctor::in, list(constructor)::in,
    int::in, int::in, cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_shared_local_tags(_, [], _, _, !CtorTags).
assign_shared_local_tags(TypeCtor, [Ctor | Ctors], PrimaryVal, SecondaryVal,
        !CtorTags) :-
    Ctor = ctor(_MaybeExistConstraints, SymName, _Args, Arity, _Ctxt),
    ConsId = cons(SymName, Arity, TypeCtor),
    Tag = shared_local_tag(PrimaryVal, SecondaryVal),
    % We call set instead of det_insert because we don't want types
    % that erroneously contain more than one copy of a cons_id to crash
    % the compiler.
    map.set(ConsId, Tag, !CtorTags),
    assign_shared_local_tags(TypeCtor, Ctors, PrimaryVal, SecondaryVal + 1,
        !CtorTags).

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
    map.foldl_values(accumulate_max_int_tag, ConsIdToTagMap, 0, MaxFunctor),
    int.log2(MaxFunctor + 1, NumBits).

:- pred accumulate_max_int_tag(cons_tag::in, int::in, int::out) is det.

accumulate_max_int_tag(ConsTag, !Max) :-
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

%-----------------------------------------------------------------------------%

    % Look for general du type definitions that can be converted into
    % direct arg type definitions.
    %
:- pred post_process_types_direct_args(module_info::in, module_info::out,
    list(error_spec)::out) is det.

post_process_types_direct_args(!HLDS, Specs) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        globals.lookup_bool_option(Globals, record_term_sizes_as_words,
            TermSizeWords),
        globals.lookup_bool_option(Globals, record_term_sizes_as_cells,
            TermSizeCells),
        ( if
            TermSizeWords = no,
            TermSizeCells = no
        then
            module_info_get_type_table(!.HLDS, TypeTable0),
            module_info_get_name(!.HLDS, ModuleName),
            get_all_type_ctor_defns(TypeTable0, TypeCtorsDefns),
            globals.lookup_int_option(Globals, num_tag_bits, NumTagBits),
            globals.lookup_bool_option(Globals, debug_type_rep, DebugTypeRep),
            MaxTag = max_num_tags(NumTagBits) - 1,
            convert_direct_arg_functors(Target, ModuleName, DebugTypeRep,
                MaxTag, TypeCtorsDefns, TypeTable0, TypeTable, [], Specs),
            module_info_set_type_table(TypeTable, !HLDS)
        else
            % We cannot use direct arg functors in term size grades.
            Specs = []
        )
    ;
        ( Target = target_csharp
        ; Target = target_java
        ; Target = target_erlang
        ),
        % Direct arg functors have not (yet) been implemented on these targets.
        Specs = []
    ).

:- pred convert_direct_arg_functors(compilation_target::in, module_name::in,
    bool::in, int::in, assoc_list(type_ctor, hlds_type_defn)::in,
    type_table::in, type_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

convert_direct_arg_functors(_, _, _, _, [], !TypeTable, !Specs).
convert_direct_arg_functors(Target, ModuleName, DebugTypeRep, MaxTag,
        [TypeCtorDefn | TypeCtorsDefns], !TypeTable, !Specs) :-
    TypeCtorDefn = TypeCtor - TypeDefn,
    convert_direct_arg_functors_if_suitable(Target, ModuleName, DebugTypeRep,
        MaxTag, TypeCtor, TypeDefn, !TypeTable, !Specs),
    convert_direct_arg_functors(Target, ModuleName, DebugTypeRep, MaxTag,
        TypeCtorsDefns, !TypeTable, !Specs).

:- pred convert_direct_arg_functors_if_suitable(compilation_target::in,
    module_name::in, bool::in, int::in, type_ctor::in, hlds_type_defn::in,
    type_table::in, type_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

convert_direct_arg_functors_if_suitable(Target, ModuleName, DebugTypeRep,
        MaxTag, TypeCtor, TypeDefn, !TypeTable, !Specs) :-
    get_type_defn_body(TypeDefn, Body),
    (
        Body = hlds_du_type(Ctors, _MaybeCanonical, MaybeRepn0, MaybeForeign),
        ( if
            MaybeRepn0 = yes(Repn0),
            Repn0 = du_type_repn(_ConsIdToTagMap, CtorRepns0, _CtorRepnMap,
                _MaybeCheaperTagTest, DuKind, MaybeAssertedDirectArgCtors),
            CtorRepns0 = [_, _ | _],
            DuKind = du_type_kind_general,
            MaybeForeign = no,
            TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
            sym_name_get_module_name(TypeCtorSymName, TypeCtorModule)
        then
            get_type_defn_status(TypeDefn, TypeStatus),
            (
                MaybeAssertedDirectArgCtors = yes(AssertedDirectArgFunctors)
            ;
                MaybeAssertedDirectArgCtors = no,
                AssertedDirectArgFunctors = []
            ),
            separate_out_constants(Ctors, Constants, Functors),
            list.filter(
                is_direct_arg_ctor(!.TypeTable, Target, TypeCtorModule,
                    TypeStatus, AssertedDirectArgFunctors),
                Functors, DirectArgFunctors, NonDirectArgFunctors),
            (
                DirectArgFunctors = []
                % We cannot use the direct argument representation for any
                % functors.
            ;
                DirectArgFunctors = [_ | _],
                some [!NextTag, !CtorTags] (
                    !:NextTag = 0,
                    map.init(!:CtorTags),
                    assign_constant_tags(TypeCtor, Constants,
                        !NextTag, !CtorTags),
                    % We prefer to allocate primary tags to direct argument
                    % functors.
                    assign_direct_arg_tags(TypeCtor, DirectArgFunctors,
                        !NextTag, MaxTag, NonDirectArgFunctors,
                        LeftOverDirectArgFunctors, !CtorTags),
                    assign_unshared_tags(TypeCtor,
                        LeftOverDirectArgFunctors ++ NonDirectArgFunctors,
                        !.NextTag, MaxTag, !CtorTags),
                    DirectArgConsIdToTagMap = !.CtorTags
                ),
                compute_cheaper_tag_test(DirectArgConsIdToTagMap,
                    MaybeCheaperTagTest),
                DirectArgFunctorNames =
                    list.map(constructor_to_sym_name_and_arity,
                    DirectArgFunctors),
                (
                    DebugTypeRep = yes,
                    trace [io(!IO)] (
                        output_direct_arg_functor_summary(ModuleName, TypeCtor,
                            DirectArgFunctorNames, !IO)
                    )
                ;
                    DebugTypeRep = no
                ),
                list.map_foldl(
                    update_repn_of_ctor(TypeCtor, DirectArgConsIdToTagMap),
                    CtorRepns0, CtorRepns, map.init, CtorRepnMap),
                DirectArgRepn = du_type_repn(DirectArgConsIdToTagMap,
                    CtorRepns, CtorRepnMap, MaybeCheaperTagTest, DuKind,
                    yes(DirectArgFunctorNames)),
                DirectArgBody = Body ^ du_type_repn := yes(DirectArgRepn),
                set_type_defn_body(DirectArgBody, TypeDefn, DirectArgTypeDefn),
                replace_type_ctor_defn(TypeCtor, DirectArgTypeDefn, !TypeTable)
            ),
            check_incorrect_direct_arg_assertions(AssertedDirectArgFunctors,
                NonDirectArgFunctors, !Specs)
        else
            % We cannot use the direct argument representation for any
            % functors.
            true
        )
    ;
        ( Body = hlds_eqv_type(_)
        ; Body = hlds_foreign_type(_)
        ; Body = hlds_solver_type(_)
        ; Body = hlds_abstract_type(_)
        )
        % Leave these types alone.
    ).

:- pred is_direct_arg_ctor(type_table::in, compilation_target::in,
    module_name::in, type_status::in, list(sym_name_and_arity)::in,
    constructor::in) is semidet.

is_direct_arg_ctor(TypeTable, Target, TypeCtorModule, TypeStatus,
        AssertedDirectArgCtors, Ctor) :-
    Ctor = ctor(MaybeExistConstraints, ConsSymName, ConsArgs, ConsArity,
        _CtorContext),
    MaybeExistConstraints = no_exist_constraints,
    ConsArgs = [ConsArg],
    expect(unify(ConsArity, 1), $module, $pred, "ConsArity != 1"),
    ConsArg = ctor_arg(_MaybeFieldName, ArgType, _ArgContext),
    type_to_ctor_and_args(ArgType, ArgTypeCtor, ArgTypeCtorArgTypes),

    % XXX TYPE_REPN Repeating the tests on the properties of the *type*
    % for every *constructor* of the type is wasteful.
    ConsConsId = sym_name_arity(ConsSymName, ConsArity),
    ( if
        % Trust the `direct_arg' attribute of an imported type.
        type_status_is_imported(TypeStatus) = yes,
        list.contains(AssertedDirectArgCtors, ConsConsId)
    then
        ArgCond = direct_arg_asserted
    else if
        % Tuples are always acceptable argument types as they are represented
        % by word-aligned vector pointers.
        % Strings are *not* always word-aligned (yet) so are not acceptable.
        type_ctor_is_tuple(ArgTypeCtor)
    then
        ArgCond = arg_type_is_word_aligned_pointer
    else
        search_type_ctor_defn(TypeTable, ArgTypeCtor, ArgTypeDefn),
        get_type_defn_body(ArgTypeDefn, ArgTypeDefnBody),
        ( if
            is_foreign_type_for_target(ArgTypeDefnBody, Target, Assertions)
        then
            % Foreign types are acceptable arguments if asserted that their
            % values are word-aligned pointers.
            asserted_word_aligned_pointer(Assertions),
            ArgCond = arg_type_is_word_aligned_pointer
        else
            % The argument type is not a foreign type.

            ArgTypeCtorArgTypes = [],
            % XXX We could let this be a subset of the type params, but that
            % would require the runtime system to be able to handle variables
            % in the argument type, during unification and comparison
            % (mercury_unify_compare_body.h) during deconstruction
            % (mercury_ml_expand_body.h), during deep copying
            % (mercury_deep_copy_body.h), and maybe during some other
            % operations.

            ArgTypeDefnBody = hlds_du_type(ArgCtors, _ArgMaybeUserEqComp,
                ArgMaybeRepn, ArgMaybeForeign),
            (
                ArgMaybeRepn = no,
                unexpected($pred, "ArgMaybeRepn = no")
            ;
                ArgMaybeRepn = yes(ArgRepn)
            ),
            ArgRepn = du_type_repn(ArgConsIdToTagMap, ArgCtorRepns,
                _ArgConsCtorMap, ArgMaybeCheaperTagTest, ArgDuKind,
                ArgDirectArgCtors),
            ArgCtors = [_],
            ArgCtorRepns = [_],
            ArgMaybeCheaperTagTest = no_cheaper_tag_test,
            ArgDuKind = du_type_kind_general,
            ArgDirectArgCtors = no,
            ArgMaybeForeign = no,

            map.to_assoc_list(ArgConsIdToTagMap, ArgConsIdTagList),
            ArgConsIdTagList = [ArgConsIdTag],
            ArgConsIdTag = _ConsId - single_functor_tag,

            ( if
                type_status_defined_in_this_module(TypeStatus) = yes,
                list.contains(AssertedDirectArgCtors, ConsConsId)
            then
                ArgCond = direct_arg_asserted
            else
                ArgTypeCtor = type_ctor(ArgTypeCtorSymName, _ArgTypeCtorArity),
                sym_name_get_module_name(ArgTypeCtorSymName,
                    ArgTypeCtorModule),
                ( if TypeCtorModule = ArgTypeCtorModule then
                    get_type_defn_status(ArgTypeDefn, ArgTypeStatus),
                    ArgCond = arg_type_defined_in_same_module(ArgTypeStatus)
                else
                    ArgCond = arg_type_defined_in_different_module
                )
            )
        )
    ),
    module_visibilities_allow_direct_arg(TypeStatus, ArgCond) = yes.

:- type direct_arg_cond
    --->    direct_arg_asserted
            % The constructor being checked has a single argument, and a
            % `where direct_arg' attribute asserts that the direct arg
            % representation may be used for the constructor.

    ;       arg_type_is_word_aligned_pointer
            % The constructor being checked has a single argument, and either
            % the argument has a builtin type that is represented with a
            % word-aligned pointer, or the argument has a foreign type with the
            % `word_aligned_pointer' assertion.

    ;       arg_type_defined_in_same_module(type_status)
            % The constructor being checked has a single argument, and the
            % argument type is defined in the same module as the constructor.
            % The argument type has the given import status.

    ;       arg_type_defined_in_different_module.
            % The constructor being checked has a single argument, and the
            % argument type is defined in a different module from the
            % constructor.

    % When this predicate is called, we should have checked that
    % the constructor has a single argument, and the argument has a type
    % such that the direct arg functor representation may apply
    % to the constructor. We still need to check that other modules
    % would infer the same type representation for the same constructor,
    % given that they may not have the same knowledge of the constructor's type
    % or the argument type.
    %
    % TypeStatus is import status of the type of the constructor being checked.
    %
:- func module_visibilities_allow_direct_arg(type_status, direct_arg_cond)
    = bool.

module_visibilities_allow_direct_arg(TypeStatus, ArgCond) = AllowDirectArg :-
    % XXX STATUS
    TypeStatus = type_status(OldImportStatus),
    (
        % If the outer type _definition_ is not exported from this module,
        % then the direct arg representation may be used. In the absence of
        % intermodule optimisation, only this module can [de]construct values
        % of this type.
        ( OldImportStatus = status_local
        ; OldImportStatus = status_abstract_exported
        ),
        AllowDirectArg = yes
    ;
        % If the outer type is opt-exported, another module may opt-import this
        % type, but abstract-import the argument type. It could not then infer
        % if the direct arg representation is required for any functors of the
        % outer type. The problem is overcome by adding `where direct_arg'
        % attributes to the opt-exported type definition in .opt files,
        % which state the functors that require the direct arg representation.
        OldImportStatus = status_opt_exported,
        AllowDirectArg = yes
    ;
        % If the outer type is exported from this module, then the direct arg
        % representation may be used, so long as any importing modules will
        % infer the same thing.
        ( OldImportStatus = status_exported
        ; OldImportStatus = status_exported_to_submodules
        ),
        (
            ( ArgCond = direct_arg_asserted
            ; ArgCond = arg_type_is_word_aligned_pointer
            ),
            AllowDirectArg = yes
        ;
            ArgCond = arg_type_defined_in_same_module(ArgTypeStatus),
            ArgTypeStatus = type_status(ArgOldTypeStatus),
            ( if
                (
                    OldImportStatus = status_exported,
                    ArgOldTypeStatus = status_exported
                ;
                    % If the wrapper type is exported to submodules only, then
                    % the only modules whose access to the argument type
                    % matters is those submodules. Each of these
                    % ArgOldTypeStatus values allows these submodules access
                    % to the argument type's definition. The fact that some
                    % of them also allow other modules access doesn't matter,
                    % because (due to their lack of visibility to the wrapper
                    % type) the question of the wrapper's type representation
                    % won't come up in them.
                    OldImportStatus = status_exported_to_submodules,
                    ( ArgOldTypeStatus = status_exported
                    ; ArgOldTypeStatus = status_exported_to_submodules
                    ; ArgOldTypeStatus = status_abstract_exported
                    )
                )
            then
                AllowDirectArg = yes
            else
                AllowDirectArg = no
            )
        ;
            ArgCond = arg_type_defined_in_different_module,
            AllowDirectArg = no
        )
    ;
        % The direct arg representation is required if the type of the
        % constructor being checked is imported, and:
        % - if a `where direct_arg' attribute says so
        % - if the argument value is a word-aligned pointer
        % - if the argument type is imported from the same module
        OldImportStatus = status_imported(TypeImportLocn),
        (
            ( ArgCond = direct_arg_asserted
            ; ArgCond = arg_type_is_word_aligned_pointer
            ),
            AllowDirectArg = yes
        ;
            ArgCond = arg_type_defined_in_same_module(ArgTypeStatus),
            ( if
                ArgTypeStatus = type_status(status_imported(ArgImportLocn)),
                % If the argument type is only exported by an ancestor to its
                % submodules (of which we are one), the outer type must also
                % only be exported to submodules. Otherwise submodules and
                % non-submodules would infer different things.
                (
                    ArgImportLocn =
                        import_locn_ancestor_private_interface_proper
                =>
                    TypeImportLocn =
                        import_locn_ancestor_private_interface_proper
                )
            then
                AllowDirectArg = yes
            else
                AllowDirectArg = no
            )
        ;
            ArgCond = arg_type_defined_in_different_module,
            AllowDirectArg = no
        )
    ;
        % If the outer type is opt-imported, there will always be a
        % `where direct_arg' attribute on the type definition which states
        % if the direct argument representation must be used.
        ( OldImportStatus = status_opt_imported
        ; OldImportStatus = status_abstract_imported
        ),
        (
            ArgCond = direct_arg_asserted,
            AllowDirectArg = yes
        ;
            ( ArgCond = arg_type_is_word_aligned_pointer
            ; ArgCond = arg_type_defined_in_same_module(_)
            ; ArgCond = arg_type_defined_in_different_module
            ),
            AllowDirectArg = no
        )
    ;
        ( OldImportStatus = status_external(_)
        ; OldImportStatus = status_pseudo_exported
        ; OldImportStatus = status_pseudo_imported
        ),
        unexpected($module, $pred, "inappropriate status for type")
    ).

:- pred is_foreign_type_for_target(hlds_type_body::in, compilation_target::in,
    foreign_type_assertions::out) is semidet.

is_foreign_type_for_target(TypeBody, Target, Assertions) :-
    (
        TypeBody = hlds_du_type(_, _, _, MaybeForeignType),
        MaybeForeignType = yes(ForeignType)
    ;
        TypeBody = hlds_foreign_type(ForeignType)
    ),
    (
        Target = target_c,
        ForeignType ^ c = yes(foreign_type_lang_data(_, _, Assertions))
    ;
        Target = target_java,
        ForeignType ^ java = yes(foreign_type_lang_data(_, _, Assertions))
    ;
        Target = target_csharp,
        ForeignType ^ csharp = yes(foreign_type_lang_data(_, _, Assertions))
    ;
        Target = target_erlang,
        ForeignType ^ erlang = yes(foreign_type_lang_data(_, _, Assertions))
    ).

:- pred assign_direct_arg_tags(type_ctor::in, list(constructor)::in,
    int::in, int::out, int::in, list(constructor)::in, list(constructor)::out,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_direct_arg_tags(_, [], !Val, _, _, [], !CtorTags).
assign_direct_arg_tags(TypeCtor, [Ctor | Ctors], !Val, MaxTag,
        NonDirectArgFunctors, LeftOverCtors, !CtorTags) :-
    Ctor = ctor(_MaybeExistConstraints, Name, _Args, Arity, _Ctxt),
    ConsId = cons(Name, Arity, TypeCtor),
    ( if
        % If we are about to run out of unshared tags, stop, and return
        % the leftovers.
        !.Val = MaxTag,
        ( Ctors = [_ | _]
        ; NonDirectArgFunctors = [_ | _]
        )
    then
        LeftOverCtors = [Ctor | Ctors]
    else
        Tag = direct_arg_tag(!.Val),
        % We call set instead of det_insert because we don't want types
        % that erroneously contain more than one copy of a cons_id to crash
        % the compiler.
        map.set(ConsId, Tag, !CtorTags),
        !:Val = !.Val + 1,
        assign_direct_arg_tags(TypeCtor, Ctors, !Val, MaxTag,
            NonDirectArgFunctors, LeftOverCtors, !CtorTags)
    ).

:- pred check_incorrect_direct_arg_assertions(list(sym_name_and_arity)::in,
    list(constructor)::in, list(error_spec)::in, list(error_spec)::out) is det.

check_incorrect_direct_arg_assertions(_AssertedDirectArgCtors, [], !Specs).
check_incorrect_direct_arg_assertions(AssertedDirectArgCtors, [Ctor | Ctors],
        !Specs) :-
    Ctor = ctor(_, SymName, _Args, Arity, Context),
    SymNameArity = sym_name_arity(SymName, Arity),
    ( if list.contains(AssertedDirectArgCtors, SymNameArity) then
        Pieces = [words("Error:"),
            unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
            words("cannot be represented as a direct pointer to its"),
            words("sole argument."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_type_check, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    check_incorrect_direct_arg_assertions(AssertedDirectArgCtors, Ctors,
        !Specs).

:- func constructor_to_sym_name_and_arity(constructor) = sym_name_and_arity.

constructor_to_sym_name_and_arity(ctor(_, Name, _Args, Arity, _)) =
    sym_name_arity(Name, Arity).

:- pred output_direct_arg_functor_summary(module_name::in, type_ctor::in,
    list(sym_name_and_arity)::in, io::di, io::uo) is det.

output_direct_arg_functor_summary(ModuleName, TypeCtor, DirectArgFunctorNames,
        !IO) :-
    write_sym_name(ModuleName, !IO),
    io.write_string(" : ", !IO),
    write_type_ctor(TypeCtor, !IO),
    io.write_string(" : ", !IO),
    io.write_list(DirectArgFunctorNames, ", ", write_sym_name_and_arity, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

    % For data types with exactly two alternatives, one of which is a constant,
    % we can test against the constant (negating the result of the test,
    % if needed), since a test against a constant is cheaper than a tag test.
    %
    % This trick does not work on types that use reserved addresses,
    % and so it must not be used on them.
    %
:- pred compute_cheaper_tag_test(cons_id_to_tag_map::in,
    maybe_cheaper_tag_test::out) is det.

compute_cheaper_tag_test(CtorTagMap, CheaperTagTest) :-
    ( if
        map.to_assoc_list(CtorTagMap, CtorTagList),
        CtorTagList = [ConsIdA - ConsTagA, ConsIdB - ConsTagB],
        ConsIdA = cons(_, ArityA, _),
        ConsIdB = cons(_, ArityB, _)
    then
        ( if
            ArityB = 0,
            ArityA > 0
        then
            CheaperTagTest = cheaper_tag_test(ConsIdA, ConsTagA,
                ConsIdB, ConsTagB)
        else if
            ArityA = 0,
            ArityB > 0
        then
            CheaperTagTest = cheaper_tag_test(ConsIdB, ConsTagB,
                ConsIdA, ConsTagA)
        else
            CheaperTagTest = no_cheaper_tag_test
        )
    else
        CheaperTagTest = no_cheaper_tag_test
    ).

%-----------------------------------------------------------------------------%
%
% Predicates to create and maintain ctor_name_to_repn_maps.
%

:- func add_default_repn_to_ctor_arg(constructor_arg) = constructor_arg_repn.

add_default_repn_to_ctor_arg(ConsArg) = ConsArgRepn :-
    ConsArg = ctor_arg(MaybeFieldName, Type, Context),
    ConsArgRepn = ctor_arg_repn(MaybeFieldName, Type, full_word, Context).

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

:- pred update_repn_of_ctor(type_ctor::in, cons_id_to_tag_map::in,
    constructor_repn::in, constructor_repn::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

update_repn_of_ctor(TypeCtor, ConsTagMap, CtorRepn0, CtorRepn, !CtorRepnMap) :-
    % update_repn_of_ctor and add_repn_to_ctor do very similar jobs.
    CtorRepn0 = ctor_repn(_MaybeExistConstraints, SymName, _ConsTag0,
        _ArgRepns, Arity, _Context),
    ConsId = cons(SymName, Arity, TypeCtor),
    map.lookup(ConsTagMap, ConsId, ConsTag),
    CtorRepn = CtorRepn0 ^ cr_tag := ConsTag,
    insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap).

%-----------------------------------------------------------------------------%
%
% Auxiliary functions and predicates.
%

:- func max_num_tags(int) = int.

max_num_tags(NumTagBits) = MaxTags :-
    int.pow(2, NumTagBits, MaxTags).

:- pred ctors_are_all_constants(list(constructor)::in) is semidet.

ctors_are_all_constants([]).
ctors_are_all_constants([Ctor | Rest]) :-
    Ctor = ctor(_MaybeExistConstraints, _Name, Args, _, _Ctxt),
    Args = [],
    ctors_are_all_constants(Rest).

:- pred separate_out_constants(list(constructor)::in,
    list(constructor)::out, list(constructor)::out) is det.

separate_out_constants([], [], []).
separate_out_constants([Ctor | Ctors], Constants, Functors) :-
    separate_out_constants(Ctors, Constants0, Functors0),
    Args = Ctor ^ cons_args,
    (
        Args = [],
        Constants = [Ctor | Constants0],
        Functors = Functors0
    ;
        Args = [_ | _],
        Constants = Constants0,
        Functors = [Ctor | Functors0]
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.du_type_layout.
%---------------------------------------------------------------------------%
