%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.make_hlds_passes.du_type_layout.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_data.

:- pred decide_du_type_layout(module_info::in, type_ctor::in,
    hlds_type_defn::in, type_table::in, type_table::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module libs.globals.
:- import_module libs.options.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------------%

decide_du_type_layout(ModuleInfo, TypeCtor, TypeDefn, !TypeTable) :-
    get_type_defn_body(TypeDefn, Body0),
    (
        Body0 = hlds_du_type(Ctors0, ConsTagValues, MaybeCheaperTagTest,
            DuKind, MaybeUserEqComp, DirectArgFunctors, ReservedTag,
            ReservedAddr, MaybeForeign),
        list.map(layout_du_ctor_args(ModuleInfo, DuKind), Ctors0, Ctors),
        Body = hlds_du_type(Ctors, ConsTagValues, MaybeCheaperTagTest,
            DuKind, MaybeUserEqComp, DirectArgFunctors, ReservedTag,
            ReservedAddr, MaybeForeign),
        set_type_defn_body(Body, TypeDefn, PackedTypeDefn),
        replace_type_ctor_defn(TypeCtor, PackedTypeDefn, !TypeTable)
    ;
        ( Body0 = hlds_eqv_type(_)
        ; Body0 = hlds_foreign_type(_)
        ; Body0 = hlds_solver_type(_)
        ; Body0 = hlds_abstract_type(_)
        )
        % Leave these types alone.
    ).

:- pred layout_du_ctor_args(module_info::in, du_type_kind::in,
    constructor::in, constructor::out) is det.

layout_du_ctor_args(ModuleInfo, DuKind, Ctor0, Ctor) :-
    Ctor0 = ctor(ExistTVars, Constraints, Name, Args0, Arity, Context),
    module_info_get_globals(ModuleInfo, Globals),
    (
        ( DuKind = du_type_kind_mercury_enum
        ; DuKind = du_type_kind_foreign_enum(_)
        ; DuKind = du_type_kind_direct_dummy
        ; DuKind = du_type_kind_notag(_, _, _)
        ),
        Args1 = Args0
    ;
        DuKind = du_type_kind_general,
        % A functor with a single float argument can have a double-width word
        % if it is not a no-tag functor. An example is `poly_type.f(float)'.
        use_double_word_floats(Globals, UseDoubleWordFloats),
        (
            UseDoubleWordFloats = yes,
            set_double_word_floats(ModuleInfo, Args0, Args1)
        ;
            UseDoubleWordFloats = no,
            Args1 = Args0
        )
    ),
    globals.lookup_int_option(Globals, arg_pack_bits, ArgPackBits),
    ( if ArgPackBits > 0 then
        pack_du_ctor_args(ModuleInfo, ArgPackBits, 0, Args1, Args2, _),
        WorthPacking = worth_arg_packing(Args1, Args2),
        (
            WorthPacking = yes,
            Args = Args2
        ;
            WorthPacking = no,
            Args = Args1
        )
    else
        Args = Args1
    ),
    % The individual args may have changed, but the number of args
    % can't change.
    Ctor = ctor(ExistTVars, Constraints, Name, Args, Arity, Context).

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
    list(constructor_arg)::in, list(constructor_arg)::out) is det.

set_double_word_floats(_ModuleInfo, [], []).
set_double_word_floats(ModuleInfo, [Arg0 | Args0], [Arg | Args]) :-
    Arg0 = ctor_arg(Name, Type, _, Context),
    ( if type_is_float_eqv(ModuleInfo, Type) then
        ArgWidth = double_word,
        Arg = ctor_arg(Name, Type, ArgWidth, Context)
    else
        Arg = Arg0
    ),
    set_double_word_floats(ModuleInfo, Args0, Args).

:- pred pack_du_ctor_args(module_info::in, int::in, int::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    arg_width::out) is det.

pack_du_ctor_args(_ModuleInfo, _TargetWordBits, _Shift, [], [], full_word).
pack_du_ctor_args(ModuleInfo, TargetWordBits, Shift,
        [Arg0 | Args0], [Arg | Args], ArgWidth) :-
    Arg0 = ctor_arg(Name, Type, ArgWidth0, Context),
    ( if type_is_enum_bits(ModuleInfo, Type, NumBits) then
        Mask = int.pow(2, NumBits) - 1,
        % Try to place the argument in the current word, otherwise move on to
        % the next word.
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
        Arg = ctor_arg(Name, Type, ArgWidth, Context)
    else
        Arg = Arg0,
        ArgWidth = ArgWidth0,
        NextShift = 0,
        pack_du_ctor_args(ModuleInfo, TargetWordBits, NextShift, Args0, Args,
            _)
    ).

:- pred type_is_enum_bits(module_info::in, mer_type::in, int::out) is semidet.

type_is_enum_bits(ModuleInfo, Type, NumBits) :-
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    TypeCategory = classify_type_defn_body(TypeBody),
    (
        TypeCategory = ctor_cat_enum(cat_enum_mercury),
        NumBits = cons_tags_bits(TypeBody ^ du_type_cons_tag_values)
    ;
        TypeCategory = ctor_cat_user(cat_user_general),
        TypeBody = hlds_abstract_type(abstract_enum_type(NumBits))
    ).

:- func cons_tags_bits(cons_tag_values) = int.

cons_tags_bits(ConsTagValues) = NumBits :-
    map.foldl_values(max_int_tag, ConsTagValues, 0, MaxFunctor),
    int.log2(MaxFunctor + 1, NumBits).

:- pred max_int_tag(cons_tag::in, int::in, int::out) is det.

max_int_tag(ConsTag, !Max) :-
    ( if ConsTag = int_tag(int_tag_int(Int)) then
        int.max(Int, !Max)
    else
        unexpected($module, $pred, "non-integer value for enumeration")
    ).

:- func worth_arg_packing(list(constructor_arg), list(constructor_arg)) = bool.

worth_arg_packing(UnpackedArgs, PackedArgs) = Worthwhile :-
    count_words(UnpackedArgs, 0, UnpackedLength),
    count_words(PackedArgs, 0, PackedLength),
    expect(PackedLength =< UnpackedLength, $module, $pred,
        "packed length exceeds unpacked length"),
    % Boehm GC will round up allocations (at least) to the next even number
    % of words. There is no point saving a single word if that word will be
    % allocated anyway.
    ( if round_to_even(PackedLength) < round_to_even(UnpackedLength) then
        Worthwhile = yes
    else
        Worthwhile = no
    ).

:- pred count_words(list(constructor_arg)::in, int::in, int::out) is det.

count_words([], !Count).
count_words([Arg | Args], !Count) :-
    ArgWidth = Arg ^ arg_width,
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
:- end_module hlds.make_hlds.make_hlds_passes.du_type_layout.
%-----------------------------------------------------------------------------%
