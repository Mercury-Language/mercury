%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2008-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_mode.m.
% Main author: fjh.
%
% Utility predicates dealing with modes and insts that do not require access
% to the HLDS. (The predicates that do are in mode_util.m.)
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_mode.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Construct a mode corresponding to the standard `in', `out', `uo'
    % or `unused' mode.
    %
:- pred in_mode(mer_mode::out) is det.
:- func in_mode = mer_mode.
:- func in_mode(mer_inst) = mer_mode.
:- pred out_mode(mer_mode::out) is det.
:- func out_mode = mer_mode.
:- func out_mode(mer_inst) = mer_mode.
:- pred di_mode(mer_mode::out) is det.
:- func di_mode = mer_mode.
:- pred uo_mode(mer_mode::out) is det.
:- func uo_mode = mer_mode.
:- pred mdi_mode(mer_mode::out) is det.
:- func mdi_mode = mer_mode.
:- pred muo_mode(mer_mode::out) is det.
:- func muo_mode = mer_mode.
:- pred unused_mode(mer_mode::out) is det.
:- func unused_mode = mer_mode.
:- func in_any_mode = mer_mode.
:- func out_any_mode = mer_mode.

:- func ground_inst = mer_inst.
:- func free_inst = mer_inst.
:- func any_inst = mer_inst.

:- pred make_std_mode(string::in, list(mer_inst)::in, mer_mode::out) is det.
:- func make_std_mode(string, list(mer_inst)) = mer_mode.

%-----------------------------------------------------------------------------%

    % mode_substitute_arg_list(Mode0, Params, Args, Mode) is true iff Mode is
    % the mode that results from substituting all occurrences of Params
    % in Mode0 with the corresponding value in Args.
    %
:- pred mode_substitute_arg_list(mer_mode::in, list(inst_var)::in,
    list(mer_inst)::in, mer_mode::out) is det.

    % inst_lists_to_mode_list(InitialInsts, FinalInsts, Modes):
    %
    % Given two lists of corresponding initial and final insts, return
    % a list of modes which maps from the initial insts to the final insts.
    %
:- pred inst_lists_to_mode_list(list(mer_inst)::in, list(mer_inst)::in,
    list(mer_mode)::out) is det.

:- pred insts_to_mode(mer_inst::in, mer_inst::in, mer_mode::out) is det.

%-----------------------------------------------------------------------------%

    % inst_substitute_arg_list(Params, Args, Inst0, Inst) is true iff Inst
    % is the inst that results from substituting all occurrences of Params
    % in Inst0 with the corresponding value in Args.
    %
:- pred inst_substitute_arg_list(list(inst_var)::in, list(mer_inst)::in,
    mer_inst::in, mer_inst::out) is det.

    % inst_list_apply_substitution(Subst, Insts0, Insts) is true
    % iff Inst is the inst that results from applying Subst to Insts0.
    %
:- pred inst_list_apply_substitution(inst_var_sub::in,
    list(mer_inst)::in, list(mer_inst)::out) is det.

    % mode_list_apply_substitution(Subst, Modes0, Modes) is true
    % iff Mode is the mode that results from applying Subst to Modes0.
    %
:- pred mode_list_apply_substitution(inst_var_sub::in,
    list(mer_mode)::in, list(mer_mode)::out) is det.

:- pred rename_apart_inst_vars(inst_varset::in, inst_varset::in,
    list(mer_mode)::in, list(mer_mode)::out) is det.

    % inst_contains_unconstrained_var(Inst) iff Inst includes an
    % unconstrained inst variable.
    %
:- pred inst_contains_unconstrained_var(mer_inst::in) is semidet.

%-----------------------------------------------------------------------------%

    % Given an expanded inst and a cons_id and its arity, return the
    % insts of the arguments of the top level functor, failing if the
    % inst could not be bound to the functor.
    %
:- pred get_arg_insts(mer_inst::in, cons_id::in, arity::in,
    list(mer_inst)::out) is semidet.

    % Given a (list of) bound_insts, get the corresponding cons_ids.
    % The type_ctor, if given,
    %
:- pred bound_inst_to_cons_id(type_ctor::in, bound_inst::in,
    cons_id::out) is det.
:- pred bound_insts_to_cons_ids(type_ctor::in, list(bound_inst)::in,
    list(cons_id)::out) is det.

:- pred mode_id_to_int(mode_id::in, int::out) is det.

    % Predicates to make error messages more readable by stripping
    % "builtin." module qualifiers from modes.
    %
:- pred strip_builtin_qualifier_from_cons_id(cons_id::in, cons_id::out) is det.

:- pred strip_builtin_qualifiers_from_mode_list(list(mer_mode)::in,
    list(mer_mode)::out) is det.

:- pred strip_builtin_qualifiers_from_inst_list(list(mer_inst)::in,
    list(mer_inst)::out) is det.

:- pred strip_builtin_qualifiers_from_inst(mer_inst::in, mer_inst::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.

:- import_module map.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

in_mode(in_mode).
out_mode(out_mode).
di_mode(di_mode).
uo_mode(uo_mode).
mdi_mode(mdi_mode).
muo_mode(muo_mode).
unused_mode(unused_mode).

in_mode = make_std_mode("in", []).
in_mode(I) = make_std_mode("in", [I]).
out_mode = make_std_mode("out", []).
out_mode(I) = make_std_mode("out", [I]).
di_mode = make_std_mode("di", []).
uo_mode = make_std_mode("uo", []).
mdi_mode = make_std_mode("mdi", []).
muo_mode = make_std_mode("muo", []).
unused_mode = make_std_mode("unused", []).
in_any_mode = make_std_mode("in", [any_inst]).
out_any_mode = make_std_mode("out", [any_inst]).

ground_inst = ground(shared, none).
free_inst = free.
any_inst = any(shared, none).

make_std_mode(Name, Args, make_std_mode(Name, Args)).

make_std_mode(Name, Args) = Mode :-
    MercuryBuiltin = mercury_public_builtin_module,
    QualifiedName = qualified(MercuryBuiltin, Name),
    Mode = user_defined_mode(QualifiedName, Args).

%-----------------------------------------------------------------------------%

inst_lists_to_mode_list([], [_ | _], _) :-
    unexpected($module, $pred, "length mismatch").
inst_lists_to_mode_list([_ | _], [], _) :-
    unexpected($module, $pred, "length mismatch").
inst_lists_to_mode_list([], [], []).
inst_lists_to_mode_list([Initial | Initials], [Final | Finals],
        [Mode | Modes]) :-
    insts_to_mode(Initial, Final, Mode),
    inst_lists_to_mode_list(Initials, Finals, Modes).

insts_to_mode(Initial, Final, Mode) :-
    % Use some abbreviations.
    % This is just to make error messages and inferred modes more readable.

    ( Initial = free, Final = ground(shared, none) ->
        make_std_mode("out", [], Mode)
    ; Initial = free, Final = ground(unique, none) ->
        make_std_mode("uo", [], Mode)
    ; Initial = free, Final = ground(mostly_unique, none) ->
        make_std_mode("muo", [], Mode)
    ; Initial = ground(shared, none), Final = ground(shared, none) ->
        make_std_mode("in", [], Mode)
    ; Initial = ground(unique, none), Final = ground(clobbered, none) ->
        make_std_mode("di", [], Mode)
    ; Initial = ground(mostly_unique, none),
      Final = ground(mostly_clobbered, none) ->
        make_std_mode("mdi", [], Mode)
    ; Initial = ground(unique, none), Final = ground(unique, none) ->
        make_std_mode("ui", [], Mode)
    ; Initial = ground(mostly_unique, none),
      Final = ground(mostly_unique, none) ->
        make_std_mode("mdi", [], Mode)
    ; Initial = free ->
        make_std_mode("out", [Final], Mode)
    ; Final = ground(clobbered, none) ->
        make_std_mode("di", [Initial], Mode)
    ; Initial = Final ->
        make_std_mode("in", [Initial], Mode)
    ;
        Mode = (Initial -> Final)
    ).

%-----------------------------------------------------------------------------%

mode_substitute_arg_list(Mode0, Params, Args, Mode) :-
    (
        Params = [],
        Mode = Mode0    % optimize common case
    ;
        Params = [_ | _],
        map.from_corresponding_lists(Params, Args, Subst),
        mode_apply_substitution(Subst, Mode0, Mode)
    ).

inst_substitute_arg_list(Params, Args, Inst0, Inst) :-
    (
        Params = [],
        Inst = Inst0    % optimize common case
    ;
        Params = [_ | _],
        map.from_corresponding_lists(Params, Args, Subst),
        inst_apply_substitution(Subst, Inst0, Inst)
    ).

    % mode_apply_substitution(Mode0, Subst, Mode) is true iff
    % Mode is the mode that results from apply Subst to Mode0.
    %
:- pred mode_apply_substitution(inst_var_sub::in, mer_mode::in, mer_mode::out)
    is det.

mode_apply_substitution(Subst, (I0 -> F0), (I -> F)) :-
    inst_apply_substitution(Subst, I0, I),
    inst_apply_substitution(Subst, F0, F).
mode_apply_substitution(Subst, user_defined_mode(Name, Args0),
        user_defined_mode(Name, Args)) :-
    inst_list_apply_substitution_2(Subst, Args0, Args).

inst_list_apply_substitution(Subst, Insts0, Insts) :-
    ( map.is_empty(Subst) ->
        Insts = Insts0
    ;
        inst_list_apply_substitution_2(Subst, Insts0, Insts)
    ).

:- pred inst_list_apply_substitution_2(inst_var_sub::in,
    list(mer_inst)::in, list(mer_inst)::out) is det.

inst_list_apply_substitution_2(_, [], []).
inst_list_apply_substitution_2(Subst, [A0 | As0], [A | As]) :-
    inst_apply_substitution(Subst, A0, A),
    inst_list_apply_substitution_2(Subst, As0, As).

    % inst_substitute_arg(Inst0, Subst, Inst) is true iff Inst is the inst that
    % results from substituting all occurrences of Param in Inst0 with Arg.
    %
:- pred inst_apply_substitution(inst_var_sub::in, mer_inst::in, mer_inst::out)
    is det.

inst_apply_substitution(Subst, any(Uniq, HOInstInfo0), Inst) :-
    ho_inst_info_apply_substitution(Subst, HOInstInfo0, HOInstInfo),
    Inst = any(Uniq, HOInstInfo).
inst_apply_substitution(_, free, free).
inst_apply_substitution(_, free(T), free(T)).
inst_apply_substitution(Subst, ground(Uniq, HOInstInfo0), Inst) :-
    ho_inst_info_apply_substitution(Subst, HOInstInfo0, HOInstInfo),
    Inst = ground(Uniq, HOInstInfo).
inst_apply_substitution(Subst, bound(Uniq, Alts0), bound(Uniq, Alts)) :-
    alt_list_apply_substitution(Subst, Alts0, Alts).
inst_apply_substitution(_, not_reached, not_reached).
inst_apply_substitution(Subst, inst_var(Var), Result) :-
    ( map.search(Subst, Var, Replacement) ->
        Result = Replacement
    ;
        Result = inst_var(Var)
    ).
inst_apply_substitution(Subst, constrained_inst_vars(Vars, Inst0), Result) :-
    ( set.is_singleton(Vars, Var0) ->
        Var = Var0
    ;
        unexpected($module, $pred, "multiple inst_vars found")
    ),
    ( map.search(Subst, Var, Replacement) ->
        Result = Replacement
        % XXX Should probably have a sanity check here that
        % Replacement =< Inst0
    ;
        inst_apply_substitution(Subst, Inst0, Result0),
        Result = constrained_inst_vars(Vars, Result0)
    ).
inst_apply_substitution(Subst, defined_inst(InstName0),
        defined_inst(InstName)) :-
    ( inst_name_apply_substitution(Subst, InstName0, InstName1) ->
        InstName = InstName1
    ;
        InstName = InstName0
    ).
inst_apply_substitution(Subst, abstract_inst(Name, Args0),
            abstract_inst(Name, Args)) :-
    inst_list_apply_substitution_2(Subst, Args0, Args).

    % This predicate fails if the inst_name is not one of user_inst,
    % typed_inst or typed_ground. The other types of inst_names are just used
    % as keys in the inst_table so it does not make sense to apply
    % substitutions to them.
    %
:- pred inst_name_apply_substitution(inst_var_sub::in,
    inst_name::in, inst_name::out) is semidet.

inst_name_apply_substitution(Subst, user_inst(Name, Args0),
        user_inst(Name, Args)) :-
    inst_list_apply_substitution_2(Subst, Args0, Args).
inst_name_apply_substitution(Subst, typed_inst(T, Inst0),
        typed_inst(T, Inst)) :-
    inst_name_apply_substitution(Subst, Inst0, Inst).
inst_name_apply_substitution(_, typed_ground(Uniq, T), typed_ground(Uniq, T)).

:- pred alt_list_apply_substitution(inst_var_sub::in,
    list(bound_inst)::in, list(bound_inst)::out) is det.

alt_list_apply_substitution(_, [], []).
alt_list_apply_substitution(Subst, [Alt0 | Alts0], [Alt | Alts]) :-
    Alt0 = bound_functor(Name, Args0),
    inst_list_apply_substitution_2(Subst, Args0, Args),
    Alt = bound_functor(Name, Args),
    alt_list_apply_substitution(Subst, Alts0, Alts).

:- pred ho_inst_info_apply_substitution(inst_var_sub::in,
    ho_inst_info::in, ho_inst_info::out) is det.

ho_inst_info_apply_substitution(_, none, none).
ho_inst_info_apply_substitution(Subst, HOInstInfo0, HOInstInfo) :-
    HOInstInfo0 = higher_order(pred_inst_info(PredOrFunc, Modes0, MaybeArgRegs,
        Det)),
    mode_list_apply_substitution(Subst, Modes0, Modes),
    HOInstInfo = higher_order(pred_inst_info(PredOrFunc, Modes, MaybeArgRegs,
        Det)).

mode_list_apply_substitution(Subst, Modes0, Modes) :-
    ( map.is_empty(Subst) ->
        Modes = Modes0
    ;
        mode_list_apply_substitution_2(Subst, Modes0, Modes)
    ).

:- pred mode_list_apply_substitution_2(inst_var_sub::in,
    list(mer_mode)::in, list(mer_mode)::out) is det.

mode_list_apply_substitution_2(_, [], []).
mode_list_apply_substitution_2(Subst, [A0 | As0], [A | As]) :-
    mode_apply_substitution(Subst, A0, A),
    mode_list_apply_substitution_2(Subst, As0, As).

%-----------------------------------------------------------------------------%

rename_apart_inst_vars(VarSet, NewVarSet, Modes0, Modes) :-
    varset.merge_subst(VarSet, NewVarSet, _, Sub),
    list.map(rename_apart_inst_vars_in_mode(Sub), Modes0, Modes).

:- pred rename_apart_inst_vars_in_mode(substitution(inst_var_type)::in,
    mer_mode::in, mer_mode::out) is det.

rename_apart_inst_vars_in_mode(Sub, I0 -> F0, I -> F) :-
    rename_apart_inst_vars_in_inst(Sub, I0, I),
    rename_apart_inst_vars_in_inst(Sub, F0, F).
rename_apart_inst_vars_in_mode(Sub, user_defined_mode(Name, Insts0),
        user_defined_mode(Name, Insts)) :-
    list.map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).

:- pred rename_apart_inst_vars_in_inst(substitution(inst_var_type)::in,
    mer_inst::in, mer_inst::out) is det.

rename_apart_inst_vars_in_inst(Sub, any(Uniq, HOInstInfo0),
        any(Uniq, HOInstInfo)) :-
    (
        HOInstInfo0 = higher_order(pred_inst_info(PorF, Modes0, MaybeArgRegs,
            Det)),
        list.map(rename_apart_inst_vars_in_mode(Sub), Modes0, Modes),
        HOInstInfo = higher_order(pred_inst_info(PorF, Modes, MaybeArgRegs,
            Det))
    ;
        HOInstInfo0 = none,
        HOInstInfo = none
    ).
rename_apart_inst_vars_in_inst(_, free, free).
rename_apart_inst_vars_in_inst(_, free(T), free(T)).
rename_apart_inst_vars_in_inst(Sub, bound(U, BIs0), bound(U, BIs)) :-
    list.map(
        (pred(bound_functor(C, Is0)::in, bound_functor(C, Is)::out) is det :-
            list.map(rename_apart_inst_vars_in_inst(Sub), Is0, Is)),
        BIs0, BIs).
rename_apart_inst_vars_in_inst(Sub, ground(Uniq, HOInstInfo0),
        ground(Uniq, HOInstInfo)) :-
    (
        HOInstInfo0 = higher_order(pred_inst_info(PorF, Modes0, ArgRegs, Det)),
        list.map(rename_apart_inst_vars_in_mode(Sub), Modes0, Modes),
        HOInstInfo = higher_order(pred_inst_info(PorF, Modes, ArgRegs, Det))
    ;
        HOInstInfo0 = none,
        HOInstInfo = none
    ).
rename_apart_inst_vars_in_inst(_, not_reached, not_reached).
rename_apart_inst_vars_in_inst(Sub, inst_var(Var0), inst_var(Var)) :-
    ( map.search(Sub, Var0, term.variable(Var1, _)) ->
        Var = Var1
    ;
        Var = Var0
    ).
rename_apart_inst_vars_in_inst(Sub, constrained_inst_vars(Vars0, Inst0),
        constrained_inst_vars(Vars, Inst)) :-
    rename_apart_inst_vars_in_inst(Sub, Inst0, Inst),
    Vars = set.map(func(Var0) =
        ( map.search(Sub, Var0, term.variable(Var, _)) ->
            Var
        ;
            Var0
        ), Vars0).
rename_apart_inst_vars_in_inst(Sub, defined_inst(Name0), defined_inst(Name)) :-
    ( rename_apart_inst_vars_in_inst_name(Sub, Name0, Name1) ->
        Name = Name1
    ;
        Name = Name0
    ).
rename_apart_inst_vars_in_inst(Sub, abstract_inst(Sym, Insts0),
        abstract_inst(Sym, Insts)) :-
    list.map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).

:- pred rename_apart_inst_vars_in_inst_name(substitution(inst_var_type)::in,
    inst_name::in, inst_name::out) is semidet.

rename_apart_inst_vars_in_inst_name(Sub, user_inst(Sym, Insts0),
        user_inst(Sym, Insts)) :-
    list.map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).
rename_apart_inst_vars_in_inst_name(Sub, typed_inst(Type, Name0),
        typed_inst(Type, Name)) :-
    rename_apart_inst_vars_in_inst_name(Sub, Name0, Name).
rename_apart_inst_vars_in_inst_name(_, typed_ground(U, T), typed_ground(U, T)).

%-----------------------------------------------------------------------------%

inst_contains_unconstrained_var(Inst) :-
    require_complete_switch [Inst]
    (
        Inst = bound(_Uniq, BoundInsts),
        list.member(BoundInst, BoundInsts),
        BoundInst = bound_functor(_ConsId, ArgInsts),
        list.member(ArgInst, ArgInsts),
        inst_contains_unconstrained_var(ArgInst)
    ;
        ( Inst = ground(_Uniq, GroundInstInfo)
        ; Inst = any(_Uniq, GroundInstInfo)
        ),
        GroundInstInfo = higher_order(PredInstInfo),
        PredInstInfo = pred_inst_info(_PredOrFunc, Modes, _MaybeArgRegs,
            _Detism),
        list.member(Mode, Modes),
        (
            Mode = (SubInst -> _)
        ;
            Mode = (_ -> SubInst)
        ;
            Mode = user_defined_mode(_SymName, SubInsts),
            list.member(SubInst, SubInsts)
        ),
        inst_contains_unconstrained_var(SubInst)
    ;
        Inst = inst_var(_InstVar)
    ;
        Inst = defined_inst(InstName),
        (
            InstName = user_inst(_, SubInsts),
            list.member(SubInst, SubInsts),
            inst_contains_unconstrained_var(SubInst)
        ;
            InstName = merge_inst(SubInst, _),
            inst_contains_unconstrained_var(SubInst)
        ;
            InstName = merge_inst(_, SubInst),
            inst_contains_unconstrained_var(SubInst)
        ;
            InstName = unify_inst(_, SubInstA, SubInstB, _),
            (
                inst_contains_unconstrained_var(SubInstA)
            ;
                inst_contains_unconstrained_var(SubInstB)
            )
        ;
            InstName = ground_inst(SubInstName, _, _, _),
            inst_contains_unconstrained_var(defined_inst(SubInstName))
        ;
            InstName = any_inst(SubInstName, _, _, _),
            inst_contains_unconstrained_var(defined_inst(SubInstName))
        ;
            InstName = shared_inst(SubInstName),
            inst_contains_unconstrained_var(defined_inst(SubInstName))
        ;
            InstName = mostly_uniq_inst(SubInstName),
            inst_contains_unconstrained_var(defined_inst(SubInstName))
        ;
            InstName = typed_inst(_, SubInstName),
            inst_contains_unconstrained_var(defined_inst(SubInstName))
        )
    ;
        Inst = abstract_inst(_SymName, SubInsts),
        list.member(SubInst, SubInsts),
        inst_contains_unconstrained_var(SubInst)
    ;
        ( Inst = not_reached
        ; Inst = free
        ; Inst = free(_)
        ),
        fail
    ;
        Inst = constrained_inst_vars(_, _),
        % XXX Is this the right thing to do here? Just because Inst constrains
        % SOME of the instvars in it, it does not necessarily constrain all.
        % What we do here preserves the old behavior of this predicate.
        fail
    ).

%-----------------------------------------------------------------------------%

bound_inst_to_cons_id(TypeCtor, BoundInst, ConsId) :-
    BoundInst = bound_functor(ConsId0, _ArgInsts),
    ( ConsId0 = cons(SymName, Arity, _TypeCtor) ->
        ConsId = cons(SymName, Arity, TypeCtor)
    ;
        ConsId = ConsId0
    ).

bound_insts_to_cons_ids(_, [], []).
bound_insts_to_cons_ids(TypeCtor, [BoundInst | BoundInsts],
        [ConsId | ConsIds]) :-
    bound_inst_to_cons_id(TypeCtor, BoundInst, ConsId),
    bound_insts_to_cons_ids(TypeCtor, BoundInsts, ConsIds).

%-----------------------------------------------------------------------------%

get_arg_insts(Inst, ConsId, Arity, ArgInsts) :-
    (
        Inst = not_reached,
        list.duplicate(Arity, not_reached, ArgInsts)
    ;
        Inst = ground(Uniq, _PredInst),
        list.duplicate(Arity, ground(Uniq, none), ArgInsts)
    ;
        Inst = bound(_Uniq, List),
        ( get_arg_insts_2(List, ConsId, ArgInsts0) ->
            ArgInsts = ArgInsts0
        ;
            % The code is unreachable.
            list.duplicate(Arity, not_reached, ArgInsts)
        )
    ;
        ( Inst = free
        ; Inst = free(_)
        ),
        list.duplicate(Arity, free, ArgInsts)
    ;
        Inst = any(Uniq, _),
        list.duplicate(Arity, any(Uniq, none), ArgInsts)
    ).

:- pred get_arg_insts_2(list(bound_inst)::in, cons_id::in, list(mer_inst)::out)
    is semidet.

get_arg_insts_2([BoundInst | BoundInsts], ConsId, ArgInsts) :-
    (
        BoundInst = bound_functor(FunctorConsId, ArgInsts0),
        equivalent_cons_ids(ConsId, FunctorConsId)
    ->
        ArgInsts = ArgInsts0
    ;
        get_arg_insts_2(BoundInsts, ConsId, ArgInsts)
    ).

    % In case we later decide to change the representation of mode_ids.
mode_id_to_int(mode_id(_, X), X).

%-----------------------------------------------------------------------------%
%
% The active part of this code is strip_builtin_qualifier_from_sym_name;
% the rest is basically just recursive traversals to get there.
%

strip_builtin_qualifiers_from_mode_list(Modes0, Modes) :-
    list.map(strip_builtin_qualifiers_from_mode, Modes0, Modes).

:- pred strip_builtin_qualifiers_from_mode(mer_mode::in, mer_mode::out) is det.

strip_builtin_qualifiers_from_mode(Mode0, Mode) :-
    (
        Mode0 = (Initial0 -> Final0),
        strip_builtin_qualifiers_from_inst(Initial0, Initial),
        strip_builtin_qualifiers_from_inst(Final0, Final),
        Mode = (Initial -> Final)
    ;
        Mode0 = user_defined_mode(SymName0, Insts0),
        strip_builtin_qualifiers_from_inst_list(Insts0, Insts),
        strip_builtin_qualifier_from_sym_name(SymName0, SymName),
        Mode = user_defined_mode(SymName, Insts)
    ).

strip_builtin_qualifier_from_cons_id(ConsId0, ConsId) :-
    ( ConsId0 = cons(Name0, Arity, TypeCtor) ->
        strip_builtin_qualifier_from_sym_name(Name0, Name),
        ConsId = cons(Name, Arity, TypeCtor)
    ;
        ConsId = ConsId0
    ).

:- pred strip_builtin_qualifier_from_sym_name(sym_name::in, sym_name::out)
    is det.

strip_builtin_qualifier_from_sym_name(SymName0, SymName) :-
    (
        SymName0 = qualified(Module, Name),
        Module = mercury_public_builtin_module
    ->
        SymName = unqualified(Name)
    ;
        SymName = SymName0
    ).

strip_builtin_qualifiers_from_inst_list(Insts0, Insts) :-
    list.map(strip_builtin_qualifiers_from_inst, Insts0, Insts).

strip_builtin_qualifiers_from_inst(Inst0, Inst) :-
    (
        ( Inst0 = inst_var(_)
        ; Inst0 = not_reached
        ; Inst0 = free
        ; Inst0 = free(_)
        ),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(Vars, SubInst0),
        strip_builtin_qualifiers_from_inst(SubInst0, SubInst),
        Inst = constrained_inst_vars(Vars, SubInst)
    ;
        Inst0 = any(Uniq, HOInstInfo0),
        strip_builtin_qualifiers_from_ho_inst_info(HOInstInfo0, HOInstInfo),
        Inst = any(Uniq, HOInstInfo)
    ;
        Inst0 = ground(Uniq, HOInstInfo0),
        strip_builtin_qualifiers_from_ho_inst_info(HOInstInfo0, HOInstInfo),
        Inst = ground(Uniq, HOInstInfo)
    ;
        Inst0 = bound(Uniq, BoundInsts0),
        strip_builtin_qualifiers_from_bound_inst_list(BoundInsts0, BoundInsts),
        Inst = bound(Uniq, BoundInsts)
    ;
        Inst0 = defined_inst(InstName0),
        strip_builtin_qualifiers_from_inst_name(InstName0, InstName),
        Inst = defined_inst(InstName)
    ;
        Inst0 = abstract_inst(Name0, Args0),
        strip_builtin_qualifier_from_sym_name(Name0, Name),
        strip_builtin_qualifiers_from_inst_list(Args0, Args),
        Inst = abstract_inst(Name, Args)
    ).

:- pred strip_builtin_qualifiers_from_bound_inst_list(list(bound_inst)::in,
    list(bound_inst)::out) is det.

strip_builtin_qualifiers_from_bound_inst_list(Insts0, Insts) :-
    list.map(strip_builtin_qualifiers_from_bound_inst, Insts0, Insts).

:- pred strip_builtin_qualifiers_from_bound_inst(bound_inst::in,
    bound_inst::out) is det.

strip_builtin_qualifiers_from_bound_inst(BoundInst0, BoundInst) :-
    BoundInst0 = bound_functor(ConsId0, Insts0),
    strip_builtin_qualifier_from_cons_id(ConsId0, ConsId),
    list.map(strip_builtin_qualifiers_from_inst, Insts0, Insts),
    BoundInst = bound_functor(ConsId, Insts).

:- pred strip_builtin_qualifiers_from_inst_name(inst_name::in, inst_name::out)
    is det.

strip_builtin_qualifiers_from_inst_name(Inst0, Inst) :-
    (
        Inst0 = user_inst(SymName0, Insts0),
        strip_builtin_qualifier_from_sym_name(SymName0, SymName),
        strip_builtin_qualifiers_from_inst_list(Insts0, Insts),
        Inst = user_inst(SymName, Insts)
    ;
        Inst0 = merge_inst(InstA0, InstB0),
        strip_builtin_qualifiers_from_inst(InstA0, InstA),
        strip_builtin_qualifiers_from_inst(InstB0, InstB),
        Inst = merge_inst(InstA, InstB)
    ;
        Inst0 = unify_inst(Live, InstA0, InstB0, Real),
        strip_builtin_qualifiers_from_inst(InstA0, InstA),
        strip_builtin_qualifiers_from_inst(InstB0, InstB),
        Inst = unify_inst(Live, InstA, InstB, Real)
    ;
        Inst0 = ground_inst(InstName0, Live, Uniq, Real),
        strip_builtin_qualifiers_from_inst_name(InstName0, InstName),
        Inst = ground_inst(InstName, Live, Uniq, Real)
    ;
        Inst0 = any_inst(InstName0, Live, Uniq, Real),
        strip_builtin_qualifiers_from_inst_name(InstName0, InstName),
        Inst = any_inst(InstName, Live, Uniq, Real)
    ;
        Inst0 = shared_inst(InstName0),
        strip_builtin_qualifiers_from_inst_name(InstName0, InstName),
        Inst = shared_inst(InstName)
    ;
        Inst0 = mostly_uniq_inst(InstName0),
        strip_builtin_qualifiers_from_inst_name(InstName0, InstName),
        Inst = mostly_uniq_inst(InstName)
    ;
        Inst0 = typed_ground(_Uniq, _Type),
        Inst = Inst0
    ;
        Inst0 = typed_inst(Type, InstName0),
        strip_builtin_qualifiers_from_inst_name(InstName0, InstName),
        Inst = typed_inst(Type, InstName)
    ).

:- pred strip_builtin_qualifiers_from_ho_inst_info(ho_inst_info::in,
    ho_inst_info::out) is det.

strip_builtin_qualifiers_from_ho_inst_info(HOInstInfo0, HOInstInfo) :-
    (
        HOInstInfo0 = none,
        HOInstInfo = none
    ;
        HOInstInfo0 = higher_order(Pred0),
        Pred0 = pred_inst_info(PorF, Modes0, ArgRegs, Det),
        strip_builtin_qualifiers_from_mode_list(Modes0, Modes),
        Pred = pred_inst_info(PorF, Modes, ArgRegs, Det),
        HOInstInfo = higher_order(Pred)
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_mode.
%-----------------------------------------------------------------------------%
