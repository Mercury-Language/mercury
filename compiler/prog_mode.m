%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: prog_mode.m.
% Main author: fjh.

% Utility predicates dealing with modes and insts that do not require access
% to the HLDS. (The predicates that do are in mode_util.m.)

%-----------------------------------------------------------------------------%

:- module parse_tree__prog_mode.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module list.

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

    % Given a list of bound_insts, get the corresponding list of cons_ids
    %
:- pred functors_to_cons_ids(list(bound_inst)::in, list(cons_id)::out) is det.

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

:- implementation.

:- import_module libs.compiler_util.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_util.

:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module term.
:- import_module varset.

in_mode(in_mode).
out_mode(out_mode).
di_mode(di_mode).
uo_mode(uo_mode).
unused_mode(unused_mode).

in_mode = make_std_mode("in", []).
in_mode(I) = make_std_mode("in", [I]).
out_mode = make_std_mode("out", []).
out_mode(I) = make_std_mode("out", [I]).
di_mode = make_std_mode("di", []).
uo_mode = make_std_mode("uo", []).
unused_mode = make_std_mode("unused", []).
in_any_mode = make_std_mode("in", [any_inst]).
out_any_mode = make_std_mode("out", [any_inst]).

ground_inst = ground(shared, none).
free_inst = free.
any_inst = any(shared).

make_std_mode(Name, Args, make_std_mode(Name, Args)).

make_std_mode(Name, Args) = Mode :-
    mercury_public_builtin_module(MercuryBuiltin),
    QualifiedName = qualified(MercuryBuiltin, Name),
    Mode = user_defined_mode(QualifiedName, Args).

%-----------------------------------------------------------------------------%

inst_lists_to_mode_list([], [_ | _], _) :-
    unexpected(this_file, "inst_lists_to_mode_list: length mismatch").
inst_lists_to_mode_list([_ | _], [], _) :-
    unexpected(this_file, "inst_lists_to_mode_list: length mismatch").
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
        map__from_corresponding_lists(Params, Args, Subst),
        mode_apply_substitution(Subst, Mode0, Mode)
    ).

inst_substitute_arg_list(Params, Args, Inst0, Inst) :-
    (
        Params = [],
        Inst = Inst0    % optimize common case
    ;
        Params = [_ | _],
        map__from_corresponding_lists(Params, Args, Subst),
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
    ( map__is_empty(Subst) ->
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

inst_apply_substitution(_, any(Uniq), any(Uniq)).
inst_apply_substitution(_, free, free).
inst_apply_substitution(_, free(T), free(T)).
inst_apply_substitution(Subst, ground(Uniq, GroundInstInfo0), Inst) :-
    ground_inst_info_apply_substitution(Subst, Uniq, GroundInstInfo0, Inst).
inst_apply_substitution(Subst, bound(Uniq, Alts0), bound(Uniq, Alts)) :-
    alt_list_apply_substitution(Subst, Alts0, Alts).
inst_apply_substitution(_, not_reached, not_reached).
inst_apply_substitution(Subst, inst_var(Var), Result) :-
    ( map__search(Subst, Var, Replacement) ->
        Result = Replacement
    ;
        Result = inst_var(Var)
    ).
inst_apply_substitution(Subst, constrained_inst_vars(Vars, Inst0), Result) :-
    ( set__singleton_set(Vars, Var0) ->
        Var = Var0
    ;
        unexpected(this_file,
            "inst_apply_substitution: multiple inst_vars found")
    ),
    ( map__search(Subst, Var, Replacement) ->
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
    Alt0 = functor(Name, Args0),
    inst_list_apply_substitution_2(Subst, Args0, Args),
    Alt = functor(Name, Args),
    alt_list_apply_substitution(Subst, Alts0, Alts).

:- pred ground_inst_info_apply_substitution(inst_var_sub::in, uniqueness::in,
    ground_inst_info::in, mer_inst::out) is det.

ground_inst_info_apply_substitution(_, Uniq, none, ground(Uniq, none)).
ground_inst_info_apply_substitution(Subst, Uniq, GII0, ground(Uniq, GII)) :-
    GII0 = higher_order(pred_inst_info(PredOrFunc, Modes0, Det)),
    mode_list_apply_substitution(Subst, Modes0, Modes),
    GII = higher_order(pred_inst_info(PredOrFunc, Modes, Det)).

mode_list_apply_substitution(Subst, Modes0, Modes) :-
    ( map__is_empty(Subst) ->
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
    varset__merge_subst(VarSet, NewVarSet, _, Sub),
    list__map(rename_apart_inst_vars_in_mode(Sub), Modes0, Modes).

:- pred rename_apart_inst_vars_in_mode(substitution(inst_var_type)::in,
    mer_mode::in, mer_mode::out) is det.

rename_apart_inst_vars_in_mode(Sub, I0 -> F0, I -> F) :-
    rename_apart_inst_vars_in_inst(Sub, I0, I),
    rename_apart_inst_vars_in_inst(Sub, F0, F).
rename_apart_inst_vars_in_mode(Sub, user_defined_mode(Name, Insts0),
        user_defined_mode(Name, Insts)) :-
    list__map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).

:- pred rename_apart_inst_vars_in_inst(substitution(inst_var_type)::in,
    mer_inst::in, mer_inst::out) is det.

rename_apart_inst_vars_in_inst(_, any(U), any(U)).
rename_apart_inst_vars_in_inst(_, free, free).
rename_apart_inst_vars_in_inst(_, free(T), free(T)).
rename_apart_inst_vars_in_inst(Sub, bound(U, BIs0), bound(U, BIs)) :-
    list__map((pred(functor(C, Is0)::in, functor(C, Is)::out) is det :-
        list__map(rename_apart_inst_vars_in_inst(Sub), Is0, Is)),
        BIs0, BIs).
rename_apart_inst_vars_in_inst(Sub, ground(U, GI0), ground(U, GI)) :-
    (
        GI0 = higher_order(pred_inst_info(PoF, Modes0, Det)),
        list__map(rename_apart_inst_vars_in_mode(Sub), Modes0, Modes),
        GI = higher_order(pred_inst_info(PoF, Modes, Det))
    ;
        GI0 = none,
        GI = none
    ).
rename_apart_inst_vars_in_inst(_, not_reached, not_reached).
rename_apart_inst_vars_in_inst(Sub, inst_var(Var0), inst_var(Var)) :-
    ( map__search(Sub, Var0, term__variable(Var1)) ->
        Var = Var1
    ;
        Var = Var0
    ).
rename_apart_inst_vars_in_inst(Sub, constrained_inst_vars(Vars0, Inst0),
        constrained_inst_vars(Vars, Inst)) :-
    rename_apart_inst_vars_in_inst(Sub, Inst0, Inst),
    Vars = set__map(func(Var0) =
        ( map__search(Sub, Var0, term__variable(Var)) ->
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
    list__map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).

:- pred rename_apart_inst_vars_in_inst_name(substitution(inst_var_type)::in,
    inst_name::in, inst_name::out) is semidet.

rename_apart_inst_vars_in_inst_name(Sub, user_inst(Sym, Insts0),
        user_inst(Sym, Insts)) :-
    list__map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).
rename_apart_inst_vars_in_inst_name(Sub, typed_inst(Type, Name0),
        typed_inst(Type, Name)) :-
    rename_apart_inst_vars_in_inst_name(Sub, Name0, Name).
rename_apart_inst_vars_in_inst_name(_, typed_ground(U, T), typed_ground(U, T)).

%-----------------------------------------------------------------------------%

inst_contains_unconstrained_var(bound(_Uniqueness, BoundInsts)) :-
    list.member(BoundInst, BoundInsts),
    BoundInst = functor(_ConsId, ArgInsts),
    list.member(ArgInst, ArgInsts),
    inst_contains_unconstrained_var(ArgInst).
inst_contains_unconstrained_var(ground(_Uniqueness, GroundInstInfo)) :-
    GroundInstInfo = higher_order(PredInstInfo),
    PredInstInfo = pred_inst_info(_PredOrFunc, Modes, _Detism),
    list.member(Mode, Modes),
    (
        Mode = (Inst -> _)
    ;
        Mode = (_ -> Inst)
    ;
        Mode = user_defined_mode(_SymName, Insts),
        list.member(Inst, Insts)
    ),
    inst_contains_unconstrained_var(Inst).
inst_contains_unconstrained_var(inst_var(_InstVar)).
inst_contains_unconstrained_var(defined_inst(InstName)) :-
    (
        InstName = user_inst(_, Insts),
        list.member(Inst, Insts),
        inst_contains_unconstrained_var(Inst)
    ;
        InstName = merge_inst(Inst, _),
        inst_contains_unconstrained_var(Inst)
    ;
        InstName = merge_inst(_, Inst),
        inst_contains_unconstrained_var(Inst)
    ;
        InstName = unify_inst(_, Inst, _, _),
        inst_contains_unconstrained_var(Inst)
    ;
        InstName = unify_inst(_, _, Inst, _),
        inst_contains_unconstrained_var(Inst)
    ;
        InstName = ground_inst(InstName1, _, _, _),
        inst_contains_unconstrained_var(defined_inst(InstName1))
    ;
        InstName = any_inst(InstName1, _, _, _),
        inst_contains_unconstrained_var(defined_inst(InstName1))
    ;
        InstName = shared_inst(InstName1),
        inst_contains_unconstrained_var(defined_inst(InstName1))
    ;
        InstName = mostly_uniq_inst(InstName1),
        inst_contains_unconstrained_var(defined_inst(InstName1))
    ;
        InstName = typed_inst(_, InstName1),
        inst_contains_unconstrained_var(defined_inst(InstName1))
    ).
inst_contains_unconstrained_var(abstract_inst(_SymName, Insts)) :-
    list.member(Inst, Insts),
    inst_contains_unconstrained_var(Inst).

%-----------------------------------------------------------------------------%

functors_to_cons_ids([], []).
functors_to_cons_ids([Functor | Functors], [ConsId | ConsIds]) :-
    Functor = functor(ConsId, _ArgInsts),
    functors_to_cons_ids(Functors, ConsIds).

%-----------------------------------------------------------------------------%

get_arg_insts(not_reached, _ConsId, Arity, ArgInsts) :-
    list__duplicate(Arity, not_reached, ArgInsts).
get_arg_insts(ground(Uniq, _PredInst), _ConsId, Arity, ArgInsts) :-
    list__duplicate(Arity, ground(Uniq, none), ArgInsts).
get_arg_insts(bound(_Uniq, List), ConsId, Arity, ArgInsts) :-
    ( get_arg_insts_2(List, ConsId, ArgInsts0) ->
        ArgInsts = ArgInsts0
    ;
        % The code is unreachable.
        list__duplicate(Arity, not_reached, ArgInsts)
    ).
get_arg_insts(free, _ConsId, Arity, ArgInsts) :-
    list__duplicate(Arity, free, ArgInsts).
get_arg_insts(free(_Type), _ConsId, Arity, ArgInsts) :-
    list__duplicate(Arity, free, ArgInsts).
get_arg_insts(any(Uniq), _ConsId, Arity, ArgInsts) :-
    list__duplicate(Arity, any(Uniq), ArgInsts).

:- pred get_arg_insts_2(list(bound_inst)::in, cons_id::in, list(mer_inst)::out)
    is semidet.

get_arg_insts_2([BoundInst | BoundInsts], ConsId, ArgInsts) :-
    ( BoundInst = functor(ConsId, ArgInsts0) ->
        ArgInsts = ArgInsts0
    ;
        get_arg_insts_2(BoundInsts, ConsId, ArgInsts)
    ).

    % In case we later decided to change the representation of mode_ids.
mode_id_to_int(_ - X, X).

%-----------------------------------------------------------------------------%

    % The interesting part is strip_builtin_qualifier_from_sym_name;
    % the rest is basically just recursive traversals.
strip_builtin_qualifiers_from_mode_list(Modes0, Modes) :-
    list__map(strip_builtin_qualifiers_from_mode, Modes0, Modes).

:- pred strip_builtin_qualifiers_from_mode(mer_mode::in, mer_mode::out) is det.

strip_builtin_qualifiers_from_mode((Initial0 -> Final0), (Initial -> Final)) :-
    strip_builtin_qualifiers_from_inst(Initial0, Initial),
    strip_builtin_qualifiers_from_inst(Final0, Final).

strip_builtin_qualifiers_from_mode(user_defined_mode(SymName0, Insts0),
        user_defined_mode(SymName, Insts)) :-
    strip_builtin_qualifiers_from_inst_list(Insts0, Insts),
    strip_builtin_qualifier_from_sym_name(SymName0, SymName).

strip_builtin_qualifier_from_cons_id(ConsId0, ConsId) :-
    ( ConsId0 = cons(Name0, Arity) ->
        strip_builtin_qualifier_from_sym_name(Name0, Name),
        ConsId = cons(Name, Arity)
    ;
        ConsId = ConsId0
    ).

:- pred strip_builtin_qualifier_from_sym_name(sym_name::in, sym_name::out)
    is det.

strip_builtin_qualifier_from_sym_name(SymName0, SymName) :-
    (
        SymName0 = qualified(Module, Name),
        mercury_public_builtin_module(Module)
    ->
        SymName = unqualified(Name)
    ;
        SymName = SymName0
    ).

strip_builtin_qualifiers_from_inst_list(Insts0, Insts) :-
    list__map(strip_builtin_qualifiers_from_inst, Insts0, Insts).

strip_builtin_qualifiers_from_inst(inst_var(V), inst_var(V)).
strip_builtin_qualifiers_from_inst(constrained_inst_vars(Vars, Inst0),
        constrained_inst_vars(Vars, Inst)) :-
    strip_builtin_qualifiers_from_inst(Inst0, Inst).
strip_builtin_qualifiers_from_inst(not_reached, not_reached).
strip_builtin_qualifiers_from_inst(free, free).
strip_builtin_qualifiers_from_inst(free(Type), free(Type)).
strip_builtin_qualifiers_from_inst(any(Uniq), any(Uniq)).
strip_builtin_qualifiers_from_inst(ground(Uniq, GII0), ground(Uniq, GII)) :-
    strip_builtin_qualifiers_from_ground_inst_info(GII0, GII).
strip_builtin_qualifiers_from_inst(bound(Uniq, BoundInsts0),
        bound(Uniq, BoundInsts)) :-
    strip_builtin_qualifiers_from_bound_inst_list(BoundInsts0, BoundInsts).
strip_builtin_qualifiers_from_inst(defined_inst(Name0), defined_inst(Name)) :-
    strip_builtin_qualifiers_from_inst_name(Name0, Name).
strip_builtin_qualifiers_from_inst(abstract_inst(Name0, Args0),
        abstract_inst(Name, Args)) :-
    strip_builtin_qualifier_from_sym_name(Name0, Name),
    strip_builtin_qualifiers_from_inst_list(Args0, Args).

:- pred strip_builtin_qualifiers_from_bound_inst_list(list(bound_inst)::in,
    list(bound_inst)::out) is det.

strip_builtin_qualifiers_from_bound_inst_list(Insts0, Insts) :-
    list__map(strip_builtin_qualifiers_from_bound_inst, Insts0, Insts).

:- pred strip_builtin_qualifiers_from_bound_inst(bound_inst::in,
    bound_inst::out) is det.

strip_builtin_qualifiers_from_bound_inst(BoundInst0, BoundInst) :-
    BoundInst0 = functor(ConsId0, Insts0),
    strip_builtin_qualifier_from_cons_id(ConsId0, ConsId),
    BoundInst = functor(ConsId, Insts),
    list__map(strip_builtin_qualifiers_from_inst, Insts0, Insts).

:- pred strip_builtin_qualifiers_from_inst_name(inst_name::in, inst_name::out)
    is det.

strip_builtin_qualifiers_from_inst_name(user_inst(SymName0, Insts0),
        user_inst(SymName, Insts)) :-
    strip_builtin_qualifier_from_sym_name(SymName0, SymName),
    strip_builtin_qualifiers_from_inst_list(Insts0, Insts).
strip_builtin_qualifiers_from_inst_name(merge_inst(InstA0, InstB0),
        merge_inst(InstA, InstB)) :-
    strip_builtin_qualifiers_from_inst(InstA0, InstA),
    strip_builtin_qualifiers_from_inst(InstB0, InstB).
strip_builtin_qualifiers_from_inst_name(unify_inst(Live, InstA0, InstB0, Real),
        unify_inst(Live, InstA, InstB, Real)) :-
    strip_builtin_qualifiers_from_inst(InstA0, InstA),
    strip_builtin_qualifiers_from_inst(InstB0, InstB).
strip_builtin_qualifiers_from_inst_name(
        ground_inst(InstName0, Live, Uniq, Real),
        ground_inst(InstName, Live, Uniq, Real)) :-
    strip_builtin_qualifiers_from_inst_name(InstName0, InstName).
strip_builtin_qualifiers_from_inst_name(
        any_inst(InstName0, Live, Uniq, Real),
        any_inst(InstName, Live, Uniq, Real)) :-
    strip_builtin_qualifiers_from_inst_name(InstName0, InstName).
strip_builtin_qualifiers_from_inst_name(shared_inst(InstName0),
        shared_inst(InstName)) :-
    strip_builtin_qualifiers_from_inst_name(InstName0, InstName).
strip_builtin_qualifiers_from_inst_name(mostly_uniq_inst(InstName0),
        mostly_uniq_inst(InstName)) :-
    strip_builtin_qualifiers_from_inst_name(InstName0, InstName).
strip_builtin_qualifiers_from_inst_name(typed_ground(Uniq, Type),
        typed_ground(Uniq, Type)).
strip_builtin_qualifiers_from_inst_name(typed_inst(Type, InstName0),
        typed_inst(Type, InstName)) :-
    strip_builtin_qualifiers_from_inst_name(InstName0, InstName).

:- pred strip_builtin_qualifiers_from_ground_inst_info(ground_inst_info::in,
    ground_inst_info::out) is det.

strip_builtin_qualifiers_from_ground_inst_info(none, none).
strip_builtin_qualifiers_from_ground_inst_info(higher_order(Pred0),
        higher_order(Pred)) :-
    Pred0 = pred_inst_info(PorF, Modes0, Det),
    Pred = pred_inst_info(PorF, Modes, Det),
    strip_builtin_qualifiers_from_mode_list(Modes0, Modes).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_mode.m".

%-----------------------------------------------------------------------------%
