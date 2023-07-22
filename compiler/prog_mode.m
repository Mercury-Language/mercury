%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2008-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_mode.m.
% Main author: fjh.
%
% Utility predicates dealing with modes and insts that do not require access
% to the HLDS. (The predicates that do are in mode_util.m.)
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_mode.
:- interface.

:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_util.

:- import_module list.
:- import_module maybe.
:- import_module term.

%---------------------------------------------------------------------------%

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

:- func free_inst = mer_inst.
:- func ground_inst = mer_inst.
:- func unique_inst = mer_inst.
:- func mostly_unique_inst = mer_inst.
:- func clobbered_inst = mer_inst.
:- func mostly_clobbered_inst = mer_inst.
:- func any_inst = mer_inst.

:- pred make_std_mode(string::in, list(mer_inst)::in, mer_mode::out) is det.
:- func make_std_mode(string, list(mer_inst)) = mer_mode.

%---------------------------------------------------------------------------%

    % inst_lists_to_mode_list(InitialInsts, FinalInsts, Modes):
    %
    % Given two lists of corresponding initial and final insts, return
    % a list of modes which maps from the initial insts to the final insts.
    % Try to make the modes as short and as simple as possible, to make
    % them easier to understand for readers.
    %
:- pred inst_lists_to_mode_list(list(mer_inst)::in, list(mer_inst)::in,
    list(mer_mode)::out) is det.

    % insts_to_mode(InitialInst, FinalInst, Modes):
    %
    % Given an initial and a final inst, return the mode which maps
    % the initial inst to the final inst.
    % Try to make the mode as short and as simple as possible, to make
    % it easier to understand for readers.
    %
:- pred insts_to_mode(mer_inst::in, mer_inst::in, mer_mode::out) is det.

    % from_to_insts_is_standard_mode(InitialInst, FinalInst, Modes):
    %
    % Does the given pair of initial and final insts correspond to
    % one of the eight builtin arity-0 modes (in, out; ui, di, uo;
    % and mui, mdi and muo), and if so, which one?
    %
:- pred from_to_insts_is_standard_mode(mer_inst::in, mer_inst::in, string::out)
    is semidet.

:- func simplify_std_from_to_mode(mer_mode) = mer_mode.

%---------------------------------------------------------------------------%

    % mode_substitute_arg_list(Mode0, Params, Args, Mode) is true iff Mode is
    % the mode that results from substituting all occurrences of Params
    % in Mode0 with the corresponding value in Args.
    %
:- pred mode_substitute_arg_list(mer_mode::in, list(inst_var)::in,
    list(mer_inst)::in, mer_mode::out) is det.

    % inst_substitute_arg_list(Params, Args, Inst0, Inst) is true iff Inst
    % is the inst that results from substituting all occurrences of Params
    % in Inst0 with the corresponding value in Args.
    %
:- pred inst_substitute_arg_list(list(inst_var)::in, list(mer_inst)::in,
    mer_inst::in, mer_inst::out) is det.

    % inst_list_apply_substitution(Subst, Insts0, Insts) is true
    % iff Insts is the result of applying Subst to every inst in Insts0.
    %
:- pred inst_list_apply_substitution(inst_var_sub::in,
    list(mer_inst)::in, list(mer_inst)::out) is det.

    % inst_apply_substitution(Inst0, Subst, Inst) is true iff Inst is the inst
    % that results from applying Subst to Inst0.
    %
:- pred inst_apply_substitution(inst_var_sub::in, mer_inst::in, mer_inst::out)
    is det.

    % mode_list_apply_substitution(Subst, Modes0, Modes) is true
    % iff Mode is the mode that results from applying Subst to Modes0.
    %
:- pred mode_list_apply_substitution(inst_var_sub::in,
    list(mer_mode)::in, list(mer_mode)::out) is det.

:- pred rename_apart_inst_vars(inst_varset::in, inst_varset::in,
    inst_varset::out, list(mer_mode)::in, list(mer_mode)::out) is det.

    % inst_contains_unconstrained_var(Inst) iff Inst includes an
    % unconstrained inst variable.
    %
:- pred inst_contains_unconstrained_var(mer_inst::in) is semidet.

%---------------------------------------------------------------------------%

    % Given an expanded inst and a cons_id and its arity, return the
    % insts of the arguments of the top level functor, failing if the
    % inst could not be bound to the functor.
    %
    % Note that inst_expand does not expand insts with constrained_inst_vars
    % at the top level.
    %
:- pred get_arg_insts(mer_inst::in, cons_id::in, arity::in,
    list(mer_inst)::out) is semidet.

    % As above, but abort instead of failing.
    %
:- pred get_arg_insts_det(mer_inst::in, cons_id::in, arity::in,
    list(mer_inst)::out) is det.

%---------------------------------------------------------------------------%

:- pred mode_ctor_to_int(mode_ctor::in, int::out) is det.

%---------------------------------------------------------------------------%

    % Given a (list of) bound_insts, get the corresponding cons_ids.
    % The type_ctor should be the type constructor of the variable
    % the bound_insts are for.
    %
    % Note that it is entirely possible for two *different* bound_insts
    % in a bound/3 inst to refer to the *same* cons_id, because they can
    % specify different insts for the cons_id's arguments. Therefore the
    % list returned from bound_insts_to_cons_ids may contain duplicates.
    %
:- pred bound_inst_to_cons_id(type_ctor::in, bound_inst::in,
    cons_id::out) is det.
:- pred bound_insts_to_cons_ids(type_ctor::in, list(bound_inst)::in,
    list(cons_id)::out) is det.

%---------------------------------------------------------------------------%
%
% Predicates to make error messages more readable by stripping either
% "builtin." module qualifiers, or all module qualifiers, from modes.
%

:- pred strip_module_names_from_mode_list(strip_what_module_names::in,
    list(mer_mode)::in, list(mer_mode)::out) is det.
:- pred strip_module_names_from_mode(strip_what_module_names::in,
    mer_mode::in, mer_mode::out) is det.

:- pred strip_module_names_from_inst_list(strip_what_module_names::in,
    list(mer_inst)::in, list(mer_inst)::out) is det.
:- pred strip_module_names_from_inst(strip_what_module_names::in,
    mer_inst::in, mer_inst::out) is det.

%---------------------------------------------------------------------------%
%
% Predicates to make error messages more readable by stripping "$typed_inst"
% wrappers around insts.
%

:- pred strip_typed_insts_from_mode_list(list(mer_mode)::in,
    list(mer_mode)::out) is det.
:- pred strip_typed_insts_from_mode(mer_mode::in, mer_mode::out) is det.

:- pred strip_typed_insts_from_inst_list(list(mer_inst)::in,
    list(mer_inst)::out) is det.
:- pred strip_typed_insts_from_inst(mer_inst::in, mer_inst::out) is det.

%---------------------------------------------------------------------------%

    % Replace all occurrences of inst_var(InstVar) with
    % constrained_inst_var(InstVar, ground(shared, none)).
    %
:- pred constrain_inst_vars_in_mode(mer_mode::in, mer_mode::out) is det.

    % Replace all occurrences of inst_var(InstVar) with
    % constrained_inst_var(InstVar, Inst) where InstVar -> Inst
    % is in the inst_var_sub.
    % If InstVar is not in the inst_var_sub, default to ground(shared, none).
    %
:- pred constrain_inst_vars_in_mode_sub(inst_var_sub::in,
    mer_mode::in, mer_mode::out) is det.

%---------------------------------------------------------------------------%

    % For each constrained inst_var, check whether all its occurrences
    % have the same constraint. Return a sorted list of the inst_vars
    % for which the answer is "no".
    %
:- pred inconsistent_constrained_inst_vars_in_modes(
    list(mer_mode)::in, list(inst_var)::out) is det.
:- pred inconsistent_constrained_inst_vars_in_type_and_modes(
    list(type_and_mode)::in, list(inst_var)::out) is det.

:- pred report_inconsistent_constrained_inst_vars(string::in, term.context::in,
    inst_varset::in, list(inst_var)::in, maybe(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.

:- import_module map.
:- import_module require.
:- import_module set.
:- import_module varset.

%---------------------------------------------------------------------------%

in_mode(in_mode).
in_mode = make_std_mode("in", []).
in_mode(I) = make_std_mode("in", [I]).

out_mode(out_mode).
out_mode = make_std_mode("out", []).
out_mode(I) = make_std_mode("out", [I]).

di_mode(di_mode).
di_mode = make_std_mode("di", []).

uo_mode(uo_mode).
uo_mode = make_std_mode("uo", []).

mdi_mode(mdi_mode).
mdi_mode = make_std_mode("mdi", []).

muo_mode(muo_mode).
muo_mode = make_std_mode("muo", []).

unused_mode(unused_mode).
unused_mode = make_std_mode("unused", []).

in_any_mode = make_std_mode("in", [any_inst]).
out_any_mode = make_std_mode("out", [any_inst]).

free_inst = free.
ground_inst = ground(shared, none_or_default_func).
unique_inst = ground(unique, none_or_default_func).
mostly_unique_inst = ground(mostly_unique, none_or_default_func).
clobbered_inst = ground(clobbered, none_or_default_func).
mostly_clobbered_inst = ground(mostly_clobbered, none_or_default_func).
any_inst = any(shared, none_or_default_func).

make_std_mode(Name, Args, make_std_mode(Name, Args)).

make_std_mode(Name, Args) = Mode :-
    MercuryBuiltin = mercury_public_builtin_module,
    QualifiedName = qualified(MercuryBuiltin, Name),
    Mode = user_defined_mode(QualifiedName, Args).

%---------------------------------------------------------------------------%

inst_lists_to_mode_list([], [], []).
inst_lists_to_mode_list([], [_ | _], _) :-
    unexpected($pred, "length mismatch").
inst_lists_to_mode_list([_ | _], [], _) :-
    unexpected($pred, "length mismatch").
inst_lists_to_mode_list([Initial | Initials], [Final | Finals],
        [Mode | Modes]) :-
    insts_to_mode(Initial, Final, Mode),
    inst_lists_to_mode_list(Initials, Finals, Modes).

insts_to_mode(FromInst, ToInst, Mode) :-
    % Do FromInst and ToInst represent one of the builtin arity-0 modes?
    ( if from_to_insts_is_standard_mode(FromInst, ToInst, StdMode) then
        make_std_mode(StdMode, [], Mode)
    else
        % Do they represent one of the builtin arity-1 modes?
        ( if FromInst = ToInst then
            make_std_mode("in", [FromInst], Mode)
        else if FromInst = free then
            make_std_mode("out", [ToInst], Mode)
        else if ToInst = ground(clobbered, none_or_default_func) then
            make_std_mode("di", [FromInst], Mode)
        else if ToInst = ground(mostly_clobbered, none_or_default_func) then
            make_std_mode("mdi", [FromInst], Mode)
        else
            Mode = from_to_mode(FromInst, ToInst)
        )
    ).

from_to_insts_is_standard_mode(FromInst, ToInst, StdMode) :-
    ToInst = ground(ToUniq, none_or_default_func),
    (
        ToUniq = shared,
        (
            FromInst = ground(shared, none_or_default_func),
            StdMode = "in"
        ;
            FromInst = free,
            StdMode = "out"
        )
    ;
        ToUniq = clobbered,
        FromInst = ground(unique, none_or_default_func),
        StdMode = "di"
    ;
        ToUniq = unique,
        (
            FromInst = ground(unique, none_or_default_func),
            StdMode = "ui"
        ;
            FromInst = free,
            StdMode = "uo"
        )
    ;
        ToUniq = mostly_clobbered,
        FromInst = ground(mostly_unique, none_or_default_func),
        StdMode = "mdi"
    ;
        ToUniq = mostly_unique,
        (
            FromInst = ground(mostly_unique, none_or_default_func),
            StdMode = "mui"
        ;
            FromInst = free,
            StdMode = "muo"
        )
    ).

simplify_std_from_to_mode(Mode0) = Mode :-
    (
        Mode0 = from_to_mode(FromInst, ToInst),
        ( if from_to_insts_is_standard_mode(FromInst, ToInst, StdModeName) then
            Mode = user_defined_mode(unqualified(StdModeName), [])
        else
            Mode = Mode0
        )
    ;
        Mode0 = user_defined_mode(_, _),
        Mode = Mode0
    ).

%---------------------------------------------------------------------------%

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

    % mode_apply_substitution(Subst, Mode0, Mode) is true iff
    % Mode is the mode that results from applying Subst to Mode0.
    %
:- pred mode_apply_substitution(inst_var_sub::in, mer_mode::in, mer_mode::out)
    is det.

mode_apply_substitution(Subst, Mode0, Mode) :-
    (
        Mode0 = from_to_mode(I0, F0),
        inst_apply_substitution(Subst, I0, I),
        inst_apply_substitution(Subst, F0, F),
        Mode = from_to_mode(I, F)
    ;
        Mode0 = user_defined_mode(Name, Args0),
        inst_list_apply_substitution_2(Subst, Args0, Args),
        Mode = user_defined_mode(Name, Args)
    ).

inst_list_apply_substitution(Subst, Insts0, Insts) :-
    ( if map.is_empty(Subst) then
        Insts = Insts0
    else
        inst_list_apply_substitution_2(Subst, Insts0, Insts)
    ).

:- pred inst_list_apply_substitution_2(inst_var_sub::in,
    list(mer_inst)::in, list(mer_inst)::out) is det.

inst_list_apply_substitution_2(_, [], []).
inst_list_apply_substitution_2(Subst, [Inst0 | Insts0], [Inst | Insts]) :-
    inst_apply_substitution(Subst, Inst0, Inst),
    inst_list_apply_substitution_2(Subst, Insts0, Insts).

inst_apply_substitution(Subst, Inst0, Inst) :-
    (
        ( Inst0 = not_reached
        ; Inst0 = free
        ),
        Inst = Inst0
    ;
        Inst0 = ground(Uniq, HOInstInfo0),
        ho_inst_info_apply_substitution(Subst, HOInstInfo0, HOInstInfo),
        Inst = ground(Uniq, HOInstInfo)
    ;
        Inst0 = any(Uniq, HOInstInfo0),
        ho_inst_info_apply_substitution(Subst, HOInstInfo0, HOInstInfo),
        Inst = any(Uniq, HOInstInfo)
    ;
        Inst0 = bound(Uniq0, InstResults0, BoundInsts0),
        (
            InstResults0 = inst_test_results_fgtc,
            % There is nothing to substitute.
            Inst = Inst0
        ;
            InstResults0 = inst_test_results(_, _, _, InstVarsResult, _, _),
            ( if
                InstVarsResult =
                    inst_result_contains_inst_vars_known(InstVarsSet),
                set.to_sorted_list(InstVarsSet, InstVars),
                no_inst_var_is_in_map(InstVars, Subst)
            then
                Inst = Inst0
            else
                bound_insts_apply_substitution(Subst, BoundInsts0, BoundInsts),
                % The substitution can invalidate all existing test results.
                % XXX depends on the applied ReplacementInsts.
                Inst = bound(Uniq0, inst_test_no_results, BoundInsts)
            )
        ;
            InstResults0 = inst_test_no_results,
            bound_insts_apply_substitution(Subst, BoundInsts0, BoundInsts),
            % The substitution can invalidate all existing test results.
            % XXX depends on the applied ReplacementInsts.
            Inst = bound(Uniq0, inst_test_no_results, BoundInsts)
        )
    ;
        Inst0 = inst_var(Var),
        ( if map.search(Subst, Var, ReplacementInst) then
            Inst = ReplacementInst
        else
            Inst = Inst0
        )
    ;
        Inst0 = constrained_inst_vars(Vars, SubInst0),
        ( if set.is_singleton(Vars, Var0) then
            Var = Var0
        else
            unexpected($pred, "multiple inst_vars found")
        ),
        ( if map.search(Subst, Var, ReplacementInst) then
            Inst = ReplacementInst
            % XXX Should probably have a sanity check here that
            % ReplacementInst =< Inst0
        else
            inst_apply_substitution(Subst, SubInst0, SubInst),
            Inst = constrained_inst_vars(Vars, SubInst)
        )
    ;
        Inst0 = defined_inst(InstName0),
        ( if inst_name_apply_substitution(Subst, InstName0, InstName) then
            Inst = defined_inst(InstName)
        else
            Inst = Inst0
        )
    ).

:- pred no_inst_var_is_in_map(list(inst_var)::in, map(inst_var, T)::in)
    is semidet.

no_inst_var_is_in_map([], _Map).
no_inst_var_is_in_map([InstVar | InstVars], Map) :-
    not (
        map.search(Map, InstVar, _ReplacementInst)
    ),
    no_inst_var_is_in_map(InstVars, Map).

    % This predicate fails if the inst_name is not one of user_inst,
    % typed_inst or typed_ground. The other types of inst_names are just used
    % as keys in the inst_table so it does not make sense to apply
    % substitutions to them.
    %
:- pred inst_name_apply_substitution(inst_var_sub::in,
    inst_name::in, inst_name::out) is semidet.

inst_name_apply_substitution(Subst, InstName0, InstName) :-
    (
        InstName0 = user_inst(Name, ArgInsts0),
        inst_list_apply_substitution_2(Subst, ArgInsts0, ArgInsts),
        InstName = user_inst(Name, ArgInsts)
    ;
        InstName0 = typed_inst(T, SubInst0),
        inst_name_apply_substitution(Subst, SubInst0, SubInst),
        InstName = typed_inst(T, SubInst)
    ;
        InstName0 = typed_ground(_Uniq, _T),
        % XXX Why is this here? The caller would do the same thing
        % if it wasn't here.
        InstName = InstName0
    ).

:- pred bound_insts_apply_substitution(inst_var_sub::in,
    list(bound_inst)::in, list(bound_inst)::out) is det.

bound_insts_apply_substitution(_, [], []).
bound_insts_apply_substitution(Subst,
        [BoundInst0 | BoundInsts0], [BoundInst | BoundInsts]) :-
    BoundInst0 = bound_functor(Name, Args0),
    inst_list_apply_substitution_2(Subst, Args0, Args),
    BoundInst = bound_functor(Name, Args),
    bound_insts_apply_substitution(Subst, BoundInsts0, BoundInsts).

:- pred ho_inst_info_apply_substitution(inst_var_sub::in,
    ho_inst_info::in, ho_inst_info::out) is det.

ho_inst_info_apply_substitution(_, none_or_default_func, none_or_default_func).
ho_inst_info_apply_substitution(Subst, HOInstInfo0, HOInstInfo) :-
    HOInstInfo0 =
        higher_order(pred_inst_info(PredOrFunc, Modes0, MaybeArgRegs, Det)),
    mode_list_apply_substitution(Subst, Modes0, Modes),
    HOInstInfo =
        higher_order(pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Det)).

mode_list_apply_substitution(Subst, Modes0, Modes) :-
    ( if map.is_empty(Subst) then
        Modes = Modes0
    else
        mode_list_apply_substitution_2(Subst, Modes0, Modes)
    ).

:- pred mode_list_apply_substitution_2(inst_var_sub::in,
    list(mer_mode)::in, list(mer_mode)::out) is det.

mode_list_apply_substitution_2(_, [], []).
mode_list_apply_substitution_2(Subst, [Mode0 | Modes0], [Mode | Modes]) :-
    mode_apply_substitution(Subst, Mode0, Mode),
    mode_list_apply_substitution_2(Subst, Modes0, Modes).

%---------------------------------------------------------------------------%

rename_apart_inst_vars(VarSet, NewVarSet, MergedVarSet, Modes0, Modes) :-
    varset.merge_renaming(VarSet, NewVarSet, MergedVarSet, Renaming),
    list.map(rename_apart_inst_vars_in_mode(Renaming), Modes0, Modes).

:- pred rename_apart_inst_vars_in_mode(renaming(inst_var_type)::in,
    mer_mode::in, mer_mode::out) is det.

rename_apart_inst_vars_in_mode(Renaming, Mode0, Mode) :-
    (
        Mode0 = from_to_mode(I0, F0),
        rename_apart_inst_vars_in_inst(Renaming, I0, I),
        rename_apart_inst_vars_in_inst(Renaming, F0, F),
        Mode = from_to_mode(I, F)
    ;
        Mode0 = user_defined_mode(Name, Insts0),
        list.map(rename_apart_inst_vars_in_inst(Renaming), Insts0, Insts),
        Mode = user_defined_mode(Name, Insts)
    ).

:- pred rename_apart_inst_vars_in_inst(renaming(inst_var_type)::in,
    mer_inst::in, mer_inst::out) is det.

rename_apart_inst_vars_in_inst(Renaming, Inst0, Inst) :-
    (
        ( Inst0 = not_reached
        ; Inst0 = free
        ),
        Inst = Inst0
    ;
        Inst0 = ground(Uniq, HOInstInfo0),
        (
            HOInstInfo0 =
                higher_order(pred_inst_info(PorF, Modes0, MaybeArgRegs, Det)),
            list.map(rename_apart_inst_vars_in_mode(Renaming), Modes0, Modes),
            HOInstInfo =
                higher_order(pred_inst_info(PorF, Modes, MaybeArgRegs, Det))
        ;
            HOInstInfo0 = none_or_default_func,
            HOInstInfo = none_or_default_func
        ),
        Inst = ground(Uniq, HOInstInfo)
    ;
        Inst0 = any(Uniq, HOInstInfo0),
        (
            HOInstInfo0 =
                higher_order(pred_inst_info(PorF, Modes0, MaybeArgRegs, Det)),
            list.map(rename_apart_inst_vars_in_mode(Renaming), Modes0, Modes),
            HOInstInfo =
                higher_order(pred_inst_info(PorF, Modes, MaybeArgRegs, Det))
        ;
            HOInstInfo0 = none_or_default_func,
            HOInstInfo = none_or_default_func
        ),
        Inst = any(Uniq, HOInstInfo)
    ;
        Inst0 = bound(Uniq0, InstResults0, BoundInsts0),
        (
            InstResults0 = inst_test_results_fgtc,
            % There is nothing to substitute.
            Inst = Inst0
        ;
            InstResults0 = inst_test_results(_, _, _, InstVarsResult, _, _),
            ( if
                InstVarsResult =
                    inst_result_contains_inst_vars_known(InstVarsSet),
                set.to_sorted_list(InstVarsSet, InstVars),
                no_inst_var_is_in_map(InstVars, Renaming)
            then
                Inst = Inst0
            else
                list.map(rename_apart_inst_vars_in_bound_inst(Renaming),
                    BoundInsts0, BoundInsts),
                % The substitution can invalidate all existing test results.
                % XXX depends on the applied ReplacementInsts.
                Inst = bound(Uniq0, inst_test_no_results, BoundInsts)
            )
        ;
            InstResults0 = inst_test_no_results,
            list.map(rename_apart_inst_vars_in_bound_inst(Renaming),
                BoundInsts0, BoundInsts),
            % The substitution can invalidate all existing test results.
            % XXX depends on the applied ReplacementInsts.
            Inst = bound(Uniq0, inst_test_no_results, BoundInsts)
        )
    ;
        Inst0 = inst_var(Var0),
        ( if map.search(Renaming, Var0, Var1) then
            Inst = inst_var(Var1)
        else
            Inst = Inst0
        )
    ;
        Inst0 = constrained_inst_vars(Vars0, SubInst0),
        rename_apart_inst_vars_in_inst(Renaming, SubInst0, SubInst),
        MaybeReplaceFunc = (
            func(Var0) =
                ( if map.search(Renaming, Var0, Var) then
                    Var
                else
                    Var0
                )
            ),
        Vars = set.map(MaybeReplaceFunc, Vars0),
        Inst = constrained_inst_vars(Vars, SubInst)
    ;
        Inst0 = defined_inst(Name0),
        ( if rename_apart_inst_vars_in_inst_name(Renaming, Name0, Name1) then
            Inst = defined_inst(Name1)
        else
            Inst = Inst0
        )
    ).

:- pred rename_apart_inst_vars_in_bound_inst(renaming(inst_var_type)::in,
    bound_inst::in, bound_inst::out) is det.

rename_apart_inst_vars_in_bound_inst(Renaming, BoundInst0, BoundInst) :-
    BoundInst0 = bound_functor(ConsId, ArgInsts0),
    list.map(rename_apart_inst_vars_in_inst(Renaming), ArgInsts0, ArgInsts),
    BoundInst = bound_functor(ConsId, ArgInsts).

:- pred rename_apart_inst_vars_in_inst_name(renaming(inst_var_type)::in,
    inst_name::in, inst_name::out) is semidet.

rename_apart_inst_vars_in_inst_name(Renaming, InstName0, InstName) :-
    (
        InstName0 = user_inst(Sym, Insts0),
        list.map(rename_apart_inst_vars_in_inst(Renaming), Insts0, Insts),
        InstName = user_inst(Sym, Insts)
    ;
        InstName0 = typed_inst(Type, Name0),
        rename_apart_inst_vars_in_inst_name(Renaming, Name0, Name),
        InstName = typed_inst(Type, Name)
    ;
        InstName0 = typed_ground(_U, _T),
        % XXX Why is this here? The caller would do the same thing
        % if it wasn't here.
        InstName = InstName0
    ).

%---------------------------------------------------------------------------%

inst_contains_unconstrained_var(Inst) :-
    require_complete_switch [Inst]
    (
        ( Inst = not_reached
        ; Inst = free
        ),
        fail
    ;
        Inst = inst_var(_InstVar)
    ;
        ( Inst = ground(_Uniq, GroundInstInfo)
        ; Inst = any(_Uniq, GroundInstInfo)
        ),
        GroundInstInfo = higher_order(PredInstInfo),
        PredInstInfo = pred_inst_info(_PredOrFunc, Modes,
            _MaybeArgRegs, _Detism),
        list.member(Mode, Modes),
        (
            Mode = from_to_mode(SubInst, _)
        ;
            Mode = from_to_mode(_, SubInst)
        ;
            Mode = user_defined_mode(_SymName, SubInsts),
            list.member(SubInst, SubInsts)
        ),
        inst_contains_unconstrained_var(SubInst)
    ;
        Inst = bound(_Uniq, InstResults, BoundInsts),
        (
            InstResults = inst_test_no_results
        ;
            InstResults = inst_test_results(_, _, _, InstVarsResult, _, _),
            (
                InstVarsResult = inst_result_contains_inst_vars_unknown
            ;
                InstVarsResult =
                    inst_result_contains_inst_vars_known(InstVars),
                set.is_non_empty(InstVars)
            )
        ;
            InstResults = inst_test_results_fgtc,
            fail
        ),
        list.member(BoundInst, BoundInsts),
        BoundInst = bound_functor(_ConsId, ArgInsts),
        list.member(ArgInst, ArgInsts),
        inst_contains_unconstrained_var(ArgInst)
    ;
        Inst = defined_inst(InstName),
        inst_name_contains_unconstrained_var(InstName)
    ;
        Inst = constrained_inst_vars(_, _),
        % XXX Is this the right thing to do here? Just because Inst constrains
        % SOME of the instvars in it, it does not necessarily constrain all.
        % What we do here preserves the old behavior of this predicate.
        fail
    ).

    % inst_name_contains_unconstrained_var(InstName) iff InstName includes
    % an unconstrained inst variable.
    %
:- pred inst_name_contains_unconstrained_var(inst_name::in) is semidet.

inst_name_contains_unconstrained_var(InstName) :-
    require_complete_switch [InstName]
    (
        InstName = user_inst(_, SubInsts),
        list.member(SubInst, SubInsts),
        inst_contains_unconstrained_var(SubInst)
    ;
        InstName = unify_inst(_, _, SubInstA, SubInstB),
        ( inst_contains_unconstrained_var(SubInstA)
        ; inst_contains_unconstrained_var(SubInstB)
        )
    ;
        InstName = merge_inst(SubInstA, SubInstB),
        ( inst_contains_unconstrained_var(SubInstA)
        ; inst_contains_unconstrained_var(SubInstB)
        )
    ;
        InstName = ground_inst(SubInstName, _, _, _),
        inst_name_contains_unconstrained_var(SubInstName)
    ;
        InstName = any_inst(SubInstName, _, _, _),
        inst_name_contains_unconstrained_var(SubInstName)
    ;
        InstName = shared_inst(SubInstName),
        inst_name_contains_unconstrained_var(SubInstName)
    ;
        InstName = mostly_uniq_inst(SubInstName),
        inst_name_contains_unconstrained_var(SubInstName)
    ;
        InstName = typed_inst(_, SubInstName),
        inst_name_contains_unconstrained_var(SubInstName)
    ;
        InstName = typed_ground(_, _),
        fail
    ).

%---------------------------------------------------------------------------%

get_arg_insts(Inst, ConsId, Arity, ArgInsts) :-
    % XXX This is very similar to get_single_arg_inst in mode_util.
    % XXX Actually, it should be MORE similar; it does not handle
    % some cases that get_single_arg_inst does.
    require_complete_switch [Inst]
    (
        Inst = not_reached,
        list.duplicate(Arity, not_reached, ArgInsts)
    ;
        Inst = ground(Uniq, _PredInst),
        list.duplicate(Arity, ground(Uniq, none_or_default_func), ArgInsts)
    ;
        Inst = bound(_Uniq, _InstResults, BoundInsts),
        ( if get_arg_insts_2(BoundInsts, ConsId, ArgInsts0) then
            ArgInsts = ArgInsts0
        else
            % The code is unreachable.
            list.duplicate(Arity, not_reached, ArgInsts)
        )
    ;
        Inst = free,
        list.duplicate(Arity, free, ArgInsts)
    ;
        Inst = any(Uniq, _),
        list.duplicate(Arity, any(Uniq, none_or_default_func), ArgInsts)
    ;
        Inst = defined_inst(_),
        % The documentation of this predicate says that it should be called
        % only with insts that have already been expanded.
        unexpected($pred, "defined_inst")
    ;
        Inst = constrained_inst_vars(_Vars, SubInst),
        get_arg_insts(SubInst, ConsId, Arity, ArgInsts)
    ;
        Inst = inst_var(_),
        % The compiler has no information about what function symbol
        % the variable whose inst Inst represents is bound to, so it
        % cannot have any information about the insts of its arguments.
        fail
    ).

get_arg_insts_det(Inst, ConsId, Arity, ArgInsts) :-
    ( if get_arg_insts(Inst, ConsId, Arity, ArgInstsPrime) then
        ArgInsts = ArgInstsPrime
    else
        unexpected($pred, "get_arg_insts failed")
    ).

:- pred get_arg_insts_2(list(bound_inst)::in, cons_id::in, list(mer_inst)::out)
    is semidet.

get_arg_insts_2([BoundInst | BoundInsts], ConsId, ArgInsts) :-
    ( if
        BoundInst = bound_functor(FunctorConsId, ArgInsts0),
        equivalent_cons_ids(ConsId, FunctorConsId)
    then
        ArgInsts = ArgInsts0
    else
        get_arg_insts_2(BoundInsts, ConsId, ArgInsts)
    ).

%---------------------------------------------------------------------------%

    % In case we later decide to change the representation of mode_ctors.
mode_ctor_to_int(mode_ctor(_, X), X).

%---------------------------------------------------------------------------%

bound_inst_to_cons_id(TypeCtor, BoundInst, ConsId) :-
    BoundInst = bound_functor(ConsId0, _ArgInsts),
    ( if ConsId0 = cons(ConsIdSymName0, ConsIdArity, _ConsIdTypeCtor) then
        % Insts don't (yet) have to say what type they are for,
        % so we cannot rely on the bound_functors inside them
        % being correctly module qualified, and in fact it may be that
        % the same functor in such an inst should be module qualified
        % *differently* in different contexts. Therefore in cases like this,
        % when the context of use (the type of the function symbol) is known,
        % we copy the module qualifier from the type to the function symbol.
        TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
        (
            TypeCtorSymName = unqualified(_),
            unexpected($pred, "unqualified TypeCtorSymName")
        ;
            TypeCtorSymName = qualified(TypeCtorModuleName, _TypeCtorName),
            ConsIdName = unqualify_name(ConsIdSymName0),
            ConsIdSymName = qualified(TypeCtorModuleName, ConsIdName)
        ),
        ConsId = cons(ConsIdSymName, ConsIdArity, TypeCtor)
    else
        ConsId = ConsId0
    ).

bound_insts_to_cons_ids(_, [], []).
bound_insts_to_cons_ids(TypeCtor, [BoundInst | BoundInsts],
        [ConsId | ConsIds]) :-
    bound_inst_to_cons_id(TypeCtor, BoundInst, ConsId),
    bound_insts_to_cons_ids(TypeCtor, BoundInsts, ConsIds).

%---------------------------------------------------------------------------%
%
% The active parts of this code are strip_module_name_from_{sym_name,cons_id};
% the rest is basically just recursive traversals to get there.
%

strip_module_names_from_mode_list(StripWhat, Modes0, Modes) :-
    list.map(strip_module_names_from_mode(StripWhat), Modes0, Modes).

strip_module_names_from_mode(StripWhat, Mode0, Mode) :-
    (
        Mode0 = from_to_mode(Initial0, Final0),
        strip_module_names_from_inst(StripWhat, Initial0, Initial),
        strip_module_names_from_inst(StripWhat, Final0, Final),
        Mode = from_to_mode(Initial, Final)
    ;
        Mode0 = user_defined_mode(SymName0, Insts0),
        strip_module_names_from_inst_list(StripWhat, Insts0, Insts),
        strip_module_names_from_sym_name(StripWhat, SymName0, SymName),
        Mode = user_defined_mode(SymName, Insts)
    ).

strip_module_names_from_inst_list(StripWhat, Insts0, Insts) :-
    list.map(strip_module_names_from_inst(StripWhat), Insts0, Insts).

strip_module_names_from_inst(StripWhat, Inst0, Inst) :-
    (
        ( Inst0 = inst_var(_)
        ; Inst0 = not_reached
        ; Inst0 = free
        ),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(Vars, SubInst0),
        strip_module_names_from_inst(StripWhat, SubInst0, SubInst),
        Inst = constrained_inst_vars(Vars, SubInst)
    ;
        Inst0 = any(Uniq, HOInstInfo0),
        strip_module_names_from_ho_inst_info(StripWhat,
            HOInstInfo0, HOInstInfo),
        Inst = any(Uniq, HOInstInfo)
    ;
        Inst0 = ground(Uniq, HOInstInfo0),
        strip_module_names_from_ho_inst_info(StripWhat,
            HOInstInfo0, HOInstInfo),
        Inst = ground(Uniq, HOInstInfo)
    ;
        Inst0 = bound(Uniq, InstResults, BoundInsts0),
        strip_module_names_from_bound_inst_list(StripWhat,
            BoundInsts0, BoundInsts),
        Inst = bound(Uniq, InstResults, BoundInsts)
    ;
        Inst0 = defined_inst(InstName0),
        strip_module_names_from_inst_name(StripWhat, InstName0, InstName),
        Inst = defined_inst(InstName)
    ).

:- pred strip_module_names_from_bound_inst_list(strip_what_module_names::in,
    list(bound_inst)::in, list(bound_inst)::out) is det.

strip_module_names_from_bound_inst_list(StripWhat, Insts0, Insts) :-
    list.map(strip_module_names_from_bound_inst(StripWhat), Insts0, Insts).

:- pred strip_module_names_from_bound_inst(strip_what_module_names::in,
    bound_inst::in, bound_inst::out) is det.

strip_module_names_from_bound_inst(StripWhat, BoundInst0, BoundInst) :-
    BoundInst0 = bound_functor(ConsId0, Insts0),
    strip_module_names_from_cons_id(StripWhat, ConsId0, ConsId),
    list.map(strip_module_names_from_inst(StripWhat), Insts0, Insts),
    BoundInst = bound_functor(ConsId, Insts).

:- pred strip_module_names_from_inst_name(strip_what_module_names::in,
    inst_name::in, inst_name::out) is det.

strip_module_names_from_inst_name(StripWhat, InstName0, InstName) :-
    (
        InstName0 = user_inst(SymName0, Insts0),
        strip_module_names_from_sym_name(StripWhat, SymName0, SymName),
        strip_module_names_from_inst_list(StripWhat, Insts0, Insts),
        InstName = user_inst(SymName, Insts)
    ;
        InstName0 = unify_inst(Live, Real, InstA0, InstB0),
        strip_module_names_from_inst(StripWhat, InstA0, InstA),
        strip_module_names_from_inst(StripWhat, InstB0, InstB),
        InstName = unify_inst(Live, Real, InstA, InstB)
    ;
        InstName0 = merge_inst(InstA0, InstB0),
        strip_module_names_from_inst(StripWhat, InstA0, InstA),
        strip_module_names_from_inst(StripWhat, InstB0, InstB),
        InstName = merge_inst(InstA, InstB)
    ;
        InstName0 = ground_inst(SubInstName0, Uniq, Live, Real),
        strip_module_names_from_inst_name(StripWhat,
            SubInstName0, SubInstName),
        InstName = ground_inst(SubInstName, Uniq, Live, Real)
    ;
        InstName0 = any_inst(SubInstName0, Uniq, Live, Real),
        strip_module_names_from_inst_name(StripWhat,
            SubInstName0, SubInstName),
        InstName = any_inst(SubInstName, Uniq, Live, Real)
    ;
        InstName0 = shared_inst(SubInstName0),
        strip_module_names_from_inst_name(StripWhat,
            SubInstName0, SubInstName),
        InstName = shared_inst(SubInstName)
    ;
        InstName0 = mostly_uniq_inst(SubInstName0),
        strip_module_names_from_inst_name(StripWhat,
            SubInstName0, SubInstName),
        InstName = mostly_uniq_inst(SubInstName)
    ;
        InstName0 = typed_ground(_Uniq, _Type),
        InstName = InstName0
    ;
        InstName0 = typed_inst(Type, SubInstName0),
        strip_module_names_from_inst_name(StripWhat,
            SubInstName0, SubInstName),
        InstName = typed_inst(Type, SubInstName)
    ).

:- pred strip_module_names_from_ho_inst_info(strip_what_module_names::in,
    ho_inst_info::in, ho_inst_info::out) is det.

strip_module_names_from_ho_inst_info(StripWhat, HOInstInfo0, HOInstInfo) :-
    (
        HOInstInfo0 = none_or_default_func,
        HOInstInfo = none_or_default_func
    ;
        HOInstInfo0 = higher_order(Pred0),
        Pred0 = pred_inst_info(PorF, Modes0, ArgRegs, Det),
        strip_module_names_from_mode_list(StripWhat, Modes0, Modes),
        Pred = pred_inst_info(PorF, Modes, ArgRegs, Det),
        HOInstInfo = higher_order(Pred)
    ).

%---------------------------------------------------------------------------%

strip_typed_insts_from_mode_list(Modes0, Modes) :-
    list.map(strip_typed_insts_from_mode, Modes0, Modes).

strip_typed_insts_from_mode(Mode0, Mode) :-
    (
        Mode0 = from_to_mode(Initial0, Final0),
        strip_typed_insts_from_inst(Initial0, Initial),
        strip_typed_insts_from_inst(Final0, Final),
        Mode = from_to_mode(Initial, Final)
    ;
        Mode0 = user_defined_mode(SymName, Insts0),
        strip_typed_insts_from_inst_list(Insts0, Insts),
        Mode = user_defined_mode(SymName, Insts)
    ).

strip_typed_insts_from_inst_list(Insts0, Insts) :-
    list.map(strip_typed_insts_from_inst, Insts0, Insts).

strip_typed_insts_from_inst(Inst0, Inst) :-
    (
        ( Inst0 = inst_var(_)
        ; Inst0 = not_reached
        ; Inst0 = free
        ),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(Vars, SubInst0),
        strip_typed_insts_from_inst(SubInst0, SubInst),
        Inst = constrained_inst_vars(Vars, SubInst)
    ;
        Inst0 = any(Uniq, HOInstInfo0),
        strip_typed_insts_from_ho_inst_info(HOInstInfo0, HOInstInfo),
        Inst = any(Uniq, HOInstInfo)
    ;
        Inst0 = ground(Uniq, HOInstInfo0),
        strip_typed_insts_from_ho_inst_info(HOInstInfo0, HOInstInfo),
        Inst = ground(Uniq, HOInstInfo)
    ;
        Inst0 = bound(Uniq, InstResults, BoundInsts0),
        strip_typed_insts_from_bound_inst_list(BoundInsts0, BoundInsts),
        Inst = bound(Uniq, InstResults, BoundInsts)
    ;
        Inst0 = defined_inst(InstName0),
        strip_typed_insts_from_inst_name(InstName0, InstName),
        Inst = defined_inst(InstName)
    ).

:- pred strip_typed_insts_from_bound_inst_list(list(bound_inst)::in,
    list(bound_inst)::out) is det.

strip_typed_insts_from_bound_inst_list(Insts0, Insts) :-
    list.map(strip_typed_insts_from_bound_inst, Insts0, Insts).

:- pred strip_typed_insts_from_bound_inst(bound_inst::in,
    bound_inst::out) is det.

strip_typed_insts_from_bound_inst(BoundInst0, BoundInst) :-
    BoundInst0 = bound_functor(ConsId, Insts0),
    list.map(strip_typed_insts_from_inst, Insts0, Insts),
    BoundInst = bound_functor(ConsId, Insts).

:- pred strip_typed_insts_from_inst_name(inst_name::in, inst_name::out)
    is det.

strip_typed_insts_from_inst_name(InstName0, InstName) :-
    (
        InstName0 = user_inst(SymName0, Insts0),
        strip_builtin_qualifier_from_sym_name(SymName0, SymName),
        strip_typed_insts_from_inst_list(Insts0, Insts),
        InstName = user_inst(SymName, Insts)
    ;
        InstName0 = unify_inst(Live, Real, InstA0, InstB0),
        strip_typed_insts_from_inst(InstA0, InstA),
        strip_typed_insts_from_inst(InstB0, InstB),
        InstName = unify_inst(Live, Real, InstA, InstB)
    ;
        InstName0 = merge_inst(InstA0, InstB0),
        strip_typed_insts_from_inst(InstA0, InstA),
        strip_typed_insts_from_inst(InstB0, InstB),
        InstName = merge_inst(InstA, InstB)
    ;
        InstName0 = ground_inst(SubInstName0, Uniq, Live, Real),
        strip_typed_insts_from_inst_name(SubInstName0, SubInstName),
        InstName = ground_inst(SubInstName, Uniq, Live, Real)
    ;
        InstName0 = any_inst(SubInstName0, Uniq, Live, Real),
        strip_typed_insts_from_inst_name(SubInstName0, SubInstName),
        InstName = any_inst(SubInstName, Uniq, Live, Real)
    ;
        InstName0 = shared_inst(SubInstName0),
        strip_typed_insts_from_inst_name(SubInstName0, SubInstName),
        InstName = shared_inst(SubInstName)
    ;
        InstName0 = mostly_uniq_inst(SubInstName0),
        strip_typed_insts_from_inst_name(SubInstName0, SubInstName),
        InstName = mostly_uniq_inst(SubInstName)
    ;
        InstName0 = typed_ground(_Uniq, _Type),
        % XXX Since "ground" is an inst but not an inst *name*, we cannot
        % strip the "ground" part as long as we want to return an inst name.
        InstName = InstName0
    ;
        InstName0 = typed_inst(_Type, SubInstName0),
        strip_typed_insts_from_inst_name(SubInstName0, SubInstName),
        InstName = SubInstName
    ).

:- pred strip_typed_insts_from_ho_inst_info(ho_inst_info::in,
    ho_inst_info::out) is det.

strip_typed_insts_from_ho_inst_info(HOInstInfo0, HOInstInfo) :-
    (
        HOInstInfo0 = none_or_default_func,
        HOInstInfo = none_or_default_func
    ;
        HOInstInfo0 = higher_order(Pred0),
        Pred0 = pred_inst_info(PorF, Modes0, ArgRegs, Det),
        strip_typed_insts_from_mode_list(Modes0, Modes),
        Pred = pred_inst_info(PorF, Modes, ArgRegs, Det),
        HOInstInfo = higher_order(Pred)
    ).

%---------------------------------------------------------------------------%

constrain_inst_vars_in_mode(Mode0, Mode) :-
    constrain_inst_vars_in_mode_sub(map.init, Mode0, Mode).

constrain_inst_vars_in_mode_sub(InstConstraints, Mode0, Mode) :-
    (
        Mode0 = from_to_mode(I0, F0),
        constrain_inst_vars_in_inst(InstConstraints, I0, I),
        constrain_inst_vars_in_inst(InstConstraints, F0, F),
        Mode = from_to_mode(I, F)
    ;
        Mode0 = user_defined_mode(Name, Args0),
        list.map(constrain_inst_vars_in_inst(InstConstraints), Args0, Args),
        Mode = user_defined_mode(Name, Args)
    ).

:- pred constrain_inst_vars_in_inst(inst_var_sub::in,
    mer_inst::in, mer_inst::out) is det.

constrain_inst_vars_in_inst(InstConstraints, Inst0, Inst) :-
    (
        ( Inst0 = not_reached
        ; Inst0 = free
        ; Inst0 = ground(_Uniq, none_or_default_func)
        ; Inst0 = any(_Uniq, none_or_default_func)
        ),
        Inst = Inst0
    ;
        Inst0 = ground(Uniq, higher_order(PredInstInfo0)),
        constrain_inst_vars_in_pred_inst_info(InstConstraints,
            PredInstInfo0, PredInstInfo),
        Inst = ground(Uniq, higher_order(PredInstInfo))
    ;
        Inst0 = any(Uniq, higher_order(PredInstInfo0)),
        constrain_inst_vars_in_pred_inst_info(InstConstraints,
            PredInstInfo0, PredInstInfo),
        Inst = any(Uniq, higher_order(PredInstInfo))
    ;
        Inst0 = bound(Uniq, InstResults0, BoundInsts0),
        (
            InstResults0 = inst_test_results_fgtc,
            % There are no inst_vars to substitute.
            Inst = Inst0
        ;
            InstResults0 = inst_test_results(_, _, _, InstVarsResult, _, _),
            ( if
                InstVarsResult =
                    inst_result_contains_inst_vars_known(InstVarsSet),
                set.to_sorted_list(InstVarsSet, InstVars),
                no_inst_var_is_in_map(InstVars, InstConstraints)
            then
                Inst = Inst0
            else
                list.map(constrain_inst_vars_in_bound_inst(InstConstraints),
                    BoundInsts0, BoundInsts),
                % The substitutions inside BoundInsts can invalidate
                % any of the existing results.
                % XXX can they?
                Inst = bound(Uniq, inst_test_no_results, BoundInsts)
            )
        ;
            InstResults0 = inst_test_no_results,
            list.map(constrain_inst_vars_in_bound_inst(InstConstraints),
                BoundInsts0, BoundInsts),
            % The substitutions inside BoundInsts can invalidate
            % any of the existing results.
            % XXX can they?
            Inst = bound(Uniq, inst_test_no_results, BoundInsts)
        )
    ;
        Inst0 = constrained_inst_vars(Vars0, SubInst0),
        constrain_inst_vars_in_inst(InstConstraints, SubInst0, SubInst1),
        ( if SubInst1 = constrained_inst_vars(SubVars, SubSubInst) then
            set.union(Vars0, SubVars, Vars),
            SubInst = SubSubInst
        else
            Vars = Vars0,
            SubInst = SubInst1
        ),
        Inst = constrained_inst_vars(Vars, SubInst)
    ;
        Inst0 = inst_var(Var),
        ( if map.search(InstConstraints, Var, SubInstPrime) then
            SubInst = SubInstPrime
        else
            SubInst = ground(shared, none_or_default_func)
        ),
        Inst = constrained_inst_vars(set.make_singleton_set(Var), SubInst)
    ;
        Inst0 = defined_inst(Name0),
        constrain_inst_vars_in_inst_name(InstConstraints, Name0, Name),
        Inst = defined_inst(Name)
    ).

:- pred constrain_inst_vars_in_bound_inst(inst_var_sub::in,
    bound_inst::in, bound_inst::out) is det.

constrain_inst_vars_in_bound_inst(InstConstraints, BoundInst0, BoundInst) :-
    BoundInst0 = bound_functor(ConsId, ArgInsts0),
    list.map(constrain_inst_vars_in_inst(InstConstraints),
        ArgInsts0, ArgInsts),
    BoundInst = bound_functor(ConsId, ArgInsts).

:- pred constrain_inst_vars_in_pred_inst_info(inst_var_sub::in,
    pred_inst_info::in, pred_inst_info::out) is det.

constrain_inst_vars_in_pred_inst_info(InstConstraints, PII0, PII) :-
    PII0 = pred_inst_info(PredOrFunc, Modes0, MaybeArgRegs, Det),
    list.map(constrain_inst_vars_in_mode_sub(InstConstraints), Modes0, Modes),
    PII = pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Det).

:- pred constrain_inst_vars_in_inst_name(inst_var_sub::in,
    inst_name::in, inst_name::out) is det.

constrain_inst_vars_in_inst_name(InstConstraints, Name0, Name) :-
    ( if Name0 = user_inst(SymName, Args0) then
        list.map(constrain_inst_vars_in_inst(InstConstraints), Args0, Args),
        Name = user_inst(SymName, Args)
    else
        Name = Name0
    ).

%---------------------------------------------------------------------------%

inconsistent_constrained_inst_vars_in_modes(Modes, InconsistentVars) :-
    list.foldl2(gather_inconsistent_constrained_inst_vars_in_mode,
        Modes, set.init, InconsistentVarsSet, map.init, _),
    set.to_sorted_list(InconsistentVarsSet, InconsistentVars).

inconsistent_constrained_inst_vars_in_type_and_modes(TypeAndModes,
        InconsistentVars) :-
    list.foldl2(gather_inconsistent_constrained_inst_vars_in_type_and_mode,
        TypeAndModes, set.init, InconsistentVarsSet, map.init, _),
    set.to_sorted_list(InconsistentVarsSet, InconsistentVars).

report_inconsistent_constrained_inst_vars(WhereDesc, Context, InstVarSet,
        InconsistentVars, MaybeSpec) :-
    (
        InconsistentVars = [],
        MaybeSpec = no
    ;
        InconsistentVars = [HeadInstVar | TailInstVars],
        (
            TailInstVars = [],
            varset.lookup_name(InstVarSet, HeadInstVar, HeadInstVarName),
            VarsPieces = [words("inst variable"), fixed(HeadInstVarName)]
        ;
            TailInstVars = [_ | _],
            list.map(varset.lookup_name(InstVarSet), InconsistentVars,
                InstVarNames),
            VarsPieces = [words("inst variables") |
                list_to_pieces(InstVarNames)]
        ),
        Pieces = [words("Error: inconsistent constraints on") | VarsPieces]
            ++ [words(WhereDesc), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeSpec = yes(Spec)
    ).

%---------------------%

:- pred gather_inconsistent_constrained_inst_vars_in_type_and_mode(
    type_and_mode::in, set(inst_var)::in, set(inst_var)::out,
    inst_var_sub::in, inst_var_sub::out) is det.

gather_inconsistent_constrained_inst_vars_in_type_and_mode(TypeAndMode,
        !InconsistentVars, !Sub) :-
    (
        TypeAndMode = type_only(_)
    ;
        TypeAndMode = type_and_mode(_, Mode),
        gather_inconsistent_constrained_inst_vars_in_mode(Mode,
            !InconsistentVars, !Sub)
    ).

:- pred gather_inconsistent_constrained_inst_vars_in_modes(list(mer_mode)::in,
    set(inst_var)::in, set(inst_var)::out,
    inst_var_sub::in, inst_var_sub::out) is det.

gather_inconsistent_constrained_inst_vars_in_modes(Modes,
        !InconsistentVars, !Sub) :-
    list.foldl2(gather_inconsistent_constrained_inst_vars_in_mode, Modes,
        !InconsistentVars, !Sub).

:- pred gather_inconsistent_constrained_inst_vars_in_mode(mer_mode::in,
    set(inst_var)::in, set(inst_var)::out,
    inst_var_sub::in, inst_var_sub::out) is det.

gather_inconsistent_constrained_inst_vars_in_mode(Mode,
        !InconsistentVars, !Sub) :-
    (
        Mode = from_to_mode(InitialInst, FinalInst),
        gather_inconsistent_constrained_inst_vars_in_inst(InitialInst,
            !InconsistentVars, !Sub),
        gather_inconsistent_constrained_inst_vars_in_inst(FinalInst,
            !InconsistentVars, !Sub)
    ;
        Mode = user_defined_mode(_, ArgInsts),
        gather_inconsistent_constrained_inst_vars_in_insts(ArgInsts,
            !InconsistentVars, !Sub)
    ).

:- pred gather_inconsistent_constrained_inst_vars_in_insts(list(mer_inst)::in,
    set(inst_var)::in, set(inst_var)::out,
    inst_var_sub::in, inst_var_sub::out) is det.

gather_inconsistent_constrained_inst_vars_in_insts(Insts,
        !InconsistentVars, !Sub) :-
    list.foldl2(gather_inconsistent_constrained_inst_vars_in_inst, Insts,
        !InconsistentVars, !Sub).

:- pred gather_inconsistent_constrained_inst_vars_in_inst(mer_inst::in,
    set(inst_var)::in, set(inst_var)::out,
    inst_var_sub::in, inst_var_sub::out) is det.

gather_inconsistent_constrained_inst_vars_in_inst(Inst,
        !InconsistentVars, !Sub) :-
    (
        ( Inst = free
        ; Inst = not_reached
        )
    ;
        Inst = bound(_, InstResults, BoundInsts),
        (
            InstResults = inst_test_results_fgtc
        ;
            InstResults = inst_test_results(_, _, _, InstVarsResult, _, _),
            ( if
                InstVarsResult =
                    inst_result_contains_inst_vars_known(InstVarsSet),
                set.is_empty(InstVarsSet)
            then
                true
            else
                list.foldl2(
                    gather_inconsistent_constrained_inst_vars_in_bound_args,
                    BoundInsts, !InconsistentVars, !Sub)
            )
        ;
            InstResults = inst_test_no_results,
            list.foldl2(
                gather_inconsistent_constrained_inst_vars_in_bound_args,
                BoundInsts, !InconsistentVars, !Sub)
        )
    ;
        ( Inst = ground(_, HOInstInfo)
        ; Inst = any(_, HOInstInfo)
        ),
        (
            HOInstInfo = none_or_default_func
        ;
            HOInstInfo = higher_order(pred_inst_info(_, Modes, _, _)),
            gather_inconsistent_constrained_inst_vars_in_modes(Modes,
                !InconsistentVars, !Sub)
        )
    ;
        Inst = inst_var(_),
        unexpected($pred, "unconstrained inst_var")
    ;
        Inst = defined_inst(InstName),
        ( if InstName = user_inst(_, ArgInsts) then
            gather_inconsistent_constrained_inst_vars_in_insts(ArgInsts,
                !InconsistentVars, !Sub)
        else
            true
        )
    ;
        Inst = constrained_inst_vars(InstVars, SubInst),
        set.fold2(
            gather_inconsistent_constrained_inst_vars_in_inst_var(SubInst),
            InstVars, !InconsistentVars, !Sub),
        gather_inconsistent_constrained_inst_vars_in_inst(SubInst,
            !InconsistentVars, !Sub)
    ).

:- pred gather_inconsistent_constrained_inst_vars_in_bound_args(
    bound_inst::in, set(inst_var)::in, set(inst_var)::out,
    inst_var_sub::in, inst_var_sub::out) is det.

gather_inconsistent_constrained_inst_vars_in_bound_args(BoundInst,
        !InconsistentVars, !Sub) :-
    BoundInst = bound_functor(_, ArgInsts),
    gather_inconsistent_constrained_inst_vars_in_insts(ArgInsts,
        !InconsistentVars, !Sub).

:- pred gather_inconsistent_constrained_inst_vars_in_inst_var(mer_inst::in,
    inst_var::in, set(inst_var)::in, set(inst_var)::out,
    inst_var_sub::in, inst_var_sub::out) is det.

gather_inconsistent_constrained_inst_vars_in_inst_var(SubInst, InstVar,
        !InconsistentVars, !Sub) :-
    ( if map.search(!.Sub, InstVar, InstVarInst) then
        % Check that the inst_var constraint is consistent with
        % the previous constraint on this inst_var.
        ( if InstVarInst = SubInst then
            true
        else
            set.insert(InstVar, !InconsistentVars)
        )
    else
        map.det_insert(InstVar, SubInst, !Sub)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_mode.
%---------------------------------------------------------------------------%
