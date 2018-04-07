%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: untupling.m.
% Author: wangp.
%
% This module takes the HLDS and transforms the locally-defined procedures as
% follows. If a formal parameter of a procedure has a type that has only
% a single function symbol (i.e. it is a kind of tuple), then it replaces
% the parameter holding the tuple with one parameter for each field of the
% tuple. If some of those fields also have types that have only one function
% symbol, it expand them as well. It recurses until it has expanded the
% argument list as deeply as possible, yielding a parameter list that has
% *no* arguments of such tuple-like types. We call such parameter lists "flat",
% because they have no expandable structure (at least no structure that is
% expandable by this module).
%
% For example, for the following predicate and types,
%
%   :- type t ---> t(u).
%   :- type u ---> u(v, w).
%   :- type v ---> v1 ; v2.
%   :- type w ---> w(int, string).
%
%   :- pred f(t::in) is det.
%   f(T) :- blah.
%
% we would generate this transformed version of f/1:
%
%   :- pred f_untupled(v::in, int::in, string::in) is det.
%   f_untupled(V, W1, W2) :- blah.
%
% The first pass creates transformed versions for all the procedures
% whose argument lists weren't already flat already.
% The second pass then replaces all the calls in the module which refer
% to the old procedures with calls to their transformed versions.
% It does this by adding deconstruction and construction unifications
% as needed, which can later be simplified by a simplification pass.
% (This module does not itself invoke simplification, because we expect that
% the HLDS it generates will be subject to further optimization passes;
% simplification *will* be called by the target backend before it starts
% code generation.)
%
% For example, we transform this code, which calls the predicate above,
%
%   :- pred g(T::in) is det.
%   g(_) :-
%       A = 1,
%       B = "foo",
%       C = w(A, B),
%       D = v1,
%       E = u(D, C),
%       F = t(E),
%       f(F).
%
% to this:
%
%   g(_) :-
%       A = 1,
%       B = "foo",
%       C = w(A, B),
%       D = v1,
%       E = u(D, C),
%       F = t(E),
%       F = t(G),   % added deconstructions
%       G = u(H, I),
%       I = w(J, K),
%       f_untupled(H, J, K).
%
% which, after simplification, should become:
%
%   g(_) :-
%       A = 1,
%       B = "foo",
%       D = v1,
%       f_untupled(D, A, B).
%
% Limitations:
%
% - When a formal parameter is expanded, both the parameter's type and mode
%   have to be expanded. Currently only arguments with in and out modes can
%   be expanded, as I don't know how to do it for the general case.
%   It should be enough for the majority of code.
%
% - Some predicates may or may not be expandable but won't be right now,
%   because I don't understand the features they use (see expand_args_in_pred
%   below).
%
% Julien says: "it should be possible for this transformation to work across
% module boundaries by exporting the goal templates [search for CallAux
% below] in the `.opt' files."
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.untupling.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module io.

:- pred untuple_arguments(module_info::in, module_info::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_goal.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % The transform_map structure records which procedures were
    % transformed into what procedures during the first pass.
    %
:- type transform_map == map(pred_proc_id, transformed_proc).

:- type transformed_proc
    --->    transformed_proc(
                % A procedure that was generated by the untupling
                % transformation.
                pred_proc_id,

                % A call goal template that is used to update calls
                % referring to the old procedure to the new procedure.
                hlds_goal
            ).

untuple_arguments(!ModuleInfo, !IO) :-
    expand_args_in_module(!ModuleInfo, TransformMap),
    fix_calls_to_expanded_procs(TransformMap, !ModuleInfo).

%-----------------------------------------------------------------------------%
%
% Pass 1.
%

    % This is the top level of the first pass. It expands procedure arguments
    % where possible, adding new versions of the transformed procedures
    % into the module and recording the mapping between the old and new
    % procedures in the transform map.
    %
:- pred expand_args_in_module(module_info::in, module_info::out,
    transform_map::out) is det.

expand_args_in_module(!ModuleInfo, TransformMap) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl3(expand_args_in_pred, PredIds,
        !ModuleInfo, map.init, TransformMap, counter.init(0), _).

:- pred expand_args_in_pred(pred_id::in, module_info::in, module_info::out,
    transform_map::in, transform_map::out, counter::in, counter::out) is det.

expand_args_in_pred(PredId, !ModuleInfo, !TransformMap, !Counter) :-
    module_info_get_type_table(!.ModuleInfo, TypeTable),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ( if
        % Only perform the transformation on predicates which
        % satisfy the following criteria.
        pred_info_get_status(PredInfo, PredStatus),
        pred_status_defined_in_this_module(PredStatus) = yes,
        pred_info_get_goal_type(PredInfo, goal_type_clause),
        % Some of these limitations may be able to be lifted later.
        % For now, take the safe option and don't touch them.
        pred_info_get_exist_quant_tvars(PredInfo, []),
        pred_info_get_external_type_params(PredInfo, []),
        pred_info_get_class_context(PredInfo, constraints([], [])),
        pred_info_get_origin(PredInfo, origin_user(_)),
        pred_info_get_arg_types(PredInfo, TypeVarSet, ExistQVars, ArgTypes),
        varset.is_empty(TypeVarSet),
        ExistQVars = [],
        at_least_one_expandable_type(ArgTypes, TypeTable)
    then
        ProcIds = pred_info_non_imported_procids(PredInfo),
        list.foldl3(expand_args_in_proc(PredId), ProcIds,
            !ModuleInfo, !TransformMap, !Counter)
    else
        true
    ).

:- pred at_least_one_expandable_type(list(mer_type)::in, type_table::in)
    is semidet.

at_least_one_expandable_type([Type | Types], TypeTable) :-
    ( expand_type(Type, [], TypeTable, expansion(_, _))
    ; at_least_one_expandable_type(Types, TypeTable)
    ).

%-----------------------------------------------------------------------------%

    % This structure records the mapping between a head variable of the
    % original procedure, and the list of variables that it was finally
    % expanded into. If the head variable expands into some intermediate
    % variables which are then expanded further, the intermediate
    % variables are not listed in the mapping.
    %
:- type untuple_map == map(prog_var, prog_vars).

:- pred expand_args_in_proc(pred_id::in, proc_id::in, module_info::in,
    module_info::out, transform_map::in, transform_map::out,
    counter::in, counter::out) is det.

expand_args_in_proc(PredId, ProcId, !ModuleInfo, !TransformMap, !Counter) :-
    some [!ProcInfo] (
        module_info_get_type_table(!.ModuleInfo, TypeTable),
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            PredInfo0, !:ProcInfo),

        proc_info_get_headvars(!.ProcInfo, HeadVars0),
        proc_info_get_argmodes(!.ProcInfo, ArgModes0),
        proc_info_get_goal(!.ProcInfo, Goal0),
        proc_info_get_vartypes(!.ProcInfo, VarTypes0),
        proc_info_get_varset(!.ProcInfo, VarSet0),

        expand_args_in_proc_2(HeadVars0, ArgModes0, HeadVars, ArgModes,
            Goal0, Goal, VarSet0, VarSet, VarTypes0, VarTypes,
            TypeTable, UntupleMap),

        proc_info_set_headvars(HeadVars, !ProcInfo),
        proc_info_set_argmodes(ArgModes, !ProcInfo),
        proc_info_set_goal(Goal, !ProcInfo),
        proc_info_set_varset(VarSet, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),
        requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
        recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
            !ProcInfo, !ModuleInfo),

        counter.allocate(Num, !Counter),
        create_aux_pred(PredId, ProcId, PredInfo0, !.ProcInfo, Num,
            AuxPredId, AuxProcId, CallAux,
            AuxPredInfo, AuxProcInfo0, !ModuleInfo),
        proc_info_set_maybe_untuple_info(
            yes(untuple_proc_info(UntupleMap)),
            AuxProcInfo0, AuxProcInfo),
        module_info_set_pred_proc_info(AuxPredId, AuxProcId,
            AuxPredInfo, AuxProcInfo, !ModuleInfo),
        map.det_insert(proc(PredId, ProcId),
            transformed_proc(proc(AuxPredId, AuxProcId), CallAux),
            !TransformMap)
    ).

:- pred expand_args_in_proc_2(prog_vars::in, list(mer_mode)::in,
    prog_vars::out, list(mer_mode)::out, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    type_table::in, untuple_map::out) is det.

expand_args_in_proc_2(HeadVars0, ArgModes0, HeadVars, ArgModes,
        Goal0, hlds_goal(GoalExpr, GoalInfo), !VarSet, !VarTypes, TypeTable,
        UntupleMap) :-
    expand_args_in_proc_3(HeadVars0, ArgModes0, ListOfHeadVars,
        ListOfArgModes, Goal0, hlds_goal(GoalExpr, GoalInfo1), !VarSet,
        !VarTypes, [], TypeTable),
    Context = goal_info_get_context(Goal0 ^ hg_info),
    goal_info_set_context(Context, GoalInfo1, GoalInfo),
    list.condense(ListOfHeadVars, HeadVars),
    list.condense(ListOfArgModes, ArgModes),
    build_untuple_map(HeadVars0, ListOfHeadVars, map.init, UntupleMap).

:- pred expand_args_in_proc_3(list(prog_var)::in, list(mer_mode)::in,
    list(list(prog_var))::out, list(list(mer_mode))::out,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, list(mer_type)::in, type_table::in) is det.

expand_args_in_proc_3([], [], [], [], !_, !_, !_, _, _).
expand_args_in_proc_3([], [_|_], _, _, !_, !_, !_, _, _) :-
    unexpected($pred, "length mismatch").
expand_args_in_proc_3([_|_], [], _, _, !_, !_, !_, _, _) :-
    unexpected($pred, "length mismatch").
expand_args_in_proc_3([HeadVar0 | HeadVars0], [ArgMode0 | ArgModes0],
        [HeadVar | HeadVars], [ArgMode | ArgModes],
        !Goal, !VarSet, !VarTypes, ContainerTypes, TypeTable) :-
    expand_one_arg_in_proc(HeadVar0, ArgMode0, HeadVar, ArgMode,
        !Goal, !VarSet, !VarTypes, ContainerTypes, TypeTable),
    expand_args_in_proc_3(HeadVars0, ArgModes0, HeadVars, ArgModes,
        !Goal, !VarSet, !VarTypes, ContainerTypes, TypeTable).

:- pred expand_one_arg_in_proc(prog_var::in, mer_mode::in, prog_vars::out,
    list(mer_mode)::out, hlds_goal::in, hlds_goal::out, prog_varset::in,
    prog_varset::out, vartypes::in, vartypes::out, list(mer_type)::in,
    type_table::in) is det.

expand_one_arg_in_proc(HeadVar0, ArgMode0, HeadVars, ArgModes,
        !Goal, !VarSet, !VarTypes, ContainerTypes0, TypeTable) :-
    expand_one_arg_in_proc_2(HeadVar0, ArgMode0, MaybeHeadVarsAndArgModes,
        !Goal, !VarSet, !VarTypes, ContainerTypes0, ContainerTypes, TypeTable),
    (
        MaybeHeadVarsAndArgModes = yes(HeadVars1 - ArgModes1),
        expand_args_in_proc_3(HeadVars1, ArgModes1,
            ListOfHeadVars, ListOfArgModes, !Goal, !VarSet, !VarTypes,
            ContainerTypes, TypeTable),
        HeadVars = list.condense(ListOfHeadVars),
        ArgModes = list.condense(ListOfArgModes)
    ;
        MaybeHeadVarsAndArgModes = no,
        HeadVars = [HeadVar0],
        ArgModes = [ArgMode0]
    ).

:- pred expand_one_arg_in_proc_2(prog_var::in, mer_mode::in,
    maybe(pair(list(prog_var), list(mer_mode)))::out,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, list(mer_type)::in, list(mer_type)::out,
    type_table::in) is det.

expand_one_arg_in_proc_2(HeadVar0, ArgMode0, MaybeHeadVarsAndArgModes,
        !Goal, !VarSet, !VarTypes, ContainerTypes0, ContainerTypes,
        TypeTable) :-
    lookup_var_type(!.VarTypes, HeadVar0, Type),
    expand_argument(ArgMode0, Type, ContainerTypes0, TypeTable, Expansion),
    (
        Expansion = expansion(ConsId, NewTypes),
        varset.lookup_name(!.VarSet, HeadVar0, ParentName),
        create_untuple_vars(ParentName, 0, NewTypes, NewHeadVars,
            !VarSet, !VarTypes),
        list.duplicate(list.length(NewHeadVars), ArgMode0, NewArgModes),
        MaybeHeadVarsAndArgModes = yes(NewHeadVars - NewArgModes),
        ( if ArgMode0 = in_mode then
            construct_functor(HeadVar0, ConsId, NewHeadVars, UnifGoal),
            conjoin_goals_keep_detism(UnifGoal, !Goal)
        else if ArgMode0 = out_mode then
            deconstruct_functor(HeadVar0, ConsId, NewHeadVars, UnifGoal),
            conjoin_goals_keep_detism(!.Goal, UnifGoal, !:Goal)
        else
            unexpected($pred, "unsupported mode")
        ),
        ContainerTypes = [Type | ContainerTypes0]
    ;
        Expansion = no_expansion,
        MaybeHeadVarsAndArgModes = no,
        ContainerTypes = ContainerTypes0
    ).

:- pred create_untuple_vars(string::in, int::in, list(mer_type)::in,
    list(prog_var)::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

create_untuple_vars(_, _, [], [], !VarSet, !VarTypes).
create_untuple_vars(ParentName, Num, [Type | Types], [NewVar | NewVars],
        !VarSet, !VarTypes) :-
    string.format("Untupled_%s_%d", [s(ParentName), i(Num)], Name),
    varset.new_named_var(Name, NewVar, !VarSet),
    add_var_type(NewVar, Type, !VarTypes),
    create_untuple_vars(ParentName, Num+1, Types, NewVars, !VarSet, !VarTypes).

:- pred conjoin_goals_keep_detism(hlds_goal::in, hlds_goal::in,
    hlds_goal::out) is det.

conjoin_goals_keep_detism(GoalA, GoalB, Goal) :-
    goal_to_conj_list(GoalA, GoalListA),
    goal_to_conj_list(GoalB, GoalListB),
    list.append(GoalListA, GoalListB, GoalList),
    goal_list_determinism(GoalList, Determinism),
    goal_info_init(GoalInfo0),
    goal_info_set_determinism(Determinism, GoalInfo0, GoalInfo),
    Goal = hlds_goal(conj(plain_conj, GoalList), GoalInfo).

:- pred build_untuple_map(list(prog_var)::in, list(list(prog_var))::in,
    untuple_map::in, untuple_map::out) is det.

build_untuple_map([], [], !UntupleMap).
build_untuple_map([], [_| _], !_) :-
    unexpected($pred, "length mismatch").
build_untuple_map([_| _], [], !_) :-
    unexpected($pred, "length mismatch").
build_untuple_map([OldVar | OldVars], [NewVars | NewVarss], !UntupleMap) :-
    ( if NewVars = [OldVar] then
        build_untuple_map(OldVars, NewVarss, !UntupleMap)
    else
        map.det_insert(OldVar, NewVars, !UntupleMap),
        build_untuple_map(OldVars, NewVarss, !UntupleMap)
    ).

%-----------------------------------------------------------------------------%

    % This predicate makes a new version of the given procedure in a module.
    % Amongst other things the new procedure is given a new pred_id and
    % proc_id, a new name and a new goal.
    %
    % CallAux is an output variable, which is unified with a goal that
    % can be used as a template for constructing calls to the newly
    % created procedure.
    %
    % See also create_aux_pred in loop_inv.m.
    %
:- pred create_aux_pred(pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, int::in, pred_id::out, proc_id::out, hlds_goal::out,
    pred_info::out, proc_info::out, module_info::in, module_info::out) is det.

create_aux_pred(PredId, ProcId, PredInfo, ProcInfo, Counter,
        AuxPredId, AuxProcId, CallAux, AuxPredInfo, AuxProcInfo,
        !ModuleInfo) :-
    proc_info_get_headvars(ProcInfo, AuxHeadVars),
    proc_info_get_goal(ProcInfo, Goal @ hlds_goal(_GoalExpr, GoalInfo)),
    proc_info_get_initial_instmap(ProcInfo, !.ModuleInfo, InitialAuxInstMap),
    pred_info_get_typevarset(PredInfo, TVarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    pred_info_get_class_context(PredInfo, ClassContext),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_origin(PredInfo, OrigOrigin),
    proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
    pred_info_get_var_name_remap(PredInfo, VarNameRemap),

    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Context = goal_info_get_context(GoalInfo),
    term.context_line(Context, Line),
    make_pred_name_with_context(PredModule, "untupling",
        PredOrFunc, PredName, Line, Counter, AuxPredSymName0),
    proc_id_to_int(ProcId, ProcNo),
    Suffix = string.format("_%d", [i(ProcNo)]),
    add_sym_name_suffix(AuxPredSymName0, Suffix, AuxPredSymName),

    Origin = origin_transformed(transform_untuple(ProcNo), OrigOrigin, PredId),
    hlds_pred.define_new_pred(Origin, Goal, CallAux, AuxHeadVars, _ExtraArgs,
        InitialAuxInstMap, AuxPredSymName, TVarSet, VarTypes, ClassContext,
        RttiVarMaps, VarSet, InstVarSet, Markers, address_is_not_taken,
        HasParallelConj, VarNameRemap, !ModuleInfo, AuxPredProcId),
    AuxPredProcId = proc(AuxPredId, AuxProcId),

    module_info_pred_proc_info(!.ModuleInfo, AuxPredId, AuxProcId,
        AuxPredInfo, AuxProcInfo).

%-----------------------------------------------------------------------------%
%
% Pass 2.
%

    % This is the top level of the second pass. It takes the transform map
    % built during the first pass as input. For every call to a procedure
    % in the transform map, it rewrites the call to use the new procedure
    % instead, inserting unifications before and after the call as necessary.
    %
:- pred fix_calls_to_expanded_procs(transform_map::in, module_info::in,
    module_info::out) is det.

fix_calls_to_expanded_procs(TransformMap, !ModuleInfo) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl(fix_calls_in_pred(TransformMap), PredIds, !ModuleInfo).

:- pred fix_calls_in_pred(transform_map::in, pred_id::in, module_info::in,
    module_info::out) is det.

fix_calls_in_pred(TransformMap, PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    list.foldl(fix_calls_in_proc(TransformMap, PredId), ProcIds, !ModuleInfo).

:- pred fix_calls_in_proc(transform_map::in, pred_id::in, proc_id::in,
    module_info::in, module_info::out) is det.

fix_calls_in_proc(TransformMap, PredId, ProcId, !ModuleInfo) :-
    some [!ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            PredInfo, !:ProcInfo),
        proc_info_get_goal(!.ProcInfo, Goal0),
        proc_info_get_vartypes(!.ProcInfo, VarTypes0),
        proc_info_get_varset(!.ProcInfo, VarSet0),
        fix_calls_in_goal(Goal0, Goal, VarSet0, VarSet,
            VarTypes0, VarTypes, TransformMap, !.ModuleInfo),
        ( if Goal0 = Goal then
            true
        else
            proc_info_set_goal(Goal, !ProcInfo),
            proc_info_set_varset(VarSet, !ProcInfo),
            proc_info_set_vartypes(VarTypes, !ProcInfo),
            requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
            recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
                !ProcInfo, !ModuleInfo),
            module_info_set_pred_proc_info(PredId, ProcId,
                PredInfo, !.ProcInfo, !ModuleInfo)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred fix_calls_in_goal(hlds_goal::in, hlds_goal::out, prog_varset::in,
    prog_varset::out, vartypes::in, vartypes::out, transform_map::in,
    module_info::in) is det.

fix_calls_in_goal(Goal0, Goal, !VarSet, !VarTypes, TransformMap, ModuleInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = plain_call(CalleePredId, CalleeProcId, OrigArgs, _, _, _),
        ( if
            map.search(TransformMap, proc(CalleePredId, CalleeProcId),
                transformed_proc(_, hlds_goal(CallAux0, CallAuxInfo)))
        then
            module_info_get_type_table(ModuleInfo, TypeTable),
            module_info_pred_proc_info(ModuleInfo, CalleePredId,
                CalleeProcId, _CalleePredInfo, CalleeProcInfo),
            proc_info_get_argmodes(CalleeProcInfo, OrigArgModes),
            expand_call_args(OrigArgs, OrigArgModes, Args,
                EnterUnifs, ExitUnifs, !VarSet, !VarTypes, TypeTable),
            ( if CallAux = CallAux0 ^ call_args := Args then
                Call = hlds_goal(CallAux, CallAuxInfo),
                ConjList = EnterUnifs ++ [Call] ++ ExitUnifs,
                conj_list_to_goal(ConjList, GoalInfo0, Goal)
            else
                unexpected($pred, "not a call template")
            )
        else
            Goal = hlds_goal(GoalExpr0, GoalInfo0)
        )
    ;
        GoalExpr0 = negation(SubGoal0),
        fix_calls_in_goal(SubGoal0, SubGoal, !VarSet, !VarTypes, TransformMap,
            ModuleInfo),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % There are no calls in these scopes.
            Goal = Goal0
        else
            fix_calls_in_goal(SubGoal0, SubGoal, !VarSet, !VarTypes,
                TransformMap, ModuleInfo),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            fix_calls_in_conj(Goals0, Goals, !VarSet, !VarTypes, TransformMap,
                ModuleInfo)
        ;
            ConjType = parallel_conj,
            % I am not sure whether parallel conjunctions should be treated
            % with fix_calls_in_goal or fix_calls_in_goal_list. At any rate,
            % this is untested.
            fix_calls_in_goal_list(Goals0, Goals, !VarSet, !VarTypes,
                TransformMap, ModuleInfo)
        ),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        fix_calls_in_goal_list(Goals0, Goals, !VarSet, !VarTypes,
            TransformMap, ModuleInfo),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        fix_calls_in_cases(Cases0, Cases, !VarSet, !VarTypes, TransformMap,
            ModuleInfo),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        fix_calls_in_goal(Cond0, Cond, !VarSet, !VarTypes, TransformMap,
            ModuleInfo),
        fix_calls_in_goal(Then0, Then, !VarSet, !VarTypes, TransformMap,
            ModuleInfo),
        fix_calls_in_goal(Else0, Else, !VarSet, !VarTypes, TransformMap,
            ModuleInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%

:- pred fix_calls_in_conj(hlds_goals::in, hlds_goals::out, prog_varset::in,
    prog_varset::out, vartypes::in, vartypes::out, transform_map::in,
    module_info::in) is det.

fix_calls_in_conj([], [], !VarSet, !VarTypes, _, _).
fix_calls_in_conj([Goal0 | Goals0], Goals, !VarSet, !VarTypes, TransformMap,
        ModuleInfo) :-
    fix_calls_in_goal(Goal0, Goal1, !VarSet, !VarTypes, TransformMap,
        ModuleInfo),
    fix_calls_in_conj(Goals0, Goals1, !VarSet, !VarTypes, TransformMap,
        ModuleInfo),
    ( if Goal1 = hlds_goal(conj(plain_conj, ConjGoals), _) then
        Goals = ConjGoals ++ Goals1
    else
        Goals = [Goal1 | Goals1]
    ).

:- pred fix_calls_in_goal_list(hlds_goals::in, hlds_goals::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    transform_map::in, module_info::in) is det.

fix_calls_in_goal_list([], [], !VarSet, !VarTypes, _, _).
fix_calls_in_goal_list([Goal0 | Goals0], [Goal | Goals], !VarSet, !VarTypes,
        TransformMap, ModuleInfo) :-
    fix_calls_in_goal(Goal0, Goal, !VarSet, !VarTypes,
        TransformMap, ModuleInfo),
    fix_calls_in_goal_list(Goals0, Goals, !VarSet, !VarTypes,
        TransformMap, ModuleInfo).

:- pred fix_calls_in_cases(list(case)::in, list(case)::out, prog_varset::in,
    prog_varset::out, vartypes::in, vartypes::out, transform_map::in,
    module_info::in) is det.

fix_calls_in_cases([], [], !VarSet, !VarTypes, _, _).
fix_calls_in_cases([Case0 | Cases0], [Case | Cases], !VarSet, !VarTypes,
        TransformMap, ModuleInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    fix_calls_in_goal(Goal0, Goal, !VarSet, !VarTypes,
        TransformMap, ModuleInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    fix_calls_in_cases(Cases0, Cases, !VarSet, !VarTypes,
        TransformMap, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred expand_call_args(prog_vars::in, list(mer_mode)::in, prog_vars::out,
    hlds_goals::out, hlds_goals::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, type_table::in) is det.

expand_call_args(Args0, ArgModes0, Args, EnterUnifs, ExitUnifs,
        !VarSet, !VarTypes, TypeTable) :-
    expand_call_args_2(Args0, ArgModes0, Args, EnterUnifs, ExitUnifs,
        !VarSet, !VarTypes, [], TypeTable).

:- pred expand_call_args_2(prog_vars::in, list(mer_mode)::in, prog_vars::out,
    hlds_goals::out, hlds_goals::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, list(mer_type)::in, type_table::in) is det.

expand_call_args_2([], [], [], [], [], !VarSet, !VarTypes, _, _).
expand_call_args_2([], [_|_], _, _, _, !_, !_, _, _) :-
    unexpected($pred, "length mismatch").
expand_call_args_2([_|_], [], _, _, _, !_, !_, _, _) :-
    unexpected($pred, "length mismatch").
expand_call_args_2([Arg0 | Args0], [ArgMode | ArgModes], Args,
        EnterUnifs, ExitUnifs, !VarSet, !VarTypes,
        ContainerTypes0, TypeTable) :-
    lookup_var_type(!.VarTypes, Arg0, Arg0Type),
    expand_argument(ArgMode, Arg0Type, ContainerTypes0, TypeTable, Expansion),
    (
        Expansion = expansion(ConsId, Types),
        NumVars = list.length(Types),
        varset.new_vars(NumVars, ReplacementArgs, !VarSet),
        vartypes_add_corresponding_lists(ReplacementArgs, Types, !VarTypes),
        list.duplicate(NumVars, ArgMode, ReplacementModes),
        ContainerTypes = [Arg0Type | ContainerTypes0],
        ( if ArgMode = in_mode then
            deconstruct_functor(Arg0, ConsId, ReplacementArgs, Unif),
            EnterUnifs = [Unif | EnterUnifs1],
            expand_call_args_2(ReplacementArgs ++ Args0,
                ReplacementModes ++ ArgModes,
                Args, EnterUnifs1, ExitUnifs, !VarSet,
                !VarTypes, ContainerTypes, TypeTable)
        else if ArgMode = out_mode then
            construct_functor(Arg0, ConsId, ReplacementArgs, Unif),
            ExitUnifs = ExitUnifs1 ++ [Unif],
            expand_call_args_2(ReplacementArgs ++ Args0,
                ReplacementModes ++ ArgModes,
                Args, EnterUnifs, ExitUnifs1, !VarSet,
                !VarTypes, ContainerTypes, TypeTable)
        else
            unexpected($pred, "unsupported mode")
        )
    ;
        Expansion = no_expansion,
        Args = [Arg0 | Args1],
        expand_call_args(Args0, ArgModes, Args1, EnterUnifs,
            ExitUnifs, !VarSet, !VarTypes, TypeTable)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type expansion_result
    --->    expansion(
                % The cons_id of the expanded constructor.
                cons_id,

                % The types of the arguments for the expanded constructor.
                list(mer_type)
            )
    ;       no_expansion.

    % This predicate tries to expand the argument of the given mode and type.
    % If this is possible then Expansion is unified with the `expansion'
    % functor, giving the details of the expansion. Otherwise, it is
    % unified with `no_expansion'.
    %
:- pred expand_argument(mer_mode::in, mer_type::in, list(mer_type)::in,
    type_table::in, expansion_result::out) is det.

expand_argument(ArgMode, ArgType, ContainerTypes, TypeTable, Expansion) :-
    ( if expandable_arg_mode(ArgMode) then
        expand_type(ArgType, ContainerTypes, TypeTable, Expansion)
    else
        Expansion = no_expansion
    ).

    % This module so far only knows how to expand arguments which have
    % the following modes.
    %
:- pred expandable_arg_mode(mer_mode::in) is semidet.

expandable_arg_mode(in_mode).
expandable_arg_mode(out_mode).

:- pred expand_type(mer_type::in, list(mer_type)::in, type_table::in,
    expansion_result::out) is det.

expand_type(Type, ContainerTypes, TypeTable, Expansion) :-
    ( if
        % Always expand tuple types.
        type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
        type_ctor_is_tuple(TypeCtor)
    then
        Arity = list.length(TypeArgs),
        ConsId = tuple_cons(Arity),
        Expansion = expansion(ConsId, TypeArgs)
    else if
        % Expand a discriminated union type if it has only a
        % single functor and the type has no parameters.
        type_to_ctor_and_args(Type, TypeCtor, []),
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        get_type_defn_tparams(TypeDefn, []),
        get_type_defn_body(TypeDefn, TypeBody),
        TypeBody ^ du_type_ctors = [SingleCtor],
        SingleCtor ^ cons_maybe_exist = no_exist_constraints,
        SingleCtorName = SingleCtor ^ cons_name,
        SingleCtorArgs = SingleCtor ^ cons_args,
        SingleCtorArgs = [_ | _],
        % Prevent infinite loop with recursive types.
        not list.member(Type, ContainerTypes)
    then
        Arity = list.length(SingleCtorArgs),
        ConsId = cons(SingleCtorName, Arity, TypeCtor),
        ExpandedTypes = list.map(func(C) = C ^ arg_type, SingleCtorArgs),
        Expansion = expansion(ConsId, ExpandedTypes)
    else
        Expansion = no_expansion
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.untupling.
%-----------------------------------------------------------------------------%
