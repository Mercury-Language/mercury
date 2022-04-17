%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: closure_analysis.m
% Main author: juliensf
%
% Perform local closure analysis on procedures. This involves tracking
% the possible values that a higher-order variable can take within a
% procedure. We attach this information to places where knowing the
% possible values of a higher-order call may be useful.
%
% This is similar to the analysis done by higher-order specialization, except
% that here, we do care if a higher-order variable can take multiple values.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.closure_analysis.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- pred closure_analyse_module(module_info::in, module_info::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.vartypes.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%----------------------------------------------------------------------------%

closure_analyse_module(!ModuleInfo) :-
    % XXX At the moment it is not necessary to do this on a per-SCC basis,
    % since the analysis is only procedure-local, but we would eventually
    % like to extend it.

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_closure, Debug),
    module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
    SCCs = dependency_info_get_bottom_up_sccs(DepInfo),
    list.foldl(closure_analyse_scc(Debug), SCCs, !ModuleInfo).

%----------------------------------------------------------------------------%
%
% Perform closure analysis on an SCC.
%

:- pred closure_analyse_scc(bool::in, scc::in,
    module_info::in, module_info::out) is det.

closure_analyse_scc(Debug, SCC, !ModuleInfo) :-
    set.foldl(closure_analyse_proc(Debug), SCC, !ModuleInfo).

%----------------------------------------------------------------------------%

    % This type represents the possible values of a higher-order valued
    % variable.
    %
:- type closure_values
    --->    unknown
            % The higher-order variable may be bound to something,
            % but we don't know what it is.

    ;       partial(set(pred_proc_id))
            % The higher-order variable may be bound to these values,
            % or it may be bound to something else we don't know about.
            % (This is intended to be useful in producing error messages
            % for the termination analysis; if one of the higher-order values
            % is definitely non-terminating, we can certainly let the user
            % know about it.)

    ;       exclusive(set(pred_proc_id)).
            % The higher-order variable can be bound only to one of the
            % procedures identified by this set.

    % We attach a closure_info to each goal where it may be of interest;
    % at the moment calls and generic_calls.
    %
:- type closure_info == map(prog_var, closure_values).

%----------------------------------------------------------------------------%

:- func closure_info_init(module_info, vartypes, prog_vars, list(mer_mode))
    = closure_info.

closure_info_init(ModuleInfo, VarTypes, HeadVars, ArgModes) = ClosureInfo :-
    partition_higher_order_arguments(ModuleInfo, VarTypes, HeadVars, ArgModes,
        set_of_var.init, Inputs0, set_of_var.init, _Outputs),
    Inputs = set_of_var.filter(var_has_ho_type(VarTypes), Inputs0),
    set_of_var.fold(insert_unknown, Inputs, map.init, ClosureInfo).

    % Succeeds iff the given variable has a higher-order type.
    %
:- pred var_has_ho_type(vartypes::in, prog_var::in) is semidet.

var_has_ho_type(VarTypes, Var) :-
    lookup_var_type(VarTypes, Var, Type),
    type_is_higher_order(Type).

    % Insert the given prog_var into the closure_info, and set the
    % possible values to unknown.
    %
:- pred insert_unknown(prog_var::in, closure_info::in, closure_info::out)
    is det.

insert_unknown(Var, !ClosureInfo) :-
    map.det_insert(Var, unknown, !ClosureInfo).

%----------------------------------------------------------------------------%
%
% Perform local closure analysis on a procedure.
%

:- pred closure_analyse_proc(bool::in, pred_proc_id::in,
    module_info::in, module_info::out) is det.

closure_analyse_proc(Debug, PPId, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
    proc_info_get_headvars(ProcInfo0, HeadVars),
    proc_info_get_varset_vartypes(ProcInfo0, VarSet, VarTypes),
    proc_info_get_argmodes(ProcInfo0, ArgModes),
    ClosureInfo0 = closure_info_init(!.ModuleInfo, VarTypes, HeadVars,
        ArgModes),
    trace [io(!TIO)] (
        write_proc_progress_message(!.ModuleInfo,
            "Analysing closures in", PPId, !TIO)
    ),
    proc_info_get_goal(ProcInfo0, BodyGoal0),
    closure_analyse_goal(!.ModuleInfo, VarTypes, BodyGoal0, BodyGoal,
        ClosureInfo0, _ClosureInfo),
    (
        Debug = yes,
        trace [io(!IO)] (
            get_debug_output_stream(!.ModuleInfo, DebugStream, !IO),
            dump_closure_info(DebugStream, VarSet, BodyGoal, !IO),
            io.flush_output(DebugStream, !IO)
        )
    ;
        Debug = no
    ),
    proc_info_set_goal(BodyGoal, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%
%
% Track higher-order values through goals.
%

:- pred closure_analyse_goal(module_info::in, vartypes::in,
    hlds_goal::in, hlds_goal::out, closure_info::in, closure_info::out) is det.

closure_analyse_goal(ModuleInfo, VarTypes, Goal0, Goal, !ClosureInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        list.map_foldl(closure_analyse_goal(ModuleInfo, VarTypes),
            Goals0, Goals, !ClosureInfo),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = plain_call(CallPredId, CallProcId, CallArgs, _, _, _),

        % Look for any higher-order arguments and divide them
        % into sets of input and output arguments.
        module_info_pred_proc_info(ModuleInfo, CallPredId, CallProcId,
            _CallPredInfo, CallProcInfo),
        proc_info_get_argmodes(CallProcInfo, CallArgModes),

        % NOTE: We construct sets of arguments, rather than lists,
        % in case there are duplicate arguments.
        partition_higher_order_arguments(ModuleInfo, VarTypes,
            CallArgs, CallArgModes,
            set_of_var.init, InputArgs, set_of_var.init, OutputArgs),

        % Update the goal_info to include any information about the
        % values of higher-order valued variables.
        set_of_var.fold(add_any_exclusive_ho_values(!.ClosureInfo), InputArgs,
            map.init, HoValueMap),
        goal_info_set_higher_order_value_map(HoValueMap, GoalInfo0, GoalInfo),

        % Insert any information about higher-order outputs from this call
        % into the closure_info.
        set_of_var.fold(insert_unknown, OutputArgs, !ClosureInfo),
        Goal = hlds_goal(GoalExpr0, GoalInfo)
    ;
        GoalExpr0 = generic_call(GenericDetails, GCallArgs, GCallModes, _, _),
        partition_higher_order_arguments(ModuleInfo, VarTypes,
            GCallArgs, GCallModes,
            set_of_var.init, InputArgs0, set_of_var.init, OutputArgs),

        % For higher-order calls we need to make sure that the actual
        % higher-order variable being called is also considered (it will
        % typically be the variable of interest). This variable is not included
        % in 'GCallArgs' so we need to include in the set of input argument
        % separately.
        (
            GenericDetails = higher_order(CalledClosure0, _, _, _),
            set_of_var.insert(CalledClosure0, InputArgs0, InputArgs)
        ;
            ( GenericDetails = class_method(_, _, _, _)
            ; GenericDetails = event_call(_)
            ; GenericDetails = cast(_)
            ),
            InputArgs = InputArgs0
        ),
        % The closure_info won't yet contain any information about
        % higher-order outputs from this call.
        set_of_var.fold(add_any_exclusive_ho_values(!.ClosureInfo), InputArgs,
            map.init, HoValueMap),
        goal_info_set_higher_order_value_map(HoValueMap, GoalInfo0, GoalInfo),

        % Insert any information about higher-order outputs from this call
        % into the closure_info.
        set_of_var.fold(insert_unknown, OutputArgs, !ClosureInfo),
        Goal = hlds_goal(GoalExpr0, GoalInfo)
    ;
        GoalExpr0 = switch(SwitchVar, SwitchCanFail, Cases0),
        closure_analyse_cases(ModuleInfo, VarTypes, Cases0, Cases,
            !.ClosureInfo, map.init, !:ClosureInfo),
        GoalExpr = switch(SwitchVar, SwitchCanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = unify(_, _, _, Unification, _),
        (
            Unification = construct(LHS, RHS, _, _, _, _, _),
            ( if
                RHS = closure_cons(ShroudedPPId, EvalMethod),
                EvalMethod = lambda_normal
            then
                PPId = unshroud_pred_proc_id(ShroudedPPId),
                HO_Value = set.make_singleton_set(PPId),
                map.det_insert(LHS, exclusive(HO_Value), !ClosureInfo)
            else
                true
            )
        ;
            Unification = deconstruct(_, _, Args, _, _, _),
            % XXX We don't currently support tracking the values of closures
            % that are stored in data structures.
            HO_Args = list.filter(var_has_ho_type(VarTypes), Args),
            list.foldl(insert_unknown, HO_Args, !ClosureInfo)
        ;
            Unification = assign(LHS, RHS),
            ( if var_has_ho_type(VarTypes, LHS) then
                % Sanity check: make sure the rhs is also a higher-order
                % variable.
                ( if var_has_ho_type(VarTypes, RHS) then
                    true
                else
                    unexpected($pred, "not a higher-order var")
                ),
                Values = map.lookup(!.ClosureInfo, RHS),
                map.det_insert(LHS, Values, !ClosureInfo)
            else
                true
            )
        ;
            Unification = simple_test(_, _)
        ;
            Unification = complicated_unify(_, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = disj(Goals0),
        closure_analyse_disjuncts(ModuleInfo, VarTypes, Goals0, Goals,
            !.ClosureInfo, map.init, !:ClosureInfo),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(NegatedGoal0),
        closure_analyse_goal(ModuleInfo, VarTypes, NegatedGoal0, NegatedGoal,
            !.ClosureInfo, _),
        GoalExpr = negation(NegatedGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            SubGoal = SubGoal0
        else
            closure_analyse_goal(ModuleInfo, VarTypes,
                SubGoal0, SubGoal, !ClosureInfo)
        ),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(ExistQVars, Cond0, Then0, Else0),
        closure_analyse_goal(ModuleInfo, VarTypes, Cond0, Cond,
            !.ClosureInfo, CondInfo),
        closure_analyse_goal(ModuleInfo, VarTypes, Then0, Then,
            CondInfo, CondThenInfo),
        closure_analyse_goal(ModuleInfo, VarTypes, Else0, Else,
            !.ClosureInfo, ElseInfo),
        map.union(merge_closure_values, CondThenInfo, ElseInfo, !:ClosureInfo),
        GoalExpr = if_then_else(ExistQVars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, Args, _ExtraArgs, _, _),
        % XXX We may eventually want to annotate foreign_procs with
        % clousure_infos as well. It isn't useful at the moment however.

        ForeignHOArgs =
            ( pred(Arg::in, Out::out) is semidet :-
                Arg = foreign_arg(Var, NameMode, Type, _BoxPolicy),

                % A 'no' here means that the foreign argument is unused.
                NameMode = yes(foreign_arg_name_mode(_, Mode)),
                mode_is_output(ModuleInfo, Mode),
                type_is_higher_order(Type),
                Out = Var - unknown
            ),
        list.filter_map(ForeignHOArgs, Args, OutputForeignHOArgs),
        map.det_insert_from_assoc_list(OutputForeignHOArgs, !ClosureInfo),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred closure_analyse_disjuncts(module_info::in, vartypes::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    closure_info::in, closure_info::in, closure_info::out) is det.

closure_analyse_disjuncts(_, _, [], [], _, !MergedDisjunctClosureInfo).
closure_analyse_disjuncts(ModuleInfo, VarTypes,
        [Disjunct0 | Disjuncts0], [Disjunct | Disjuncts],
        ClosureInfo0, !MergedDisjunctClosureInfo) :-
    closure_analyse_goal(ModuleInfo, VarTypes, Disjunct0, Disjunct,
        ClosureInfo0, DisjunctClosureInfo),
    merge_closure_infos(DisjunctClosureInfo, !MergedDisjunctClosureInfo),
    closure_analyse_disjuncts(ModuleInfo, VarTypes, Disjuncts0, Disjuncts,
        ClosureInfo0, !MergedDisjunctClosureInfo).

:- pred closure_analyse_cases(module_info::in, vartypes::in,
    list(case)::in, list(case)::out,
    closure_info::in, closure_info::in, closure_info::out) is det.

closure_analyse_cases(_, _, [], [], _, !MergedCaseClosureInfo).
closure_analyse_cases(ModuleInfo, VarTypes, [Case0 | Cases0], [Case | Cases],
        ClosureInfo0, !MergedCaseClosureInfo) :-
    closure_analyse_case(ModuleInfo, VarTypes, Case0, Case,
        ClosureInfo0, CaseClosureInfo),
    merge_closure_infos(CaseClosureInfo, !MergedCaseClosureInfo),
    closure_analyse_cases(ModuleInfo, VarTypes, Cases0, Cases,
        ClosureInfo0, !MergedCaseClosureInfo).

:- pred closure_analyse_case(module_info::in, vartypes::in,
    case::in, case::out, closure_info::in, closure_info::out) is det.

closure_analyse_case(ModuleInfo, VarTypes, Case0, Case,
        ClosureInfo0, CaseClosureInfo) :-
    Case0 = case(MainConsId, OtherConsIds, CaseGoal0),
    closure_analyse_goal(ModuleInfo, VarTypes, CaseGoal0, CaseGoal,
        ClosureInfo0, CaseClosureInfo),
    Case = case(MainConsId, OtherConsIds, CaseGoal).

:- pred add_any_exclusive_ho_values(closure_info::in, prog_var::in,
    higher_order_value_map::in, higher_order_value_map::out) is det.

add_any_exclusive_ho_values(ClosureInfo, Var, !HoValueMap) :-
    ( if map.search(ClosureInfo, Var, PossibleValues) then
        (
            PossibleValues = unknown
        ;
            PossibleValues = partial(_)
        ;
            PossibleValues = exclusive(KnownValues),
            map.det_insert(Var, KnownValues, !HoValueMap)
        )
    else
        true
    ).

%----------------------------------------------------------------------------%

:- pred partition_higher_order_arguments(module_info::in, vartypes::in,
    prog_vars::in, list(mer_mode)::in,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

partition_higher_order_arguments(_, _, [],    [], !Inputs, !Outputs).
partition_higher_order_arguments(_, _, [_ | _], [], _, _, _, _) :-
    unexpected($pred, "unequal length lists.").
partition_higher_order_arguments(_, _, [],    [_ | _], _, _, _, _) :-
    unexpected($pred, "unequal length lists.").
partition_higher_order_arguments(ModuleInfo, VarTypes,
        [Var | Vars], [Mode | Modes], !Inputs, !Outputs) :-
    ( if var_has_ho_type(VarTypes, Var) then
        ( if mode_is_input(ModuleInfo, Mode) then
            set_of_var.insert(Var, !Inputs)
        else if mode_is_output(ModuleInfo, Mode) then
            set_of_var.insert(Var, !Outputs)
        else
            true
        )
    else
        true
    ),
    partition_higher_order_arguments(ModuleInfo, VarTypes, Vars, Modes,
        !Inputs, !Outputs).

:- pred merge_closure_infos(closure_info::in, closure_info::in,
    closure_info::out) is det.

merge_closure_infos(A, B, C) :-
    map.union(merge_closure_values, A, B, C).

:- pred merge_closure_values(closure_values::in, closure_values::in,
    closure_values::out) is det.

merge_closure_values(unknown,      unknown,      unknown).
merge_closure_values(unknown,      partial(A),   partial(A)).
merge_closure_values(unknown,      exclusive(A), partial(A)).
merge_closure_values(partial(A),   unknown,      partial(A)).
merge_closure_values(partial(A),   partial(B),   partial(A `set.union` B)).
merge_closure_values(partial(A),   exclusive(B), partial(A `set.union` B)).
merge_closure_values(exclusive(A), unknown,      partial(A)).
merge_closure_values(exclusive(A), partial(B),   partial(A `set.union` B)).
merge_closure_values(exclusive(A), exclusive(B), exclusive(A `set.union` B)).

%----------------------------------------------------------------------------%
%
% Debugging code, used if the '--debug-closure' option is given.
%

:- pred dump_closure_info(io.text_output_stream::in, prog_varset::in,
    hlds_goal::in, io::di, io::uo) is det.

dump_closure_info(DebugStream, VarSet, Goal, !IO) :-
    % XXX zs: It seems to me that the output from this predicate
    % would be much easier to understand if each piece of the output
    % was preceded by the identity of the goal that it came from.
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = unify(_, _, _, _, _)
    ;
        GoalExpr = plain_call(_, _, _, _, _, _),
        dump_ho_values(DebugStream, VarSet, GoalInfo, !IO)
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        dump_ho_values(DebugStream, VarSet, GoalInfo, !IO)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr = conj(_ConjType, SubGoals),
        list.foldl(dump_closure_info(DebugStream, VarSet), SubGoals, !IO)
    ;
        GoalExpr = disj(SubGoals),
        list.foldl(dump_closure_info(DebugStream, VarSet), SubGoals, !IO)
    ;
        GoalExpr = switch(_, _, Cases),
        SubGoals = list.map((func(case(_, _, CaseGoal)) = CaseGoal), Cases),
        list.foldl(dump_closure_info(DebugStream, VarSet), SubGoals, !IO)
    ;
        GoalExpr = if_then_else(_, CondGoal, ThenGoal, ElseGoal),
        dump_closure_info(DebugStream, VarSet, CondGoal, !IO),
        dump_closure_info(DebugStream, VarSet, ThenGoal, !IO),
        dump_closure_info(DebugStream, VarSet, ElseGoal, !IO)
    ;
        GoalExpr = negation(SubGoal),
        dump_closure_info(DebugStream, VarSet, SubGoal, !IO)
    ;
        GoalExpr = scope(_, SubGoal),
        dump_closure_info(DebugStream, VarSet, SubGoal, !IO)
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred dump_ho_values(io.text_output_stream::in, prog_varset::in,
    hlds_goal_info::in, io::di, io::uo) is det.

dump_ho_values(DebugStream, VarSet, GoalInfo, !IO) :-
    HoValueMap = goal_info_get_higher_order_value_map(GoalInfo),
    ( if map.is_empty(HoValueMap) then
        true
    else
        Context = goal_info_get_context(GoalInfo),
        prog_out.write_context(DebugStream, Context, !IO),
        io.nl(DebugStream, !IO),
        map.foldl(dump_ho_value_map_entry(DebugStream, VarSet),
            HoValueMap, !IO)
    ).

:- pred dump_ho_value_map_entry(io.text_output_stream::in, prog_varset::in,
    prog_var::in, set(pred_proc_id)::in, io::di, io::uo) is det.

dump_ho_value_map_entry(DebugStream, VarSet, ProgVar, Values, !IO) :-
    VarName = varset.lookup_name(VarSet, ProgVar),
    io.format(DebugStream, "%s =\n", [s(VarName)], !IO),
    WritePPIds =
        ( pred(PPId::in, !.IO::di, !:IO::uo) is det :-
            io.write_string(DebugStream, "\t", !IO),
            io.write_line(DebugStream, PPId, !IO)
        ),
    set.fold(WritePPIds, Values, !IO).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.closure_analysis.
%----------------------------------------------------------------------------%
