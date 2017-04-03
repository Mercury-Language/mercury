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

:- import_module io.

:- pred closure_analyse_module(module_info::in, module_info::out,
    io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module hlds.vartypes.
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

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%----------------------------------------------------------------------------%

closure_analyse_module(!ModuleInfo, !IO) :-
    % XXX At the moment it is not necessary to do this on a per-SCC basis,
    % since the analysis is only procedure-local, but we would eventually
    % like to extend it.

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_closure, Debug),
    module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
    SCCs = dependency_info_get_bottom_up_sccs(DepInfo),
    list.foldl2(closure_analyse_scc(Debug), SCCs, !ModuleInfo, !IO).

%----------------------------------------------------------------------------%
%
% Perform closure analysis on an SCC.
%

:- pred closure_analyse_scc(bool::in, scc::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

closure_analyse_scc(Debug, SCC, !ModuleInfo, !IO) :-
    set.foldl2(closure_analyse_proc(Debug), SCC, !ModuleInfo, !IO).

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
    partition_arguments(ModuleInfo, VarTypes, HeadVars, ArgModes,
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
    module_info::in, module_info::out, io::di, io::uo) is det.

closure_analyse_proc(Debug, PPId, !ModuleInfo, !IO) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
    proc_info_get_headvars(ProcInfo0, HeadVars),
    proc_info_get_vartypes(ProcInfo0, VarTypes),
    proc_info_get_argmodes(ProcInfo0, ArgModes),
    ClosureInfo0 = closure_info_init(!.ModuleInfo, VarTypes, HeadVars,
        ArgModes),
    write_proc_progress_message("% Analysing closures in ", PPId, !.ModuleInfo,
        !IO),
    proc_info_get_goal(ProcInfo0, Body0),
    closure_analyse_goal(VarTypes, !.ModuleInfo, Body0, Body,
        ClosureInfo0, _ClosureInfo),
    (
        Debug = yes,
        proc_info_get_varset(ProcInfo, Varset),
        dump_closure_info(Varset, Body, !IO),
        io.flush_output(!IO)
    ;
        Debug = no
    ),
    proc_info_set_goal(Body, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%
%
% Track higher-order values through goals.
%

:- pred closure_analyse_goal(vartypes::in, module_info::in,
    hlds_goal::in, hlds_goal::out, closure_info::in, closure_info::out) is det.

closure_analyse_goal(VarTypes, ModuleInfo, Goal0, Goal, !ClosureInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        list.map_foldl(closure_analyse_goal(VarTypes, ModuleInfo),
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

        partition_arguments(ModuleInfo, VarTypes, CallArgs, CallArgModes,
            set_of_var.init, InputArgs, set_of_var.init, OutputArgs),

        % Update the goal_info to include any information about the
        % values of higher-order valued variables.

        AddValues = (pred(Var::in, !.ValueMap::in, !:ValueMap::out) is det :-
            % The closure_info won't yet contain any information about
            % higher-order outputs from this call.

            ( if map.search(!.ClosureInfo, Var, PossibleValues) then
                (
                    PossibleValues = unknown
                ;
                    PossibleValues = partial(_)
                ;
                    PossibleValues = exclusive(KnownValues),
                    map.det_insert(Var, KnownValues, !ValueMap)
                )
            else
                true
            )
        ),
        set_of_var.fold(AddValues, InputArgs, map.init, Values),
        goal_info_set_ho_values(Values, GoalInfo0, GoalInfo),

        % Insert any information about higher-order outputs from this call
        % into the closure_info.
        set_of_var.fold(insert_unknown, OutputArgs, !ClosureInfo),
        Goal = hlds_goal(GoalExpr0, GoalInfo)
    ;
        GoalExpr0 = generic_call(Details, GCallArgs, GCallModes, _, _),
        partition_arguments(ModuleInfo, VarTypes, GCallArgs, GCallModes,
            set_of_var.init, InputArgs0, set_of_var.init, OutputArgs),

        % For higher-order calls we need to make sure that the actual
        % higher-order variable being called is also considered (it will
        % typically be the variable of interest). This variable is not included
        % in 'GCallArgs' so we need to include in the set of input argument
        % separately.

        ( if Details = higher_order(CalledClosure0, _, _, _) then
            set_of_var.insert(CalledClosure0, InputArgs0, InputArgs)
        else
            InputArgs = InputArgs0
        ),
        AddValues = (pred(Var::in, !.ValueMap::in, !:ValueMap::out) is det :-
            % The closure_info won't yet contain any information about
            % higher-order outputs from this call.

            ( if map.search(!.ClosureInfo, Var, PossibleValues) then
                (
                    PossibleValues = unknown
                ;
                    PossibleValues = partial(_)
                ;
                    PossibleValues = exclusive(KnownValues),
                    map.det_insert(Var, KnownValues, !ValueMap)
                )
            else
                true
            )
        ),
        set_of_var.fold(AddValues, InputArgs, map.init, Values),
        goal_info_set_ho_values(Values, GoalInfo0, GoalInfo),

        % Insert any information about higher-order outputs from this call
        % into the closure_info.
        set_of_var.fold(insert_unknown, OutputArgs, !ClosureInfo),
        Goal = hlds_goal(GoalExpr0, GoalInfo)
    ;
        GoalExpr0 = switch(SwitchVar, SwitchCanFail, Cases0),
        ProcessCase = (func(Case0) = Case - CaseInfo :-
            Case0 = case(MainConsId, OtherConsIds, CaseGoal0),
            closure_analyse_goal(VarTypes, ModuleInfo, CaseGoal0, CaseGoal,
              !.ClosureInfo, CaseInfo),
            Case = case(MainConsId, OtherConsIds, CaseGoal)
        ),
        CasesAndInfos = list.map(ProcessCase, Cases0),
        assoc_list.keys_and_values(CasesAndInfos, Cases, CasesInfo),
        list.foldl(merge_closure_infos, CasesInfo, map.init, !:ClosureInfo),
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
                    unexpected($module, $pred, "not a higher-order var")
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
        ProcessDisjunct = (func(Disjunct0) = DisjunctResult :-
            closure_analyse_goal(VarTypes, ModuleInfo, Disjunct0, Disjunct,
                !.ClosureInfo, ClosureInfoForDisjunct),
            DisjunctResult = Disjunct - ClosureInfoForDisjunct
        ),
        DisjunctsAndInfos = list.map(ProcessDisjunct, Goals0),
        assoc_list.keys_and_values(DisjunctsAndInfos, Goals, DisjunctsInfo),
        list.foldl(merge_closure_infos, DisjunctsInfo,
            map.init, !:ClosureInfo),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(NegatedGoal0),
        closure_analyse_goal(VarTypes, ModuleInfo, NegatedGoal0, NegatedGoal,
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
            closure_analyse_goal(VarTypes, ModuleInfo,
                SubGoal0, SubGoal, !ClosureInfo)
        ),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(ExistQVars, Cond0, Then0, Else0),
        closure_analyse_goal(VarTypes, ModuleInfo, Cond0, Cond,
            !.ClosureInfo, CondInfo),
        closure_analyse_goal(VarTypes, ModuleInfo, Then0, Then,
            CondInfo, CondThenInfo),
        closure_analyse_goal(VarTypes, ModuleInfo, Else0, Else,
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
                mode_util.mode_is_output(ModuleInfo, Mode),
                type_is_higher_order(Type),
                Out = Var - unknown
            ),
        list.filter_map(ForeignHOArgs, Args, OutputForeignHOArgs),
        map.det_insert_from_assoc_list(OutputForeignHOArgs, !ClosureInfo),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(_),
        unexpected($module, $pred, "shorthand")
    ).

%----------------------------------------------------------------------------%

:- pred partition_arguments(module_info::in, vartypes::in,
    prog_vars::in, list(mer_mode)::in,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

partition_arguments(_, _, [],    [], !Inputs, !Outputs).
partition_arguments(_, _, [_|_], [], _, _, _, _) :-
    unexpected($module, $pred, "unequal length lists.").
partition_arguments(_, _, [],    [_|_], _, _, _, _) :-
    unexpected($module, $pred, "unequal length lists.").
partition_arguments(ModuleInfo, VarTypes, [ Var | Vars ], [ Mode | Modes ],
        !Inputs, !Outputs) :-
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
    partition_arguments(ModuleInfo, VarTypes, Vars, Modes, !Inputs, !Outputs).

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

:- pred dump_closure_info(prog_varset::in, hlds_goal::in,
    io::di, io::uo) is det.

dump_closure_info(Varset, Goal, !IO) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    dump_closure_info_expr(Varset, GoalExpr, GoalInfo, !IO).

:- pred dump_closure_info_expr(prog_varset::in, hlds_goal_expr::in,
    hlds_goal_info::in, io::di, io::uo) is det.

dump_closure_info_expr(Varset, conj(_ConjType, Goals), _, !IO) :-
    list.foldl(dump_closure_info(Varset), Goals, !IO).
dump_closure_info_expr(Varset, plain_call(_,_,_,_,_,_), GoalInfo, !IO) :-
    dump_ho_values(GoalInfo, Varset, !IO).
dump_closure_info_expr(Varset, generic_call(_,_,_,_,_), GoalInfo, !IO) :-
    dump_ho_values(GoalInfo, Varset, !IO).
dump_closure_info_expr(Varset, scope(_, Goal), _, !IO) :-
    dump_closure_info(Varset, Goal, !IO).
dump_closure_info_expr(Varset, switch(_, _, Cases), _, !IO) :-
    CaseToGoal = (func(case(_, _, Goal)) = Goal),
    Goals = list.map(CaseToGoal, Cases),
    list.foldl(dump_closure_info(Varset), Goals, !IO).
dump_closure_info_expr(Varset, if_then_else(_, Cond, Then, Else), _, !IO) :-
    list.foldl(dump_closure_info(Varset), [Cond, Then, Else], !IO).
dump_closure_info_expr(_, unify(_,_,_,_,_), _, !IO).
dump_closure_info_expr(Varset, negation(Goal), _, !IO) :-
    dump_closure_info(Varset, Goal, !IO).
dump_closure_info_expr(_, call_foreign_proc(_, _, _, _, _, _, _), _, !IO).
dump_closure_info_expr(Varset, disj(Goals), _, !IO) :-
    list.foldl(dump_closure_info(Varset), Goals, !IO).
dump_closure_info_expr(_, shorthand(_), _, _, _) :-
    unexpected($module, $pred, "shorthand").

:- pred dump_ho_values(hlds_goal_info::in, prog_varset::in,
    io::di, io::uo) is det.

dump_ho_values(GoalInfo, Varset, !IO) :-
    HO_Values = goal_info_get_ho_values(GoalInfo),
    ( if map.is_empty(HO_Values) then
        true
    else
        prog_out.write_context(goal_info_get_context(GoalInfo), !IO),
        io.nl(!IO),
        map.foldl(dump_ho_value(Varset), HO_Values, !IO)
    ).

:- pred dump_ho_value(prog_varset::in, prog_var::in, set(pred_proc_id)::in,
    io::di, io::uo) is det.

dump_ho_value(Varset, ProgVar, Values, !IO) :-
    VarName = varset.lookup_name(Varset, ProgVar),
    io.format("%s =\n", [s(VarName)], !IO),
    WritePPIds = (pred(PPId::in, !.IO::di, !:IO::uo) is det :-
        io.write_string("\t", !IO),
        io.write(PPId, !IO),
        io.nl(!IO)
    ),
    set.fold(WritePPIds, Values, !IO).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.closure_analysis.
%----------------------------------------------------------------------------%
