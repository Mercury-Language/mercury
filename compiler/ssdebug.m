%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: transform_hlds.ssdebug.m.
% Main authors: oannet.
%
% The ssdebug module does a source to source tranformation on each procedure
% which allows the procedure to be debugged.
%
% XXX Only user-made procedures are debugged, not yet the library procedures.
%
% Here is the transformation:
%
% original:
%
%    p(...) :-
%        <original body>
%
% model_det transformed:
%
%    p(...) :-
%        promise_<original_purity> (
%            CallVarDescs = [ ... ],
%            impure call_port(ProcId, CallVarDescs),
%            <original body>,    % renaming outputs
%            ExitVarDescs = [ ... | CallVarDescs ],
%            impure exit_port(ProcId, ExitVarDescs, DoRetry),
%            (
%                DoRetry = do_retry,
%                p(...)
%            ;
%                DoRetry = do_not_retry,
%                % bind outputs
%            )
%        ).
%
% model_semi transformed:
%
%    p(...) :-
%        promise_<original_purity> (
%            CallVarDescs = [ ... ],
%            (
%                impure call_port(ProcId, CallVarDescs),
%                <original body>    % renaming outputs
%            ->
%                ExitVarDescs = [ ... | CallVarDescs ],
%                impure exit_port(ProcId, ExitVarDescs, DoRetryA),
%                (
%                    DoRetryA = do_retry,
%                    p(...)
%                ;
%                    DoRetryA = do_not_retry,
%                    % bind outputs
%                )
%            ;
%                impure fail_port(ProcId, CallVarDescs, DoRetryB),
%                (
%                    DoRetryB = do_retry,
%                    p(...)
%                ;
%                    DoRetryB = do_not_retry,
%                    fail
%                )
%            )
%        ).
%
% model_non transformed:
%
%    p(...) :-
%        promise_<original_purity> (
%            (
%                CallVarDescs = [ ... ],
%                impure call_port(ProcId, CallVarDescs),
%                <original body>,
%                ExitVarDescs = [ ... | CallVarDescs ],
%                (
%                    impure exit_port(ProcId, ExitVarDescs)
%                    % Go to fail port if retry.
%                ;
%                    % preserve_backtrack_into,
%                    impure redo_port(ProcId, ExitVarDescs),
%                    fail
%                )
%            ;
%                % preserve_backtrack_into
%                impure fail_port(ProcId, CallVarDescs, DoRetryB),
%                (
%                    DoRetryB = do_retry,
%                    p(...)
%                ;
%                    DoRetryB = do_not_retry,
%                    fail
%                )
%            )
%        ).
%
% detism_failure:
%
%   p(...) :-
%       promise_<original_purity> (
%           (
%               CallVarDescs = [ ... ],
%               impure call_port(ProcId, CallVarDescs),
%               <original body>
%           ;
%               % preserve_backtrack_into
%               impure fail_port(ProcId, CallVarDescs, DoRetry),
%               (
%                   DoRetry = do_retry,
%                   p(...)
%               ;
%                   DoRetry = do_not_retry,
%                   fail
%               )
%           )
%       ).
%
%
% detism_erroneous:
%
%   p(...) :-
%       promise_<original_purity> (
%           CallVarDescs = [ ... ],
%           impure call_port(ProcId, CallVarDescs),
%           <original body>
%       ).
%
%
% where CallVarDescs, ExitVarDescs are lists of var_value
%
%    :- type var_value
%        --->    unbound_head_var(var_name, pos)           :: out      variable
%        ;       some [T] bound_head_var(var_name, pos, T) :: in       variable
%        ;       some [T] bound_other_var(var_name, T).    :: internal variable
%
%    :- type var_name == string.
%
%    :- type pos == int.
%
% Output head variables may appear twice in a variable description list --
% initially unbound, then overridden by a bound_head_var functor.  Then the
% ExitVarDescs can add output variable bindings to the CallVarDescs list,
% instead of building new lists.  The pos fields give the argument numbers
% of head variables.
%
% The ProcId is of type ssdb.ssdb_proc_id.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ssdebug.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

    % Place the different events (call/exit/fail/redo) around each procedure to
    % allow debugging.
    %
:- pred ssdebug.process_proc(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.modes.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.purity.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module ssdb.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module svmap.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

process_proc(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO) :-
    % We have different transformations for procedures of different
    % determinisms.

    % XXX The definitions of the four process_proc_* predicates are very
    % similar; they look to have generated using cut-and-paste.
    % The common parts should be factored out and moved here.

    proc_info_get_inferred_determinism(!.ProcInfo, Determinism),
    (
        ( Determinism = detism_det
        ; Determinism = detism_cc_multi
        ),
        process_proc_det(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO)
    ;
        ( Determinism = detism_semi
        ; Determinism = detism_cc_non
        ),
        process_proc_semi(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO)
    ;
        ( Determinism = detism_multi
        ; Determinism = detism_non
        ),
        process_proc_nondet(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO)
    ;
        Determinism = detism_erroneous,
        process_proc_erroneous(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO)
    ;
        Determinism = detism_failure,
        process_proc_failure(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO)
    ).

    % Source-to-source transformation for a deterministic goal.
    %
:- pred process_proc_det(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

process_proc_det(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO) :-
    proc_info_get_goal(!.ProcInfo, BodyGoal0),
    BodyGoalInfo0 = get_hlds_goal_info(BodyGoal0),

    some [!PredInfo, !Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),

        % Make the ssdb_proc_id.
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        make_proc_id_construction(!.PredInfo, ProcIdGoals, ProcIdVar, !Varset,
            !Vartypes),

        % Get the list of head variables and their instantiation state.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InitInstMap),

        % Make a list which records the value for each of the head variables
        % at the call port.
        make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
            CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
            !Vartypes, map.init, BoundVarDescsAtCall),

        % Generate the call to handle_event_call(ProcId, VarList).
        make_handle_event("handle_event_call", [ProcIdVar, CallArgListVar],
            HandleEventCallGoal, !ModuleInfo, !Varset, !Vartypes),

        % Get the InstMap at the end of the procedure.
        update_instmap(BodyGoal0, InitInstMap, FinalInstMap),

        % In the case of a retry, the output variables will be bound by the
        % retried call.
        proc_info_instantiated_head_vars(!.ModuleInfo, !.ProcInfo,
            InstantiatedVars),
        goal_info_get_instmap_delta(BodyGoalInfo0) = InstMapDelta,
        create_renaming(InstantiatedVars, InstMapDelta, !Varset, !Vartypes,
            RenamingGoals, _NewVars, Renaming),
        rename_some_vars_in_goal(Renaming, BodyGoal0, BodyGoal1),

        % Make the variable list at the exit port. It's currently a completely
        % new list instead of adding on to the list generated for the call
        % port.
        make_arg_list(0, FinalInstMap, HeadVars, Renaming, ExitArgListVar,
            ExitArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
            !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtExit),

        % Create DoRetry output variable.
        make_retry_var("DoRetry", RetryVar, !Varset, !Vartypes),

        % Generate the call to handle_event_exit(ProcId, VarList, DoRetry).
        make_handle_event("handle_event_exit",
            [ProcIdVar, ExitArgListVar, RetryVar], HandleEventExitGoal,
            !ModuleInfo, !Varset, !Vartypes),

        % Generate the recursive call in the case of a retry.
        make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId, HeadVars,
            RecursiveGoal),

        % Organize the order of the generated code.
        goal_to_conj_list(BodyGoal1, BodyGoalList),
        % Set the determinism.
        Determinism = detism_det,
        goal_info_init(GoalInfo0),
        goal_info_set_determinism(Determinism, GoalInfo0, GoalInfoDet),
        goal_info_set_purity(purity_impure, GoalInfoDet, GoalInfoImpureDet),

        conj_list_to_goal(RenamingGoals, GoalInfoImpureDet, RenamingGoal),
        % Create the switch on Retry at exit port.
        make_switch_goal(RetryVar, RecursiveGoal, RenamingGoal,
            GoalInfoImpureDet, SwitchGoal),

        ConjGoals = ProcIdGoals ++ CallArgListGoals ++
            [HandleEventCallGoal | BodyGoalList] ++
            ExitArgListGoals ++ [HandleEventExitGoal, SwitchGoal],

        conj_list_to_goal(ConjGoals, GoalInfoImpureDet, GoalWithoutPurity),

        % Add the purity scope.
        Purity = goal_info_get_purity(BodyGoalInfo0),
        wrap_with_purity_scope(Purity, GoalInfoDet, GoalWithoutPurity, Goal),

        commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo,
            !ModuleInfo, !.Varset, !.Vartypes)
    ).

    % Source-to-source transformation for a semidet goal.
    %
:- pred process_proc_semi(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

process_proc_semi(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO) :-
    proc_info_get_goal(!.ProcInfo, BodyGoal0),
    get_hlds_goal_info(BodyGoal0) = BodyGoalInfo0,

    some [!PredInfo, !Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),

        % Get the list of head variables and their initial instantiations.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InitInstMap),
        proc_info_get_argmodes(!.ProcInfo, ListMerMode),

        ( check_arguments_modes(!.ModuleInfo, ListMerMode) ->

            % Make the ssdb_proc_id.
            module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
            make_proc_id_construction(!.PredInfo, ProcIdGoals, ProcIdVar,
                !Varset, !Vartypes),

            % Make a list which records the value for each of the head
            % variables at the call port.
            make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
                CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, map.init, BoundVarDescsAtCall),

            % Generate the call to handle_event_call(ProcId, VarList).
            make_handle_event("handle_event_call", [ProcIdVar, CallArgListVar],
                HandleEventCallGoal, !ModuleInfo, !Varset, !Vartypes),

            % Get the InstMap at the end of the procedure.
            update_instmap(BodyGoal0, InitInstMap, FinalInstMap),

            % In the case of a retry, the output variables will be bound by the
            % retried call.
            proc_info_instantiated_head_vars(!.ModuleInfo, !.ProcInfo,
                InstantiatedVars),
            goal_info_get_instmap_delta(BodyGoalInfo0) = InstMapDelta,
            create_renaming(InstantiatedVars, InstMapDelta, !Varset, !Vartypes,
                RenamingGoals, _NewVars, Renaming),
            rename_some_vars_in_goal(Renaming, BodyGoal0, BodyGoal1),

            % Make the variable list at the exit port. It's currently a
            % completely new list instead of adding on to the list generated
            % for the call port.
            make_arg_list(0, FinalInstMap, HeadVars, Renaming, ExitArgListVar,
                ExitArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtExit),

            % Create DoRetryA output variable
            make_retry_var("DoRetryA", RetryAVar, !Varset, !Vartypes),

            % Generate the call to
            %   handle_event_exit(ProcId, VarList, DoRetryA).
            make_handle_event("handle_event_exit",
                [ProcIdVar, ExitArgListVar, RetryAVar], HandleEventExitGoal,
                !ModuleInfo, !Varset, !Vartypes),

            % Generate the recursive call in the case of a retry
            make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId,
                HeadVars, RecursiveGoal),

            % Generate the list of arguments at the fail port.
            make_arg_list(0, InitInstMap, [], Renaming, FailArgListVar,
                FailArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtFail),

            % Create DoRetryB output variable
            make_retry_var("DoRetryB", RetryBVar, !Varset, !Vartypes),

            % Generate the call to
            %   handle_event_fail(ProcId, VarList, DoRetryB).
            make_handle_event("handle_event_fail",
                [ProcIdVar, FailArgListVar, RetryBVar], HandleEventFailGoal,
                !ModuleInfo, !Varset, !Vartypes),

            make_fail_call(FailGoal, !.ModuleInfo),

            % Organize the order of the generated code.

            % Get a flattened goal to avoid nested conjuction.
            goal_to_conj_list(BodyGoal1, BodyGoalList),
            GoalsCond = BodyGoalList,

            % Create the switch on DoRetryA at exit port.
            goal_info_init(GoalInfo0),
            goal_info_set_purity(purity_impure, GoalInfo0, GoalInfoImpure),
            goal_list_purity(GoalsCond, PurityCond),
            goal_list_determinism(GoalsCond, DetismCond),
            goal_info_set_determinism(DetismCond, GoalInfo0,
                GoalInfoCondDet),
            goal_info_set_purity(PurityCond, GoalInfoCondDet,
                GoalInfoCondPurDet),

            SemiDet = detism_semi,
            goal_info_set_determinism(SemiDet, GoalInfo0, GoalInfoSemiDet),
            goal_info_set_purity(purity_impure, GoalInfoSemiDet,
                GoalInfoImpureSemiDet),
            goal_info_set_determinism(detism_det, GoalInfoImpure,
                GoalInfoImpureDet),
            conj_list_to_goal(RenamingGoals, GoalInfoImpureDet, RenamingGoal),

            % Create the switch on DoRetryA at exit port.
            make_switch_goal(RetryAVar, RecursiveGoal, RenamingGoal,
                GoalInfoImpureSemiDet, SwitchExitPortGoal),
            % Create the switch on DoRetryB at fail port.
            make_switch_goal(RetryBVar, RecursiveGoal, FailGoal,
                GoalInfoImpureSemiDet, SwitchFailPortGoal),

            GoalsThen   = ExitArgListGoals ++
                [HandleEventExitGoal, SwitchExitPortGoal],
            GoalsElse   = FailArgListGoals ++
                [HandleEventFailGoal, SwitchFailPortGoal],

            goal_info_set_determinism(detism_det, GoalInfoImpure, GoalInfoThen),
            goal_info_set_determinism(detism_semi, GoalInfoImpure,
                GoalInfoElse),

            IteExistVars = [],
            conj_list_to_goal(GoalsCond, GoalInfoCondPurDet, CondGoal),
            ThenGoal = hlds_goal(conj(plain_conj, GoalsThen), GoalInfoThen),
            ElseGoal = hlds_goal(conj(plain_conj, GoalsElse), GoalInfoElse),

            CallVarGoal = ProcIdGoals ++ CallArgListGoals ++
                [HandleEventCallGoal],
            % XXX not sure about determinism in an if-then-else.
            GoalITE = hlds_goal(if_then_else(IteExistVars, CondGoal, ThenGoal,
                ElseGoal), GoalInfoCondPurDet),

            ConjGoal = CallVarGoal ++ [GoalITE],
            GoalWithoutPurity = hlds_goal(conj(plain_conj, ConjGoal),
                GoalInfoCondPurDet),

            % Add the purity scope.
            Purity = goal_info_get_purity(BodyGoalInfo0),
            wrap_with_purity_scope(Purity, GoalInfoSemiDet, GoalWithoutPurity,
                Goal),

            commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo,
                !ModuleInfo, !.Varset, !.Vartypes)
        ;
            % In the case of a mode which is not fully input or output,
            % the procedure is not transformed.
            true
        )
    ).

    % Source-to-source transformation for a nondeterministic procedure.
    %
:- pred process_proc_nondet(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

process_proc_nondet(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO) :-
    proc_info_get_goal(!.ProcInfo, BodyGoal0),
    get_hlds_goal_info(BodyGoal0) = BodyGoalInfo0,

    some [!PredInfo, !Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),

        % Make the ssdb_proc_id.
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        make_proc_id_construction(!.PredInfo, ProcIdGoals, ProcIdVar, !Varset,
            !Vartypes),

        % Get the list of head variables and their instantiation state.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InitInstMap),

        % Make a list which records the value for each of the head variables
        % at the call port.
        make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
            CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
            !Vartypes, map.init, BoundVarDescsAtCall),

        % Generate the call to handle_event_call(ProcId, VarList).
        make_handle_event("handle_event_call_nondet",
            [ProcIdVar, CallArgListVar],
            HandleEventCallGoal, !ModuleInfo, !Varset, !Vartypes),

        % Get the InstMap at the end of the procedure.
        update_instmap(BodyGoal0, InitInstMap, FinalInstMap),

        % Make the variable list at the exit port. It's currently a completely
        % new list instead of adding on to the list generated for the call
        % port.
        make_arg_list(0, FinalInstMap, HeadVars, map.init, ExitArgListVar,
            ExitArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
            !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtExit),

        % Generate the call to handle_event_exit_nondet(ProcId, VarList).
        make_handle_event("handle_event_exit_nondet",
            [ProcIdVar, ExitArgListVar],
            HandleEventExitGoal, !ModuleInfo, !Varset, !Vartypes),

        % Generate the call to handle_event_redo(ProcId, VarList).
        make_handle_event("handle_event_redo_nondet",
            [ProcIdVar, ExitArgListVar],
            HandleEventRedoGoal, !ModuleInfo, !Varset, !Vartypes),

        % Generate the list of argument at the fail port.
        make_arg_list(0, InitInstMap, [], map.init, FailArgListVar,
            FailArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
            !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtFail),

        % Create DoRetry output variable
        make_retry_var("DoRetry", RetryVar, !Varset, !Vartypes),

        % Generate the call to
        %   handle_event_fail_nondet(ProcId, VarList, DoRetry).
        make_handle_event("handle_event_fail_nondet",
            [ProcIdVar, FailArgListVar, RetryVar],
            HandleEventFailGoal, !ModuleInfo, !Varset, !Vartypes),

        make_fail_call(FailGoal, !.ModuleInfo),

        % Organize the order of the generated code.
        % Get a flattened goal to avoid nested conjuction.
        goal_to_conj_list(BodyGoal0, BodyGoalList0),
        CallVarGoal0 = CallArgListGoals ++
            [HandleEventCallGoal | BodyGoalList0] ++ ExitArgListGoals,
        goal_info_init(GoalInfo0),
        conj_list_to_goal(CallVarGoal0, GoalInfo0, CallVarGoal1),
        goal_to_conj_list(CallVarGoal1, CallVarGoal),

        % Generate the recursive call in the case of a retry
        make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId, HeadVars,
            RecursiveGoal),

        Det = detism_det,
        FailDet = detism_failure,
        NonDet = detism_non,
        goal_info_set_purity(purity_impure, GoalInfo0, GoalInfoImpure),
        goal_info_set_determinism(Det, GoalInfoImpure, GoalInfoImpureDet),
        goal_info_set_determinism(FailDet, GoalInfoImpure,
            GoalInfoImpureFailDet),
        goal_info_set_determinism(NonDet, GoalInfoImpure,
            GoalInfoImpureNonDet),
        goal_list_determinism(BodyGoalList0, Detism),
        goal_info_set_determinism(Detism, GoalInfo0, GoalInfoDetism),
        goal_info_set_determinism(Detism, GoalInfoImpure,
            GoalInfoImpureDetism),

        % Create the switch on DoRetry at fail port.
        make_switch_goal(RetryVar, RecursiveGoal, FailGoal,
            GoalInfoImpureNonDet, SwitchFailPortGoal),

        ConjGoal11 = hlds_goal(conj(plain_conj,
            [HandleEventExitGoal]), GoalInfoImpureDet),
        ConjGoal120 = hlds_goal(conj(plain_conj,
            [HandleEventRedoGoal, FailGoal]), GoalInfoImpureFailDet),
        goal_add_feature(feature_preserve_backtrack_into, ConjGoal120,
            ConjGoal12),

        DisjGoal1 = hlds_goal(disj([ConjGoal11, ConjGoal12]),
            GoalInfoImpureDetism),

        ConjGoal21 = hlds_goal(conj(plain_conj,
            CallVarGoal ++ [DisjGoal1]), GoalInfoImpureDetism),
        ConjGoal220 = hlds_goal(conj(plain_conj, FailArgListGoals ++
            [HandleEventFailGoal, SwitchFailPortGoal]), GoalInfoImpureNonDet),
        goal_add_feature(feature_preserve_backtrack_into, ConjGoal220,
            ConjGoal22),
        DisjGoal2 = hlds_goal(disj([ConjGoal21, ConjGoal22]),
            GoalInfoImpureDetism),

        GoalWithoutPurity = hlds_goal(conj(plain_conj,
            ProcIdGoals ++ [DisjGoal2]), GoalInfoImpureDetism),

        % Add the purity scope.
        Purity = goal_info_get_purity(BodyGoalInfo0),
        wrap_with_purity_scope(Purity, GoalInfoDetism, GoalWithoutPurity, Goal),

        commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo,
            !ModuleInfo, !.Varset, !.Vartypes)
    ).

      % Source-to-source transformation for a failure procedure.
      %
:- pred process_proc_failure(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

process_proc_failure(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO) :-
    proc_info_get_goal(!.ProcInfo, BodyGoal0),
    BodyGoalInfo0 = get_hlds_goal_info(BodyGoal0),

    some [!PredInfo, !Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),

        % Make the ssdb_proc_id.
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        make_proc_id_construction(!.PredInfo, ProcIdGoals, ProcIdVar, !Varset,
            !Vartypes),

        % Get the list of head variables and their instantiation state.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InitInstMap),

        % Make a list which records the value for each of the head variables at
        % the call port.
        make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
            CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
            !Vartypes, map.init, BoundVarDescsAtCall),

        % Generate the call to handle_event_call(ProcId, VarList).
        make_handle_event("handle_event_call", [ProcIdVar, CallArgListVar],
            HandleEventCallGoal, !ModuleInfo, !Varset, !Vartypes),

        % Make the variable list at the exit port. It's currently a completely
        % new list instead of adding on to the list generated for the call
        % port.
        make_arg_list(0, InitInstMap, [], map.init, FailArgListVar,
            FailArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
            !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtFail),

        % Create DoRetry output variable.
        make_retry_var("DoRetry", RetryVar, !Varset, !Vartypes),

        % Generate the call to handle_event_exit(ProcId, VarList, DoRetry).
        make_handle_event("handle_event_fail",
            [ProcIdVar, FailArgListVar, RetryVar],
            HandleEventFailGoal, !ModuleInfo, !Varset, !Vartypes),

        make_fail_call(FailGoal, !.ModuleInfo),

        % Generate the recursive call in the case of a retry.
        make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId, HeadVars,
            RecursiveGoal),

        % Organize the order of the generated code.

        goal_to_conj_list(BodyGoal0, BodyGoalList),
        % Set the determinism.
        Determinism = detism_failure,
        goal_info_init(GoalInfo0),
        goal_info_set_determinism(Determinism, GoalInfo0, GoalInfoFail),
        goal_info_set_purity(purity_impure, GoalInfoFail, GoalInfoImpureFail),

        % Create the switch on Retry at fail port.
        make_switch_goal(RetryVar, RecursiveGoal, FailGoal, GoalInfoImpureFail,
            SwitchGoal),

        ConjGoal1 = hlds_goal(conj(plain_conj, BodyGoalList),
            GoalInfoImpureFail),
        ConjGoal20 = hlds_goal(conj(plain_conj, FailArgListGoals ++
            [HandleEventFailGoal, SwitchGoal]), GoalInfoImpureFail),
        goal_add_feature(feature_preserve_backtrack_into, ConjGoal20,
            ConjGoal2),

        DisjGoal = hlds_goal(disj([ConjGoal1, ConjGoal2]), GoalInfoImpureFail),

        ConjGoals = ProcIdGoals ++ CallArgListGoals ++
            [HandleEventCallGoal, DisjGoal],

        conj_list_to_goal(ConjGoals, GoalInfoImpureFail, GoalWithoutPurity),

        % Add the purity scope.
        Purity = goal_info_get_purity(BodyGoalInfo0),
        wrap_with_purity_scope(Purity, GoalInfoFail, GoalWithoutPurity, Goal),

        commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo,
            !ModuleInfo, !.Varset, !.Vartypes)
    ).

      % Source-to-source transformation for an erroneous procedure.
      % XXX ERRONEOUS procedure have currently just a call port.
      %
:- pred process_proc_erroneous(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

process_proc_erroneous(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO) :-
    proc_info_get_goal(!.ProcInfo, BodyGoal0),
    BodyGoalInfo0 = get_hlds_goal_info(BodyGoal0),

    some [!PredInfo, !Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),

        % Make the ssdb_proc_id.
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        make_proc_id_construction(!.PredInfo, ProcIdGoals, ProcIdVar, !Varset,
            !Vartypes),

        % Get the list of head variables and their instantiation state.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InitInstMap),

        % Make a list which records the value for each of the head variables at
        % the call port.
        make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
            CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
            !Vartypes, map.init, _BoundVarDescsAtCall),

        % Generate the call to handle_event_call(ProcId, VarList).
        make_handle_event("handle_event_call", [ProcIdVar, CallArgListVar],
            HandleEventCallGoal, !ModuleInfo, !Varset, !Vartypes),

        % Organize the order of the generated code.
        goal_to_conj_list(BodyGoal0, BodyGoalList),
        % Set the determinism.
        DeterminismErr = detism_erroneous,
        goal_info_init(GoalInfo0),
        goal_info_set_determinism(DeterminismErr, GoalInfo0,
            GoalInfoErr),
        goal_info_set_purity(purity_impure, GoalInfoErr, GoalInfoImpureErr),

        ConjGoals = ProcIdGoals ++ CallArgListGoals ++
            [HandleEventCallGoal | BodyGoalList],

        conj_list_to_goal(ConjGoals, GoalInfoImpureErr, GoalWithoutPurity),

        % Add the purity scope.
        Purity = goal_info_get_purity(BodyGoalInfo0),
        wrap_with_purity_scope(Purity, GoalInfoErr, GoalWithoutPurity, Goal),

        commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo,
            !ModuleInfo, !.Varset, !.Vartypes)
    ).

%-----------------------------------------------------------------------------%

    % Create the output variable DoRetry.
    %
:- pred make_retry_var(string::in, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

make_retry_var(VarName, RetryVar, !VarSet, !VarTypes) :-
    SSDBModule = mercury_ssdb_builtin_module,
    TypeCtor = type_ctor(qualified(SSDBModule, "ssdb_retry"), 0),
    construct_type(TypeCtor, [], RetryType),
    svvarset.new_named_var(VarName, RetryVar, !VarSet),
    svmap.det_insert(RetryVar, RetryType, !VarTypes).

    % Create the goal for recursive call in the case of a retry.
    %
:- pred make_recursive_call(pred_info::in, module_info::in, pred_id::in,
    proc_id::in, list(prog_var)::in, hlds_goal::out) is det.

make_recursive_call(PredInfo, ModuleInfo, PredId, ProcId, HeadVars, Goal) :-
    PredName = pred_info_name(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    SymName = qualified(ModuleName, PredName),
    BuiltIn = builtin_state(ModuleInfo, PredId, PredId, ProcId),
    GoalExpr = plain_call(PredId, ProcId, HeadVars, BuiltIn, no, SymName),
    goal_info_init(GoalInfoHG),
    Goal = hlds_goal(GoalExpr, GoalInfoHG).

    % make_switch_goal(SwitchVar, SwitchCase1, SwitchCase2, GoalInfo, Goal).
    %
    % Create an output Goal, which is a switch with following pattern :
    %   (
    %       SwitchVar = do_retry,
    %       SwitchCase1
    %   ;
    %       SwitchVar = do_not_retry,
    %       SwitchCase2
    %   )
    %
:- pred make_switch_goal(prog_var::in, hlds_goal::in, hlds_goal::in,
    hlds_goal_info::in, hlds_goal::out) is det.

make_switch_goal(SwitchVar, DoRetryGoal, DoNotRetryGoal, GoalInfo,
        SwitchGoal) :-
    SSDBModule = mercury_ssdb_builtin_module,
    ConsIdDoRetry = cons(qualified(SSDBModule, "do_retry"), 0),
    ConsIdDoNotRetry = cons(qualified(SSDBModule, "do_not_retry"), 0),
    CaseDoRetry = case(ConsIdDoRetry, [], DoRetryGoal),
    CaseDoNotRetry = case(ConsIdDoNotRetry, [], DoNotRetryGoal),
    SwitchGoal = hlds_goal(
        switch(SwitchVar, cannot_fail, [CaseDoRetry, CaseDoNotRetry]),
        GoalInfo).

    % wrap_with_purity_scope(Purity, GoalInfo, Goal0, Goal):
    %
    % The Goal0 is wrap with the Purity to give Goal.
    %
:- pred wrap_with_purity_scope(purity::in, hlds_goal_info::in, hlds_goal::in,
    hlds_goal::out) is det.

wrap_with_purity_scope(Purity, GoalInfo0, GoalWithoutPurity, Goal) :-
    goal_info_set_purity(Purity, GoalInfo0, GoalInfo),
    ScopeReason = promise_purity(dont_make_implicit_promises, Purity),
    Goal = hlds_goal(scope(ScopeReason, GoalWithoutPurity), GoalInfo).

    % Update the proc_info and pred_info with the result of the
    % source-to-source transformation.
    %
:- pred commit_goal_changes(hlds_goal::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out, prog_varset::in, vartypes::in) is det.

commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo, !ModuleInfo,
        Varset, Vartypes) :-
    proc_info_set_varset(Varset, !ProcInfo),
    proc_info_set_vartypes(Vartypes, !ProcInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    requantify_proc(!ProcInfo),
    recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
        !ProcInfo, !ModuleInfo),
    pred_info_set_proc_info(ProcId, !.ProcInfo, !PredInfo),
    repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),
    module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

    % Build the following goal : handle_event_EVENT(ProcId, Arguments).
    % EVENT = call,exit,fail or redo
    % Argument = ProcId, ListHeadVars and eventually Retry
    %
:- pred make_handle_event(string::in, list(prog_var)::in, hlds_goal::out,
    module_info::in, module_info::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

make_handle_event(HandleTypeString, Arguments, HandleEventGoal, !ModuleInfo,
        !Varset, !Vartypes) :-
    SSDBModule = mercury_ssdb_builtin_module,
    Features = [],
    InstMapSrc = [],
    Context = term.context_init,
    goal_util.generate_simple_call(SSDBModule, HandleTypeString,
        pf_predicate, only_mode, detism_det, purity_impure,
        Arguments, Features, InstMapSrc, !.ModuleInfo, Context,
        HandleEventGoal).

    % make_proc_id_construction(PredInfo, Goals, Var, !Varset, !Vartypes)
    %
    % Returns a set of goals, Goals, which build the ssdb_proc_id structure
    % for the given pred and proc infos.  The Var returned holds the
    % ssdb_proc_id.
    %
:- pred make_proc_id_construction(pred_info::in, hlds_goals::out,
    prog_var::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

make_proc_id_construction(PredInfo, Goals, ProcIdVar, !Varset, !Vartypes) :-
    SymModuleName = pred_info_module(PredInfo),
    ModuleName = sym_name_to_string(SymModuleName),
    PredName = pred_info_name(PredInfo),

    make_string_const_construction_alloc(ModuleName, yes("ModuleName"),
        ConstructModuleName, ModuleNameVar, !Varset, !Vartypes),

    make_string_const_construction_alloc(PredName, yes("PredName"),
        ConstructPredName, PredNameVar, !Varset, !Vartypes),

    SSDBModule = mercury_ssdb_builtin_module,
    TypeCtor = type_ctor(qualified(SSDBModule, "ssdb_proc_id"), 0),

    svvarset.new_named_var("ProcId", ProcIdVar, !Varset),
    ConsId = cons(qualified(SSDBModule, "ssdb_proc_id"), 2),
    construct_type(TypeCtor, [], ProcIdType),
    svmap.det_insert(ProcIdVar, ProcIdType, !Vartypes),
    construct_functor(ProcIdVar, ConsId, [ModuleNameVar, PredNameVar],
        ConstructProcIdGoal),

    Goals = [ConstructModuleName, ConstructPredName, ConstructProcIdGoal].

    % make_fail_call(FailGoal, ModuleInfo)
    %
    % Construct the fail goal.
    %
:- pred make_fail_call(hlds_goal::out, module_info::in) is det.

make_fail_call(FailGoal, ModuleInfo) :-
    Features = [],
    InstMapSrc = [],
    Context = term.context_init,
    goal_util.generate_simple_call(mercury_public_builtin_module,
        "false", pf_predicate, only_mode, detism_failure, purity_pure,
        [], Features, InstMapSrc, ModuleInfo, Context, FailGoal).

    % Detect if all argument's mode are fully input or output.
    % XXX Other mode than fully input or output are not handled for the
    % moment. So the code of these procedures will not be generated.
    %
:- pred check_arguments_modes(module_info::in, list(mer_mode)::in)
    is semidet.

check_arguments_modes(ModuleInfo, HeadModes) :-
    all [Modes] (
        list.member(Mode, HeadModes)
    =>
        ( mode_is_fully_input(ModuleInfo, Mode)
        ; mode_is_fully_output(ModuleInfo, Mode)
        )
    ).

%-----------------------------------------------------------------------------%

    % The following code concern predicates which create the list argument at
    % event point.
    %

    % make_arg_list(Pos, InstMap, Vars, RenamedVar, FullListVar, Goals,
    %   !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes, !BoundedVarDesc)
    %
    % Processes each variable in Vars creating a list(var_value) named
    % FullListVar which records the value of each of the variables. Vars points
    % to the start of the list and Goals is the list of goals to construct the
    % list. Pos indicates which argument position the first variable in Vars
    % is.
    % InstMap is used to work out if the variable is instantiated enough yet
    % to display.
    % RenamedVar is a map(X, Y) where Y is the X renamed Var, it is use to
    % replace the output variable at the call of the predicate.
    % BoundedVarDes is a map(X, Y) where Y is the VarDesc of X, it is
    % use while generation to recover the description of already bounded
    % variables.
    %
:- pred make_arg_list(int::in, instmap::in, list(prog_var)::in,
    map(prog_var, prog_var)::in, prog_var::out, list(hlds_goal)::out,
    module_info::in, module_info::out, proc_info::in, proc_info::out,
    pred_info::in, pred_info::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

make_arg_list(_Pos, _InstMap, [], _Renaming, Var, [Goal], !ModuleInfo,
        !ProcInfo, !PredInfo, !Varset, !Vartypes, !BoundVarDescs) :-
    svvarset.new_named_var("EmptyVarList", Var, !Varset),
    svmap.det_insert(Var, list_var_value_type, !Vartypes),
    ConsId = cons(qualified(unqualified("list"), "[]" ), 0),
    construct_functor(Var, ConsId, [], Goal).

make_arg_list(Pos0, InstMap, [VarToInspect | ListVar], Renaming, Var,
        Goals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes,
        !BoundVarDescs) :-
    Pos = Pos0 + 1,
    make_arg_list(Pos, InstMap, ListVar, Renaming, Var0, Goals0,
        !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes, !BoundVarDescs),

    % BoundVarDescs is filled with the description of the input variable during
    % the first call to make_arg_list predicate.
    % At the second call, we search if the current VarToInspect already exist
    % in the map and if yes, copy his recorded description.

    ( map.search(!.BoundVarDescs, VarToInspect, ExistingVarDesc) ->
        ValueGoals = [],
        VarDesc = ExistingVarDesc
    ;
        make_var_value(InstMap, VarToInspect, Renaming, VarDesc, Pos0,
            ValueGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes,
            !BoundVarDescs)
    ),

    svvarset.new_named_var("FullListVar", Var, !Varset),
    svmap.det_insert(Var, list_var_value_type, !Vartypes),
    ConsId = cons(qualified(unqualified("list"), "[|]" ), 2),
    construct_functor(Var, ConsId, [VarDesc, Var0], Goal),

    %XXX Optimize me: repeated appends are slow.
    Goals = Goals0 ++ ValueGoals ++ [Goal].

    % Return the type list(var_value).
    %
:- func list_var_value_type = mer_type.

list_var_value_type = ListVarValueType :-
    SSDBModule = mercury_ssdb_builtin_module,
    VarValueTypeCtor = type_ctor(qualified(SSDBModule, "var_value"), 0),
    construct_type(VarValueTypeCtor, [], VarValueType),

    ListTypeCtor = type_ctor(qualified(unqualified("list"), "list"), 1),
    construct_type(ListTypeCtor, [VarValueType], ListVarValueType).

    % Create the goal's argument description :
    % -> unbound_head_var(Name, Pos) if it is an unbound argument
    % -> bound_head_var(type_of_T, Name, Position, T) if it is a bound argument
    %
:- pred make_var_value(instmap::in, prog_var::in, map(prog_var, prog_var)::in,
    prog_var::out, int::in, list(hlds_goal)::out,
    module_info::in, module_info::out, proc_info::in, proc_info::out,
    pred_info::in, pred_info::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, map(prog_var, prog_var)::in,
    map(prog_var, prog_var)::out) is det.

make_var_value(InstMap, VarToInspect, Renaming, VarDesc, VarPos, Goals,
        !ModuleInfo, !ProcInfo, !PredInfo, !VarSet, !VarTypes,
        !BoundVarDescs) :-
    SSDBModule = mercury_ssdb_builtin_module,
    TypeCtor = type_ctor(qualified(SSDBModule, "var_value"), 0),
    varset.lookup_name(!.VarSet, VarToInspect, VarName),
    make_string_const_construction_alloc(VarName, yes("VarName"),
        ConstructVarName, VarNameVar, !VarSet, !VarTypes),
    make_int_const_construction_alloc(VarPos, yes("VarPos"),
        ConstructVarPos, VarPosVar, !VarSet, !VarTypes),

    ( var_is_ground_in_instmap(!.ModuleInfo, InstMap, VarToInspect) ->
        % Update proc_varset and proc_vartypes, without this, the
        % polymorphism_make_type_info_var uses a prog_var which is
        % already bound.

        proc_info_set_varset(!.VarSet, !ProcInfo),
        proc_info_set_vartypes(!.VarTypes, !ProcInfo),

        % Create dynamic constructor for the value of the argument.
        %
        % Call polymorphism.m to create the type_infos, add an hidden field
        % which is the polymorphic type of the value.
        %
        % some[T] bound_head_var(string, int, T) ---->
        %   some[T] bound_head_var(type_of_T, string, int, T)

        create_poly_info(!.ModuleInfo, !.PredInfo, !.ProcInfo, PolyInfo0),
        term.context_init(Context),
        map.lookup(!.VarTypes, VarToInspect, MerType),
        polymorphism_make_type_info_var(MerType, Context, TypeInfoVar,
            TypeInfoGoal, PolyInfo0, PolyInfo),
        poly_info_extract(PolyInfo, !PredInfo, !ProcInfo, !:ModuleInfo),

        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),

        % Constructor of the variable's description.
        svvarset.new_named_var("VarDesc", VarDesc, !VarSet),
        ConsId = cons(qualified(SSDBModule, "bound_head_var"), 3),
        construct_type(TypeCtor, [], VarType),
        svmap.det_insert(VarDesc, VarType, !VarTypes),

        % Renaming contains the names of all instantiated arguments
        % during the execution of the procedure's body.
        ( map.is_empty(Renaming) ->
            construct_functor(VarDesc, ConsId, [TypeInfoVar, VarNameVar,
                VarPosVar, VarToInspect], ConstructVarGoal)
        ;
            map.lookup(Renaming, VarToInspect, RenamedVar),
            construct_functor(VarDesc, ConsId, [TypeInfoVar, VarNameVar,
                VarPosVar, RenamedVar], ConstructVarGoal)
        ),
        Goals = [ConstructVarName, ConstructVarPos | TypeInfoGoal] ++
            [ConstructVarGoal],
        svmap.det_insert(VarToInspect, VarDesc, !BoundVarDescs)
    ;
        svvarset.new_named_var("VarDesc", VarDesc, !VarSet),
        ConsId = cons(qualified(SSDBModule, "unbound_head_var"), 2),
        construct_type(TypeCtor, [], VarType),
        svmap.det_insert(VarDesc, VarType, !VarTypes),
        construct_functor(VarDesc, ConsId, [VarNameVar, VarPosVar],
            ConstructVarGoal),

        Goals = [ConstructVarName, ConstructVarPos, ConstructVarGoal]
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
