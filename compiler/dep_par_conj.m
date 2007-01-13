%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: dep_par_conj.m.
% Author: wangp.
%
% This module implements dependent parallel conjunction using a HLDS->HLDS
% transformation.  The transformation adds calls to the synchronisation
% predicates defined in library/par_builtin.m.
%
% For a parallel conjunction (A & B), if the goal B is dependent on a variable
% X which is bound by goal A, we first transform the conjunction into the
% following:
%
%    par_builtin.new_future(FutureX),
%    (
%        (
%            A(X),  % binds X
%            impure par_builtin.signal(FutureX, X)
%        )
%    &
%        (
%            par_builtin.wait(FutureX, X1),
%            B(X1)  % uses X
%        )
%    )
%
% That is, goal B must wait for the value to be produced by A before it begins
% executing.  If B is a compound goal then the wait call is moved as late
% into B as possible.  Signal calls will be moved to as early as possible.
% Future variables become the only variables shared by parallel conjuncts.
%
% A subsequent pass looks for contiguous sequences of waits, a call to a
% predicate P, followed by signals in a conjunction (there must be at least one
% wait or one signal).  If the code of P is available then a specialised
% ("parallel") version of P is produced, taking futures in place of any
% arguments which need to be waited on or signalled.  For example:
%
%   wait(FutureX, X),
%   p(X, Y),
%   impure signal(FutureY, Y)
%
% would be transformed into:
%
%   Parallel__p(FutureX, FutureY),
%
% where the wait and signal calls are now in the body of Parallel__p.
%
%
% In non-parallel grades dependent parallel conjunctions are converted
% into sequential conjunctions.
%
% TODO:
% - reconsider when this pass is run; in particular par_builtin primitives
%   ought to be inlined
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.dep_par_conj.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module set.

%-----------------------------------------------------------------------------%

:- pred dependent_par_conj(module_info::in, module_info::out, io::di, io::uo)
    is det.

    % Exported for use by the implicit_parallelism pass.
:- func find_shared_variables(module_info, instmap, hlds_goals)
    = set(prog_var).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.purity.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % This type holds information relevant to the dependent parallel
    % conjunction transformation.
    %
:- type dep_par_info
    --->    dep_par_info(
                dp_par_procs    :: par_procs,
                % Parallelised procedures added or waiting to be added.

                dp_module_info  :: module_info,
                % The current module.

                dp_varset       :: prog_varset,
                dp_vartypes     :: vartypes,
                % The varset and vartypes for the procedure being analysed.

                dp_ignore_vars  :: set(prog_var)
                % Variables which should not be replaced by futures in this
                % pass because it has already been done.
            ).

    % Parallelised versions of procedures that have been added to the module,
    % or are scheduled to be added.
    %
:- type par_procs
    --->    par_procs(done_par_procs, pending_par_procs).

    % Parallelised procedures that have been added to the module already.
    % The calling pattern is the original pred_proc_id of the procedure
    % being called, plus the list of arguments which have been replaced
    % by futures.
    %
:- type done_par_procs ==       map(par_proc_call_pattern, new_par_proc).

    % Parallelised procedures that are scheduled to be added.
    % One or more procedures in the module will already be making calls
    % to the scheduled procedure.
    %
:- type pending_par_procs ==    assoc_list(par_proc_call_pattern, new_par_proc).

:- type par_proc_call_pattern
    --->    par_proc_call_pattern(
                old_ppid        :: pred_proc_id,
                future_args     :: list(arg_pos)
            ).

:- type new_par_proc
    --->    new_par_proc(
                new_ppid        :: pred_proc_id,
                new_name        :: sym_name
            ).

:- type arg_pos == int.

    % A map from a variable to the future object created for that variable.
    % i.e. for variable X with future F, when X is bound to a value then F is
    % signalled.  A consumer of X waits (blocks) on F until that happens.
    %
:- type future_map == map(prog_var, prog_var).

%-----------------------------------------------------------------------------%

dependent_par_conj(!ModuleInfo, !IO) :-
    ModuleInfo0 = !.ModuleInfo,

    % First process all parallel conjunctions in user-defined procedures.
    module_info_predids(!.ModuleInfo, PredIds),
    ParProcs0 = par_procs(map.init, []),
    list.foldl3(process_pred_for_dep_par_conj, PredIds,
        !ModuleInfo, ParProcs0, ParProcs, !IO),
    ParProcs = par_procs(DoneParProcs, PendingProcs),

    expect(map.is_empty(DoneParProcs), this_file, "DoneParProcs non empty"),

    % Create the pending parallelised versions of procedures.
    % These may in turn create more parallelised versions.
    add_pending_par_procs(DoneParProcs, PendingProcs,
        ModuleInfo0, !ModuleInfo, !IO).

    % Parallel conjunctions only supported on lowlevel C parallel
    % grades.
    %
:- pred handle_par_conj(module_info::in) is semidet.

handle_par_conj(ModuleInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.lookup_bool_option(Globals, parallel, Parallel),
    Target = target_c,
    HighLevelCode = no,
    Parallel = yes.

%-----------------------------------------------------------------------------%

:- pred process_pred_for_dep_par_conj(pred_id::in,
    module_info::in, module_info::out, par_procs::in, par_procs::out,
    io::di, io::uo) is det.

process_pred_for_dep_par_conj(PredId, !ModuleInfo, !ParProcs, !IO) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    list.foldl3(process_proc_for_dep_par_conj(PredId), ProcIds,
        !ModuleInfo, !ParProcs, !IO).

:- pred process_proc_for_dep_par_conj(pred_id::in, proc_id::in,
    module_info::in, module_info::out, par_procs::in, par_procs::out,
    io::di, io::uo) is det.

process_proc_for_dep_par_conj(PredId, ProcId, !ModuleInfo, !ParProcs, !IO) :-
    module_info_proc_info(!.ModuleInfo, PredId, ProcId, ProcInfo),
    proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
    (
        HasParallelConj = no
    ;
        HasParallelConj = yes,
        process_proc_for_dep_par_conj_with_ignores(PredId, ProcId, set.init,
            !ModuleInfo, !ParProcs, !IO)
    ).

:- pred process_proc_for_dep_par_conj_with_ignores(pred_id::in, proc_id::in,
    set(prog_var)::in, module_info::in, module_info::out,
    par_procs::in, par_procs::out, io::di, io::uo) is det.

process_proc_for_dep_par_conj_with_ignores(PredId, ProcId, IgnoreVars,
        !ModuleInfo, !ParProcs, !IO) :-
    some [!PredInfo, !ProcInfo, !Body, !VarSet, !VarTypes] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            !:PredInfo, !:ProcInfo),
        proc_info_get_goal(!.ProcInfo, !:Body),
        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap0),

        Info0 = dep_par_info(!.ParProcs, !.ModuleInfo,
            !.VarSet, !.VarTypes, IgnoreVars),

        search_goal_for_par_conj(!Body, InstMap0, _, Info0, Info1),

        (if handle_par_conj(!.ModuleInfo) then
            replace_sequences_in_goal(!Body, Info1, Info2),
            Info2 = dep_par_info(!:ParProcs, !:ModuleInfo,
                !:VarSet, !:VarTypes, _IgnoreVars),
            % XXX RTTI varmaps may need to be updated
            rename_apart_in_goal(!.ModuleInfo, !Body, InstMap0,
                !VarSet, !VarTypes)
        else
            Info1 = dep_par_info(!:ParProcs, !:ModuleInfo,
                !:VarSet, !:VarTypes, _IgnoreVars)
        ),

        % We really only need to run this part if something changed, but we
        % only run this predicate on procedures which are likely to have
        % parallel conjunctions.
        proc_info_set_varset(!.VarSet, !ProcInfo),
        proc_info_set_vartypes(!.VarTypes, !ProcInfo),
        proc_info_set_goal(!.Body, !ProcInfo),
        fixup_and_reinsert_proc(PredId, ProcId, !.PredInfo, !.ProcInfo,
            !ModuleInfo)
    ).

:- pred fixup_and_reinsert_proc(pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, module_info::in, module_info::out) is det.

fixup_and_reinsert_proc(PredId, ProcId, !.PredInfo, !.ProcInfo, !ModuleInfo) :-
    requantify_proc(!ProcInfo),
    RecomputeAtomic = no,
    recompute_instmap_delta_proc(RecomputeAtomic, !ProcInfo, !ModuleInfo),
    pred_info_set_proc_info(ProcId, !.ProcInfo, !PredInfo),
    repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),
    module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred add_pending_par_procs(done_par_procs::in, pending_par_procs::in,
    module_info::in, module_info::in, module_info::out, io::di, io::uo) is det.

add_pending_par_procs(_DoneParProcs, [], _ModuleInfo0, !ModuleInfo, !IO).
add_pending_par_procs(DoneParProcs0, [CallPattern - NewProc | Pending0],
        ModuleInfo0, !ModuleInfo, !IO) :-
    % Move the procedure we are about to parallelise into the list of
    % done procedures, in case of recursive calls.
    map.det_insert(DoneParProcs0, CallPattern, NewProc, DoneParProcs),
    add_pending_par_proc(CallPattern, NewProc, DoneParProcs,
        Pending0, Pending, ModuleInfo0, !ModuleInfo, !IO),
    add_pending_par_procs(DoneParProcs, Pending,
        ModuleInfo0, !ModuleInfo, !IO).

:- pred add_pending_par_proc(par_proc_call_pattern::in, new_par_proc::in,
    done_par_procs::in, pending_par_procs::in, pending_par_procs::out,
    module_info::in, module_info::in, module_info::out, io::di, io::uo)
    is det.

add_pending_par_proc(CallPattern, NewProc, DoneParProcs, PendingProcs0,
        PendingProcs, ModuleInfo0, !ModuleInfo, !IO) :-
    CallPattern = par_proc_call_pattern(OldPredProcId, FutureArgs),
    NewProc     = new_par_proc(NewPredProcId, _Name),
    ParProcs0   = par_procs(DoneParProcs, PendingProcs0),
    ParProcs    = par_procs(_DoneParProcs, PendingProcs),
    add_pending_par_proc_2(OldPredProcId, NewPredProcId, FutureArgs,
        ModuleInfo0, !ModuleInfo, ParProcs0, ParProcs, !IO).

:- pred add_pending_par_proc_2(pred_proc_id::in, pred_proc_id::in,
    list(arg_pos)::in, module_info::in, module_info::in, module_info::out,
    par_procs::in, par_procs::out, io::di, io::uo) is det.

add_pending_par_proc_2(proc(OldPredId, OldProcId), proc(PredId, ProcId),
        FutureArgs, ModuleInfo0, !ModuleInfo, !ParProcs, !IO) :-
    some [!VarSet, !VarTypes, !ProcInfo] (
        % Get the proc_info from _before_ the dependent parallel conjunction
        % pass was ever run, so we get untransformed procedure bodies.
        % Our transformation does not attempt to handle already transformed
        % parallel conjunctions.
        module_info_proc_info(ModuleInfo0, OldPredId, OldProcId, !:ProcInfo),
        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),
        proc_info_get_headvars(!.ProcInfo, HeadVars0),
        proc_info_get_argmodes(!.ProcInfo, ArgModes0),
        proc_info_get_goal(!.ProcInfo, Goal0),
        proc_info_get_initial_instmap(!.ProcInfo, ModuleInfo0, InstMap0),

        % Set up the mapping from head variables to futures.
        list.foldl4(reproduce_future_map(HeadVars0), FutureArgs,
            map.init, FutureMap, !VarSet, !VarTypes, !ModuleInfo),

        % Replace head variables by their futures.
        replace_head_vars(!.ModuleInfo, FutureMap,
            HeadVars0, HeadVars, ArgModes0, ArgModes),

        % Insert signals and waits into procedure body as a conjunct of a
        % parallel conjunction.
        SharedVars = set.from_list(map.keys(FutureMap)),
        transform_conjunct(SharedVars, FutureMap, Goal0, Goal, InstMap0, _,
            !VarSet, !VarTypes, !ModuleInfo, !ParProcs),

        proc_info_set_varset(!.VarSet, !ProcInfo),
        proc_info_set_vartypes(!.VarTypes, !ProcInfo),
        proc_info_set_headvars(HeadVars, !ProcInfo),
        proc_info_set_argmodes(ArgModes, !ProcInfo),
        proc_info_set_goal(Goal, !ProcInfo),

        module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),

        % Mark this predicate impure if it no longer has any output arguments
        % (having been replaced by a future, which is an input argument which
        % is destructively updated).
        (if any_output_arguments(!.ModuleInfo, ArgModes) then
            PredInfo = PredInfo0
        else
            pred_info_get_markers(PredInfo0, Markers0),
            add_marker(marker_is_impure, Markers0, Markers),
            pred_info_set_markers(Markers, PredInfo0, PredInfo)
        ),

        fixup_and_reinsert_proc(PredId, ProcId, PredInfo, !.ProcInfo,
            !ModuleInfo),

        % Continue processing the procedure and look for further dependent
        % parallel conjunctions.
        IgnoreVars = set.from_list(map.keys(FutureMap)),
        process_proc_for_dep_par_conj_with_ignores(PredId, ProcId, IgnoreVars,
            !ModuleInfo, !ParProcs, !IO)
    ).

:- pred reproduce_future_map(list(prog_var)::in,
    arg_pos::in, future_map::in, future_map::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    module_info::in, module_info::out) is det.

reproduce_future_map(HeadVars, FutureArg, !FutureMap,
        !VarSet, !VarTypes, !ModuleInfo) :-
    HeadVar = list.det_index1(HeadVars, FutureArg),
    map.lookup(!.VarTypes, HeadVar, VarType),
    make_future(!.ModuleInfo, VarType, HeadVar, !VarTypes, !VarSet,
        _AllocGoal, FutureVar),
    svmap.det_insert(HeadVar, FutureVar, !FutureMap).

:- pred replace_head_vars(module_info::in, future_map::in,
    prog_vars::in, prog_vars::out, list(mer_mode)::in, list(mer_mode)::out)
    is det.

replace_head_vars(_ModuleInfo, _FutureMap, [], [], [], []).
replace_head_vars(ModuleInfo, FutureMap,
        [Var0 | Vars0], [Var | Vars], [Mode0 | Modes0], [Mode | Modes]) :-
    ( map.search(FutureMap, Var0, Var1) ->
        Var = Var1,
        ( mode_is_input(ModuleInfo, Mode0) ->
            Mode = Mode0
        ; mode_is_output(ModuleInfo, Mode0) ->
            Mode = (ground(shared, none) -> ground(shared, none))
        ;
            sorry(this_file,
                "dependent parallel conjunction transformation " ++
                "only understands input and output modes")
        )
    ;
        Var = Var0,
        Mode = Mode0
    ),
    replace_head_vars(ModuleInfo, FutureMap, Vars0, Vars, Modes0, Modes).
replace_head_vars(_, _, [_|_], _, [], _) :-
    unexpected(this_file, "replace_head_vars: length mismatch").
replace_head_vars(_, _, [], _, [_|_], _) :-
    unexpected(this_file, "replace_head_vars: length mismatch").

:- pred any_output_arguments(module_info::in, list(mer_mode)::in) is semidet.

any_output_arguments(ModuleInfo, [Mode | Modes]) :-
    ( mode_is_output(ModuleInfo, Mode)
    ; any_output_arguments(ModuleInfo, Modes)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Determine if a parallel conjunction is a dependent parallel conjunction.
    % If so, allocate futures for variables shared between conjuncts.
    % Insert wait and signal calls for those futures into the conjuncts.
    %
:- pred search_goal_for_par_conj(hlds_goal::in, hlds_goal::out,
    instmap::in, instmap::out, dep_par_info::in, dep_par_info::out) is det.

search_goal_for_par_conj(Goal0, Goal, InstMap0, InstMap, !Info) :-
    search_goal_for_par_conj_2(Goal0, Goal, InstMap0, !Info),
    update_instmap(Goal, InstMap0, InstMap).

:- pred search_goal_for_par_conj_2(hlds_goal::in, hlds_goal::out,
    instmap::in, dep_par_info::in, dep_par_info::out) is det.

search_goal_for_par_conj_2(Goal0, Goal, InstMap0, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            search_goals_for_par_conj(Goals0, Goals, InstMap0, !Info),
            conj_list_to_goal(Goals, GoalInfo0, Goal)
        ;
            ConjType = parallel_conj,
            maybe_transform_par_conj(Goals0, GoalInfo0, Goal, InstMap0, !Info)
        )
    ;
        GoalExpr0 = disj(Goals0),
        search_disj_for_par_conj(Goals0, Goals, InstMap0, !Info),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        search_cases_for_par_conj(Cases0, Cases, InstMap0, !Info),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Quant, If0, Then0, Else0),
        search_goal_for_par_conj(If0, If, InstMap0, InstMap1, !Info),
        search_goal_for_par_conj(Then0, Then, InstMap1, _InstMap2, !Info),
        search_goal_for_par_conj(Else0, Else, InstMap0, _InstMap3, !Info),
        GoalExpr = if_then_else(Quant, If, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        search_goal_for_par_conj(SubGoal0, SubGoal, InstMap0, _, !Info),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, ScopeGoal0),
        search_goal_for_par_conj(ScopeGoal0, ScopeGoal, InstMap0, _, !Info),
        GoalExpr = scope(Reason, ScopeGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        ( GoalExpr0 = unify(_, _, _, _Kind, _)
        ; GoalExpr0 = plain_call(_CallPredId, _CallProcId, _CallArgs, _, _, _)
        ; GoalExpr0 = generic_call(_Details, _Args, _ArgModes, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file,
            "shorthand goal encountered during dependent parallel " ++
            "conjunction transformation.")
    ).

:- pred search_goals_for_par_conj(hlds_goals::in, hlds_goals::out,
    instmap::in, dep_par_info::in, dep_par_info::out) is det.

search_goals_for_par_conj([], [], _InstMap0, !Info).
search_goals_for_par_conj([Goal0 | Goals0], [Goal | Goals], InstMap0, !Info) :-
    search_goal_for_par_conj(Goal0, Goal, InstMap0, InstMap, !Info),
    search_goals_for_par_conj(Goals0, Goals, InstMap, !Info).

:- pred search_disj_for_par_conj(hlds_goals::in, hlds_goals::out,
    instmap::in, dep_par_info::in, dep_par_info::out) is det.

search_disj_for_par_conj([], [], _InstMap0, !Info).
search_disj_for_par_conj([Goal0 | Goals0], [Goal | Goals], InstMap0, !Info) :-
    search_goal_for_par_conj(Goal0, Goal, InstMap0, _InstMap, !Info),
    search_disj_for_par_conj(Goals0, Goals, InstMap0, !Info).

:- pred search_cases_for_par_conj(list(case)::in, list(case)::out, instmap::in,
    dep_par_info::in, dep_par_info::out) is det.

search_cases_for_par_conj([], [], _InstMap0, !Info).
search_cases_for_par_conj([Case0 | Cases0], [Case | Cases], InstMap0, !Info) :-
    Case0 = case(Functor, Goal0),
    search_goal_for_par_conj(Goal0, Goal, InstMap0, _, !Info),
    Case = case(Functor, Goal),
    search_cases_for_par_conj(Cases0, Cases, InstMap0, !Info).

%-----------------------------------------------------------------------------%

    % We found a parallel conjunction.  Check for any dependencies
    % between the conjuncts and, if so, insert sychronisation primitives.
    %
:- pred maybe_transform_par_conj(hlds_goals::in, hlds_goal_info::in,
    hlds_goal::out, instmap::in, dep_par_info::in, dep_par_info::out)
    is det.

maybe_transform_par_conj(Conjuncts0, GoalInfo, NewGoal, InstMap,
        !Info) :-
    % Search subgoals for nested parallel conjunctions.
    search_goals_for_par_conj(Conjuncts0, Conjuncts, InstMap, !Info),

    % Find the variables that are shared between conjuncts.
    SharedVars0 = find_shared_variables(!.Info ^ dp_module_info,
        InstMap, Conjuncts),

    % Filter out all the variables which have already have associated futures,
    % i.e. they were head variables which were replaced by futures; signal and
    % wait calls will already have been inserted for them.
    SharedVars = set.filter(isnt(set.contains(!.Info ^ dp_ignore_vars)),
        SharedVars0),

    !.Info = dep_par_info(ParProcs0, ModuleInfo0,
        VarSet0, VarTypes0, IgnoreVars),
    (if
        handle_par_conj(ModuleInfo0)
    then
        (if
            set.empty(SharedVars)
        then
            % Independent parallel conjunctions need no transformation.
            par_conj_list_to_goal(Conjuncts, GoalInfo, NewGoal)
        else
            transform_conjunction(SharedVars, Conjuncts, GoalInfo, NewGoal,
                InstMap, VarSet0, VarSet, VarTypes0, VarTypes,
                ModuleInfo0, ModuleInfo, ParProcs0, ParProcs),
            !:Info = dep_par_info(ParProcs, ModuleInfo,
                VarSet, VarTypes, IgnoreVars)
        )
    else
        % Replace all parallel conjunctions by sequential conjunctions.
        conj_list_to_goal(Conjuncts, GoalInfo, NewGoal)
    ).

    % Transforming the parallel conjunction.
    %
    % We insert waits as deeply into the conjunction as possible, and signals
    % as early as possible.
    %
    % Example:
    %
    %    p(A, B, ABA) :-
    %        ( append(A, B, AB)
    %        & append(AB, A, ABA)
    %        ).
    %
    % becomes:
    %
    %   p(A, B, ABA) :-
    %       new_future(FutureAB),
    %       (
    %           append(A, B, AB_7),
    %           impure signal(FutureAB, AB_7)
    %       &
    %           wait(FutureAB, AB_10),
    %           append(AB_10, A, ABA)
    %       ).
    %
:- pred transform_conjunction(set(prog_var)::in,
    hlds_goals::in, hlds_goal_info::in, hlds_goal::out, instmap::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    module_info::in, module_info::out,
    par_procs::in, par_procs::out) is det.

transform_conjunction(SharedVars, Goals, GoalInfo, NewGoal, InstMap,
        !VarSet, !VarTypes, !ModuleInfo, !ParProcs) :-
    SharedVarsList = set.to_sorted_list(SharedVars),
    list.map_foldl3(allocate_future(!.ModuleInfo), SharedVarsList,
        AllocateFutures, !VarTypes, !VarSet, map.init, FutureMap),
    list.map_foldl5(transform_conjunct(SharedVars, FutureMap),
        Goals, NewGoals,
        InstMap, _, !VarSet, !VarTypes, !ModuleInfo, !ParProcs),

    LastGoal = hlds_goal(conj(parallel_conj, NewGoals), GoalInfo),
    Conj = AllocateFutures ++ [LastGoal],
    conj_list_to_goal(Conj, GoalInfo, NewGoal0),

    % Wrap a purity scope around the goal if purity would have been lessened
    % by the addition of signal goals (which are impure) or calls to
    % parallelised procs (which may be impure).
    goal_info_get_purity(GoalInfo, Purity),
    ( Purity = purity_impure ->
        NewGoal = NewGoal0
    ;
        Reason = promise_purity(dont_make_implicit_promises, Purity),
        NewGoal = hlds_goal(scope(Reason, NewGoal0), GoalInfo)
    ).

:- pred allocate_future(module_info::in, prog_var::in, hlds_goal::out,
    vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
    future_map::in, future_map::out) is det.

allocate_future(ModuleInfo, SharedVar, AllocGoal,
        !VarTypes, !VarSet, !FutureMap) :-
    map.lookup(!.VarTypes, SharedVar, SharedVarType),
    make_future(ModuleInfo, SharedVarType, SharedVar, !VarTypes, !VarSet,
        AllocGoal, FutureVar),
    svmap.det_insert(SharedVar, FutureVar, !FutureMap).

:- pred transform_conjunct(set(prog_var)::in, future_map::in,
    hlds_goal::in, hlds_goal::out, instmap::in, instmap::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    module_info::in, module_info::out,
    par_procs::in, par_procs::out) is det.

transform_conjunct(SharedVars, FutureMap, Goal0, Goal, !InstMap,
        !VarSet, !VarTypes, !ModuleInfo, !ParProcs) :-
    goal_get_nonlocals(Goal0, Nonlocals),
    set.intersect(Nonlocals, SharedVars, Intersect),
    ( set.empty(Intersect) ->
        Goal = Goal0
    ;
        Goal0 = hlds_goal(_, GoalInfo0),
        goal_info_get_instmap_delta(GoalInfo0, InstMapDelta0),

        % Divide shared variables into those that are produced by this
        % conjunct, and those that are consumed by it.
        IsProducedVar = var_is_bound_in_instmap_delta(!.ModuleInfo,
            !.InstMap, InstMapDelta0),
        set.divide(IsProducedVar, Intersect, ProducedVars, ConsumedVars),

        % Insert waits into the conjunct as deeply as possible.
        list.foldl3(insert_wait_in_goal(!.ModuleInfo, FutureMap),
            set.to_sorted_list(ConsumedVars),
            Goal0, Goal1, !VarSet, !VarTypes),

        % Insert signals into the conjunct as early as possible.
        list.foldl3(insert_signal_in_goal(!.ModuleInfo, FutureMap),
            set.to_sorted_list(ProducedVars),
            Goal1, Goal, !VarSet, !VarTypes)
    ),
    update_instmap(Goal, !InstMap).

%-----------------------------------------------------------------------------%

    % If a variable is nonlocal to a conjunct, and appears in the
    % instmap_delta of a _different_ conjunct, then we say that variable is
    % shared.
    %
    % (1) A variable must be nonlocal to a conjunct if it is shared.
    % (2) If the variable does not appear in the instmap_delta
    %     of any of the conjuncts of the parallel conjunction
    %     then it could not have been further instantiated within
    %     by the conjunction as a whole.
    %
    % XXX this code is probably too complicated.  I think Thomas already had a
    % more elegant way to find the shared variables somewhere, using multisets.
    %
find_shared_variables(ModuleInfo, InstMap, Goals) = SharedVars :-
    list.map2(get_nonlocals_and_instmaps, Goals, Nonlocals, InstMapDeltas),
    find_shared_variables_2(ModuleInfo, 0, Nonlocals, InstMap, InstMapDeltas,
        set.init, SharedVars).

:- pred get_nonlocals_and_instmaps(hlds_goal::in,
    set(prog_var)::out, instmap_delta::out) is det.

get_nonlocals_and_instmaps(hlds_goal(_, GoalInfo), Nonlocals, InstMapDelta) :-
    goal_info_get_nonlocals(GoalInfo, Nonlocals),
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta).

:- pred find_shared_variables_2(module_info::in, int::in,
    list(set(prog_var))::in, instmap::in, list(instmap_delta)::in,
    set(prog_var)::in, set(prog_var)::out) is det.

find_shared_variables_2(_ModuleInfo, _ConjunctIndex,
        [], _InstMap, _InstMapDeltas, !SharedVars).
find_shared_variables_2(ModuleInfo, ConjunctIndex,
        [Nonlocals | MoreNonlocals], InstMap, InstMapDeltas, !SharedVars) :-
    det_delete_nth(ConjunctIndex, InstMapDeltas, InstMapDeltasB),
    % Keep only nonlocals which were not already bound at the start of the
    % parallel conjunction.
    Filter = (pred(Var::in) is semidet :-
        instmap.lookup_var(InstMap, Var, VarInst),
        not inst_is_bound(ModuleInfo, VarInst)
    ),
    set.filter(Filter, Nonlocals) = UnboundNonlocals,
    set.filter(changed_var(ModuleInfo, InstMapDeltasB), UnboundNonlocals)
        = Changed,
    set.union(Changed, !SharedVars),
    find_shared_variables_2(ModuleInfo, ConjunctIndex+1, MoreNonlocals,
        InstMap, InstMapDeltas, !SharedVars).

:- pred changed_var(module_info::in, list(instmap_delta)::in, prog_var::in)
    is semidet.

changed_var(ModuleInfo, InstMapDeltas, UnboundVar) :-
    % Is the unbound nonlocal bound in one of the conjuncts?
    InstMapDelta `list.member` InstMapDeltas,
    instmap_delta_search_var(InstMapDelta, UnboundVar, Inst),
    inst_is_bound(ModuleInfo, Inst).

%-----------------------------------------------------------------------------%

    % Look for the first instance of the consumed variable down every
    % computation path.  The first goal referring to the variable needs to
    % have a wait call inserted right before it.
    %
:- pred insert_wait_in_goal(module_info::in, future_map::in, prog_var::in,
    hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    ( var_in_nonlocals(ConsumedVar, Goal0) ->
        insert_wait_in_goal_2(ModuleInfo, FutureMap, ConsumedVar,
            Goal0, Goal, !VarSet, !VarTypes)
    ;
        Goal = Goal0
    ).

    % ConsumedVar is nonlocal to Goal0.
    %
:- pred insert_wait_in_goal_2(module_info::in, future_map::in, prog_var::in,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

insert_wait_in_goal_2(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            insert_wait_in_conj(ModuleInfo, FutureMap, ConsumedVar,
                Goals0, Goal, !VarSet, !VarTypes)
        ;
            ConjType = parallel_conj,
            insert_wait_in_goals(ModuleInfo, FutureMap, ConsumedVar,
                Goals0, Goals, !VarSet, !VarTypes),
            GoalExpr = conj(ConjType, Goals),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = disj(Goals0),
        insert_wait_in_goals(ModuleInfo, FutureMap, ConsumedVar,
            Goals0, Goals, !VarSet, !VarTypes),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        ( ConsumedVar = SwitchVar ->
            insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                Goal0, Goal, !VarSet, !VarTypes)
        ;
            insert_wait_in_cases(ModuleInfo, FutureMap, ConsumedVar,
                Cases0, Cases, !VarSet, !VarTypes),
            GoalExpr = switch(SwitchVar, CanFail, Cases),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = if_then_else(Quant, If, Then0, Else0),
        ( var_in_nonlocals(ConsumedVar, If) ->
            insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                Goal0, Goal, !VarSet, !VarTypes)
        ;
            insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
                Then0, Then, !VarSet, !VarTypes),
            insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
                Else0, Else, !VarSet, !VarTypes),
            GoalExpr = if_then_else(Quant, If, Then, Else),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = negation(SubGoal0),
        insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
            SubGoal0, SubGoal, !VarSet, !VarTypes),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
            SubGoal0, SubGoal, !VarSet, !VarTypes),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        ( GoalExpr0 = unify(_LHS, _RHS0, _C, _D, _UnifyContext)
        ; GoalExpr0 = plain_call(_PredId, _ProcId, _Args, _, _, _)
        ; GoalExpr0 = generic_call(_GenericCall, _Args, _Modes, _Detism)
        ; GoalExpr0 = call_foreign_proc(_, _PredId, _, _Args, _, _, _)
        ),
        insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
            Goal0, Goal, !VarSet, !VarTypes)
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file,
            "shorthand goal encountered during dependent parallel " ++
            "conjunction transformation.")
    ).

:- pred insert_wait_before_goal(module_info::in, future_map::in,
    prog_var::in, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    FutureVar = map.lookup(FutureMap, ConsumedVar),
    make_wait_goal(ModuleInfo, FutureVar, ConsumedVar, WaitGoal),
    conjoin_goals_update_goal_infos(WaitGoal, Goal0, Goal).

:- pred insert_wait_in_conj(module_info::in, future_map::in, prog_var::in,
    hlds_goals::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_conj(_ModuleInfo, _FutureMap, _ConsumedVar,
        [], true_goal, !VarSet, !VarTypes).
insert_wait_in_conj(ModuleInfo, FutureMap, ConsumedVar,
        [Goal0 | Goals0], Goal, !VarSet, !VarTypes) :-
    (if var_in_nonlocals(ConsumedVar, Goal0) then
        insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
            Goal0, Goal1, !VarSet, !VarTypes),
        conjoin_goal_and_goal_list_update_goal_infos(Goal1, Goals0, Goal)
    else
        insert_wait_in_conj(ModuleInfo, FutureMap, ConsumedVar,
            Goals0, Goal1, !VarSet, !VarTypes),
        conjoin_goals_update_goal_infos(Goal0, Goal1, Goal)
    ).

:- pred insert_wait_in_goals(module_info::in, future_map::in, prog_var::in,
    hlds_goals::in, hlds_goals::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_goals(_ModuleInfo, _FutureMap, _ConsumedVar,
        [], [], !VarSet, !VarTypes).
insert_wait_in_goals(ModuleInfo, FutureMap, ConsumedVar,
        [Goal0 | Goals0], [Goal | Goals], !VarSet, !VarTypes) :-
    insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes),
    insert_wait_in_goals(ModuleInfo, FutureMap, ConsumedVar,
        Goals0, Goals, !VarSet, !VarTypes).

:- pred insert_wait_in_cases(module_info::in, future_map::in, prog_var::in,
    list(case)::in, list(case)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_cases(_ModuleInfo, _FutureMap, _ConsumedVar,
        [], [], !VarSet, !VarTypes).
insert_wait_in_cases(ModuleInfo, FutureMap, ConsumedVar,
        [Case0 | Cases0], [Case | Cases], !VarSet, !VarTypes) :-
    Case0 = case(Functor, Goal0),
    insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes),
    Case = case(Functor, Goal),
    insert_wait_in_cases(ModuleInfo, FutureMap, ConsumedVar,
        Cases0, Cases, !VarSet, !VarTypes).

%-----------------------------------------------------------------------------%

    % Look for the first instance of the produced variable down every
    % computation path.  The first goal referring to the variable must produce
    % it, so insert a signal call right after that goal.
    %
:- pred insert_signal_in_goal(module_info::in, future_map::in, prog_var::in,
    hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    (if var_in_nonlocals(ProducedVar, Goal0) then
        insert_signal_in_goal_2(ModuleInfo, FutureMap, ProducedVar,
            Goal0, Goal, !VarSet, !VarTypes)
    else
        Goal = Goal0
    ).

    % ProducedVar is nonlocal to Goal0.
    %
:- pred insert_signal_in_goal_2(module_info::in, future_map::in, prog_var::in,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

insert_signal_in_goal_2(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            insert_signal_in_conj(ModuleInfo, FutureMap, ProducedVar,
                Goals0, Goal, !VarSet, !VarTypes)
        ;
            ConjType = parallel_conj,
            insert_signal_in_goals(ModuleInfo, FutureMap, ProducedVar,
                Goals0, Goals, !VarSet, !VarTypes),
            GoalExpr = conj(ConjType, Goals),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = disj(Goals0),
        insert_signal_in_goals(ModuleInfo, FutureMap, ProducedVar,
            Goals0, Goals, !VarSet, !VarTypes),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        ( ProducedVar = SwitchVar ->
            unexpected(this_file, "switch on unbound shared variable")
        ;
            insert_signal_in_cases(ModuleInfo, FutureMap, ProducedVar,
                Cases0, Cases, !VarSet, !VarTypes),
            GoalExpr = switch(SwitchVar, CanFail, Cases),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = if_then_else(Quant, If, Then0, Else0),
        expect(var_not_in_nonlocals(ProducedVar, If),
            this_file, "condition binds shared variable"),
        insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
            Then0, Then, !VarSet, !VarTypes),
        insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
            Else0, Else, !VarSet, !VarTypes),
        GoalExpr = if_then_else(Quant, If, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        expect(var_not_in_nonlocals(ProducedVar, SubGoal0),
            this_file, "negation binds shared variable"),
        Goal = Goal0
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
            SubGoal0, SubGoal, !VarSet, !VarTypes),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        ( GoalExpr0 = unify(_LHS, _RHS0, _C, _D, _UnifyContext)
        ; GoalExpr0 = plain_call(_PredId, _ProcId, _Args, _, _, _)
        ; GoalExpr0 = generic_call(_GenericCall, _Args, _Modes, _Detism)
        ; GoalExpr0 = call_foreign_proc(_, _PredId, _, _Args, _, _, _)
        ),
        insert_signal_after_goal(ModuleInfo, FutureMap, ProducedVar,
            Goal0, Goal, !VarSet, !VarTypes)
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file,
            "shorthand goal encountered during dependent parallel " ++
            "conjunction transformation.")
    ).

:- pred insert_signal_after_goal(module_info::in, future_map::in,
    prog_var::in, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_signal_after_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    make_signal_goal(ModuleInfo, FutureMap, ProducedVar, SignalGoal),
    conjoin_goals_update_goal_infos(Goal0, SignalGoal, Goal).

:- pred insert_signal_in_conj(module_info::in, future_map::in, prog_var::in,
    hlds_goals::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_signal_in_conj(_ModuleInfo, _FutureMap, _ProducedVar,
        [], true_goal, !VarSet, !VarTypes).
insert_signal_in_conj(ModuleInfo, FutureMap, ProducedVar,
        [Goal0 | Goals0], Goal, !VarSet, !VarTypes) :-
    (if var_in_nonlocals(ProducedVar, Goal0) then
        insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
            Goal0, Goal1, !VarSet, !VarTypes),
        conjoin_goal_and_goal_list_update_goal_infos(Goal1, Goals0, Goal)
    else
        insert_signal_in_conj(ModuleInfo, FutureMap, ProducedVar,
            Goals0, Goal1, !VarSet, !VarTypes),
        conjoin_goals_update_goal_infos(Goal0, Goal1, Goal)
    ).

:- pred insert_signal_in_goals(module_info::in, future_map::in, prog_var::in,
    hlds_goals::in, hlds_goals::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_signal_in_goals(_ModuleInfo, _FutureMap, _ProducedVar,
        [], [], !VarSet, !VarTypes).
insert_signal_in_goals(ModuleInfo, FutureMap, ProducedVar,
        [Goal0 | Goals0], [Goal | Goals], !VarSet, !VarTypes) :-
    insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarSet, !VarTypes),
    insert_signal_in_goals(ModuleInfo, FutureMap, ProducedVar,
        Goals0, Goals, !VarSet, !VarTypes).

:- pred insert_signal_in_cases(module_info::in, future_map::in, prog_var::in,
    list(case)::in, list(case)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_signal_in_cases(_ModuleInfo, _FutureMap, _ProducedVar,
        [], [], !VarSet, !VarTypes).
insert_signal_in_cases(ModuleInfo, FutureMap, ProducedVar,
        [Case0 | Cases0], [Case | Cases], !VarSet, !VarTypes) :-
    Case0 = case(Functor, Goal0),
    insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarSet, !VarTypes),
    Case = case(Functor, Goal),
    insert_signal_in_cases(ModuleInfo, FutureMap, ProducedVar,
        Cases0, Cases, !VarSet, !VarTypes).

%-----------------------------------------------------------------------------%

:- pred var_in_nonlocals(prog_var::in, hlds_goal::in) is semidet.

var_in_nonlocals(Var, Goal) :-
    goal_get_nonlocals(Goal, Nonlocals),
    set.member(Var, Nonlocals).

:- pred var_not_in_nonlocals(prog_var::in, hlds_goal::in) is semidet.

var_not_in_nonlocals(Var, Goal) :-
    not var_in_nonlocals(Var, Goal).

%-----------------------------------------------------------------------------%

    % Replace contiguous sequences of waits, a call to P, then signals by a
    % call to a parallelised procedure P'.  Queue P' to be created later,
    % if it has not been created already.
    %
:- pred replace_sequences_in_goal(hlds_goal::in, hlds_goal::out,
    dep_par_info::in, dep_par_info::out) is det.

replace_sequences_in_goal(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            replace_sequences_in_conj(Goals0, Goals, !Info),
            conj_list_to_goal(Goals, GoalInfo0, Goal)
        ;
            ConjType = parallel_conj,
            replace_sequences_in_goals(Goals0, Goals, !Info),
            GoalExpr = conj(ConjType, Goals),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = disj(Goals0),
        replace_sequences_in_goals(Goals0, Goals, !Info),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        replace_sequences_in_cases(Cases0, Cases, !Info),
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Quant, If0, Then0, Else0),
        replace_sequences_in_goal(If0, If, !Info),
        replace_sequences_in_goal(Then0, Then, !Info),
        replace_sequences_in_goal(Else0, Else, !Info),
        GoalExpr = if_then_else(Quant, If, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        replace_sequences_in_goal(SubGoal0, SubGoal, !Info),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        replace_sequences_in_goal(SubGoal0, SubGoal, !Info),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        ( GoalExpr0 = unify(_LHS, _RHS0, _C, _D, _UnifyContext)
        ; GoalExpr0 = plain_call(_PredId, _ProcId, _Args, _, _, _)
        ; GoalExpr0 = generic_call(_GenericCall, _Args, _Modes, _Detism)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file,
            "shorthand goal encountered during dependent parallel " ++
            "conjunction transformation.")
    ).

:- pred replace_sequences_in_conj(hlds_goals::in, hlds_goals::out,
    dep_par_info::in, dep_par_info::out) is det.

replace_sequences_in_conj(Goals0, Goals, !Info) :-
    % For each call goal, look backwards for as many wait calls
    % as possible and forward for as many signal calls as possible.
    % To look backwards maintain a stack of the preceding goals.
    replace_sequences_in_conj_2([], Goals0, Goals, !Info).

:- pred replace_sequences_in_conj_2(hlds_goals::in, hlds_goals::in,
    hlds_goals::out, dep_par_info::in, dep_par_info::out) is det.

replace_sequences_in_conj_2(RevGoals, [], reverse(RevGoals), !Info).
replace_sequences_in_conj_2(RevGoals0, [Goal0 | Goals0], Goals, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        not is_wait_goal(Goal0),
        not is_signal_goal(Goal0)
    ->
        CallGoal0 = hlds_goal(GoalExpr0, GoalInfo0),  % dumb mode system
        maybe_replace_call(RevGoals0, CallGoal0, Goals0, RevGoals1, Goals1,
            !Info),
        replace_sequences_in_conj_2(RevGoals1, Goals1, Goals, !Info)
    ;
        replace_sequences_in_goal(Goal0, Goal, !Info),
        replace_sequences_in_conj_2([Goal | RevGoals0], Goals0, Goals, !Info)
    ).

:- pred replace_sequences_in_goals(hlds_goals::in, hlds_goals::out,
    dep_par_info::in, dep_par_info::out) is det.

replace_sequences_in_goals([], [], !Info).
replace_sequences_in_goals([Goal0 | Goals0], [Goal | Goals], !Info) :-
    replace_sequences_in_goal(Goal0, Goal, !Info),
    replace_sequences_in_goals(Goals0, Goals, !Info).

:- pred replace_sequences_in_cases(list(case)::in, list(case)::out,
    dep_par_info::in, dep_par_info::out) is det.

replace_sequences_in_cases([], [], !Info).
replace_sequences_in_cases([Case0 | Cases0], [Case | Cases], !Info) :-
    Case0 = case(Functor, Goal0),
    replace_sequences_in_goal(Goal0, Goal, !Info),
    Case = case(Functor, Goal),
    replace_sequences_in_cases(Cases0, Cases, !Info).

:- inst call_goal_expr
    ==  bound(plain_call(ground, ground, ground, ground, ground, ground)).

:- inst call_goal
    ==  bound(hlds_goal(call_goal_expr, ground)).

:- pred maybe_replace_call(hlds_goals::in, hlds_goal::in(call_goal),
    hlds_goals::in, hlds_goals::out, hlds_goals::out,
    dep_par_info::in, dep_par_info::out) is det.

maybe_replace_call(RevGoals0, Goal0, FwdGoals0, RevGoals, FwdGoals, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, _),
    GoalExpr0 = plain_call(PredId, ProcId, CallVars, _, _, _),

    module_info_pred_info(!.Info ^ dp_module_info, PredId, PredInfo),
    ( list.member(ProcId, pred_info_non_imported_procids(PredInfo)) ->
        % RevGoals0 = WaitGoals1   ++ RevGoals1
        % FwdGoals0 = SignalGoals1 ++ FwdGoals1
        %
        list.takewhile(is_wait_goal,   RevGoals0, WaitGoals1,   RevGoals1),
        list.takewhile(is_signal_goal, FwdGoals0, SignalGoals1, FwdGoals1),

        % Filter out relevant and irrelevant wait and signal goals
        % i.e. wait and signal goals for variables that appear as call
        % arguments or not.
        %
        list.filter_map(relevant_wait_goal(CallVars), WaitGoals1,
            WaitPairs, IrrelevantWaitGoals),
        list.filter_map(relevant_signal_goal(CallVars), SignalGoals1,
            SignalPairs, IrrelevantSignalGoals),

        (
            WaitPairs = [],
            SignalPairs = []
        ->
            RevGoals = [Goal0 | RevGoals0],
            FwdGoals = FwdGoals0
        ;
            replace_call(WaitPairs, SignalPairs, Goal0, Goal, !Info),

            % After the replaced call may be further references to a signalled
            % or waited variable.  Add `get' goals after the transformed goal
            % to make sure the variable is bound.  We assume the get goals will
            % be simplified away if they turn out to be unnecessary.
            %
            % XXX the simplify pass that comes later doesn't always remove
            %     these calls even if they're unnecessary
            %
            list.map(make_get_goal(!.Info ^ dp_module_info),
                SignalPairs ++ WaitPairs, GetGoals),

            RevGoals = GetGoals ++ [Goal] ++ IrrelevantWaitGoals ++ RevGoals1,
            FwdGoals = IrrelevantSignalGoals ++ FwdGoals1
        )
    ;
        RevGoals = [Goal0 | RevGoals0],
        FwdGoals = FwdGoals0
    ).

:- type future_var_pair
    --->    future_var_pair(
                fvp_future  :: prog_var,
                fvp_var     :: prog_var
            ).

:- func fvp_var(future_var_pair) = prog_var.

:- pred relevant_wait_goal(list(prog_var)::in, hlds_goal::in,
    future_var_pair::out) is semidet.

relevant_wait_goal(CallVars, hlds_goal(GoalExpr, _GoalInfo),
        future_var_pair(Future, WaitVar)) :-
    GoalExpr = plain_call(_, _, [Future, WaitVar], _, _, _),
    list.member(WaitVar, CallVars).

:- pred relevant_signal_goal(list(prog_var)::in, hlds_goal::in,
    future_var_pair::out) is semidet.

relevant_signal_goal(CallVars, hlds_goal(GoalExpr, _GoalInfo),
        future_var_pair(Future, SignalVar)) :-
    GoalExpr = plain_call(_, _, [Future, SignalVar], _, _, _),
    list.member(SignalVar, CallVars).

:- pred replace_call(list(future_var_pair)::in, list(future_var_pair)::in,
    hlds_goal::in(call_goal), hlds_goal::out,
    dep_par_info::in, dep_par_info::out) is det.

replace_call(WaitPairs, SignalPairs, Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    GoalExpr0 = plain_call(PredId, ProcId, CallVars, _Builtin, Context, _Name),
    OrigPPId = proc(PredId, ProcId),

    WaitVars   = list.map(fvp_var, WaitPairs),
    SignalVars = list.map(fvp_var, SignalPairs),
    number_future_args(1, CallVars, WaitVars ++ SignalVars, [], FutureArgs),

    CallPattern = par_proc_call_pattern(OrigPPId, FutureArgs),
    (
        find_par_proc_for_call_pattern(!.Info ^ dp_par_procs,
            CallPattern, ParProc)
    ->
        ParProc = new_par_proc(ParPPId, ParName)
    ;
        % Queue a new parallel procedure to be made.
        !.Info = dep_par_info(ParProcs0, ModuleInfo0,
            VarSet, VarTypes, IgnoreVars),

        create_new_pred(FutureArgs, OrigPPId, ParPPId, NewName,
            ModuleInfo0, ModuleInfo),
        module_info_get_name(ModuleInfo, ModuleName),
        ParName = qualified(ModuleName, NewName),
        queue_par_proc(CallPattern, new_par_proc(ParPPId, ParName),
            ParProcs0, ParProcs),

        !:Info = dep_par_info(ParProcs, ModuleInfo,
            VarSet, VarTypes, IgnoreVars)
    ),

    % Replace the call with a call to the parallelised procedure.
    ParPPId = proc(ParPredId, ParProcId),
    list.map(replace_args_with_futures(WaitPairs ++ SignalPairs),
        CallVars, NewCallVars),
    GoalExpr = plain_call(ParPredId, ParProcId, NewCallVars, not_builtin,
        Context, ParName),
    Goal = hlds_goal(GoalExpr, GoalInfo0).

:- pred find_par_proc_for_call_pattern(par_procs::in,
    par_proc_call_pattern::in, new_par_proc::out) is semidet.

find_par_proc_for_call_pattern(par_procs(DoneParProcs, PendingProcs),
        CallPattern, NewProc) :-
    ( search(DoneParProcs, CallPattern, NewProc0) ->
        NewProc = NewProc0
    ; search(PendingProcs, CallPattern, NewProc0) ->
        NewProc = NewProc0
    ;
        fail
    ).

:- pred queue_par_proc(par_proc_call_pattern::in, new_par_proc::in,
    par_procs::in, par_procs::out) is det.

queue_par_proc(CallPattern, NewProc,
        par_procs(Done, Pending),
        par_procs(Done, [CallPattern - NewProc | Pending])).

:- pred replace_args_with_futures(list(future_var_pair)::in,
    prog_var::in, prog_var::out) is det.

replace_args_with_futures([], Var, Var).
replace_args_with_futures([H | T], Var0, Var) :-
    H = future_var_pair(Future, X),
    ( X = Var0 ->
        Var = Future
    ;
        replace_args_with_futures(T, Var0, Var)
    ).

:- pred number_future_args(arg_pos::in, prog_vars::in, list(prog_var)::in,
    list(arg_pos)::in, list(arg_pos)::out) is det.

number_future_args(_, [], _, RevAcc, reverse(RevAcc)).
number_future_args(ArgNo, [Arg | Args], WaitSignalVars, !RevAcc) :-
    ( Arg `list.member` WaitSignalVars ->
        list.cons(ArgNo, !RevAcc)
    ;
        true
    ),
    number_future_args(ArgNo+1, Args, WaitSignalVars, !RevAcc).

:- pred create_new_pred(list(arg_pos)::in, pred_proc_id::in,
    pred_proc_id::out, string::out, module_info::in, module_info::out)
    is det.

create_new_pred(FutureArgs, OrigPPId, proc(NewPredId, ProcId), NewPredName,
        !ModuleInfo) :-
    OrigPPId = proc(_, ProcId),
    module_info_pred_proc_info(!.ModuleInfo, OrigPPId,
        OrigPredInfo, OrigProcInfo),

    Status = status_local,
    make_new_pred_info(FutureArgs, Status, OrigPPId, OrigPredInfo,
        NewPredInfo0),
    NewPredName = pred_info_name(NewPredInfo0),

    % Assign the old procedure to a new predicate, which will be modified
    % in a later pass.
    pred_info_get_procedures(NewPredInfo0, NewProcs0),
    map.set(NewProcs0, ProcId, OrigProcInfo, NewProcs),
    pred_info_set_procedures(NewProcs, NewPredInfo0, NewPredInfo),

    % Add the new predicate to the pred table.
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(NewPredInfo, NewPredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo).

    % The comments in this predicate are from unused_args.m
    %
:- pred make_new_pred_info(list(arg_pos)::in, import_status::in,
    pred_proc_id::in, pred_info::in, pred_info::out) is det.

make_new_pred_info(FutureArgs, Status, proc(PredId, ProcId), !PredInfo) :-
    PredModule = pred_info_module(!.PredInfo),
    Name0 = pred_info_name(!.PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
    pred_info_get_arg_types(!.PredInfo, Tvars, ExistQVars, ArgTypes0),
    pred_info_get_origin(!.PredInfo, OrigOrigin),
    make_pred_name(PredModule, "Parallel", yes(PredOrFunc),
        Name0, newpred_parallel_args(FutureArgs), Name1),
    % The mode number is included because we want to avoid the creation of
    % more than one predicate with the same name if more than one mode of
    % a predicate is parallelised. Since the names of e.g. deep profiling
    % proc_static structures are derived from the names of predicates,
    % duplicate predicate names lead to duplicate global variable names
    % and hence to link errors.
    proc_id_to_int(ProcId, ProcInt),
    add_sym_name_suffix(Name1, "_" ++ int_to_string(ProcInt), Name),
    Arity = pred_info_orig_arity(!.PredInfo),
    pred_info_get_typevarset(!.PredInfo, TypeVars),

    futurise_argtypes(1, FutureArgs, ArgTypes0, ArgTypes),

    pred_info_context(!.PredInfo, Context),
    pred_info_clauses_info(!.PredInfo, ClausesInfo),
    pred_info_get_markers(!.PredInfo, Markers),
    pred_info_get_goal_type(!.PredInfo, GoalType),
    pred_info_get_class_context(!.PredInfo, ClassContext),

    % Since this pred_info isn't built until after the polymorphism
    % transformation is complete, we just use dummy maps for the class
    % constraints.
    map.init(EmptyProofs),
    map.init(EmptyConstraintMap),
    Origin = origin_transformed(transform_dependent_parallel_conjunction,
        OrigOrigin, PredId),
    pred_info_init(PredModule, Name, Arity, PredOrFunc, Context, Origin,
        Status, GoalType, Markers, ArgTypes, Tvars, ExistQVars,
        ClassContext, EmptyProofs, EmptyConstraintMap, ClausesInfo,
        !:PredInfo),
    pred_info_set_typevarset(TypeVars, !PredInfo).

:- pred futurise_argtypes(arg_pos::in, list(arg_pos)::in, list(mer_type)::in,
    list(mer_type)::out) is det.

futurise_argtypes(_, [], ArgTypes, ArgTypes).
futurise_argtypes(ArgNo, [FutureArg | FutureArgs], [ArgType0 | ArgTypes0],
        [ArgType | ArgTypes]) :-
    (if ArgNo = FutureArg then
        construct_future_type(ArgType0, ArgType),
        futurise_argtypes(ArgNo+1, FutureArgs,
            ArgTypes0, ArgTypes)
    else
        ArgType = ArgType0,
        futurise_argtypes(ArgNo+1, [FutureArg | FutureArgs],
            ArgTypes0, ArgTypes)
    ).
futurise_argtypes(_, [_|_], [], _) :-
    unexpected(this_file,
        "futurise_argtypes: more future arguments than argument types").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Rename apart variables so that parallel conjuncts do not share
    % the names of any variables except for futures.
    % In exactly one conjunct the name of a shared variable is left
    % unchanged, so that code following the parallel conjunction
    % can refer to the old name.
    %
    % We do this as a separate pass to keep the code introducing futures,
    % waits and signals simpler.  Also that pass works inside out, whereas
    % this pass works from the outside in.
    %
:- pred rename_apart_in_goal(module_info::in,
    hlds_goal::in, hlds_goal::out, instmap::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

rename_apart_in_goal(ModuleInfo, Goal0, Goal, InstMap,
        !VarSet, !VarTypes) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            rename_apart_in_conj(ModuleInfo, Goals0, Goals, InstMap,
                !VarSet, !VarTypes)
        ;
            ConjType = parallel_conj,

            % Get nonlocal variables which are not futures.
            SharedVars0 = find_shared_variables(ModuleInfo, InstMap, Goals0),
            NonFutureVar = (pred(Var::in) is semidet :-
                map.lookup(!.VarTypes, Var, Type),
                not is_future_type(Type)
            ),
            SharedVars = set.filter(NonFutureVar, SharedVars0),

            list.map_foldl3(rename_apart_in_par_conjunct(SharedVars),
                Goals0, Goals1,
                SharedVars, _, !VarSet, !VarTypes),
            rename_apart_in_goals(ModuleInfo,
                Goals1, Goals, InstMap, !VarSet, !VarTypes)
        ),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        rename_apart_in_goals(ModuleInfo,
            Goals0, Goals, InstMap, !VarSet, !VarTypes),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        rename_apart_in_cases(ModuleInfo,
            Cases0, Cases, InstMap, !VarSet, !VarTypes),
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Quant, If0, Then0, Else0),
        rename_apart_in_goal(ModuleInfo,
            If0, If, InstMap, !VarSet, !VarTypes),
        rename_apart_in_goal(ModuleInfo,
            Then0, Then, InstMap, !VarSet, !VarTypes),
        rename_apart_in_goal(ModuleInfo,
            Else0, Else, InstMap, !VarSet, !VarTypes),
        GoalExpr = if_then_else(Quant, If, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        rename_apart_in_goal(ModuleInfo,
            SubGoal0, SubGoal, InstMap, !VarSet, !VarTypes),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        rename_apart_in_goal(ModuleInfo,
            SubGoal0, SubGoal, InstMap, !VarSet, !VarTypes),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        ( GoalExpr0 = unify(_LHS, _RHS0, _C, _D, _UnifyContext)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = shorthand(_)
        ),
        Goal = Goal0
    ).

:- pred rename_apart_in_conj(module_info::in,
    hlds_goals::in, hlds_goals::out, instmap::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

rename_apart_in_conj(_ModuleInfo,
        [], [], _InstMap, !VarSet, !VarTypes).
rename_apart_in_conj(ModuleInfo,
        [Goal0 | Goals0], [Goal | Goals], InstMap0, !VarSet, !VarTypes) :-
    rename_apart_in_goal(ModuleInfo,
        Goal0, Goal, InstMap0, !VarSet, !VarTypes),
    update_instmap(Goal, InstMap0, InstMap),
    rename_apart_in_conj(ModuleInfo,
        Goals0, Goals, InstMap, !VarSet, !VarTypes).

:- pred rename_apart_in_goals(module_info::in,
    hlds_goals::in, hlds_goals::out, instmap::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

rename_apart_in_goals(_ModuleInfo,
        [], [], _InstMap, !VarSet, !VarTypes).
rename_apart_in_goals(ModuleInfo,
        [Goal0 | Goals0], [Goal | Goals], InstMap0, !VarSet, !VarTypes) :-
    rename_apart_in_goal(ModuleInfo,
        Goal0, Goal, InstMap0, !VarSet, !VarTypes),
    rename_apart_in_goals(ModuleInfo,
        Goals0, Goals, InstMap0, !VarSet, !VarTypes).

:- pred rename_apart_in_cases(module_info::in,
    list(case)::in, list(case)::out, instmap::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

rename_apart_in_cases(_ModuleInfo,
        [], [], _InstMap0, !VarSet, !VarTypes).
rename_apart_in_cases(ModuleInfo,
        [Case0 | Cases0], [Case | Cases], InstMap0,
        !VarSet, !VarTypes) :-
    Case0 = case(Functor, Goal0),
    rename_apart_in_goal(ModuleInfo,
        Goal0, Goal, InstMap0, !VarSet, !VarTypes),
    Case = case(Functor, Goal),
    rename_apart_in_cases(ModuleInfo,
        Cases0, Cases, InstMap0, !VarSet, !VarTypes).

:- pred rename_apart_in_par_conjunct(set(prog_var)::in,
    hlds_goal::in, hlds_goal::out, set(prog_var)::in, set(prog_var)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

rename_apart_in_par_conjunct(AllNonLocals, Goal0, Goal,
        DontRename0, DontRename, !VarSet, !VarTypes) :-
    free_goal_vars(Goal0) = GoalVars,
    set.intersect(GoalVars, AllNonLocals, Intersect),

    set.difference(Intersect, DontRename0) = DoRename,
    set.difference(DontRename0, Intersect) = DontRename,

    create_variables(set.to_sorted_list(DoRename),
        !.VarSet, !.VarTypes, !VarSet, !VarTypes, map.init, Renaming),
    rename_some_vars_in_goal(Renaming, Goal0, Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Utilities for working with par_builtin.
%

    % Given a variable SharedVar of type SharedVarType, add a new variable
    % FutureVar of type future(SharedVarType).
    % Also make a goal AllocGoal that calls `par_builtin.new_future/1' to
    % allocate FutureVar.
    %
:- pred make_future(module_info::in, mer_type::in, prog_var::in, vartypes::in,
    vartypes::out, prog_varset::in, prog_varset::out, hlds_goal::out,
    prog_var::out) is det.

make_future(ModuleInfo, SharedVarType, SharedVar, !VarTypes, !VarSet,
        AllocGoal, FutureVar) :-
    construct_future_type(SharedVarType, FutureType),
    varset.lookup_name(!.VarSet, SharedVar, SharedVarName),
    svvarset.new_named_var("Future" ++ SharedVarName, FutureVar, !VarSet),
    svmap.det_insert(FutureVar, FutureType, !VarTypes),

    ModuleName = mercury_par_builtin_module,
    PredName = "new_future",
    Args = [FutureVar],
    Features = [],
    InstMapSrc = [FutureVar - ground(shared, none)],
    Context = term.context_init,
    goal_util.generate_simple_call(ModuleName, PredName, predicate,
        only_mode, detism_det, purity_pure, Args, Features, InstMapSrc,
        ModuleInfo, Context, AllocGoal).

    % Construct type future(T) given type T.
    %
:- pred construct_future_type(mer_type::in, mer_type::out) is det.

construct_future_type(T, FutureT) :-
    Future = qualified(mercury_par_builtin_module, "future"),
    FutureCtor = type_ctor(Future, 1),
    construct_type(FutureCtor, [T], FutureT).

:- pred is_future_type(mer_type::in) is semidet.

is_future_type(T) :-
    Future = qualified(mercury_par_builtin_module, "future"),
    FutureCtor = type_ctor(Future, 1),
    type_to_ctor_and_args(T, FutureCtor, _Args).

:- pred make_wait_goal(module_info::in, prog_var::in, prog_var::in,
    hlds_goal::out) is det.

make_wait_goal(ModuleInfo, FutureVar, WaitVar, WaitGoal) :-
    make_wait_or_get(ModuleInfo, FutureVar, WaitVar, "wait", WaitGoal).

:- pred make_get_goal(module_info::in, future_var_pair::in,
    hlds_goal::out) is det.

make_get_goal(ModuleInfo, future_var_pair(FutureVar, WaitVar), WaitGoal) :-
    make_wait_or_get(ModuleInfo, FutureVar, WaitVar, "get", WaitGoal).

:- pred make_wait_or_get(module_info::in, prog_var::in, prog_var::in,
    string::in, hlds_goal::out) is det.

make_wait_or_get(ModuleInfo, FutureVar, WaitVar, PredName, WaitGoal) :-
    ModuleName = mercury_par_builtin_module,
    Args = [FutureVar, WaitVar],
    Features = [],
    InstMapSrc = [WaitVar - ground(shared, none)],
    Context = term.context_init,
    goal_util.generate_simple_call(ModuleName, PredName, predicate,
        only_mode, detism_det, purity_pure, Args, Features, InstMapSrc,
        ModuleInfo, Context, WaitGoal).

:- pred make_signal_goal(module_info::in, future_map::in, prog_var::in,
    hlds_goal::out) is det.

make_signal_goal(ModuleInfo, FutureMap, ProducedVar, SignalGoal) :-
    FutureVar = map.lookup(FutureMap, ProducedVar),
    ModuleName = mercury_par_builtin_module,
    PredName = "signal",
    Args = [FutureVar, ProducedVar],
    Features = [],
    InstMapSrc = [],
    Context = term.context_init,
    goal_util.generate_simple_call(ModuleName, PredName, predicate,
        only_mode, detism_det, purity_impure, Args, Features, InstMapSrc,
        ModuleInfo, Context, SignalGoal).

:- pred is_wait_goal(hlds_goal::in) is semidet.

is_wait_goal(hlds_goal(plain_call(_, _, _, _, _, SymName), _GoalInfo)) :-
    SymName = qualified(mercury_par_builtin_module, "wait").

:- pred is_signal_goal(hlds_goal::in) is semidet.

is_signal_goal(hlds_goal(plain_call(_, _, _, _, _, SymName), _GoalInfo)) :-
    SymName = qualified(mercury_par_builtin_module, "signal").

%-----------------------------------------------------------------------------%

:- pred conjoin_goal_and_goal_list_update_goal_infos(hlds_goal::in,
    list(hlds_goal)::in, hlds_goal::out) is det.

conjoin_goal_and_goal_list_update_goal_infos(Goal0, Goals, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( GoalExpr0 = conj(plain_conj, GoalList0) ->
        list.append(GoalList0, Goals, GoalList)
    ;
        GoalList = [Goal0 | Goals]
    ),
    GoalExpr = conj(plain_conj, GoalList),
    goal_list_determinism(GoalList, Detism),
    goal_list_instmap_delta(GoalList, InstMapDelta),
    goal_list_nonlocals(GoalList, NonLocals),
    goal_info_set_determinism(Detism, GoalInfo1, GoalInfo2),
    goal_info_set_instmap_delta(InstMapDelta, GoalInfo2, GoalInfo),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred conjoin_goals_update_goal_infos(hlds_goal::in, hlds_goal::in,
    hlds_goal::out) is det.

conjoin_goals_update_goal_infos(Goal1, Goal2, Goal) :-
    ( Goal2 = hlds_goal(conj(plain_conj, Goals2), _) ->
        GoalList = Goals2
    ;
        GoalList = [Goal2]
    ),
    conjoin_goal_and_goal_list_update_goal_infos(Goal1, GoalList, Goal).

:- pred det_delete_nth(int::in, list(T)::in, list(T)::out) is det.

det_delete_nth(N, List0, List) :-
    list.det_split_list(N, List0, Left, Right),
    List = Left ++ det_tail(Right).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "dep_par_conj.m".

%-----------------------------------------------------------------------------%
:- end_module dep_par_conj.
%-----------------------------------------------------------------------------%
