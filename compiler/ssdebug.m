%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
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
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- module transform_hlds.ssdebug.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

    %
    % Place a call/exit event and the beginning/end of each procedure.
    %
:- pred ssdebug.process_proc(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module ssdb.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module svmap.
:- import_module svvarset.
:- import_module term.

%-----------------------------------------------------------------------------%

process_proc(_PredId, _ProcId, !ProcInfo, !ModuleInfo, !IO) :-
    proc_info_get_goal(!.ProcInfo, Goal0),

    some [!Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),
            
            %
            % Build the following two goals
            %   CallVar = ssdb_call,
            %   handle_event(CallVar)
            %
        make_ssdb_event_type_construction(ssdb_call,
            CallConstructor, CallVar, !Varset, !Vartypes),

        SSDBModule = mercury_ssdb_builtin_module,
        Features = [],
        InstMapSrc = [],
        Context = term.context_init,
        goal_util.generate_simple_call(SSDBModule, "handle_event",		
            pf_predicate, only_mode, detism_det, purity_impure, [CallVar],
            Features, InstMapSrc, !.ModuleInfo, Context, HandleCallEventGoal),		
            %
            % Build the following two goals
            %   ExitVar = ssdb_exit,
            %   handle_event(ExitVar)
            %
        make_ssdb_event_type_construction(ssdb_exit,
            ExitConstructor, ExitVar, !Varset, !Vartypes),

        goal_util.generate_simple_call(SSDBModule, "handle_event",		
            pf_predicate, only_mode, detism_det, purity_impure, [ExitVar],
            Features, InstMapSrc, !.ModuleInfo, Context, HandleExitEventGoal),		
            %
            % Place the call and exit events around the initial goal.
            % XXX we still need to extend this to handle the other event types
            %
        ConjGoals = [CallConstructor, HandleCallEventGoal,
            Goal0, ExitConstructor, HandleExitEventGoal],

        goal_info_init(GoalInfo),
        Goal = hlds_goal(conj(plain_conj, ConjGoals), GoalInfo),

        proc_info_set_varset(!.Varset, !ProcInfo),
        proc_info_set_vartypes(!.Vartypes, !ProcInfo)
    ),

    proc_info_set_goal(Goal, !ProcInfo),

    requantify_proc(!ProcInfo),
    recompute_instmap_delta_proc(yes, !ProcInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

    %
    % make_ssdb_event_type_construction(EventType,
    %   Goal, Var, !Varset, !Vartypes)
    % 
    % makes a construction unification, Goal, where Var will have the value
    % EventType, updating the varset and vartypes to reflect this new goal.
    %
:- pred make_ssdb_event_type_construction(
    ssdb_event_type::in, hlds_goal::out, prog_var::out, 
    prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

make_ssdb_event_type_construction(Event, Goal, EventVar, !Varset, !Vartypes) :-
    (
        Event = ssdb_call,
        SSDB_Event = "ssdb_call"
    ;
        Event = ssdb_exit,
        SSDB_Event = "ssdb_exit"
    ;
        Event = ssdb_redo,
        SSDB_Event = "ssdb_redo"
    ;
        Event = ssdb_fail,
        SSDB_Event = "ssdb_fail"
    ),
	
    SSDBModule = mercury_ssdb_builtin_module,
    TypeCtor = type_ctor(qualified(SSDBModule, "ssdb_event_type"), 0),

    svvarset.new_named_var(SSDB_Event, EventVar, !Varset), 
    ConsId = cons(qualified(SSDBModule, SSDB_Event), 0),
    construct_type(TypeCtor, [], EventVarType),	
    svmap.det_insert(EventVar, EventVarType, !Vartypes),
    construct_functor(EventVar, ConsId, [], Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
