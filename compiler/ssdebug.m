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
% Here is the transformation (note currently we don't do all of this)
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
%                <original body>,    % renaming outputs
%                ExitVarDescs = [ ... | CallVarDescs ],
%                (
%                    impure exit_port(ProcId, ExitVarDescs, DoRetryA),
%                    (
%                        DoRetryA = do_retry,
%                        p(...)
%                        % Will give same result as long as p is pure or
%                        % semipure.  Retry of impure procedures should probably
%                        % be disallowed anyway.
%                    ;
%                        DoRetryB = do_not_retry,
%                        % bind outputs
%                    )
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
:- import_module check_hlds.polymorphism.
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

process_proc(PredId, _ProcId, !ProcInfo, !ModuleInfo, !IO) :-
    proc_info_get_goal(!.ProcInfo, Goal0),

    some [!PredInfo, !Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),

        %
        % Make the ssdb_proc_id.
        %
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        make_proc_id_construction(!.PredInfo, !.ProcInfo, ProcIdGoals, 
            ProcIdVar, !Varset, !Vartypes),
        
        %
        % Get list(prog_var) and their type.
        %
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InitInstMap),
	
	%
        % Make a list which records the value for each of the head variables at
        % the call port.
        %
	make_call_list_arg(ssdb_call, InitInstMap, HeadVars, CallArgListVar, 
	    CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset, 
	    !Vartypes),

        %
        % Generate the call.
        % Generate the call to handle_event(call).
        % Return a list of goals.
        %
        make_handle_event_call(ssdb_call, ProcIdVar, CallArgListVar,
            HandleEventCallGoals, !ModuleInfo, !Varset, !Vartypes),
        
        %
        % Get the updated InstMap.
        %
        update_instmap(Goal0, InitInstMap, UpdatedInstMap),

	%
        % Make the variable list at the exit port. It's currently a completely 
        % new list instead of adding on to the list generated for the call 
        % port.
        %
        % XXX Optimization : Only output variables should be regenerated.
        %
	make_call_list_arg(ssdb_exit, UpdatedInstMap, HeadVars, ExitArgListVar, 
	    ExitArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset, 
	    !Vartypes),

        %
        % Generate the exit.
        % Generate the call to handle_event(exit).
        % Return a list of goals.
        %
        make_handle_event_call(ssdb_exit, ProcIdVar, ExitArgListVar,
            HandleEventExitGoals, !ModuleInfo, !Varset, !Vartypes),


        %
        % Place the call and exit events around the initial goal.
        %
        ConjGoals = ProcIdGoals ++ CallArgListGoals ++ HandleEventCallGoals ++ 
            [Goal0 | ExitArgListGoals] ++ HandleEventExitGoals,

        goal_info_init(GoalInfo),
        Goal = hlds_goal(conj(plain_conj, ConjGoals), GoalInfo),

        proc_info_set_varset(!.Varset, !ProcInfo),
        proc_info_set_vartypes(!.Vartypes, !ProcInfo),

        proc_info_set_goal(Goal, !ProcInfo),

        requantify_proc(!ProcInfo),
        recompute_instmap_delta_proc(yes, !ProcInfo, !ModuleInfo),

        module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo)
    ).



:- pred make_call_list_arg(ssdb_event_type::in, instmap::in, 
    list(prog_var)::in, prog_var::out, list(hlds_goal)::out, 
    module_info::in, module_info::out, proc_info::in, proc_info::out,
    pred_info::in, pred_info::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

make_call_list_arg(Event, InstMap, HeadVars, ArgListVar, ArgListGoals, 
    !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes) :-

    (    
        ( Event = ssdb_call
        ; Event = ssdb_exit
        ),
    
        %
        % Make the list of argument at call/exit point
        %
        make_arg_list(0, InstMap, HeadVars, ArgListVar, ArgListGoals,
            !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes)

    ;
        ( Event = ssdb_fail
        ; Event = ssdb_redo
        ),

        %
        % Make the list of argument at fail/redo point
        % Need only to generate the empty list.
        %
        make_arg_list(0, InstMap, HeadVars, ArgListVar, ArgListGoals,
            !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes)
    ).




:- pred make_handle_event_call(ssdb_event_type::in, prog_var::in, 
    prog_var::in, list(hlds_goal)::out, module_info::in, module_info::out, 
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

make_handle_event_call(Event, ProcIdVar, ArgListVar, Goals, !ModuleInfo, 
    !Varset, !Vartypes) :-

    make_ssdb_event_type_construction(Event, EventConstructor, EventVar, 
        !Varset, !Vartypes),

    %
    % Build the following goal
    %   handle_event(ProcId, Event, VarList).
    %
    SSDBModule = mercury_ssdb_builtin_module,
    Features = [],
    InstMapSrc = [],
    Context = term.context_init,
    goal_util.generate_simple_call(SSDBModule, "handle_event", 
    pf_predicate, only_mode, detism_det, purity_impure, 
    [ProcIdVar, EventVar, ArgListVar], Features, InstMapSrc, !.ModuleInfo,
    Context, HandleEventGoal),
        
    Goals = [EventConstructor, HandleEventGoal].

%-----------------------------------------------------------------------------%

    %
    % make_proc_id_construction(PredInfo, ProcInfo,
    %   Goals, Var, !Varset, !Vartypes)
    %
    % Returns a set of goals, Goals, which build the ssdb_proc_id structure
    % for the given pred and proc infos.  The var returned holds the
    % ssdb_proc_id.
    %
:- pred make_proc_id_construction(pred_info::in, proc_info::in,
    hlds_goals::out, prog_var::out,
    prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

make_proc_id_construction(PredInfo,
    _ProcInfo, Goals, ProcIdVar, !Varset, !Vartypes) :-
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

%-----------------------------------------------------------------------------%



    % make_arg_list(Pos, InstMap, Vars, ListVar, Goals, !Varset, !Vartypes)
    %
    % Processes each variable in Vars creating a list(var_value) which records
    % the value of each of the variables. ListVar points to the start of the
    % list and Goals is the list of goals to construct the list. Pos
    % indicates which argument position the first variable in Vars is.
    % InstMap is used to work out if the variable is instantiated enough yet
    % to display.
    %
:- pred make_arg_list(int::in, instmap::in, list(prog_var)::in, 
    prog_var::out, list(hlds_goal)::out, module_info::in, module_info::out, 
    proc_info::in, proc_info::out, pred_info::in, pred_info::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

make_arg_list(_, _, [], Var, [Goal], !ModuleInfo, !ProcInfo, !PredInfo, 
    !Varset, !Vartypes) :-
    
    svvarset.new_named_var("EmptyVarList", Var, !Varset), 
    svmap.det_insert(Var, list_var_value_type, !Vartypes),
    ConsId = cons(qualified(unqualified("list"), "[]" ), 0),
    construct_functor(Var, ConsId, [], Goal).

make_arg_list(Pos0, InstMap, [VarToInspect | ListCallVar], Var, Goals, 
    !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes) :-

    Pos = Pos0 + 1,

    make_arg_list(Pos, InstMap, ListCallVar, Var0, Goals0, 
        !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes),
    make_var_value(InstMap, VarToInspect, VarDesc, Pos0, ValueGoals, 
        !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes),
   
    svvarset.new_named_var("FullListVar", Var, !Varset), 
    svmap.det_insert(Var, list_var_value_type, !Vartypes),
    ConsId = cons(qualified(unqualified("list"), "[|]" ), 2),
    construct_functor(Var, ConsId, [VarDesc, Var0], Goal),
    
    %XXX Optimization : Unefficience problem with append
    Goals = Goals0 ++ ValueGoals ++ [Goal].


    %
    % Return the type list(var_value)
    %
:- func list_var_value_type = mer_type.

list_var_value_type = ListVarValueType :-
    SSDBModule = mercury_ssdb_builtin_module,
    VarValueTypeCtor = type_ctor(qualified(SSDBModule, "var_value"), 0),
    construct_type(VarValueTypeCtor, [], VarValueType),

    ListTypeCtor = type_ctor(qualified(unqualified("list"), "list"), 1),
    construct_type(ListTypeCtor, [VarValueType], ListVarValueType). 
    

%-----------------------------------------------------------------------------%

    %
    % Create the goal's argument description :
    % -> unbound_head_var(Name, Pos) if it is an unbound argument
    % -> bound_head_var(type_of_T, Name, Position, T) if it is a bound argument
    %
:- pred make_var_value(instmap::in, prog_var::in, prog_var::out, 
    int::in, list(hlds_goal)::out, module_info::in, module_info::out, 
    proc_info::in, proc_info::out, pred_info::in, pred_info::out, 
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.


make_var_value(InstMap, VarToInspect, VarDesc, VarPos, Goals, 
    !ModuleInfo, !ProcInfo, !PredInfo, !VarSet, !VarTypes) :-


    SSDBModule = mercury_ssdb_builtin_module,
    TypeCtor = type_ctor(qualified(SSDBModule, "var_value"), 0),

    % Find the name of the prog_var.
    varset.lookup_name(!.VarSet, VarToInspect, VarName),
    
    make_string_const_construction_alloc(VarName, yes("VarName"),
        ConstructVarName, VarNameVar, !VarSet, !VarTypes),

    make_int_const_construction_alloc(VarPos, yes("VarPos"),
        ConstructVarPos, VarPosVar, !VarSet, !VarTypes),

    ( var_is_ground_in_instmap(!.ModuleInfo, InstMap, VarToInspect) ->
                    
	%
	% Update proc_varset and proc_vartypes, without this, the
	% polymorphism_make_type_info_var uses a prog_var which is 
	% already bound.
	%
	proc_info_set_varset(!.VarSet, !ProcInfo),
	proc_info_set_vartypes(!.VarTypes, !ProcInfo),

	%
        % Create dynamic constructor for the value of the argument.
        %
        % Call polymorphism.m to create the type_infos, add an hidden field
        % which is the polymorphic type of the value.
        %
        % some[T] bound_head_var(string, int, T) ---->
        %   some[T] bound_head_var(type_of_T, string, int, T)
        %
	create_poly_info(!.ModuleInfo, !.PredInfo, !.ProcInfo, 
	    PolyInfo0),
	term.context_init(Context),
	map.lookup(!.VarTypes, VarToInspect, MerType),
	polymorphism_make_type_info_var(MerType, Context, TypeInfoVar, 
	    TypeInfoGoal, PolyInfo0, PolyInfo),
	poly_info_extract(PolyInfo, !PredInfo, !ProcInfo, 
	    !:ModuleInfo),
	
	%
	% Give a new prog_var to the polymorphic structure.
	%
	svvarset.new_named_var("VarType", VarTypo, !VarSet),
	svmap.det_insert(VarTypo, MerType, !VarTypes),

        % Constructor of the variable's description.
        svvarset.new_named_var("VarDesc", VarDesc, !VarSet), 
        ConsId = cons(qualified(SSDBModule, "bound_head_var"), 3),
        construct_type(TypeCtor, [], VarType), 
        svmap.det_insert(VarDesc, VarType, !VarTypes),
        construct_functor(VarDesc, ConsId, [TypeInfoVar, VarNameVar, 
            VarPosVar, VarToInspect], ConstructVarGoal),

        Goals = [ConstructVarName, ConstructVarPos | TypeInfoGoal] ++
            [ConstructVarGoal]
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
