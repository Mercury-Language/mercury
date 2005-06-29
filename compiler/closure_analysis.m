%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: closure_analysis.m
% main author: juliensf

% Perform local closure analysis on procedures.  This involves tracking
% the possible values that a higher-order variable can take within a 
% procedure.  We attach this information to places where knowing the
% possible values of a higher-order call may be useful.

% This is similar to the analysis done by higher-order specialization, except
% that here, we do care if a higher-order variable can take multiple values.

%-----------------------------------------------------------------------------%

:- module transform_hlds.closure_analysis.

:- interface.

:- import_module hlds.hlds_module.
:- import_module io.

:- pred process_module(module_info::in, module_info::out,
	io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.dependency_graph.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module svset.
:- import_module varset.

%----------------------------------------------------------------------------%

process_module(!ModuleInfo, !IO) :-
    %
    % XXX At the moment it is not necessary to do this on a per-SCC basis,
    % since the analysis is only procedure-local, but we would eventually
    % like to extend it.
    %
    globals.io_lookup_bool_option(debug_closure, Debug, !IO),
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
    list.foldl2(process_scc(Debug), SCCs, !ModuleInfo, !IO).

%----------------------------------------------------------------------------%
%
% Perform closure analysis on a SCC
%

:- pred process_scc(bool::in, list(pred_proc_id)::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

process_scc(Debug, SCC, !ModuleInfo, !IO) :-
    list.foldl2(process_proc(Debug), SCC, !ModuleInfo, !IO).

%----------------------------------------------------------------------------%

    % This type represents the possible values of a higher-order valued
    % variable.
    %
:- type closure_values
    --->    unknown
                % The higher-order variable may be bound to something
                % but we don't know what it is.
                
    ;       partial(set(pred_proc_id))
                % The higher-order variable may be bound to these
                % values, or it may be bound to something else we don't
                % know about.  (This is intended to be useful in producing
                % error messages for the termination analysis; if one
                % of the higher-order values is definitely non-terminating
                % we can certainly let the user know about it.)

    ;       exclusive(set(pred_proc_id)).
                % The higher-order variable will be exclusively bound
                % to this set of values.    

    % We attach a closure_info to each goal where it may be of interest;
    % at the moment calls and generic_calls.  
    % 
:- type closure_info == map(prog_var, closure_values).

%----------------------------------------------------------------------------%

:- func closure_info_init(module_info, vartypes, prog_vars, list(mode))
    = closure_info.

closure_info_init(ModuleInfo, VarTypes, HeadVars, ArgModes) = ClosureInfo :-
    partition_arguments(ModuleInfo, VarTypes, HeadVars, ArgModes,
        set.init, Inputs0, set.init, _Outputs),
    Inputs = set.filter(var_has_ho_type(VarTypes), Inputs0),
    set.fold(insert_unknown, Inputs, map.init, ClosureInfo).         

    % Succeeds iff the given variable has a higher-order type. 
    % 
:- pred var_has_ho_type(vartypes::in, prog_var::in) is semidet.

var_has_ho_type(VarTypes, Var) :-
    Type = map.lookup(VarTypes, Var),
    type_is_higher_order(Type). 

    % Insert the given prog_var into the closure_info and set the
    % possible values to unknown.
    %
:- pred insert_unknown(prog_var::in, closure_info::in, closure_info::out)
    is det.    

insert_unknown(Var, !ClosureInfo) :-
    svmap.det_insert(Var, unknown, !ClosureInfo).

%----------------------------------------------------------------------------%
%
% Perform local closure analysis on a procedure
%

:- pred process_proc(bool::in, pred_proc_id::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

process_proc(Debug, PPId, !ModuleInfo, !IO) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
    proc_info_headvars(ProcInfo0, HeadVars),
    proc_info_vartypes(ProcInfo0, VarTypes), 
    proc_info_argmodes(ProcInfo0, ArgModes),
    ClosureInfo0 = closure_info_init(!.ModuleInfo, VarTypes, HeadVars,
        ArgModes),
    write_proc_progress_message("% Analysing closures in ", PPId, !.ModuleInfo,
        !IO), 
    proc_info_goal(ProcInfo0, Body0),
    process_goal(VarTypes, !.ModuleInfo, Body0, Body,
        ClosureInfo0, _ClosureInfo),
    (
        Debug = yes,
        proc_info_varset(ProcInfo, Varset),
        dump_closure_info(Varset, Body, !IO),
        io.flush_output(!IO)
    ;
        Debug = no
    ),
    proc_info_set_goal(Body, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%
% 
% Track higher-order values through goals 
%
 
:- pred process_goal(vartypes::in, module_info::in,
    hlds_goal::in, hlds_goal::out, closure_info::in, closure_info::out) is det.

process_goal(VarTypes, ModuleInfo, Goal0, Goal, !ClosureInfo) :-
    Goal0 = conj(Goals0) - GoalInfo,
    list.map_foldl(process_goal(VarTypes, ModuleInfo), Goals0, Goals,
        !ClosureInfo),
    Goal = conj(Goals) - GoalInfo.
process_goal(VarTypes, ModuleInfo, Goal0, Goal, !ClosureInfo) :-
    Goal0 = GoalExpr - GoalInfo0,
    GoalExpr =  call(CallPredId, CallProcId, CallArgs, _, _, _),
    %
    % Look for any higher-order arguments and divide them
    % into sets of input and output arguments.
    %
    module_info_pred_proc_info(ModuleInfo, CallPredId, CallProcId,
        _CallPredInfo, CallProcInfo),
    proc_info_argmodes(CallProcInfo, CallArgModes),
    %
    % NOTE: we construct sets of arguments, rather than lists,
    %       in case there are duplicate arguments.
    %
    partition_arguments(ModuleInfo, VarTypes, CallArgs, CallArgModes,
        set.init, InputArgs, set.init, OutputArgs),
    % 
    % Update the goal_info to include any information about the
    % values of higher-order valued variables.
    %
    AddValues = (pred(Var::in, !.ValueMap::in, !:ValueMap::out) is det :-
        %
        % The closure_info won't yet contain any information about
        % higher-order outputs from this call.
        %
        ( map.search(!.ClosureInfo, Var, PossibleValues) ->
            (
                PossibleValues = unknown
            ;
                PossibleValues = partial(_)
            ;
                PossibleValues = exclusive(KnownValues),
                svmap.det_insert(Var, KnownValues, !ValueMap)
            )
        ;
            true
        )
    ),
    set.fold(AddValues, InputArgs, map.init, Values),
    goal_info_set_ho_values(Values, GoalInfo0, GoalInfo),
    %
    % Insert any information about higher-order
    % outputs from this call into the closure_info.
    %
    set.fold(insert_unknown, OutputArgs, !ClosureInfo),
    Goal = GoalExpr - GoalInfo.       
process_goal(VarTypes, ModuleInfo, Goal0, Goal, !ClosureInfo) :-
    Goal0 = GoalExpr - GoalInfo0,
    %    
    % XXX We should probably just ignore Aditi stuff and unsafe_casts
    % but annotating them with closure_infos won't hurt.
    %
    GoalExpr = generic_call(Details, GCallArgs, GCallModes, _),
    partition_arguments(ModuleInfo, VarTypes, GCallArgs, GCallModes,
        set.init, InputArgs0, set.init, OutputArgs),
    %
    % For higher-order calls we need to make sure that the actual higher-order
    % variable being called is also considered (it will typically be the
    % variable of interest).  This variable is not included in 'GCallArgs' so
    % we need to include in the set of input argument separately.  
    %
    ( Details = higher_order(CalledClosure0, _, _, _) ->
        svset.insert(CalledClosure0, InputArgs0, InputArgs)
    ;
        InputArgs = InputArgs0 
    ),         
    AddValues = (pred(Var::in, !.ValueMap::in, !:ValueMap::out) is det :-
        %
        % The closure_info won't yet contain any information about
        % higher-order outputs from this call.
        %
        ( map.search(!.ClosureInfo, Var, PossibleValues) ->
            (
                PossibleValues = unknown
            ;
                PossibleValues = partial(_)
            ;
                PossibleValues = exclusive(KnownValues),
                svmap.det_insert(Var, KnownValues, !ValueMap)
            )
        ;
            true
        )
    ),
    set.fold(AddValues, InputArgs, map.init, Values),
    goal_info_set_ho_values(Values, GoalInfo0, GoalInfo),
    %
    % Insert any information about higher-order
    % outputs from this call into the closure_info.
    %
    set.fold(insert_unknown, OutputArgs, !ClosureInfo),
    Goal = GoalExpr - GoalInfo.       
process_goal(VarTypes, ModuleInfo, Goal0, Goal, !ClosureInfo) :-
    Goal0 = switch(SwitchVar, SwitchCanFail, Cases0) - GoalInfo,  
    ProcessCase = (func(Case0) = Case - CaseInfo :-
        Case0 = case(ConsId, CaseGoal0),
        process_goal(VarTypes, ModuleInfo, CaseGoal0, CaseGoal,
          !.ClosureInfo, CaseInfo),
        Case = case(ConsId, CaseGoal)
    ),
    CasesAndInfos = list.map(ProcessCase, Cases0), 
    assoc_list.keys_and_values(CasesAndInfos, Cases, CasesInfo),     
    list.foldl(merge_closure_infos, CasesInfo, map.init, !:ClosureInfo),
    Goal  = switch(SwitchVar, SwitchCanFail, Cases) - GoalInfo.
process_goal(VarTypes, _, Goal, Goal, !ClosureInfo) :-
    Goal = unify(_, _, _, Unification, _) - _,
    ( 
        Unification = construct(LHS, RHS, _, _, _, _, _),
        ( 
            % NOTE: we don't bother worrying about features
            % that relate to Aditi, i.e. when EvalMethod = (aditi_bottom_up)
            RHS = pred_const(ShroudedPPId, EvalMethod),
            EvalMethod = normal 
        ->
            PPId = unshroud_pred_proc_id(ShroudedPPId),
            HO_Value = set.make_singleton_set(PPId),
            svmap.det_insert(LHS, exclusive(HO_Value), !ClosureInfo)
        ;
            true
        )
    ;   
        Unification = deconstruct(_, _, Args, _, _, _),
        %
        % XXX We don't currently support tracking the values of
        % closures that are stored in data structures.
        %
        HO_Args = list.filter(var_has_ho_type(VarTypes), Args),    
        list.foldl(insert_unknown, HO_Args, !ClosureInfo) 
    ;
        Unification = assign(LHS, RHS),
        ( var_has_ho_type(VarTypes, LHS) ->
            %
            % Sanity check: make sure the rhs is also a higher-order variable.
            %
            ( not var_has_ho_type(VarTypes, RHS) ->
                unexpected(this_file,
                    "not a higher-order var in process_goal_2")
            ;
                true
            ),
            Values = map.lookup(!.ClosureInfo, RHS),
            svmap.det_insert(LHS, Values, !ClosureInfo)
        ;
            true
        )
    ;
        Unification = simple_test(_, _)
    ;
        Unification = complicated_unify(_, _, _)
    ).
process_goal(VarTypes, ModuleInfo, Goal0, Goal, !ClosureInfo) :-
    Goal0 = disj(Goals0) - GoalInfo,
    ProcessDisjunct = (func(Disjunct0) = DisjunctResult :-
        process_goal(VarTypes, ModuleInfo, Disjunct0, Disjunct,
            !.ClosureInfo, ClosureInfoForDisjunct),
        DisjunctResult = Disjunct - ClosureInfoForDisjunct
    ),
    DisjunctsAndInfos = list.map(ProcessDisjunct, Goals0),
    assoc_list.keys_and_values(DisjunctsAndInfos, Goals, DisjunctsInfo),
    list.foldl(merge_closure_infos, DisjunctsInfo, map.init, !:ClosureInfo),
    Goal = disj(Goals) - GoalInfo.
process_goal(VarTypes, ModuleInfo, Goal0, Goal, !ClosureInfo) :-
    Goal0 = not(NegatedGoal0) - GoalInfo,
    process_goal(VarTypes, ModuleInfo, NegatedGoal0, NegatedGoal,
        !.ClosureInfo, _),
    Goal  = not(NegatedGoal) - GoalInfo.
process_goal(VarTypes, ModuleInfo, Goal0, Goal, !ClosureInfo) :-
    Goal0 = scope(Reason, ScopedGoal0) - GoalInfo,
    process_goal(VarTypes, ModuleInfo, ScopedGoal0, ScopedGoal, !ClosureInfo),
    Goal  = scope(Reason, ScopedGoal) - GoalInfo.
process_goal(VarTypes, ModuleInfo, Goal0, Goal, !ClosureInfo) :-
    Goal0 = if_then_else(ExistQVars, If0, Then0, Else0) - GoalInfo,
    process_goal(VarTypes, ModuleInfo, If0,   If,   !.ClosureInfo, IfInfo),
    process_goal(VarTypes, ModuleInfo, Then0, Then, IfInfo, IfThenInfo),
    process_goal(VarTypes, ModuleInfo, Else0, Else, !.ClosureInfo, ElseInfo),
    map.union(merge_closure_values, IfThenInfo, ElseInfo, !:ClosureInfo),
    Goal = if_then_else(ExistQVars, If, Then, Else) - GoalInfo.
process_goal(_, ModuleInfo, Goal0, Goal, !ClosureInfo) :-
    %
    % XXX 'ExtraArgs' should probably be ignored here since it is only
    % used by the tabling transformation.
    %
    % XXX We may eventually want to annotate foreign_procs with
    % clousure_infos as well.  It isn't useful at the moment however.
    %
    Goal0 = GoalExpr - GoalInfo,
    GoalExpr = foreign_proc(_, _, _, Args, _ExtraArgs, _),
    ForeignHOArgs = (pred(Arg::in, Out::out) is semidet :-
        Arg = foreign_arg(Var, NameMode, Type),
        %
        % A 'no' here means that the foreign argument is unused.
        %
        NameMode = yes(_ - Mode),
        mode_util.mode_is_output(ModuleInfo, Mode),
        type_is_higher_order(Type),
        Out = Var - unknown
    ),
    list.filter_map(ForeignHOArgs, Args, OutputForeignHOArgs),
    svmap.det_insert_from_assoc_list(OutputForeignHOArgs, !ClosureInfo),
    Goal = GoalExpr - GoalInfo. 
process_goal(VarTypes, ModuleInfo, Goal0, Goal, !ClosureInfo) :-
    Goal0 = par_conj(Goals0) - GoalInfo,
    list.map_foldl(process_goal(VarTypes, ModuleInfo), Goals0, Goals,
        !ClosureInfo),
    Goal = par_conj(Goals) - GoalInfo.
process_goal(_, _, shorthand(_) - _, _, _, _) :-
    unexpected(this_file, "shorthand/1 goal during closure analysis.").

%----------------------------------------------------------------------------%

:- pred partition_arguments(module_info::in, vartypes::in,
    prog_vars::in, list(mode)::in,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det.

partition_arguments(_, _, [],    [], !Inputs, !Outputs).
partition_arguments(_, _, [_|_], [], _, _, _, _) :-
    unexpected(this_file, "partition_arguments/7 unequal length lists.").
partition_arguments(_, _, [],    [_|_], _, _, _, _) :-
    unexpected(this_file, "partition_arguments/7 unequal length lists.").
partition_arguments(ModuleInfo, VarTypes, [ Var | Vars ], [ Mode | Modes ],
        !Inputs, !Outputs) :-
    ( var_has_ho_type(VarTypes, Var) ->
        ( mode_is_input(ModuleInfo, Mode) ->
            svset.insert(Var, !Inputs)
        ; mode_is_output(ModuleInfo, Mode) ->
            svset.insert(Var, !Outputs)
        ;
            true
        )
    ;
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
% Debugging code (used by '--debug-closure' option)
%

:- pred dump_closure_info(prog_varset::in, hlds_goal::in,
    io::di, io::uo) is det.

dump_closure_info(Varset, conj(Goals) - _, !IO) :-
    list.foldl(dump_closure_info(Varset), Goals, !IO).
dump_closure_info(Varset, par_conj(Goals) - _, !IO) :-
    list.foldl(dump_closure_info(Varset), Goals, !IO).
dump_closure_info(Varset, call(_,_,_,_,_,_) - GoalInfo, !IO) :-
    dump_ho_values(GoalInfo, Varset, !IO).
dump_closure_info(Varset, generic_call(_,_,_,_) - GoalInfo, !IO) :-
    dump_ho_values(GoalInfo, Varset, !IO).
dump_closure_info(Varset, scope(_, Goal) - _, !IO) :-
    dump_closure_info(Varset, Goal, !IO).
dump_closure_info(Varset, switch(_, _, Cases) - _, !IO) :-
    CaseToGoal = (func(case(_, Goal)) = Goal),
    Goals = list.map(CaseToGoal, Cases),
    list.foldl(dump_closure_info(Varset), Goals, !IO).
dump_closure_info(Varset, if_then_else(_, If, Then, Else) - _, !IO) :-
    list.foldl(dump_closure_info(Varset), [If, Then, Else], !IO).
dump_closure_info(_, unify(_,_,_,_,_) - _, !IO).
dump_closure_info(Varset, not(Goal) - _, !IO) :-
    dump_closure_info(Varset, Goal, !IO).
dump_closure_info(_, foreign_proc(_,_,_,_,_,_) - _, !IO).
dump_closure_info(Varset, disj(Goals) - _, !IO) :-
    list.foldl(dump_closure_info(Varset), Goals, !IO).
dump_closure_info(_, shorthand(_) - _, _, _) :-
    unexpected(this_file, "shorthand goal encountered.\n").

:- pred dump_ho_values(hlds_goal_info::in, prog_varset::in,
    io::di, io::uo) is det.

dump_ho_values(GoalInfo, Varset, !IO) :-
    HO_Values = goal_info_get_ho_values(GoalInfo),
    ( not map.is_empty(HO_Values) ->
        goal_info_get_context(GoalInfo, Context),
        prog_out.write_context(Context, !IO),
        io.nl(!IO),
        map.foldl(dump_ho_value(Varset), HO_Values, !IO)
    ;
        true
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

:- func this_file = string.

this_file = "closure_analysis.m".

%----------------------------------------------------------------------------%
:- end_module closure_analysis.
%----------------------------------------------------------------------------%
