:- module debugger.

:- interface.
:- import_module hlds, io.

:- pred the_debugger(module_info, io__state, io__state).
:- mode the_debugger(in,di,uo) is det.

:- implementation.
%  modules in compiler directory
:- import_module hlds_out, prog_io, mercury_to_mercury.
:- import_module mode_util, prog_util, debug_util.
%  modules in library directory
:- import_module map, list, term, std_util, assoc_list, varset, require, string.

% already defined in debug_util.m
% :- type highinfo ---> highinfo(module_info,list(var)).

the_debugger(HLDS) -->  
	{ module_info_get_predicate_table(HLDS,PredTable) },
	{ module_info_preds(HLDS,Ptable) },
	({ predicate_table_search_name_arity(PredTable,"main",2,PredIdList) } ->
	  print_list(PredIdList,Ptable,HLDS)
	;
	  io__write_string("Error: Can't find pred main\n") 
	).

:- pred print_list(list(int),pred_table,module_info,io__state,io__state).
:- mode print_list(in,in,in,di,uo) is det.

print_list([],_,_) --> [].
print_list([PredId|_PredIds],Ptable,HLDS) -->
	{ map__lookup(Ptable,PredId,PredInfo) },
	{ pred_info_arg_types(PredInfo,TVarSet,ArgTypes) },
	{ pred_info_context(PredInfo,Context) },
	{ pred_info_procedures(PredInfo,ProcTable) },
	{ pred_info_name(PredInfo,PredName) },
	io__write_string("mercury_output_pred_type\n"),
	mercury_output_pred_type(TVarSet,unqualified(PredName),ArgTypes,
		no,Context),
	{ pred_info_clauses_info(PredInfo,ClausesInfo) },
	{ ClausesInfo = clauses_info(VarSet,VarTypes,_HeadVars,_Clauses) },
	io__write_string("debug__write_var_types\n"),
	debug__write_var_types(0,VarSet,VarTypes,TVarSet),
	{ map__keys(ProcTable,ProcIds) },
	print_proc_list(ProcIds,ProcTable,HLDS).
	% print_list(PredIds,Ptable,HLDS).

:- pred print_proc_list(list(proc_id),proc_table,module_info,io__state,io__state).
:- mode print_proc_list(in,in,in,di,uo) is det.

print_proc_list([],_,_) --> [].
print_proc_list([ProcId|_ProcIds],ProcTable,HLDS) --> 
	{ map__lookup(ProcTable,ProcId,ProcInfo) } ,
	{ proc_info_inferred_determinism(ProcInfo,InferredDeterminism) },
	{ proc_info_variables(ProcInfo,VarSet) },
	{ proc_info_goal(ProcInfo, GoalPair) },
	{ proc_info_headvars(ProcInfo, HeadVars) },
        { proc_info_argmodes(ProcInfo, ArgModes) },
	{ assoc_list__from_corresponding_lists(HeadVars,ArgModes,
		HeadVarsPair) },
	io__write_string("Inferred Determinism is "),
	hlds_out__write_determinism(InferredDeterminism),
	io__write_string("\n"),
	hlds_out__write_goal(GoalPair, HLDS, VarSet, 0 ),
	( { InferredDeterminism = det },
          io__write_string("\n*** Executing det ***\n"),
	  % Get a list of headvars in the head of the predicate
	  { do_get_in_headvarlist(HLDS,HeadVarsPair,Headinlist) },
	  { INFO = highinfo(HLDS,Headinlist) },
	  do_goal_det(GoalPair,INFO,VarSet,VarSetOut,_Result),
	  % display the output of main()
	  io__write_string("\n*** Output of Main is ***\n"),
	  do_output_goal_args(HLDS,VarSetOut,HeadVarsPair)
	; % SEMIDET
	  { InferredDeterminism = semidet },
	  io__write_string("\n*** Executing semidet ***\n"),
	  % Get a list of headvars in the head of the predicate
	  { do_get_in_headvarlist(HLDS,HeadVarsPair,Headinlist) },
	  { INFO = highinfo(HLDS,Headinlist) },
	  do_goal_semidet(GoalPair,INFO,VarSet,VarSetOut,Result),
	  % display the output of main()
	  io__write_string("\n*** Output of Main is ***\n"),
	  ( { Result = yes },
	    do_output_goal_args(HLDS,VarSetOut,HeadVarsPair)
	  ;
 	    { Result = no },
	    io__write_string("\nSemidet returns failure\n")
	  )
	; % NONDET
	  { InferredDeterminism = nondet },
	  []
	; 
	  { InferredDeterminism = multidet },
	  []
	; 
	  { InferredDeterminism = erroneous },
	  io__write_string("Execution returns erroneous\n") 
	; 
	  { InferredDeterminism = failure },
	  io__write_string("Execution returns failure\n")
	).
	% print_proc_list(ProcIds,ProcTable,HLDS).

:- pred debug__write_var_types(int, varset, map(var, type), varset,
					io__state, io__state).
:- mode debug__write_var_types(in, in, in, in, di, uo) is det.

debug__write_var_types(Indent, VarSet, VarTypes, TVarSet) -->
	{ map__keys(VarTypes, Vars) },
	debug__write_var_types_2(Vars, Indent, VarSet, VarTypes, TVarSet).

:- pred debug__write_var_types_2(list(var), int, varset, map(var, type),
					varset, io__state, io__state).
:- mode debug__write_var_types_2(in, in, in, in, in, di, uo) is det.

debug__write_var_types_2([], _, _, _, _) --> [].
debug__write_var_types_2([Var | Vars], Indent, VarSet, VarTypes, TypeVarSet)
		-->
	{ map__lookup(VarTypes, Var, Type) },
	io__write_string("% "),
	mercury_output_var(Var, VarSet),
	io__write_string(" (number "),
	{ term__var_to_int(Var, VarNum) },
	io__write_int(VarNum),
	io__write_string(")"),
	io__write_string(" :: "),
	mercury_output_term(Type, TypeVarSet),
	io__write_string("\n"),
	debug__write_var_types_2(Vars, Indent, VarSet, VarTypes, TypeVarSet).

% Process each goal deterministicly
:- pred do_goal_det(hlds__goal,highinfo,varset,varset,bool,
	io__state,io__state).
:- mode do_goal_det(in,in,in,out,out,di,uo) is det.

do_goal_det(Goal - _GoalInfo,INFO,VarSet,VarSetOut,Result) --> 
	do_goal2_det(Goal,INFO,VarSet,VarSetOut,Result).

:- pred do_goal2_det(hlds__goal_expr,highinfo,varset,varset,bool,
	io__state,io__state).
:- mode do_goal2_det(in,in,in,out,out,di,uo) is det.

do_goal2_det(switch(Var,CanFail,Cases),INFO,VarSet,VarSetOut,Result) --> 
	( { CanFail = cannot_fail },
	  io__write_string("In det switch\n"),
	  { do_lookup_var(VarSet,Var,Term) },
	  det_switch(Term,Cases,INFO,VarSet,VarSetOut,Result)
	;
	  { CanFail = can_fail },
 	  { error("do_gaol2_det: switch cannot fail in det\n") }
	).
:- pred det_switch(term,list(case),highinfo,varset,varset,bool,
	io__state,io__state).
:- mode det_switch(in,in,in,in,out,out,di,uo) is det.
det_switch(_T,[],_INFO,VarSet,VarSet,no) -->
  { error("det_switch: In det switch you cannot fail.\n") }.
det_switch(Term,[Case|Cases],INFO,VarSet,VarSetOut,Result) -->
	{ Case = case(Cons_id,GoalPair) },
	( det_match_cons_id(Term,Cons_id,VarSet,VarSetOut1) ->
	  % do goal GoalPair
	  io__write_string("det_match return yes\n"),
	  { VarSetOut = VarSetOut1 },
	  { Result = yes }
	;
	  io__write_string("det_match return no\n"),
	  det_switch(Term,Cases,INFO,VarSet,VarSetOut,Result)
	).	
:- pred det_match_cons_id(term,cons_id,varset,varset,io__state,io__state).
:- mode det_match_cons_id(in,in,in,out,di,uo) is semidet.
det_match_cons_id(Term,Cons_id,VarSet,VarSetOut) -->
	( { Cons_id = cons(String,Arity) },
	  io__write_string("cons\n"),
	  io__write_string("Var = "),
	  what_term(Term),
	  { Term = term__functor(TermS,_,_) },
	  io__write_string(" S= "),
	  io__write_string(String),
	  io__write_string(" arity= "),
	  io__write_int(Arity),
	  io__write_string("\n"),
	  { VarSetOut = VarSet }
	;
	  { Cons_id = int_const(Int) },
	  io__write_string("Int\n"),
	  { Term = term__functor(Const,[],_) }, 
	  { Const = term__integer(INT) },
	  { INT = Int },
	  { VarSetOut = VarSet } 
	;
	  { Cons_id = string_const(String) },
	  io__write_string("String\n"),
	  { VarSetOut = VarSet },
	  { true }
	;
	  { Cons_id = float_const(Float) },
	  io__write_string("Float\n"),
	  { VarSetOut = VarSet },
	  { true }
	;
	  { Cons_id = pred_const(_Predid,_Procid) },
	  io__write_string("pred_const\n"),
	  { VarSetOut = VarSet },
	  { true } % not used anymore
	;
	  { Cons_id = address_const(_Predid,_Procid) },
	  io__write_string("address_const\n"),
	  { VarSetOut = VarSet },
	  { true } % not used anymore
	). 

do_goal2_det(some(_,_),_INFO,V,V,yes) --> [].
do_goal2_det(if_then_else(_V,A,B,C),INFO,VarSet,VarSetOut,Result) --> 
	do_goal_det(A,INFO,VarSet,VarSetOut1,Result1),
	( { Result1 = yes } ->
	  do_goal_det(B,INFO,VarSetOut1,VarSetOut,Result)
	;
	  do_goal_det(C,INFO,VarSet,VarSetOut,Result)
	).
do_goal2_det(not(_),_INFO,V,V,no) --> 
	{ error("There should not be a not() in det predicate\n") }.
do_goal2_det(conj(List),INFO,VarSet,VarSetOut,Result) --> 
	( { List = [Goal|Goals] },
	  { list__length(List,L) },
	  io__write_string("Conj have list of "),
	  io__write_int(L),
	  io__write_string("\n"),
	  do_goal_det(Goal,INFO,VarSet,VarSet1,Result1),
	  ( { Result1 = yes } ->
	    do_goal2_det_conj(Goals,INFO,VarSet1,VarSetOut,Result)
	  ;
	    { Result = no },
	    { VarSetOut = VarSet }
	  )
	;
	  { List = [] },
	  { Result = yes },
	  { VarSetOut = VarSet }
	).
do_goal2_det(disj(_),_INFO,V,V,no) --> 
	{ error("There should not be a disj in det predicate\n") }.
do_goal2_det(call(PredId,ProcId,Vars,_isbuiltin,PredName,_followvars)
	,INFO,VarSet,VarSetOut,Result) 
    -->	{ INFO = highinfo(HLDS,_Headinlist) },
	{ unqualify_name(PredName,Name) },
	% obtain the ProcInfo, ProcVarSet and Goal
	{ module_info_preds(HLDS,Ptable) },
	{ map__lookup(Ptable,PredId,PredInfo) },
	{ pred_info_procedures(PredInfo,ProcTable) },
	{ map__lookup(ProcTable,ProcId,ProcInfo) },
	{ proc_info_inferred_determinism(ProcInfo,InferredDeterminism) },
	{ proc_info_variables(ProcInfo,ProcVarSet) },
	{ proc_info_goal(ProcInfo,GoalPair) },
	% Obtain the HeadVarsPair and Headlist
	{ proc_info_headvars(ProcInfo,HeadVars) },
	{ proc_info_argmodes(ProcInfo,ArgModes) },
	{ assoc_list__from_corresponding_lists(HeadVars,ArgModes,
	  HeadVarsPair) },
	{ assoc_list__from_corresponding_lists(HeadVarsPair,Vars,
	  Headlist) },
	% Get a list of head variables with "in" mode 
	{ do_get_in_headvarlist(HLDS,HeadVarsPair,Headinlist) },
	{ NEWINFO = highinfo(HLDS,Headinlist) },
	% Bind the headvars whose mode is "in" with constant values
	do_bind_headvars(Headlist,HLDS,VarSet,ProcVarSet,ProcVarSetOut),
	io__write_string("Entering Pred "), io__write_string(Name),
	( { InferredDeterminism = det },
	  io__write_string(" (det)\n"),
	  do_goal_det(GoalPair,NEWINFO,ProcVarSetOut,ResultVarSet,Result),
	  % Obtain the result values of the headvars with "out" mode
	  % and bind them to the coresponding variables
          bind_out_headvars(Headlist,HLDS,VarSet,ResultVarSet,VarSetOut)
	;
	  { InferredDeterminism = semidet },
	  io__write_string(" (semidet)\n"),
	  do_goal_semidet(GoalPair,NEWINFO,ProcVarSetOut,ResultVarSet,Result),
	  % Obtain the result values of the headvars with "out" mode
	  % and bind them to the coresponding variables
	  ( { Result = yes } ->
            bind_out_headvars(Headlist,HLDS,VarSet,ResultVarSet,VarSetOut)
	  ; % Result = no therefore do not bind any values of headvars
	    { VarSetOut = VarSet }
	  )
	;
	  { InferredDeterminism = nondet },
	  io__write_string(" (nondet)\n"),
	  { VarSetOut = VarSet },
	  { Result = yes }
	;
	  { InferredDeterminism = multidet },
	  io__write_string(" (multidet)\n"),
	  { VarSetOut = VarSet },
	  { Result = yes }
	;
	  { InferredDeterminism = erroneous },
	  io__write_string(" (error)\n"),
	  { VarSetOut = VarSet },
	  { Result = no }
	;
	  { InferredDeterminism = failure },
	  io__write_string(" (failure)\n"),
	  { VarSetOut = VarSet },
	  { Result = no }
	),
	io__write_string("Leaving Pred "), io__write_string(Name),
	io__write_string("\n").

do_goal2_det(unify(A,B,_,Unification,_),INFO,VarSet,VarSetOut,Result) -->
	{ INFO = highinfo(_HLDS,Headinlist) },
	( { Unification = assign(_,_) },
	  ( { A= term__variable(Var) } ->
	    ( { list__member(Var,Headinlist) } ->
	      ( { B= term__variable(VarB) },
		{ do_termvar2termconst(VarSet,A,Aconst) },
		{ varset__bind_var(VarSet,VarB,Aconst,VarSetOut) },
		{ Result = yes },
		mercury_output_var(VarB,VarSet),
		io__write_string(" is assign "),
		mercury_output_var(Var,VarSet),
		io__write_string(" = "),
		mercury_output_term(Aconst,VarSet),
		io__write_string("\n")
	      ;
		{ B= term__functor(_,_,_) },
		io__write_string("A is a headvar but B is a term__functor\n"),
		{ Result = yes },
		{ VarSetOut = VarSet }
	      ) 
	    ;
	      { do_lookup_termvarname(VarSet,B,Bname) },
	      { do_termvar2termconst(VarSet,B,Bconst) },
	      { varset__bind_var(VarSet,Var,Bconst,VarSetOut) },
	      { Result = yes },
	      mercury_output_var(Var,VarSet),
	      io__write_string(" is assign "),
	      io__write_string(Bname), io__write_string(" = "),
	      mercury_output_term(Bconst,VarSet),
	      io__write_string("\n")
	    ) 
	  ;
	    { Result = yes },
	    { VarSetOut = VarSet },
	    { error("Assignment to non-variable\n") }
	  ) 
	;
	  { Unification = simple_test(_,_) },
	  io__write_string("simple test\n"),
	  { Result = yes },
	  { VarSetOut = VarSet }
	;  
	  { Unification = construct(_,_,_,_) },
	  ( { A= term__variable(Var) } ->
	    { do_termvar2termconst(VarSet,B,Bconst) },
	    { varset__bind_var(VarSet,Var,Bconst,VarSetOut) },
	    { Result = yes },
	    mercury_output_var(Var,VarSet),
	    io__write_string(" is construct "),
	    mercury_output_term(Bconst,VarSet),
	    io__write_string("\n") 
	  ;
	    { Result = yes },
	    { VarSetOut = VarSet },
	    { error("Assignment to non-variable\n") }
	  )
	;
	  { Unification = deconstruct(_,_,_,_,_) },
	  io__write_string("deconstruct\n"),
	  { Result = yes },
	  { VarSetOut = VarSet }
	;
	  { Unification = complicated_unify(_,_,_) },
	  io__write_string("complicated unify\n"),
	  { Result = yes },
	  { VarSetOut = VarSet }
	).  

:- pred do_goal2_det_conj(list(hlds__goal),highinfo,varset,varset,bool,
	io__state,io__state).
:- mode do_goal2_det_conj(in,in,in,out,out,di,uo) is det.

do_goal2_det_conj([],_INFO,V,V,yes) --> [].
do_goal2_det_conj([Goal|Goals],INFO,VarSet,VarSetOut,Result) --> 
	do_goal_det(Goal,INFO,VarSet,VarSet1,Result1),
	( { Result1 = yes } ->
	  do_goal2_det_conj(Goals,INFO,VarSet1,VarSetOut,Result)
	;
	  { Result = no },
	  { VarSetOut = VarSet }
	).
 
:- pred do_output_goal_args(module_info,varset,list(pair(var,mode)),io__state,io__state).
:- mode do_output_goal_args(in,in,in,di,uo) is det.

do_output_goal_args(_,_,[]) --> [].
do_output_goal_args(HLDS,VarSet,[PairHeadVar|PairHeadVars]) -->
	do_output_goal_arg(HLDS,VarSet,PairHeadVar),
	do_output_goal_args(HLDS,VarSet,PairHeadVars).

:- pred do_output_goal_arg(module_info,varset,pair(var,mode),io__state,io__state).
:- mode do_output_goal_arg(in,in,in,di,uo) is det.

do_output_goal_arg(HLDS,VarSet,PairHeadVar) -->
	{ PairHeadVar = HeadVar - Mode },
	( { mode_is_output(HLDS,Mode) } ->
	  mercury_output_var(HeadVar,VarSet),
	  io__write_string(" = "),
	  ( { varset__lookup_var(VarSet,HeadVar,TermValue) } ->
	    mercury_output_term(TermValue,VarSet),
	    io__write_string("\n")
	  ;
	    io__write_string(" have no value in varset\n")
	  )
	;
	  []
	).

:- pred do_get_in_headvarlist(module_info,list(pair(var,mode)),list(var)).
:- mode do_get_in_headvarlist(in,in,out) is det.

do_get_in_headvarlist(HLDS,HeadVarsPair,Headinlist) :-
	do_get_in_headvarlist2(HLDS,HeadVarsPair,[],Headinlist).

:- pred do_get_in_headvarlist2(module_info,list(pair(var,mode)),list(var),
	list(var)).
:- mode do_get_in_headvarlist2(in,in,in,out) is det.

do_get_in_headvarlist2(_,[],L,L).
do_get_in_headvarlist2(HLDS,[PairHeadVar|PairHeadVars],Listin,Listout) :-
	do_get_in_headvarlist3(HLDS,PairHeadVar,Listin,Listin1),
	do_get_in_headvarlist2(HLDS,PairHeadVars,Listin1,Listout).

:- pred do_get_in_headvarlist3(module_info,pair(var,mode),list(var),list(var)).
:- mode do_get_in_headvarlist3(in,in,in,out) is det.

do_get_in_headvarlist3(HLDS,PairHeadVar,Listin,Listout) :-
	PairHeadVar = HeadVar - Mode,
	( mode_is_input(HLDS,Mode) ->
	  list__append([HeadVar],Listin,Listout)
	;
	  Listout = Listin
	).

:- pred do_bind_headvars(list(pair(pair(var,mode),var)),
	module_info,varset,varset,varset,
	io__state,io__state).
:- mode do_bind_headvars(in,in,in,in,out,di,uo) is det.

do_bind_headvars([],_HLDS,_VarSet,P,P) --> [].

do_bind_headvars([Arg|Args],HLDS,VarSet,ProcVarSet,ProcVarSetOut) -->
	{ Arg = TOP - Var },
	{ TOP = HeadVar - Mode },
	( { mode_is_input(HLDS,Mode) } ->
	  { do_lookup_varname(VarSet,Var,Vname) },
	  ( { varset__lookup_var(VarSet,Var,Term) } ->
	    { do_termvar2termconst(VarSet,Term,Tconst) },
	    { varset__bind_var(ProcVarSet,HeadVar,Tconst,ProcVarSet1) },
	    mercury_output_var(HeadVar,ProcVarSet),
	    io__write_string(" is assign "),
	    io__write_string(Vname), io__write_string(" = "),
	    mercury_output_term(Tconst,VarSet),
	    io__write_string("\n"),
	    do_bind_headvars(Args,HLDS,VarSet,ProcVarSet1,ProcVarSetOut)
	  ;
	    { error("All variable have to have a term in do_bind_headvars\n") }
	  )
	;
	  do_bind_headvars(Args,HLDS,VarSet,ProcVarSet,ProcVarSetOut)
	).


:- pred bind_out_headvars(list(pair(pair(var,mode),var)),
	module_info,varset,varset,varset,
	io__state,io__state).
:- mode bind_out_headvars(in,in,in,in,out,di,uo) is det.

bind_out_headvars([],_H,V,_R,V) --> [].
bind_out_headvars([Arg|Args],HLDS,VarSet,ResultVarSet,VarSetOut) -->
	{ Arg = TOP - Var },
	{ TOP = HeadVar - Mode },
	( { mode_is_output(HLDS,Mode) } ->
	  { do_lookup_var(ResultVarSet,HeadVar,TermValue) },
	  { do_termvar2termconst(ResultVarSet,TermValue,Tconst) },
	  { varset__bind_var(VarSet,Var,Tconst,VarSetOut1) },
	  mercury_output_var(Var,VarSet),
	  io__write_string(" is assign "),
	  mercury_output_var(HeadVar,ResultVarSet), io__write_string(" = "),
	  mercury_output_term(Tconst,ResultVarSet),
	  io__write_string("\n"),
	  bind_out_headvars(Args,HLDS,VarSetOut1,ResultVarSet,VarSetOut)
	;
	  bind_out_headvars(Args,HLDS,VarSet,ResultVarSet,VarSetOut)
	).


:- pred do_goal_semidet(hlds__goal,highinfo,varset,varset,bool,io__state,
	io__state).
:- mode do_goal_semidet(in,in,in,out,out,di,uo) is det.

do_goal_semidet(Goal - _GoalInfo,INFO,VarSet,VarSetOut,Result) -->
	do_goal2_semidet(Goal,INFO,VarSet,VarSetOut,Result).

:- pred do_goal2_semidet(hlds__goal_expr,highinfo,varset,varset,bool,
	io__state,io__state).
:- mode do_goal2_semidet(in,in,in,out,out,di,uo) is det.

do_goal2_semidet(switch(_,_,_),_INFO,V,V,no) --> [].	
do_goal2_semidet(some(_,_),_INFO,V,V,no) --> [].
do_goal2_semidet(if_then_else(_Vars,A,B,C),INFO,VarSet,VarSetOut,Result) --> 
	do_goal_semidet(A,INFO,VarSet,VarSetOut1,Result1),
	( { Result1 = yes } ->
	  do_goal_semidet(B,INFO,VarSetOut1,VarSetOut,Result)
	;
	  do_goal_semidet(C,INFO,VarSet,VarSetOut,Result)
	).
do_goal2_semidet(not(GoalPair),INFO,VarSet,VarSetOut,Result) --> 
	do_goal_semidet(GoalPair,INFO,VarSet,VarSetOut,Result1),
	( { Result1 = no } ->
	  io__write_string("Not returns TRUE\n"),
	  { Result = yes }
	;
	  io__write_string("Not returns FALSE\n"),
	  { Result = no }
	).	
do_goal2_semidet(conj(List),INFO,VarSet,VarSetOut,Result) --> 
	( { List = [Goal|Goals] },
	  { list__length(List,L) },
	  io__write_string("Conj have list of "),
	  io__write_int(L),
	  io__write_string("\n"),
	  do_goal_semidet(Goal,INFO,VarSet,VarSet1,Result1),
	  ( { Result1 = yes },
	    do_goal2_semidet_conj(Goals,INFO,VarSet1,VarSetOut,Result)
	  ;
	    { Result1 = no },
	    { Result = no },
	    { VarSetOut = VarSet }
	  )
	; % empty list
	  { List = [] },
	  { VarSetOut = VarSet },
	  { Result = yes } % empty conjunction is yes
	).
do_goal2_semidet(disj(_),_INFO,V,V,no) --> 
	{ error("There should not be a disj in semidet predicate\n") }.
do_goal2_semidet(call(PredId,ProcId,Vars,_isbuiltin,PredName,_followvars)
	,INFO,VarSet,VarSetOut,Result) 
    -->	{ INFO = highinfo(HLDS,_Headinlist) },
	{ unqualify_name(PredName,Name) },
	% obtain the ProcInfo, ProcVarSet and Goal
	{ module_info_preds(HLDS,Ptable) },
	{ map__lookup(Ptable,PredId,PredInfo) },
	{ pred_info_procedures(PredInfo,ProcTable) },
	{ map__lookup(ProcTable,ProcId,ProcInfo) },
	{ proc_info_inferred_determinism(ProcInfo,InferredDeterminism) },
	{ proc_info_variables(ProcInfo,ProcVarSet) },
	{ proc_info_goal(ProcInfo,Goal) },
	% Obtain the HeadVarsPair and Headlist
	{ proc_info_headvars(ProcInfo,HeadVars) },
	{ proc_info_argmodes(ProcInfo,ArgModes) },
	{ assoc_list__from_corresponding_lists(HeadVars,ArgModes,
	  HeadVarsPair) },
	{ assoc_list__from_corresponding_lists(HeadVarsPair,Vars,
	  Headlist) },
	% Get a list of head variables with "in" mode 
	{ do_get_in_headvarlist(HLDS,HeadVarsPair,Headinlist) },
	{ NEWINFO = highinfo(HLDS,Headinlist) },
	% Bind the headvars whose mode is "in" with constant values
	do_bind_headvars(Headlist,HLDS,VarSet,ProcVarSet,ProcVarSetOut),
	io__write_string("Entering Pred "), io__write_string(Name),
	io__write_string("\n"),
	( { InferredDeterminism = det },
	  io__write_string(" (det)\n"),
	  do_goal_det(Goal,NEWINFO,ProcVarSetOut,ResultVarSet,Result),
	  % Obtain the result values of the headvars with "out" mode
	  % and bind them to the coresponding variables
          bind_out_headvars(Headlist,HLDS,VarSet,ResultVarSet,VarSetOut)
	;
	  { InferredDeterminism = semidet },
	  io__write_string(" (semidet)\n"),
	  do_goal_semidet(Goal,NEWINFO,ProcVarSetOut,ResultVarSet,Result),
	  % Obtain the result values of the headvars with "out" mode
	  % and bind them to the coresponding variables
	  ( { Result = yes } ->
            bind_out_headvars(Headlist,HLDS,VarSet,ResultVarSet,VarSetOut)
	  ; % Result = no therefore do not bind any values of headvars
	    { VarSetOut = VarSet }
	  )
	;
	  { InferredDeterminism = nondet },
	  io__write_string(" (nondet)\n"),
	  { VarSetOut = VarSet },
	  { Result = yes }
	;
	  { InferredDeterminism = multidet },
	  io__write_string(" (multidet)\n"),
	  { VarSetOut = VarSet },
	  { Result = yes }
	;
	  { InferredDeterminism = erroneous },
	  io__write_string(" (error)\n"),
	  { VarSetOut = VarSet },
	  { Result = no }
	;
	  { InferredDeterminism = failure },
	  io__write_string(" (failure)\n"),
	  { VarSetOut = VarSet },
	  { Result = no }
	),
	io__write_string("Leaving Pred "), io__write_string(Name),
	io__write_string("\n").

do_goal2_semidet(unify(A,B,_,Unification,_),INFO,VarSet,VarSetOut,Result) -->
	{ INFO = highinfo(_HLDS,Headinlist) },
	( { Unification = assign(_,_) },
	  ( { A= term__variable(Var) } ->
	    ( { list__member(Var,Headinlist) } ->
	      ( { B= term__variable(VarB) },
		{ do_termvar2termconst(VarSet,A,Aconst) },
		{ varset__bind_var(VarSet,VarB,Aconst,VarSetOut) },
		mercury_output_var(VarB,VarSet),
		io__write_string(" is assign "),
		mercury_output_var(Var,VarSet),
		io__write_string(" = "),
		mercury_output_term(Aconst,VarSet),
		io__write_string("\n"),
		{ Result = yes }
	      ; % A is term_var, var is mode in, b is term__functor
		{ B= term__functor(_,_,_) },
		io__write_string("A is a headvar but B is a term__functor\n"),
	        { Result = no },
		{ VarSetOut = VarSet }
	      ) 
	    ;  % A is a variable and A is assign B
	      { do_lookup_termvarname(VarSet,B,Bname) },
	      { do_termvar2termconst(VarSet,B,Bconst) },
	      { varset__bind_var(VarSet,Var,Bconst,VarSetOut) },
	      mercury_output_var(Var,VarSet),
	      io__write_string(" is assign "),
	      io__write_string(Bname), io__write_string(" = "),
	      mercury_output_term(Bconst,VarSet),
	      io__write_string("\n"),
	      { Result = yes }
	    ) 
	  ;  % A is not a variable
	    { VarSetOut = VarSet },
	    { Result = no },
	    { error("Assignment to non-variable\n") }
	  ) 
	;
	  { Unification = simple_test(_,_) },
	  io__write_string("simple test\n"),
	  { Result = yes },
	  { VarSetOut = VarSet }
	;  
	  { Unification = construct(_,_,_,_) },
	  ( { A= term__variable(Var) } ->
	    { do_termvar2termconst(VarSet,B,Bconst) },
	    { varset__bind_var(VarSet,Var,Bconst,VarSetOut) },
	    mercury_output_var(Var,VarSet),
	    io__write_string(" is construct "),
	    mercury_output_term(Bconst,VarSet),
	    io__write_string("\n"),
	    { Result = yes } 
	  ;
	    { VarSetOut = VarSet },
	    { Result = no },
	    { error("Assignment to non-variable\n") }
	  )
	;
	  { Unification = deconstruct(_,_,_,_,_) },
	  % io__write_string("deconstruct\n"),
	  ( { A= term__variable(Var) },
	    { termvar2termfunctor(VarSet,A,Afunctor) },
	    ( { B= term__variable(_) },
	      { error("In semidet B must be a functor\n") }
	    ; % A is a functor and B is a functor
	      { B = term__functor(B1,Blist,_) },
	      ( { Afunctor = term__functor(A1,Alist,_) },
		mercury_output_term(Afunctor,VarSet),
		io__write_string(" =: "),
		mercury_output_term(B,VarSet),
	        ( { A1 = B1 } ->
		  io__write_string(" okay!\n"),
  		  { list__length(Alist,Alength) },
		  { list__length(Blist,Blength) },
		  ( { Alength = Blength } ->
		    { assoc_list__from_corresponding_lists(Alist,Blist,ABlist) },
		    semidet_deconstruct(ABlist,VarSet,VarSetOut,Result)
		  ;
		    io__write_string("Alist and B list are not of same length\n"),
		    io__write_string("A length is "),
		    io__write_int(Alength),
		    io__write_string("B length is "),
		    io__write_int(Blength),
		    { Result = no },
		    { VarSetOut = VarSet }
		  )
	        ; % A1 /= B1
		  io__write_string(" NOT OKAY\n"),
  	          { Result = no },
	          { VarSetOut = VarSet }
	        )
	      ; % Afunctor is not a functor
	  	{ Afunctor = term__variable(_) },
		{ error("\nAfunctor must be a functor") }
	      )
	    )
	  ; % A is a term functor which it shouldn't be
	    { A = term__functor(_,_,_) },
	    io__write_string("A is a term_func\n"),
	    { Result = yes },
	    { VarSetOut = VarSet }
	  )
	;
	  { Unification = complicated_unify(_,_,_) },
	  io__write_string("complicated unify\n"),
	  { Result = yes },
	  { VarSetOut = VarSet }
	).  

:- pred do_goal2_semidet_conj(list(hlds__goal),highinfo,varset,varset,bool,
	io__state,io__state).
:- mode do_goal2_semidet_conj(in,in,in,out,out,di,uo) is det.

do_goal2_semidet_conj([],_INFO,V,V,yes) --> [].
do_goal2_semidet_conj([Goal|Goals],INFO,VarSet,VarSetOut,Result) --> 
	do_goal_semidet(Goal,INFO,VarSet,VarSet1,Result1),
	( { Result1 = yes },
	  do_goal2_semidet_conj(Goals,INFO,VarSet1,VarSetOut,Result)
	;
	  { Result1 = no },
	  { VarSetOut = VarSet},
	  { Result = no }
	).

:- pred semidet_deconstruct(list(pair(term,term)),
	varset,varset,bool,io__state,io__state).
:- mode semidet_deconstruct(in,in,out,out,di,uo) is det.

semidet_deconstruct([],V,V,yes) --> [].
semidet_deconstruct([P|Ps],VarSet,VarSetOut,Result) -->
	{ P = A - B },
	( { B = term__variable(Var) } ->
	  ( { A = term__variable(VarA) } ->
	    io__write_string("\n Error! Apair is a var "),
	    mercury_output_var(VarA,VarSet),
	    { error("Error in semidet_deconstruct\n") }
	  ;
	    mercury_output_var(Var,VarSet),
	    io__write_string(" is assign(decon) "),
	    mercury_output_term(A,VarSet),
	    io__write_string("\n"),
	    { do_termvar2termconst(VarSet,A,Aconst) },
	    { varset__bind_var(VarSet,Var,Aconst,VarSetOut1) },
	    semidet_deconstruct(Ps,VarSetOut1,VarSetOut,Result)
	  )
	;
	  io__write_string("Bpair is not a term variable "),
	  mercury_output_term(B,VarSet),
	  { error("Error in semidet_deconstruct\n") }
	).

