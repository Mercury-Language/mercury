%---------------------------------------------------------------------------%
% Copyright (C) 1996-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module checks conformance of instance declarations to the typeclass
% declaration. 
% 
% It does so by, for every method of every instance, generating a new pred
% whose types and modes are as expected by the typeclass declaration and
% whose body just calls the implementation provided by the instance 
% declaration.
%
% eg. given the declarations:
%
% :- typeclass c(T) where [
% 	pred m(T::in, T::out) is semidet
% ].
%
% :- instance c(int) where [
% 	pred(m/2) is my_m
% ].
%
% The correctness of my_m/2 as an implementation of m/2 is checked by 
% generating the new predicate:
% 	
% :- pred 'implementation of m/2'(int::in, int::out) is semidet.
%
% 'implementation of m/2'(HeadVar_1, HeadVar_2) :-
% 	my_m(HeadVar_1, HeadVar_2).
%
% By generating the new pred, we check the instance method for type, mode,
% determinism and uniqueness correctness since the generated pred is checked
% in each of those passes too.
%
% In addition, this pass checks that all superclass constraints are satisfied
% by the instance declaration.
%
% This pass fills in the super class proofs and instance method pred/proc ids
% in the instance table of the HLDS.
%
% Author: dgj.
%
%---------------------------------------------------------------------------%

:- module check_typeclass.


:- interface.

:- import_module hlds_module, bool, io.

:- pred check_typeclass__check_instance_decls(module_info, module_info, bool,
	io__state, io__state).
:- mode check_typeclass__check_instance_decls(in, out, out, di, uo) is det.

:- implementation.

:- import_module type_util, assoc_list, mode_util, inst_match, hlds_module.
:- import_module base_typeclass_info, hlds_goal, prog_out, hlds_pred.
:- import_module inst_util, instmap, error_util, mercury_to_mercury.
:- import_module typecheck, globals, make_hlds, hlds_data, prog_data. 
:- import_module term, varset, int, std_util, list, string, set, map, require.

:- type error_message == pair(term__context, list(format_component)).
:- type error_messages == list(error_message).

check_typeclass__check_instance_decls(ModuleInfo0, ModuleInfo, FoundError, 
		IO0, IO) :-
	module_info_classes(ModuleInfo0, ClassTable),
	module_info_instances(ModuleInfo0, InstanceTable0),
	map__to_assoc_list(InstanceTable0, InstanceList0),
	list__map_foldl(check_one_class(ClassTable), InstanceList0,
		InstanceList, [] - ModuleInfo0, Errors - ModuleInfo1),
	(
		Errors = []
	->
		map__from_assoc_list(InstanceList, InstanceTable),
		module_info_set_instances(ModuleInfo1, InstanceTable,
			ModuleInfo),
		IO = IO0,
		FoundError = no
	;
		ModuleInfo = ModuleInfo1,
		list__reverse(Errors, ErrorList),
		WriteError = lambda([E::in, TheIO0::di, TheIO::uo] is det,
			(
				E = ErrorContext - ErrorPieces,
				write_error_pieces(ErrorContext, 0, 
					ErrorPieces, TheIO0, TheIO)
			)),
		list__foldl(WriteError, ErrorList, IO0, IO1),
		io__set_exit_status(1, IO1, IO),
		FoundError = yes
	).  
		
	% check all the instances of one class.
:- pred check_one_class(class_table,
	pair(class_id, list(hlds_instance_defn)), 
	pair(class_id, list(hlds_instance_defn)), 
	pair(error_messages, module_info), 
	pair(error_messages, module_info)).
:- mode check_one_class(in, in, out, in, out) is det.

check_one_class(ClassTable, ClassId - InstanceDefns0, 
	ClassId - InstanceDefns, ModuleInfo0, ModuleInfo) :-

	map__lookup(ClassTable, ClassId, ClassDefn),
	ClassDefn = hlds_class_defn(SuperClasses, ClassVars, ClassInterface,
		ClassVarSet, _TermContext),
	solutions(
		lambda([PredId::out] is nondet, 
			(
				list__member(ClassProc, ClassInterface),
				ClassProc = hlds_class_proc(PredId, _)
			)),
		PredIds),
	list__map_foldl(check_class_instance(ClassId, SuperClasses, ClassVars,
				ClassInterface, ClassVarSet,
				PredIds),
		InstanceDefns0, InstanceDefns, 
		ModuleInfo0, ModuleInfo).


	% check one instance of one class
:- pred check_class_instance(class_id, list(class_constraint), list(var),
	hlds_class_interface, varset, list(pred_id), 
	hlds_instance_defn, hlds_instance_defn, 
	pair(error_messages, module_info), 
	pair(error_messages, module_info)).
:- mode check_class_instance(in, in, in, in, in, in, in, out, in, out) is det.

check_class_instance(ClassId, SuperClasses, Vars, ClassInterface, ClassVarSet,
		PredIds, InstanceDefn0, InstanceDefn, 
		ModuleInfo0, ModuleInfo):-
		
		% check conformance of the instance interface
	(
		PredIds \= []
	->
		list__foldl2(
			check_instance_pred(ClassId, Vars, ClassInterface), 
			PredIds, InstanceDefn0, InstanceDefn1,
			ModuleInfo0, ModuleInfo1)
	;
		% there are no methods for this class
		InstanceDefn0 = hlds_instance_defn(A, B, C, D, E,
				_MaybeInstancePredProcs, G, H),
		InstanceDefn1 = hlds_instance_defn(A, B, C, D, E,
				yes([]), G, H),
		ModuleInfo1 = ModuleInfo0
	),


		% check that the superclass constraints are satisfied for the
		% types in this instance declaration
	check_superclass_conformance(ClassId, SuperClasses, Vars, ClassVarSet,
		InstanceDefn1, InstanceDefn, ModuleInfo1, ModuleInfo).

%----------------------------------------------------------------------------%

	% This structure holds the information about a particular instance
	% method
:- type instance_method_info ---> instance_method_info(
		module_info,
		sym_name,				% Name that the
							% introduced pred
							% should be given.
		arity,					% Arity of the method.
		existq_tvars,				% Existentially quant.
							% type variables
		list(type),				% Expected types of
							% arguments.
		class_constraints,			% Constraints from
							% class method.
		list(pair(argument_modes, determinism)),% Modes and 
							% determinisms of the
							% required procs.
		error_messages,				% Error messages
							% that have been
							% generated.
		tvarset,		
		import_status,				% Import status of
							% instance decl.
		pred_or_func				% Is method pred or
							% func?
	).

%----------------------------------------------------------------------------%

	% check one pred in one instance of one class
:- pred check_instance_pred(class_id, list(var), hlds_class_interface, 
	pred_id, hlds_instance_defn, hlds_instance_defn,
	pair(error_messages, module_info), pair(error_messages, module_info)).
:- mode check_instance_pred(in,in, in, in, in, out, in, out) is det.

check_instance_pred(ClassId, ClassVars, ClassInterface, PredId,
		InstanceDefn0, InstanceDefn, 
		Errors0 - ModuleInfo0, Errors - ModuleInfo):-
	solutions(
		lambda([ProcId::out] is nondet, 
			(
				list__member(ClassProc, ClassInterface),
				ClassProc = hlds_class_proc(PredId, ProcId)
			)),
		ProcIds),
	module_info_pred_info(ModuleInfo0, PredId, PredInfo),
	pred_info_arg_types(PredInfo, ArgTypeVars, ExistQVars, ArgTypes),
	pred_info_get_class_context(PredInfo, ClassContext0),
		% The first constraint in the class context of a class method
		% is always the constraint for the class of which it is
		% a member. Seeing that we are checking an instance 
		% declaration, we don't check that constraint... the instance
		% declaration itself satisfies it!
	(
		ClassContext0 = constraints([_|OtherUnivCs], ExistCs)
	->
		UnivCs = OtherUnivCs,
		ClassContext = constraints(UnivCs, ExistCs)
	;
		error("check_instance_pred: no constraint on class method")
	),

	pred_info_name(PredInfo, MethodName0),
	pred_info_module(PredInfo, PredModule),
	MethodName = qualified(PredModule, MethodName0),
	pred_info_arity(PredInfo, PredArity),
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	pred_info_procedures(PredInfo, ProcTable),
	list__map(
		lambda([TheProcId::in, ModesAndDetism::out] is det, 
			(
				map__lookup(ProcTable, TheProcId, ProcInfo), 
				proc_info_argmodes(ProcInfo, Modes),
				proc_info_interface_determinism(ProcInfo, 
					Detism),
				ModesAndDetism = Modes - Detism
			)),
		ProcIds, 
		ArgModes),
	
	InstanceDefn0 = hlds_instance_defn(Status, _, _, InstanceTypes, 
		_, _, _, _),

		% Work out the name of the predicate that we will generate
		% to check this instance method.
	make_introduced_pred_name(ClassId, MethodName, PredArity, 
		InstanceTypes, PredName),
	
	Info0 = instance_method_info(ModuleInfo0, PredName, PredArity, 
		ExistQVars, ArgTypes, ClassContext, ArgModes, Errors0,
		ArgTypeVars, Status, PredOrFunc),

	check_instance_pred_procs(ClassId, ClassVars, MethodName,
		InstanceDefn0, InstanceDefn, Info0, Info),

	Info = instance_method_info(ModuleInfo, _PredName, _PredArity, 
		_ExistQVars, _ArgTypes, _ClassContext, _ArgModes, Errors,
		_ArgTypeVars, _Status, _PredOrFunc).

:- pred check_instance_pred_procs(class_id, list(var), sym_name,
	hlds_instance_defn, hlds_instance_defn, 
	instance_method_info, instance_method_info).
:- mode check_instance_pred_procs(in, in, in, in, out, in, out) is det.

check_instance_pred_procs(ClassId, ClassVars, MethodName, InstanceDefn0,
		InstanceDefn, Info0, Info) :-
	InstanceDefn0 = hlds_instance_defn(A, InstanceContext, 
				InstanceConstraints, InstanceTypes,
				InstanceInterface, MaybeInstancePredProcs,
				InstanceVarSet, H),
	Info0 = instance_method_info(ModuleInfo, PredName, PredArity, 
		ExistQVars, ArgTypes, ClassContext, ArgModes, Errors0,
		ArgTypeVars, Status, PredOrFunc),
	get_matching_instance_names(InstanceInterface, PredOrFunc, MethodName,
		PredArity, InstanceNames),
	(
		InstanceNames = [InstancePredName - Context]
	->
		produce_auxiliary_procs(ClassVars, 
			InstanceTypes, InstanceConstraints, 
			InstanceVarSet, 
			InstancePredName, Context,
			InstancePredId, InstanceProcIds, Info0, Info),

		MakeClassProc = 
			lambda([TheProcId::in, PredProcId::out] is det,
			(
				PredProcId = hlds_class_proc(InstancePredId,
					TheProcId)
			)),
		list__map(MakeClassProc, InstanceProcIds, InstancePredProcs1),
		(
			MaybeInstancePredProcs = yes(InstancePredProcs0),
			list__append(InstancePredProcs0,
				InstancePredProcs1, InstancePredProcs)
		;
			MaybeInstancePredProcs = no,
			InstancePredProcs = InstancePredProcs1
		),
		InstanceDefn = hlds_instance_defn(A, Context, 
			InstanceConstraints, InstanceTypes, InstanceInterface,
			yes(InstancePredProcs), InstanceVarSet, H)
	;
		InstanceNames = [I1, I2 | Is]
	->
			% one kind of error
		InstanceDefn = InstanceDefn0,
		ClassId = class_id(ClassName, _ClassArity),
		prog_out__sym_name_to_string(MethodName, MethodNameString),
		prog_out__sym_name_to_string(ClassName, ClassNameString),
		string__int_to_string(PredArity, PredArityString),
		mercury_type_list_to_string(InstanceVarSet, InstanceTypes,
			InstanceTypesString),
		string__append_list([
			"In instance declaration for `",
			ClassNameString,
			"(",
			InstanceTypesString,
			")': ",
			"multiple implementations of type class method `",
			MethodNameString,
			"/",
			PredArityString,
			"'."],
			ErrorHeader),
		I1 = _ - I1Context, 
		Heading = 
			[I1Context - [words("First definition appears here.")],
			InstanceContext - [words(ErrorHeader)]],
		list__map(lambda([Definition::in, ContextAndError::out] is det,
		(
			Definition = _ - TheContext,
			Error = [words("Subsequent definition appears here.")],
			ContextAndError = TheContext - Error
		)), [I2|Is], SubsequentErrors),
			
			% errors are built up in reverse.
		list__append(SubsequentErrors, Heading, NewErrors),
		list__append(NewErrors, Errors0, Errors),
		Info = instance_method_info(ModuleInfo, PredName, PredArity,
			ExistQVars, ArgTypes, ClassContext, ArgModes, Errors,
			ArgTypeVars, Status, PredOrFunc)
	;
			% another kind of error
		InstanceDefn = InstanceDefn0,
		ClassId = class_id(ClassName, _ClassArity),
		prog_out__sym_name_to_string(MethodName, MethodNameString),
		prog_out__sym_name_to_string(ClassName, ClassNameString),
		string__int_to_string(PredArity, PredArityString),
		mercury_type_list_to_string(InstanceVarSet, InstanceTypes,
			InstanceTypesString),
		string__append_list([
			"In instance declaration for `",
			ClassNameString,
			"(",
			InstanceTypesString,
			")': ",
			"no implementation for type class method `",
			MethodNameString,
			"/",
			PredArityString,
			"'."],
			NewError),
		Errors = [InstanceContext - [words(NewError)] | Errors0],
		Info = instance_method_info(ModuleInfo, PredName, PredArity,
			ExistQVars, ArgTypes, ClassContext, ArgModes, Errors,
			ArgTypeVars, Status, PredOrFunc)
	).

:- pred get_matching_instance_names(list(instance_method), pred_or_func,
	sym_name, arity, list(pair(sym_name, term__context))).
:- mode get_matching_instance_names(in, in, in, in, out) is det.

get_matching_instance_names(InstanceInterface, PredOrFunc, PredName,
	PredArity, InstanceNames) :-
	(
		PredOrFunc = predicate,
		solutions(
			lambda([Pair::out] is nondet, 
				(
					list__member(Method, InstanceInterface),
					Method = pred_instance(PredName, 
							SymName, PredArity,
							Context),
					Pair = SymName - Context
				)),
			InstanceNames)
	;
		PredOrFunc = function,
		FuncArity is PredArity - 1,
		solutions(
			lambda([Pair::out] is nondet, 
				(
					list__member(Method, InstanceInterface),
					Method = func_instance(PredName, 
							SymName, FuncArity,
							Context),
					Pair = SymName - Context
				)),
			InstanceNames)
	).

	% Just a bit simpler than using a pair of pairs
:- type triple(T1, T2, T3) ---> triple(T1, T2, T3).

:- pred produce_auxiliary_procs(list(var), 
	list(type), list(class_constraint), varset, sym_name, term__context,
	pred_id, list(proc_id), instance_method_info, instance_method_info).
:- mode produce_auxiliary_procs(in, in, in, in, in, in, out, out, 
	in, out) is det.

produce_auxiliary_procs(ClassVars, 
		InstanceTypes0, InstanceConstraints0, InstanceVarSet,
		InstancePredName, Context, PredId,
		InstanceProcIds, Info0, Info) :-

	Info0 = instance_method_info(ModuleInfo0, PredName, PredArity, 
		ExistQVars0, ArgTypes0, ClassContext0, ArgModes, Errors,
		ArgTypeVars0, Status, PredOrFunc),

		% Rename the instance variables apart from the class variables
	varset__merge_subst(ArgTypeVars0, InstanceVarSet, ArgTypeVars1,
		RenameSubst),
	term__apply_substitution_to_list(InstanceTypes0, RenameSubst,
		InstanceTypes),
	apply_subst_to_constraint_list(RenameSubst, InstanceConstraints0,
		InstanceConstraints),

		% Work out what the type variables are bound to for this
		% instance, and update the class types appropriately.
	map__from_corresponding_lists(ClassVars, InstanceTypes, TypeSubst),
	term__apply_substitution_to_list(ArgTypes0, TypeSubst, ArgTypes1),
	apply_subst_to_constraints(TypeSubst, ClassContext0, ClassContext1),

		% Add the constraints from the instance declaration to the 
		% constraints from the class method. This allows an instance
		% method to have constraints on it which are not part of the
		% instance declaration as a whole.
	ClassContext1 = constraints(UnivConstraints1, ExistConstraints),
	list__append(InstanceConstraints, UnivConstraints1, UnivConstraints),
	ClassContext2 = constraints(UnivConstraints, ExistConstraints),

		% Get rid of any unwanted type variables
	term__vars_list(ArgTypes1, VarsToKeep0),
	list__sort_and_remove_dups(VarsToKeep0, VarsToKeep),
	varset__squash(ArgTypeVars1, VarsToKeep, ArgTypeVars, SquashSubst),
	term__apply_variable_renaming_to_list(ArgTypes1, SquashSubst, 
		ArgTypes),
	apply_variable_renaming_to_constraints(SquashSubst,
		ClassContext2, ClassContext),
	apply_partial_map_to_list(ExistQVars0, SquashSubst, ExistQVars),

		% Introduce a new predicate which calls the implementation
		% given in the instance declaration.
	module_info_name(ModuleInfo0, ModuleName),

	Cond = true,
	map__init(Proofs),
	init_markers(Markers),

		% We have to add the actual clause after we have added the
		% procs because we need a list of proc numbers for which the
		% clauses holds.
	DummyClause = [],
	varset__init(VarSet0),
	make_n_fresh_vars("HeadVar__", PredArity, VarSet0, HeadVars, VarSet), 
	map__from_corresponding_lists(HeadVars, ArgTypes, VarTypes),
	DummyClausesInfo = clauses_info(VarSet, VarTypes, VarTypes, HeadVars,
		DummyClause),

	pred_info_init(ModuleName, PredName, PredArity, ArgTypeVars, 
		ExistQVars, ArgTypes, Cond, Context, DummyClausesInfo, Status,
		Markers, none, PredOrFunc, ClassContext, Proofs,
		PredInfo0),

	module_info_globals(ModuleInfo0, Globals),
	globals__get_args_method(Globals, ArgsMethod),

		% Add procs with the expected modes and determinisms
	AddProc = lambda([ModeAndDet::in, NewProcId::out,
			OldPredInfo::in, NewPredInfo::out] is det,
	(
		ModeAndDet = Modes - Det,
		add_new_proc(OldPredInfo, PredArity, Modes, yes(Modes), no,
			yes(Det), Context, ArgsMethod, NewPredInfo, NewProcId)
	)),
	list__map_foldl(AddProc, ArgModes, InstanceProcIds, 
		PredInfo0, PredInfo1),

		% Add the body of the introduced pred

		% First the goal info
	goal_info_init(GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo1),
	set__list_to_set(HeadVars, NonLocals),
	goal_info_set_nonlocals(GoalInfo1, NonLocals, GoalInfo),

		% Then the goal itself
	invalid_pred_id(InvalidPredId),
	invalid_proc_id(InvalidProcId),
	(
		PredOrFunc = predicate,
		Call = call(InvalidPredId, InvalidProcId, HeadVars, not_builtin,
			no, InstancePredName),
		IntroducedGoal = Call - GoalInfo
	;
		PredOrFunc = function,
		pred_args_to_func_args(HeadVars, RealHeadVars, ReturnVar),
		create_atomic_unification(ReturnVar, 
			functor(cons(InstancePredName, PredArity),
				RealHeadVars), 
			Context, explicit, [], IntroducedGoal0),
		% set the goal_info
		IntroducedGoal0 = IntroducedGoalExpr - _,
		IntroducedGoal = IntroducedGoalExpr - GoalInfo
	),
	IntroducedClause = clause(InstanceProcIds, IntroducedGoal, Context),
	ClausesInfo = clauses_info(VarSet, VarTypes, VarTypes, HeadVars,
		[IntroducedClause]),
	pred_info_set_clauses_info(PredInfo1, ClausesInfo, PredInfo),

	module_info_get_predicate_table(ModuleInfo0, PredicateTable0),
	predicate_table_insert(PredicateTable0, PredInfo,
		may_be_unqualified, PredId, PredicateTable),
	module_info_set_predicate_table(ModuleInfo0, PredicateTable,
		ModuleInfo),

	Info = instance_method_info(ModuleInfo, PredName, PredArity,
		ExistQVars, ArgTypes, ClassContext, ArgModes, Errors,
		ArgTypeVars, Status, PredOrFunc).

:- pred apply_substitution_to_var_list(list(var), map(var, term), list(var)).
:- mode apply_substitution_to_var_list(in, in, out) is det.

apply_substitution_to_var_list(Vars0, RenameSubst, Vars) :-
	term__var_list_to_term_list(Vars0, Terms0),
	term__apply_substitution_to_list(Terms0, RenameSubst, Terms),
	term__term_list_to_var_list(Terms, Vars).

%---------------------------------------------------------------------------%
	% Make the name of the introduced pred used to check a particular
	% instance of a particular class method
	%
	% XXX This isn't quite perfect, I suspect
:- pred make_introduced_pred_name(class_id, sym_name, arity, list(type), 
	sym_name).
:- mode make_introduced_pred_name(in, in, in, in, out) is det.

make_introduced_pred_name(ClassId, MethodName, PredArity, 
		InstanceTypes, PredName) :-
	ClassId = class_id(ClassName, _ClassArity),
	prog_out__sym_name_to_string(ClassName, "__", ClassNameString),
	prog_out__sym_name_to_string(MethodName, "__", MethodNameString),
		% Perhaps we should include the pred arity in this mangled
		% string?
	string__int_to_string(PredArity, PredArityString),
	base_typeclass_info__make_instance_string(InstanceTypes, 
		InstanceString),
	string__append_list(
		["Introduced_pred_for_",
		ClassNameString,
		"__",
		InstanceString,
		"____",
		MethodNameString,
		"_",
		PredArityString
		], 
		PredNameString),
	PredName = unqualified(PredNameString).

%---------------------------------------------------------------------------%

% check that the superclass constraints are satisfied for the
% types in this instance declaration

:- pred check_superclass_conformance(class_id, list(class_constraint), 
	list(var), varset, hlds_instance_defn, hlds_instance_defn, 
	pair(error_messages, module_info), pair(error_messages, module_info)).
:- mode check_superclass_conformance(in, in, in, in, in, out, in, out) is det.

check_superclass_conformance(ClassId, SuperClasses0, ClassVars0, ClassVarSet, 
		InstanceDefn0, InstanceDefn, 
		Errors0 - ModuleInfo, Errors - ModuleInfo) :-

	InstanceDefn0 = hlds_instance_defn(A, Context, InstanceConstraints,
		InstanceTypes, E, F, InstanceVarSet0, Proofs0),
	varset__merge_subst(InstanceVarSet0, ClassVarSet, InstanceVarSet1,
		Subst),

		% Make the constraints in terms of the instance variables
	apply_subst_to_constraint_list(Subst, SuperClasses0, SuperClasses),

		% Now handle the class variables
	map__apply_to_list(ClassVars0, Subst, ClassVarTerms),
	(
		term__var_list_to_term_list(ClassVars1, ClassVarTerms)
	->
		ClassVars = ClassVars1
	;
		error("ClassVarTerms are not vars")
	),

		% Calculate the bindings
	map__from_corresponding_lists(ClassVars, InstanceTypes, TypeSubst),

	module_info_instances(ModuleInfo, InstanceTable),
	module_info_superclasses(ModuleInfo, SuperClassTable),

		% Try to reduce the superclass constraints,
		% using the declared instance constraints
		% and the usual context reduction rules.
	typecheck__reduce_context_by_rule_application(InstanceTable, 
		SuperClassTable, InstanceConstraints, TypeSubst,
		InstanceVarSet1, InstanceVarSet2, Proofs0, Proofs1,
		SuperClasses, UnprovenConstraints),

	(
		UnprovenConstraints = []
	->
		Errors = Errors0,
		InstanceDefn = hlds_instance_defn(A, Context, 
			InstanceConstraints, InstanceTypes, E, F, 
			InstanceVarSet2, Proofs1)
	;
		ClassId = class_id(ClassName, _ClassArity),
		prog_out__sym_name_to_string(ClassName, ClassNameString),
		mercury_type_list_to_string(InstanceVarSet2, InstanceTypes,
			InstanceTypesString),
		constraint_list_to_string(ClassVarSet, UnprovenConstraints, 
			ConstraintsString),
		string__append_list([
			"In instance declaration for `",
			ClassNameString,
			"(",
			InstanceTypesString,
			")': ",
			"superclass constraint(s) not satisfied: ",
			ConstraintsString,
			"."],
			NewError),
		Errors = [Context - [words(NewError)] | Errors0],
		InstanceDefn = InstanceDefn0
	).

:- pred constraint_list_to_string(varset, list(class_constraint), string).
:- mode constraint_list_to_string(in, in, out) is det.

constraint_list_to_string(_, [], "").
constraint_list_to_string(VarSet, [C|Cs], String) :-
	mercury_constraint_to_string(VarSet, C, String0),
	constraint_list_to_string_2(VarSet, Cs, String1),
	string__append_list(["`", String0, "'", String1], String).

:- pred constraint_list_to_string_2(varset, list(class_constraint), string).
:- mode constraint_list_to_string_2(in, in, out) is det.

constraint_list_to_string_2(_VarSet, [], "").
constraint_list_to_string_2(VarSet, [C|Cs], String) :-
	mercury_constraint_to_string(VarSet, C, String0),
	constraint_list_to_string_2(VarSet, Cs, String1),
	string__append_list([", `", String0, "'", String1], String).

%---------------------------------------------------------------------------%
