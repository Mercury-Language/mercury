%---------------------------------------------------------------------------%
% Copyright (C) 1996-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module checks conformance of instance declarations to the typeclass
% declaration. (ie. it checks that for every method of the class, the
% types in the instance are correct, and that there is a mode of the instance
% method that matches the typeclass method mode _exactly_).
%
% Author: dgj.
%
%---------------------------------------------------------------------------%

:- module check_typeclass.


	% XXX what about constraints on class methods?
	
:- interface.

:- import_module hlds_module, bool, io.

:- pred check_typeclass__check_instance_decls(module_info, module_info, bool,
	io__state, io__state).
:- mode check_typeclass__check_instance_decls(in, out, out, di, uo) is det.

:- implementation.

:- import_module map, list, std_util, hlds_pred, hlds_data, prog_data, require.
:- import_module type_util, assoc_list, mode_util, inst_match, hlds_module.
:- import_module term, varset, typecheck, int.

check_typeclass__check_instance_decls(ModuleInfo0, ModuleInfo, FoundError, 
		IO0, IO) :-
	module_info_classes(ModuleInfo0, ClassTable),
	module_info_instances(ModuleInfo0, InstanceTable0),
	map__to_assoc_list(InstanceTable0, InstanceList0),
	list__map_foldl(check_one_class(ClassTable, ModuleInfo0), InstanceList0,
		InstanceList, [], Errors),
	(
		Errors = []
	->
		map__from_assoc_list(InstanceList, InstanceTable),
		module_info_set_instances(ModuleInfo0, InstanceTable,
			ModuleInfo),
		IO = IO0,
		FoundError = no
	;
		ModuleInfo = ModuleInfo0,
		list__reverse(Errors, ErrorList),
		io__write_list(ErrorList, "\n", io__write_string, IO0, IO1),
		io__write_string("\n", IO1, IO2),
		io__set_exit_status(1, IO2, IO),
		FoundError = yes
	).  
		
	% check all the instances of one class.
:- pred check_one_class(class_table, module_info, 
	pair(class_id, list(hlds_instance_defn)), 
	pair(class_id, list(hlds_instance_defn)), list(string), list(string)).
:- mode check_one_class(in, in, in, out, in, out) is det.

check_one_class(ClassTable, ModuleInfo, ClassId - InstanceDefns0, 
	ClassId - InstanceDefns, Errors0, Errors) :-

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
	list__map_foldl(check_class_instance(SuperClasses, ClassVars,
				ClassInterface, ClassVarSet, ModuleInfo,
				PredIds),
		InstanceDefns0, InstanceDefns, Errors0, Errors).


	% check one instance of one class
:- pred check_class_instance(list(class_constraint), list(var),
	hlds_class_interface, varset, module_info, list(pred_id), 
	hlds_instance_defn, hlds_instance_defn, list(string), list(string)).
:- mode check_class_instance(in, in, in, in, in, in, in, out, in, out) is det.

check_class_instance(SuperClasses, Vars, ClassInterface, ClassVarSet,
		ModuleInfo, PredIds, InstanceDefn0, InstanceDefn, 
		Errors0, Errors):-
		
		% check conformance of the instance interface
	(
		PredIds \= []
	->
		list__foldl2(check_instance_pred(Vars, ClassInterface,
			ModuleInfo), PredIds, InstanceDefn0, InstanceDefn1,
			Errors0, Errors1)
	;
		% there are no methods for this class
		InstanceDefn0 = hlds_instance_defn(A, B, C, D, 
				_MaybeInstancePredProcs, F, G),
		InstanceDefn1 = hlds_instance_defn(A, B, C, D, 
				yes([]), F, G),
		Errors1 = Errors0
	),


		% check that the superclass constraints are satisfied for the
		% types in this instance declaration
	check_superclass_conformance(SuperClasses, Vars, ClassVarSet,
		ModuleInfo, InstanceDefn1, InstanceDefn, Errors1, Errors).


	% check one pred in one instance of one class
:- pred check_instance_pred(list(var), hlds_class_interface, module_info,
	pred_id, hlds_instance_defn, hlds_instance_defn,
	list(string), list(string)).
:- mode check_instance_pred(in, in, in, in, in, out, in, out) is det.

check_instance_pred(ClassVars, ClassInterface, ModuleInfo, PredId,
		InstanceDefn0, InstanceDefn, Errors0, Errors):-
	solutions(
		lambda([ProcId::out] is nondet, 
			(
				list__member(ClassProc, ClassInterface),
				ClassProc = hlds_class_proc(PredId, ProcId)
			)),
		ProcIds),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_arg_types(PredInfo, _ArgTypeVars, ArgTypes),
	pred_info_name(PredInfo, PredName0),
	pred_info_module(PredInfo, PredModule),
	PredName = qualified(PredModule, PredName0),
	pred_info_arity(PredInfo, PredArity0),
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	(
		PredOrFunc = predicate,
		PredArity = PredArity0
	;
		PredOrFunc = function,
		PredArity is PredArity0 - 1
	),
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
	check_instance_pred_procs(ModuleInfo, PredOrFunc, PredName, PredArity,
		ArgTypes, ArgModes, ClassVars, InstanceDefn0, InstanceDefn,
		Errors0, Errors).

:- pred check_instance_pred_procs(module_info, pred_or_func, sym_name, arity,
	list(type), list(pair(list(mode), determinism)), list(var),
	hlds_instance_defn, hlds_instance_defn, list(string), list(string)).
:- mode check_instance_pred_procs(in, in, in, in, in, in, in, in, out, 
	in, out) is det.

check_instance_pred_procs(ModuleInfo, PredOrFunc, PredName, PredArity,
		ArgTypes0, ArgModes, ClassVars, InstanceDefn0, InstanceDefn, 
		Errors0, Errors) :-
	InstanceDefn0 = hlds_instance_defn(A, B, InstanceTypes,
				InstanceInterface, MaybeInstancePredProcs, 
				E, F),
	get_matching_instance_names(InstanceInterface, PredOrFunc, PredName,
		PredArity, InstanceNames),
	(
		InstanceNames = [InstancePredName]
	->
		(
			get_matching_instance_pred_ids(ModuleInfo,
				InstancePredName, PredOrFunc, PredArity,
				InstancePredIds)
		->
			handle_instance_method_overloading(ModuleInfo,
				ClassVars, InstanceTypes, ArgTypes0, ArgModes,
				InstancePredIds, Errors0, Errors,
				InstancePredId, InstanceProcIds),

			MakeClassProc = 
				lambda([TheProcId::in, PredProcId::out] is det,
				(
					PredProcId =
						hlds_class_proc(InstancePredId,
							TheProcId)
				)),
			list__map(MakeClassProc, InstanceProcIds,
				InstancePredProcs1),
			(
				MaybeInstancePredProcs =
					yes(InstancePredProcs0),
				list__append(InstancePredProcs0,
					InstancePredProcs1, InstancePredProcs)
			;
				MaybeInstancePredProcs = no,
				InstancePredProcs = InstancePredProcs1
			),
			InstanceDefn = hlds_instance_defn(A, B, InstanceTypes,
					InstanceInterface,
					yes(InstancePredProcs), E, F)
		;
			InstanceDefn = InstanceDefn0,
				% XXX make a better error message
			Errors = ["instance method not found"|Errors0]
		)
	;
		InstanceNames = [_,_|_]
	->
			% one kind of error
			% XXX make a better error message
		InstanceDefn = InstanceDefn0,
		Errors = ["multiply defined class method"|Errors0]
	;
			 % another kind of error
			 % XXX make a better error message
		InstanceDefn = InstanceDefn0,
		Errors = ["undefined class method"|Errors0]
	).

:- pred get_matching_instance_names(list(instance_method), pred_or_func,
	sym_name, arity, list(sym_name)).
:- mode get_matching_instance_names(in, in, in, in, out) is det.

get_matching_instance_names(InstanceInterface, PredOrFunc, PredName,
	PredArity, InstanceNames) :-
	(
		PredOrFunc = predicate,
		solutions(
			lambda([SymName::out] is nondet, 
				(
					list__member(Method, InstanceInterface),
					Method = pred_instance(PredName, 
							SymName, PredArity)
				)),
			InstanceNames)
	;
		PredOrFunc = function,
		solutions(
			lambda([SymName::out] is nondet, 
				(
					list__member(Method, InstanceInterface),
					Method = func_instance(PredName, 
							SymName, PredArity)
				)),
			InstanceNames)
	).

:- pred get_matching_instance_pred_ids(module_info, sym_name, pred_or_func,
	arity, list(pred_id)).
:- mode get_matching_instance_pred_ids(in, in, in, in, out) is semidet.

get_matching_instance_pred_ids(ModuleInfo, InstancePredName, PredOrFunc,
		PredArity, InstancePredIds) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	(
		(
			InstancePredName = unqualified(InstancePred)
		;
			InstancePredName = qualified("", InstancePred)
		)
	->
		(
			PredOrFunc = predicate,
			predicate_table_search_pred_name_arity( PredicateTable,
				InstancePred, PredArity, InstancePredIds)
		;	
			PredOrFunc = function,
			predicate_table_search_func_name_arity( PredicateTable,
				InstancePred, PredArity, InstancePredIds)
		)
	;
		InstancePredName = qualified(InstanceModule, InstancePred),
		(
			PredOrFunc = predicate,
			predicate_table_search_pred_m_n_a( PredicateTable,
				InstanceModule, InstancePred, PredArity,
				InstancePredIds)
		;	
			PredOrFunc = function,
			predicate_table_search_func_m_n_a( PredicateTable,
				InstanceModule, InstancePred, PredArity,
				InstancePredIds)
		)
	).

:- pred handle_instance_method_overloading(module_info, list(var), list(type),
	list(type), list(pair(list(mode), determinism)), list(pred_id), 
	list(string), list(string), pred_id, list(proc_id)).
:- mode handle_instance_method_overloading(in, in, in, in, in, in, in, 
	out, out, out) is det.

handle_instance_method_overloading(ModuleInfo, ClassVars, InstanceTypes,
		ArgTypes0, ArgModes, InstancePredIds, Errors0, Errors,
		InstancePredId, InstanceProcIds) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	predicate_table_get_preds(PredicateTable, PredTable),
	(
			% There is a single matching pred_id.
			% No overloading

		InstancePredIds = [InstancePredId0]
	->
		InstancePredId = InstancePredId0,
		map__lookup(PredTable, InstancePredId,
			InstancePredInfo), 
		map__from_corresponding_lists(ClassVars,
			InstanceTypes, TypeSubst),
		term__apply_substitution_to_list(ArgTypes0,
			TypeSubst, ArgTypes),
		check_instance_types_and_modes(ModuleInfo,
			InstancePredInfo, ArgTypes, ArgModes,
			Errors0, Errors, InstanceProcIds)
	;

		% Now we have a list of potential pred_ids for
		% the instance method. We have to resolve
		% overloading by checking each possible pred_id
		% to see if it is type and mode correct.

		Resolve = lambda(
			[PredId::in, Procs::out] is semidet,
			(
			map__lookup(PredTable, PredId, InstancePredInfo), 
			map__from_corresponding_lists(ClassVars, InstanceTypes,
				TypeSubst),
			term__apply_substitution_to_list(ArgTypes0, TypeSubst,
				ArgTypes),
			check_instance_types_and_modes(ModuleInfo,
				InstancePredInfo, ArgTypes, ArgModes, [], [],
				Procs0),
			Procs = PredId - Procs0
			)),
		list__filter_map(Resolve, InstancePredIds,
			Matches),
		(
				% no matching preds
			Matches = [],
			invalid_pred_id(InstancePredId),
			InstanceProcIds = [],
				% XXX improve error message
			NewError = "instance method not found",
			Errors = [NewError|Errors0]
		;
				% There is a single matching
				% pred_id. 
			Matches = [InstancePredId - 
					InstanceProcIds],
			Errors  = Errors0
		;
				% unresolved overloading
			Matches = [_,_|_],
			invalid_pred_id(InstancePredId),
			InstanceProcIds = [],
				% XXX improve error message
			NewError = "unresolved overloading in instance method",
			Errors = [NewError|Errors0]
		)
	).

:- pred check_instance_types_and_modes(module_info, pred_info, list(type),
	list(pair(list(mode), determinism)), list(string), list(string),
	list(proc_id)).
:- mode check_instance_types_and_modes(in, in, in, in, in, out, out) is det.

check_instance_types_and_modes(ModuleInfo, InstancePredInfo, ArgTypes, ArgModes,
		Errors0, Errors, InstanceProcIds) :-
	pred_info_arg_types(InstancePredInfo, _, InstanceArgTypes),
	(
		type_list_matches_exactly(InstanceArgTypes, ArgTypes)
	->
		pred_info_procedures(InstancePredInfo, InstanceProcedures0),
		map__to_assoc_list(InstanceProcedures0, InstanceProcedures),
		list__map_foldl(
			check_instance_modes(ModuleInfo, InstanceProcedures), 
			ArgModes, InstanceProcIds, Errors0, Errors)
	;
			% XXX fix the error message
		Errors = ["types don't match"|Errors0],
		InstanceProcIds = []
	).

:- pred check_instance_modes(module_info, assoc_list(proc_id, proc_info),
	pair(list(mode), determinism), proc_id, list(string), list(string)).
:- mode check_instance_modes(in, in, in, out, in, out) is det.

check_instance_modes(ModuleInfo, Procedures, ArgModes, ProcId, 
		Errors0, Errors) :-
	(
		find_first_matching_proc(ModuleInfo, Procedures, ArgModes,
			ProcId0)
	->
		ProcId = ProcId0,
		Errors = Errors0
	;
			% XXX Fix the error message
		Errors = ["no such mode with correct detism for pred/func"|Errors0],
		invalid_proc_id(ProcId)
	).

:- pred find_first_matching_proc(module_info, assoc_list(proc_id, proc_info),
	pair(list(mode), determinism), proc_id).
:- mode find_first_matching_proc(in, in, in, out) is semidet.

find_first_matching_proc(ModuleInfo, [ProcId - ProcInfo|Ps], ArgModes - Detism,
		TheProcId) :-
	proc_info_argmodes(ProcInfo, ProcArgModes),
		% If there was a decl. for the proc, then use that determinism,
		% otherwise use what was inferred.
	proc_info_interface_determinism(ProcInfo, ProcDetism),
	(
		matching_mode_list(ModuleInfo, ProcArgModes, ArgModes),
		ProcDetism = Detism
	->
		TheProcId = ProcId
	;
		find_first_matching_proc(ModuleInfo, Ps, ArgModes - Detism,
			TheProcId)
	).

:- pred matching_mode_list(module_info, list(mode), list(mode)).
:- mode matching_mode_list(in, in, in) is semidet.

matching_mode_list(_, [], []).
matching_mode_list(ModuleInfo, [A|As], [B|Bs]) :-
	mode_get_insts(ModuleInfo, A, Ainit, Afinal),
	mode_get_insts(ModuleInfo, B, Binit, Bfinal),
	inst_matches_final(Ainit, Binit, ModuleInfo),
	inst_matches_final(Afinal, Bfinal, ModuleInfo),
	matching_mode_list(ModuleInfo, As, Bs).

%---------------------------------------------------------------------------%

:- pred check_superclass_conformance(list(class_constraint), list(var),
	varset, module_info, hlds_instance_defn, hlds_instance_defn, 
	list(string), list(string)).
:- mode check_superclass_conformance(in, in, in, in, in, out, in, out) is det.

check_superclass_conformance(SuperClasses0, ClassVars0, ClassVarSet, 
		ModuleInfo, InstanceDefn0, InstanceDefn, Errors0, Errors) :-

	InstanceDefn0 = hlds_instance_defn(A, InstanceConstraints,
		InstanceTypes, D, E, InstanceVarSet0, Proofs0),
	varset__merge_subst(InstanceVarSet0, ClassVarSet, InstanceVarSet1,
		Subst),

		% Make the constraints in terms of the instance variables
	apply_subst_to_constraints(Subst, SuperClasses0, SuperClasses),

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
	module_info_classes(ModuleInfo, ClassTable),

	(
			% Reduce the superclass constraints
		typecheck__reduce_context_by_rule_application(InstanceTable, 
			ClassTable, TypeSubst, InstanceVarSet1, InstanceVarSet2,
			Proofs0, Proofs1, SuperClasses, 
			ReducedSuperClasses0),

			% Reduce the constraints from the instance declaration
		typecheck__reduce_context_by_rule_application(InstanceTable, 
			ClassTable, TypeSubst, InstanceVarSet2, InstanceVarSet2,
			Proofs1, Proofs2, InstanceConstraints,
			ReducedInstanceConstraints0)
	->
		ReducedSuperClasses0 = ReducedSuperClasses,
		ReducedInstanceConstraints = ReducedInstanceConstraints0,
		Proofs = Proofs2, 
		InstanceVarSet = InstanceVarSet2
	;
			% This should never happen, since the superclass and
			% instance constraints must all contain only variables.
			% Context reduction can only fail if there is a 
			% constraint with a type with a bound top-level functor
			% for which there is no instance decl.
		error("check_superclass_conformance: context reduction failed")
	),

		% Check for superclass constraints that are unsatisfied by
		% the instance declaration
	list__delete_elems(ReducedSuperClasses, ReducedInstanceConstraints,
		Unsatisfied),
	(
		Unsatisfied = []
	->
		Errors = Errors0,
		InstanceDefn = hlds_instance_defn(A, InstanceConstraints,
			InstanceTypes, D, E, InstanceVarSet, Proofs)
	;
			% XXX improve the error message
		NewError = "superclass constraint unsatisfied",
		Errors = [NewError|Errors0],
		InstanceDefn = InstanceDefn0
	).

%---------------------------------------------------------------------------%

