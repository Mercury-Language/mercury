%---------------------------------------------------------------------------%
% Copyright (C) 1996-2001 The University of Melbourne.
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

:- import_module hlds_module, make_hlds.
:- import_module bool, io.

:- pred check_typeclass__check_instance_decls(module_info, qual_info,
	module_info, qual_info, bool, io__state, io__state).
:- mode check_typeclass__check_instance_decls(in, in, out, out,
	out, di, uo) is det.

:- implementation.

:- import_module prog_data, prog_out, prog_util.
:- import_module hlds_pred, hlds_data, hlds_goal, hlds_out.
:- import_module type_util, typecheck, mode_util, inst_match.
:- import_module base_typeclass_info.
:- import_module mercury_to_mercury, error_util.
:- import_module globals, options. 

:- import_module int, string.
:- import_module list, assoc_list, map, set, term, varset.
:- import_module std_util, require. 

:- type error_message == pair(prog_context, list(format_component)).
:- type error_messages == list(error_message).

check_typeclass__check_instance_decls(ModuleInfo0, QualInfo0,
		ModuleInfo, QualInfo, FoundError, IO0, IO) :-
	module_info_classes(ModuleInfo0, ClassTable),
	module_info_instances(ModuleInfo0, InstanceTable0),
	map__to_assoc_list(InstanceTable0, InstanceList0),
	list_map_foldl2(check_one_class(ClassTable), InstanceList0,
		InstanceList, check_tc_info([], ModuleInfo0, QualInfo0),
		check_tc_info(Errors, ModuleInfo1, QualInfo),
		IO0, IO1),
	(
		Errors = []
	->
		map__from_assoc_list(InstanceList, InstanceTable),
		module_info_set_instances(ModuleInfo1, InstanceTable,
			ModuleInfo),
		IO = IO1,
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
		list__foldl(WriteError, ErrorList, IO1, IO2),
		io__set_exit_status(1, IO2, IO),
		FoundError = yes
	).  

:- type check_tc_info
	--->	check_tc_info(
			error_messages :: error_messages,
			module_info :: module_info,
			qual_info :: qual_info
		).

	% list__map_foldl2(Pred, InList, OutList, StartA, EndA, StartB, EndB)
	% calls Pred with two accumulator (with the initial values of
	% StartA and StartB respectively) on each element of InList
	% (working left-to-right) to transform InList into OutList.
	% The final values of the accumulators are returned in EndA
	% and EndB respectively.
:- pred list_map_foldl2(pred(X, Y, Z, Z, W, W), list(X), list(Y), Z, Z, W, W).
:- mode list_map_foldl2(pred(in, out, in, out, di, uo) is det,
			     in, out, in, out, di, uo) is det.

list_map_foldl2(_, [],  [], A, A) -->
        [].
list_map_foldl2(P, [H0|T0], [H|T], A0, A) -->
        call(P, H0, H, A0, A1),
        list_map_foldl2(P, T0, T, A1, A).
		
	% check all the instances of one class.
:- pred check_one_class(class_table,
	pair(class_id, list(hlds_instance_defn)), 
	pair(class_id, list(hlds_instance_defn)), 
	check_tc_info, check_tc_info,
	io__state, io__state).
:- mode check_one_class(in, in, out, in, out, di, uo) is det.

check_one_class(ClassTable, ClassId - InstanceDefns0, 
	ClassId - InstanceDefns, CheckTCInfo0, CheckTCInfo, IO0, IO) :-

	map__lookup(ClassTable, ClassId, ClassDefn),
	ClassDefn = hlds_class_defn(ImportStatus, SuperClasses, ClassVars,
		Interface, ClassInterface, ClassVarSet, TermContext),

	(
		status_defined_in_this_module(ImportStatus, yes),
		Interface = abstract
	->
		ClassId = class_id(ClassName, ClassArity),
		sym_name_and_arity_to_string(ClassName / ClassArity,
			ClassNameStr),
		ErrorPieces = [
			words("Error: no definition for typeclass"),
			words(string__append_list(["`", ClassNameStr, "'."]))
		],

		Messages0 = CheckTCInfo0 ^ error_messages,
		CheckTCInfo = CheckTCInfo0 ^ error_messages :=
				[TermContext - ErrorPieces | Messages0],
		InstanceDefns = InstanceDefns0,
		IO = IO0
	;
		solutions(
			(pred(PredId::out) is nondet :-
				list__member(ClassProc, ClassInterface),
				ClassProc = hlds_class_proc(PredId, _)
			),
			PredIds),
		list_map_foldl2(
			check_class_instance(ClassId, SuperClasses,
				ClassVars, ClassInterface, Interface,
				ClassVarSet, PredIds),
			InstanceDefns0, InstanceDefns, 
			CheckTCInfo0, CheckTCInfo,
			IO0, IO)
	).

	% check one instance of one class
:- pred check_class_instance(class_id, list(class_constraint), list(tvar),
	hlds_class_interface, class_interface, tvarset, list(pred_id), 
	hlds_instance_defn, hlds_instance_defn, 
	check_tc_info, check_tc_info, 
	io__state, io__state).
:- mode check_class_instance(in, in, in, in, in, in, in, in, out, in, out,
	di, uo) is det.

check_class_instance(ClassId, SuperClasses, Vars, HLDSClassInterface,
		ClassInterface, ClassVarSet, PredIds, InstanceDefn0,
		InstanceDefn, 
		check_tc_info(Errors0, ModuleInfo0, QualInfo0),
		check_tc_info(Errors, ModuleInfo, QualInfo),
		IO0, IO):-
		
		% check conformance of the instance body
	InstanceDefn0 = hlds_instance_defn(_, _, TermContext, _, _,
		InstanceBody, _, _, _),
	(
	    InstanceBody = abstract,
	    InstanceDefn2 = InstanceDefn0,
	    ModuleInfo1 = ModuleInfo0,
	    QualInfo = QualInfo0,
	    Errors2 = Errors0,
	    IO = IO0
	;
	    InstanceBody = concrete(InstanceMethods),
	    (
	    	ClassInterface = abstract,
		ClassId = class_id(ClassName, ClassArity),
		sym_name_and_arity_to_string(ClassName / ClassArity,
			ClassNameStr),
		ErrorPieces = [
			words("Error: instance declaration for"),
			words("abstract typeclass"),
			words(string__append_list(["`", ClassNameStr, "'."]))
		],
		Errors2 = [TermContext - ErrorPieces | Errors0],
		InstanceDefn2 = InstanceDefn0,
		ModuleInfo1 = ModuleInfo0,
		QualInfo = QualInfo0,
		IO = IO0
	    ;
	    	ClassInterface = concrete(_),
		InstanceCheckInfo0 = instance_check_info(InstanceDefn0,
				[], Errors0, ModuleInfo0, QualInfo0),
		list__foldl2(
			check_instance_pred(ClassId, Vars, HLDSClassInterface), 
			PredIds, InstanceCheckInfo0, InstanceCheckInfo,
			IO0, IO),
		InstanceCheckInfo = instance_check_info(InstanceDefn1,
				RevInstanceMethods, Errors1, ModuleInfo1,
				QualInfo), 
			
		%
		% We need to make sure that the MaybePredProcs field is
		% set to yes(_) after this pass.  Normally that will be
		% handled by check_instance_pred, but we also need to handle
		% it below, in case the class has no methods.
		%
		InstanceDefn1 = hlds_instance_defn(A, B, C, D, E, _, 
				MaybePredProcs1, H, I),
		(
			MaybePredProcs1 = yes(_),
			MaybePredProcs = MaybePredProcs1
		;
			MaybePredProcs1 = no,
			MaybePredProcs = yes([])
		),

		%
		% Make sure the list of instance methods is in the same
		% order as the methods in the class definition. intermod.m
		% relies on this
		OrderedInstanceMethods = list__reverse(RevInstanceMethods),

		InstanceDefn2 = hlds_instance_defn(A, B, C, D, E,
				concrete(OrderedInstanceMethods),
				MaybePredProcs, H, I),

		%
		% Check if there are any instance methods left over,
		% which did not match any of the methods from the
		% class interface.
		%
		InstanceDefn2 = hlds_instance_defn(_, _, Context,
			_, _, _, _, _, _),
		check_for_bogus_methods(InstanceMethods, ClassId, PredIds,
			Context, ModuleInfo1, Errors1, Errors2)
	    )
	),

		% check that the superclass constraints are satisfied for the
		% types in this instance declaration
	check_superclass_conformance(ClassId, SuperClasses, Vars, ClassVarSet,
		InstanceDefn2, InstanceDefn,
		Errors2 - ModuleInfo1, Errors - ModuleInfo).

		%
		% Check if there are any instance methods left over,
		% which did not match any of the methods from the
		% class interface.  If so, add an appropriate error
		% message to the list of error messages.
		%
:- pred check_for_bogus_methods(list(instance_method), class_id, list(pred_id),
		prog_context, module_info, error_messages, error_messages).
:- mode check_for_bogus_methods(in, in, in, in, in, in, out) is det.

check_for_bogus_methods(InstanceMethods, ClassId, ClassPredIds, Context,
		ModuleInfo1, Errors0, Errors) :-
	module_info_get_predicate_table(ModuleInfo1, PredTable),
	DefnIsOK = (pred(Method::in) is semidet :-
		% Find this method definition's p/f, name, arity
		Method = instance_method(MethodPredOrFunc,
			MethodName, _MethodDefn,
			MethodArity, _Context),
		% Search for pred_ids matching that p/f, name, arity,
		% and succeed if the method definition p/f, name, and
		% arity matches at least one of the methods from the
		% class interface
		adjust_func_arity(MethodPredOrFunc, MethodArity,
			MethodPredArity),
		predicate_table_search_pf_sym_arity(PredTable,
			MethodPredOrFunc, MethodName, MethodPredArity,
			MatchingPredIds),
		some [PredId] (
			list__member(PredId, MatchingPredIds),
			list__member(PredId, ClassPredIds)
		)
	),
	list__filter(DefnIsOK, InstanceMethods, _OKInstanceMethods,
		BogusInstanceMethods),
	(
		BogusInstanceMethods = []
	->
		Errors = Errors0
	;
		%
		% There were one or more bogus methods.
		% Construct an appropriate error message.
		%
		ClassId = class_id(ClassName, ClassArity),
		prog_out__sym_name_to_string(ClassName,
			ClassNameString),
		string__int_to_string(ClassArity, ClassArityString),
		string__append_list([
			"In instance declaration for `",
			ClassNameString, "/", ClassArityString, "': ",
			"incorrect method name(s): "],
			ErrorMsgStart),
		BogusInstanceMethodNames = list__map(format_method_name,
			BogusInstanceMethods),
		error_util__list_to_pieces(BogusInstanceMethodNames,
			ErrorMsgBody0),
		ErrorMsgBody = list__append(ErrorMsgBody0, [words(".")]),
		NewError = Context - [words(ErrorMsgStart) | ErrorMsgBody],
		Errors = [NewError | Errors0]
	).

:- func format_method_name(instance_method) = string.
format_method_name(Method) = StringName :-
	Method = instance_method(PredOrFunc, Name, _Defn, Arity, _Context),
	adjust_func_arity(PredOrFunc, Arity, PredArity),
	hlds_out__simple_call_id_to_string(
		PredOrFunc - Name/PredArity, StringName).

%----------------------------------------------------------------------------%

:- type instance_check_info
	---> instance_check_info(
		hlds_instance_defn,
		instance_methods,	% The instance methods in reverse
					% order of the methods in the class
					% declaration.
		error_messages,
		module_info,
		qual_info
	).

	% This structure holds the information about a particular instance
	% method
:- type instance_method_info ---> instance_method_info(
		module_info,
		qual_info,
		sym_name,				% Name that the
							% introduced pred
							% should be given.
		arity,					% Arity of the method.
							% (For funcs, this is
							% the original arity,
							% not the arity as a
							% predicate.)
		existq_tvars,				% Existentially quant.
							% type variables
		list(type),				% Expected types of
							% arguments.
		class_constraints,			% Constraints from
							% class method.
		list(modes_and_detism),			% Modes and 
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
:- pred check_instance_pred(class_id, list(tvar), hlds_class_interface, 
	pred_id, instance_check_info, instance_check_info,
	io__state, io__state).
:- mode check_instance_pred(in,in, in, in, in, out, di, uo) is det.

check_instance_pred(ClassId, ClassVars, ClassInterface, PredId,
		InstanceCheckInfo0, InstanceCheckInfo, IO0, IO) :-

	InstanceCheckInfo0 = instance_check_info(InstanceDefn0,
				OrderedMethods0, Errors0, ModuleInfo0,
				QualInfo0),
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
	pred_info_get_markers(PredInfo, Markers0),
	remove_marker(Markers0, class_method, Markers),
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
	adjust_func_arity(PredOrFunc, Arity, PredArity),
	pred_info_procedures(PredInfo, ProcTable),
	list__map(
		lambda([TheProcId::in, ModesAndDetism::out] is det, 
			(
				map__lookup(ProcTable, TheProcId, ProcInfo), 
				proc_info_argmodes(ProcInfo, Modes),
				% if the determinism declaration on the method
				% was omitted, then make_hlds.m will have
				% already issued an error message, so
				% don't complain here.
				proc_info_declared_determinism(ProcInfo, 
					MaybeDetism),
				proc_info_inst_varset(ProcInfo, InstVarSet),
				ModesAndDetism = modes_and_detism(Modes,
						InstVarSet, MaybeDetism)
			)),
		ProcIds, 
		ArgModes),
	
	InstanceDefn0 = hlds_instance_defn(_, Status, _, _, InstanceTypes, 
		_, _, _, _),

		% Work out the name of the predicate that we will generate
		% to check this instance method.
	make_introduced_pred_name(ClassId, MethodName, Arity, 
		InstanceTypes, PredName),
	
	Info0 = instance_method_info(ModuleInfo0, QualInfo0, PredName,
		Arity, ExistQVars, ArgTypes, ClassContext, ArgModes,
		Errors0, ArgTypeVars, Status, PredOrFunc),

	check_instance_pred_procs(ClassId, ClassVars, MethodName, Markers,
		InstanceDefn0, InstanceDefn, OrderedMethods0, OrderedMethods,
		Info0, Info, IO0, IO),

	Info = instance_method_info(ModuleInfo, QualInfo, _PredName,
		_Arity, _ExistQVars, _ArgTypes, _ClassContext, _ArgModes,
		Errors, _ArgTypeVars, _Status, _PredOrFunc),

	InstanceCheckInfo = instance_check_info(InstanceDefn,
				OrderedMethods, Errors, ModuleInfo, QualInfo).

:- type modes_and_detism
	--->	modes_and_detism(list(mode), inst_varset, maybe(determinism)).

:- pred check_instance_pred_procs(class_id, list(tvar), sym_name, pred_markers,
	hlds_instance_defn, hlds_instance_defn, 
	instance_methods, instance_methods,
	instance_method_info, instance_method_info,
	io__state, io__state).
:- mode check_instance_pred_procs(in, in, in, in, in, out,
	in, out, in, out, di, uo) is det.

check_instance_pred_procs(ClassId, ClassVars, MethodName, Markers,
		InstanceDefn0, InstanceDefn, OrderedInstanceMethods0,
		OrderedInstanceMethods, Info0, Info, IO0, IO) :-
	InstanceDefn0 = hlds_instance_defn(A, B, InstanceContext, 
				InstanceConstraints, InstanceTypes,
				InstanceBody, MaybeInstancePredProcs,
				InstanceVarSet, I),
	Info0 = instance_method_info(ModuleInfo, QualInfo, PredName, Arity,
		ExistQVars, ArgTypes, ClassContext, ArgModes, Errors0,
		ArgTypeVars, Status, PredOrFunc),
	get_matching_instance_defns(InstanceBody, PredOrFunc, MethodName,
		Arity, MatchingInstanceMethods),
	(
		MatchingInstanceMethods = [InstanceMethod]
	->
		OrderedInstanceMethods =
			[InstanceMethod | OrderedInstanceMethods0],
		InstanceMethod = instance_method(_, _, InstancePredDefn,
					_, Context),
		produce_auxiliary_procs(ClassId, ClassVars, Markers,
			InstanceTypes, InstanceConstraints, 
			InstanceVarSet, 
			InstancePredDefn, Context,
			InstancePredId, InstanceProcIds, Info0, Info,
			IO0, IO),

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
		InstanceDefn = hlds_instance_defn(A, B, Context, 
			InstanceConstraints, InstanceTypes, InstanceBody,
			yes(InstancePredProcs), InstanceVarSet, I)
	;
		MatchingInstanceMethods = [I1, I2 | Is]
	->
		%
		% duplicate method definition error
		%
		OrderedInstanceMethods = OrderedInstanceMethods0,
		InstanceDefn = InstanceDefn0,
		ClassId = class_id(ClassName, _ClassArity),
		prog_out__sym_name_to_string(MethodName, MethodNameString),
		prog_out__sym_name_to_string(ClassName, ClassNameString),
		pred_or_func_to_string(PredOrFunc, PredOrFuncString),
		string__int_to_string(Arity, ArityString),
		InstanceTypesString = mercury_type_list_to_string(
			InstanceVarSet, InstanceTypes),
		string__append_list([
			"In instance declaration for `",
			ClassNameString, "(", InstanceTypesString, ")': ",
			"multiple implementations of type class ",
			PredOrFuncString, " method `",
			MethodNameString, "/", ArityString, "'."],
			ErrorHeader),
		I1 = instance_method(_, _, _, _, I1Context), 
		Heading = 
			[I1Context - [words("First definition appears here.")],
			InstanceContext - [words(ErrorHeader)]],
		list__map(lambda([Definition::in, ContextAndError::out] is det,
		(
			Definition = instance_method(_, _, _, _, TheContext),
			Error = [words("Subsequent definition appears here.")],
			ContextAndError = TheContext - Error
		)), [I2|Is], SubsequentErrors),
			
			% errors are built up in reverse.
		list__append(SubsequentErrors, Heading, NewErrors),
		list__append(NewErrors, Errors0, Errors),
		Info = instance_method_info(ModuleInfo, QualInfo, PredName,
			Arity, ExistQVars, ArgTypes, ClassContext,
			ArgModes, Errors, ArgTypeVars, Status, PredOrFunc),
		IO = IO0
	;
		%
		% undefined method error
		%
		OrderedInstanceMethods = OrderedInstanceMethods0,
		InstanceDefn = InstanceDefn0,
		ClassId = class_id(ClassName, _ClassArity),
		prog_out__sym_name_to_string(MethodName, MethodNameString),
		prog_out__sym_name_to_string(ClassName, ClassNameString),
		pred_or_func_to_string(PredOrFunc, PredOrFuncString),
		string__int_to_string(Arity, ArityString),
		InstanceTypesString = mercury_type_list_to_string(
			InstanceVarSet, InstanceTypes),
		string__append_list([
			"In instance declaration for `",
			ClassNameString, "(", InstanceTypesString, ")': ",
			"no implementation for type class ",
			PredOrFuncString, " method `",
			MethodNameString, "/", ArityString, "'."],
			NewError),
		Errors = [InstanceContext - [words(NewError)] | Errors0],
		Info = instance_method_info(ModuleInfo, QualInfo, PredName,
			Arity, ExistQVars, ArgTypes, ClassContext,
			ArgModes, Errors,
			ArgTypeVars, Status, PredOrFunc),
		IO = IO0
	).

	%
	% Get all the instance definitions which match the specified
	% predicate/function name/arity, with multiple clause definitions
	% being combined into a single definition.
	%
:- pred get_matching_instance_defns(instance_body, pred_or_func,
	sym_name, arity, list(instance_method)).
:- mode get_matching_instance_defns(in, in, in, in, out) is det.

get_matching_instance_defns(abstract, _, _, _, []).
get_matching_instance_defns(concrete(InstanceMethods), PredOrFunc, MethodName,
		MethodArity, ResultList) :-
	%
	% First find the instance method definitions that match this
	% predicate/function's name and arity
	%
	list__filter(
		(pred(Method::in) is semidet :-
			Method = instance_method(PredOrFunc,
				MethodName, _MethodDefn,
				MethodArity, _Context)
		),
		InstanceMethods, MatchingMethods),
	(
		MatchingMethods = [First, _Second | _],
		First = instance_method(_, _, _, _, FirstContext),
		\+ (
			list__member(DefnViaName, MatchingMethods),
			DefnViaName = instance_method(_, _, name(_), _, _)
		)
	->
		%
		% If all of the instance method definitions for this
		% pred/func are clauses, and there are more than one
		% of them, then we must combine them all into a
		% single definition.
		%
		MethodToClause = (pred(Method::in, Clauses::out) is semidet :-
			Method = instance_method(_, _, Defn, _, _),
			Defn = clauses(Clauses)),
		list__filter_map(MethodToClause, MatchingMethods, ClausesList),
		list__condense(ClausesList, FlattenedClauses),
		CombinedMethod = instance_method(PredOrFunc,
			MethodName, clauses(FlattenedClauses),
			MethodArity, FirstContext),
		ResultList = [CombinedMethod]
	;
		%
		% If there are less than two matching method definitions,
		% or if any of the instance method definitions is a method
		% name, then we're done.
		%
		ResultList = MatchingMethods
	).
	
:- pred pred_or_func_to_string(pred_or_func::in, string::out) is det.
pred_or_func_to_string(predicate, "predicate").
pred_or_func_to_string(function, "function").

:- pred produce_auxiliary_procs(class_id, list(tvar), pred_markers, list(type),
	list(class_constraint), tvarset, instance_proc_def, prog_context,
	pred_id, list(proc_id), instance_method_info, instance_method_info,
	io__state, io__state).
:- mode produce_auxiliary_procs(in, in, in, in, in, in, in, in, out, out, 
	in, out, di, uo) is det.

produce_auxiliary_procs(ClassId, ClassVars, Markers0,
		InstanceTypes0, InstanceConstraints0, InstanceVarSet,
		InstancePredDefn, Context, PredId,
		InstanceProcIds, Info0, Info, IO0, IO) :-

	Info0 = instance_method_info(ModuleInfo0, QualInfo0, PredName,
		Arity, ExistQVars0, ArgTypes0, ClassMethodClassContext0,
		ArgModes, Errors, ArgTypeVars0, Status0, PredOrFunc),

		% Rename the instance variables apart from the class variables
	varset__merge_subst(ArgTypeVars0, InstanceVarSet, ArgTypeVars1,
		RenameSubst),
	term__apply_substitution_to_list(InstanceTypes0, RenameSubst,
		InstanceTypes1),
	apply_subst_to_constraint_list(RenameSubst, InstanceConstraints0,
		InstanceConstraints1),

		% Work out what the type variables are bound to for this
		% instance, and update the class types appropriately.
	map__from_corresponding_lists(ClassVars, InstanceTypes1, TypeSubst),
	term__apply_substitution_to_list(ArgTypes0, TypeSubst, ArgTypes1),
	apply_subst_to_constraints(TypeSubst, ClassMethodClassContext0,
		ClassMethodClassContext1),

		% Get rid of any unwanted type variables
	term__vars_list(ArgTypes1, VarsToKeep0),
	list__sort_and_remove_dups(VarsToKeep0, VarsToKeep),
	varset__squash(ArgTypeVars1, VarsToKeep, ArgTypeVars, SquashSubst),
	term__apply_variable_renaming_to_list(ArgTypes1, SquashSubst, 
		ArgTypes),
	apply_variable_renaming_to_constraints(SquashSubst,
		ClassMethodClassContext1, ClassMethodClassContext),
	apply_partial_map_to_list(ExistQVars0, SquashSubst, ExistQVars),
	apply_variable_renaming_to_list(InstanceTypes1, SquashSubst,
		InstanceTypes),
	apply_variable_renaming_to_constraint_list(SquashSubst,
		InstanceConstraints1, InstanceConstraints),

		% Add the constraints from the instance declaration to the 
		% constraints from the class method. This allows an instance
		% method to have constraints on it which are not part of the
		% instance declaration as a whole.
	ClassMethodClassContext = constraints(UnivConstraints1,
					ExistConstraints),
	list__append(InstanceConstraints, UnivConstraints1, UnivConstraints),
	ClassContext = constraints(UnivConstraints, ExistConstraints),

		% Introduce a new predicate which calls the implementation
		% given in the instance declaration.
	module_info_name(ModuleInfo0, ModuleName),

	Cond = true,
	map__init(Proofs),
	add_marker(Markers0, class_instance_method, Markers1),
	( InstancePredDefn = name(_) ->
		% For instance methods which are defined using the named
		% syntax (e.g. "pred(...) is ...") rather than the clauses
		% syntax, we record an additional marker; the only effect
		% of this marker is that we output slightly different
		% error messages for such predicates.
		add_marker(Markers1, named_class_instance_method, Markers)
	;
		Markers = Markers1
	),
	module_info_globals(ModuleInfo0, Globals),
	globals__lookup_string_option(Globals, aditi_user, User),

	( status_is_imported(Status0, yes) ->
		Status = opt_imported
	;
		Status = Status0
	),

	adjust_func_arity(PredOrFunc, Arity, PredArity),
	produce_instance_method_clauses(InstancePredDefn, PredOrFunc,
		PredArity, ArgTypes, Markers, Context, Status, ClausesInfo,
		ModuleInfo0, ModuleInfo1, QualInfo0, QualInfo, IO0, IO),

	pred_info_init(ModuleName, PredName, PredArity, ArgTypeVars, 
		ExistQVars, ArgTypes, Cond, Context, ClausesInfo, Status,
		Markers, none, PredOrFunc, ClassContext, Proofs, User,
		PredInfo0),
	pred_info_set_clauses_info(PredInfo0, ClausesInfo, PredInfo1),

	% Fill in some information in the pred_info which is
	% used by polymorphism to make sure the type-infos
	% and typeclass-infos are added in the correct order.
	MethodConstraints = instance_method_constraints(ClassId,
			InstanceTypes, InstanceConstraints,
			ClassMethodClassContext),
	pred_info_set_maybe_instance_method_constraints(PredInfo1,
		yes(MethodConstraints), PredInfo2),

		% Add procs with the expected modes and determinisms
	AddProc = lambda([ModeAndDet::in, NewProcId::out,
			OldPredInfo::in, NewPredInfo::out] is det,
	(
		ModeAndDet = modes_and_detism(Modes, InstVarSet, MaybeDet),
		add_new_proc(OldPredInfo, InstVarSet, PredArity, Modes,
			yes(Modes), no, MaybeDet, Context, address_is_taken,
			NewPredInfo, NewProcId)
	)),
	list__map_foldl(AddProc, ArgModes, InstanceProcIds, 
		PredInfo2, PredInfo),

	module_info_get_predicate_table(ModuleInfo1, PredicateTable1),
	module_info_get_partial_qualifier_info(ModuleInfo1, PQInfo),
	% XXX why do we need to pass may_be_unqualified here,
	%     rather than passing must_be_qualified or calling the /4 version?
	predicate_table_insert(PredicateTable1, PredInfo,
		may_be_unqualified, PQInfo, PredId, PredicateTable),
	module_info_set_predicate_table(ModuleInfo1, PredicateTable,
		ModuleInfo),

	Info = instance_method_info(ModuleInfo, QualInfo, PredName, Arity,
		ExistQVars, ArgTypes, ClassContext, ArgModes, Errors,
		ArgTypeVars, Status, PredOrFunc).

:- pred apply_substitution_to_var_list(list(var(T)), map(var(T), term(T)),
		list(var(T))).
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

make_introduced_pred_name(ClassId, MethodName, Arity, 
		InstanceTypes, PredName) :-
	ClassId = class_id(ClassName, _ClassArity),
	prog_out__sym_name_to_string(ClassName, "__", ClassNameString),
	prog_out__sym_name_to_string(MethodName, "__", MethodNameString),
		% Perhaps we should include the arity in this mangled
		% string?
	string__int_to_string(Arity, ArityString),
	base_typeclass_info__make_instance_string(InstanceTypes, 
		InstanceString),
	string__append_list(
		[check_typeclass__introduced_pred_name_prefix,
		ClassNameString, "____",
		InstanceString, "____",
		MethodNameString, "_",
		ArityString], 
		PredNameString),
	PredName = unqualified(PredNameString).

	% The prefix added to the class method name for the predicate
	% used to call a class method for a specific instance.
:- func check_typeclass__introduced_pred_name_prefix = string.

check_typeclass__introduced_pred_name_prefix = "ClassMethod_for_".

%---------------------------------------------------------------------------%

% check that the superclass constraints are satisfied for the
% types in this instance declaration

:- pred check_superclass_conformance(class_id, list(class_constraint), 
	list(tvar), tvarset, hlds_instance_defn, hlds_instance_defn, 
	pair(error_messages, module_info), pair(error_messages, module_info)).
:- mode check_superclass_conformance(in, in, in, in, in, out, in, out) is det.

check_superclass_conformance(ClassId, SuperClasses0, ClassVars0, ClassVarSet, 
		InstanceDefn0, InstanceDefn, 
		Errors0 - ModuleInfo, Errors - ModuleInfo) :-

	InstanceDefn0 = hlds_instance_defn(A, B, Context, InstanceConstraints,
		InstanceTypes, F, G, InstanceVarSet0, Proofs0),
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
		InstanceDefn = hlds_instance_defn(A, B, Context, 
			InstanceConstraints, InstanceTypes, F, G,
			InstanceVarSet2, Proofs1)
	;
		ClassId = class_id(ClassName, _ClassArity),
		prog_out__sym_name_to_string(ClassName, ClassNameString),
		InstanceTypesString = mercury_type_list_to_string(
			InstanceVarSet2, InstanceTypes),
		constraint_list_to_string(ClassVarSet, UnprovenConstraints, 
			ConstraintsString),
		string__append_list([
			"In instance declaration for `",
			ClassNameString, "(", InstanceTypesString, ")': ",
			"superclass constraint(s) not satisfied: ",
			ConstraintsString, "."],
			NewError),
		Errors = [Context - [words(NewError)] | Errors0],
		InstanceDefn = InstanceDefn0
	).

:- pred constraint_list_to_string(tvarset, list(class_constraint), string).
:- mode constraint_list_to_string(in, in, out) is det.

constraint_list_to_string(_, [], "").
constraint_list_to_string(VarSet, [C|Cs], String) :-
	String0 = mercury_constraint_to_string(VarSet, C),
	constraint_list_to_string_2(VarSet, Cs, String1),
	string__append_list(["`", String0, "'", String1], String).

:- pred constraint_list_to_string_2(tvarset, list(class_constraint), string).
:- mode constraint_list_to_string_2(in, in, out) is det.

constraint_list_to_string_2(_VarSet, [], "").
constraint_list_to_string_2(VarSet, [C|Cs], String) :-
	String0 = mercury_constraint_to_string(VarSet, C),
	constraint_list_to_string_2(VarSet, Cs, String1),
	string__append_list([", `", String0, "'", String1], String).

%---------------------------------------------------------------------------%
