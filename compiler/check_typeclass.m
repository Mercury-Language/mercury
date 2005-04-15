%---------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2005 The University of Melbourne.
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
% This pass also checks for cycles in the typeclass hierarchy.
%
% This pass fills in the super class proofs and instance method pred/proc ids
% in the instance table of the HLDS.
%
% Author: dgj.
%
%---------------------------------------------------------------------------%

:- module check_hlds__check_typeclass.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__make_hlds.

:- import_module bool, io.

:- pred check_typeclass__check_typeclasses(qual_info::in, qual_info::out,
	module_info::in, module_info::out, bool::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__typecheck.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_code_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_out.
:- import_module hlds__hlds_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svmulti_map.
:- import_module svset.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

check_typeclass__check_typeclasses(!QualInfo, !ModuleInfo, FoundError, !IO) :-
	check_typeclass__check_instance_decls(!QualInfo, !ModuleInfo,
		FoundInstanceError, !IO),
 	check_for_missing_concrete_instances(!ModuleInfo, !IO),
	module_info_classes(!.ModuleInfo, ClassTable),
	check_for_cyclic_classes(ClassTable, FoundCycleError, !IO),
	FoundError = bool.or(FoundInstanceError, FoundCycleError).

%---------------------------------------------------------------------------%

:- type error_message == pair(prog_context, list(format_component)).
:- type error_messages == list(error_message).

:- pred check_typeclass__check_instance_decls(qual_info::in, qual_info::out,
	module_info::in, module_info::out, bool::out, io::di, io::uo) is det.

check_typeclass__check_instance_decls(!QualInfo, !ModuleInfo, FoundError,
		!IO) :-
	module_info_classes(!.ModuleInfo, ClassTable),
	module_info_instances(!.ModuleInfo, InstanceTable0),
	map__to_assoc_list(InstanceTable0, InstanceList0),
	list__map_foldl2(check_one_class(ClassTable), InstanceList0,
		InstanceList, check_tc_info([], !.ModuleInfo, !.QualInfo),
		check_tc_info(Errors, !:ModuleInfo, !:QualInfo), !IO),
	(
		Errors = []
	->
		map__from_assoc_list(InstanceList, InstanceTable),
		module_info_set_instances(InstanceTable, !ModuleInfo),
		FoundError = no
	;
		list__reverse(Errors, ErrorList),
		WriteError = (pred(E::in, IO0::di, IO::uo) is det :-
			E = ErrorContext - ErrorPieces,
			write_error_pieces(ErrorContext, 0, ErrorPieces,
				IO0, IO)
		),
		list__foldl(WriteError, ErrorList, !IO),
		io__set_exit_status(1, !IO),
		FoundError = yes
	).

:- type check_tc_info
	--->	check_tc_info(
			error_messages	:: error_messages,
			module_info	:: module_info,
			qual_info	:: qual_info
		).

	% Check all the instances of one class.
	%
:- pred check_one_class(class_table::in,
	pair(class_id, list(hlds_instance_defn))::in,
	pair(class_id, list(hlds_instance_defn))::out,
	check_tc_info::in, check_tc_info::out,
	io::di, io::uo) is det.

check_one_class(ClassTable, ClassId - InstanceDefns0,
	ClassId - InstanceDefns, !CheckTCInfo, !IO) :-

	map__lookup(ClassTable, ClassId, ClassDefn),
	ClassDefn = hlds_class_defn(ImportStatus, SuperClasses, ClassVars,
		Interface, ClassInterface, ClassVarSet, TermContext),

	(
		status_defined_in_this_module(ImportStatus, yes),
		Interface = abstract
	->
		ClassId = class_id(ClassName, ClassArity),
		ErrorPieces = [
			words("Error: no definition for typeclass"),
			sym_name_and_arity(ClassName / ClassArity)
		],
		Messages0 = !.CheckTCInfo ^ error_messages,
		!:CheckTCInfo = !.CheckTCInfo ^ error_messages :=
			[TermContext - ErrorPieces | Messages0],
		InstanceDefns = InstanceDefns0
	;
		solutions(
			(pred(PredId::out) is nondet :-
				list__member(ClassProc, ClassInterface),
				ClassProc = hlds_class_proc(PredId, _)
			),
			PredIds),
		list__map_foldl2(
			check_class_instance(ClassId, SuperClasses,
				ClassVars, ClassInterface, Interface,
				ClassVarSet, PredIds),
			InstanceDefns0, InstanceDefns,
			!CheckTCInfo, !IO)
	).

	% Check one instance of one class.
	%
:- pred check_class_instance(class_id::in, list(class_constraint)::in,
	list(tvar)::in, hlds_class_interface::in, class_interface::in,
	tvarset::in, list(pred_id)::in,
	hlds_instance_defn::in, hlds_instance_defn::out,
	check_tc_info::in, check_tc_info::out,
	io::di, io::uo) is det.

check_class_instance(ClassId, SuperClasses, Vars, HLDSClassInterface,
		ClassInterface, ClassVarSet, PredIds,
		InstanceDefn0, InstanceDefn,
		check_tc_info(Errors0, ModuleInfo0, QualInfo0),
		check_tc_info(Errors, ModuleInfo, QualInfo),
		!IO):-

		% check conformance of the instance body
	InstanceDefn0 = hlds_instance_defn(_, _, TermContext, _, _,
		InstanceBody, _, _, _),
	(
		InstanceBody = abstract,
		InstanceDefn1 = InstanceDefn0,
		ModuleInfo = ModuleInfo0,
		QualInfo = QualInfo0,
		Errors1 = Errors0
	;
		InstanceBody = concrete(InstanceMethods),
		check_concrete_class_instance(ClassId, Vars,
			HLDSClassInterface, ClassInterface,
			PredIds, TermContext, InstanceMethods,
			InstanceDefn0, InstanceDefn1, Errors0, Errors1,
			ModuleInfo0, ModuleInfo, QualInfo0, QualInfo, !IO)
	),
		% check that the superclass constraints are satisfied for the
		% types in this instance declaration
	check_superclass_conformance(ClassId, SuperClasses, Vars, ClassVarSet,
		ModuleInfo, InstanceDefn1, InstanceDefn, Errors1, Errors).

:- pred check_concrete_class_instance(class_id::in, list(tvar)::in,
	hlds_class_interface::in, class_interface::in,
	list(pred_id)::in, term__context::in,
	instance_methods::in, hlds_instance_defn::in, hlds_instance_defn::out,
	error_messages::in, error_messages::out,
	module_info::in, module_info::out, qual_info::in, qual_info::out,
	io::di, io::uo) is det.

check_concrete_class_instance(ClassId, Vars, HLDSClassInterface,
		ClassInterface, PredIds, TermContext,
		InstanceMethods, !InstanceDefn, !Errors, !ModuleInfo,
		!QualInfo, !IO) :-
	(
		ClassInterface = abstract,
		ClassId = class_id(ClassName, ClassArity),
		ErrorPieces = [
			words("Error: instance declaration for"),
			words("abstract typeclass"),
			sym_name_and_arity(ClassName / ClassArity),
			suffix(".")	
		],
		!:Errors = [TermContext - ErrorPieces | !.Errors]
	;
		ClassInterface = concrete(_),
		InstanceCheckInfo0 = instance_check_info(!.InstanceDefn,
			[], !.Errors, !.ModuleInfo, !.QualInfo),
		list__foldl2(
			check_instance_pred(ClassId, Vars, HLDSClassInterface),
			PredIds, InstanceCheckInfo0, InstanceCheckInfo, !IO),
		InstanceCheckInfo = instance_check_info(!:InstanceDefn,
			RevInstanceMethods, !:Errors, !:ModuleInfo,
			!:QualInfo),

		%
		% We need to make sure that the MaybePredProcs field is
		% set to yes(_) after this pass.  Normally that will be
		% handled by check_instance_pred, but we also need to handle
		% it below, in case the class has no methods.
		%
		MaybePredProcs1 = !.InstanceDefn ^ instance_hlds_interface,
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

		!:InstanceDefn = ((!.InstanceDefn
			^ instance_hlds_interface := MaybePredProcs)
			^ instance_body := concrete(OrderedInstanceMethods)),

		%
		% Check if there are any instance methods left over,
		% which did not match any of the methods from the
		% class interface.
		%
		Context = !.InstanceDefn ^ instance_context,
		check_for_bogus_methods(InstanceMethods, ClassId, PredIds,
			Context, !.ModuleInfo, !Errors)
	).

	%
	% Check if there are any instance methods left over,
	% which did not match any of the methods from the
	% class interface.  If so, add an appropriate error
	% message to the list of error messages.
	%
:- pred check_for_bogus_methods(list(instance_method)::in, class_id::in,
	list(pred_id)::in, prog_context::in, module_info::in,
	error_messages::in, error_messages::out) is det.

check_for_bogus_methods(InstanceMethods, ClassId, ClassPredIds, Context,
		ModuleInfo1, !Errors) :-
	module_info_get_predicate_table(ModuleInfo1, PredTable),
	DefnIsOK = (pred(Method::in) is semidet :-
		% Find this method definition's p/f, name, arity
		Method = instance_method(MethodPredOrFunc,
			MethodName, _MethodDefn, MethodArity, _Context),
		% Search for pred_ids matching that p/f, name, arity,
		% and succeed if the method definition p/f, name, and
		% arity matches at least one of the methods from the
		% class interface
		adjust_func_arity(MethodPredOrFunc, MethodArity,
			MethodPredArity),
		predicate_table_search_pf_sym_arity(PredTable,
			is_fully_qualified, MethodPredOrFunc,
			MethodName, MethodPredArity, MatchingPredIds),
		some [PredId] (
			list__member(PredId, MatchingPredIds),
			list__member(PredId, ClassPredIds)
		)
	),
	list__filter(DefnIsOK, InstanceMethods, _OKInstanceMethods,
		BogusInstanceMethods),
	(
		BogusInstanceMethods = []
	;
		BogusInstanceMethods = [_ | _],
		%
		% There were one or more bogus methods.
		% Construct an appropriate error message.
		%
		ClassId = class_id(ClassName, ClassArity),
		ErrorMsgStart =  [
			words("In instance declaration for"),
			sym_name_and_arity(ClassName / ClassArity),
			suffix(":"),
			words("incorrect method name(s):")
		],
		ErrorMsgBody0 = list.map(format_method_name,
			BogusInstanceMethods),
		ErrorMsgBody1 = list.condense(ErrorMsgBody0),
		ErrorMsgBody = list__append(ErrorMsgBody1, [suffix(".")]),
		NewError = Context - ( ErrorMsgStart ++ ErrorMsgBody ),
		!:Errors = [NewError | !.Errors]
	).

:- func format_method_name(instance_method) = format_components.

format_method_name(Method) = MethodName :-
	Method = instance_method(PredOrFunc, Name, _Defn, Arity, _Context),
	adjust_func_arity(PredOrFunc, Arity, PredArity),
	MethodName = [
		pred_or_func(PredOrFunc),
		sym_name_and_arity(Name / PredArity)
	].	

%----------------------------------------------------------------------------%

:- type instance_check_info --->
	instance_check_info(
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
:- type instance_method_info --->
	instance_method_info(
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
:- pred check_instance_pred(class_id::in, list(tvar)::in,
	hlds_class_interface::in, pred_id::in,
	instance_check_info::in, instance_check_info::out,
	io::di, io::uo) is det.

check_instance_pred(ClassId, ClassVars, ClassInterface, PredId,
		!InstanceCheckInfo, !IO) :-
	!.InstanceCheckInfo = instance_check_info(InstanceDefn0,
		OrderedMethods0, Errors0, ModuleInfo0, QualInfo0),
	solutions((pred(ProcId::out) is nondet :-
			list__member(ClassProc, ClassInterface),
			ClassProc = hlds_class_proc(PredId, ProcId)
		), ProcIds),
	module_info_pred_info(ModuleInfo0, PredId, PredInfo),
	pred_info_arg_types(PredInfo, ArgTypeVars, ExistQVars, ArgTypes),
	pred_info_get_class_context(PredInfo, ClassContext0),
	pred_info_get_markers(PredInfo, Markers0),
	remove_marker(class_method, Markers0, Markers),
		% The first constraint in the class context of a class method
		% is always the constraint for the class of which it is
		% a member. Seeing that we are checking an instance
		% declaration, we don't check that constraint... the instance
		% declaration itself satisfies it!
	( ClassContext0 = constraints([_ | OtherUnivCs], ExistCs) ->
		UnivCs = OtherUnivCs,
		ClassContext = constraints(UnivCs, ExistCs)
	;
		unexpected(this_file,
			"check_instance_pred: no constraint on class method")
	),

	MethodName0 = pred_info_name(PredInfo),
	PredModule = pred_info_module(PredInfo),
	MethodName = qualified(PredModule, MethodName0),
	PredArity = pred_info_orig_arity(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	adjust_func_arity(PredOrFunc, Arity, PredArity),
	pred_info_procedures(PredInfo, ProcTable),
	list__map((pred(TheProcId::in, ModesAndDetism::out) is det :-
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
		), ProcIds, ArgModes),

	InstanceDefn0 = hlds_instance_defn(_, Status, _, _, InstanceTypes,
		_, _, _, _),

		% Work out the name of the predicate that we will generate
		% to check this instance method.
	make_introduced_pred_name(ClassId, MethodName, Arity,
		InstanceTypes, PredName),

	MethodInfo0 = instance_method_info(ModuleInfo0, QualInfo0, PredName,
		Arity, ExistQVars, ArgTypes, ClassContext, ArgModes,
		Errors0, ArgTypeVars, Status, PredOrFunc),

	check_instance_pred_procs(ClassId, ClassVars, MethodName, Markers,
		InstanceDefn0, InstanceDefn, OrderedMethods0, OrderedMethods,
		MethodInfo0, MethodInfo, !IO),

	MethodInfo = instance_method_info(ModuleInfo, QualInfo, _PredName,
		_Arity, _ExistQVars, _ArgTypes, _ClassContext, _ArgModes,
		Errors, _ArgTypeVars, _Status, _PredOrFunc),

	!:InstanceCheckInfo = instance_check_info(InstanceDefn,
		OrderedMethods, Errors, ModuleInfo, QualInfo).

:- type modes_and_detism
	--->	modes_and_detism(list(mode), inst_varset, maybe(determinism)).

:- pred check_instance_pred_procs(class_id::in, list(tvar)::in, sym_name::in,
	pred_markers::in, hlds_instance_defn::in, hlds_instance_defn::out,
	instance_methods::in, instance_methods::out,
	instance_method_info::in, instance_method_info::out,
	io::di, io::uo) is det.

check_instance_pred_procs(ClassId, ClassVars, MethodName, Markers,
		InstanceDefn0, InstanceDefn, OrderedInstanceMethods0,
		OrderedInstanceMethods, Info0, Info, !IO) :-
	InstanceDefn0 = hlds_instance_defn(InstanceModuleName, B,
		InstanceContext, InstanceConstraints, InstanceTypes,
		InstanceBody, MaybeInstancePredProcs, InstanceVarSet, I),
	Info0 = instance_method_info(ModuleInfo, QualInfo, PredName, Arity,
		ExistQVars, ArgTypes, ClassContext, ArgModes, Errors0,
		ArgTypeVars, Status, PredOrFunc),
	get_matching_instance_defns(InstanceBody, PredOrFunc, MethodName,
		Arity, MatchingInstanceMethods),
	(
		MatchingInstanceMethods = [InstanceMethod],
		OrderedInstanceMethods =
			[InstanceMethod | OrderedInstanceMethods0],
		InstanceMethod = instance_method(_, _, InstancePredDefn,
					_, Context),
		produce_auxiliary_procs(ClassId, ClassVars, Markers,
			InstanceTypes, InstanceConstraints,
			InstanceVarSet, InstanceModuleName,
			InstancePredDefn, Context,
			InstancePredId, InstanceProcIds, Info0, Info, !IO),

		MakeClassProc = (pred(TheProcId::in, PredProcId::out) is det :-
				PredProcId = hlds_class_proc(InstancePredId,
					TheProcId)
			),
		list__map(MakeClassProc, InstanceProcIds, InstancePredProcs1),
		(
			MaybeInstancePredProcs = yes(InstancePredProcs0),
			list__append(InstancePredProcs0,
				InstancePredProcs1, InstancePredProcs)
		;
			MaybeInstancePredProcs = no,
			InstancePredProcs = InstancePredProcs1
		),
		InstanceDefn = hlds_instance_defn(InstanceModuleName, B,
			Context, InstanceConstraints, InstanceTypes,
			InstanceBody, yes(InstancePredProcs), InstanceVarSet, I)
	;
		MatchingInstanceMethods = [I1, I2 | Is],
		%
		% duplicate method definition error
		%
		OrderedInstanceMethods = OrderedInstanceMethods0,
		InstanceDefn = InstanceDefn0,
		ClassId = class_id(ClassName, _ClassArity),
		mdbcomp__prim_data__sym_name_to_string(MethodName, 
			MethodNameString),
		mdbcomp__prim_data__sym_name_to_string(ClassName, 
			ClassNameString),
		PredOrFuncString = pred_or_func_to_string(PredOrFunc),
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
		list__map((pred(Definition::in, ContextAndError::out) is det :-
			Definition = instance_method(_, _, _, _, TheContext),
			Error = [words("Subsequent definition appears here.")],
			ContextAndError = TheContext - Error
		), [I2 | Is], SubsequentErrors),

			% errors are built up in reverse.
		list__append(SubsequentErrors, Heading, NewErrors),
		list__append(NewErrors, Errors0, Errors),
		Info = instance_method_info(ModuleInfo, QualInfo, PredName,
			Arity, ExistQVars, ArgTypes, ClassContext,
			ArgModes, Errors, ArgTypeVars, Status, PredOrFunc)
	;
		MatchingInstanceMethods = [],
		%
		% undefined method error
		%
		OrderedInstanceMethods = OrderedInstanceMethods0,
		InstanceDefn = InstanceDefn0,
		ClassId = class_id(ClassName, _ClassArity),
		mdbcomp__prim_data__sym_name_to_string(ClassName, 
			ClassNameString),
		InstanceTypesString = mercury_type_list_to_string(
			InstanceVarSet, InstanceTypes),
		
		Error = [words("In instance declaration for"),
			fixed("`" ++ ClassNameString 
				++ "(" ++ InstanceTypesString 
				++ ")'"),
			suffix(":"),
			words("no implementation for type class"),
			pred_or_func(PredOrFunc),
			words("method"),
			sym_name_and_arity(MethodName / Arity),
			suffix(".")
		], 
		Errors = [InstanceContext - Error | Errors0],
		Info = instance_method_info(ModuleInfo, QualInfo, PredName,
			Arity, ExistQVars, ArgTypes, ClassContext,
			ArgModes, Errors,
			ArgTypeVars, Status, PredOrFunc)
	).

	%
	% Get all the instance definitions which match the specified
	% predicate/function name/arity, with multiple clause definitions
	% being combined into a single definition.
	%
:- pred get_matching_instance_defns(instance_body::in, pred_or_func::in,
	sym_name::in, arity::in, list(instance_method)::out) is det.

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

:- pred produce_auxiliary_procs(class_id::in, list(tvar)::in, pred_markers::in,
	list(type)::in, list(class_constraint)::in, tvarset::in,
	module_name::in, instance_proc_def::in, prog_context::in,
	pred_id::out, list(proc_id)::out,
	instance_method_info::in, instance_method_info::out,
	io::di, io::uo) is det.

produce_auxiliary_procs(ClassId, ClassVars, Markers0,
		InstanceTypes0, InstanceConstraints0, InstanceVarSet,
		InstanceModuleName, InstancePredDefn, Context, PredId,
		InstanceProcIds, Info0, Info, !IO) :-

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
	map__init(Proofs),
	add_marker(class_instance_method, Markers0, Markers1),
	( InstancePredDefn = name(_) ->
		% For instance methods which are defined using the named
		% syntax (e.g. "pred(...) is ...") rather than the clauses
		% syntax, we record an additional marker; the only effect
		% of this marker is that we output slightly different
		% error messages for such predicates.
		add_marker(named_class_instance_method, Markers1, Markers)
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
		ModuleInfo0, ModuleInfo1, QualInfo0, QualInfo, !IO),

		% Fill in some information in the pred_info which is
		% used by polymorphism to make sure the type-infos
		% and typeclass-infos are added in the correct order.
	MethodConstraints = instance_method_constraints(ClassId,
		InstanceTypes, InstanceConstraints, ClassMethodClassContext),
	pred_info_init(InstanceModuleName, PredName, PredArity, PredOrFunc,
		Context, instance_method(MethodConstraints), Status, none,
		Markers, ArgTypes, ArgTypeVars, ExistQVars, ClassContext,
		Proofs, User, ClausesInfo, PredInfo0),
	pred_info_set_clauses_info(ClausesInfo, PredInfo0, PredInfo1),

		% Add procs with the expected modes and determinisms
	AddProc = (pred(ModeAndDet::in, NewProcId::out,
			OldPredInfo::in, NewPredInfo::out) is det :-
		ModeAndDet = modes_and_detism(Modes, InstVarSet, MaybeDet),
		add_new_proc(InstVarSet, PredArity, Modes, yes(Modes), no,
			MaybeDet, Context, address_is_taken,
			OldPredInfo, NewPredInfo, NewProcId)
	),
	list__map_foldl(AddProc, ArgModes, InstanceProcIds,
		PredInfo1, PredInfo),

	module_info_get_predicate_table(ModuleInfo1, PredicateTable1),
	module_info_get_partial_qualifier_info(ModuleInfo1, PQInfo),
	% XXX why do we need to pass may_be_unqualified here,
	%     rather than passing must_be_qualified or calling the /4 version?
	predicate_table_insert(PredInfo, may_be_unqualified, PQInfo,
		PredId, PredicateTable1, PredicateTable),
	module_info_set_predicate_table(PredicateTable,
		ModuleInfo1, ModuleInfo),

	Info = instance_method_info(ModuleInfo, QualInfo, PredName, Arity,
		ExistQVars, ArgTypes, ClassContext, ArgModes, Errors,
		ArgTypeVars, Status, PredOrFunc).

:- pred apply_substitution_to_var_list(list(var(T))::in,
	map(var(T), term(T))::in, list(var(T))::out) is det.

apply_substitution_to_var_list(Vars0, RenameSubst, Vars) :-
	term__var_list_to_term_list(Vars0, Terms0),
	term__apply_substitution_to_list(Terms0, RenameSubst, Terms),
	term__term_list_to_var_list(Terms, Vars).

%---------------------------------------------------------------------------%

	% Make the name of the introduced pred used to check a particular
	% instance of a particular class method
	%
	% XXX This isn't quite perfect, I suspect

:- pred make_introduced_pred_name(class_id::in, sym_name::in, arity::in,
	list(type)::in, sym_name::out) is det.

make_introduced_pred_name(ClassId, MethodName, Arity,
		InstanceTypes, PredName) :-
	ClassId = class_id(ClassName, _ClassArity),
	mdbcomp__prim_data__sym_name_to_string(ClassName, "__", 
		ClassNameString),
	mdbcomp__prim_data__sym_name_to_string(MethodName, "__", 
		MethodNameString),
		% Perhaps we should include the arity in this mangled
		% string?
	string__int_to_string(Arity, ArityString),
	make_instance_string(InstanceTypes, InstanceString),
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

	% Check that the superclass constraints are satisfied for the
	% types in this instance declaration.

:- pred check_superclass_conformance(class_id::in, list(class_constraint)::in,
	list(tvar)::in, tvarset::in, module_info::in,
	hlds_instance_defn::in, hlds_instance_defn::out,
	error_messages::in, error_messages::out) is det.

check_superclass_conformance(ClassId, SuperClasses0, ClassVars0, ClassVarSet,
		ModuleInfo, InstanceDefn0, InstanceDefn, Errors0, Errors) :-

	InstanceDefn0 = hlds_instance_defn(A, B, Context, InstanceConstraints,
		InstanceTypes, F, G, InstanceVarSet0, Proofs0),
	varset__merge_subst(InstanceVarSet0, ClassVarSet, InstanceVarSet1,
		Subst),

		% Make the constraints in terms of the instance variables
	apply_subst_to_constraint_list(Subst, SuperClasses0, SuperClasses),

		% Now handle the class variables
	map__apply_to_list(ClassVars0, Subst, ClassVarTerms),
	( term__var_list_to_term_list(ClassVars1, ClassVarTerms) ->
		ClassVars = ClassVars1
	;
		unexpected(this_file, "ClassVarTerms are not vars.")
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
		UnprovenConstraints = [],
		Errors = Errors0,
		InstanceDefn = hlds_instance_defn(A, B, Context,
			InstanceConstraints, InstanceTypes, F, G,
			InstanceVarSet2, Proofs1)
	;
		UnprovenConstraints = [_ | _],
		ClassId = class_id(ClassName, _ClassArity),
		mdbcomp__prim_data__sym_name_to_string(ClassName, 
			ClassNameString),
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

:- pred constraint_list_to_string(tvarset::in, list(class_constraint)::in,
	string::out) is det.

constraint_list_to_string(_, [], "").
constraint_list_to_string(VarSet, [C | Cs], String) :-
	String0 = mercury_constraint_to_string(VarSet, C),
	constraint_list_to_string_2(VarSet, Cs, String1),
	string__append_list(["`", String0, "'", String1], String).

:- pred constraint_list_to_string_2(tvarset::in, list(class_constraint)::in,
	string::out) is det.

constraint_list_to_string_2(_VarSet, [], "").
constraint_list_to_string_2(VarSet, [C | Cs], String) :-
	String0 = mercury_constraint_to_string(VarSet, C),
	constraint_list_to_string_2(VarSet, Cs, String1),
	string__append_list([", `", String0, "'", String1], String).

%---------------------------------------------------------------------------%
%
% Check that every abstract instance in the interface of a module
% has a corresponding concrete instance in the implementation.
%

:- pred check_for_missing_concrete_instances(
	module_info::in, module_info::out, io::di, io::uo) is det.

check_for_missing_concrete_instances(!ModuleInfo, !IO) :-
	module_info_instances(!.ModuleInfo, InstanceTable),
	%
	% Grab all the abstract instance declarations in the interface
	% of this module and all the concrete instances defined in the
	% implementation.
	%
	gather_abstract_and_concrete_instances(InstanceTable,
		AbstractInstances, ConcreteInstances),
	map.foldl(check_for_corresponding_instances(ConcreteInstances),
		AbstractInstances, !IO).

	% gather_abstract_and_concrete_instances(Table,
	% 	AbstractInstances, ConcreteInstances).
	%
	% Search the instance_table and create a table of abstract
	% instances that occur in the module interface and a table of
	% concrete instances that occur in the module implementation.
	% Imported instances are not included at all. 
	% 	
:- pred gather_abstract_and_concrete_instances(instance_table::in,
	instance_table::out, instance_table::out) is det.

gather_abstract_and_concrete_instances(InstanceTable, Abstracts,
		Concretes) :-
	map.foldl2(partition_instances_for_class, InstanceTable,
		multi_map.init, Abstracts, multi_map.init, Concretes).

	% Partition all the non-imported instances for a particular
	% class into two groups, those that are abstract and in the
	% module interface and those that are concrete and in the module
	% implementation.  Concrete instances cannot occur in the
	% interface and we ignore abstract instances in the
	% implementation.
	%
:- pred partition_instances_for_class(class_id::in,
	list(hlds_instance_defn)::in, instance_table::in, instance_table::out,
	instance_table::in, instance_table::out) is det.

partition_instances_for_class(ClassId, Instances, !Abstracts, !Concretes) :-
	list.foldl2(partition_instances_for_class_2(ClassId), Instances,
		!Abstracts, !Concretes).

:- pred partition_instances_for_class_2(class_id::in, hlds_instance_defn::in, 
	instance_table::in, instance_table::out,
	instance_table::in, instance_table::out) is det.

partition_instances_for_class_2(ClassId, InstanceDefn, !Abstracts,
		!Concretes) :-
	ImportStatus = InstanceDefn ^ instance_status,
	status_is_imported(ImportStatus, IsImported),
	(
		IsImported = no,
		Body = InstanceDefn ^ instance_body,
		(
			Body = abstract,
			status_is_exported_to_non_submodules(ImportStatus,
				IsExported),
			(
				IsExported = yes,
				svmulti_map.add(ClassId, InstanceDefn,
					!Abstracts)
			;
				IsExported = no
			)
		;
			Body = concrete(_),
			svmulti_map.add(ClassId, InstanceDefn,
				!Concretes)	
		)
	;
		IsImported = yes
	).

:- pred check_for_corresponding_instances(instance_table::in,
	class_id::in, list(hlds_instance_defn)::in, io::di, io::uo) is det.

check_for_corresponding_instances(Concretes, ClassId, InstanceDefns, !IO) :-
	list.foldl(check_for_corresponding_instances_2(Concretes, ClassId),
		InstanceDefns, !IO).

:- pred check_for_corresponding_instances_2(instance_table::in, class_id::in,
	hlds_instance_defn::in, io::di, io::uo) is det.

check_for_corresponding_instances_2(Concretes, ClassId, AbstractInstance,
		!IO) :- 
	AbstractTypes = AbstractInstance ^ instance_types,
	( multi_map.search(Concretes, ClassId, ConcreteInstances) ->		
		(
			list.member(ConcreteInstance, ConcreteInstances),
			ConcreteTypes = ConcreteInstance ^ instance_types,
			ConcreteTypes = AbstractTypes
		->
			MissingConcreteError = no
		;
			% There were concrete instances for ClassId in the
			% implementation but none of them matches the
			% abstract instance we have.
			MissingConcreteError = yes
		)				
	;
		% There were no concrete instances for ClassId in the
		% implementation.
		MissingConcreteError = yes
	),
	(
		MissingConcreteError = yes,
		ClassId = class_id(ClassName, _),
		prim_data.sym_name_to_string(ClassName, ClassNameString),
		AbstractTypesString = mercury_type_list_to_string(
			AbstractInstance ^ instance_tvarset, AbstractTypes),
		AbstractInstanceName = "`" ++ ClassNameString ++
			"(" ++ AbstractTypesString ++ ")'",
		% XXX Should we mention any constraints on the instance
		%     declaration here?
		ErrorPieces = [words("Error: abstract instance declaration"),
			words("for"), fixed(AbstractInstanceName),
			words("has no corresponding concrete"),
			words("instance in the implementation.")
		],	
		AbstractInstanceContext = AbstractInstance ^ instance_context,
		write_error_pieces(AbstractInstanceContext, 0, ErrorPieces,
			!IO),
		io.set_exit_status(1, !IO)	
	;
		MissingConcreteError = no
	).

%---------------------------------------------------------------------------%

:- pred check_for_cyclic_classes(class_table::in, bool::out, io::di, io::uo)
	is det.

check_for_cyclic_classes(ClassTable, Errors, !IO) :-
	ClassIds = map__keys(ClassTable),
	foldl2(find_cycles(ClassTable, []), ClassIds, set.init, _, [], Cycles),
	(
		Cycles = [],
		Errors = no
	;
		Cycles = [_ | _],
		Errors = yes,
		foldl(report_cyclic_classes(ClassTable), Cycles, !IO)
	).

:- type class_path == list(class_id).

	% find_cycles(ClassTable, Path, ClassId, !Visited, !Cycles)
	%
	% Perform a depth first traversal of the class hierarchy, starting
	% from ClassId.  Path contains a list of nodes joining the current
	% node to the root.  When we reach a node that has already been
	% visited, check whether there is a cycle in the Path.
	%
:- pred find_cycles(class_table::in, class_path::in, class_id::in,
	set(class_id)::in, set(class_id)::out,
	list(class_path)::in, list(class_path)::out) is det.

find_cycles(ClassTable, Path, ClassId, !Visited, !Cycles) :-
	(
		set.member(ClassId, !.Visited)
	->
		(
			find_cycle(ClassId, Path, [ClassId], Cycle)
		->
			!:Cycles = [Cycle | !.Cycles]
		;
			true
		)
	;
		svset.insert(ClassId, !Visited),
		ClassIds = get_superclass_ids(ClassTable, ClassId),
		foldl2(find_cycles(ClassTable, [ClassId | Path]), ClassIds,
			!Visited, !Cycles)
	).

	% find_cycle(ClassId, PathRemaining, PathSoFar, Cycle)
	%
	% Check if ClassId is present in PathRemaining, and if so then make
	% a cycle out of the front part of the path up to the point where
	% the ClassId is found.  The part of the path checked so far is
	% accumulated in PathSoFar.
	%
:- pred find_cycle(class_id::in, class_path::in, class_path::in,
	class_path::out) is semidet.

find_cycle(ClassId, [Head | Tail], Path0, Cycle) :-
	Path = [Head | Path0],
	(
		ClassId = Head
	->
		Cycle = Path
	;
		find_cycle(ClassId, Tail, Path, Cycle)
	).

:- func get_superclass_ids(class_table, class_id) = list(class_id).

get_superclass_ids(ClassTable, ClassId) = SuperclassIds :-
	ClassDefn = map.lookup(ClassTable, ClassId),
	SuperclassIds = list.map(get_constraint_id, ClassDefn ^ class_supers).

:- func get_constraint_id(class_constraint) = class_id.

get_constraint_id(constraint(Name, Args)) = class_id(Name, length(Args)).

	% Report an error using the format
	%
	%	module.m:NNN: Error: cyclic superclass relation detected:
	%	module.m:NNN:   `foo/N' <= `bar/N' <= `baz/N' <= `foo/N'
	%
:- pred report_cyclic_classes(class_table::in, class_path::in, io::di, io::uo)
	is det.

report_cyclic_classes(ClassTable, ClassPath, !IO) :-
	(
		ClassPath = [],
		unexpected(this_file,
			"report_cyclic_classes: empty cycle found.")
	;
		ClassPath = [ClassId | Tail],
		Context = map.lookup(ClassTable, ClassId) ^ class_context,
		ClassId = class_id(Name, Arity),
		RevPieces0 = [
			sym_name_and_arity(Name/Arity),
			words("Error: cyclic superclass relation detected:")
		],
		RevPieces1 = foldl(add_path_element, Tail, RevPieces0),
		Pieces = list.reverse(RevPieces1),
		write_error_pieces(Context, 0, Pieces, !IO)
	).

:- func add_path_element(class_id, list(format_component))
	= list(format_component).

add_path_element(class_id(Name, Arity), RevPieces0) =
	[sym_name_and_arity(Name/Arity), words("<=") | RevPieces0].

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "check_typeclass.m".

%---------------------------------------------------------------------------%
:- end_module check_typeclass.
%---------------------------------------------------------------------------%
