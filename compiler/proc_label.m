%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: proc_label.m
% main author: zs

% This file defines backend-independent identifiers for procedures that a
% backend can use as the basis for the names of the labels or functions
% implementing those procedures. It also has functions for creating these
% identifiers.

%-----------------------------------------------------------------------------%

:- module backend_libs__proc_label.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__special_pred.
:- import_module parse_tree__prog_data.

:- import_module bool.

	% A proc_label is a data structure a backend can use to as the basis
	% of the label used as the entry point of a procedure.
	%
	% The defining module is the module that provides the code for the
	% predicate, the declaring module contains the `:- pred' declaration.
	% When these are different, as for specialised versions of predicates
	% from `.opt' files, the defining module's name may need to be added
	% as a qualifier to the label.

:- type proc_label
	--->	proc(
			module_name,	% defining module
			pred_or_func,
			module_name,	% declaring module
			string,		% name
			int,		% arity
			proc_id		% mode number
		)
	;	special_proc(
			module_name,	% defining module
			special_pred_id,% indirectly defines pred name
			module_name,	% type module
			string,		% type name
			int,		% type arity
			proc_id		% mode number
		).

	% Return the id of the procedure specified by the rtti_proc_label.

:- func make_proc_label_from_rtti(rtti_proc_label) = proc_label.

	% Return the id of the specified procedure.

:- func make_proc_label(module_info, pred_id, proc_id) = proc_label.

	% make_user_proc_label(ModuleName, PredIsImported,
	%	PredOrFunc, ModuleName, PredName, Arity, ProcId) = Label:
	% Make a proc_label for a user-defined procedure.
	%
	% The PredIsImported argument should be the result of
	% calling pred_info_is_imported.

:- func make_user_proc_label(module_name, bool, pred_or_func, module_name,
	string, arity, proc_id) = proc_label.

	% Return the id of the specified mode of the unification procedure
	% for the given type.

:- func make_uni_label(module_info, type_ctor, proc_id) = proc_label.

:- implementation.

:- import_module backend_libs__rtti.
:- import_module check_hlds__type_util.
:- import_module hlds__special_pred.
:- import_module parse_tree__prog_util.

:- import_module string, list, std_util, require.

make_proc_label_from_rtti(RttiProcLabel) = ProcLabel :-
	RttiProcLabel = rtti_proc_label(PredOrFunc, ThisModule,
		PredModule, PredName, PredArity, _ArgTypes, _PredId, ProcId,
		_ProcHeadVarsWithNames, _ArgModes, _CodeModel,
		PredIsImported, _PredIsPseudoImported, IsSpecialPred,
		_ProcIsExported, _ProcIsImported),
	( IsSpecialPred = yes(SpecialPred - TypeCtor) ->
		(
			% All type_ctors other than tuples here should be
			% module qualified, since builtin types are
			% handled separately in polymorphism.m.
			(
				TypeCtor = unqualified(TypeName) - _,
				type_ctor_is_tuple(TypeCtor),
				mercury_public_builtin_module(TypeModule)
			;
				TypeCtor = qualified(TypeModule, TypeName) - _
			)
		->
			TypeCtor = _ - TypeArity,
			(
				ThisModule \= TypeModule,
				SpecialPred = unify,
				\+ hlds_pred__in_in_unification_proc_id(ProcId)
			->
				DefiningModule = ThisModule
			;
				DefiningModule = TypeModule
			),
			ProcLabel = special_proc(DefiningModule, SpecialPred,
				TypeModule, TypeName, TypeArity, ProcId)
		;
			string__append_list(["make_proc_label:\n",
				"cannot make label for special pred `",
				PredName, "'"], ErrorMessage),
			error(ErrorMessage)
		)
	;
		ProcLabel = make_user_proc_label(ThisModule, PredIsImported,
			PredOrFunc, PredModule, PredName, PredArity, ProcId)
	).

make_proc_label(ModuleInfo, PredId, ProcId) = ProcLabel :-
	RttiProcLabel = rtti__make_rtti_proc_label(ModuleInfo, PredId, ProcId),
	make_proc_label_from_rtti(RttiProcLabel) = ProcLabel.

make_user_proc_label(ThisModule, PredIsImported, PredOrFunc, PredModule,
		PredName, PredArity, ProcId) = ProcLabel :-
	(
		% Work out which module supplies the code for
		% the predicate.
		ThisModule \= PredModule,
		PredIsImported = no
	->
		% This predicate is a specialized version of
		% a pred from a `.opt' file.
		DefiningModule = ThisModule
	;
		DefiningModule = PredModule
	),
	ProcLabel = proc(DefiningModule, PredOrFunc,
		PredModule, PredName, PredArity, ProcId).

make_uni_label(ModuleInfo, TypeCtor, UniModeNum) = ProcLabel :-
	module_info_name(ModuleInfo, ModuleName),
	( TypeCtor = qualified(TypeModule, TypeName) - Arity ->
		( hlds_pred__in_in_unification_proc_id(UniModeNum) ->
			Module = TypeModule
		;
			Module = ModuleName
		),
		ProcLabel = special_proc(Module, unify, TypeModule,
			TypeName, Arity, UniModeNum)
	;
		error("make_uni_label: unqualified type_ctor")
	).
