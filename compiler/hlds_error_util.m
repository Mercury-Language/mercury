%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% error_util.m
% Main author: zs.
%
% This module contains code that can be helpful in the formatting of
% error messages. It builds upon parse_tree__error_util, and extends it
% with predicates that access HLDS data structures.
%
%-----------------------------------------------------------------------------%

:- module hlds__hlds_error_util.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__error_util.

:- import_module assoc_list.
:- import_module list.
:- import_module std_util.

	% Predicates to convert a predicate and procedure names to strings.

:- type should_module_qualify
	--->	should_module_qualify
	;	should_not_module_qualify.

:- func describe_one_pred_name(module_info, should_module_qualify, pred_id)
	= list(format_component).

:- func describe_one_pred_info_name(should_module_qualify, pred_info)
	= list(format_component).

:- func describe_one_pred_name_mode(module_info, should_module_qualify,
	pred_id, inst_varset, list(mode)) = list(format_component).

:- func describe_several_pred_names(module_info, should_module_qualify,
	list(pred_id)) = list(format_component).

:- func describe_one_proc_name(module_info, should_module_qualify,
	pred_proc_id) = list(format_component).

:- func describe_one_proc_name_mode(module_info, should_module_qualify,
	pred_proc_id) = list(format_component).

:- func describe_several_proc_names(module_info, should_module_qualify,
	list(pred_proc_id)) = list(format_component).

:- func describe_one_call_site(module_info, should_module_qualify,
	pair(pred_proc_id, prog_context)) = list(format_component).

:- func describe_several_call_sites(module_info, should_module_qualify,
	assoc_list(pred_proc_id, prog_context)) = list(format_component).

:- implementation.

:- import_module check_hlds__mode_util.
:- import_module hlds__special_pred.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

:- import_module int.
:- import_module string.
:- import_module list.
:- import_module term.
:- import_module require.

%-----------------------------------------------------------------------------%

	% The code of this predicate duplicates the functionality of
	% hlds_out__write_pred_id. Changes here should be made there as well.

describe_one_pred_name(Module, ShouldModuleQualify, PredId) = Pieces :-
	module_info_pred_info(Module, PredId, PredInfo),
	Pieces = describe_one_pred_info_name(ShouldModuleQualify, PredInfo).

describe_one_pred_info_name(ShouldModuleQualify, PredInfo) = Pieces :-
	PredName = pred_info_name(PredInfo),
	ModuleName = pred_info_module(PredInfo),
	Arity = pred_info_orig_arity(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	PredOrFuncStr = pred_or_func_to_string(PredOrFunc),
	adjust_func_arity(PredOrFunc, OrigArity, Arity),
	pred_info_get_markers(PredInfo, Markers),
	pred_info_get_origin(PredInfo, Origin),
	( Origin = special_pred(SpecialId - TypeCtor) ->
		special_pred_description(SpecialId, Descr),
		TypeCtor = TypeSymName - TypeArity,
		( TypeArity = 0 ->
			Pieces = [words(Descr), words("for type"),
				sym_name(TypeSymName)]
		;
			Pieces = [words(Descr), words("for type constructor"),
				sym_name(TypeSymName)]
		)
	; check_marker(Markers, class_instance_method) ->
		Pieces = [words("type class method implementation")]
	; pred_info_get_goal_type(PredInfo, promise(PromiseType)) ->
		Pieces = [words("`" ++ promise_to_string(PromiseType) ++ "'"),
			words("declaration")]
	;
		string__int_to_string(OrigArity, ArityPart),
		string__append_list([
			"`",
			module_qualification(ModuleName, ShouldModuleQualify),
			PredName,
			"/",
			ArityPart,
			"'"], SpecStr),
		Pieces = [words(PredOrFuncStr), fixed(SpecStr)]
	).

describe_one_pred_name_mode(Module, ShouldModuleQualify, PredId, InstVarSet,
		ArgModes0) = Pieces :-
	module_info_pred_info(Module, PredId, PredInfo),
	ModuleName = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo),
	Arity = pred_info_orig_arity(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	list__length(ArgModes0, NumArgModes),
	% We need to strip off the extra type_info arguments inserted at the
	% front by polymorphism.m - we only want the last `Arity' of them.
	( list__drop(NumArgModes - Arity, ArgModes0, ArgModes) ->
		strip_builtin_qualifiers_from_mode_list(ArgModes,
			StrippedArgModes)
	;
		error("describe_one_pred_name_mode: bad argument list")
	),
	(
		PredOrFunc = predicate,
		ArgModesPart =
			arg_modes_to_string(InstVarSet, StrippedArgModes)
	;
		PredOrFunc = function,
		pred_args_to_func_args(StrippedArgModes, FuncArgModes,
			FuncRetMode),
		ArgModesPart =
			arg_modes_to_string(InstVarSet, FuncArgModes) ++
			" = " ++
			mercury_mode_to_string(FuncRetMode, InstVarSet)
	),
	string__append_list([
		"`",
		module_qualification(ModuleName, ShouldModuleQualify),
		PredName,
		ArgModesPart,
		"'"], Descr),
	Pieces = [words(Descr)].

describe_several_pred_names(Module, ShouldModuleQualify, PredIds) = Pieces :-
	PiecesList = list__map(
		describe_one_pred_name(Module, ShouldModuleQualify), PredIds),
	Pieces = component_lists_to_pieces(PiecesList).

describe_one_proc_name(Module, ShouldModuleQualify, proc(PredId, ProcId))
		= Pieces :-
	PredPieces = describe_one_pred_name(Module, ShouldModuleQualify,
		PredId),
	proc_id_to_int(ProcId, ProcIdInt),
	string__int_to_string(ProcIdInt, ProcIdStr),
	Pieces = PredPieces ++ [words("mode"), words(ProcIdStr)].

describe_one_proc_name_mode(Module, ShouldModuleQualify, proc(PredId, ProcId))
		= Pieces :-
	module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo),
	proc_info_argmodes(ProcInfo, ArgModes),
	proc_info_inst_varset(ProcInfo, InstVarSet),
	Pieces = describe_one_pred_name_mode(Module, ShouldModuleQualify,
		PredId, InstVarSet, ArgModes).

describe_several_proc_names(Module, ShouldModuleQualify, PPIds) = Pieces :-
	PiecesList = list__map(
		describe_one_proc_name(Module, ShouldModuleQualify), PPIds),
	Pieces = component_lists_to_pieces(PiecesList).

describe_one_call_site(Module, ShouldModuleQualify, PPId - Context) = Pieces :-
	ProcNamePieces = describe_one_proc_name(Module, ShouldModuleQualify,
		PPId),
	term__context_file(Context, FileName),
	term__context_line(Context, LineNumber),
	string__int_to_string(LineNumber, LineNumberStr),
	Pieces = ProcNamePieces ++
		[words("at"), fixed(FileName ++ ":" ++ LineNumberStr)].

describe_several_call_sites(Module, ShouldModuleQualify, Sites) = Pieces :-
	PiecesList = list__map(
		describe_one_call_site(Module, ShouldModuleQualify), Sites),
	Pieces = component_lists_to_pieces(PiecesList).

:- func module_qualification(module_name, should_module_qualify) = string.

module_qualification(ModuleName, ShouldModuleQualify) = ModuleQualification :-
	(
		ShouldModuleQualify = should_module_qualify,
		mdbcomp__prim_data__sym_name_to_string(ModuleName, 
			ModuleNameString),
		ModuleQualification = string__append(ModuleNameString, ".")
	;
		ShouldModuleQualify = should_not_module_qualify,
		ModuleQualification = ""
	).

:- func arg_modes_to_string(inst_varset, list(mode)) = string.

arg_modes_to_string(InstVarSet, ArgModes) = Str :-
	(
		ArgModes = [],
		Str = ""
	;
		ArgModes = [_ | _],
		ArgsStr = mercury_mode_list_to_string(ArgModes, InstVarSet),
		Str = "(" ++ ArgsStr ++ ")"
	).
