%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2004 The University of Melbourne.
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

:- import_module assoc_list, list, std_util.

	% Predicates to convert a predicate and procedure names to strings.

:- type should_module_qualify
	--->	should_module_qualify
	;	should_not_module_qualify.

:- pred describe_one_pred_name(module_info::in, should_module_qualify::in,
	pred_id::in, string::out) is det.

:- pred describe_one_pred_name_mode(module_info::in, should_module_qualify::in,
	pred_id::in, inst_varset::in, list(mode)::in, string::out) is det.

:- pred describe_several_pred_names(module_info::in, should_module_qualify::in,
	list(pred_id)::in, list(format_component)::out) is det.

:- pred describe_one_proc_name(module_info::in, should_module_qualify::in,
	pred_proc_id::in, string::out) is det.

:- pred describe_one_proc_name_mode(module_info::in, should_module_qualify::in,
	pred_proc_id::in, string::out) is det.

:- pred describe_several_proc_names(module_info::in, should_module_qualify::in,
	list(pred_proc_id)::in, list(format_component)::out) is det.

:- pred describe_one_call_site(module_info::in, should_module_qualify::in,
	pair(pred_proc_id, prog_context)::in, string::out) is det.

:- pred describe_several_call_sites(module_info::in, should_module_qualify::in,
	assoc_list(pred_proc_id, prog_context)::in,
	list(format_component)::out) is det.

:- implementation.

:- import_module check_hlds__mode_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

:- import_module int, string, list, term, require.

%-----------------------------------------------------------------------------%

	% The code of this predicate duplicates the functionality of
	% hlds_out__write_pred_id. Changes here should be made there as well.

describe_one_pred_name(Module, ShouldModuleQualify, PredId, Piece) :-
	module_info_pred_info(Module, PredId, PredInfo),
	ModuleName = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo),
	Arity = pred_info_arity(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	PredOrFuncPart = pred_or_func_to_string(PredOrFunc),
	adjust_func_arity(PredOrFunc, OrigArity, Arity),
	(
		pred_info_get_goal_type(PredInfo, promise(PromiseType))
	->
		Piece = "`" ++ promise_to_string(PromiseType)
			++ "' declaration"
	;
		string__int_to_string(OrigArity, ArityPart),
		string__append_list([
			PredOrFuncPart,
			" `",
			module_qualification(ModuleName, ShouldModuleQualify),
			PredName,
			"/",
			ArityPart,
			"'"], Piece)
	).

describe_one_pred_name_mode(Module, ShouldModuleQualify, PredId, InstVarSet,
		ArgModes0, Piece) :-
	module_info_pred_info(Module, PredId, PredInfo),
	ModuleName = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo),
	Arity = pred_info_arity(PredInfo),
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
		"'"], Piece).

describe_several_pred_names(Module, ShouldModuleQualify, PredId, Pieces) :-
	list__map(describe_one_pred_name(Module, ShouldModuleQualify),
		PredId, Pieces0),
	list_to_pieces(Pieces0, Pieces).

describe_one_proc_name(Module, ShouldModuleQualify, proc(PredId, ProcId),
		Piece) :-
	describe_one_pred_name(Module, ShouldModuleQualify, PredId, PredPiece),
	proc_id_to_int(ProcId, ProcIdInt),
	string__int_to_string(ProcIdInt, ProcIdPart),
	string__append_list([
		PredPiece,
		" mode ",
		ProcIdPart
		], Piece).

describe_one_proc_name_mode(Module, ShouldModuleQualify, proc(PredId, ProcId),
		Piece) :-
	module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo),
	proc_info_argmodes(ProcInfo, ArgModes),
	proc_info_inst_varset(ProcInfo, InstVarSet),
	describe_one_pred_name_mode(Module, ShouldModuleQualify, PredId,
		InstVarSet, ArgModes, Piece).

describe_several_proc_names(Module, ShouldModuleQualify, PPIds, Pieces) :-
	list__map(describe_one_proc_name(Module, ShouldModuleQualify),
		PPIds, Pieces0),
	list_to_pieces(Pieces0, Pieces).

describe_one_call_site(Module, ShouldModuleQualify, PPId - Context, Piece) :-
	describe_one_proc_name(Module, ShouldModuleQualify, PPId, ProcName),
	term__context_file(Context, FileName),
	term__context_line(Context, LineNumber),
	string__int_to_string(LineNumber, LineNumberPart),
	string__append_list([
		ProcName,
		" at ",
		FileName,
		":",
		LineNumberPart
		], Piece).

describe_several_call_sites(Module, ShouldModuleQualify, Sites, Pieces) :-
	list__map(describe_one_call_site(Module, ShouldModuleQualify),
		Sites, Pieces0),
	list_to_pieces(Pieces0, Pieces).

:- func module_qualification(module_name, should_module_qualify) = string.

module_qualification(ModuleName, ShouldModuleQualify) = ModuleQualification :-
	(
		ShouldModuleQualify = should_module_qualify,
		prog_out__sym_name_to_string(ModuleName, ModuleNameString),
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
