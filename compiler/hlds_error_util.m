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

	% Predicates to convert a predicate names to strings.

:- pred describe_one_pred_name(module_info::in, pred_id::in,
	string::out) is det.

:- pred describe_several_pred_names(module_info::in,
	list(pred_id)::in, list(format_component)::out) is det.

:- pred describe_one_proc_name(module_info::in, pred_proc_id::in,
	string::out) is det.

:- pred describe_several_proc_names(module_info::in,
	list(pred_proc_id)::in, list(format_component)::out) is det.

:- pred describe_one_call_site(module_info::in,
	pair(pred_proc_id, prog_context)::in, string::out) is det.

:- pred describe_several_call_sites(module_info::in,
	assoc_list(pred_proc_id, prog_context)::in,
	list(format_component)::out) is det.

:- implementation.

:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

:- import_module string, list, term.

%-----------------------------------------------------------------------------%

	% The code of this predicate duplicates the functionality of
	% hlds_out__write_pred_id. Changes here should be made there as well.

describe_one_pred_name(Module, PredId, Piece) :-
	module_info_pred_info(Module, PredId, PredInfo),
	ModuleName = pred_info_module(PredInfo),
	prog_out__sym_name_to_string(ModuleName, ModuleNameString),
	PredName = pred_info_name(PredInfo),
	Arity = pred_info_arity(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	PredOrFuncPart = pred_or_func_to_string(PredOrFunc),
	adjust_func_arity(PredOrFunc, OrigArity, Arity),
	(
		pred_info_get_goal_type(PredInfo, promise(PromiseType))
	->
		Piece = "`" ++ promise_to_string(PromiseType) ++ "' declaration"
	;
		string__int_to_string(OrigArity, ArityPart),
		string__append_list([
			PredOrFuncPart,
			" `",
			ModuleNameString,
			".",
			PredName,
			"/",
			ArityPart,
			"'"], Piece)
	).

describe_several_pred_names(Module, PredId, Pieces) :-
	list__map(describe_one_pred_name(Module), PredId, Pieces0),
	list_to_pieces(Pieces0, Pieces).

describe_one_proc_name(Module, proc(PredId, ProcId), Piece) :-
	describe_one_pred_name(Module, PredId, PredPiece),
	proc_id_to_int(ProcId, ProcIdInt),
	string__int_to_string(ProcIdInt, ProcIdPart),
	string__append_list([
		PredPiece,
		" mode ",
		ProcIdPart
		], Piece).

describe_several_proc_names(Module, PPIds, Pieces) :-
	list__map(describe_one_proc_name(Module), PPIds, Pieces0),
	list_to_pieces(Pieces0, Pieces).

describe_one_call_site(Module, PPId - Context, Piece) :-
	describe_one_proc_name(Module, PPId, ProcName),
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

describe_several_call_sites(Module, Sites, Pieces) :-
	list__map(describe_one_call_site(Module), Sites, Pieces0),
	list_to_pieces(Pieces0, Pieces).
