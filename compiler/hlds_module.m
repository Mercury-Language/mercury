%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module defines the part of the High Level Data Structure or HLDS
% that deals with issues that are wider than a single predicate.

% The four main data structures defined here are the types
%
%	module_info
%	dependency_info
%	predicate_table
%
% There is a separate interface section for each of these.

% Main authors: fjh, conway.

:- module hlds_module.

:- interface.

:- import_module hlds_pred, unify_proc, special_pred.
:- import_module relation, globals, continuation_info.

:- implementation.

:- import_module hlds_data, hlds_out, llds, prog_data, prog_util.
:- import_module typecheck.
:- import_module bool, require, int, string, list, map, set, std_util.

%-----------------------------------------------------------------------------%

:- interface.

:- type module_info.

:- type c_code_info 	--->	c_code_info(
					c_header_info,
					c_body_info
				).

:- type pragma_exported_proc	
			--->	pragma_exported_proc(
					pred_id,
					proc_id,
					string	% the name of the C function
				).

	% This structure contains the information we need to generate
	% a base_type_info structure for a type defined in this module.

:- type base_gen_info	--->	base_gen_info(
					type_id,
					string,		% module name
					string,		% type name
					int,		% type arity
					import_status,	% of the type
					maybe(int),	% eliminated procs?
							% and how many if so
					list(pred_proc_id)
							% the ids of the procs
							% referred to from the
							% base_type_info
				).

	% This structure contains the information we need to generate
	% a base_type_layout structure for a type defined in this module.
	
:- type base_gen_layout	--->	base_gen_layout(
					type_id,
					string,		% module name
					string,		% type name
					int,		% type arity
					import_status,	% of the type
					hlds_type_defn % defn of type
				).

	% This structure contains information needed to create
	% base_type_* structures.

:- type base_gen_data ---> 	base_gen_data(
					list(base_gen_info),
					list(base_gen_layout)
				).

	% Various predicates for manipulating the module_info data structure
	% map from proc to a list of unused argument numbers.
:- type unused_arg_info == map(pred_proc_id, list(int)).

	% Create an empty module_info for a given module name (and the
	% global options).

:- pred module_info_init(string, globals, module_info).
:- mode module_info_init(in, in, out) is det.

:- pred module_info_name(module_info, string).
:- mode module_info_name(in, out) is det.

:- pred module_info_get_c_header(module_info, c_header_info).
:- mode module_info_get_c_header(in, out) is det.

:- pred module_info_set_c_header(module_info, c_header_info, module_info).
:- mode module_info_set_c_header(in, in, out) is det.

:- pred module_info_get_c_body_code(module_info, c_body_info).
:- mode module_info_get_c_body_code(in, out) is det.

:- pred module_info_set_c_body_code(module_info, c_body_info, module_info).
:- mode module_info_set_c_body_code(in, in, out) is det.

:- pred module_info_get_predicate_table(module_info, predicate_table).
:- mode module_info_get_predicate_table(in, out) is det.

:- pred module_info_preds(module_info, pred_table).
:- mode module_info_preds(in, out) is det.

:- pred module_info_pred_info(module_info, pred_id, pred_info).
:- mode module_info_pred_info(in, in, out) is det.

	% Given a pred_id and a proc_id, get the
	% pred_info that predicate and the proc_info for that
	% mode of that predicate.

:- pred module_info_pred_proc_info(module_info, pred_id, proc_id,
	pred_info, proc_info).
:- mode module_info_pred_proc_info(in, in, in, out, out) is det.

	% Return a list of the pred_ids of all the "valid" predicates.
	% (Predicates whose definition contains a type error, etc.
	% get removed from this list, so that later passes can rely
	% on the predicates in this list being type-correct, etc.)
:- pred module_info_predids(module_info, list(pred_id)).
:- mode module_info_predids(in, out) is det.

	% Reverse the list of pred_ids.
	% (The list is built up by inserting values at the front,
	% for efficiency; once we've done so, we reverse the list
	% so that progress messages and error messages come out
	% in the expected order.)
:- pred module_info_reverse_predids(module_info, module_info).
:- mode module_info_reverse_predids(in, out) is det.

	% For an explanation of the unify_requests structure,
	% see unify_info.m.
:- pred module_info_get_unify_requests(module_info, unify_requests).
:- mode module_info_get_unify_requests(in, out) is det.

:- pred module_info_get_special_pred_map(module_info, special_pred_map).
:- mode module_info_get_special_pred_map(in, out) is det.

:- pred module_info_get_continuation_info(module_info, continuation_info).
:- mode module_info_get_continuation_info(in, out) is det.

	% the cell count is used as a unique label number for
	% constants in the generated C code
:- pred module_info_get_cell_count(module_info, int).
:- mode module_info_get_cell_count(in, out) is det.

:- pred module_info_types(module_info, type_table).
:- mode module_info_types(in, out) is det.

:- pred module_info_typeids(module_info, list(type_id)).
:- mode module_info_typeids(in, out) is det.

:- pred module_info_insts(module_info, inst_table).
:- mode module_info_insts(in, out) is det.

:- pred module_info_instids(module_info, list(inst_id)).
:- mode module_info_instids(in, out) is det.

:- pred module_info_modes(module_info, mode_table).
:- mode module_info_modes(in, out) is det.

:- pred module_info_modeids(module_info, list(mode_id)).
:- mode module_info_modeids(in, out) is det.

:- pred module_info_ctors(module_info, cons_table).
:- mode module_info_ctors(in, out) is det.

:- pred module_info_num_errors(module_info, int).
:- mode module_info_num_errors(in, out) is det.

% not used
% :- pred module_info_num_warnings(module_info, int).
% :- mode module_info_num_warnings(in, out) is det.

:- pred module_info_consids(module_info, list(cons_id)).
:- mode module_info_consids(in, out) is det.

	% The dependency information must have been build before
	% calling this predicate.

:- pred module_info_dependency_info(module_info, dependency_info).
:- mode module_info_dependency_info(in, out) is det.

	% Succeeds iff the dependency information has already been built

:- pred module_info_dependency_info_built(module_info).
:- mode module_info_dependency_info_built(in) is semidet.

:- pred module_info_unused_arg_info(module_info, unused_arg_info).
:- mode module_info_unused_arg_info(in, out) is det.

:- pred module_info_set_name(module_info, string, module_info).
:- mode module_info_set_name(in, in, out) is det.

:- pred module_info_set_predicate_table(module_info, predicate_table,
					module_info).
:- mode module_info_set_predicate_table(in, in, out) is det.

:- pred module_info_set_preds(module_info, pred_table, module_info).
:- mode module_info_set_preds(in, in, out) is det.

:- pred module_info_set_pred_info(module_info, pred_id, pred_info, module_info).
:- mode module_info_set_pred_info(in, in, in, out) is det.

:- pred module_info_set_unify_requests(module_info, unify_requests,
					module_info).
:- mode module_info_set_unify_requests(in, in, out) is det.

:- pred module_info_set_special_pred_map(module_info, special_pred_map,
					module_info).
:- mode module_info_set_special_pred_map(in, in, out) is det.

:- pred module_info_set_continuation_info(module_info, continuation_info, 
		module_info).
:- mode module_info_set_continuation_info(in, in, out) is det.

:- pred module_info_set_cell_count(module_info, int, module_info).
:- mode module_info_set_cell_count(in, in, out) is det.

:- pred module_info_set_types(module_info, type_table, module_info).
:- mode module_info_set_types(in, in, out) is det.

:- pred module_info_set_insts(module_info, inst_table, module_info).
:- mode module_info_set_insts(in, in, out) is det.

:- pred module_info_set_modes(module_info, mode_table, module_info).
:- mode module_info_set_modes(in, in, out) is det.

:- pred module_info_set_ctors(module_info, cons_table, module_info).
:- mode module_info_set_ctors(in, in, out) is det.

:- pred module_info_set_dependency_info(module_info, dependency_info,
	module_info).
:- mode module_info_set_dependency_info(in, in, out) is det.

:- pred module_info_clobber_dependency_info(module_info, module_info).
:- mode module_info_clobber_dependency_info(in, out) is det.

:- pred module_info_set_unused_arg_info(module_info,
		unused_arg_info, module_info).
:- mode module_info_set_unused_arg_info(in, in, out) is det.

:- pred module_info_set_num_errors(module_info, int, module_info).
:- mode module_info_set_num_errors(in, in, out) is det.

:- pred module_info_incr_errors(module_info, module_info).
:- mode module_info_incr_errors(in, out) is det.

/* not used
:- pred module_info_incr_warnings(module_info, module_info).
:- mode module_info_incr_warnings(in, out) is det.
*/

	% The module_info stores a counter which is used to number
	% introduced lambda predicates as __LambdaGoal__1, __LambdaGoal__2,
	% etc.; this predicate returns the next number and increments
	% the counter.

:- pred module_info_next_lambda_count(module_info, int, module_info).
:- mode module_info_next_lambda_count(in, out, out) is det.

:- pred module_info_get_pragma_exported_procs(module_info,
	list(pragma_exported_proc)).
:- mode module_info_get_pragma_exported_procs(in, out) is det.

:- pred module_info_set_pragma_exported_procs(module_info,
	list(pragma_exported_proc), module_info).
:- mode module_info_set_pragma_exported_procs(in, in, out) is det.

	% Remove a predicate from the list of pred_ids, to prevent
	% further processing of this predicate after an error is
	% encountered.

:- pred module_info_remove_predid(module_info, pred_id, module_info).
:- mode module_info_remove_predid(in, in, out) is det.

	% Once the module_info has been built, we call module_info_optimize
	% to attempt to optimize the data structures for lots of accesses
	% and relatively few insertion/deletions. (This was useful when
	% we were using unbalanced binary trees, but now that we are using
	% 234-trees, it is a no-op, except for the mode and inst tables,
	% where the cached lists of mode_ids and inst_ids are sorted for
	% efficient conversion to sets in module_qual.m.)

:- pred module_info_optimize(module_info, module_info).
:- mode module_info_optimize(in, out) is det.

:- pred module_info_base_gen_infos(module_info, list(base_gen_info)).
:- mode module_info_base_gen_infos(in, out) is det.

:- pred module_info_set_base_gen_infos(module_info, list(base_gen_info),
	module_info).
:- mode module_info_set_base_gen_infos(in, in, out) is det.

:- pred module_info_base_gen_layouts(module_info, list(base_gen_layout)).
:- mode module_info_base_gen_layouts(in, out) is det.

:- pred module_info_set_base_gen_layouts(module_info, list(base_gen_layout),
	module_info).
:- mode module_info_set_base_gen_layouts(in, in, out) is det.

:- pred module_info_globals(module_info, globals).
:- mode module_info_globals(in, out) is det.

:- pred module_info_stratified_preds(module_info, set(pred_id)).
:- mode module_info_stratified_preds(in, out) is det.

:- pred module_info_set_stratified_preds(module_info, set(pred_id),
	module_info).
:- mode module_info_set_stratified_preds(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type module_info
	--->	module(
			string,		% module name
			c_code_info,	
			predicate_table,
			unify_requests,
			special_pred_map,
			continuation_info,
			type_table,
			inst_table,
			mode_table,
			cons_table,
			maybe(dependency_info),
			int,		% number of errors
			%%% num_warnings not used:
			%%% int,	% number of warnings
			int,	% lambda predicate counter
			list(pragma_exported_proc),
				% list of the procs for which
				% there is a pragma(export, ...)
				% declaration
			base_gen_data,
				% info about the the types 
				% defined here
			globals, % global options
			set(pred_id),
				% list of preds which 
				% must be stratified
			unused_arg_info,
				% unused argument info about
				% predicates in the current
				% module which has been exported
				% in .opt files.
			int	% cell count, passed into code_info
				% and used to generate unique label
				% numbers for constant terms in the
				% generated C code
		).

	% A predicate which creates an empty module

module_info_init(Name, Globals, Module_Info) :-
	C_Code_Info = c_code_info([], []),
	predicate_table_init(PredicateTable),
	unify_proc__init_requests(Requests),
	map__init(UnifyPredMap),
	map__init(Types),
	inst_table_init(Insts),
	mode_table_init(Modes),
	continuation_info__init(ContinuationInfo),
	map__init(Ctors),
	DepInfo = no,
	PragmaExports = [],
	BaseTypeData = base_gen_data([], []),
	set__init(StratPreds),
	map__init(UnusedArgInfo),
	Module_Info = module(Name, C_Code_Info, PredicateTable, Requests, 
		UnifyPredMap, ContinuationInfo, Types, Insts, Modes, 
		Ctors, DepInfo, 0, 0, PragmaExports, BaseTypeData, Globals,
		StratPreds, UnusedArgInfo, 0).

	% Various access predicates which extract different pieces
	% of info from the module_info data structure.

module_info_name(ModuleInfo, Name) :-
	ModuleInfo = module(Name, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _, _, _).

module_info_get_c_header(ModuleInfo, C_Header) :-
	ModuleInfo = module(_, C_Code_Info, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _, _, _, _),
	C_Code_Info = c_code_info(C_Header, _).

module_info_set_c_header(ModuleInfo1, C_Header, ModuleInfo2) :-
	ModuleInfo1 = module(A, C_Code_Info0, 
		C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	C_Code_Info0 = c_code_info(_C_Header0, C_Body),
	C_Code_Info = c_code_info(C_Header, C_Body),
	ModuleInfo2 = module(A, C_Code_Info, 
		C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

module_info_get_c_body_code(ModuleInfo, C_Body) :-
	ModuleInfo = module(_, C_Code_Info, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _, _, _, _),
	C_Code_Info = c_code_info(_, C_Body).

module_info_set_c_body_code(ModuleInfo1, C_Body, ModuleInfo2) :-
	ModuleInfo1 = module(A, C_Code_Info0, 
		C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	C_Code_Info0 = c_code_info(C_Header, _C_Body0),
	C_Code_Info = c_code_info(C_Header, C_Body),
	ModuleInfo2 = module(A, C_Code_Info, 
		C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

module_info_get_predicate_table(ModuleInfo, PredicateTable) :-
	ModuleInfo = module(_, _, PredicateTable, 
		_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

module_info_preds(ModuleInfo, Preds) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	predicate_table_get_preds(PredicateTable, Preds).

module_info_pred_info(ModuleInfo, PredId, PredInfo) :-
	module_info_preds(ModuleInfo, Preds),
	( map__search(Preds, PredId, PredInfoPrime) ->
		PredInfo = PredInfoPrime
	;
		pred_id_to_int(PredId, PredInt),
		string__int_to_string(PredInt, PredStr),
		string__append("cannot find predicate number ", PredStr, Msg),
		error(Msg)
	).

module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo).

module_info_predids(ModuleInfo, PredIds) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	predicate_table_get_predids(PredicateTable, PredIds).

module_info_reverse_predids(ModuleInfo0, ModuleInfo) :-
	module_info_get_predicate_table(ModuleInfo0, PredicateTable0),
	predicate_table_reverse_predids(PredicateTable0, PredicateTable),
	module_info_set_predicate_table(ModuleInfo0, PredicateTable,
		ModuleInfo).

module_info_get_unify_requests(ModuleInfo, Requests) :-
	ModuleInfo = module(_, _, _, Requests, _, _, _, _, _, _, _, _,
		_, _, _, _, _, _, _).

module_info_get_special_pred_map(ModuleInfo, SpecialPredMap) :-
	ModuleInfo = module(_, _, _, _, SpecialPredMap, 
		_, _, _, _, _, _, _, _, _, _, _, _, _, _).

module_info_types(ModuleInfo, Types) :-
	ModuleInfo = module(_, _, _, _, _, _, Types, _, _, _, _, _, _, 
		_, _, _, _, _, _).

module_info_typeids(ModuleInfo, TypeIDs) :-
	ModuleInfo = module(_, _, _, _, _, _, Types, _, _, _, _, _, _, 
		_, _, _, _, _, _),
	map__keys(Types, TypeIDs).

module_info_insts(ModuleInfo, Insts) :-
	ModuleInfo = module(_, _, _, _, _, _, _, Insts, _, _, _, _, _, 
		_, _, _, _, _, _).

module_info_instids(ModuleInfo, InstIDs) :-
	module_info_insts(ModuleInfo, InstTable),
	inst_table_get_user_insts(InstTable, UserInstTable),
	user_inst_table_get_inst_ids(UserInstTable, InstIDs).

module_info_modes(ModuleInfo, Modes) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, Modes, _, _, _, _, 
		_, _, _, _, _, _).

module_info_modeids(ModuleInfo, ModeIDs) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, Modes, _, _, _, _, 
		_, _, _, _, _, _),
	mode_table_get_mode_ids(Modes, ModeIDs).

module_info_ctors(ModuleInfo, Ctors) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, Ctors, _, _, _, 
		_, _, _, _, _, _).

module_info_consids(ModuleInfo, ConsIDs) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, Ctors, _, _, _, 
		_, _, _, _, _, _),
	map__keys(Ctors, ConsIDs).

module_info_dependency_info(ModuleInfo, DepInfo) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, DepInfo0, _, _,
		_, _, _, _, _, _),
	( DepInfo0 = yes(DepInfo1) ->
		DepInfo = DepInfo1
	;
		error("Attempted to access uninitialised dependency_info")
	).

module_info_unused_arg_info(ModuleInfo, UnusedArgInfo) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _, UnusedArgInfo, _).

module_info_dependency_info_built(ModuleInfo) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, yes(_), _, _,
		_, _, _, _, _, _).

module_info_num_errors(ModuleInfo, NumErrors) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, NumErrors,
		_, _, _, _, _, _, _).

module_info_base_gen_infos(ModuleInfo, BaseGenInfos) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, _, _, _,
		base_gen_data(BaseGenInfos, _), _, _, _, _).

module_info_base_gen_layouts(ModuleInfo, BaseGenLayouts) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, _, _, _,
		base_gen_data(_, BaseGenLayouts), _, _, _, _).

module_info_globals(ModuleInfo, Globals) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		Globals, _, _, _).
		
module_info_stratified_preds(ModuleInfo, StratPreds) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, StratPreds, _, _).

module_info_get_cell_count(ModuleInfo, CellCount) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, CellCount).

% not used:
% module_info_num_warnings(ModuleInfo, NumWarnings) :-
% 	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, _, NumWarnings).

	% Various predicates which modify the module_info data structure.

module_info_set_name(ModuleInfo0, Name, ModuleInfo) :-
	ModuleInfo0 = module(_, B, C, D, E, F, G, H, I, J, K, L, M, N, 
		O, P, Q, R, S),
	ModuleInfo = module(Name, B, C, D, E, F, G, H, I, J, K, L, M, N, 
		O, P, Q, R, S).

module_info_set_predicate_table(ModuleInfo0, PredicateTable, ModuleInfo) :-
	ModuleInfo0 = module(A, B, _, D, E, F, G, H, I, J, K, L, M, N, 
		O, P, Q, R, S),
	ModuleInfo = module(A, B, PredicateTable, 
		D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

module_info_set_preds(ModuleInfo0, Preds, ModuleInfo) :-
	module_info_get_predicate_table(ModuleInfo0, PredicateTable0),
	predicate_table_set_preds(PredicateTable0, Preds, PredicateTable),
	module_info_set_predicate_table(ModuleInfo0, PredicateTable,
		ModuleInfo).

module_info_set_pred_info(ModuleInfo0, PredID, PredInfo, ModuleInfo) :-
	module_info_preds(ModuleInfo0, Preds0),
	map__set(Preds0, PredID, PredInfo, Preds),
	module_info_set_preds(ModuleInfo0, Preds, ModuleInfo).

module_info_set_unify_requests(ModuleInfo0, Requests, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, _, E, F, G, H, I, J, K, L, M, N, 
		O, P, Q, R, S), 
	ModuleInfo = module(A, B, C, Requests, E, F, G, H, I, J, K, L, 
		M, N, O, P, Q, R, S).

module_info_set_special_pred_map(ModuleInfo0, SpecialPredMap, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, _, F, G, H, I, J, K, L, M, 
		N, O, P, Q, R, S),
	ModuleInfo = module(A, B, C, D, SpecialPredMap, 
		F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

module_info_set_continuation_info(ModuleInfo0, ContinuationInfo, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, _, G, H, I, J, K, L, M, N, 
		O, P, Q, R, S),
	ModuleInfo = module(A, B, C, D, E, ContinuationInfo, G, H, I, J, K, L, 
		M, N, O, P, Q, R, S).

module_info_set_types(ModuleInfo0, Types, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, _, H, I, J, K, L, M, N, 
		O, P, Q, R, S),
	ModuleInfo = module(A, B, C, D, E, F, Types, H, I, J, K, L, M, 
		N, O, P, Q, R, S).

module_info_set_insts(ModuleInfo0, Insts, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, _, I, J, K, L, M, N, 
		O, P, Q, R, S),
	ModuleInfo = module(A, B, C, D, E, F, G, Insts, I, J, K, L, M, 
		N, O, P, Q, R, S).

module_info_set_modes(ModuleInfo0, Modes, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, _, J, K, L, M, N, 
		O, P, Q, R, S),
	ModuleInfo = module(A, B, C, D, E, F, G, H, Modes, J, K, L, M, 
		N, O, P, Q, R, S).

module_info_set_ctors(ModuleInfo0, Ctors, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, _, K, L, M, N, 
		O, P, Q, R, S),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, Ctors, K, L, M, 
		N, O, P, Q, R, S).

module_info_set_dependency_info(ModuleInfo0, DepInfo, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, _, L, M, N, 
		O, P, Q, R, S),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, yes(DepInfo), 
		L, M, N, O, P, Q, R, S).

module_info_clobber_dependency_info(ModuleInfo0, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, _,
		L, M, N, O, P, Q, R, S),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, no, 
		L, M, N, O, P, Q, R, S).

module_info_set_num_errors(ModuleInfo0, Errs, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, _, M, N, 
		O, P, Q, R, S),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, Errs, M, N, 
		O, P, Q, R, S).

module_info_incr_errors(ModuleInfo0, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, Errs0, M, 
		N, O, P, Q, R, S),
	Errs is Errs0 + 1,
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, Errs, M, N, 
		O, P, Q, R, S).

/* not used
module_info_incr_warnings(ModuleInfo0, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, L, Warns0),
	Warns is Warns0 + 1,
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, L, Warns).
*/
module_info_next_lambda_count(ModuleInfo0, Count, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, L, Count0, N, O,
		P, Q, R, S),
	Count is Count0 + 1,
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, L, Count, 
		N, O, P, Q, R, S).

module_info_get_continuation_info(ModuleInfo, ContinuationInfo) :-
	ModuleInfo = module(_, _, _, _, _, ContinuationInfo, _, _, _, _, _, _, 
		_, _, _, _, _, _, _).

module_info_get_pragma_exported_procs(ModuleInfo, Procs) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, _, _, _, _, 
		Procs, _, _, _, _, _).

module_info_set_pragma_exported_procs(ModuleInfo0, Procs, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, L, M, _, 
		O, P, Q, R, S),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, L, M, Procs, 
		O, P, Q, R, S).

module_info_set_base_gen_infos(ModuleInfo0, BaseGenInfos, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N, 
		base_gen_data(_, BaseGenLayouts), P, Q, R, S),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N,
		base_gen_data(BaseGenInfos, BaseGenLayouts), P, Q, R, S).

module_info_set_base_gen_layouts(ModuleInfo0, BaseGenLayouts, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N, 
		base_gen_data(BaseGenInfos, _), P, Q, R, S),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N,
		base_gen_data(BaseGenInfos, BaseGenLayouts), P, Q, R, S).

module_info_set_stratified_preds(ModuleInfo0, StratPreds, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, _, R, S),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N,
		O, P, StratPreds, R, S).

module_info_set_unused_arg_info(ModuleInfo0, UnusedArgInfo, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K,
		L, M, N, O, P, Q, _, S),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K,
		L, M, N, O, P, Q, UnusedArgInfo, S).

module_info_set_cell_count(ModuleInfo0, CellCount, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, _),
	ModuleInfo = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N,
		O, P, Q, R, CellCount).

module_info_remove_predid(ModuleInfo0, PredId, ModuleInfo) :-
	module_info_get_predicate_table(ModuleInfo0, PredicateTable0),
	predicate_table_remove_predid(PredicateTable0, PredId,
				PredicateTable),
	module_info_set_predicate_table(ModuleInfo0, PredicateTable,
				ModuleInfo).

	% After we have finished constructing the symbol tables,
	% we balance all the binary trees, to improve performance
	% in later stages of the compiler.

module_info_optimize(ModuleInfo0, ModuleInfo) :-

	module_info_get_predicate_table(ModuleInfo0, Preds0),
	predicate_table_optimize(Preds0, Preds),
	module_info_set_predicate_table(ModuleInfo0, Preds, ModuleInfo3),

	% XXX Might want to optimize continuation_info here.

	module_info_types(ModuleInfo3, Types0),
	map__optimize(Types0, Types),
	module_info_set_types(ModuleInfo3, Types, ModuleInfo4),

	module_info_insts(ModuleInfo4, InstTable0),
	inst_table_get_user_insts(InstTable0, Insts0),
	user_inst_table_optimize(Insts0, Insts),
	inst_table_set_user_insts(InstTable0, Insts, InstTable),
	module_info_set_insts(ModuleInfo4, InstTable, ModuleInfo5),

	module_info_modes(ModuleInfo5, Modes0),
	mode_table_optimize(Modes0, Modes),
	module_info_set_modes(ModuleInfo4, Modes, ModuleInfo6),

	module_info_ctors(ModuleInfo6, Ctors0),
	map__optimize(Ctors0, Ctors),
	module_info_set_ctors(ModuleInfo6, Ctors, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- type dependency_ordering	== list(list(pred_proc_id)).
:- type dependency_graph	== relation(pred_proc_id).
:- type dependency_info.

:- pred hlds_dependency_info_init(dependency_info).
:- mode hlds_dependency_info_init(out) is det.

:- pred hlds_dependency_info_get_dependency_graph(dependency_info, 
	dependency_graph).
:- mode hlds_dependency_info_get_dependency_graph(in, out) is det.

:- pred hlds_dependency_info_get_dependency_ordering(dependency_info, 
	dependency_ordering).
:- mode hlds_dependency_info_get_dependency_ordering(in, out) is det.

:- pred hlds_dependency_info_set_dependency_graph(dependency_info,
	dependency_graph, dependency_info).
:- mode hlds_dependency_info_set_dependency_graph(in, in, out) is det.

:- pred hlds_dependency_info_set_dependency_ordering(dependency_info,
	dependency_ordering, dependency_info).
:- mode hlds_dependency_info_set_dependency_ordering(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type dependency_info --->
		dependency_info(
			dependency_graph,	% Dependency graph
			dependency_ordering,	% Dependency ordering
			set(pred_proc_id),	% Unused procs
			unit,			% Junk slots
			unit,
			unit
		).

hlds_dependency_info_init(DepInfo) :-
	DepInfo = dependency_info(DepRel, DepOrd, Unused, unit, unit, unit),
	relation__init(DepRel),
	DepOrd = [],
	set__init(Unused).

hlds_dependency_info_get_dependency_graph(DepInfo, DepRel) :-
	DepInfo = dependency_info(DepRel, _, _, _, _, _).

hlds_dependency_info_get_dependency_ordering(DepInfo, DepOrd) :-
	DepInfo = dependency_info(_, DepOrd, _, _, _, _).

hlds_dependency_info_set_dependency_graph(DepInfo0, DepRel, DepInfo) :-
	DepInfo0 = dependency_info(_, B, C, D, E, F),
	DepInfo = dependency_info(DepRel, B, C, D, E, F).

hlds_dependency_info_set_dependency_ordering(DepInfo0, DepRel, DepInfo) :-
	DepInfo0 = dependency_info(A, _, C, D, E, F),
	DepInfo = dependency_info(A, DepRel, C, D, E, F).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- type predicate_table.

:- type pred_table	==	map(pred_id, pred_info).

	% Various predicates for accessing the predicate_table type.
	% The predicate_table holds information about the predicates
	% and functions defined in this module or imported from other modules.
	% The primary key for this table is the `pred_id', but there
	% are also secondary indexes on each of name, name+arity, and
	% module+name+arity, for both functions and predicates.

	% Initialize the predicate table

:- pred predicate_table_init(predicate_table).
:- mode predicate_table_init(out) is det.

	% Balance all the binary trees in the predicate table

:- pred predicate_table_optimize(predicate_table, predicate_table).
:- mode predicate_table_optimize(in, out) is det.

	% Get the pred_id->pred_info map.

:- pred predicate_table_get_preds(predicate_table, pred_table).
:- mode predicate_table_get_preds(in, out) is det.

	% Set the pred_id->pred_info map.
	% NB You shouldn't modify the keys in this table, only
	% use predicate_table_insert and predicate_table_remove_predid.

:- pred predicate_table_set_preds(predicate_table, pred_table, predicate_table).
:- mode predicate_table_set_preds(in, in, out) is det.

	% Get a list of all the valid predids in the predicate_table.

:- pred predicate_table_get_predids(predicate_table, list(pred_id)).
:- mode predicate_table_get_predids(in, out) is det.

	% Remove a pred_id from the valid list.

:- pred predicate_table_remove_predid(predicate_table, pred_id,
					predicate_table).
:- mode predicate_table_remove_predid(in, in, out) is det.

	% Search the table for (a) predicates or functions
	% (b) predicates only or (c) functions only
	% matching this (possibly module-qualified) sym_name.

:- pred predicate_table_search_sym(predicate_table, sym_name, list(pred_id)).
:- mode predicate_table_search_sym(in, in, out) is semidet.

:- pred predicate_table_search_pred_sym(predicate_table, sym_name,
					list(pred_id)).
:- mode predicate_table_search_pred_sym(in, in, out) is semidet.

:- pred predicate_table_search_func_sym(predicate_table, sym_name,
					list(pred_id)).
:- mode predicate_table_search_func_sym(in, in, out) is semidet.

	% Search the table for (a) predicates or functions
	% (b) predicates only or (c) functions only matching this
	% (possibly module-qualified) sym_name & arity.

:- pred predicate_table_search_sym_arity(predicate_table, sym_name, arity,
					list(pred_id)).
:- mode predicate_table_search_sym_arity(in, in, in, out) is semidet.

:- pred predicate_table_search_pred_sym_arity(predicate_table, sym_name, arity,
					list(pred_id)).
:- mode predicate_table_search_pred_sym_arity(in, in, in, out) is semidet.

:- pred predicate_table_search_func_sym_arity(predicate_table, sym_name, arity,
					list(pred_id)).
:- mode predicate_table_search_func_sym_arity(in, in, in, out) is semidet.

	% Search the table for (a) predicates or functions
	% (b) predicates only or (c) functions only
	% matching this name.

:- pred predicate_table_search_name(predicate_table, string, list(pred_id)).
:- mode predicate_table_search_name(in, in, out) is semidet.

:- pred predicate_table_search_pred_name(predicate_table, string,
					list(pred_id)).
:- mode predicate_table_search_pred_name(in, in, out) is semidet.

:- pred predicate_table_search_func_name(predicate_table, string,
					list(pred_id)).
:- mode predicate_table_search_func_name(in, in, out) is semidet.

	% Search the table for (a) predicates or functions
	% (b) predicates only or (c) functions only
	% matching this name & arity.
	% When searching for functions, the arity used
	% is the arity of the function, not the arity N+1 predicate
	% that it gets converted to.

:- pred predicate_table_search_name_arity(predicate_table, string, arity,
						list(pred_id)).
:- mode predicate_table_search_name_arity(in, in, in, out) is semidet.

:- pred predicate_table_search_pred_name_arity(predicate_table, string, arity,
						list(pred_id)).
:- mode predicate_table_search_pred_name_arity(in, in, in, out) is semidet.

:- pred predicate_table_search_func_name_arity(predicate_table, string, arity,
						list(pred_id)).
:- mode predicate_table_search_func_name_arity(in, in, in, out) is semidet.

	% Search the table for (a) predicates or functions
	% (b) predicates only or (c) functions only
	% matching this module, name & arity.
	% When searching for functions, the arity used
	% is the arity of the function, not the arity N+1 predicate
	% that it gets converted to.
	% Note that in cases (b) and (c) there should be at most
	% one matching pred_id, since under the current overloading
	% rules each predicate or function can be uniquely identified
	% by its module, name, arity, and category (function/predicate).
	% (`m_n_a' here is short for "module, name, arity".)

:- pred predicate_table_search_m_n_a(predicate_table, module_name, string,
						arity, list(pred_id)).
:- mode predicate_table_search_m_n_a(in, in, in, in, out) is semidet.

:- pred predicate_table_search_pred_m_n_a(predicate_table, module_name, string,
						arity, list(pred_id)).
:- mode predicate_table_search_pred_m_n_a(in, in, in, in, out) is semidet.

:- pred predicate_table_search_func_m_n_a(predicate_table, module_name, string,
						arity, list(pred_id)).
:- mode predicate_table_search_func_m_n_a(in, in, in, in, out) is semidet.

	% Search the table for predicates or functions matching
	% this pred_or_func category, module, name, and arity.
	% When searching for functions, the arity used
	% is the arity of the predicate that the function gets converted
	% to, i.e. the arity of the function plus one.
	% NB.  This is opposite to what happens with the search
	% predicates declared above!!

:- pred predicate_table_search_pf_m_n_a(predicate_table, pred_or_func,
						module_name, string,
						arity, list(pred_id)).
:- mode predicate_table_search_pf_m_n_a(in, in, in, in, in, out) is semidet.

	% Search the table for predicates or functions matching
	% this pred_or_func category, name, and arity.
	% When searching for functions, the arity used
	% is the arity of the predicate that the function gets converted
	% to, i.e. the arity of the function plus one.
	% NB.  This is opposite to what happens with the search
	% predicates declared above!!

:- pred predicate_table_search_pf_name_arity(predicate_table, pred_or_func,
					string, arity, list(pred_id)).
:- mode predicate_table_search_pf_name_arity(in, in, in, in, out) is semidet.

	% Search the table for predicates or functions matching
	% this pred_or_func category, sym_name, and arity.
	% When searching for functions, the arity used
	% is the arity of the predicate that the function gets converted
	% to, i.e. the arity of the function plus one.
	% NB.  This is opposite to what happens with the search
	% predicates declared above!!

:- pred predicate_table_search_pf_sym_arity(predicate_table, pred_or_func,
				sym_name, arity, list(pred_id)) is semidet.
:- mode predicate_table_search_pf_sym_arity(in, in, in, in, out) is semidet.

	% predicate_table_insert(PredTable0, PredInfo, NeedQual, PredId,
	% 		PredTable).
	% 
	% Insert PredInfo into PredTable0 and assign it a new pred_id.
	% You should check beforehand that the pred doesn't already 
	% occur in the table. 
:- pred predicate_table_insert(predicate_table, pred_info, need_qualifier, 
				pred_id, predicate_table).
:- mode predicate_table_insert(in, in, in, out, out) is det.

	% Equivalent to predicate_table_insert(PredTable0, PredInfo, 
	%	yes, PredId, PredTable). 
:- pred predicate_table_insert(predicate_table, pred_info, pred_id,
				predicate_table).
:- mode predicate_table_insert(in, in, out, out) is det.

:- pred predicate_id(module_info, pred_id, module_name, string, arity).
:- mode predicate_id(in, in, out, out, out) is det.

:- pred predicate_module(module_info, pred_id, module_name).
:- mode predicate_module(in, in, out) is det.

:- pred predicate_name(module_info, pred_id, string).
:- mode predicate_name(in, in, out) is det.

:- pred predicate_arity(module_info, pred_id, arity).
:- mode predicate_arity(in, in, out) is det.

	% Get the pred_id and proc_id matching a higher-order term with
	% the given argument types, aborting with an error if none is
	% found.
:- pred get_pred_id_and_proc_id(sym_name, pred_or_func, tvarset, list(type),
				module_info, pred_id, proc_id).
:- mode get_pred_id_and_proc_id(in, in, in, in, in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type predicate_table --->
	predicate_table(
		pred_table,		% map from pred_id to pred_info
		pred_id,		% next available pred_id
		list(pred_id),		% the keys of the pred_table -
					% cached here for efficiency
		% indexes on predicates
		name_index,		% map from pred name to pred_id
		name_arity_index,	% map from pred name & arity to pred_id
		module_name_arity_index,
					% map from pred module, name & arity
					% to pred_id
		% indexes on functions
		name_index,		% map from func name to pred_id
		name_arity_index,	% map from func name & arity to pred_id
		module_name_arity_index
					% map from func module, name & arity
					% to pred_id
	).

:- type name_index	== map(string, list(pred_id)).

:- type name_arity_index == map(name_arity, list(pred_id)).
:- type name_arity ---> string / arity.

	% First search on module and name, then search on arity. The two 
	% levels are needed because typecheck.m needs to be able to search
	% on module and name only for higher-order terms.
:- type module_name_arity_index == map(pair(module_name, string), 
					map(arity, list(pred_id))).

predicate_table_init(PredicateTable) :-
	PredicateTable = predicate_table(Preds, NextPredId, PredIds,
				Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
				Func_N_Index, Func_NA_Index, Func_MNA_Index),
	map__init(Preds),
	hlds_pred__initial_pred_id(NextPredId),
	PredIds = [],
	map__init(Pred_N_Index),
	map__init(Pred_NA_Index),
	map__init(Pred_MNA_Index),
	map__init(Func_N_Index),
	map__init(Func_NA_Index),
	map__init(Func_MNA_Index).

predicate_table_optimize(PredicateTable0, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, C,
				Pred_N_Index0, Pred_NA_Index0, Pred_MNA_Index0,
				Func_N_Index0, Func_NA_Index0, Func_MNA_Index0),
	map__optimize(Pred_N_Index0, Pred_N_Index),
	map__optimize(Pred_NA_Index0, Pred_NA_Index),
	map__optimize(Pred_MNA_Index0, Pred_MNA_Index),
	map__optimize(Func_N_Index0, Func_N_Index),
	map__optimize(Func_NA_Index0, Func_NA_Index),
	map__optimize(Func_MNA_Index0, Func_MNA_Index),
	PredicateTable = predicate_table(A, B, C,
				Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
				Func_N_Index, Func_NA_Index, Func_MNA_Index).

predicate_table_get_preds(PredicateTable, Preds) :-
	PredicateTable = predicate_table(Preds, _, _, _, _, _, _, _, _).

predicate_table_set_preds(PredicateTable0, Preds, PredicateTable) :-
	PredicateTable0 = predicate_table(_, B, C, D, E, F, G, H, I),
	PredicateTable = predicate_table(Preds, B, C, D, E, F, G, H, I).

predicate_table_get_predids(PredicateTable, PredIds) :-
	PredicateTable = predicate_table(_, _, PredIds, _, _, _, _, _, _).

predicate_table_remove_predid(PredicateTable0, PredId, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, PredIds0, D, E, F, G, H, I),
	list__delete_all(PredIds0, PredId, PredIds),
	PredicateTable = predicate_table(A, B, PredIds, D, E, F, G, H, I).

:- pred predicate_table_reverse_predids(predicate_table, predicate_table).
:- mode predicate_table_reverse_predids(in, out) is det.

predicate_table_reverse_predids(PredicateTable0, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, PredIds0, D, E, F, G, H, I),
	list__reverse(PredIds0, PredIds),
	PredicateTable = predicate_table(A, B, PredIds, D, E, F, G, H, I).

%-----------------------------------------------------------------------------%

:- predicate_table_search_sym(_, X, _) when X. % NU-Prolog indexing.

predicate_table_search_sym(PredicateTable, unqualified(Name), PredIdList) :-
	predicate_table_search_name(PredicateTable, Name, PredIdList).
predicate_table_search_sym(PredicateTable, qualified(Module, Name),
		PredIdList) :-
	predicate_table_search_module_name(PredicateTable, 
		Module, Name, PredIdList),
	PredIdList \= [].

:- predicate_table_search_pred_sym(_, X, _) when X. % NU-Prolog indexing.

predicate_table_search_pred_sym(PredicateTable, unqualified(Name), PredIdList)
		:-
	predicate_table_search_pred_name(PredicateTable, Name, PredIdList).
predicate_table_search_pred_sym(PredicateTable, qualified(Module, Name),
		PredIdList) :-
	predicate_table_search_pred_module_name(PredicateTable, 
		Module, Name, PredIdList),
	PredIdList \= [].

:- predicate_table_search_func_sym(_, X, _) when X. % NU-Prolog indexing.

predicate_table_search_func_sym(PredicateTable, unqualified(Name), PredIdList)
		:-
	predicate_table_search_func_name(PredicateTable, Name, PredIdList).
predicate_table_search_func_sym(PredicateTable, qualified(Module, Name),
		PredIdList) :-
	predicate_table_search_func_module_name(PredicateTable, Module,
		Name, PredIdList),
	PredIdList \= [].

	% Given a list of predicates, and a module name, find all the
	% predicates which came from that module.

%-----------------------------------------------------------------------------%

:- predicate_table_search_sym_arity(_, X, _, _) when X. % NU-Prolog indexing.

predicate_table_search_sym_arity(PredicateTable, qualified(Module, Name),
		Arity, PredIdList) :-
	predicate_table_search_m_n_a(PredicateTable, Module, Name, Arity,
		PredIdList).
predicate_table_search_sym_arity(PredicateTable, unqualified(Name),
		Arity, PredIdList) :-
	predicate_table_search_name_arity(PredicateTable, Name, Arity,
		PredIdList).

:- predicate_table_search_pred_sym_arity(_, X, _, _) when X.

predicate_table_search_pred_sym_arity(PredicateTable, qualified(Module, Name),
		Arity, PredIdList) :-
	predicate_table_search_pred_m_n_a(PredicateTable, Module, Name, Arity,
		PredIdList).
predicate_table_search_pred_sym_arity(PredicateTable, unqualified(Name),
		Arity, PredIdList) :-
	predicate_table_search_pred_name_arity(PredicateTable, Name, Arity,
		PredIdList).

:- predicate_table_search_func_sym_arity(_, X, _, _) when X.

predicate_table_search_func_sym_arity(PredicateTable, qualified(Module, Name),
		Arity, PredIdList) :-
	predicate_table_search_func_m_n_a(PredicateTable, Module, Name, Arity,
		PredIdList).
predicate_table_search_func_sym_arity(PredicateTable, unqualified(Name),
		Arity, PredIdList) :-
	predicate_table_search_func_name_arity(PredicateTable, Name, Arity,
		PredIdList).

%-----------------------------------------------------------------------------%

predicate_table_search_name(PredicateTable, Name, PredIds) :-
	(
		predicate_table_search_pred_name(PredicateTable, Name,
			PredPredIds0)
	->
		PredPredIds = PredPredIds0
	;
		PredPredIds = []
	),
	(
		predicate_table_search_func_name(PredicateTable, Name,
			FuncPredIds0)
	->
		FuncPredIds = FuncPredIds0
	;
		FuncPredIds = []
	),
	list__append(FuncPredIds, PredPredIds, PredIds),
	PredIds \= [].

predicate_table_search_pred_name(PredicateTable, PredName, PredIds) :-
	PredicateTable = predicate_table(_, _, _, PredNameIndex, _, _, _, _, _),
	map__search(PredNameIndex, PredName, PredIds).

predicate_table_search_func_name(PredicateTable, FuncName, PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, _, _, FuncNameIndex, _, _),
	map__search(FuncNameIndex, FuncName, PredIds).

%-----------------------------------------------------------------------------%

:- pred predicate_table_search_module_name(predicate_table, module_name, 
		string, list(pred_id)).
:- mode predicate_table_search_module_name(in, in, in, out) is semidet.

predicate_table_search_module_name(PredicateTable, Module, Name, PredIds) :-
	(
		predicate_table_search_pred_module_name(PredicateTable, 
			Module, Name, PredPredIds0)
	->
		PredPredIds = PredPredIds0
	;
		PredPredIds = []
	),
	(
		predicate_table_search_func_module_name(PredicateTable, 
			Module, Name, FuncPredIds0)
	->
		FuncPredIds = FuncPredIds0
	;
		FuncPredIds = []
	),
	list__append(FuncPredIds, PredPredIds, PredIds),
	PredIds \= [].

:- pred predicate_table_search_pred_module_name(predicate_table, module_name,
		string, list(pred_id)).
:- mode predicate_table_search_pred_module_name(in, in, in, out) is semidet.

predicate_table_search_pred_module_name(PredicateTable, 
		Module, PredName, PredIds) :-
	PredicateTable = predicate_table(_,_,_,_,_, Pred_MNA_Index, _,_,_),
	map__search(Pred_MNA_Index, Module - PredName, Arities),
	map__values(Arities, PredIdLists),
	list__condense(PredIdLists, PredIds).

:- pred predicate_table_search_func_module_name(predicate_table, module_name,
		string, list(pred_id)).
:- mode predicate_table_search_func_module_name(in, in, in, out) is semidet.

predicate_table_search_func_module_name(PredicateTable, 
		Module, FuncName, PredIds) :-
	PredicateTable = predicate_table(_,_,_,_,_,_,_,_, Func_MNA_Index),
	map__search(Func_MNA_Index, Module - FuncName, Arities),
	map__values(Arities, PredIdLists),
	list__condense(PredIdLists, PredIds).

%-----------------------------------------------------------------------------%

predicate_table_search_name_arity(PredicateTable, Name, Arity, PredIds) :-
	(
		predicate_table_search_pred_name_arity(PredicateTable,
			Name, Arity, PredPredIds0)
	->
		PredPredIds = PredPredIds0
	;
		PredPredIds = []
	),
	(
		predicate_table_search_func_name_arity(PredicateTable,
			Name, Arity, FuncPredIds0)
	->
		FuncPredIds = FuncPredIds0
	;
		FuncPredIds = []
	),
	list__append(FuncPredIds, PredPredIds, PredIds),
	PredIds \= [].

predicate_table_search_pred_name_arity(PredicateTable, PredName, Arity,
		PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, PredNameArityIndex, _,
					_, _, _),
	map__search(PredNameArityIndex, PredName / Arity, PredIds).

predicate_table_search_func_name_arity(PredicateTable, FuncName, Arity,
		PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, _, _,
					_, FuncNameArityIndex, _),
	map__search(FuncNameArityIndex, FuncName / Arity, PredIds).

%-----------------------------------------------------------------------------%

predicate_table_search_m_n_a(PredicateTable, Module, Name, Arity,
		PredIds) :-
	(
		predicate_table_search_pred_m_n_a(PredicateTable, Module,
			Name, Arity, PredPredIds0)
	->
		PredPredIds = PredPredIds0
	;
		PredPredIds = []
	),
	(
		predicate_table_search_func_m_n_a(PredicateTable, Module,
			Name, Arity, FuncPredIds0)
	->
		FuncPredIds = FuncPredIds0
	;
		FuncPredIds = []
	),
	list__append(FuncPredIds, PredPredIds, PredIds),
	PredIds \= [].

predicate_table_search_pred_m_n_a(PredicateTable, Module, PredName, Arity,
		PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, _, P_MNA_Index, _, _, _),
	map__search(P_MNA_Index, Module - PredName, ArityIndex),
	map__search(ArityIndex, Arity, PredIds).

predicate_table_search_func_m_n_a(PredicateTable, Module, FuncName, Arity,
		PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, _, _, _, _, F_MNA_Index),
	map__search(F_MNA_Index, Module - FuncName, ArityIndex),
	map__search(ArityIndex, Arity, PredIds).

%-----------------------------------------------------------------------------%

:- predicate_table_search_pf_m_n_a(_, X, _, _, _, _) when X.

predicate_table_search_pf_m_n_a(PredicateTable, predicate, Module, Name, Arity,
		PredIds) :-
	predicate_table_search_pred_m_n_a(PredicateTable, Module, Name, Arity,
			PredIds).
predicate_table_search_pf_m_n_a(PredicateTable, function, Module, Name, Arity,
		PredIds) :-
	FuncArity is Arity - 1,
	predicate_table_search_func_m_n_a(PredicateTable, Module, Name,
			FuncArity, PredIds).

:- predicate_table_search_pf_name_arity(_, X, _, _, _, _) when X.

predicate_table_search_pf_name_arity(PredicateTable, predicate, Name, Arity,
		PredIds) :-
	predicate_table_search_pred_name_arity(PredicateTable, Name, Arity,
			PredIds).
predicate_table_search_pf_name_arity(PredicateTable, function, Name, Arity,
		PredIds) :-
	FuncArity is Arity - 1,
	predicate_table_search_func_name_arity(PredicateTable, Name, FuncArity,
			PredIds).

:- predicate_table_search_pf_sym_arity(_, X, _, _, _) when X.

predicate_table_search_pf_sym_arity(PredicateTable, PredOrFunc,
		qualified(Module, Name), Arity, PredIdList) :-
	predicate_table_search_pf_m_n_a(PredicateTable, PredOrFunc,
		Module, Name, Arity, PredIdList).
predicate_table_search_pf_sym_arity(PredicateTable, PredOrFunc,
		unqualified(Name), Arity, PredIdList) :-
	predicate_table_search_pf_name_arity(PredicateTable, PredOrFunc,
		Name, Arity, PredIdList).

%-----------------------------------------------------------------------------%

predicate_table_insert(PredicateTable0, PredInfo, PredId, PredicateTable) :-
	predicate_table_insert(PredicateTable0, PredInfo, must_be_qualified,
		PredId, PredicateTable).

predicate_table_insert(PredicateTable0, PredInfo, NeedQual,
		PredId, PredicateTable) :-
	PredicateTable0 = predicate_table(Preds0, NextPredId0, PredIds0,
				Pred_N_Index0, Pred_NA_Index0, Pred_MNA_Index0,
				Func_N_Index0, Func_NA_Index0, Func_MNA_Index0),
	pred_info_module(PredInfo, Module),
	pred_info_name(PredInfo, Name),
	pred_info_arity(PredInfo, Arity),

		% allocate a new pred_id
	PredId = NextPredId0,
	hlds_pred__next_pred_id(PredId, NextPredId),

		% insert the pred_id into either the function or predicate
		% indices, as appropriate
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	( 
		PredOrFunc = predicate,
		predicate_table_do_insert(Module, Name, Arity, NeedQual,
			PredId, Pred_N_Index0, Pred_N_Index, 
			Pred_NA_Index0, Pred_NA_Index,
			Pred_MNA_Index0, Pred_MNA_Index),

		Func_N_Index = Func_N_Index0,
		Func_NA_Index = Func_NA_Index0,
		Func_MNA_Index = Func_MNA_Index0
	;
		PredOrFunc = function,

		FuncArity is Arity - 1,

		predicate_table_do_insert(Module, Name, FuncArity, NeedQual,
			PredId, Func_N_Index0, Func_N_Index, 
			Func_NA_Index0, Func_NA_Index,
			Func_MNA_Index0, Func_MNA_Index),

		Pred_N_Index = Pred_N_Index0,
		Pred_NA_Index = Pred_NA_Index0,
		Pred_MNA_Index = Pred_MNA_Index0
	),

		% insert the pred_id into the pred_id list
	PredIds = [PredId | PredIds0],

		% save the pred_info for this pred_id
	map__det_insert(Preds0, PredId, PredInfo, Preds),

	PredicateTable = predicate_table(Preds, NextPredId, PredIds,
				Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
				Func_N_Index, Func_NA_Index, Func_MNA_Index).

:- pred predicate_table_do_insert(module_name, string, arity, need_qualifier,
	pred_id, name_index, name_index, name_arity_index, name_arity_index,
	module_name_arity_index, module_name_arity_index).
:- mode predicate_table_do_insert(in, in, in, in, in, 
	in, out, in, out, in, out) is det.

predicate_table_do_insert(Module, Name, Arity, NeedQual, PredId, 
		N_Index0, N_Index, NA_Index0, NA_Index, 
		MNA_Index0, MNA_Index) :-
	( NeedQual = may_be_unqualified ->
			% insert the pred_id into the name index
		( map__search(N_Index0, Name, N_PredIdList0) ->
			N_PredIdList = [PredId | N_PredIdList0],
			map__det_update(N_Index0, Name,
				N_PredIdList, N_Index)
		;
			N_PredIdList = [PredId],
			map__det_insert(N_Index0, Name, 
				N_PredIdList, N_Index)
		),

			% insert it into the name/arity index
		NA = Name / Arity,
		( map__search(NA_Index0, NA, NA_PredIdList0) ->
			NA_PredIdList = [PredId | NA_PredIdList0],
			map__det_update(NA_Index0, NA,
				NA_PredIdList, NA_Index)
		;
			NA_PredIdList = [PredId],
			map__det_insert(NA_Index0, NA,
				NA_PredIdList,	NA_Index)
		)
	;
		N_Index = N_Index0,
		NA_Index = NA_Index0
	),

		% insert it into the module:name/arity index
	( map__search(MNA_Index0, Module - Name, MN_Arities0) ->
		( map__search(MN_Arities0, Arity, MNA_PredIdList0) ->
			map__det_update(MN_Arities0, Arity, 
				[PredId | MNA_PredIdList0], MN_Arities)
		;
			map__det_insert(MN_Arities0, Arity, 
				[PredId], MN_Arities)
		),
		map__det_update(MNA_Index0, Module - Name, MN_Arities,
			MNA_Index)
	;
		map__init(MN_Arities0),
		map__det_insert(MN_Arities0, Arity, 
			[PredId], MN_Arities),
		map__det_insert(MNA_Index0, Module - Name, MN_Arities,
			MNA_Index)
	).

%-----------------------------------------------------------------------------%

get_pred_id_and_proc_id(SymName, PredOrFunc, TVarSet, ArgTypes, ModuleInfo,
			PredId, ProcId) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	list__length(ArgTypes, Arity),
	(
		(
			% In this case there is no overloading to resolve,
			% so just look up the pred_id. 
			SymName = qualified(Module, Name),
			predicate_table_search_pf_m_n_a(PredicateTable,
				PredOrFunc, Module, Name, Arity, [PredId0])
		;
			% Resolve overloading using the arguments types. 
			SymName = unqualified(Name),
			predicate_table_search_pf_name_arity(PredicateTable,
				PredOrFunc, Name, Arity, PredIds),
			typecheck__find_matching_pred_id(PredIds, ModuleInfo,
				TVarSet, ArgTypes, PredId0, _PredName)
		)
	->
		PredId = PredId0,
		get_proc_id(PredicateTable, PredId, ProcId)
	;
		% Undefined/invalid pred or func.
		% the type-checker should ensure that this never happens
		hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr),
		unqualify_name(SymName, Name2),
		string__int_to_string(Arity, ArityString),
		string__append_list(
			["get_pred_id_and_proc_id: ",
			"undefined/invalid ", PredOrFuncStr,
			"\n`", Name2, "/", ArityString, "'"],
			Msg),
		error(Msg)
	).

:- pred get_proc_id(predicate_table, pred_id, proc_id).
:- mode get_proc_id(in, in, out) is det.

get_proc_id(PredicateTable, PredId, ProcId) :-
	predicate_table_get_preds(PredicateTable, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__keys(Procs, ProcIds),
	( ProcIds = [ProcId0] ->
		ProcId = ProcId0
	;
		pred_info_name(PredInfo, Name),
		pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
		pred_info_arity(PredInfo, Arity),
		hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr),
		string__int_to_string(Arity, ArityString),
		( ProcIds = [] ->
			string__append_list([
				"cannot take address of ", PredOrFuncStr,
				"\n`", Name, "/", ArityString,
				"' with no modes.\n",
				"(Sorry, confused by earlier errors -- ",
				"bailing out.)"],
				Message)
		;
			string__append_list([
				"sorry, not implemented: ",
				"taking address of ", PredOrFuncStr,
				"\n`", Name, "/", ArityString,
				"' with multiple modes.\n",
				"(use an explicit lambda expression instead)"],
				Message)
		),
		error(Message)
	).

%-----------------------------------------------------------------------------%

predicate_id(ModuleInfo, PredId, ModuleName, PredName, Arity) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_module(PredInfo, ModuleName),
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, Arity).

predicate_module(ModuleInfo, PredId, ModuleName) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_module(PredInfo, ModuleName).

predicate_name(ModuleInfo, PredId, PredName) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_name(PredInfo, PredName).

predicate_arity(ModuleInfo, PredId, Arity) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_arity(PredInfo, Arity).

%-----------------------------------------------------------------------------%
