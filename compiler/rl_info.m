%-----------------------------------------------------------------------------%
% Copyright (C) 1998, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_info.m.
% Main author: stayl
%
% State type + access predicates for the Aditi-RL code generator.
%-----------------------------------------------------------------------------%
:- module aditi_backend__rl_info.

:- interface.

:- import_module aditi_backend__rl.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module libs__tree.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module string.

:- type rl_info.

:- type rl_tree == tree(list(rl_instruction)).

:- type relation_spec == pair(proc_relation_type, pred_proc_id).

:- type proc_relation_type
	--->	diff		% Differences local to this iteration.
	;	new_diff	% New differences in this iteration,
				% to be copied into the diff relation
				% at the end of the iteration.
	;	acc_diff	% All differences accumulated
				% since the previous call to the
				% current RL procedure.
	;	full		% The entire relation.
	.

:- type var_status
	--->	normal
	;	input_closure	% contains an input relation, one of the
				% input arguments to the RL procedure.
	;	closure_pred(list(prog_var), pred_proc_id)
				% an input closure constructed during the
				% current rule.
	.

:- type relation_schema
	--->	same_as_pred(pred_proc_id)
	;	same_as_relation(relation_id)
	;	schema(list(type)).

:- inst uniq_rl_info == ground.

:- mode rl_info_uo == free >> uniq_rl_info.
:- mode rl_info_ui == uniq_rl_info >> uniq_rl_info.
:- mode rl_info_di == uniq_rl_info >> uniq_rl_info.

:- inst rl_info_no_io == ground.

:- mode rl_info_get_io_state == uniq_rl_info >> rl_info_no_io.
:- mode rl_info_no_io        == rl_info_no_io >> rl_info_no_io.
:- mode rl_info_set_io_state == rl_info_no_io >> dead.

%-----------------------------------------------------------------------------%

:- pred rl_info_init(module_info, io__state, rl_info).
:- mode rl_info_init(in, di, rl_info_uo) is det.

%-----------------------------------------------------------------------------%

	% Access predicates.

:- pred rl_info_get_io_state(io__state, rl_info, rl_info).
:- mode rl_info_get_io_state(uo, rl_info_get_io_state,
		(free >> rl_info_no_io)) is det.

:- pred rl_info_set_io_state(io__state, rl_info, rl_info).
:- mode rl_info_set_io_state(di, rl_info_set_io_state, rl_info_uo) is det.

:- pred rl_info_get_module_info(module_info, rl_info, rl_info).
:- mode rl_info_get_module_info(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_module_info(module_info, rl_info, rl_info).
:- mode rl_info_set_module_info(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_pred_info(pred_info, rl_info, rl_info).
:- mode rl_info_get_pred_info(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_pred_info(pred_info, rl_info, rl_info).
:- mode rl_info_set_pred_info(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_proc_info(proc_info, rl_info, rl_info).
:- mode rl_info_get_proc_info(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_proc_info(proc_info, rl_info, rl_info).
:- mode rl_info_set_proc_info(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_scc_list(list(pred_proc_id), rl_info, rl_info).
:- mode rl_info_get_scc_list(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_scc_list(list(pred_proc_id), rl_info, rl_info).
:- mode rl_info_set_scc_list(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_relation_info(map(relation_id, relation_info),
		rl_info, rl_info).
:- mode rl_info_get_relation_info(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_relation_info(map(relation_id, relation_info),
		rl_info, rl_info).
:- mode rl_info_set_relation_info(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_next_label_id(label_id, rl_info, rl_info).
:- mode rl_info_get_next_label_id(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_next_relation_id(relation_id, rl_info, rl_info).
:- mode rl_info_get_next_relation_id(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_pred_proc_id(pred_proc_id, rl_info, rl_info).
:- mode rl_info_get_pred_proc_id(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_pred_proc_id(pred_proc_id, rl_info, rl_info).
:- mode rl_info_set_pred_proc_id(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_is_highest_scc(bool, rl_info, rl_info).
:- mode rl_info_get_is_highest_scc(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_is_highest_scc(bool, rl_info, rl_info).
:- mode rl_info_set_is_highest_scc(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_scc_entry_points(list(pred_proc_id), rl_info, rl_info).
:- mode rl_info_get_scc_entry_points(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_scc_entry_points(list(pred_proc_id), rl_info, rl_info).
:- mode rl_info_set_scc_entry_points(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_scc(list(pred_proc_id), rl_info, rl_info).
:- mode rl_info_get_scc(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_scc(list(pred_proc_id), rl_info, rl_info).
:- mode rl_info_set_scc(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_var_rels(map(prog_var, relation_id), rl_info, rl_info).
:- mode rl_info_get_var_rels(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_var_rels(map(prog_var, relation_id), rl_info, rl_info).
:- mode rl_info_set_var_rels(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_rule_number(int, rl_info, rl_info).
:- mode rl_info_get_rule_number(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_rule_number(int, rl_info, rl_info).
:- mode rl_info_set_rule_number(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_scc_list_args(list(relation_id), rl_info, rl_info).
:- mode rl_info_get_scc_list_args(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_scc_list_args(list(relation_id), rl_info, rl_info).
:- mode rl_info_set_scc_list_args(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_var_status_map(map(prog_var, var_status), rl_info, rl_info).
:- mode rl_info_get_var_status_map(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_var_stats(map(prog_var, var_status), rl_info, rl_info).
:- mode rl_info_set_var_stats(in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_var_status(prog_var, var_status, rl_info, rl_info).
:- mode rl_info_get_var_status(in, out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_delayed_diffs(set(pred_proc_id), rl_info, rl_info).
:- mode rl_info_get_delayed_diffs(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_delayed_diffs(set(pred_proc_id), rl_info, rl_info).
:- mode rl_info_set_delayed_diffs(in, rl_info_di, rl_info_uo) is det.

%-----------------------------------------------------------------------------%

:- pred rl_info_get_new_temporary(relation_schema, relation_id,
		rl_info, rl_info).
:- mode rl_info_get_new_temporary(in, out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_relation_schema_to_type_list(relation_schema::in,
	list(type)::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

	% Get the relation corresponding to the given relation_spec,
	% creating it if it doesn't exist.
:- pred rl_info_lookup_relation(relation_spec, relation_id, rl_info, rl_info).
:- mode rl_info_lookup_relation(in, out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_relation_schema(relation_id, list(type), rl_info, rl_info).
:- mode rl_info_get_relation_schema(in, out, rl_info_di, rl_info_uo) is det.

	% Return whether a variable holds an input closure.
:- pred rl_info_set_var_status(prog_var, var_status, rl_info, rl_info).
:- mode rl_info_set_var_status(in, in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_make_vars_equivalent(prog_var, prog_var, rl_info, rl_info).
:- mode rl_info_make_vars_equivalent(in, in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_current_proc_output_schema(relation_schema,
		rl_info, rl_info).
:- mode rl_info_get_current_proc_output_schema(out,
		rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_current_proc_output_vars(list(prog_var), rl_info, rl_info).
:- mode rl_info_get_current_proc_output_vars(out,
		rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_proc_schema(pred_proc_id, relation_schema,
		rl_info, rl_info).
:- mode rl_info_get_proc_schema(in, out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_partition_call_args(pred_proc_id, list(T),
		list(T), list(T), rl_info, rl_info).
:- mode rl_info_partition_call_args(in, in, out, out,
		rl_info_di, rl_info_uo) is det.

:- pred rl_info_get_var_type(prog_var, (type), rl_info, rl_info).
:- mode rl_info_get_var_type(in, out, rl_info_di, rl_info_uo) is det.

%-----------------------------------------------------------------------------%

:- pred rl_info_bind_var_to_relation(prog_var, relation_id, rl_info, rl_info).
:- mode rl_info_bind_var_to_relation(in, in, rl_info_di, rl_info_uo) is det.

:- pred rl_info_lookup_var_relation(prog_var, relation_id, rl_info, rl_info).
:- mode rl_info_lookup_var_relation(in, out, rl_info_di, rl_info_uo) is det.

%-----------------------------------------------------------------------------%

	% Create a comment showing which predicate the current rule came from.
:- pred rl_info__comment(string::out, rl_info::rl_info_di,
		rl_info::rl_info_uo) is det.

:- pred rl_info_write_message(string, list(string__poly_type),
		rl_info, rl_info).
:- mode rl_info_write_message(in, in, rl_info_di, rl_info_uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_goal.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__prog_out.
:- import_module mdbcomp__prim_data.

:- import_module int.
:- import_module require.

:- type rl_info
	--->	rl_info(
			io__state,
			module_info,
			maybe(pred_info),
			maybe(proc_info),
			list(pred_proc_id),	% predicates in the current
						% RL procedure.
			relation_map,
			map(relation_id, relation_info),
			unit,
			map(prog_var, relation_id),
						% map from var to
						% magic input relation.
			list(pred_proc_id),	% preds in the current SCC.
			int,			% current rule within
						% a predicate
			label_id,		% next label_id
			relation_id,		% next relation_id
			pred_proc_id,		% current pred_proc_id
			list(relation_id),	% RL procedure
						% argument relations
			map(prog_var, var_status),
			set(pred_proc_id),	% predicates for which we need
						% to delay updating the diff
						% relation until the end of
						% the iteration. See the
						% comment on rl_gen__order_scc.

			bool,			% is the current SCC
						% highest in SCC list
						% for the current RL procedure.
			list(pred_proc_id)	% entry-points of the current
						% SCC.
		).

:- pred rl_info_get_relation_map(relation_map, rl_info, rl_info).
:- mode rl_info_get_relation_map(out, rl_info_di, rl_info_uo) is det.

:- pred rl_info_set_relation_map(relation_map, rl_info, rl_info).
:- mode rl_info_set_relation_map(in, rl_info_di, rl_info_uo) is det.

:- type relation_map == map(relation_spec, relation_id).

%-----------------------------------------------------------------------------%

rl_info_init(ModuleInfo, IO, RLInfo) :-
	map__init(RelMap),
	map__init(RelInfo),
	map__init(VarRels),
	map__init(VarStat),
	set__init(DelayedDiffs),
	PredId = invalid_pred_id,
	ProcId = invalid_proc_id,
	RLInfo = rl_info(IO, ModuleInfo, no, no, [], RelMap, RelInfo,
		unit, VarRels, [], 0, 0, 0, proc(PredId, ProcId),
		[], VarStat, DelayedDiffs, no, []).

%-----------------------------------------------------------------------------%

rl_info_get_io_state(IO, RLInfo, RLInfo) :-
	RLInfo = rl_info(IO0, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
	unsafe_promise_unique(IO0, IO).

rl_info_set_io_state(IO0, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(_, B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S),
	unsafe_promise_unique(IO0, IO),
	RLInfo = rl_info(IO, B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).

rl_info_get_module_info(ModuleInfo, RLInfo, RLInfo) :-
	RLInfo = rl_info(_, ModuleInfo, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

rl_info_set_module_info(ModuleInfo, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A, _, C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S),
	RLInfo = rl_info(A, ModuleInfo, C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).

rl_info_get_pred_info(PredInfo, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_, MaybePredInfo, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
	( MaybePredInfo = yes(PredInfo1) ->
		PredInfo = PredInfo1
	;
		error("rl_info_get_pred_info: pred_info not set")
	).

rl_info_set_pred_info(PredInfo, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B, _, D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S),
	RLInfo = rl_info(A,B, yes(PredInfo), D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).

rl_info_get_proc_info(ProcInfo, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_, MaybeProcInfo, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
	( MaybeProcInfo = yes(ProcInfo1) ->
		ProcInfo = ProcInfo1
	;
		error("rl_info_get_pred_info: pred_info not set")
	).


rl_info_set_proc_info(ProcInfo, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C ,_, E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S),
	RLInfo = rl_info(A,B,C, yes(ProcInfo), E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).

rl_info_get_scc_list(SubModule, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_, SubModule, _,_,_,_,_,_,_,_,_,_,_,_,_,_).

rl_info_set_scc_list(SubModule, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D, _, F,G,H,I,J,K,L,M,N,O,P,Q,R,S),
	RLInfo = rl_info(A,B,C,D, SubModule, F,G,H,I,J,K,L,M,N,O,P,Q,R,S).

rl_info_get_relation_map(RelMap, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_,_, RelMap, _,_,_,_,_,_,_,_,_,_,_,_,_).

rl_info_set_relation_map(RelMap, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E, _, G,H,I,J,K,L,M,N,O,P,Q,R,S),
	RLInfo = rl_info(A,B,C,D,E, RelMap, G,H,I,J,K,L,M,N,O,P,Q,R,S).

rl_info_get_relation_info(RelInfo, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_,_,_, RelInfo, _,_,_,_,_,_,_,_,_,_,_,_).

rl_info_set_relation_info(RelInfo, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F, _, H,I,J,K,L,M,N,O,P,Q,R,S),
	RLInfo = rl_info(A,B,C,D,E,F, RelInfo, H,I,J,K,L,M,N,O,P,Q,R,S).

rl_info_get_var_rels(VarRels, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_,_,_,_,_,VarRels,_,_,_,_,_,_,_,_,_,_).

rl_info_set_var_rels(VarRels, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H, _, J,K,L,M,N,O,P,Q,R,S),
	RLInfo = rl_info(A,B,C,D,E,F,G,H, VarRels, J,K,L,M,N,O,P,Q,R,S).

rl_info_get_scc(SCC, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_,_,_,_,_,_,SCC,_,_,_,_,_,_,_,_,_).

rl_info_set_scc(SCC, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H,I, _, K,L,M,N,O,P,Q,R,S),
	RLInfo = rl_info(A,B,C,D,E,F,G,H,I, SCC, K,L,M,N,O,P,Q,R,S).

rl_info_get_rule_number(RuleNumber, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_,_,_,_,_,_,_,RuleNumber,_,_,_,_,_,_,_,_).

rl_info_set_rule_number(RuleNumber, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H,I,J,_,L,M,N,O,P,Q,R,S),
	RLInfo = rl_info(A,B,C,D,E,F,G,H,I,J,RuleNumber,L,M,N,O,P,Q,R,S).

rl_info_get_next_label_id(NextLabelId, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H,I,J,K,LabelId,M,N,O,P,Q,R,S),
	NextLabelId = LabelId + 1,
	RLInfo = rl_info(A,B,C,D,E,F,G,H,I,J,K,NextLabelId,M,N,O,P,Q,R,S).

rl_info_get_next_relation_id(NextRelationId, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,RelationId,N,O,P,Q,R,S),
	NextRelationId = RelationId + 1,
	RLInfo = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,NextRelationId,N,O,P,Q,R,S).

rl_info_get_pred_proc_id(PredProcId, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_,_,_,_,_,_,_,_,_,_,PredProcId,_,_,_,_,_).

rl_info_set_pred_proc_id(PredProcId, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,_,O,P,Q,R,S),
	RLInfo = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,PredProcId,O,P,Q,R,S).

rl_info_get_scc_list_args(Args, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,Args,_,_,_,_).

rl_info_set_scc_list_args(Args, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,_,P,Q,R,S),
	RLInfo = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Args,P,Q,R,S).

rl_info_get_var_status_map(Stat, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,Stat,_,_,_).

rl_info_set_var_stats(Stat, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,_,Q,R,S),
	RLInfo = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Stat,Q,R,S).

rl_info_get_delayed_diffs(Procs, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,Procs,_,_).

rl_info_set_delayed_diffs(Procs, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,_,R,S),
	RLInfo = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Procs,R,S).

rl_info_get_is_highest_scc(IsHighestSCC, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,IsHighestSCC,_).

rl_info_set_is_highest_scc(IsHighestSCC, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,_,S),
	RLInfo = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,IsHighestSCC,S).

rl_info_get_scc_entry_points(EntryPoints, RLInfo, RLInfo) :-
	RLInfo = rl_info(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,EntryPoints).

rl_info_set_scc_entry_points(EntryPoints, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,_),
	RLInfo = rl_info(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,EntryPoints).

%-----------------------------------------------------------------------------%

rl_info_get_new_temporary(TempRelationSchema, RelationId) -->
	rl_info_get_next_relation_id(RelationId),
	rl_info_get_relation_info(RelationInfo0),
	rl_info_relation_schema_to_type_list(TempRelationSchema, Types),
	{ rl__relation_id_to_string(RelationId, RelationName) },
	rl_info_get_module_info(ModuleInfo),
	{ rl__default_temporary_state(ModuleInfo, TmpState) },
	{ RelInfo = relation_info(temporary(TmpState),
			Types, [], RelationName) },
	{ map__det_insert(RelationInfo0, RelationId, RelInfo, RelationInfo) },
	rl_info_set_relation_info(RelationInfo).

rl_info_relation_schema_to_type_list(same_as_pred(proc(PredId, ProcId)),
		Types) -->
	rl_info_get_module_info(ModuleInfo),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_arg_types(PredInfo, AllArgTypes) },
	rl_info_partition_call_args(proc(PredId, ProcId),
			AllArgTypes, _, Types).
rl_info_relation_schema_to_type_list(same_as_relation(RelationId), Types) -->
	rl_info_get_relation_info(RelInfo),
	{ map__lookup(RelInfo, RelationId, RelationInfo) },
	{ RelationInfo = relation_info(_, Types, _, _) }.
rl_info_relation_schema_to_type_list(schema(Types), Types) --> [].

rl_info_lookup_relation(TempRelationId, RelationId) -->
	rl_info_get_relation_map(RelMap0),
	( { map__search(RelMap0, TempRelationId, RelationId1) } ->
		{ RelationId = RelationId1 }
	;
		{ TempRelationId = ProcRelType - PredProcId },
		rl_info_get_proc_schema(PredProcId, Schema),
		rl_info_relation_schema_to_type_list(Schema, Types),
		rl_info_get_module_info(ModuleInfo),

		% There can be only one Aditi procedure with this
		% pred_id (others are separated out), so the proc_id
		% can be ignored when creating the name.
		{ PredProcId = proc(PredId, _) },
		{ module_info_pred_info(ModuleInfo,
			PredId, PredInfo) },
		( { hlds_pred__pred_info_is_base_relation(PredInfo) } ->
			{ RelType = permanent(PredProcId) },
			{ pred_info_get_indexes(PredInfo, Indexes) }
		;
			{ rl__default_temporary_state(ModuleInfo, TmpState) },
			{ RelType = temporary(TmpState) },
			{ Indexes = [] }
		),

		% Get a (sort of) human readable version of the relation name.
		{ proc_relation_type_to_str(ProcRelType, ProcRelStr) },
		{ PredModule0 = pred_info_module(PredInfo) },
		{ mdbcomp__prim_data__sym_name_to_string(PredModule0, 
			PredModule) },
		{ PredName = pred_info_name(PredInfo) },
		{ Arity = pred_info_orig_arity(PredInfo) },
		rl_info_get_next_relation_id(RelationId),
		{ string__format("%s-%s.%s/%i-%i",
			[s(ProcRelStr), s(PredModule), s(PredName),
			i(Arity), i(RelationId)], RelationName) },

		{ RelationInfo = relation_info(RelType, Types,
			Indexes, RelationName) },
		rl_info_get_relation_info(RelationInfos0),
		{ map__det_insert(RelationInfos0, RelationId,
			RelationInfo, RelationInfos) },
		rl_info_set_relation_info(RelationInfos),
		{ map__det_insert(RelMap0, TempRelationId,
			RelationId, RelMap) },
		rl_info_set_relation_map(RelMap)
	).

:- pred proc_relation_type_to_str(proc_relation_type::in, string::out) is det.

proc_relation_type_to_str(full, "Full").
proc_relation_type_to_str(diff, "Diff").
proc_relation_type_to_str(acc_diff, "AccDiff").
proc_relation_type_to_str(new_diff, "NewDiff").

rl_info_get_relation_schema(Rel, Schema) -->
	rl_info_get_relation_info(RelationInfos0),
	{ map__lookup(RelationInfos0, Rel,
		relation_info(_, Schema, _, _)) }.

rl_info_make_vars_equivalent(Var1, Var2, RLInfo0, RLInfo) :-
	RLInfo0 = rl_info(A,B,C,D,E,F,G,H, VarRels0, J,K,L,M,N,O,
			VarStat0, Q,R,S),
	( map__search(VarRels0, Var2, RelId) ->
		map__det_insert(VarRels0, Var1, RelId, VarRels)
	;
		VarRels = VarRels0
	),
	( map__search(VarStat0, Var2, Stat) ->
		map__det_insert(VarStat0, Var1, Stat, VarStat)
	;
		VarStat = VarStat0
	),
	RLInfo = rl_info(A,B,C,D,E,F,G,H, VarRels, J,K,L,M,N,O,
			VarStat, Q,R,S).

rl_info_get_var_type(Var, Type) -->
	rl_info_get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ map__lookup(VarTypes, Var, Type) }.

%-----------------------------------------------------------------------------%

rl_info_lookup_var_relation(Var, RelationId) -->
	rl_info_get_var_rels(VarRels),
	{ map__lookup(VarRels, Var, RelationId) }.

rl_info_bind_var_to_relation(Var, RelationId) -->
	rl_info_get_var_rels(VarRels0),
	{ map__set(VarRels0, Var, RelationId, VarRels) },
	rl_info_set_var_rels(VarRels),
	rl_info_set_var_status(Var, input_closure).

rl_info_get_var_status(Var, Status) -->
	rl_info_get_var_status_map(VarStat),
	{ map__search(VarStat, Var, Status1) ->
		Status = Status1
	;
		Status = normal
	}.

rl_info_set_var_status(Var, Status) -->
	rl_info_get_var_status_map(VarStat0),
	{ map__set(VarStat0, Var, Status, VarStat) },
	rl_info_set_var_stats(VarStat).

%-----------------------------------------------------------------------------%

rl_info_partition_call_args(proc(PredId, ProcId), AllArgs,
		InputArgs, OutputArgs) -->
	rl_info_get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredId,
		ProcId, _, ProcInfo) },
	{ proc_info_argmodes(ProcInfo, ArgModes) },
	{ partition_args(ModuleInfo, ArgModes,
		AllArgs, InputArgs, OutputArgs) }.

%-----------------------------------------------------------------------------%

rl_info_get_current_proc_output_schema(schema(OutputArgTypes)) -->
	rl_info_get_pred_info(PredInfo),
	rl_info_get_proc_info(ProcInfo),
	{ proc_info_argmodes(ProcInfo, ArgModes) },
	{ pred_info_arg_types(PredInfo, ArgTypes) },
	rl_info_get_module_info(ModuleInfo),
	{ partition_args(ModuleInfo, ArgModes, ArgTypes, _, OutputArgTypes) }.

rl_info_get_current_proc_output_vars(Vars) -->
	rl_info_get_proc_info(ProcInfo),
	{ proc_info_headvars(ProcInfo, HeadVars) },
	{ proc_info_argmodes(ProcInfo, ArgModes) },
	rl_info_get_module_info(ModuleInfo),
	{ partition_args(ModuleInfo, ArgModes, HeadVars, _, Vars) }.

rl_info_get_proc_schema(proc(PredId, ProcId), schema(Schema)) -->
	rl_info_get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo,
		PredId, ProcId, PredInfo, ProcInfo) },
	{ pred_info_arg_types(PredInfo, ArgTypes) },
	{ pred_info_get_markers(PredInfo, Markers) },
	( { check_marker(Markers, base_relation) } ->
		{ Schema = ArgTypes }
	;
		{ proc_info_argmodes(ProcInfo, ArgModes) },
		{ partition_args(ModuleInfo, ArgModes, ArgTypes, _, Schema) }
	).

rl_info_write_message(FormatStr, Items) -->
	rl_info_get_io_state(IO0),
	{ globals__io_lookup_bool_option(debug_rl_gen, Debug, IO0, IO1) },
	{ Debug = yes ->
		string__format(FormatStr, Items, Message),
		io__write_string(Message, IO1, IO2),
		io__flush_output(IO2, IO)
	;
		IO = IO1
	},
	rl_info_set_io_state(IO).

%-----------------------------------------------------------------------------%

rl_info__comment(Comment) -->
	rl_info_get_pred_info(PredInfo),
	rl_info_get_rule_number(RuleNo),
	{ string__format("%s rule %i",
		[s(pred_info_name(PredInfo)), i(RuleNo)], Comment) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
