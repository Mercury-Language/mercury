%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_dump.m
% Main author: stayl
%
% Output a human readable representation of RL for debugging.
%
%-----------------------------------------------------------------------------%
:- module aditi_backend__rl_dump.

:- interface.

:- import_module aditi_backend__rl.
:- import_module hlds__hlds_module.

:- import_module io.

:- pred rl_dump__write_procedure(module_info, rl_proc, io__state, io__state).
:- mode rl_dump__write_procedure(in, in, di, uo) is det.

:- pred rl_dump__write_instruction(module_info, relation_info_map,
		rl_instruction, io__state, io__state).
:- mode rl_dump__write_instruction(in, in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module hlds__hlds_out.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module term.
:- import_module term_io.
:- import_module varset.

rl_dump__write_procedure(ModuleInfo, Proc) -->
	{ Proc = rl_proc(Name, Inputs, Outputs, MemoedRels,
			RelationInfo, Instructions, SCC) },
	io__write_string("% Procedure for\n"),
	{ OutputProcName = (pred(PredProcId::in, IO0::di, IO::uo) is det :-
		PredProcId = proc(PredId, _ProcId),
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		PredName = pred_info_name(PredInfo),
		io__write_string("%\t", IO0, IO1),
		io__write_string(PredName, IO1, IO2),
		io__nl(IO2, IO)
	) },
	list__foldl(OutputProcName, SCC),
	io__write_string("% Memoed relations "),
	{ set__to_sorted_list(MemoedRels, MemoedList) },
	rl_dump__write_list(rl_dump__write_relation_id(RelationInfo),
		MemoedList),
	io__write_string("\n\nPROCEDURE "),
	{ rl__proc_name_to_string(Name, NameStr) },
	io__write_string(NameStr),
	io__write_string("("),
	rl_dump__write_list(rl_dump__write_relation_id(RelationInfo),
		Inputs),
	comma,
	rl_dump__write_list(rl_dump__write_relation_id(RelationInfo),
		Outputs),
	io__write_string(")\n{\n"),
	{ map__keys(RelationInfo, RelationIds) },
	list__foldl(rl_dump__declare_relation(ModuleInfo, RelationInfo),
		RelationIds),
	io__nl,
	list__foldl(rl_dump__write_instruction(ModuleInfo, RelationInfo),
		Instructions),
	io__write_string("}\n\n\n\n").

%-----------------------------------------------------------------------------%

:- pred rl_dump__declare_relation(module_info::in,
		map(relation_id, relation_info)::in, relation_id::in,
		io__state::di, io__state::uo) is det.

rl_dump__declare_relation(ModuleInfo, RelationInfos, RelId) -->
	{ map__lookup(RelationInfos, RelId, RelInfo) },
	{ RelInfo = relation_info(RelType, Types, Index, _) },
	rl_dump__verbose_write_relation_id(RelationInfos, RelId),
	io__write_string(" : "),
	(
		{ RelType = permanent(proc(PredId, _)) },
		io__write_string("base relation `"),
		{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
		{ PredName = pred_info_name(PredInfo) },
		{ PredArity = pred_info_orig_arity(PredInfo) },
		io__write_string(PredName),
		io__write_string("'/"),
		io__write_int(PredArity)
	;
		{ RelType = temporary(State) },
		io__write_string("temporary ("),
		io__write(State),
		io__write_string(")")
	),
	{ varset__init(TVarSet) },
	io__write_string(" "),
	rl_dump__write_list(term_io__write_term(TVarSet), Types),
	io__write_string(" : "),
	io__write_list(Index, " ", mercury_output_index_spec),
	io__write_string(".\n").

%-----------------------------------------------------------------------------%

rl_dump__write_instruction(ModuleInfo, RelationInfo,
		join(Output, Input1, Input2, JoinType, Exprn,
			SemiJoinInfo, TrivialJoinInfo) - Comment) -->
	rl_dump__write_output_rel(RelationInfo, Output),
	io__write_string(" = join("),
	rl_dump__write_relation_id(RelationInfo, Input1),
	comma,
	rl_dump__write_relation_id(RelationInfo, Input2),
	comma,
	io__nl,
	io__write_string("\t\t"),
	rl_dump__write_join_type(ModuleInfo, JoinType),
	(
		{ SemiJoinInfo = yes(SemiTuple) },
		io__write_string(" (semi-join tuple "),
		io__write(SemiTuple),
		io__write_string(")")
	;
		{ SemiJoinInfo = no }
	),
	rl_dump__write_trivial_join_or_subtract(TrivialJoinInfo),
	comma,
	io__nl,
	rl_dump__write_goal(ModuleInfo, Exprn),
	io__write_string(").\t"),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(ModuleInfo, RelationInfo,
		subtract(Output, Input1, Input2, SubType,
			Exprn, TrivialSubtractInfo) - Comment) -->
	rl_dump__write_output_rel(RelationInfo, Output),
	io__write_string(" = subtract("),
	rl_dump__write_relation_id(RelationInfo, Input1),
	comma,
	rl_dump__write_relation_id(RelationInfo, Input2),
	comma,
	rl_dump__write_subtract_type(ModuleInfo, SubType),
	comma,
	rl_dump__write_trivial_join_or_subtract(TrivialSubtractInfo),
	io__nl,
	rl_dump__write_goal(ModuleInfo, Exprn),
	io__write_string(").\t"),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(ModuleInfo, RelationInfo,
		difference(Output, Input1, Input2, Type) - Comment) -->
	rl_dump__write_output_rel(RelationInfo, Output),
	io__write_string(" = difference("),
	rl_dump__write_relation_id(RelationInfo, Input1),
	comma,
	rl_dump__write_relation_id(RelationInfo, Input2),
	comma,
	rl_dump__write_difference_type(ModuleInfo, Type),
	io__write_string(").\t"),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(ModuleInfo, RelationInfo,
		project(Output, Input, Exprn, OtherOutputs, Type) - Comment) -->
	rl_dump__write_output_rel(RelationInfo, Output),
	io__write_string(" = project("),
	rl_dump__write_relation_id(RelationInfo, Input),
	comma,
	rl_dump__write_project_type(ModuleInfo, Type),
	io__nl,
	rl_dump__write_goal(ModuleInfo, Exprn),
	io__nl,
	io__write_string("\t"),
	io__write_list(OtherOutputs, ",\n",
		rl_dump__write_project_output(ModuleInfo, RelationInfo)),

	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(ModuleInfo, RelationInfo,
		union(Output, Inputs, UnionType) - Comment) -->
	rl_dump__write_output_rel(RelationInfo, Output),
	io__write_string(" = union("),
	rl_dump__write_list(rl_dump__write_relation_id(RelationInfo), ",\n\t",
		Inputs),
	io__write_string("\n\t"),
	comma,
	rl_dump__write_union_type(ModuleInfo, UnionType),
	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, RelationInfo,
	union_diff(Union, Input1, Input2, Diff, Index, CopyInfo) - Comment) -->

	rl_dump__write_output_rel(RelationInfo, Diff),
	io__write_string(" = union_diff("),
	rl_dump__write_relation_id(RelationInfo, Input1),
	io__write_string(" => "),
	rl_dump__write_relation_id(RelationInfo, Union),
	comma,
	rl_dump__write_relation_id(RelationInfo, Input2),
	comma,
	mercury_output_index_spec(Index),
	comma,
	io__write(CopyInfo),
	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(ModuleInfo, RelationInfo,
		insert(UoOutput, DiInput, Input, Type, CopyInfo) - Comment) -->
	io__write_string("insert("),
	rl_dump__write_relation_id(RelationInfo, DiInput),
	io__write_string(" => "),
	rl_dump__write_relation_id(RelationInfo, UoOutput),
	comma,
	rl_dump__write_relation_id(RelationInfo, Input),
	comma,
	rl_dump__write_insert_type(ModuleInfo, Type),
	comma,
	io__write(CopyInfo),
	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, RelationInfo,
		sort(Output, Input, Attrs) - Comment) -->
	rl_dump__write_output_rel(RelationInfo, Output),
	io__write_string(" = sort("),
	rl_dump__write_relation_id(RelationInfo, Input),
	comma,
	rl_dump__write_sort_attr_list(Attrs),
	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, RelationInfo,
		init(RelationId) - Comment) -->
	io__write_string("init("),
	rl_dump__write_output_rel(RelationInfo, RelationId),
	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(ModuleInfo, RelationInfo,
		insert_tuple(Output, Input, Exprn) - Comment) -->
	rl_dump__write_output_rel(RelationInfo, Output),
	io__write_string(" = insert_tuple("),
	rl_dump__write_relation_id(RelationInfo, Input),
	comma,
	io__nl,
	rl_dump__write_goal(ModuleInfo, Exprn),
	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, RelationInfo,
		add_index(Output, Input) - Comment) -->
	rl_dump__write_output_rel(RelationInfo, Output),
	io__write_string(" = add_index("),
	rl_dump__write_relation_id(RelationInfo, Input),
	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, RelationInfo,
		clear(RelationId) - Comment) -->
	io__write_string("clear("),
	rl_dump__write_relation_id(RelationInfo, RelationId),
	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, RelationInfo,
		unset(RelationId) - Comment) -->
	io__write_string("unset("),
	rl_dump__write_relation_id(RelationInfo, RelationId),
	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, _RelationInfo,
		label(LabelId) - Comment) -->
	io__write_string("\nlabel("),
	{ rl__label_id_to_string(LabelId, String) },
	io__write_string(String),
	io__write_string(")\n"),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, RelationInfo,
		conditional_goto(Cond, LabelId) - Comment) -->
	io__write_string("conditional_goto("),
	rl_dump__write_goto_cond(RelationInfo, Cond),
	comma,
	{ rl__label_id_to_string(LabelId, String) },
	io__write_string(String),
	io__write_string(").\n"),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, _, goto(LabelId) - Comment) -->
	io__write_string("goto("),
	{ rl__label_id_to_string(LabelId, String) },
	io__write_string(String),
	io__write_string(").\n"),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, RelationInfo,
		ref(Output, Input) - Comment) -->
	rl_dump__write_relation_id(RelationInfo, Output),
	io__write_string(" = "),
	rl_dump__write_relation_id(RelationInfo, Input),
	io__write_string("."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, RelationInfo,
		copy(Output, Input) - Comment) -->
	rl_dump__write_output_rel(RelationInfo, Output),
	io__write_string(" = copy("),
	rl_dump__write_relation_id(RelationInfo, Input),
	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, RelationInfo,
		make_unique(Output, Input) - Comment) -->
	rl_dump__write_output_rel(RelationInfo, Output),
	io__write_string(" = make_unique("),
	rl_dump__write_relation_id(RelationInfo, Input),
	io__write_string(")."),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_ModuleInfo, RelationInfo,
		call(Name, InputArgs, OutputArgs, Saved) - Comment) -->
	io__write_string("call("),
	{ rl__proc_name_to_string(Name, NameStr) },
	io__write_string(NameStr),
	comma,
	rl_dump__write_list(rl_dump__write_relation_id(RelationInfo),
		InputArgs),
	comma,
	rl_dump__write_list(rl_dump__write_output_rel(RelationInfo),
		OutputArgs),
	comma,
	{ set__to_sorted_list(Saved, SavedList) },
	rl_dump__write_list(rl_dump__write_relation_id(RelationInfo),
		SavedList),

	io__write_string(").\n\t"),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(ModuleInfo, RelationInfo,
		aggregate(Output, Input, Initial, Update) - Comment) -->
	rl_dump__write_output_rel(RelationInfo, Output),
	io__write_string(" = aggregate("),
	rl_dump__write_relation_id(RelationInfo, Input),
	comma,
	io__nl,
	io__write_string("\t"),
	{ Initial = proc(IPredId, IProcId) },
	hlds_out__write_pred_proc_id(ModuleInfo, IPredId, IProcId),
	comma,
	{ Update = proc(UPredId, UProcId) },
	hlds_out__write_pred_proc_id(ModuleInfo, UPredId, UProcId),
	io__write_string(").\n"),
	rl_dump__write_comment(Comment),
	io__nl.

rl_dump__write_instruction(_, _, comment - Comment) -->
	io__write_strings(["\n%\n% ", Comment, "\n%\n\n"]),
	io__nl.

%-----------------------------------------------------------------------------%

:- pred rl_dump__write_join_type(module_info::in, join_type::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_join_type(_, nested_loop) -->
	io__write_string("nested_loop").
rl_dump__write_join_type(_, hash(Attrs1, Attrs2)) -->
	io__write_string("hash("),
	rl_dump__write_list(io__write, ", ", Attrs1),
	io__write_string(", "),
	rl_dump__write_list(io__write, ", ", Attrs2),
	io__write_string(")").
rl_dump__write_join_type(_, sort_merge(Attr1, Attr2)) -->
	io__write_string("sort_merge("),
	rl_dump__write_sort_spec(Attr1),
	comma,
	rl_dump__write_sort_spec(Attr2),
	io__write_string(")").
rl_dump__write_join_type(ModuleInfo, index(Spec, Range)) -->
	io__write_string("index("),
	mercury_output_index_spec(Spec),
	comma,
	rl_dump__write_key_range(ModuleInfo, Range),
	io__write_string(")").

:- pred rl_dump__write_subtract_type(module_info::in, subtract_type::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_subtract_type(_, semi_nested_loop) -->
	io__write_string("semi_nested_loop").
rl_dump__write_subtract_type(_, semi_sort_merge(SortAttr1, SortAttr2)) -->
	io__write_string("semi_sort_merge("),
	rl_dump__write_sort_spec(SortAttr1),
	comma,
	rl_dump__write_sort_spec(SortAttr2),
	io__write_string(")").
rl_dump__write_subtract_type(_, semi_hash(Attrs1, Attrs2)) -->
	io__write_string("semi_hash("),
	rl_dump__write_list(io__write, ", ", Attrs1),
	io__write_string(", "),
	rl_dump__write_list(io__write, ", ", Attrs2),
	io__write_string(")").
rl_dump__write_subtract_type(ModuleInfo, semi_index(Spec, Range)) -->
	io__write_string("semi_index("),
	mercury_output_index_spec(Spec),
	comma,
	rl_dump__write_key_range(ModuleInfo, Range),
	io__write_string(")").

:- pred rl_dump__write_difference_type(module_info::in, difference_type::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_difference_type(_, sort_merge(SortAttr)) -->
	io__write_string("sort_merge("),
	rl_dump__write_sort_spec(SortAttr),
	io__write_string(")").

:- pred rl_dump__write_project_type(module_info::in, project_type::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_project_type(_, filter) -->
	io__write_string("filter").
rl_dump__write_project_type(ModuleInfo, index(Spec, Range)) -->
	io__write_string("index("),
	mercury_output_index_spec(Spec),
	comma,
	rl_dump__write_key_range(ModuleInfo, Range),
	io__write_string(")").

:- pred rl_dump__write_union_type(module_info::in, union_type::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_union_type(_, sort_merge(SortAttr)) -->
	io__write_string("sort_merge("),
	rl_dump__write_sort_spec(SortAttr),
	io__write_string(")").

:- pred rl_dump__write_insert_type(module_info::in, insert_type::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_insert_type(_, append) -->
	io__write_string("append").
rl_dump__write_insert_type(_ModuleInfo, index(IndexSpec)) -->
	io__write_string("index("),
	mercury_output_index_spec(IndexSpec),
	io__write_string(")").

:- pred rl_dump__write_trivial_join_or_subtract(
		maybe(trivial_join_or_subtract_info)::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_trivial_join_or_subtract(TrivialJoinInfo) -->
	(
		{ TrivialJoinInfo = yes(trivial_join_or_subtract_info(Tuple,
					MaybeProject)) },
		io__write_string(" (trivial input "),
		io__write(Tuple),
		(
			{ MaybeProject = yes(_) },
			io__write_string(" projected")
		;
			{ MaybeProject = no },
			io__write_string(" not projected")
		),
		io__write_string(")")
	;
		{ TrivialJoinInfo = no }
	).

%-----------------------------------------------------------------------------%

:- pred rl_dump__write_key_range(module_info::in, key_range::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_key_range(ModuleInfo, key_range(LBound, UBound, _, _)) -->
	io__write_string("key_range("),
	rl_dump__write_key_tuple(ModuleInfo, LBound),
	comma,
	rl_dump__write_key_tuple(ModuleInfo, UBound),
	io__write_string(")").

:- pred rl_dump__write_key_tuple(module_info::in,
		bounding_tuple::in, io__state::di, io__state::uo) is det.

rl_dump__write_key_tuple(_, infinity) -->
	io__write_string("infinity").
rl_dump__write_key_tuple(_ModuleInfo, bound(Attrs)) -->
	io__write_string("["),
	io__write_list(Attrs, ", ", rl_dump__write_key_attr_pair),
	io__write_string("]").

:- pred rl_dump__write_key_attr_pair(pair(int, key_attr)::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_key_attr_pair(Index - Attr) -->
	io__write_int(Index),
	io__write_string(" - "),
	rl_dump__write_key_attr(Attr).

:- pred rl_dump__write_key_attr(key_attr::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_key_attr(Attr) -->
	{ Attr = infinity },
	io__write(Attr).
rl_dump__write_key_attr(Attr) -->
	{ Attr = input_field(_) },
	io__write(Attr).
rl_dump__write_key_attr(functor(ConsId, _, Args)) -->
	hlds_out__write_cons_id(ConsId),
	io__write_string("("),
	io__write_list(Args, ", ", rl_dump__write_key_attr),
	io__write_string(")").

%-----------------------------------------------------------------------------%

:- pred rl_dump__write_sort_spec(sort_spec::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_sort_spec(attributes(Attrs)) -->
	rl_dump__write_sort_attr_list(Attrs).
rl_dump__write_sort_spec(sort_var(Var)) -->
	io__write_string("sort_var("),
	io__write_int(Var),
	io__write_string(")").

:- pred rl_dump__write_sort_attr_list(sort_attrs::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_sort_attr_list(Attrs) -->
	rl_dump__write_list(io__write, ", ", Attrs).

%-----------------------------------------------------------------------------%

:- pred rl_dump__write_goto_cond(map(relation_id, relation_info)::in,
		goto_cond::in, io__state::di, io__state::uo) is det.

rl_dump__write_goto_cond(RelationInfo, empty(RelationId)) -->
	io__write_string("empty("),
	rl_dump__write_relation_id(RelationInfo, RelationId),
	io__write_string(")").
rl_dump__write_goto_cond(RelationInfo, and(Cond1, Cond2)) -->
	rl_dump__write_goto_cond(RelationInfo, Cond1),
	comma,
	rl_dump__write_goto_cond(RelationInfo, Cond2).
rl_dump__write_goto_cond(RelationInfo, or(Cond1, Cond2)) -->
	rl_dump__write_goto_cond(RelationInfo, Cond1),
	io__write_string(" ; "),
	rl_dump__write_goto_cond(RelationInfo, Cond2).
rl_dump__write_goto_cond(RelationInfo, not(Cond)) -->
	io__write_string("not("),
	rl_dump__write_goto_cond(RelationInfo, Cond),
	io__write_string(")").

%-----------------------------------------------------------------------------%

:- pred rl_dump__write_comment(string::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_comment(Comment) -->
	( { Comment = "" } ->
		[]
	;
		io__write_string("% "),
		io__write_string(Comment)
	).

%-----------------------------------------------------------------------------%

:- pred rl_dump__write_output_rel(map(relation_id, relation_info)::in,
		output_rel::in, io__state::di, io__state::uo) is det.

rl_dump__write_output_rel(RelationInfo, output_rel(RelationId, Indexes)) -->
	rl_dump__write_relation_id(RelationInfo, RelationId),
	( { Indexes = [] } ->
		[]
	;
		rl_dump__write_list(mercury_output_index_spec, ", ", Indexes)
	).

:- pred rl_dump__write_relation_id(map(relation_id, relation_info)::in,
		relation_id::in, io__state::di, io__state::uo) is det.

rl_dump__write_relation_id(_, RelationId) -->
	{ rl__relation_id_to_string(RelationId, RelationIdStr) },
	io__write_string(RelationIdStr).

:- pred rl_dump__verbose_write_relation_id(
		map(relation_id, relation_info)::in,
		relation_id::in, io__state::di, io__state::uo) is det.

rl_dump__verbose_write_relation_id(RelationInfo, RelationId) -->
	{ map__lookup(RelationInfo, RelationId, RelInfo) },
	{ RelInfo = relation_info(_, _, _, Name) },
	io__write_string(Name).

%-----------------------------------------------------------------------------%

:- pred rl_dump__write_project_output(module_info::in,
	map(relation_id, relation_info)::in, pair(output_rel, rl_goal)::in,
	io__state::di, io__state::uo) is det.

rl_dump__write_project_output(ModuleInfo, RelationInfo,
		Relation - RelationExprn) -->
	rl_dump__write_output_rel(RelationInfo, Relation),
	io__write_string(" <- "),
	io__nl,
	rl_dump__write_goal(ModuleInfo, RelationExprn),
	io__nl.

%-----------------------------------------------------------------------------%

:- pred rl_dump__write_goal(module_info::in, rl_goal::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_goal(ModuleInfo, RLGoal) -->
	{ RLGoal = rl_goal(_, VarSet, _, _, Inputs,
			MaybeOutputs, GoalList, Bounds) },
	(
		{ Inputs = no_inputs }
	;
		{ Inputs = one_input(InputVars) },
		io__write_string("\tinputs: "),
		rl_dump__write_var_list(VarSet, InputVars),
		io__write_string("\n\t"),
		( { Bounds \= [] } ->
			rl_dump__write_list(
				rl_dump__write_bounds(ModuleInfo, VarSet),
				Bounds)
		;
			[]
		)
	;
		{ Inputs = two_inputs(Inputs1, Inputs2) },
		io__write_string("\tinputs: "),
		rl_dump__write_var_list(VarSet, Inputs1),
		io__write_string(" "),
		rl_dump__write_var_list(VarSet, Inputs2),
		io__write_string("\n\t"),
		( { Bounds \= [] } ->
			rl_dump__write_list(
				rl_dump__write_bounds(ModuleInfo, VarSet),
				Bounds)
		;
			[]
		)
	),
	io__write_string("\n"),
	( { MaybeOutputs = yes(Outputs) } ->
		io__write_string("\toutputs: "),
		rl_dump__write_var_list(VarSet, Outputs),
		io__nl
	;
		[]
	),
	hlds_out__write_goal_list(GoalList, ModuleInfo,
		VarSet, yes, 2, ",\n", no).

:- pred rl_dump__write_bounds(module_info::in, prog_varset::in,
		rl_var_bounds::in, io__state::di, io__state::uo) is det.

rl_dump__write_bounds(ModuleInfo, VarSet, Bounds) -->
	{ map__to_assoc_list(Bounds, BoundList) },
	io__write_string("\n\t["),
	io__write_list(BoundList, "\n\t",
		rl_dump__write_bound_pair(ModuleInfo, VarSet)),
	io__write_string("\n\t]"),
	io__nl.

:- pred rl_dump__write_bound_pair(module_info::in, prog_varset::in,
		pair(prog_var, pair(key_term))::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_bound_pair(ModuleInfo, VarSet,
		Var - (LowerBound - UpperBound)) -->
	mercury_output_var(Var, VarSet, yes),
	io__write_string(" -> "),
	rl_dump__write_key_term(ModuleInfo, VarSet, LowerBound),
	io__write_string(" - "),
	rl_dump__write_key_term(ModuleInfo, VarSet, UpperBound).

:- pred rl_dump__write_key_term(module_info::in, prog_varset::in, key_term::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_key_term(_ModuleInfo, VarSet, var - Vars0) -->
	{ set__to_sorted_list(Vars0, Vars) },
	( { Vars = [] } ->
		io__write_string("infinity")
	;
		io__write_string("var"),
		rl_dump__write_var_list(VarSet, Vars)
	).
rl_dump__write_key_term(ModuleInfo, VarSet,
		functor(ConsId, _, Args) - Vars0) -->
	hlds_out__write_cons_id(ConsId),
	io__write_string("("),
	io__write_list(Args, ", ",
		rl_dump__write_key_term(ModuleInfo, VarSet)),
	io__write_string(")"),
	{ set__to_sorted_list(Vars0, Vars) },
	rl_dump__write_var_list(VarSet, Vars).

%-----------------------------------------------------------------------------%

:- pred rl_dump__write_var_list(prog_varset::in, list(prog_var)::in,
		io__state::di, io__state::uo) is det.

rl_dump__write_var_list(VarSet, Vars) -->
        { PrintVar = (pred(Var::in, IO0::di, IO::uo) is det :-
		mercury_output_var(Var, VarSet, yes, IO0, IO)
	) },
	rl_dump__write_list(PrintVar, Vars).

:- pred rl_dump__write_list(pred(T, io__state, io__state), list(T),
		io__state, io__state).
:- mode rl_dump__write_list(pred(in, di, uo) is det, in, di, uo) is det.

rl_dump__write_list(Writer, List) -->
	rl_dump__write_list(Writer, ", ", List).

:- pred rl_dump__write_list(pred(T, io__state, io__state), string, list(T),
		io__state, io__state).
:- mode rl_dump__write_list(pred(in, di, uo) is det, in, in, di, uo) is det.

rl_dump__write_list(Writer, Sep, List) -->
	io__write_string("["),
	io__write_list(List, Sep, Writer),
	io__write_string("]").

:- pred comma(io__state::di, io__state::uo) is det.

comma -->	io__write_string(", ").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
