%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_exprn.m
% Main author: stayl
%
% This module should only be imported by rl_out.m. XXX make it a sub-module.
% 
% Generate RL "expressions" to evaluate join conditions.
% 
% The code generated here is pretty awful. Each variable used in the
% expression is assigned its own register. All calls are generated inline -
% recursive calls and calls to imported predicates result in an abort.
% Unifications are generated eagerly.
%
% For complicated join conditions (for example anything containing
% calls to non-builtin predicates) we will probably generate Mercury bytecode,
% when the interpreter is done.
%
% Expressions are arranged into fragments. Each fragment consists of
% rl_PROC_expr_frag(N) followed by rl_EXP_* bytecodes to implement the
% fragment. Jumps addresses start at zero at the first instruction following
% the rl_PROC_expr_frag.
% 0 - initialisation: run once before anything else. Used to initialise
% 	the rule numbers (see below).
% 1 - group initialisation: has access to the first tuple in an aggregate group
% 2 - test - returns either zero/non-zero or -1/0/1 as for strcmp,
% 	depending on the operation.
% 3 - project - constructs an output tuple.
% 4 - cleanup - currently not used.
%
% Expressions have their own constant table separate from the procedure
% constant table. This is set up using rl_HEAD_const_* bytecodes before
% any fragments.
%
% Each expression has zero, one or two input tuples, a tuple to store
% local variables and zero, one or two output tuples.
%
% Expressions also need to set up rule numbers to identify data constructors.
% This is done with rl_EXP_define_var_rule(RuleNo, TypeIndex, NameIndex, Arity)
% (`var' refers to the schema of the tuple holding the local expression
% variables). TypeIndex and NameIndex are indices into the expression's
% constant table holding the type name and constructor name. `RuleNo' is
% used for bytecodes such as rl_EXP_test_functor and rl_EXP_construct_term
% to specify which constructor to use.
%
%-----------------------------------------------------------------------------%
:- module rl_exprn.

:- interface.

:- import_module hlds_module, hlds_pred, rl, rl_code, rl_file, prog_data.
:- import_module list.

	% rl_exprn__generate_compare_exprn(ModuleInfo, SortSpec,
	% 	InputSchema, CompareCodes).
	%
	% Generate an expression to compare tuples with the
	% given schema on the given attributes.
:- pred rl_exprn__generate_compare_exprn(module_info::in, sort_spec::in,
	list(type)::in, list(bytecode)::out) is det.

	% rl_exprn__generate_sort_merge_compare_exprn(ModuleInfo, Attrs1,
	% 	Schema1, Attrs2, Schema2, CompareCodes).
	%
	% Generate an expression to compare the join attributes in
	% a sort-merge equi-join.
:- pred rl_exprn__generate_sort_merge_compare_exprn(module_info::in,
	sort_spec::in, list(type)::in, sort_spec::in, list(type)::in,
	list(bytecode)::out) is det.

	% rl_exprn__generate_equijoin_exprn(ModuleInfo, Attrs,
	% 	Schema, Code)
	%
	% Generate an expression to compare the join attributes in
	% an equi-join.
:- pred rl_exprn__generate_equijoin_exprn(module_info::in, list(int)::in,
	list(type)::in, list(bytecode)::out) is det.
	
	% rl_exprn__generate_hash_function(ModuleInfo, HashAttrs,
	%	InputSchema, ExprnCode).
	%
	% Generate an expression to compute a hash value for the given
	% attributes of a tuple.
:- pred rl_exprn__generate_hash_function(module_info::in, list(int)::in,
	list(type)::in, list(bytecode)::out) is det.

	% rl_exprn__generate_key_range(ModuleInfo, KeyRange, ExprnCode,
	% 	NumParams, LowerBoundSchema, UpperBoundSchema,
	%	MaxTermDepth, ExprnVarTypes).
	%
	% Generate an expression to produce the upper and lower
	% bounds for a B-tree access.
:- pred rl_exprn__generate_key_range(module_info::in, key_range::in,
	list(bytecode)::out, int::out, list(type)::out, list(type)::out,
	int::out, list(type)::out) is det.

	% Generate an expression to produce either the tuple
	% to insert or the tuple to delete for a modification
	% query.
:- pred rl_exprn__generate_modify_project_exprn(module_info::in,
	tuple_num::in, list(type)::in, list(bytecode)::out) is det.

	% rl_exprn__generate(ModuleInfo, Goal, ExprnCode, NumParams,
	%	ExprnMode, ExprnVarTypes).
	%
	% Generate an expression for a join/project/subtract condition.
:- pred rl_exprn__generate(module_info::in, rl_goal::in, list(bytecode)::out,
	int::out, exprn_mode::out, list(type)::out) is det.

	% rl_exprn__aggregate(ModuleInfo, InitAccPred, UpdateAccPred,
	% 	GrpByType, NonGrpByType, AccType, ExprnCode, Decls).	
	%
	% Given the closures used to create the initial accumulator for each
	% group and update the accumulator for each tuple, create
	% an expression to evaluate the aggregate.
:- pred rl_exprn__aggregate(module_info::in, pred_proc_id::in,
		pred_proc_id::in, (type)::in, (type)::in, (type)::in,
		list(bytecode)::out, list(type)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module code_util, hlds_pred, hlds_data, inst_match.
:- import_module instmap, mode_util, tree, type_util, prog_out.
:- import_module rl_out, inlining, hlds_goal, prog_util, error_util.
:- import_module builtin_ops.

:- import_module assoc_list, bool, char, int, map.
:- import_module require, set, std_util, string, term, varset.

	% A compare expression tests each attribute in a list of attributes
	% in turn.
rl_exprn__generate_compare_exprn(_ModuleInfo, Spec, Schema, Code) :-
	(
		Spec = attributes(Attrs0),

		% We're comparing corresponding attributes from each tuple.
		assoc_list__from_corresponding_lists(Attrs0, Attrs0,
			CompareAttrs)
	;
		Spec = sort_var(_),
		error("rl_exprn__generate_compare_exprn: unbound sort_var")
	),
	rl_exprn__do_generate_compare_exprn(Schema, CompareAttrs, Code).

rl_exprn__generate_sort_merge_compare_exprn(_ModuleInfo, Spec1, Schema1,
		Spec2, _Schema2, Code) :-

	(
		Spec1 = attributes(Attrs1),
		Spec2 = attributes(Attrs2)
	->
		assoc_list__from_corresponding_lists(Attrs1, Attrs2,
			CompareAttrs)
	;
		error(
	"rl_exprn__generate_sort_merge_compare_exprn: unbound sort_var")
	),
	rl_exprn__do_generate_compare_exprn(Schema1, CompareAttrs, Code).

:- pred rl_exprn__do_generate_compare_exprn(list(type)::in, 
	assoc_list(pair(int, sort_dir))::in, list(bytecode)::out) is det.

rl_exprn__do_generate_compare_exprn(Schema1, CompareAttrs, Code) :-

	list__foldl(rl_exprn__generate_compare_instrs(Schema1),
		CompareAttrs, empty, CompareCode),

	ExprnCode = 
		tree(node([rl_PROC_expr_frag(2)]),
		tree(CompareCode,
		node([
			rl_EXP_int_immed(0), % return equal
			rl_EXP_int_result,
			rl_PROC_expr_end
		])
	)),

	tree__flatten(ExprnCode, Instrs0),
	list__condense(Instrs0, Code).

:- pred rl_exprn__generate_compare_instrs(list(type)::in,
	pair(pair(int, sort_dir))::in, byte_tree::in, byte_tree::out) is det.

rl_exprn__generate_compare_instrs(Types1, (Attr1a - Dir1) - (Attr2a - Dir2),
		Code0, Code) :-
	require(unify(Dir1, Dir2),
	    "rl_exprn__generate_compare_instrs: sort directions not equal"),

	rl_exprn__adjust_arg_number(Attr1a, Attr1),
	rl_exprn__adjust_arg_number(Attr2a, Attr2),

	list__index0_det(Types1, Attr1, Type),
	rl_exprn__type_to_aditi_type(Type, AType),
	rl_exprn__compare_bytecode(AType, CompareByteCode),
	rl_exprn__get_input_field_code(one, AType, Attr1, FieldCode1),
	rl_exprn__get_input_field_code(two, AType, Attr2, FieldCode2),
	(
		Dir1 = ascending,
		CompareAttr = node([
				FieldCode1,
				FieldCode2,
				CompareByteCode,
				rl_EXP_return_if_nez
			])
	;
		Dir1 = descending,
		CompareAttr = node([
				FieldCode2,
				FieldCode1,
				CompareByteCode,
				rl_EXP_return_if_nez
			])
	),
	Code = tree(Code0, CompareAttr).

%-----------------------------------------------------------------------------%

rl_exprn__generate_equijoin_exprn(_, Attrs0, Schema, Code) :-
	list__map(rl_exprn__adjust_arg_number, Attrs0, Attrs),
	rl_exprn__generate_equijoin_instrs(Attrs, Schema,
		empty, TestCode),
	ExprnCode =
		tree(node([rl_PROC_expr_frag(2)]),
		tree(TestCode,
		node([rl_PROC_expr_end])
	)),

	tree__flatten(ExprnCode, Instrs0),
	list__condense(Instrs0, Code).

:- pred rl_exprn__generate_equijoin_instrs(list(int)::in, list(type)::in,
		byte_tree::in, byte_tree::out) is det.

rl_exprn__generate_equijoin_instrs([], _, Code, Code).
rl_exprn__generate_equijoin_instrs([Attr | Attrs], Schema, Code0, Code) :-
	list__index0_det(Schema, Attr, AttrType),
	rl_exprn__type_to_aditi_type(AttrType, AType),
	rl_exprn__test_bytecode(AType, TestBytecode),
	rl_exprn__get_input_field_code(one, AType, Attr, FieldCode1),
	rl_exprn__get_input_field_code(two, AType, Attr, FieldCode2),
	Code1 =
		tree(Code0,
		node([
			FieldCode1,
			FieldCode2,
			TestBytecode,
			rl_EXP_fail_if_false
		])
	),
	rl_exprn__generate_equijoin_instrs(Attrs, Schema, Code1, Code).

%-----------------------------------------------------------------------------%

rl_exprn__generate_hash_function(_ModuleInfo, Attrs0, Schema, Code) :-
	list__map(rl_exprn__adjust_arg_number, Attrs0, Attrs),
	IsFirst = yes,
	rl_exprn__generate_hash_function_2(Attrs, Schema, IsFirst,
		empty, HashCode),
	ExprnCode =
		tree(node([rl_PROC_expr_frag(2)]),
		tree(HashCode,
		node([rl_EXP_hash_result, rl_PROC_expr_end])
	)),

	tree__flatten(ExprnCode, Instrs0),
	list__condense(Instrs0, Code).

:- pred rl_exprn__generate_hash_function_2(list(int)::in, list(type)::in,
		bool::in, byte_tree::in, byte_tree::out) is det.

rl_exprn__generate_hash_function_2([], _, _, Code, Code).
rl_exprn__generate_hash_function_2([Attr | Attrs], Schema,
		IsFirst, Code0, Code) :-
	list__index0_det(Schema, Attr, Type),
	rl_exprn__type_to_aditi_type(Type, AType),
	rl_exprn__hash_bytecode(AType, HashCode),
	rl_exprn__get_input_field_code(one, AType, Attr, FieldCode),
	( IsFirst = no ->
		CombineCode = node([rl_EXP_hash_combine])
	;
		CombineCode = empty
	),

	Code1 =
		tree(Code0,
		tree(node([FieldCode, HashCode]),
		CombineCode
	)),
	IsFirst1 = no,
	rl_exprn__generate_hash_function_2(Attrs, Schema, IsFirst1,
		Code1, Code).

:- pred rl_exprn__hash_bytecode(aditi_type::in, bytecode::out) is det.

rl_exprn__hash_bytecode(int, rl_EXP_int_hash).
rl_exprn__hash_bytecode(string, rl_EXP_str_hash).
rl_exprn__hash_bytecode(float, rl_EXP_flt_hash).
rl_exprn__hash_bytecode(term(_), rl_EXP_term_hash).

%-----------------------------------------------------------------------------%

	% The compiler numbers arguments starting at 1, Aditi numbers
	% arguments starting at 0.
:- pred rl_exprn__adjust_arg_number(int::in, int::out) is det.

rl_exprn__adjust_arg_number(Attr, Attr - 1).

%-----------------------------------------------------------------------------%

rl_exprn__generate_key_range(ModuleInfo,
		key_range(LowerBound, UpperBound, MaybeArgTypes, KeyTypes),
		Code, NumParams, Output1Schema, Output2Schema,
		MaxDepth, Decls) :-
	( MaybeArgTypes = yes(_) ->
		NumParams = 1
	;
		NumParams = 0
	),
	rl_exprn_info_init(ModuleInfo, Info0),
	% Generate code to produce the lower bound term.
	rl_exprn__generate_bound(ModuleInfo, MaybeArgTypes, KeyTypes,
		one, LowerBound, LowerBoundCode, Output1Schema,
		MaxDepth0, Info0, Info1),
	% Generate code to produce the upper bound term.
	rl_exprn__generate_bound(ModuleInfo, MaybeArgTypes, KeyTypes,
		two, UpperBound, UpperBoundCode, Output2Schema,
		MaxDepth1, Info1, Info2),
	ProjectCode = tree(LowerBoundCode, UpperBoundCode),
	int__max(MaxDepth0, MaxDepth1, MaxDepth),

	rl_exprn__generate_decls(ConstCode, InitCode, Decls, Info2, _Info),

	rl_exprn__generate_fragments(ConstCode, InitCode, empty,
		empty, ProjectCode, empty, Code).

:- pred rl_exprn__generate_bound(module_info::in, maybe(list(type))::in,
	list(type)::in, tuple_num::in, bounding_tuple::in, byte_tree::out,
	list(type)::out, int::out, rl_exprn_info::in,
	rl_exprn_info::out) is det.

	% An output schema of [] signals to the relational operation that
	% that end of the key range has no bound (it doesn't make sense
	% to have a key with no attributes).
rl_exprn__generate_bound(_, _, _, _, infinity, empty, [], 0) --> [].
rl_exprn__generate_bound(ModuleInfo, MaybeArgTypes, KeyTypes,
		TupleNum, bound(Attrs), Code, KeyTypes, MaxDepth) -->
	{ assoc_list__values(Attrs, AttrValues) },
	rl_exprn__generate_bound_2(ModuleInfo, MaybeArgTypes,
		TupleNum, no, AttrValues, empty, Code, 0, 1, MaxDepth).

:- pred rl_exprn__generate_bound_2(module_info::in, maybe(list(type))::in,
	tuple_num::in, bool::in, list(key_attr)::in, byte_tree::in,
	byte_tree::out, int::in, int::in, int::out, rl_exprn_info::in,
	rl_exprn_info::out) is det.

rl_exprn__generate_bound_2(_, _, _, _, [], Code, Code,
		_, MaxDepth, MaxDepth) --> [].
rl_exprn__generate_bound_2(ModuleInfo, MaybeArgTypes, TupleNum, IsSubTerm,
		[Attr | Attrs], Code0, Code, Index0, MaxDepth0, MaxDepth) -->
	rl_exprn__generate_bound_3(ModuleInfo, MaybeArgTypes, IsSubTerm,
		Index0, TupleNum, Attr, AttrCode, Depth),
	{ int__max(MaxDepth0, Depth, MaxDepth1) },
	{ Index is Index0 + 1 },
	rl_exprn__generate_bound_2(ModuleInfo, MaybeArgTypes, TupleNum,
		IsSubTerm, Attrs, tree(Code0, AttrCode),
		Code, Index, MaxDepth1, MaxDepth).

:- pred rl_exprn__generate_bound_3(module_info::in, maybe(list(type))::in,
	bool::in, int::in, tuple_num::in, key_attr::in, byte_tree::out,
	int::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__generate_bound_3(_, _, _, _, _, infinity, _, _, _, _) :-
	% Eventually the B-tree lookup code will be able to handle this case.
	% For now we have to be careful not to generate it in rl_key.m.
	error("rl_exprn__generate_bound_3: embedded infinities NYI").

rl_exprn__generate_bound_3(_ModuleInfo, MaybeArgTypes, IsSubTerm, FieldNum,
		TupleNum, input_field(InputFieldNum0), Code, 1, Info, Info) :-
	rl_exprn__adjust_arg_number(InputFieldNum0, InputFieldNum),
	rl_exprn__get_key_arg(MaybeArgTypes, InputFieldNum, FieldType0),
	rl_exprn__type_to_aditi_type(FieldType0, FieldType),
	rl_exprn__get_input_field_code(one, FieldType, InputFieldNum, GetCode),
	(
		IsSubTerm = yes,
		rl_exprn__set_term_arg_code(FieldType, FieldNum, PutCode)
	;
		IsSubTerm = no,
		rl_exprn__set_output_field_code(TupleNum, FieldType,
			FieldNum, PutCode)
	),
	Code = node([GetCode, PutCode]).
		
rl_exprn__generate_bound_3(ModuleInfo, MaybeArgTypes, IsSubTerm, FieldNum,
		TupleNum, functor(ConsId, Type, Attrs), Code, Depth) -->
	rl_exprn__set_term_arg_cons_id_code(ConsId, Type, TupleNum,
		FieldNum, IsSubTerm, CreateTerm, NeedPop),
	rl_exprn__generate_bound_2(ModuleInfo, MaybeArgTypes, TupleNum, yes,
		Attrs, node(CreateTerm), Code0, 0, 1, Depth0),
	{ NeedPop = yes ->
		Code = tree(Code0, node([rl_EXP_term_pop]))
	;
		Code = Code0
	},
	{ Depth is Depth0 + 1 }.

:- pred rl_exprn__get_key_arg(maybe(list(T))::in, int::in, T::out) is det.

rl_exprn__get_key_arg(yes(Args), Index, Arg) :-
	list__index0_det(Args, Index, Arg).
rl_exprn__get_key_arg(no, _, _) :-
	error("rl_exprn__get_key_arg").

:- pred rl_exprn__set_term_arg_cons_id_code(cons_id::in, (type)::in,
	tuple_num::in, int::in, bool::in, list(bytecode)::out, bool::out,
	rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__set_term_arg_cons_id_code(cons(SymName, Arity), Type, TupleNum,
		FieldNum, IsSubTerm, Code, NeedPop) -->
	( { rl_exprn__is_char_cons_id(cons(SymName, Arity), Type, Int) } ->
		rl_exprn__set_term_arg_cons_id_code(int_const(Int), Type,
			TupleNum, FieldNum, IsSubTerm, Code, NeedPop)
	;
		{
			TupleNum = one,
			ExprnTuple = output1
		;
			TupleNum = two,
			ExprnTuple = output2
		},
		rl_exprn__cons_id_to_rule_number(cons(SymName, Arity), Type,
			ExprnTuple, Rule),
		{
			IsSubTerm = no,
			(
				TupleNum = one,
				Code = [rl_EXP_new_term_output1(FieldNum,
					Rule)]
			;
				TupleNum = two,
				Code = [rl_EXP_new_term_output2(FieldNum,
					Rule)]
			),
			NeedPop = no
		;
			IsSubTerm = yes,
			Code = [
				rl_EXP_term_dup,
				rl_EXP_set_term_arg(FieldNum, Rule)
			],
			NeedPop = yes
		}
	).
rl_exprn__set_term_arg_cons_id_code(int_const(Int), _, TupleNum, FieldNum,
		IsSubTerm, Code, no) -->
	rl_exprn_info_lookup_const(int(Int), Index),
	{ rl_exprn__set_term_arg_cons_id_code_2(int, TupleNum,
		FieldNum, IsSubTerm, SetArgCode) },
	{ Code0 = [rl_EXP_int_push(Index), SetArgCode] },
	{ IsSubTerm = yes ->
		Code = [rl_EXP_term_dup | Code0]
	;
		Code = Code0
	}.
rl_exprn__set_term_arg_cons_id_code(float_const(Float), _, TupleNum, FieldNum,
		IsSubTerm, Code, no) -->
	rl_exprn_info_lookup_const(float(Float), Index),
	{ rl_exprn__set_term_arg_cons_id_code_2(float, TupleNum,
		FieldNum, IsSubTerm, SetArgCode) },
	{ Code0 = [rl_EXP_flt_push(Index), SetArgCode] },
	{ IsSubTerm = yes ->
		Code = [rl_EXP_term_dup | Code0]
	;
		Code = Code0
	}.
rl_exprn__set_term_arg_cons_id_code(string_const(Str), _, TupleNum, FieldNum,
		IsSubTerm, Code, no) -->
	rl_exprn_info_lookup_const(string(Str), Index),
	{ rl_exprn__set_term_arg_cons_id_code_2(string, TupleNum,
		FieldNum, IsSubTerm, SetArgCode) },
	{ Code0 = [rl_EXP_str_push(Index), SetArgCode] },
	{ IsSubTerm = yes ->
		Code = [rl_EXP_term_dup | Code0]
	;
		Code = Code0
	}.
rl_exprn__set_term_arg_cons_id_code(pred_const(_, _, _), _, _, _, _, _, _) -->
	{ error("rl_exprn__set_term_arg_cons_id_code") }.
rl_exprn__set_term_arg_cons_id_code(code_addr_const(_, _),
		_, _, _, _, _, _) -->
	{ error("rl_exprn__set_term_arg_cons_id_code") }.
rl_exprn__set_term_arg_cons_id_code(type_ctor_info_const(_, _, _),
		_, _, _, _, _, _) -->
	{ error("rl_exprn__set_term_arg_cons_id_code") }.
rl_exprn__set_term_arg_cons_id_code(base_typeclass_info_const(_, _, _, _),
		_, _, _, _, _, _) -->
	{ error("rl_exprn__set_term_arg_cons_id_code") }.
rl_exprn__set_term_arg_cons_id_code(tabling_pointer_const(_, _),
		_, _, _, _, _, _) -->
	{ error("rl_exprn__set_term_arg_cons_id_code") }.
rl_exprn__set_term_arg_cons_id_code(deep_profiling_proc_static(_),
		_, _, _, _, _, _) -->
	{ error("rl_exprn__set_term_arg_cons_id_code") }.
rl_exprn__set_term_arg_cons_id_code(table_io_decl(_),
		_, _, _, _, _, _) -->
	{ error("rl_exprn__set_term_arg_cons_id_code") }.

:- pred rl_exprn__set_term_arg_cons_id_code_2(aditi_type::in, tuple_num::in,
		int::in, bool::in, bytecode::out) is det.

rl_exprn__set_term_arg_cons_id_code_2(int, one, FieldNum,
		no, rl_EXP_output1_int(FieldNum)).
rl_exprn__set_term_arg_cons_id_code_2(int, two, FieldNum,
		no, rl_EXP_output2_int(FieldNum)).
rl_exprn__set_term_arg_cons_id_code_2(int, _, FieldNum,
		yes, rl_EXP_set_int_arg(FieldNum)).
rl_exprn__set_term_arg_cons_id_code_2(float, one, FieldNum,
		no, rl_EXP_output1_flt(FieldNum)).
rl_exprn__set_term_arg_cons_id_code_2(float, two, FieldNum,
		no, rl_EXP_output2_flt(FieldNum)).
rl_exprn__set_term_arg_cons_id_code_2(float, _, FieldNum,
		yes, rl_EXP_set_flt_arg(FieldNum)).
rl_exprn__set_term_arg_cons_id_code_2(string, one, FieldNum,
		no, rl_EXP_output1_str(FieldNum)).
rl_exprn__set_term_arg_cons_id_code_2(string, two, FieldNum,
		no, rl_EXP_output2_str(FieldNum)).
rl_exprn__set_term_arg_cons_id_code_2(string, _, FieldNum,
		yes, rl_EXP_set_str_arg(FieldNum)).
rl_exprn__set_term_arg_cons_id_code_2(term(_), _, _, _, _) :-
	error("rl_exprn__set_term_arg_cons_id_code_2").

%-----------------------------------------------------------------------------%

rl_exprn__generate_modify_project_exprn(_ModuleInfo, TupleNum, Types, Codes) :-
	list__length(Types, NumAttrs),
	rl_exprn__generate_modify_project_exprn_2(Types,
		NumAttrs, TupleNum, 0, empty, ProjectCode),
	CodeTree =
		tree(node([rl_PROC_expr_frag(3)]),
		tree(ProjectCode,
		node([rl_PROC_expr_end])
	)),
	tree__flatten(CodeTree, CodeList),
	list__condense(CodeList, Codes).

:- pred rl_exprn__generate_modify_project_exprn_2(list(type)::in, int::in,
		tuple_num::in, int::in, byte_tree::in, byte_tree::out) is det.

rl_exprn__generate_modify_project_exprn_2([], _, _, _, Code, Code).
rl_exprn__generate_modify_project_exprn_2([Type | Types],
		NumAttrs, TupleNum, Attr, Code0, Code) :-
	rl_exprn__type_to_aditi_type(Type, AType),
	(
		TupleNum = one,
		InputAttr = Attr
	;
		TupleNum = two,
		InputAttr = Attr + NumAttrs
	),
	rl_exprn__get_input_field_code(one, AType, InputAttr, InputFieldCode),
	rl_exprn__set_output_field_code(one, AType, Attr, OutputFieldCode),
	Code1 =
		tree(Code0,
		node([InputFieldCode, OutputFieldCode])
	),
	rl_exprn__generate_modify_project_exprn_2(Types,
		NumAttrs, TupleNum, Attr + 1, Code1, Code).

%-----------------------------------------------------------------------------%

rl_exprn__generate(ModuleInfo, RLGoal, Code, NumParams, Mode, Decls) :-
	RLGoal = rl_goal(_, VarSet, VarTypes, InstMap,
		Inputs, MaybeOutputs, Goals, _), 
	rl_exprn_info_init(ModuleInfo, InstMap, VarTypes, VarSet, Info0),
	rl_exprn__generate_2(Inputs, MaybeOutputs, Goals,
		Code, NumParams, Mode, Decls, Info0, _).

:- pred rl_exprn__generate_2(rl_goal_inputs::in, rl_goal_outputs::in,
	list(hlds_goal)::in, list(bytecode)::out, int::out, exprn_mode::out,
	list(type)::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__generate_2(Inputs, MaybeOutputs, GoalList, 
		Code, NumParams, Mode, Decls) -->
	{ goal_list_determinism(GoalList, Detism) },
	{ determinism_components(Detism, CanFail, _) },
	{ goal_list_nonlocals(GoalList, NonLocals0) },
	{ MaybeOutputs = yes(OutputNonLocals) ->
		set__insert_list(NonLocals0, OutputNonLocals, NonLocals)
	;
		NonLocals = NonLocals0
	},
	( 
		{ Inputs = no_inputs },
		{ NumParams = 0 },
		{ InputCode = empty }
	;
		{ Inputs = one_input(InputVars) },
		{ NumParams = 1 },
		rl_exprn__deconstruct_input_tuple(one, 0, InputVars,
			NonLocals, InputCode)
	;
		{ Inputs = two_inputs(InputVars1, InputVars2) },
		{ NumParams = 2 },
		rl_exprn__deconstruct_input_tuple(one, 0,
			InputVars1, NonLocals, InputCode1),
		rl_exprn__deconstruct_input_tuple(two, 0,
			InputVars2, NonLocals, InputCode2),
		{ InputCode = tree(InputCode1, InputCode2) }
	),

	{ CanFail = can_fail ->
		Fail = node([rl_EXP_return_false])
	;
		% Should cause an error if it is encountered.
		Fail = node([rl_EXP_last_bytecode])
	},

	rl_exprn__goals(GoalList, Fail, GoalCode),

	( { MaybeOutputs = yes(OutputVars) } ->
		rl_exprn__construct_output_tuple(GoalList,
			OutputVars, OutputCode),
		{ Mode = generate }
	;
		{ OutputCode = empty },
		{ Mode = test }
	),

	{ 
		CanFail = can_fail,
		EvalCode =
			tree(InputCode,
			GoalCode
		),
		ProjectCode = OutputCode
	;
		CanFail = cannot_fail,
		% For projections, the eval fragment is not run.
		EvalCode = empty,
		ProjectCode0 =
			tree(InputCode,
			tree(GoalCode,
			OutputCode
		)),
		rl_exprn__resolve_addresses(ProjectCode0, ProjectCode)
	},

	% Need to do the init code last, since it also needs to define
	% the rule constants for the other fragments.
	rl_exprn__generate_decls(ConstCode, InitCode, Decls),

	{ rl_exprn__generate_fragments(ConstCode, InitCode,
		empty, EvalCode, ProjectCode, empty, Code) }.

:- pred rl_exprn__generate_fragments(byte_tree::in, byte_tree::in,
		byte_tree::in, byte_tree::in, byte_tree::in, byte_tree::in,
		list(bytecode)::out) is det.

rl_exprn__generate_fragments(DeclCode, InitCode, GroupInitCode,
		EvalCode, ProjectCode, CleanupCode, Code) :-
	list__foldl(
		(pred(FragAndCode::in, Tree0::in, Tree::out) is det :-
			FragAndCode = FragNo - FragCode0,
			( tree__tree_of_lists_is_empty(FragCode0) ->
				Tree = Tree0
			;
				rl_exprn__resolve_addresses(FragCode0,
					FragCode),
				Tree =
					tree(Tree0, 
					tree(node([rl_PROC_expr_frag(FragNo)]),
					FragCode
				))
			)
		),
		[0 - InitCode, 1 - GroupInitCode, 2 - EvalCode,
			3 - ProjectCode, 4 - CleanupCode],
		empty, FragmentsCode),

	CodeTree =
		tree(DeclCode,
		tree(FragmentsCode,
		node([rl_PROC_expr_end])
	)),
	tree__flatten(CodeTree, Code0),
	list__condense(Code0, Code).

:- pred rl_exprn__generate_decls(byte_tree::out, byte_tree::out,
		list(type)::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__generate_decls(node(ConstCode), node(RuleCodes), VarTypes) -->
	rl_exprn_info_get_rules(Rules - _),
	{ map__to_assoc_list(Rules, RulesAL) },
	{ assoc_list__reverse_members(RulesAL, RulesLA0) },
	{ list__sort(RulesLA0, RulesLA) },
	list__map_foldl(rl_exprn__generate_rule, RulesLA, RuleCodes),

	rl_exprn_info_get_consts(Consts - _),
	{ map__to_assoc_list(Consts, ConstsAL) },
	{ assoc_list__reverse_members(ConstsAL, ConstsLA0) },
	{ list__sort(ConstsLA0, ConstsLA) },
	{ list__map(rl_exprn__generate_const_decl, ConstsLA, ConstCode) },
	rl_exprn_info_get_decls(VarTypes).

:- pred rl_exprn__generate_const_decl(pair(int, rl_const)::in, 
		bytecode::out) is det.

rl_exprn__generate_const_decl(Addr - Const, Code) :-
	( 
		Const = int(Int),
		Code = rl_HEAD_const_int(Addr, Int)
	;
		Const = float(Float),
		Code = rl_HEAD_const_flt(Addr, Float)
	;
		Const = string(Str),
		Code = rl_HEAD_const_str(Addr, Str)
	).

:- pred rl_exprn__generate_rule(pair(int, pair(rl_rule, exprn_tuple))::in,			 bytecode::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__generate_rule(RuleNo - (Rule - RuleTuple), Code) -->
	{ Rule = rl_rule(Type, Name, Arity) },
	rl_exprn_info_lookup_const(string(Type), TypeIndex),
	rl_exprn_info_lookup_const(string(Name), NameIndex),
	{
		RuleTuple = input1,
		Code = rl_EXP_define_input1_rule(RuleNo,
			TypeIndex, NameIndex, Arity)
	;
		RuleTuple = input2,
		Code = rl_EXP_define_input2_rule(RuleNo,
			TypeIndex, NameIndex, Arity)
	;
		RuleTuple = variables,
		Code = rl_EXP_define_var_rule(RuleNo,
			TypeIndex, NameIndex, Arity)
	;
		RuleTuple = output1,
		Code = rl_EXP_define_output1_rule(RuleNo,
			TypeIndex, NameIndex, Arity)
	;
		RuleTuple = output2,
		Code = rl_EXP_define_output2_rule(RuleNo,
			TypeIndex, NameIndex, Arity)
	}.

%-----------------------------------------------------------------------------%

	% Shift the inputs to the expression out of the input tuple.
:- pred rl_exprn__deconstruct_input_tuple(tuple_num::in, int::in, 
	list(prog_var)::in, set(prog_var)::in, byte_tree::out,
	rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__deconstruct_input_tuple(_, _, [], _, empty) --> [].
rl_exprn__deconstruct_input_tuple(TupleNo, FieldNo, [Var | Vars],
		NonLocals, Code) -->
	( { set__member(Var, NonLocals) } ->
		rl_exprn_info_lookup_var(Var, VarReg),
		rl_exprn_info_lookup_var_type(Var, Type),
		rl_exprn__assign(reg(VarReg),
			input_field(TupleNo, FieldNo), Type, Code0)
	;
		{ Code0 = empty }
	),
	{ NextField is FieldNo + 1 },
	rl_exprn__deconstruct_input_tuple(TupleNo, NextField, Vars,
		NonLocals, Code1),
	{ Code = tree(Code0, Code1) }.

	% Move the outputs of the expression into the output tuple.
:- pred rl_exprn__construct_output_tuple(list(hlds_goal)::in,
	list(prog_var)::in, byte_tree::out,
	rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__construct_output_tuple(Goals, Vars, Code) -->
	{ goal_list_determinism(Goals, Detism) },
	( { determinism_components(Detism, _, at_most_zero) } ->
		% The condition never succeeds, so don't try to 
		% construct the output.
		{ Code = empty }	
	;
		{ FirstField = 0 },
		rl_exprn__construct_output_tuple_2(FirstField, Vars, Code)
	).

:- pred rl_exprn__construct_output_tuple_2(int::in, list(prog_var)::in, 
		byte_tree::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__construct_output_tuple_2(_, [], empty) --> [].
rl_exprn__construct_output_tuple_2(FieldNo, [Var | Vars], Code) -->
	rl_exprn_info_lookup_var(Var, VarReg),
	rl_exprn_info_lookup_var_type(Var, Type),
	rl_exprn__assign(output_field(FieldNo), reg(VarReg), Type, Code0),
	{ NextField is FieldNo + 1 },
	rl_exprn__construct_output_tuple_2(NextField, Vars, Code1),
	{ Code = tree(Code0, Code1) }.

%-----------------------------------------------------------------------------%

:- pred rl_exprn__goals(list(hlds_goal)::in, byte_tree::in,
	byte_tree::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__goals([], _, empty) --> [].
rl_exprn__goals([Goal | Goals], Fail, Code) --> 
	rl_exprn__goal(Goal, Fail, Code0),
	rl_exprn__goals(Goals, Fail, Code1),
	{ Code = tree(Code0, Code1) }.

:- pred rl_exprn__goal(hlds_goal::in, byte_tree::in, 
	byte_tree::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__goal(unify(_, _, _, Uni, _) - Info, Fail, Code) -->
	rl_exprn__unify(Uni, Info, Fail, Code).
rl_exprn__goal(call(PredId, ProcId, Args, _, _, _) - Info, Fail, Code) -->
	rl_exprn__call(PredId, ProcId, Args, Info, Fail, Code).
rl_exprn__goal(not(NegGoal) - _, Fail, Code) -->
	rl_exprn_info_get_next_label_id(EndLabel),
	{ NotFail = node([rl_EXP_jmp(EndLabel)]) },
	rl_exprn__goal(NegGoal, NotFail, NegCode),
	{ Code = 
		tree(NegCode, 
		tree(Fail, 
		node([rl_PROC_label(EndLabel)])
	)) }.
rl_exprn__goal(if_then_else(_, Cond, Then, Else, _) - _, Fail, Code) -->
	rl_exprn_info_get_next_label_id(StartElse),
	rl_exprn_info_get_next_label_id(EndIte),
	{ CondFail = node([rl_EXP_jmp(StartElse)]) },
	rl_exprn__goal(Cond, CondFail, CondCode),
	rl_exprn__goal(Then, Fail, ThenCode),
	rl_exprn__goal(Else, Fail, ElseCode),
	{ Code =
		tree(CondCode, 
		tree(ThenCode, 
		tree(node([rl_EXP_jmp(EndIte), rl_PROC_label(StartElse)]),
		tree(ElseCode, 
		node([rl_PROC_label(EndIte)])
	)))) }.
rl_exprn__goal(conj(Goals) - _, Fail, Code) -->
	rl_exprn__goals(Goals, Fail, Code).
rl_exprn__goal(par_conj(_, _) - _, _, _) -->
	{ error("rl_exprn__goal: par_conj not yet implemented") }.
rl_exprn__goal(disj(Goals, _) - _Info, Fail, Code) -->
		% Nondet disjunctions should have been transformed into
		% separate Aditi predicates by dnf.m.
	rl_exprn_info_get_next_label_id(EndDisj),
	{ GotoEnd = node([rl_EXP_jmp(EndDisj)]) },
	rl_exprn__disj(Goals, GotoEnd, Fail, DisjCode),
	{ Code = tree(DisjCode, node([rl_PROC_label(EndDisj)])) }.
rl_exprn__goal(switch(Var, _, Cases, _) - _, Fail, Code) -->
	rl_exprn_info_get_next_label_id(EndSwitch),
	{ GotoEnd = node([rl_EXP_jmp(EndSwitch)]) },
	rl_exprn__cases(Var, Cases, GotoEnd, Fail, SwitchCode),
	{ Code = tree(SwitchCode, node([rl_PROC_label(EndSwitch)])) }.
rl_exprn__goal(generic_call(_, _, _, _) - _, _, _) -->
	{ error("rl_exprn__goal: higher-order and class-method calls not yet implemented") }.
rl_exprn__goal(foreign_proc(_, _, _, _, _, _, _) - _, _, _) -->
	{ error("rl_exprn__goal: foreign_proc not yet implemented") }.
rl_exprn__goal(some(_, _, Goal) - _, Fail, Code) -->
	rl_exprn__goal(Goal, Fail, Code).
rl_exprn__goal(shorthand(_) - _, _, _) -->
	% these should have been expanded out by now
	{ error("rl_exprn__goal: unexpected shorthand") }.

:- pred rl_exprn__cases(prog_var::in, list(case)::in, byte_tree::in,
		byte_tree::in, byte_tree::out, 
		rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__cases(_, [], _, Fail, Fail) --> [].
rl_exprn__cases(Var, [case(ConsId, Goal) | Cases], Succeed, Fail, Code) -->
	rl_exprn_info_get_next_label_id(NextCase),
	{ Jmp = rl_EXP_jmp(NextCase) },
	rl_exprn__functor_test(Var, ConsId, node([Jmp]), TestCode),
	rl_exprn__goal(Goal, Fail, GoalCode),
	rl_exprn__cases(Var, Cases, Succeed, Fail, Code1),
	{ Code = 
		tree(TestCode,
		tree(GoalCode,
		tree(Succeed, 
		tree(node([rl_PROC_label(NextCase)]),
		Code1
	)))) }.

:- pred rl_exprn__disj(list(hlds_goal)::in, byte_tree::in,
		byte_tree::in, byte_tree::out, 
		rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__disj([], _, Fail, Fail) --> [].
rl_exprn__disj([Goal | Goals], Succeed, Fail, Code) -->
	rl_exprn_info_get_next_label_id(NextDisj),
	{ TryNext = node([rl_EXP_jmp(NextDisj)]) },
	{ NextLabel = node([rl_PROC_label(NextDisj)]) },
	rl_exprn__goal(Goal, TryNext, GoalCode),
	rl_exprn__disj(Goals, Succeed, Fail, Code1),
	{ Code = 
		tree(GoalCode,
		tree(Succeed,
		tree(NextLabel,
		Code1
	))) }.

%-----------------------------------------------------------------------------%

:- pred rl_exprn__call(pred_id::in, proc_id::in, list(prog_var)::in, 
		hlds_goal_info::in, byte_tree::in, byte_tree::out, 
		rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__call(PredId, ProcId, Vars, GoalInfo, Fail, Code) -->
	rl_exprn_info_get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo) },
	{ proc_info_inferred_determinism(ProcInfo, Detism) },
	rl_exprn_info_get_parent_pred_proc_ids(Parents0),
	(
		% XXX Nondet top-down calls are not yet implemented.
		{ determinism_components(Detism, _, at_most_many) }
	->
		{ goal_info_get_context(GoalInfo, Context) },
		{ rl_exprn__call_not_implemented_error(Context,
			ModuleInfo, PredId, ProcId,
			"nondeterministic Mercury calls in Aditi procedures") }
	;
		% XXX Top-down calls to imported predicates
		% are not yet implemented.
		{ pred_info_is_imported(PredInfo) },

		% Calls to `unify/2' and `compare/3' will have been
		% transformed into the type-specific versions
		% by polymorphism.m. Polymorphic types are not allowed
		% in Aditi predicates so the types must be known.
		\+ {
			% `index/2' doesn't work in Aditi.
			code_util__compiler_generated(PredInfo),
			\+ pred_info_name(PredInfo, "__Index__")
		},
		{ \+ code_util__predinfo_is_builtin(PredInfo) },
		{ \+ rl_exprn__is_simple_extra_aditi_builtin(PredInfo,
			ProcId, _) }
	->
		{ goal_info_get_context(GoalInfo, Context) },
		{ rl_exprn__call_not_implemented_error(Context,
			ModuleInfo, PredId, ProcId,
			"calls to imported Mercury procedures from Aditi") }
	;
		% XXX Recursive top-down calls are not yet implemented.
		{ set__member(proc(PredId, ProcId), Parents0) }
	->
		{ goal_info_get_context(GoalInfo, Context) },
		{ rl_exprn__call_not_implemented_error(Context,
			ModuleInfo, PredId, ProcId,
			"recursive Mercury calls in Aditi procedures") }
	;
		rl_exprn__call_body(PredId, ProcId, PredInfo, ProcInfo,
			Fail, Vars, Code)
	).

:- pred rl_exprn__call_not_implemented_error(prog_context::in, module_info::in,
		pred_id::in, proc_id::in, string::in) is erroneous.

rl_exprn__call_not_implemented_error(Context, 
		ModuleInfo, PredId, ProcId, ErrorDescr) :-
	error_util__describe_one_proc_name(ModuleInfo,
		proc(PredId, ProcId), ProcName),
	prog_out__context_to_string(Context, ContextStr),
	string__append_list(
		[
			ContextStr, "in call to ", ProcName, ":\n",
			"sorry, not yet implemented - ", ErrorDescr
		],
		Msg),
	error(Msg).

:- pred rl_exprn__call_body(pred_id::in, proc_id::in, pred_info::in,
	proc_info::in, byte_tree::in, list(prog_var)::in, byte_tree::out,
	rl_exprn_info::in, rl_exprn_info::out) is det.  

rl_exprn__call_body(PredId, ProcId, PredInfo, ProcInfo, Fail, Args, Code) -->
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_arity(PredInfo, Arity) },
	(
		{ code_util__predinfo_is_builtin(PredInfo) }
	->
		rl_exprn__generate_builtin_call(PredId, ProcId, PredInfo,
			Args, Fail, Code)
	;
		{ rl_exprn__is_simple_extra_aditi_builtin(PredInfo,
			ProcId, Bytecode) }
	->
		rl_exprn__generate_extra_aditi_builtin(Bytecode,
			Args, Code)
	;
		% Handle unify/2 specially, since it is possibly recursive,
		% which will cause the code below to fall over. Also, magic.m
		% doesn't add type_info arguments yet.
		{ PredName = "__Unify__" },
		{ Arity = 2 },
		{ list__reverse(Args, [Arg1, Arg2 | _]) },
		{ hlds_pred__in_in_unification_proc_id(ProcId) }
	->
		rl_exprn_info_lookup_var(Arg1, Arg1Loc),
		rl_exprn_info_lookup_var(Arg2, Arg2Loc),
		rl_exprn_info_lookup_var_type(Arg1, Type),
		rl_exprn__test(reg(Arg1Loc), reg(Arg2Loc), Type, Fail, Code)
	;
		% Handle compare/3 specially for the same reason
		% as unify/2 above.
		{ PredName = "__Compare__" },
		{ Arity = 3 },
		{ list__reverse(Args, [Arg2, Arg1, Res | _]) }
	->
		rl_exprn_info_lookup_var(Arg1, Arg1Loc),
		rl_exprn_info_lookup_var(Arg2, Arg2Loc),
		rl_exprn_info_lookup_var(Res, ResultReg),
		rl_exprn_info_lookup_var_type(Arg1, Type),
		rl_exprn_info_lookup_var_type(Res, ResType),
		rl_exprn__generate_push(reg(Arg1Loc), Type, PushCode1),
		rl_exprn__generate_push(reg(Arg2Loc), Type, PushCode2),
		{ rl_exprn__type_to_aditi_type(Type, AditiType) },
		{ rl_exprn__compare_bytecode(AditiType, Compare) },

		{ mercury_public_builtin_module(Builtin) },
		{ EQConsId = cons(qualified(Builtin, "="), 0) },
		{ LTConsId = cons(qualified(Builtin, "<"), 0) },
		{ GTConsId = cons(qualified(Builtin, ">"), 0) },
		rl_exprn__cons_id_to_rule_number(EQConsId, ResType, EQRuleNo),
		rl_exprn__cons_id_to_rule_number(GTConsId, ResType, GTRuleNo),
		rl_exprn__cons_id_to_rule_number(LTConsId, ResType, LTRuleNo),
		
		rl_exprn_info_get_next_label_id(GTLabel),
		rl_exprn_info_get_next_label_id(LTLabel),
		rl_exprn_info_get_next_label_id(EndLabel),

		{ Code = 
			tree(PushCode1,
			tree(PushCode2,
			node([
				Compare,
				rl_EXP_b3way(LTLabel, GTLabel),
				rl_EXP_new_term_var(ResultReg, EQRuleNo),
				rl_EXP_term_pop,
				rl_EXP_jmp(EndLabel),
				rl_PROC_label(LTLabel),
				rl_EXP_new_term_var(ResultReg, LTRuleNo),
				rl_EXP_term_pop,
				rl_EXP_jmp(EndLabel),
				rl_PROC_label(GTLabel),
				rl_EXP_new_term_var(ResultReg, GTRuleNo),
				rl_EXP_term_pop,
				rl_PROC_label(EndLabel)
			])
		)) }
	;
		% XXX temporary hack until we allow Mercury calls from Aditi -
		% generate the goal of the called procedure, not a call to 
		% the called procedure.
		rl_exprn_info_get_parent_pred_proc_ids(Parents0),
		{ set__insert(Parents0, proc(PredId, ProcId), Parents) },
		rl_exprn_info_set_parent_pred_proc_ids(Parents),
		rl_exprn__inline_call(PredId, ProcId,
			PredInfo, ProcInfo, Args, Goal),
		rl_exprn__goal(Goal, Fail, Code),
		rl_exprn_info_set_parent_pred_proc_ids(Parents0)
	).

:- pred rl_exprn__inline_call(pred_id::in, proc_id::in, pred_info::in,
		proc_info::in, list(prog_var)::in, hlds_goal::out,
		rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__inline_call(_PredId, _ProcId, CalledPredInfo,
		CalledProcInfo, Args, Goal) -->

	rl_exprn_info_get_varset(VarSet0),
	rl_exprn_info_get_vartypes(VarTypes0),
	{ varset__init(TVarSet0) },
	{ map__init(TVarMap0) },
	{ inlining__do_inline_call([], Args, CalledPredInfo, CalledProcInfo,
		VarSet0, VarSet, VarTypes0, VarTypes, TVarSet0, _, TVarMap0, _,
		Goal) },

	rl_exprn_info_set_varset(VarSet),
	rl_exprn_info_set_vartypes(VarTypes).

%-----------------------------------------------------------------------------%

:- pred rl_exprn__unify(unification::in, hlds_goal_info::in, 
		byte_tree::in, byte_tree::out,
		rl_exprn_info::in, rl_exprn_info::out) is det.
	
rl_exprn__unify(construct(Var, ConsId, Args, UniModes, _, _, _), 
		GoalInfo, _Fail, Code) -->
	rl_exprn_info_lookup_var_type(Var, Type),
	rl_exprn_info_lookup_var(Var, VarReg),
	( 
		{ ConsId = cons(SymName, _) },
		(
			{ mercury_private_builtin_module(Builtin) },
			{ SymName = qualified(Builtin, TypeInfo) },
			( { TypeInfo = "type_info" }
			; { TypeInfo = "type_ctor_info" }
			)
		->
			% XXX for now we ignore these and hope it doesn't
			% matter. They may be introduced for calls to the
			% automatically generated unification and comparison
			% procedures.
			{ Code = empty }
		;
			{ rl_exprn__is_char_cons_id(ConsId, Type, Int) }
		->
			rl_exprn__assign(reg(VarReg), const(int(Int)),
				Type, Code)
		;
			rl_exprn__cons_id_to_rule_number(ConsId, Type, RuleNo),
			{ Create = rl_EXP_new_term_var(VarReg, RuleNo) },
			{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
			rl_exprn__handle_functor_args(Args, UniModes,
				NonLocals, 0, ConsId, ArgCodes),
			{ Code =
				tree(node([Create]),
				tree(ArgCodes,
				node([rl_EXP_term_pop])
			)) }
		)
	;
		{ ConsId = int_const(Int) },
		rl_exprn__assign(reg(VarReg), const(int(Int)), Type, Code)
	;
		{ ConsId = string_const(String) },
		rl_exprn__assign(reg(VarReg), const(string(String)),
			Type, Code)
	;
		{ ConsId = float_const(Float) },
		rl_exprn__assign(reg(VarReg), const(float(Float)), Type, Code)
	; 
		{ ConsId = pred_const(_, _, _) },
		{ error("rl_exprn__unify: unsupported cons_id - pred_const") }
	; 
		{ ConsId = code_addr_const(_, _) },
		{ error("rl_exprn__unify: unsupported cons_id - code_addr_const") }
	; 
		{ ConsId = type_ctor_info_const(_, _, _) },
		% XXX for now we ignore these and hope it doesn't matter.
		% They may be introduced for calls to the automatically
		% generated unification and comparison procedures.
		{ Code = empty }
	; 
		{ ConsId = base_typeclass_info_const(_, _, _, _) },
		{ error("rl_exprn__unify: unsupported cons_id - base_typeclass_info_const") }
	; 
		{ ConsId = tabling_pointer_const(_, _) },
		{ error("rl_exprn__unify: unsupported cons_id - tabling_pointer_const") }
	; 
		{ ConsId = deep_profiling_proc_static(_) },
		{ error("rl_exprn__unify: unsupported cons_id - deep_profiling_proc_static") }
	; 
		{ ConsId = table_io_decl(_) },
		{ error("rl_exprn__unify: unsupported cons_id - table_io_decl") }
	).
		
rl_exprn__unify(deconstruct(Var, ConsId, Args, UniModes, CanFail, _CanCGC),
		GoalInfo, Fail, Code) -->
	rl_exprn_info_lookup_var(Var, VarLoc),
	rl_exprn_info_lookup_var_type(Var, Type),
	( { CanFail = can_fail } ->
		rl_exprn__functor_test(Var, ConsId, Fail, TestCode)
	;
		{ TestCode = empty }
	),
	( { Args \= [] } ->
		{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
		rl_exprn__generate_push(reg(VarLoc), Type, PushCode),
		rl_exprn__handle_functor_args(Args, UniModes, 
			NonLocals, 0, ConsId, ArgCodes0),
		{ ArgCodes =
			tree(PushCode,
			tree(ArgCodes0,
			node([rl_EXP_term_pop])
		)) }
	;
		{ ArgCodes = empty }
	),
	{ Code = tree(TestCode, ArgCodes) }.
rl_exprn__unify(complicated_unify(_, _, _), _, _, _) -->
	{ error("rl_gen__unify: complicated_unify") }.
rl_exprn__unify(assign(Var1, Var2), _GoalInfo, _Fail, Code) -->
	rl_exprn_info_lookup_var(Var1, Var1Loc),
	rl_exprn_info_lookup_var(Var2, Var2Loc),
	rl_exprn_info_lookup_var_type(Var1, Type),
	rl_exprn__assign(reg(Var1Loc), reg(Var2Loc), Type, Code).
rl_exprn__unify(simple_test(Var1, Var2), _GoalInfo, Fail, Code) -->
	% Note that the type here isn't necessarily one of the builtins - 
	% magic.m uses simple_test for all in-in unifications it introduces.
	rl_exprn_info_lookup_var(Var1, Var1Loc),
	rl_exprn_info_lookup_var(Var2, Var2Loc),
	rl_exprn_info_lookup_var_type(Var1, Type),
	rl_exprn__test(reg(Var1Loc), reg(Var2Loc), Type, Fail, Code).

:- pred rl_exprn__assign(rl_lval::in, rl_rval::in, (type)::in,
	byte_tree::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__assign(Lval, Rval, Type, Code) -->
	rl_exprn__generate_push(Rval, Type, PushCode),
	rl_exprn__generate_pop(Lval, Type, PopCode),
	{ Code = tree(PushCode, PopCode) }.

:- pred rl_exprn__test(rl_rval::in, rl_rval::in, (type)::in, byte_tree::in,
	byte_tree::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__test(Var1Loc, Var2Loc, Type, Fail, Code) -->
	rl_exprn__generate_push(Var1Loc, Type, PushCode1),
	rl_exprn__generate_push(Var2Loc, Type, PushCode2),
	rl_exprn_info_get_next_label_id(Label),
	{ rl_exprn__type_to_aditi_type(Type, AditiType) },

	{ rl_exprn__test_bytecode(AditiType, EqInstr) },
	{ Code = 
		tree(PushCode1, 
		tree(PushCode2, 
		tree(node([EqInstr]), 
		tree(node([rl_EXP_bnez(Label)]),
		tree(Fail,
		node([rl_PROC_label(Label)])
	))))) }.

:- pred rl_exprn__test_bytecode(aditi_type::in, bytecode::out) is det.

rl_exprn__test_bytecode(int, rl_EXP_int_eq).
rl_exprn__test_bytecode(float, rl_EXP_flt_eq).
rl_exprn__test_bytecode(string, rl_EXP_str_eq).
rl_exprn__test_bytecode(term(_), rl_EXP_term_eq).

:- pred rl_exprn__functor_test(prog_var::in, cons_id::in, byte_tree::in, 
	byte_tree::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__functor_test(Var, ConsId, Fail, Code) -->
	rl_exprn_info_lookup_var(Var, VarReg),
	rl_exprn_info_lookup_var_type(Var, Type),
	( { ConsId = int_const(Int) } ->
		rl_exprn__test(reg(VarReg), const(int(Int)), Type, Fail, Code)
	; { ConsId = string_const(String) } ->
		rl_exprn__test(reg(VarReg), const(string(String)),
			Type, Fail, Code)
	; { ConsId = float_const(Float) } ->
		rl_exprn__test(reg(VarReg), const(float(Float)),
			Type, Fail, Code)
	; { rl_exprn__is_char_cons_id(ConsId, Type, Int) } ->
		rl_exprn__test(reg(VarReg), const(int(Int)), Type, Fail, Code)
	; { ConsId = cons(_, _) } ->
		rl_exprn_info_get_next_label_id(Label),
		rl_exprn__cons_id_to_rule_number(ConsId, Type, RuleNo),
		rl_exprn__generate_push(reg(VarReg), Type, PushCode),
		{ Code = 
			tree(PushCode,
			tree(node([
				rl_EXP_test_functor(RuleNo),
				rl_EXP_bnez(Label)
			]),
			tree(Fail,
			node([rl_PROC_label(Label)])
		))) }
	;
		{ error("rl_exprn__functor_test: unsupported cons_id") }
	).

:- pred rl_exprn__is_char_cons_id(cons_id::in, 
		(type)::in, int::out) is semidet.

rl_exprn__is_char_cons_id(ConsId, Type, Int) :-
	ConsId = cons(unqualified(CharStr), 0),
	type_to_type_id(Type, unqualified("character") - 0, _),
		% Convert characters to integers.
	( string__to_char_list(CharStr, [Char]) ->
		char__to_int(Char, Int)
	;
		error("rl_exprn__unify: invalid char")
	).

:- pred rl_exprn__handle_functor_args(list(prog_var)::in, list(uni_mode)::in,
		set(prog_var)::in, int::in, cons_id::in, byte_tree::out,
		rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__handle_functor_args([], [_|_], _, _, _, _) -->
	{ error("rl_exprn__handle_functor_args") }.
rl_exprn__handle_functor_args([_|_], [], _, _, _, _) -->
	{ error("rl_exprn__handle_functor_args") }.
rl_exprn__handle_functor_args([], [], _, _, _, empty) --> [].
rl_exprn__handle_functor_args([Arg | Args], [Mode | Modes], NonLocals,
		Index, ConsId, Code) -->
	{ NextIndex is Index + 1 },
	rl_exprn__handle_functor_args(Args, Modes, NonLocals,
		NextIndex, ConsId, Code0),
	( { set__member(Arg, NonLocals) } ->
		rl_exprn_info_lookup_var_type(Arg, Type),
		rl_exprn_info_get_module_info(ModuleInfo),

		{ Mode = ((LI - RI) -> (LF - RF)) },
		{ mode_to_arg_mode(ModuleInfo, (LI -> LF), Type, LeftMode) },
		{ mode_to_arg_mode(ModuleInfo, (RI -> RF), Type, RightMode) },
		(
			{ LeftMode = top_in },
			{ RightMode = top_in }
		->
			% Can't have test in arg unification.
			{ error("test in arg of [de]construction") }
		;
			{ LeftMode = top_in },
			{ RightMode = top_out }
		->
			rl_exprn_info_lookup_var(Arg, ArgReg),
			{ rl_exprn__type_to_aditi_type(Type, AditiType) },
			{ rl_exprn__get_term_arg_code(AditiType,
				Index, TermArgCode) },
			rl_exprn__generate_pop(reg(ArgReg), Type, PopCode),
			{ Code1 =
				tree(node([rl_EXP_term_dup]),
				tree(node([TermArgCode]),
				PopCode
			)) }
		;
			{ LeftMode = top_out },
			{ RightMode = top_in }
		->
			rl_exprn_info_lookup_var(Arg, ArgLoc),
			{ rl_exprn__type_to_aditi_type(Type, AditiType) },
			{ rl_exprn__set_term_arg_code(AditiType,
				Index, TermArgCode) },
			rl_exprn__generate_push(reg(ArgLoc), Type, PushCode),
			{ Code1 =
				tree(node([rl_EXP_term_dup]),
				tree(PushCode,
				node([TermArgCode])
			)) }
		;
			{ LeftMode = top_unused },
			{ RightMode = top_unused }
		->
			{ Code1 = empty }
		;	
			{ error("rl_exprn__handle_functor_args: weird unification") }
		),
		{ Code = tree(Code1, Code0) }
	;
		{ Code = Code0 }
	).

%-----------------------------------------------------------------------------%

:- pred rl_exprn__cons_id_to_rule_number(cons_id::in, (type)::in, int::out, 
		rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__cons_id_to_rule_number(ConsId, Type, RuleNo) -->
	rl_exprn__cons_id_to_rule_number(ConsId, Type, variables, RuleNo).

:- pred rl_exprn__cons_id_to_rule_number(cons_id::in, (type)::in,
		exprn_tuple::in, int::out,
		rl_exprn_info::in, rl_exprn_info::out) is det.
	
rl_exprn__cons_id_to_rule_number(ConsId, Type, ExprnTuple, RuleNo) -->
	( 
		{ ConsId = cons(ConsName, Arity) },
		{ type_to_type_id(Type, TypeId, Args) }
	->
		% These names should not be quoted, since they are not
		% being parsed, just compared against other strings.
		{ rl__mangle_type_name(TypeId, Args, MangledTypeName) },
		{ rl__mangle_ctor_name(ConsName, Arity, MangledConsName) },
		{ Rule = rl_rule(MangledTypeName, MangledConsName, Arity) },
		rl_exprn_info_lookup_rule(Rule - ExprnTuple, RuleNo)
	;
		{ error("rl_exprn__cons_id_to_rule_number") }
	).

%-----------------------------------------------------------------------------%

	% Put a value on top of the expression stack.
:- pred rl_exprn__generate_push(rl_rval::in, (type)::in, byte_tree::out,
		rl_exprn_info::in, rl_exprn_info::out) is det.
	
rl_exprn__generate_push(reg(Reg), Type0, Code) -->
	{ rl_exprn__type_to_aditi_type(Type0, Type) },
	rl_exprn__do_generate_push_var(Reg, Type, Code).

rl_exprn__generate_push(const(Const), _Type, node([ByteCode])) -->
	rl_exprn_info_lookup_const(Const, ConstNo),
	{
		Const = int(_),
		ByteCode = rl_EXP_int_push(ConstNo)
	;
		Const = float(_),
		ByteCode = rl_EXP_flt_push(ConstNo)
	;
		Const = string(_),
		ByteCode = rl_EXP_str_push(ConstNo)
	}.
rl_exprn__generate_push(input_field(TupleNo, FieldNo), 
		Type0, node([ByteCode])) -->
	{ rl_exprn__type_to_aditi_type(Type0, Type) },
	{ rl_exprn__get_input_field_code(TupleNo, Type, FieldNo, ByteCode) }.
rl_exprn__generate_push(term_arg(TermLoc, _ConsId, Field, TermType), 
		Type0, ByteCodes) -->
	{ rl_exprn__type_to_aditi_type(Type0, AditiType) },
	rl_exprn__generate_push(TermLoc, TermType, PushCodes),
	{ 
		AditiType = int,
		ByteCode = rl_EXP_get_int_arg(Field)
	;
		AditiType = float,
		ByteCode = rl_EXP_get_flt_arg(Field)
	;
		AditiType = string,
		ByteCode = rl_EXP_get_str_arg(Field)
	;
		AditiType = term(_),
		ByteCode = rl_EXP_get_term_arg(Field)
	},
	{ ByteCodes = 
		tree(PushCodes, 
		node([ByteCode])
	) }.

:- pred rl_exprn__do_generate_push_var(int::in, aditi_type::in,
	byte_tree::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__do_generate_push_var(Index, Type, node([ByteCode])) -->
	{
		Type = int,
		ByteCode = rl_EXP_int_push_var(Index)
	;
		Type = float,
		ByteCode = rl_EXP_flt_push_var(Index)
	;
		Type = string,
		ByteCode = rl_EXP_str_push_var(Index)
	;
		Type = term(_),
		ByteCode = rl_EXP_term_push_var(Index)
	}.

%-----------------------------------------------------------------------------%

	% Get the value on top of the expression stack and put it in the 
	% specified rl_lval.
:- pred rl_exprn__generate_pop(rl_lval::in, (type)::in, byte_tree::out,
		rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__generate_pop(reg(Reg), Type0, ByteCode) -->
	{ rl_exprn__type_to_aditi_type(Type0, Type) },
	rl_exprn__do_generate_pop_var(Reg, Type, ByteCode).

rl_exprn__generate_pop(output_field(FieldNo), Type0, node([ByteCode])) -->
	{ rl_exprn__type_to_aditi_type(Type0, Type) },
	{
		Type = int,
		ByteCode = rl_EXP_output_int(FieldNo)
	;
		Type = float,
		ByteCode = rl_EXP_output_flt(FieldNo)
	;
		Type = string,
		ByteCode = rl_EXP_output_str(FieldNo)
	;
		Type = term(_),
		% This bytecode copies the argument term adjusting rule numbers
		% if the schemas of the argument term and the output tuple 
		% do not match.
		ByteCode = rl_EXP_put_term_output(FieldNo)
	}.
rl_exprn__generate_pop(term_arg(Reg, _ConsId, Field, TermType), Type0, Code) -->
	% There's no swap operation (and to do a swap, the expression
	% evaluator would probably need to know the types of the top
	% two elements of the stack, so rl_EXP_swap_int_int,
	% rl_EXP_swap_int_flt, etc). 
	{ rl_exprn__type_to_aditi_type(Type0, Type) },
	rl_exprn_info_get_free_reg(Type0, TmpIndex),
	rl_exprn__generate_pop(reg(TmpIndex), Type0, PopCode1),
	rl_exprn__generate_push(reg(Reg), TermType, PushCode1),
	rl_exprn__generate_push(reg(TmpIndex), Type0, PushCode2),
	(
		{ Type = int },
		{ SetArg = rl_EXP_set_int_arg(Field) }
	;
		{ Type = float },
		{ SetArg = rl_EXP_set_flt_arg(Field) }
	;
		{ Type = string },
		{ SetArg = rl_EXP_set_str_arg(Field) }
	;
		{ Type = term(_) },
		{ SetArg = rl_EXP_put_term_arg(Field) }
	),
	{ Code = 
		tree(PopCode1, 
		tree(PushCode1, 
		tree(PushCode2, 
		node([SetArg])
	))) }.

:- pred rl_exprn__do_generate_pop_var(int::in, aditi_type::in, byte_tree::out,
		rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__do_generate_pop_var(Index, Type, node([ByteCode])) -->
	{
		Type = int,
		ByteCode = rl_EXP_int_pop_var(Index)
	;
		Type = float,
		ByteCode = rl_EXP_flt_pop_var(Index)
	;
		Type = string,
		ByteCode = rl_EXP_str_pop_var(Index)
	;
		Type = term(_),
		ByteCode = rl_EXP_put_term_var(Index)
	}.	

%-----------------------------------------------------------------------------%

:- pred rl_exprn__generate_builtin_call(pred_id::in, proc_id::in,
	pred_info::in, list(prog_var)::in, byte_tree::in,
	byte_tree::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__generate_builtin_call(_PredId, ProcId,
		PredInfo, Args, Fail, Code) -->
	{ pred_info_module(PredInfo, PredModule0) },
	{ pred_info_name(PredInfo, PredName) },

	%
	% Generate LLDS for the builtin, then convert that to Aditi bytecode.
	%
	(
		{ builtin_ops__translate_builtin(PredModule0, PredName, 
			ProcId, Args, SimpleCode) } 
	->
		(
			{ SimpleCode = test(TestExpr) },
			( rl_exprn__simple_expr_to_rl_rval(TestExpr, RvalCode) ->
				rl_exprn_info_get_next_label_id(SuccLabel),
				{ Code =
					tree(RvalCode,
					tree(node([rl_EXP_bnez(SuccLabel)]),
					tree(Fail,
					node([rl_PROC_label(SuccLabel)])
				))) }
			;
				{ error("rl_exprn__generate_exprn_instr: invalid test") }
			)
		 ;
		 	{ SimpleCode = assign(OutputVar, AssignExpr) },
			rl_exprn_info_lookup_var(OutputVar, OutputLoc),
			rl_exprn_info_lookup_var_type(OutputVar, Type),
			{ rl_exprn__type_to_aditi_type(Type, AditiType) },
			rl_exprn__maybe_simple_expr_to_rl_rval(yes(AssignExpr),
				AditiType, RvalCode),
			rl_exprn__generate_pop(reg(OutputLoc), Type, PopCode),
			{ Code = tree(RvalCode, PopCode) }
		)
	;
		{ prog_out__sym_name_to_string(PredModule0, PredModule) },
		{ pred_info_arity(PredInfo, Arity) },
		{ string__format("Sorry, not implemented in Aditi: %s:%s/%i",
			[s(PredModule), s(PredName), i(Arity)], Msg) },
		{ error(Msg) }
	).

:- pred rl_exprn__maybe_simple_expr_to_rl_rval(maybe(simple_expr(prog_var))::in, 
		aditi_type::in, byte_tree::out,
		rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__maybe_simple_expr_to_rl_rval(no, _, empty) --> [].
rl_exprn__maybe_simple_expr_to_rl_rval(yes(LLDSRval), _ResultType, Code) -->
	( rl_exprn__simple_expr_to_rl_rval(LLDSRval, RvalCode) ->
		{ Code = RvalCode }
	;
		{ error("rl_exprn__maybe_simple_expr_to_rl_rval: invalid simple_expr") }
	).

:- pred rl_exprn__simple_expr_to_rl_rval(simple_expr(prog_var)::in, byte_tree::out,
		rl_exprn_info::in, rl_exprn_info::out) is semidet.

rl_exprn__simple_expr_to_rl_rval(leaf(Var), Code) -->
	rl_exprn_info_lookup_var(Var, VarLoc),
	rl_exprn_info_lookup_var_type(Var, Type),
	rl_exprn__generate_push(reg(VarLoc), Type, Code).
rl_exprn__simple_expr_to_rl_rval(int_const(Int), PushCode) -->
	{ rl_exprn__aditi_type_to_type(int, Type1) },
	rl_exprn__generate_push(const(int(Int)), Type1, PushCode).
rl_exprn__simple_expr_to_rl_rval(float_const(Float), PushCode) -->
	{ rl_exprn__aditi_type_to_type(float, Type1) },
	rl_exprn__generate_push(const(float(Float)), Type1, PushCode).
rl_exprn__simple_expr_to_rl_rval(unary(_UnOp, _Expr), _Code) -->
	% None of the MLDS/LLDS unary builtins are implemented in Aditi.
	% The only one which is returned by builtin_ops__translate_builtin
	% is `bitwise_complement', for which there is no corresponding
	% bytecode in Aditi-RL.
	{ fail }.
rl_exprn__simple_expr_to_rl_rval(binary(BinOp, Expr1, Expr2), Code) -->
	rl_exprn__simple_expr_to_rl_rval(Expr1, Code1),
	rl_exprn__simple_expr_to_rl_rval(Expr2, Code2),
	{ rl_exprn__binop_bytecode(BinOp, Bytecode) },
	{ Code = 
		tree(Code1, 
		tree(Code2, 
		node([Bytecode])
	)) }.

:- pred rl_exprn__binop_bytecode(binary_op::in, bytecode::out) is semidet.

rl_exprn__binop_bytecode((+), rl_EXP_int_add).
rl_exprn__binop_bytecode((-), rl_EXP_int_sub).
rl_exprn__binop_bytecode((*), rl_EXP_int_mult).
rl_exprn__binop_bytecode((/), rl_EXP_int_div).
rl_exprn__binop_bytecode((mod), rl_EXP_int_mod).
rl_exprn__binop_bytecode(eq, rl_EXP_int_eq).
rl_exprn__binop_bytecode(ne, rl_EXP_int_ne).
rl_exprn__binop_bytecode(str_eq, rl_EXP_str_eq).
rl_exprn__binop_bytecode(str_ne, rl_EXP_str_ne).
rl_exprn__binop_bytecode(str_lt, rl_EXP_str_lt).
rl_exprn__binop_bytecode(str_gt, rl_EXP_str_gt).
rl_exprn__binop_bytecode(str_le, rl_EXP_str_le).
rl_exprn__binop_bytecode(str_ge, rl_EXP_str_ge).
rl_exprn__binop_bytecode((<), rl_EXP_int_lt).
rl_exprn__binop_bytecode((>), rl_EXP_int_gt).
rl_exprn__binop_bytecode((>=), rl_EXP_int_ge).
rl_exprn__binop_bytecode((<=), rl_EXP_int_le).
rl_exprn__binop_bytecode(float_plus, rl_EXP_flt_add).
rl_exprn__binop_bytecode(float_minus, rl_EXP_flt_sub).
rl_exprn__binop_bytecode(float_times, rl_EXP_flt_mult).
rl_exprn__binop_bytecode(float_divide, rl_EXP_flt_div).
rl_exprn__binop_bytecode(float_eq, rl_EXP_flt_eq).
rl_exprn__binop_bytecode(float_ne, rl_EXP_flt_ne).
rl_exprn__binop_bytecode(float_lt, rl_EXP_flt_lt).
rl_exprn__binop_bytecode(float_gt, rl_EXP_flt_gt).
rl_exprn__binop_bytecode(float_le, rl_EXP_flt_le).
rl_exprn__binop_bytecode(float_ge, rl_EXP_flt_ge).

%-----------------------------------------------------------------------------%

	% Generate code for deterministic library predicates and functions
	% for which all arguments except the last are input.
	% This is not an exhaustive list, it's just the ones that
	% Aditi happens to have bytecodes for.
	% This is only needed until Aditi can call arbitrary Mercury code.
:- pred rl_exprn__generate_extra_aditi_builtin(bytecode::in, 
		list(prog_var)::in, byte_tree::out,
		rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__generate_extra_aditi_builtin(Bytecode, Args, Code) -->
	% The extra aditi builtins are not all functions, but
	% this does the right thing.
	{ pred_args_to_func_args(Args, InArgs, OutArg) },

	rl_exprn__push_builtin_args(InArgs, empty, PushCode),

	rl_exprn_info_lookup_var(OutArg, OutReg),
	rl_exprn_info_lookup_var_type(OutArg, OutVarType),
	rl_exprn__generate_pop(reg(OutReg), OutVarType, PopCode),
	
	{ Code =
		tree(PushCode,
		tree(node([Bytecode]),
		PopCode
	)) }.

:- pred rl_exprn__push_builtin_args(list(prog_var)::in, byte_tree::in,
	byte_tree::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__push_builtin_args([], Code, Code) --> [].
rl_exprn__push_builtin_args([Var | Vars], Code0, Code) -->
	rl_exprn_info_lookup_var(Var, VarReg),
	rl_exprn_info_lookup_var_type(Var, VarType),
	rl_exprn__generate_push(reg(VarReg), VarType, Code1),
	rl_exprn__push_builtin_args(Vars, tree(Code0, Code1), Code).

:- pred rl_exprn__is_simple_extra_aditi_builtin(pred_info::in, proc_id::in,
		bytecode::out) is semidet.

rl_exprn__is_simple_extra_aditi_builtin(PredInfo, ProcId, Bytecode) :-
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	pred_info_module(PredInfo, PredModule),
	PredModule = unqualified(PredModuleName),
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, PredArity0),
	hlds_pred__proc_id_to_int(ProcId, ProcInt),
	adjust_func_arity(PredOrFunc, PredArity, PredArity0),
	rl_exprn__simple_extra_builtin(PredOrFunc, PredModuleName,
		PredName, PredArity, ProcInt, Bytecode).

:- pred rl_exprn__simple_extra_builtin(pred_or_func::in, string::in,
		string::in, int::in, int::in, bytecode::in) is semidet.
:- mode rl_exprn__simple_extra_builtin(in, in, in, in, out, out) is semidet.

rl_exprn__simple_extra_builtin(predicate, "int", "to_float", 2, 0,
		rl_EXP_int_toflt).
rl_exprn__simple_extra_builtin(predicate, "int", "max", 3, 0, rl_EXP_int_max).
rl_exprn__simple_extra_builtin(predicate, "int", "min", 3, 0, rl_EXP_int_min).
rl_exprn__simple_extra_builtin(predicate, "int", "abs", 2, 0, rl_EXP_int_abs).

rl_exprn__simple_extra_builtin(function, "float", "float", 1, 0,
		rl_EXP_int_toflt).
rl_exprn__simple_extra_builtin(function, "float",
		"truncate_to_int", 1, 0, rl_EXP_flt_toint).
rl_exprn__simple_extra_builtin(function, "float", "pow", 2, 0, rl_EXP_flt_pow).
rl_exprn__simple_extra_builtin(predicate, "float", "pow", 3, 0,
		rl_EXP_flt_pow).
rl_exprn__simple_extra_builtin(function, "float", "abs", 1, 0, rl_EXP_flt_abs).
rl_exprn__simple_extra_builtin(predicate, "float", "abs", 2, 0,
		rl_EXP_flt_abs).
rl_exprn__simple_extra_builtin(function, "float", "max", 2, 0, rl_EXP_flt_max).
rl_exprn__simple_extra_builtin(predicate, "float", "max", 3, 0,
		rl_EXP_flt_max).
rl_exprn__simple_extra_builtin(function, "float", "min", 2, 0, rl_EXP_flt_min).
rl_exprn__simple_extra_builtin(predicate, "float", "min", 3, 0,
		rl_EXP_flt_min).

rl_exprn__simple_extra_builtin(function, "math", "ceiling", 1, 0,
		rl_EXP_flt_ceil).
rl_exprn__simple_extra_builtin(function, "math", "floor", 1, 0,
		rl_EXP_flt_floor).
rl_exprn__simple_extra_builtin(function, "math", "round", 1, 0,
		rl_EXP_flt_round).
rl_exprn__simple_extra_builtin(function, "math", "sqrt", 1, 0,
		rl_EXP_flt_sqrt).
rl_exprn__simple_extra_builtin(function, "math", "pow", 2, 0, rl_EXP_flt_pow).
rl_exprn__simple_extra_builtin(function, "math", "exp", 1, 0, rl_EXP_flt_exp).
rl_exprn__simple_extra_builtin(function, "math", "ln", 1, 0, rl_EXP_flt_log).
rl_exprn__simple_extra_builtin(function, "math", "log10", 1, 0,
		rl_EXP_flt_log10).
rl_exprn__simple_extra_builtin(function, "math", "log2", 1, 0,
		rl_EXP_flt_log2).
rl_exprn__simple_extra_builtin(function, "math", "sin", 1, 0, rl_EXP_flt_sin).
rl_exprn__simple_extra_builtin(function, "math", "cos", 1, 0, rl_EXP_flt_cos).
rl_exprn__simple_extra_builtin(function, "math", "tan", 1, 0, rl_EXP_flt_tan).
rl_exprn__simple_extra_builtin(function, "math", "asin", 1, 0,
		rl_EXP_flt_asin).
rl_exprn__simple_extra_builtin(function, "math", "acos", 1, 0,
		rl_EXP_flt_acos).
rl_exprn__simple_extra_builtin(function, "math", "atan", 1, 0,
		rl_EXP_flt_atan).
rl_exprn__simple_extra_builtin(function, "math", "sinh", 1, 0,
		rl_EXP_flt_sinh).
rl_exprn__simple_extra_builtin(function, "math", "cosh", 1, 0,
		rl_EXP_flt_cosh).
rl_exprn__simple_extra_builtin(function, "math", "tanh", 1, 0,
		rl_EXP_flt_tanh).

rl_exprn__simple_extra_builtin(predicate, "string", "length", 2, 0,
		rl_EXP_str_length).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

rl_exprn__aggregate(ModuleInfo, ComputeInitial, UpdateAcc, GrpByType, 
		NonGrpByType, AccType, AggCode, Decls) :-

	map__init(VarTypes),
	varset__init(VarSet),
	instmap__init_reachable(InstMap),
	rl_exprn_info_init(ModuleInfo, InstMap, VarTypes, VarSet, Info0),
	rl_exprn__aggregate_2(ComputeInitial, UpdateAcc, GrpByType,
		NonGrpByType, AccType, AggCode, Decls, Info0, _).

:- pred rl_exprn__aggregate_2(pred_proc_id::in, pred_proc_id::in,
	(type)::in, (type)::in, (type)::in, list(bytecode)::out,
	list(type)::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__aggregate_2(ComputeInitial, UpdateAcc, GrpByType,
		NonGrpByType, AccType, AggCode, Decls) -->

	rl_exprn_info_get_free_reg(GrpByType, GrpByReg),
	rl_exprn_info_get_free_reg(AccType, AccReg),

	%
	% Initialise the accumulator and group-by variables.
	%
	rl_exprn__aggregate_init(ComputeInitial, GrpByReg, GrpByType, 
		NonGrpByType, AccReg, AccType, InitCode0, GroupInitCode),

	%
	% Generate a test to check whether the current tuple is
	% in the current group.
	%
	rl_exprn__test(reg(GrpByReg), input_field(one, 0),
		GrpByType, node([rl_EXP_return_false]), TestCode),

	%
	% Generate code to update the accumulator.
	%
	rl_exprn__aggregate_update(UpdateAcc, GrpByReg, GrpByType,
		NonGrpByType, AccReg, AccType, UpdateCode),	
	{ EvalCode = tree(TestCode, UpdateCode) },

	%
	% Create the output tuple.
	%
	rl_exprn__assign(output_field(0), reg(GrpByReg),
		GrpByType, GrpByOutputCode), 
	rl_exprn__assign(output_field(1), reg(AccReg),
		AccType, AccOutputCode),
	{ ProjectCode = tree(GrpByOutputCode, AccOutputCode) },

	rl_exprn__generate_decls(ConstCode, DeclCode, Decls),

	{ InitCode = tree(DeclCode, InitCode0) },
	{ rl_exprn__generate_fragments(ConstCode, InitCode, GroupInitCode,
		EvalCode, ProjectCode, empty, AggCode) }.

%-----------------------------------------------------------------------------%

	% Generate code to initialise the accumulator for a group and
	% put the group-by variable in a known place.
:- pred rl_exprn__aggregate_init(pred_proc_id::in, reg_id::in, (type)::in,
	(type)::in, reg_id::in, (type)::in, byte_tree::out, byte_tree::out,
	rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__aggregate_init(ComputeClosure, GrpByReg, GrpByType, NonGrpByType,
		AccReg, AccType, InitCode, GroupInitCode) -->

	% Put the group-by value for this group in its place.
	rl_exprn__assign(reg(GrpByReg), input_field(one, 0),
		GrpByType, GrpByAssign),

	rl_exprn_info_get_free_reg(NonGrpByType, NonGrpByReg),
	rl_exprn__assign(reg(NonGrpByReg), input_field(one, 1),
		NonGrpByType, NonGrpByAssign),

	rl_exprn_info_get_free_reg(AccType, InitialAccReg),

	%
	% Compute the initial accumulator given the first tuple in
	% the group, and assign it to a register.
	% 
	{ Args = [GrpByReg, NonGrpByReg, InitialAccReg] },
	{ ArgTypes = [GrpByType, NonGrpByType, AccType] },
	rl_exprn__closure(ComputeClosure, Args, ArgTypes, IsConst, AccCode0),

	% Restore the initial value of the accumulator at the start
	% of a new group.
	rl_exprn__assign(reg(AccReg), reg(InitialAccReg), AccType, AccAssign),

	{ IsConst = yes ->
		% If the initial accumulator is constant, it can be
		% computed once in the init fragment, rather than
		% once per group.
		InitCode = AccCode0,
		GroupInitCode = tree(GrpByAssign, AccAssign)
	;
		InitCode = empty,
		GroupInitCode =
			tree(GrpByAssign,
			tree(NonGrpByAssign,
			tree(AccCode0,
			AccAssign
		)))
	}.

%-----------------------------------------------------------------------------%

	% Generate code to compute the new accumulator given the
	% next element in the group, then destructively update the
	% old accumulator.
:- pred rl_exprn__aggregate_update(pred_proc_id::in, reg_id::in,
	(type)::in, (type)::in, reg_id::in, (type)::in,
	byte_tree::out, rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__aggregate_update(UpdateClosure, GrpByReg, GrpByType, NonGrpByType,
		AccReg, AccType, Code) -->

	rl_exprn_info_get_free_reg(NonGrpByType, NonGrpByReg),
	rl_exprn__assign(reg(NonGrpByReg), input_field(one, 1),
		NonGrpByType, NonGrpByCode),

	% Allocate a location to collect the new accumulator.
	rl_exprn_info_get_free_reg(AccType, OutputAccReg),
	rl_exprn__assign(reg(AccReg), reg(OutputAccReg),
		AccType, AccAssignCode),

	{ Args = [GrpByReg, NonGrpByReg, AccReg, OutputAccReg] },
	{ ArgTypes = [GrpByType, NonGrpByType, AccType, AccType] },

	rl_exprn__closure(UpdateClosure, Args, ArgTypes, _, UpdateCode),
	{ Code =
		tree(NonGrpByCode,
		tree(UpdateCode,
		AccAssignCode
	)) }.

%-----------------------------------------------------------------------------%

	% Evaluate a deterministic closure to compute the initial value
	% or update the accumulator for an aggregate.
	% Return whether the input arguments are actually used in
	% constructing the outputs. If not, the closure is constant
	% and can be evaluated once, instead of once per group.
:- pred rl_exprn__closure(pred_proc_id::in, list(reg_id)::in, list(type)::in,
		bool::out, byte_tree::out,
		rl_exprn_info::in, rl_exprn_info::out) is det.

rl_exprn__closure(proc(PredId, ProcId), ArgLocs, ArgTypes, IsConst, Code) -->
	rl_exprn_info_get_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo) },

	% Create dummy variables for the arguments of the procedure.
	rl_exprn_info_get_varset(VarSet0),
	rl_exprn_info_get_vartypes(VarTypes0),
	{ list__length(ArgTypes, NumVars) },
	{ varset__new_vars(VarSet0, NumVars, ArgVars, VarSet) },
	{ map__det_insert_from_corresponding_lists(VarTypes0, 
		ArgVars, ArgTypes, VarTypes) },
	rl_exprn_info_set_varset(VarSet),
	rl_exprn_info_set_vartypes(VarTypes),

	rl_exprn_info_set_var_locs(ArgVars, ArgLocs),

	% Check if the closure depends on the input arguments.
	{ proc_info_goal(ProcInfo, Goal) },
	{ Goal = _ - GoalInfo },
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ proc_info_headvars(ProcInfo, HeadVars) },
	{ proc_info_argmodes(ProcInfo, ArgModes) },
	{ partition_args(ModuleInfo, ArgModes, HeadVars, InputArgs, _) },
	{ set__list_to_set(InputArgs, InputArgSet) },
	{ set__intersect(InputArgSet, NonLocals, UsedInputArgs) },
	( { set__empty(UsedInputArgs) } ->
		{ IsConst = yes }
	;
		{ IsConst = no }
	),

	{ Fail = node([rl_EXP_return_false]) },
	rl_exprn__call_body(PredId, ProcId, PredInfo, ProcInfo,
			Fail, ArgVars, Code).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Return the bytecode used to get a field from an input term.
:- pred rl_exprn__get_input_field_code(tuple_num::in, aditi_type::in,
		int::in, bytecode::out) is det. 

rl_exprn__get_input_field_code(one, int, Attr, rl_EXP_int_field1(Attr)).
rl_exprn__get_input_field_code(one, string, Attr, rl_EXP_str_field1(Attr)).
rl_exprn__get_input_field_code(one, float, Attr, rl_EXP_flt_field1(Attr)).
rl_exprn__get_input_field_code(one, term(_), Attr, rl_EXP_term_field1(Attr)).
rl_exprn__get_input_field_code(two, int, Attr, rl_EXP_int_field2(Attr)).
rl_exprn__get_input_field_code(two, string, Attr, rl_EXP_str_field2(Attr)).
rl_exprn__get_input_field_code(two, float, Attr, rl_EXP_flt_field2(Attr)).
rl_exprn__get_input_field_code(two, term(_), Attr, rl_EXP_term_field2(Attr)).

	% Return the bytecode used to set a field in the output term.
:- pred rl_exprn__set_output_field_code(tuple_num::in,
		aditi_type::in, int::in, bytecode::out) is det.

rl_exprn__set_output_field_code(one, int, Attr, rl_EXP_output1_int(Attr)).
rl_exprn__set_output_field_code(one, string, Attr, rl_EXP_output1_str(Attr)).
rl_exprn__set_output_field_code(one, float, Attr, rl_EXP_output1_flt(Attr)).
rl_exprn__set_output_field_code(one, term(_), Attr,
				rl_EXP_put_term_output1(Attr)).
rl_exprn__set_output_field_code(two, int, Attr, rl_EXP_output2_int(Attr)).
rl_exprn__set_output_field_code(two, string, Attr, rl_EXP_output2_str(Attr)).
rl_exprn__set_output_field_code(two, float, Attr, rl_EXP_output2_flt(Attr)).
rl_exprn__set_output_field_code(two, term(_), Attr,
				rl_EXP_put_term_output2(Attr)).

	% Return the bytecode used to extract a field from a term.
:- pred rl_exprn__get_term_arg_code(aditi_type::in,
		int::in, bytecode::out) is det.

rl_exprn__get_term_arg_code(int, Index, rl_EXP_get_int_arg(Index)).
rl_exprn__get_term_arg_code(float, Index, rl_EXP_get_flt_arg(Index)).
rl_exprn__get_term_arg_code(string, Index, rl_EXP_get_str_arg(Index)).
rl_exprn__get_term_arg_code(term(_), Index, rl_EXP_get_term_arg(Index)).

	% Return the bytecode used to set a field in a term.
:- pred rl_exprn__set_term_arg_code(aditi_type::in,
		int::in, bytecode::out) is det.

rl_exprn__set_term_arg_code(int, Index, rl_EXP_set_int_arg(Index)).
rl_exprn__set_term_arg_code(float, Index, rl_EXP_set_flt_arg(Index)).
rl_exprn__set_term_arg_code(string, Index, rl_EXP_set_str_arg(Index)).
	% This bytecode copies the argument term adjusting rule numbers
	% if the schemas of the argument term and the term having its
	% argument set do not match.
rl_exprn__set_term_arg_code(term(_), Index, rl_EXP_put_term_arg(Index)).

:- pred rl_exprn__compare_bytecode(aditi_type::in, bytecode::out) is det.

rl_exprn__compare_bytecode(int, rl_EXP_int_cmp).
rl_exprn__compare_bytecode(float, rl_EXP_flt_cmp).
rl_exprn__compare_bytecode(string, rl_EXP_str_cmp).
rl_exprn__compare_bytecode(term(_), rl_EXP_term_cmp).

%-----------------------------------------------------------------------------%

:- type aditi_type
	--->	int
	;	string
	;	float
	;	term(type).

:- pred rl_exprn__type_to_aditi_type((type)::in, aditi_type::out) is det.

rl_exprn__type_to_aditi_type(Type, AditiType) :-
	( type_to_type_id(Type, TypeId, _) ->
		( TypeId = unqualified("int") - 0 ->
			AditiType = int
		; TypeId = unqualified("character") - 0 ->
			AditiType = int
		; TypeId = unqualified("string") - 0 ->
			AditiType = string
		; TypeId = unqualified("float") - 0 ->
			AditiType = float
		;
			AditiType = term(Type)
		)
	;
		% All types in Aditi relations must be bound. This case
		% can happen if an argument of an aggregate init or update
		% closure is not used. int is a bit of a lie, but since
		% the argument is not used, it should be harmless.
		AditiType = int
	).	

:- pred rl_exprn__aditi_type_to_type(aditi_type::in, (type)::out) is det.

rl_exprn__aditi_type_to_type(int, Int) :-
	construct_type(unqualified("int") - 0, [], Int).
rl_exprn__aditi_type_to_type(float, Float) :-
	construct_type(unqualified("float") - 0, [], Float).
rl_exprn__aditi_type_to_type(string, Str) :-
	construct_type(unqualified("string") - 0, [], Str).
rl_exprn__aditi_type_to_type(term(Type), Type).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred rl_exprn__resolve_addresses(byte_tree::in, byte_tree::out) is det.

rl_exprn__resolve_addresses(ByteTree0, ByteTree) :-
	map__init(Labels0),
	rl_exprn__get_exprn_labels(0, _, Labels0, Labels,
		ByteTree0, ByteTree1),
	ResolveAddr =
		lambda([Code0::in, Code::out] is det, (
			% This is incomplete, but we don't generate any
			% of the other jump instructions.
			( Code0 = rl_EXP_jmp(Label0) ->
				map__lookup(Labels, Label0, Label),
				Code = rl_EXP_jmp(Label)
			; Code0 = rl_EXP_beqz(Label0) ->
				map__lookup(Labels, Label0, Label),
				Code = rl_EXP_beqz(Label)
			; Code0 = rl_EXP_bnez(Label0) ->
				map__lookup(Labels, Label0, Label),
				Code = rl_EXP_bnez(Label)
			; Code0 = rl_EXP_bltz(Label0) ->
				map__lookup(Labels, Label0, Label),
				Code = rl_EXP_bltz(Label)
			; Code0 = rl_EXP_blez(Label0) ->
				map__lookup(Labels, Label0, Label),
				Code = rl_EXP_blez(Label)
			; Code0 = rl_EXP_bgez(Label0) ->
				map__lookup(Labels, Label0, Label),
				Code = rl_EXP_bgez(Label)
			; Code0 = rl_EXP_bgtz(Label0) ->
				map__lookup(Labels, Label0, Label),
				Code = rl_EXP_bgtz(Label)
			; Code0 = rl_EXP_bt(Label0) ->
				map__lookup(Labels, Label0, Label),
				Code = rl_EXP_bt(Label)
			; Code0 = rl_EXP_bf(Label0) ->
				map__lookup(Labels, Label0, Label),
				Code = rl_EXP_bf(Label)
			;
				Code = Code0
			)
		)),
	rl_out__resolve_addresses(ResolveAddr, ByteTree1, ByteTree).

:- pred rl_exprn__get_exprn_labels(int::in, int::out, map(label_id, int)::in,
		map(label_id, int)::out, byte_tree::in, byte_tree::out) is det.

rl_exprn__get_exprn_labels(PC0, PC0, Labels, Labels, empty, empty).
rl_exprn__get_exprn_labels(PC0, PC, Labels0, Labels,
		tree(CodeA0, CodeB0), tree(CodeA, CodeB)) :-
	rl_exprn__get_exprn_labels(PC0, PC1, Labels0, Labels1, CodeA0, CodeA),
	rl_exprn__get_exprn_labels(PC1, PC, Labels1, Labels, CodeB0, CodeB).
rl_exprn__get_exprn_labels(PC0, PC, Labels0, Labels,
		node(Instrs0), node(Instrs)) :-
	rl_exprn__get_exprn_labels_list(PC0, PC,
		Labels0, Labels, Instrs0, Instrs).

:- pred rl_exprn__get_exprn_labels_list(int::in, int::out,
		map(label_id, int)::in, map(label_id, int)::out,
		list(bytecode)::in, list(bytecode)::out) is det.

rl_exprn__get_exprn_labels_list(PC, PC, Labels, Labels, [], []).
rl_exprn__get_exprn_labels_list(PC0, PC, Labels0, Labels,
		[Instr | Instrs0], Instrs) :-
	( Instr = rl_PROC_label(_) ->
		PC1 = PC0
	;
		functor(Instr, _, Arity),
		PC1 is PC0 + Arity + 1		% +1 for the opcode
	),
	rl_exprn__get_exprn_labels_list(PC1, PC, Labels0, Labels1,
		Instrs0, Instrs1),
	( Instr = rl_PROC_label(Label) ->
		% Register the label and remove the instruction.
		map__det_insert(Labels1, Label, PC0, Labels),
		Instrs = Instrs1
	;
		Labels = Labels1,
		Instrs = [Instr | Instrs1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type rl_lval
	--->	reg(reg_id)

		% A field in the output tuple
	;	output_field(
			int 	% field no
		)

		% A field of a term.
	;	term_arg(
			reg_id,
			cons_id,
			int,
			type		% type of the term
		).

:- type rl_rval
	--->	reg(reg_id)

	;	const(rl_const)
	
		% A field in one of the input tuples
	;	input_field(
			tuple_num,
			int		% field no
		)
		
		% An argument of a term in a register
	;	term_arg(
			rl_rval,	% register holding the term
			cons_id,
			int,		% arg no
			type		% type of the term
		).

:- type input_tuple
	--->	one
	;	two.

:- type reg_id == int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type rl_exprn_info.

:- pred rl_exprn_info_init(module_info, instmap, map(prog_var, type),
		prog_varset, rl_exprn_info).
:- mode rl_exprn_info_init(in, in, in, in, out) is det.

:- pred rl_exprn_info_init(module_info, rl_exprn_info).
:- mode rl_exprn_info_init(in, out) is det.

:- pred rl_exprn_info_get_module_info(module_info,
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_get_module_info(out, in, out) is det.

:- pred rl_exprn_info_get_instmap(instmap, rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_get_instmap(out, in, out) is det.

:- pred rl_exprn_info_set_instmap(instmap, rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_set_instmap(in, in, out) is det.

:- pred rl_exprn_info_get_vartypes(map(prog_var, type),
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_get_vartypes(out, in, out) is det.

:- pred rl_exprn_info_set_vartypes(map(prog_var, type),
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_set_vartypes(in, in, out) is det.

:- pred rl_exprn_info_get_varset(prog_varset, rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_get_varset(out, in, out) is det.

:- pred rl_exprn_info_set_varset(prog_varset, rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_set_varset(in, in, out) is det.

:- pred rl_exprn_info_get_vars(id_map(prog_var), rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_get_vars(out, in, out) is det.

:- pred rl_exprn_info_set_vars(id_map(prog_var), rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_set_vars(in, in, out) is det.

:- pred rl_exprn_info_lookup_var(prog_var, int, rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_lookup_var(in, out, in, out) is det.

:- pred rl_exprn_info_get_free_reg((type), reg_id,
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_get_free_reg(in, out, in, out) is det.

:- pred rl_exprn_info_get_next_label_id(label_id,
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_get_next_label_id(out, in, out) is det.

:- pred rl_exprn_info_lookup_const(rl_const, int,
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_lookup_const(in, out, in, out) is det.

:- pred rl_exprn_info_get_consts(id_map(rl_const), 
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_get_consts(out, in, out) is det.

:- pred rl_exprn_info_lookup_rule(pair(rl_rule, exprn_tuple), int,
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_lookup_rule(in, out, in, out) is det.

:- pred rl_exprn_info_get_rules(id_map(pair(rl_rule, exprn_tuple)), 
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_get_rules(out, in, out) is det.

:- pred rl_exprn_info_get_parent_pred_proc_ids(set(pred_proc_id),
		rl_exprn_info, rl_exprn_info) is det.
:- mode rl_exprn_info_get_parent_pred_proc_ids(out, in, out) is det.

:- pred rl_exprn_info_set_parent_pred_proc_ids(set(pred_proc_id),
		rl_exprn_info, rl_exprn_info) is det.
:- mode rl_exprn_info_set_parent_pred_proc_ids(in, in, out) is det.

:- pred rl_exprn_info_lookup_var_type(prog_var, type,
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_lookup_var_type(in, out, in, out) is det.

:- pred rl_exprn_info_set_var_locs(list(prog_var), list(reg_id),
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_set_var_locs(in, in, in, out) is det.

:- pred rl_exprn_info_set_var_loc(prog_var, reg_id,
		rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_set_var_loc(in, in, in, out) is det.

:- pred rl_exprn_info_get_decls(list(type), rl_exprn_info, rl_exprn_info).
:- mode rl_exprn_info_get_decls(out, in, out) is det.

:- type rl_exprn_info
	---> rl_exprn_info(
		module_info,
		instmap,		% not yet used.
		map(prog_var, type),
		prog_varset,
		id_map(prog_var),
		label_id,		% next label.
		id_map(rl_const),
		id_map(pair(rl_rule, exprn_tuple)),
		set(pred_proc_id),	% parent pred_proc_ids, used
					% to abort on recursion.
		list(type)		% variable declarations in reverse.
	).

:- type rl_rule
	---> rl_rule(
		string,		% mangled type name Module__Name
		string,		% mangled functor name Module__Name
		int		% arity
	).

	% Each expression has a number of tuples associated with it,
	% each of which has its own rule table.
:- type exprn_tuple
	---> 	input1
	;	input2
	;	variables
	;	output1
	;	output2
	.

:- type id_map(T) == pair(map(T, int), int).

:- pred id_map_init(id_map(T)::out) is det.

id_map_init(Empty - 0) :-
	map__init(Empty).

:- pred id_map_lookup(T::in, int::out, bool::out,
		id_map(T)::in, id_map(T)::out) is det.

id_map_lookup(Id, IdIndex, Added, Map0 - Index0, Map - Index) :-
	( map__search(Map0, Id, IdIndex0) ->
		IdIndex = IdIndex0,
		Map = Map0,
		Index = Index0,
		Added = no
	;
		IdIndex = Index0,
		Index is Index0 + 1,
		Added = yes,
		map__det_insert(Map0, Id, Index0, Map)
	).

:- pred id_map_lookup(T::in, int::out, id_map(T)::in, id_map(T)::out) is det.

id_map_lookup(Id, IdIndex, Map0, Map) :-
	id_map_lookup(Id, IdIndex, _, Map0, Map).

rl_exprn_info_init(ModuleInfo, Info0) :-
	map__init(VarTypes),
	varset__init(VarSet),
	instmap__init_reachable(InstMap),
	rl_exprn_info_init(ModuleInfo, InstMap, VarTypes, VarSet, Info0).

rl_exprn_info_init(ModuleInfo, InstMap, VarTypes, VarSet, Info) :-
	id_map_init(VarMap),
	id_map_init(ConstMap),
	id_map_init(RuleMap),
	set__init(Parents),
	Label = 0,
	Info = rl_exprn_info(ModuleInfo, InstMap, VarTypes, VarSet,
		VarMap, Label, ConstMap, RuleMap, Parents, []).

rl_exprn_info_get_module_info(A, Info, Info) :-
	Info = rl_exprn_info(A,_,_,_,_,_,_,_,_,_).
rl_exprn_info_get_instmap(B, Info, Info) :-
	Info = rl_exprn_info(_,B,_,_,_,_,_,_,_,_).
rl_exprn_info_get_vartypes(C, Info, Info) :-
	Info = rl_exprn_info(_,_,C,_,_,_,_,_,_,_).
rl_exprn_info_get_varset(D, Info, Info) :-
	Info = rl_exprn_info(_,_,_,D,_,_,_,_,_,_).
rl_exprn_info_get_vars(E, Info, Info) :-
	Info = rl_exprn_info(_,_,_,_,E,_,_,_,_,_).
rl_exprn_info_get_consts(G, Info, Info) :-
	Info = rl_exprn_info(_,_,_,_,_,_,G,_,_,_).
rl_exprn_info_get_rules(H, Info, Info) :-
	Info = rl_exprn_info(_,_,_,_,_,_,_,H,_,_).
rl_exprn_info_get_parent_pred_proc_ids(I, Info, Info) :-
	Info = rl_exprn_info(_,_,_,_,_,_,_,_,I,_).
rl_exprn_info_get_decls(J, Info, Info) :-
	Info = rl_exprn_info(_,_,_,_,_,_,_,_,_,J0),
	list__reverse(J0, J).

rl_exprn_info_set_instmap(B, Info0, Info) :-
	Info0 = rl_exprn_info(A,_,C,D,E,F,G,H,I,J),
	Info = rl_exprn_info(A,B,C,D,E,F,G,H,I,J).
rl_exprn_info_set_vartypes(C, Info0, Info) :-
	Info0 = rl_exprn_info(A,B,_,D,E,F,G,H,I,J),
	Info = rl_exprn_info(A,B,C,D,E,F,G,H,I,J).
rl_exprn_info_set_varset(D, Info0, Info) :-
	Info0 = rl_exprn_info(A,B,C,_,E,F,G,H,I,J),
	Info = rl_exprn_info(A,B,C,D,E,F,G,H,I,J).
rl_exprn_info_set_vars(E, Info0, Info) :-
	Info0 = rl_exprn_info(A,B,C,D,_,F,G,H,I,J),
	Info = rl_exprn_info(A,B,C,D,E,F,G,H,I,J).
rl_exprn_info_set_parent_pred_proc_ids(I, Info0, Info) :-
	Info0 = rl_exprn_info(A,B,C,D,E,F,G,H,_,J),
	Info = rl_exprn_info(A,B,C,D,E,F,G,H,I,J).
rl_exprn_info_get_free_reg(Type, Loc, Info0, Info) :-
	Info0 = rl_exprn_info(A,B,C,D,VarMap0,F,G,H,I,RegTypes0),
	VarMap0 = Map - Loc,
	Loc1 is Loc + 1,
	VarMap = Map - Loc1,
	RegTypes = [Type | RegTypes0],
	Info = rl_exprn_info(A,B,C,D,VarMap,F,G,H,I,RegTypes).
rl_exprn_info_lookup_var(Var, Loc, Info0, Info) :-
	Info0 = rl_exprn_info(A,B,VarTypes,D,VarMap0,F,G,H,I,RegTypes0),
	id_map_lookup(Var, Loc, Added, VarMap0, VarMap),
	( Added = yes ->
		map__lookup(VarTypes, Var, Type),
		RegTypes = [Type | RegTypes0]
	;
		RegTypes = RegTypes0
	),
	Info = rl_exprn_info(A,B,VarTypes,D,VarMap,F,G,H,I,RegTypes).
rl_exprn_info_get_next_label_id(Label0, Info0, Info) :-
	Info0 = rl_exprn_info(A,B,C,D,E,Label0,G,H,I,J),
	Label is Label0 + 1,
	Info = rl_exprn_info(A,B,C,D,E,Label,G,H,I,J).
rl_exprn_info_lookup_const(Const, Loc, Info0, Info) :-
	Info0 = rl_exprn_info(A,B,C,D,E,F,Consts0,H,I,J),
	id_map_lookup(Const, Loc, Consts0, Consts), 
	Info = rl_exprn_info(A,B,C,D,E,F,Consts,H,I,J).
rl_exprn_info_lookup_rule(Rule, Loc, Info0, Info) :-
	Info0 = rl_exprn_info(A,B,C,D,E,F,G,Rules0,I,J),
	id_map_lookup(Rule, Loc, Rules0, Rules),
	Info = rl_exprn_info(A,B,C,D,E,F,G,Rules,I,J).

rl_exprn_info_lookup_var_type(Var, Type) -->
	rl_exprn_info_get_vartypes(VarTypes),
	{ map__lookup(VarTypes, Var, Type) }.

rl_exprn_info_set_var_locs([], []) --> [].
rl_exprn_info_set_var_locs([_|_], []) -->
	{ error("rl_exprn_info_set_var_locs") }.
rl_exprn_info_set_var_locs([], [_|_]) -->
	{ error("rl_exprn_info_set_var_locs") }.
rl_exprn_info_set_var_locs([Var | Vars], [Loc | Locs]) -->
	rl_exprn_info_set_var_loc(Var, Loc),
	rl_exprn_info_set_var_locs(Vars, Locs).

rl_exprn_info_set_var_loc(Var, Loc) -->
	rl_exprn_info_get_vars(VarMap0 - NextVar),
	{ map__det_insert(VarMap0, Var, Loc, VarMap) },
	rl_exprn_info_set_vars(VarMap - NextVar).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
