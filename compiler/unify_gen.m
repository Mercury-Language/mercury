%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module unify_gen.

:- interface.

:- import_module list, hlds, llds, code_info, code_util.

:- pred unify_gen__generate_assignment(var, var, code_tree,
							code_info, code_info).
:- mode unify_gen__generate_assignment(in, in, out, in, out) is det.

:- pred unify_gen__simple_test(var, var, code_tree, code_info, code_info).
:- mode unify_gen__simple_test(in, in, out, in, out) is det.

:- pred unify_gen__generate_construction(var, cons_id,
				list(var), list(uni_mode),
					code_tree, code_info, code_info).
:- mode unify_gen__generate_construction(in, in, in, in, out, in, out) is det.

:- pred unify_gen__generate_det_deconstruction(var, cons_id,
				list(var), list(uni_mode),
					code_tree, code_info, code_info).
:- mode unify_gen__generate_det_deconstruction(in, in, in, in, out,
							in, out) is det.

:- pred unify_gen__generate_semi_deconstruction(var, cons_id,
				list(var), list(uni_mode),
					code_tree, code_info, code_info).
:- mode unify_gen__generate_semi_deconstruction(in, in, in, in, out,
							in, out) is det.

:- pred unify_gen__generate_test(var, var, code_tree, code_info, code_info).
:- mode unify_gen__generate_test(in, in, out, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module tree, int, map, require, std_util.
:- import_module prog_io, mode_util.

:- type uni_val		--->	ref(var)
			;	lval(lval).

%---------------------------------------------------------------------------%

unify_gen__generate_assignment(VarA, VarB, empty) -->
	code_info__cashe_expression(VarA, var(VarB)).

%---------------------------------------------------------------------------%

unify_gen__generate_test(VarA, VarB, Code) -->
	code_info__flush_variable(VarA, Code0),
	code_info__get_variable_register(VarA, RegA),
	code_info__flush_variable(VarB, Code1),
	code_info__get_variable_register(VarB, RegB),
	{ CodeA = tree(node(Code0), node(Code1)) },
	code_info__get_fall_through(FallThrough),
	{ CodeB = node([
		test(lval(RegA), lval(RegB), FallThrough) - "Test for equality"
	]) },
	{ Code = tree(CodeA, CodeB) }.

%---------------------------------------------------------------------------%


unify_gen__generate_construction(Var, Tag, Args, Modes, Code) -->
	code_info__cons_id_to_abstag(Tag, AbsTag),
	(
		{ AbsTag = simple(TagNum0) }
	->
		{ TagNum = TagNum0 },
		{ First = 0 }
	;
		{ AbsTag = unsimple(TagNum) },
		{ First = 1 }
	),
	{ unify_gen__generate_cons_args(Args, RVals) },
	code_info__cashe_expression(Var, create(AbsTag, RVals)),
	code_info__flush_variable(Var, CodeA),
	code_info__get_variable_register(Var, Lval),
	{ unify_gen__make_fields_and_argvars(Args, Lval, First, TagNum,
							Fields, ArgVars) },
	unify_gen__generate_det_unify_args(Fields, ArgVars, Modes, CodeB),
	{ Code = tree(node(CodeA), CodeB) }.

:- pred unify_gen__generate_cons_args(list(var), list(rval)).
:- mode unify_gen__generate_cons_args(in, out) is det.

unify_gen__generate_cons_args([], []).
unify_gen__generate_cons_args([_Var|Vars], [unused|RVals]) :-
	unify_gen__generate_cons_args(Vars, RVals).

%---------------------------------------------------------------------------%

:- pred unify_gen__make_fields_and_argvars(list(var), lval, int, int,
						list(uni_val), list(uni_val)).
:- mode unify_gen__make_fields_and_argvars(in, in, in, in, out, out) is det.

unify_gen__make_fields_and_argvars([], _, _, _, [], []).
unify_gen__make_fields_and_argvars([Var|Vars], Lval, Field0, TagNum,
							[F|Fs], [A|As]) :-
	F = lval(field(TagNum, Lval, Field0)),
	A = ref(Var),
	Field1 is Field0 + 1,
	unify_gen__make_fields_and_argvars(Vars, Lval, Field1, TagNum, Fs, As).

%---------------------------------------------------------------------------%

unify_gen__generate_det_deconstruction(Var, Tag, Args, Modes, Code) -->
	code_info__cons_id_to_abstag(Tag, AbsTag),
	(
		{ AbsTag = simple(TagNum0) }
	->
		{ TagNum = TagNum0 },
		{ First = 0 }
	;
		{ AbsTag = unsimple(TagNum) },
		{ First = 1 }
	),
	code_info__flush_variable(Var, Code0),
	code_info__get_variable_register(Var, Lval),
	{ CodeA = node(Code0) },
	{ unify_gen__make_fields_and_argvars(Args, Lval, First, TagNum,
							Fields, ArgVars) },
	unify_gen__generate_det_unify_args(Fields, ArgVars, Modes, CodeB),
	{ Code = tree(CodeA, CodeB) }.

%---------------------------------------------------------------------------%

unify_gen__generate_semi_deconstruction(Var, Tag, Args, Modes, Code) -->
	code_info__cons_id_to_abstag(Tag, AbsTag),
	(
		{ AbsTag = simple(TagNum0) }
	->
		{ TagNum = TagNum0 },
		{ First = 0 }
	;
		{ AbsTag = unsimple(TagNum) },
		{ First = 1 }
	),
	code_info__flush_variable(Var, Code0),
	code_info__get_variable_register(Var, Lval),
	code_info__get_fall_through(FallThrough),
	{ CodeA = node(Code0) },
	{ CodeB = node([
		if_tag(Lval, TagNum, FallThrough) - "Test tag"
	]) },
	{ unify_gen__make_fields_and_argvars(Args, Lval, First, TagNum,
							Fields, ArgVars) },
	unify_gen__generate_semi_unify_args(Fields, ArgVars, Modes, CodeC),
	{ Code = tree(CodeA, tree(CodeB, CodeC)) }.

%---------------------------------------------------------------------------%

:- pred unify_gen__generate_det_unify_args(list(uni_val), list(uni_val),
			list(uni_mode), code_tree, code_info, code_info).
:- mode unify_gen__generate_det_unify_args(in, in, in, out, in, out) is det.

unify_gen__generate_det_unify_args([], [], [], empty) --> [].
unify_gen__generate_det_unify_args([L|Ls], [R|Rs], [M|Ms], Code) -->
	unify_gen__generate_det_sub_unify(L, R, M, CodeA),
	unify_gen__generate_det_unify_args(Ls, Rs, Ms, CodeB),
	{ Code = tree(CodeA, CodeB) }.

%---------------------------------------------------------------------------%

:- pred unify_gen__generate_semi_unify_args(list(uni_val), list(uni_val),
			list(uni_mode), code_tree, code_info, code_info).
:- mode unify_gen__generate_semi_unify_args(in, in, in, out, in, out) is det.

unify_gen__generate_semi_unify_args([], [], [], empty) --> [].
unify_gen__generate_semi_unify_args([L|Ls], [R|Rs], [M|Ms], Code) -->
	unify_gen__generate_semi_sub_unify(L, R, M, CodeA),
	unify_gen__generate_semi_unify_args(Ls, Rs, Ms, CodeB),
	{ Code = tree(CodeA, CodeB) }.

%---------------------------------------------------------------------------%

:- pred unify_gen__generate_det_sub_unify(uni_val, uni_val, uni_mode, code_tree,
							code_info, code_info).
:- mode unify_gen__generate_det_sub_unify(in, in, in, out, in, out) is det.

unify_gen__generate_det_sub_unify(L, R, M, Code) -->
	{ M = ((LI - RI) -> (LF - RF)) },
	code_info__get_module_info(ModuleInfo),
	(
		{ mode_is_input(ModuleInfo, (LI -> LF)) },
		{ mode_is_input(ModuleInfo, (RI -> RF)) }
	->
		% { true }
		{ error("Det unifications may not contain tests") }
	;
		{ mode_is_input(ModuleInfo, (LI -> LF)) },
		{ mode_is_output(ModuleInfo, (RI -> RF)) }
	->
		unify_gen__generate_sub_assign(R, L, Code)
	;
		{ mode_is_output(ModuleInfo, (LI -> LF)) },
		{ mode_is_input(ModuleInfo, (RI -> RF)) }
	->
		unify_gen__generate_sub_assign(L, R, Code)
	;
		{ mode_is_output(ModuleInfo, (LI -> LF)) },
		{ mode_is_output(ModuleInfo, (RI -> RF)) }
	->
		{ error("Some strange unify") }
	;
		{ Code = empty } % free-free - ignore
	).

%---------------------------------------------------------------------------%

:- pred unify_gen__generate_semi_sub_unify(uni_val, uni_val, uni_mode, code_tree,
							code_info, code_info).
:- mode unify_gen__generate_semi_sub_unify(in, in, in, out, in, out) is det.

unify_gen__generate_semi_sub_unify(L, R, M, Code) -->
	{ M = ((LI - RI) -> (LF - RF)) },
	code_info__get_module_info(ModuleInfo),
	(
		{ mode_is_input(ModuleInfo, (LI -> LF)) },
		{ mode_is_input(ModuleInfo, (RI -> RF)) }
	->
		unify_gen__generate_sub_test(L, R, Code)
	;
		{ mode_is_input(ModuleInfo, (LI -> LF)) },
		{ mode_is_output(ModuleInfo, (RI -> RF)) }
	->
		unify_gen__generate_sub_assign(R, L, Code)
	;
		{ mode_is_output(ModuleInfo, (LI -> LF)) },
		{ mode_is_input(ModuleInfo, (RI -> RF)) }
	->
		unify_gen__generate_sub_assign(L, R, Code)
	;
		{ mode_is_output(ModuleInfo, (LI -> LF)) },
		{ mode_is_output(ModuleInfo, (RI -> RF)) }
	->
		{ error("Some strange unify") }
	;
		{ Code = empty } % free-free - ignore
	).

%---------------------------------------------------------------------------%

:- pred unify_gen__generate_sub_assign(uni_val, uni_val, code_tree,
							code_info, code_info).
:- mode unify_gen__generate_sub_assign(in, in, out, in, out) is det.

unify_gen__generate_sub_assign(lval(Lval), lval(Rval), Code) -->
	{ Code = node([
		assign(Lval, lval(Rval)) - "Copy field"
	]) }.
unify_gen__generate_sub_assign(lval(Lval), ref(Var), Code) -->
	code_info__flush_variable(Var, Code0),
	code_info__get_variable_register(Var, Source),
	{ Code = tree(
		node(Code0),
		node([
			assign(Lval, lval(Source)) - "Copy value"
		])
	) }.
unify_gen__generate_sub_assign(ref(Var), lval(Rval), empty) -->
	code_info__cashe_expression(Var, lval(Rval)).
unify_gen__generate_sub_assign(ref(Lvar), ref(Rvar), empty) -->
	code_info__cashe_expression(Lvar, var(Rvar)).

%---------------------------------------------------------------------------%

:- pred unify_gen__generate_sub_test(uni_val, uni_val, code_tree,
							code_info, code_info).
:- mode unify_gen__generate_sub_test(in, in, out, in, out) is det.

unify_gen__generate_sub_test(lval(Lval), lval(Rval), Code) -->
	code_info__get_fall_through(FallThrough),
	{ Code = node([
		test(lval(Lval), lval(Rval), FallThrough) -
				"simple test in [de]construction"
	]) }.
unify_gen__generate_sub_test(lval(Lval), ref(Rvar), Code) -->
	code_info__flush_variable(Rvar, Code0),
	code_info__get_variable_register(Rvar, Rval),
	code_info__get_fall_through(FallThrough),
	{ Code = tree(
		node(Code0),
		node([
			test(lval(Lval), lval(Rval), FallThrough) -
					"simple test in [de]construction"
		])
	)}.
unify_gen__generate_sub_test(ref(Lvar), lval(Rval), Code) -->
	code_info__flush_variable(Lvar, Code0),
	code_info__get_variable_register(Lvar, Lval),
	code_info__get_fall_through(FallThrough),
	{ Code = tree(
		node(Code0),
		node([
			test(lval(Lval), lval(Rval), FallThrough) -
					"simple test in [de]construction"
		])
	)}.
unify_gen__generate_sub_test(ref(Lvar), ref(Rvar), Code) -->
	code_info__flush_variable(Lvar, Code0),
	code_info__get_variable_register(Lvar, Lval),
	code_info__flush_variable(Rvar, Code0),
	code_info__get_variable_register(Rvar, Rval),
	code_info__get_fall_through(FallThrough),
	{ Code = tree(
		node(Code0),
		node([
			test(lval(Lval), lval(Rval), FallThrough) -
					"simple test in [de]construction"
		])
	)}.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
