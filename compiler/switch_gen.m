%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module switch_gen.

:- interface.

:- import_module hlds, llds, code_gen, code_info, code_util.

:- pred switch_gen__generate_det_switch(var, list(case),
					code_tree, code_info, code_info).
:- mode switch_gen__generate_det_switch(in, in, out, in, out) is det.

:- pred switch_gen__generate_semi_switch(var, list(case),
					code_tree, code_info, code_info).
:- mode switch_gen__generate_semi_switch(in, in, out, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module tree, list, map, std_util, require.

switch_gen__generate_det_switch(CaseVar, Cases, Instr) -->
	code_info__flush_variable(CaseVar, Code0),
	code_info__get_variable_register(CaseVar, Lval),
	{ VarCode = node(Code0) },
	switch_gen__generate_det_cases(Cases, Lval, CasesCode),
	{ Instr = tree(VarCode, CasesCode) },
	code_info__remake_code_info.

:- pred switch_gen__generate_det_cases(list(case), lval, code_tree, code_info, code_info).
:- mode switch_gen__generate_det_cases(in, in, out, in, out) is det.

switch_gen__generate_det_cases([], _Lval, empty) --> [].
switch_gen__generate_det_cases([case(Cons, Goal)|Cases], Lval, CasesCode) -->
	code_info__grab_code_info(CodeInfo),
	code_info__get_next_label(ElseLab),
	code_info__cons_id_to_abstag(Cons, Tag),
	(
		{ Tag = simple(TagNum0) }
	->
		{ TagNum = TagNum0 }
	;
		{ Tag = unsimple(TagNum1) }
	->
		{ TagNum = TagNum1 }
	;
		{ error("This can never happen") }
	),
	{ TestCode = node([
		if_tag(Lval, TagNum, ElseLab) - "Test the tag"
	]) },
		% generate the case as a semi-deterministc goal
	code_gen__generate_forced_det_goal(Goal, ThisCode),
	{ ElseLabel = node([
		label(ElseLab) - "next case"
	]) },
		% generate the rest of the cases.
	code_info__slap_code_info(CodeInfo),
	switch_gen__generate_det_cases(Cases, Lval, CasesCode0),
	{ CasesCode = tree(tree(TestCode, ThisCode),
			tree(ElseLabel, CasesCode0)) }.

%---------------------------------------------------------------------------%

switch_gen__generate_semi_switch(CaseVar, Cases, Instr) -->
	code_info__flush_variable(CaseVar, Code0),
	code_info__get_variable_register(CaseVar, Lval),
	{ VarCode = node(Code0) },
	switch_gen__generate_semi_cases(Cases, Lval, CasesCode),
	{ Instr = tree(VarCode, CasesCode) },
	code_info__remake_code_info.

:- pred switch_gen__generate_semi_cases(list(case), lval, code_tree, code_info, code_info).
:- mode switch_gen__generate_semi_cases(in, in, out, in, out) is det.

switch_gen__generate_semi_cases([], _Lval, empty) --> [].
switch_gen__generate_semi_cases([case(Cons, Goal)|Cases], Lval, CasesCode) -->
	code_info__grab_code_info(CodeInfo),
	code_info__get_next_label(ElseLab),
	code_info__cons_id_to_abstag(Cons, Tag),
	(
		{ Tag = simple(TagNum0) }
	->
		{ TagNum = TagNum0 }
	;
		{ Tag = unsimple(TagNum1) }
	->
		{ TagNum = TagNum1 }
	;
		{ error("This can never happen") }
	),
	{ TestCode = node([
		if_tag(Lval, TagNum, ElseLab) - "Test the tag"
	]) },
		% generate the case as a semi-deterministc goal
	code_gen__generate_forced_semi_goal(Goal, ThisCode),
	{ ElseLabel = node([
		label(ElseLab) - "next case"
	]) },
		% generate the rest of the cases.
	code_info__slap_code_info(CodeInfo),
	switch_gen__generate_semi_cases(Cases, Lval, CasesCode0),
	{ CasesCode = tree(tree(TestCode, ThisCode),
			tree(ElseLabel, CasesCode0)) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

