%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_switch_gen.m
% Author: fjh (adapted from switch_gen.m)
%
% This module handles the generation of code for switches for the MLDS back-end.
% Switches are disjunctions that do not require backtracking.  They are
% detected in switch_detection.m.  This is the module that determines what
% sort of indexing to use for each switch and then actually generates the
% code.  The code here is quite similar to the code in switch_gen.m, which
% does the same thing for the LLDS back-end.
%
% The following describes the different forms of indexing that we could use.
% Note that currently most of these are not implemented!
% The ones that are not are marked NYI (for "not yet implemented").
%
% 1.	For switches on atomic data types (int, char, enums), there are
%	several possibilities.
%	a)  If all the alternative goals for a switch on an atomic data type
%	    contain only construction unifications of constants, then we
%	    should generate a dense lookup table (an array) for each output
%	    variable of the switch, rather than computed goto, so that
%	    executing the switch becomes a matter of doing an array index for
%	    each output variable. (NYI)
%	b)  If the cases are not sparse, and the target supports computed
%	    gotos, we should use a computed_goto, unless the target supports
%	    switch statements and the `--prefer-switch' option is set. (NYI)
%	c)  If the target supports switch statements,
%	    we generate an MLDS switch statement.
%
% 2.	For switches on strings, there are several possibilities.
%	a)  If the target supports indirect gotos, we should we lookup the
%           address to jump to in a hash table (e.g. using open addressing to
%           resolve hash collisions), and then jump to it using an indirect
%	    goto, unless the target supports string switch statements and
%	    the `--prefer-switch' option is set. (NYI)
%	c)  If the target supports string switches,
%	    we generate an MLDS switch statement.
%
% 3.	For switches on discriminated union types, we generate code that does
%	indexing first on the primary tag, and then on the secondary tag (if
%	the primary tag is shared between several function symbols). The
%	indexing code for switches on both primary and secondary tags can be
%	in the form of a try-me-else chain, a try chain, a dense jump table
%	or a binary search. (NYI)
%
%	For all other cases (or if the --smart-indexing option was
%	disabled), we just generate a chain of if-then-elses.
%
% TODO:
%	- implement the things marked NYI above
%	- optimize switches so that the recursive case comes first
%	  (see switch_gen.m).
%
%-----------------------------------------------------------------------------%

:- module ml_switch_gen.

:- interface.

:- import_module prog_data, hlds_goal, hlds_data, mlds, ml_code_util.
:- import_module llds. % XXX for code_model
:- import_module globals.

:- import_module list.

	% Generate MLDS code for a switch.
	%
:- pred ml_gen_switch(prog_var, can_fail, list(case), code_model, prog_context,
			mlds__defns, mlds__statements,
			ml_gen_info, ml_gen_info).
:- mode ml_gen_switch(in, in, in, in, in, out, out, in, out) is det.

% The following types are exported to the modules that implement
% specialized kinds of switches.

% An ml_extended_case is an HLDS case annotated with some additional info.
:- type ml_extended_case ---> case(int, cons_tag, cons_id, hlds_goal).
:- type ml_cases_list == list(ml_extended_case).

	% Succeed iff the target supports the specified construct.
:- pred target_supports_int_switch(globals::in) is semidet.
:- pred target_supports_string_switch(globals::in) is semidet.
:- pred target_supports_goto(globals::in) is semidet.
:- pred target_supports_computed_goto(globals::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

% These ones are not yet implemented yet:
% :- import_module ml_tag_switch, ml_lookup_switch.
:- import_module ml_dense_switch, ml_string_switch.
:- import_module ml_code_gen, ml_unify_gen, ml_code_util, type_util.
:- import_module options.

:- import_module bool, int, string, map, tree, std_util, require.

:- type ml_switch_category
	--->	atomic_switch	% a switch on int/char/enum
	;	string_switch
	;	tag_switch
	;	other_switch.

%-----------------------------------------------------------------------------%

	% Choose which method to use to generate the switch.
	% CanFail says whether the switch covers all cases.

ml_gen_switch(CaseVar, CanFail, Cases, CodeModel, Context,
		MLDS_Decls, MLDS_Statements) -->
	%
	% Lookup the representation of the constructors for the tag tests
	% and their corresponding priorities.
	%
	ml_switch_lookup_tags(Cases, CaseVar, TaggedCases0),
	%
	% Sort the cases according to the priority of their tag tests.
	%
	{ list__sort_and_remove_dups(TaggedCases0, TaggedCases) },

	%
	% Figure out what kind of switch this is
	%
	ml_switch_gen__determine_category(CaseVar, SwitchCategory),
	ml_gen_info_get_globals(Globals),
	{ globals__lookup_bool_option(Globals, smart_indexing, Indexing) },
	(
/**************
XXX Lookup switches are NYI
		{ Indexing = yes },
		{ SwitchCategory = atomic_switch },
		% Note that if/when the MLDS back-end supports execution
		% tracing, we would also need to check that tracing is not
		% enabled.
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, lookup_switch_size,
			LookupSize) },
		{ NumCases >= LookupSize },
		{ globals__lookup_int_option(Globals, lookup_switch_req_density,
			ReqDensity) },
		lookup_switch__is_lookup_switch(CaseVar, TaggedCases, GoalInfo,
			CanFail, ReqDensity, 
			CodeModel, FirstVal, LastVal, NeedRangeCheck,
			NeedBitVecCheck, OutVars, CaseVals)
	->
		{ MaybeEnd = MaybeEndPrime },
		ml_lookup_switch__generate(CaseVar, OutVars, CaseVals,
			FirstVal, LastVal, NeedRangeCheck, NeedBitVecCheck,
			MLDS_Decls, MLDS_Statements)
	;
**************/
		%
		% Try using a "dense" (computed goto) switch
		%
		{ Indexing = yes },
		{ SwitchCategory = atomic_switch },
		{ target_supports_computed_goto(Globals) },
		\+ {
			target_supports_int_switch(Globals),
			globals__lookup_bool_option(Globals, prefer_switch, yes)
		},
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, dense_switch_size,
			DenseSize) },
		{ NumCases >= DenseSize },
		{ globals__lookup_int_option(Globals, dense_switch_req_density,
			ReqDensity) },
		ml_dense_switch__is_dense_switch(CaseVar, TaggedCases, CanFail,
			ReqDensity, FirstVal, LastVal, CanFail1)
	->
		ml_dense_switch__generate(TaggedCases, FirstVal, LastVal,
			CaseVar, CodeModel, CanFail1, Context,
			MLDS_Decls, MLDS_Statements)
	;
		%
		% Try using a string hash switch
		%
		{ Indexing = yes },
		{ SwitchCategory = string_switch },
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, string_switch_size,
			StringSize) },
		{ NumCases >= StringSize },
		% We can implement string hash switches using either
		% computed gotos or int switches.
		(
			{ target_supports_computed_goto(Globals) }
		;
			{ target_supports_int_switch(Globals) }
		),
		% XXX Currently string hash switches always use gotos.
		% We should change that, so that we can use string hash
		% switches for the Java back-end too.
		{ target_supports_goto(Globals) },
		% OK, we could use a string hash switch.  But should we?
		% We may prefer to do a direct-mapped string switch.
		\+ {
			target_supports_string_switch(Globals),
			globals__lookup_bool_option(Globals, prefer_switch, yes)
		}
	->
		ml_string_switch__generate(TaggedCases, CaseVar, CodeModel,
			CanFail, Context, MLDS_Decls, MLDS_Statements)
	;
/**********
XXX Tag switches are NYI
		%
		% Try using a tag switch
		%
		{ Indexing = yes },
		{ SwitchCategory = tag_switch },
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, tag_switch_size,
			TagSize) },
		{ NumCases >= TagSize }
	->
		ml_tag_switch__generate(TaggedCases, CaseVar, CodeModel, CanFail,
			Context, MLDS_Decls, MLDS_Statements)
	;
**********/
		%
		% Try using a "direct-mapped" switch
		%
		{ Indexing = yes },
		{ target_supports_switch(SwitchCategory, Globals) }
	->
		ml_switch_generate_mlds_switch(TaggedCases, CaseVar,
			CodeModel, CanFail, Context,
			MLDS_Decls, MLDS_Statements)
	;
		%
		% The fallback method: if all else fails, generate an
		% if-then-else chain which tests each of the cases in turn.
		%
		ml_switch_generate_if_else_chain(TaggedCases, CaseVar,
			CodeModel, CanFail, Context,
			MLDS_Decls, MLDS_Statements)
	).

%-----------------------------------------------------------------------------%

:- pred target_supports_switch(ml_switch_category::in, globals::in)
	is semidet.
target_supports_switch(SwitchCategory, Globals) :-
	(
		SwitchCategory = atomic_switch,
		target_supports_int_switch(Globals)
	;
		SwitchCategory = string_switch,
		target_supports_string_switch(Globals)
	).

target_supports_int_switch(Globals) :-
	globals__get_target(Globals, Target),
	target_supports_int_switch_2(Target) = yes.

target_supports_string_switch(Globals) :-
	globals__get_target(Globals, Target),
	target_supports_string_switch_2(Target) = yes.

target_supports_goto(Globals) :-
	globals__get_target(Globals, Target),
	target_supports_goto_2(Target) = yes.

target_supports_computed_goto(Globals) :-
	globals__get_target(Globals, Target),
	target_supports_computed_goto_2(Target) = yes.

:- func target_supports_int_switch_2(compilation_target) = bool.
:- func target_supports_string_switch_2(compilation_target) = bool.
:- func target_supports_goto_2(compilation_target) = bool.
:- func target_supports_computed_goto_2(compilation_target) = bool.

target_supports_int_switch_2(c) = yes.
target_supports_int_switch_2(il) = no.
target_supports_int_switch_2(java) = yes.
% target_supports_int_switch_2(c_sharp) = yes.

target_supports_string_switch_2(c) = no.
target_supports_string_switch_2(il) = no.
target_supports_string_switch_2(java) = no.
% target_supports_string_switch_2(c_sharp) = yes.

target_supports_computed_goto_2(c) = yes.
target_supports_computed_goto_2(il) = yes.
target_supports_computed_goto_2(java) = no.
% target_supports_computed_goto_2(c_sharp) = yes.

target_supports_goto_2(c) = yes.
target_supports_goto_2(il) = yes.
target_supports_goto_2(java) = no.
% target_supports_goto_2(c_sharp) = yes.

%-----------------------------------------------------------------------------%

	% We categorize switches according to whether the value
	% being switched on is an atomic type, a string, or
	% something more complicated.

:- pred ml_switch_gen__determine_category(prog_var, ml_switch_category,
	ml_gen_info, ml_gen_info).
:- mode ml_switch_gen__determine_category(in, out, in, out) is det.

ml_switch_gen__determine_category(CaseVar, SwitchCategory) -->
	ml_variable_type(CaseVar, Type),
	=(MLGenInfo),
	{ ml_gen_info_get_module_info(MLGenInfo, ModuleInfo) },
	{ type_util__classify_type(Type, ModuleInfo, TypeCategory) },
	{ ml_switch_gen__type_cat_to_switch_cat(TypeCategory, SwitchCategory) }.

:- pred ml_switch_gen__type_cat_to_switch_cat(builtin_type, ml_switch_category).
:- mode ml_switch_gen__type_cat_to_switch_cat(in, out) is det.

ml_switch_gen__type_cat_to_switch_cat(enum_type, atomic_switch).
ml_switch_gen__type_cat_to_switch_cat(int_type,  atomic_switch).
ml_switch_gen__type_cat_to_switch_cat(char_type, atomic_switch).
ml_switch_gen__type_cat_to_switch_cat(float_type, other_switch).
ml_switch_gen__type_cat_to_switch_cat(str_type,  string_switch).
ml_switch_gen__type_cat_to_switch_cat(pred_type, other_switch).
ml_switch_gen__type_cat_to_switch_cat(user_type, tag_switch).
ml_switch_gen__type_cat_to_switch_cat(polymorphic_type, other_switch).
ml_switch_gen__type_cat_to_switch_cat(tuple_type, other_switch).

%-----------------------------------------------------------------------------%

	% Look up the representation (tag) for the cons_id in each case.
	% Also look up the priority of each tag test.
	%
:- pred ml_switch_lookup_tags(list(case), prog_var, ml_cases_list,
				ml_gen_info, ml_gen_info).
:- mode ml_switch_lookup_tags(in, in, out, in, out) is det.

ml_switch_lookup_tags([], _, []) --> [].
ml_switch_lookup_tags([Case | Cases], Var, [TaggedCase | TaggedCases]) -->
	{ Case = case(ConsId, Goal) },
	ml_variable_type(Var, Type),
	ml_cons_id_to_tag(ConsId, Type, Tag),
	{ ml_switch_priority(Tag, Priority) },
	{ TaggedCase = case(Priority, Tag, ConsId, Goal) },
	ml_switch_lookup_tags(Cases, Var, TaggedCases).

	% Return the priority of a tag test.
	% A low number here indicates a high priority.
	% We prioritize the tag tests so that the cheapest
	% (most efficient) ones come first.
	%
:- pred ml_switch_priority(cons_tag, int).
:- mode ml_switch_priority(in, out) is det.

ml_switch_priority(no_tag, 0).			% should never occur
ml_switch_priority(int_constant(_), 1).
ml_switch_priority(shared_local_tag(_, _), 1).
ml_switch_priority(unshared_tag(_), 2).
ml_switch_priority(float_constant(_), 3).
ml_switch_priority(shared_remote_tag(_, _), 4).
ml_switch_priority(string_constant(_), 5).
	% The following tags should all never occur in switches.
ml_switch_priority(pred_closure_tag(_, _, _), 6).
ml_switch_priority(code_addr_constant(_, _), 6).
ml_switch_priority(type_ctor_info_constant(_, _, _), 6).
ml_switch_priority(base_typeclass_info_constant(_, _, _), 6).
ml_switch_priority(tabling_pointer_constant(_, _), 6).

%-----------------------------------------------------------------------------%

	% Generate a chain of if-then-elses to test each case in turn.
	%
:- pred ml_switch_generate_if_else_chain(list(ml_extended_case), prog_var,
	code_model, can_fail, prog_context, mlds__defns, mlds__statements,
	ml_gen_info, ml_gen_info).
:- mode ml_switch_generate_if_else_chain(in, in, in, in, in, out, out,
	in, out) is det.

ml_switch_generate_if_else_chain([], _Var, CodeModel, CanFail, Context,
		[], MLDS_Statements) -->
	( { CanFail = can_fail } ->
		ml_gen_failure(CodeModel, Context, MLDS_Statements)
	;
		{ error("switch failure") }
	).
ml_switch_generate_if_else_chain([Case | Cases], Var, CodeModel, CanFail,
		Context, MLDS_Decls, MLDS_Statements) -->
	{ Case = case(_, _Tag, ConsId, Goal) },
	(
		{ Cases = [], CanFail = cannot_fail }
	->
		ml_gen_goal(CodeModel, Goal, MLDS_Decls, MLDS_Statements)
	;
		ml_gen_tag_test(Var, ConsId, TagTestDecls, TagTestStatements,
			TagTestExpression),
		ml_gen_goal(CodeModel, Goal, GoalStatement),
		ml_switch_generate_if_else_chain(Cases, Var, CodeModel,
			CanFail, Context, RestDecls, RestStatements),
		{ Rest = ml_gen_block(RestDecls, RestStatements, Context) },
		{ IfStmt = if_then_else(TagTestExpression,
				GoalStatement, yes(Rest)) },
		{ IfStatement = mlds__statement(IfStmt,
			mlds__make_context(Context)) },
		{ MLDS_Decls = TagTestDecls },
		{ MLDS_Statements = list__append(TagTestStatements,
			[IfStatement]) }
	).

%-----------------------------------------------------------------------------%

	% Generate an MLDS switch.
	% This is used for "direct-mapped" switches, where we map a
	% Mercury switch directly to a switch in the target language.
	%
:- pred ml_switch_generate_mlds_switch(list(ml_extended_case), prog_var,
	code_model, can_fail, prog_context, mlds__defns, mlds__statements,
	ml_gen_info, ml_gen_info).
:- mode ml_switch_generate_mlds_switch(in, in, in, in, in, out, out,
	in, out) is det.

ml_switch_generate_mlds_switch(Cases, Var, CodeModel, CanFail,
		Context, MLDS_Decls, MLDS_Statements) -->
	ml_variable_type(Var, Type),
	ml_gen_type(Type, MLDS_Type),
	ml_gen_var(Var, Lval),
	{ Rval = mlds__lval(Lval) },
	ml_switch_generate_mlds_cases(Cases, CodeModel, MLDS_Cases),
	ml_switch_generate_default(CanFail, CodeModel, Context, Default),
	{ SwitchStmt = switch(MLDS_Type, Rval, MLDS_Cases, Default) },
	{ SwitchStatement = mlds__statement(SwitchStmt,
		mlds__make_context(Context)) },
	{ MLDS_Decls = [] },
	{ MLDS_Statements = [SwitchStatement] }.

:- pred ml_switch_generate_mlds_cases(list(ml_extended_case),
		code_model, list(mlds__switch_case), ml_gen_info, ml_gen_info).
:- mode ml_switch_generate_mlds_cases(in, in, out, in, out) is det.

ml_switch_generate_mlds_cases([], _, []) --> [].
ml_switch_generate_mlds_cases([Case | Cases], CodeModel,
		[MLDS_Case | MLDS_Cases]) -->
	ml_switch_generate_mlds_case(Case, CodeModel, MLDS_Case),
	ml_switch_generate_mlds_cases(Cases, CodeModel, MLDS_Cases).

:- pred ml_switch_generate_mlds_case(ml_extended_case, code_model,
		mlds__switch_case, ml_gen_info, ml_gen_info).
:- mode ml_switch_generate_mlds_case(in, in, out, in, out) is det.

ml_switch_generate_mlds_case(Case, CodeModel, MLDS_Case) -->
	{ Case = case(_Priority, Tag, _ConsId, Goal) },
	( { Tag = int_constant(Int) } ->
		{ Rval = const(int_const(Int)) }
	; { Tag = string_constant(String) } ->
		{ Rval = const(string_const(String)) }
	;
		{ error("ml_switch_gen.m: invalid tag type") }
	),
	ml_gen_goal(CodeModel, Goal, MLDS_Statement),
	{ MLDS_Case = [match_value(Rval)] - MLDS_Statement }.

	% Generate an appropriate default for a switch.
	%
:- pred ml_switch_generate_default(can_fail::in, code_model::in,
		prog_context::in, switch_default::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_switch_generate_default(CanFail, CodeModel, Context, Default) -->
	(
		{ CanFail = can_fail },
		ml_gen_failure(CodeModel, Context, FailStatements),
		( { FailStatements = [] } ->
			{ Default = default_do_nothing }
		;
			{ Fail = ml_gen_block([], FailStatements, Context) },
			{ Default = default_case(Fail) }
		)
	;
		{ CanFail = cannot_fail },
		{ Default = default_is_unreachable }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
