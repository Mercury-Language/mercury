%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_switch_gen.m
% Author: fjh (adapted from switch_gen.m)
%
% This module handles the generation of code for switches for the MLDS
% back-end. Switches are disjunctions that do not require backtracking.
% They are detected in switch_detection.m.  This is the module that determines
% what sort of indexing to use for each switch and then actually generates the
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
%	a)  If the target supports indirect gotos, we should look up the
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

:- import_module prog_data.
:- import_module hlds_goal, hlds_data.
:- import_module code_model.
:- import_module mlds, ml_code_util.
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

	% Generate an appropriate default for a switch.
	%
:- pred ml_switch_generate_default(can_fail::in, code_model::in,
		prog_context::in, switch_default::out,
		ml_gen_info::in, ml_gen_info::out) is det.

	% Succeed iff the target supports the specified construct.
:- pred target_supports_int_switch(globals::in) is semidet.
:- pred target_supports_string_switch(globals::in) is semidet.
:- pred target_supports_goto(globals::in) is semidet.
:- pred target_supports_computed_goto(globals::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_tag_switch, ml_string_switch.
:- import_module ml_code_gen, ml_unify_gen, ml_code_util, ml_simplify_switch.
:- import_module switch_util, type_util.
:- import_module foreign, options.

:- import_module bool, int, string, map, tree, std_util, require.

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
	% Figure out what kind of switch this is.
	%
	ml_switch_gen__determine_category(CaseVar, SwitchCategory),
	ml_gen_info_get_globals(Globals),
	{ globals__lookup_bool_option(Globals, smart_indexing, Indexing) },
	(
		% Check for a switch on a type whose representation
		% uses reserved addresses.
		{ list__member(Case, TaggedCases) },	
		{ Case = case(_Priority, Tag, _ConsId, _Goal) },
		{
			Tag = reserved_address(_)
		;
			Tag = shared_with_reserved_addresses(_, _)
		}
	->
		% XXX This may be inefficient in some cases.
		ml_switch_generate_if_else_chain(TaggedCases, CaseVar,
			CodeModel, CanFail, Context,
			MLDS_Decls, MLDS_Statements)
	;
		
/**************
XXX Lookup switches are NYI
When we do get around to implementing them,
they should probably be handled in ml_simplify_switch rather than here.
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
		% Try using a string hash switch.
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
		% XXX Currently string hash switches always use gotos
		% (to break out of the hash chain loop).
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
		%
		% Try using a tag switch.
		%
		{ Indexing = yes },
		{ SwitchCategory = tag_switch },
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, tag_switch_size,
			TagSize) },
		{ NumCases >= TagSize },
		{ target_supports_int_switch(Globals) }
	->
		ml_tag_switch__generate(TaggedCases, CaseVar, CodeModel,
			CanFail, Context, MLDS_Decls, MLDS_Statements)
	;
		%
		% Try using a "direct-mapped" switch.
		% This also handles dense (computed goto) switches --
		% for those, we first generate a direct-mapped switch,
		% and then convert it into a computed goto switch
		% in ml_simplify_switch.
		%
		{ Indexing = yes },
		(
			{ target_supports_switch(SwitchCategory, Globals) }
		;
			{ SwitchCategory = atomic_switch },
			{ target_supports_computed_goto(Globals) }
		)
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

:- pred target_supports_switch(switch_category::in, globals::in)
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
target_supports_int_switch_2(asm) = yes.	% asm means via gnu back-end
target_supports_int_switch_2(il) = no.
target_supports_int_switch_2(java) = yes.
% target_supports_int_switch_2(c_sharp) = yes.

target_supports_string_switch_2(c) = no.
target_supports_string_switch_2(asm) = no.	% asm means via gnu back-end
target_supports_string_switch_2(il) = no.
target_supports_string_switch_2(java) = no.
% target_supports_string_switch_2(c_sharp) = yes.

target_supports_computed_goto_2(c) = yes.
target_supports_computed_goto_2(asm) = no.	% asm means via gnu back-end
	% XXX for asm, it should be `yes', but currently
	% computed gotos are not yet implemented in gcc.m.
target_supports_computed_goto_2(il) = yes.
target_supports_computed_goto_2(java) = no.
% target_supports_computed_goto_2(c_sharp) = no.

target_supports_goto_2(c) = yes.
target_supports_goto_2(asm) = yes.		% asm means via gnu back-end
target_supports_goto_2(il) = yes.
target_supports_goto_2(java) = no.
% target_supports_goto_2(c_sharp) = yes.

%-----------------------------------------------------------------------------%

	% We categorize switches according to whether the value
	% being switched on is an atomic type, a string, or
	% something more complicated.

:- pred ml_switch_gen__determine_category(prog_var, switch_category,
	ml_gen_info, ml_gen_info).
:- mode ml_switch_gen__determine_category(in, out, in, out) is det.

ml_switch_gen__determine_category(CaseVar, SwitchCategory) -->
	ml_variable_type(CaseVar, Type),
	=(MLGenInfo),
	{ ml_gen_info_get_module_info(MLGenInfo, ModuleInfo) },
	{ type_util__classify_type(Type, ModuleInfo, TypeCategory) },
	{ switch_util__type_cat_to_switch_cat(TypeCategory, SwitchCategory) }.

%-----------------------------------------------------------------------------%

	% Look up the representation (tag) for the cons_id in each case.
	% Also look up the priority of each tag test.
	%
:- pred ml_switch_lookup_tags(list(case), prog_var, cases_list,
				ml_gen_info, ml_gen_info).
:- mode ml_switch_lookup_tags(in, in, out, in, out) is det.

ml_switch_lookup_tags([], _, []) --> [].
ml_switch_lookup_tags([Case | Cases], Var, [TaggedCase | TaggedCases]) -->
	{ Case = case(ConsId, Goal) },
	ml_variable_type(Var, Type),
	ml_cons_id_to_tag(ConsId, Type, Tag),
	{ switch_util__switch_priority(Tag, Priority) },
	{ TaggedCase = case(Priority, Tag, ConsId, Goal) },
	ml_switch_lookup_tags(Cases, Var, TaggedCases).

%-----------------------------------------------------------------------------%

	% Generate a chain of if-then-elses to test each case in turn.
	%
:- pred ml_switch_generate_if_else_chain(list(extended_case), prog_var,
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
:- pred ml_switch_generate_mlds_switch(list(extended_case), prog_var,
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
	ml_switch_gen_range(MLDS_Type, Range),
	ml_switch_generate_mlds_cases(Cases, CodeModel, MLDS_Cases),
	ml_switch_generate_default(CanFail, CodeModel, Context, Default),
	{ SwitchStmt0 = switch(MLDS_Type, Rval, Range, MLDS_Cases, Default) },
	{ MLDS_Context = mlds__make_context(Context) },
	ml_simplify_switch(SwitchStmt0, MLDS_Context, SwitchStatement),
	{ MLDS_Decls = [] },
	{ MLDS_Statements = [SwitchStatement] }.

:- pred ml_switch_gen_range(mlds__type, mlds__switch_range,
		ml_gen_info, ml_gen_info).
:- mode ml_switch_gen_range(in, out, in, out) is det.

ml_switch_gen_range(MLDS_Type, Range) -->
	=(MLGenInfo),
	{
		ml_gen_info_get_module_info(MLGenInfo, ModuleInfo),
		ExportedType = to_exported_type(ModuleInfo, Type),
		MLDS_Type = mercury_type(Type, TypeCategory, ExportedType),
		switch_util__type_range(TypeCategory, Type, ModuleInfo,
			MinRange, MaxRange)
	->
		Range = range(MinRange, MaxRange)
	;
		Range = range_unknown
	}.

:- pred ml_switch_generate_mlds_cases(list(extended_case),
		code_model, list(mlds__switch_case), ml_gen_info, ml_gen_info).
:- mode ml_switch_generate_mlds_cases(in, in, out, in, out) is det.

ml_switch_generate_mlds_cases([], _, []) --> [].
ml_switch_generate_mlds_cases([Case | Cases], CodeModel,
		[MLDS_Case | MLDS_Cases]) -->
	ml_switch_generate_mlds_case(Case, CodeModel, MLDS_Case),
	ml_switch_generate_mlds_cases(Cases, CodeModel, MLDS_Cases).

:- pred ml_switch_generate_mlds_case(extended_case, code_model,
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
