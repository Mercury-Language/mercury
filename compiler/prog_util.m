%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001, 2003-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% main author: fjh

% various utility predicates acting on the parse tree data
% structure defined in prog_data.m.

:- module parse_tree__prog_util.

:- interface.

:- import_module parse_tree__prog_data.

:- import_module std_util, list, term.

%-----------------------------------------------------------------------------%

	% Returns the name of the module containing public builtins;
	% originally this was "mercury_builtin", but it later became
	% just "builtin", and it may eventually be renamed "std:builtin".
	% This module is automatically imported, as if via `import_module'.

:- pred mercury_public_builtin_module(sym_name::out) is det.
:- func mercury_public_builtin_module = sym_name.

	% Returns the name of the module containing private builtins;
	% traditionally this was "mercury_builtin", but it later became
	% "private_builtin", and it may eventually be renamed
	% "std:private_builtin".
	% This module is automatically imported, as if via `use_module'.

:- pred mercury_private_builtin_module(sym_name::out) is det.
:- func mercury_private_builtin_module = sym_name.

	% Returns the name of the module containing builtins for tabling;
	% originally these were in "private_builtin", but they
	% may soon be moved into a separate module.
	% This module is automatically imported iff tabling is enabled.

:- pred mercury_table_builtin_module(sym_name::out) is det.
:- func mercury_table_builtin_module = sym_name.

	% Returns the name of the module containing the builtins for
	% deep profiling.
	% This module is automatically imported iff deep profiling is
	% enabled.
:- pred mercury_profiling_builtin_module(sym_name::out) is det.
:- func mercury_profiling_builtin_module = sym_name.

	% Returns the name of the module containing the builtins for
	% term size profiling.
	% This module is automatically imported iff term size profiling is
	% enabled.
:- pred mercury_term_size_prof_builtin_module(sym_name::out) is det.
:- func mercury_term_size_prof_builtin_module = sym_name.

	% Returns the name of the module containing the public builtins
	% used by the Aditi transaction interface, currently "aditi".
	% This module is not automatically imported (XXX should it be?).

:- pred aditi_public_builtin_module(sym_name::out) is det.
:- func aditi_public_builtin_module = sym_name.

	% Returns the name of the module containing the private builtins
	% used by the Aditi transaction interface, currently
	% "aditi_private_builtin".
	% This module is automatically imported iff the Aditi interface
	% is enabled.

:- pred aditi_private_builtin_module(sym_name::out) is det.
:- func aditi_private_builtin_module = sym_name.

	% Succeeds iff the specified module is one of the
	% builtin modules listed above which are automatically imported.

:- pred any_mercury_builtin_module(sym_name::in) is semidet.

%-----------------------------------------------------------------------------%

	% Given a symbol name, return its unqualified name.

:- pred unqualify_name(sym_name::in, string::out) is det.

	% sym_name_get_module_name(SymName, DefaultModName, ModName):
	% Given a symbol name, return the module qualifier(s).
	% If the symbol is unqualified, then return the specified default
	% module name.
	%
:- pred sym_name_get_module_name(sym_name::in, module_name::in,
	module_name::out) is det.

	% string_to_sym_name(String, Separator, SymName):
	%	Convert a string, possibly prefixed with
	%	module qualifiers (separated by Separator),
	%	into a symbol name.
	%
:- pred string_to_sym_name(string::in, string::in, sym_name::out) is det.

	% match_sym_name(PartialSymName, CompleteSymName):
	%	succeeds iff there is some sequence of module qualifiers
	%	which when prefixed to PartialSymName gives CompleteSymName.
	%
:- pred match_sym_name(sym_name::in, sym_name::in) is semidet.

	% remove_sym_name_prefix(SymName0, Prefix, SymName)
	% succeeds iff
	%	SymName and SymName0 have the same module qualifier
	%	and the unqualified part of SymName0 has the given prefix
	%	and the unqualified part of SymName is the unqualified
	%		part of SymName0 with the prefix removed
:- pred remove_sym_name_prefix(sym_name, string, sym_name).
:- mode remove_sym_name_prefix(in, in, out) is semidet.
:- mode remove_sym_name_prefix(out, in, in) is det.

	% remove_sym_name_suffix(SymName0, Suffix, SymName)
	% succeeds iff
	%	SymName and SymName0 have the same module qualifier
	%	and the unqualified part of SymName0 has the given suffix
	%	and the unqualified part of SymName is the unqualified
	%		part of SymName0 with the suffix removed
:- pred remove_sym_name_suffix(sym_name::in, string::in, sym_name::out)
	is semidet.

	% add_sym_name_suffix(SymName0, Suffix, SymName)
	% succeeds iff
	%	SymName and SymName0 have the same module qualifier
	%	and the unqualified part of SymName is the unqualified
	%		part of SymName0 with the suffix added
:- pred add_sym_name_suffix(sym_name::in, string::in, sym_name::out) is det.

	% insert_module_qualifier(ModuleName, SymName0, SymName):
	%	prepend the specified ModuleName onto the module
	%	qualifiers in SymName0, giving SymName.
:- pred insert_module_qualifier(string::in, sym_name::in, sym_name::out)
	is det.

	% Given a possible module qualified sym_name and a list of
	% argument types and a context, construct a term. This is
	% used to construct types.

:- pred construct_qualified_term(sym_name::in, list(term(T))::in,
	term(T)::out) is det.

:- pred construct_qualified_term(sym_name::in, list(term(T))::in,
	prog_context::in, term(T)::out) is det.

	% Given a sym_name return the top level qualifier of that name.
:- func outermost_qualifier(sym_name) = string.

%-----------------------------------------------------------------------------%

	% adjust_func_arity(PredOrFunc, FuncArity, PredArity).
	%
	% We internally store the arity as the length of the argument
	% list including the return value, which is one more than the
	% arity of the function reported in error messages.
:- pred adjust_func_arity(pred_or_func, int, int).
:- mode adjust_func_arity(in, in, out) is det.
:- mode adjust_func_arity(in, out, in) is det.

%-----------------------------------------------------------------------------%

	% make_pred_name_with_context(ModuleName, Prefix, PredOrFunc, PredName,
	%	Line, Counter, SymName).
	%
	% Create a predicate name with context, e.g. for introduced
	% lambda or deforestation predicates.
:- pred make_pred_name(module_name::in, string::in, maybe(pred_or_func)::in,
	string::in, new_pred_id::in, sym_name::out) is det.

	% make_pred_name_with_context(ModuleName, Prefix, PredOrFunc, PredName,
	%	Line, Counter, SymName).
	%
	% Create a predicate name with context, e.g. for introduced
	% lambda or deforestation predicates.
:- pred make_pred_name_with_context(module_name::in, string::in,
	pred_or_func::in, string::in, int::in, int::in, sym_name::out) is det.

:- type new_pred_id
	--->	counter(int, int)		% Line number, Counter
	;	type_subst(tvarset, type_subst)
	;	unused_args(list(int)).

%-----------------------------------------------------------------------------%

	% A pred declaration may contains just types, as in
	%	:- pred list__append(list(T), list(T), list(T)).
	% or it may contain both types and modes, as in
	%	:- pred list__append(list(T)::in, list(T)::in,
	%			list(T)::output).
	%
	% This predicate takes the argument list of a pred declaration,
	% splits it into two separate lists for the types and (if present)
	% the modes.

:- type maybe_modes == maybe(list(mode)).

:- pred split_types_and_modes(list(type_and_mode)::in, list(type)::out,
	maybe_modes::out) is det.

:- pred split_type_and_mode(type_and_mode::in, (type)::out, maybe(mode)::out)
	is det.

%-----------------------------------------------------------------------------%

	% Perform a substitution on a goal.

:- pred prog_util__rename_in_goal(prog_var::in, prog_var::in,
	goal::in, goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__inst.
:- import_module parse_tree__mercury_to_mercury.

:- import_module bool, require, string, int, map, varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% We may eventually want to put the standard library into a package "std":
% mercury_public_builtin_module = qualified(unqualified("std"), "builtin").
% mercury_private_builtin_module(M) =
%		qualified(unqualified("std"), "private_builtin"))).
mercury_public_builtin_module = unqualified("builtin").
mercury_public_builtin_module(mercury_public_builtin_module).
mercury_private_builtin_module = unqualified("private_builtin").
mercury_private_builtin_module(mercury_private_builtin_module).
mercury_table_builtin_module = unqualified("table_builtin").
mercury_table_builtin_module(mercury_table_builtin_module).
mercury_profiling_builtin_module = unqualified("profiling_builtin").
mercury_profiling_builtin_module(mercury_profiling_builtin_module).
mercury_term_size_prof_builtin_module = unqualified("term_size_prof_builtin").
mercury_term_size_prof_builtin_module(mercury_term_size_prof_builtin_module).
aditi_public_builtin_module = unqualified("aditi").
aditi_public_builtin_module(aditi_public_builtin_module).
aditi_private_builtin_module = unqualified("aditi_private_builtin").
aditi_private_builtin_module(aditi_private_builtin_module).

any_mercury_builtin_module(Module) :-
	(	mercury_public_builtin_module(Module)
	;	mercury_private_builtin_module(Module)
	;	mercury_table_builtin_module(Module)
	;	mercury_profiling_builtin_module(Module)
	;	mercury_term_size_prof_builtin_module(Module)
	;	aditi_private_builtin_module(Module)
	).

unqualify_name(unqualified(PredName), PredName).
unqualify_name(qualified(_ModuleName, PredName), PredName).

sym_name_get_module_name(unqualified(_), ModuleName, ModuleName).
sym_name_get_module_name(qualified(ModuleName, _PredName), _, ModuleName).

construct_qualified_term(qualified(Module, Name), Args, Context, Term) :-
	construct_qualified_term(Module, [], Context, ModuleTerm),
	UnqualifiedTerm = term__functor(term__atom(Name), Args, Context),
	Term = term__functor(term__atom("."),
		[ModuleTerm, UnqualifiedTerm], Context).
construct_qualified_term(unqualified(Name), Args, Context, Term) :-
	Term = term__functor(term__atom(Name), Args, Context).

construct_qualified_term(SymName, Args, Term) :-
	term__context_init(Context),
	construct_qualified_term(SymName, Args, Context, Term).

outermost_qualifier(unqualified(Name)) = Name.
outermost_qualifier(qualified(Module, _Name)) = outermost_qualifier(Module).

%-----------------------------------------------------------------------------%

adjust_func_arity(predicate, Arity, Arity).
adjust_func_arity(function, Arity - 1, Arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

split_types_and_modes(TypesAndModes, Types, MaybeModes) :-
	split_types_and_modes_2(TypesAndModes, yes, Types, Modes, Result),
	( Result = yes ->
		MaybeModes = yes(Modes)
	;
		MaybeModes = no
	).

:- pred split_types_and_modes_2(list(type_and_mode)::in, bool::in,
	list(type)::out, list(mode)::out, bool::out) is det.

	% T = type, M = mode, TM = combined type and mode
split_types_and_modes_2([], Result, [], [], Result).
split_types_and_modes_2([TM|TMs], Result0, [T|Ts], [M|Ms], Result) :-
	split_type_and_mode(TM, Result0, T, M, Result1),
	split_types_and_modes_2(TMs, Result1, Ts, Ms, Result).

	% if a pred declaration specifies modes for some but
	% not all of the arguments, then the modes are ignored
	% - should this be an error instead?
	% trd: this should never happen because prog_io.m will detect
	% these cases

:- pred split_type_and_mode(type_and_mode::in, bool::in,
	(type)::out, (mode)::out, bool::out) is det.

split_type_and_mode(type_only(T), _, T, (free -> free), no).
split_type_and_mode(type_and_mode(T,M), R, T, M, R).

split_type_and_mode(type_only(T), T, no).
split_type_and_mode(type_and_mode(T,M), T, yes(M)).

%-----------------------------------------------------------------------------%

prog_util__rename_in_goal(OldVar, NewVar, Goal0 - Context, Goal - Context) :-
	prog_util__rename_in_goal_expr(OldVar, NewVar, Goal0, Goal).

:- pred prog_util__rename_in_goal_expr(prog_var::in, prog_var::in,
	goal_expr::in, goal_expr::out) is det.

prog_util__rename_in_goal_expr(OldVar, NewVar, (GoalA0, GoalB0),
		(GoalA, GoalB)) :-
	prog_util__rename_in_goal(OldVar, NewVar, GoalA0, GoalA),
	prog_util__rename_in_goal(OldVar, NewVar, GoalB0, GoalB).
prog_util__rename_in_goal_expr(OldVar, NewVar, (GoalA0 & GoalB0),
		(GoalA & GoalB)) :-
	prog_util__rename_in_goal(OldVar, NewVar, GoalA0, GoalA),
	prog_util__rename_in_goal(OldVar, NewVar, GoalB0, GoalB).
prog_util__rename_in_goal_expr(_OldVar, _NewVar, true, true).
prog_util__rename_in_goal_expr(OldVar, NewVar, (GoalA0; GoalB0),
		(GoalA; GoalB)) :-
	prog_util__rename_in_goal(OldVar, NewVar, GoalA0, GoalA),
	prog_util__rename_in_goal(OldVar, NewVar, GoalB0, GoalB).
prog_util__rename_in_goal_expr(_Var, _NewVar, fail, fail).
prog_util__rename_in_goal_expr(OldVar, NewVar, not(Goal0), not(Goal)) :-
	prog_util__rename_in_goal(OldVar, NewVar, Goal0, Goal).
prog_util__rename_in_goal_expr(OldVar, NewVar, some(Vars0, Goal0),
		some(Vars, Goal)) :-
	prog_util__rename_in_vars(OldVar, NewVar, Vars0, Vars),
	prog_util__rename_in_goal(OldVar, NewVar, Goal0, Goal).
prog_util__rename_in_goal_expr(OldVar, NewVar, some_state_vars(Vars0, Goal0),
		some_state_vars(Vars, Goal)) :-
	prog_util__rename_in_vars(OldVar, NewVar, Vars0, Vars),
	prog_util__rename_in_goal(OldVar, NewVar, Goal0, Goal).
prog_util__rename_in_goal_expr(OldVar, NewVar, all(Vars0, Goal0),
		all(Vars, Goal)) :-
	prog_util__rename_in_vars(OldVar, NewVar, Vars0, Vars),
	prog_util__rename_in_goal(OldVar, NewVar, Goal0, Goal).
prog_util__rename_in_goal_expr(OldVar, NewVar, all_state_vars(Vars0, Goal0),
		all_state_vars(Vars, Goal)) :-
	prog_util__rename_in_vars(OldVar, NewVar, Vars0, Vars),
	prog_util__rename_in_goal(OldVar, NewVar, Goal0, Goal).
prog_util__rename_in_goal_expr(OldVar, NewVar, implies(GoalA0, GoalB0),
		implies(GoalA, GoalB)) :-
	prog_util__rename_in_goal(OldVar, NewVar, GoalA0, GoalA),
	prog_util__rename_in_goal(OldVar, NewVar, GoalB0, GoalB).
prog_util__rename_in_goal_expr(OldVar, NewVar, equivalent(GoalA0, GoalB0),
		equivalent(GoalA, GoalB)) :-
	prog_util__rename_in_goal(OldVar, NewVar, GoalA0, GoalA),
	prog_util__rename_in_goal(OldVar, NewVar, GoalB0, GoalB).
prog_util__rename_in_goal_expr(OldVar, NewVar,
		if_then(Vars0, StateVars0, Cond0, Then0),
		if_then(Vars, StateVars, Cond, Then)) :-
	prog_util__rename_in_vars(OldVar, NewVar, Vars0, Vars),
	prog_util__rename_in_vars(OldVar, NewVar, StateVars0, StateVars),
	prog_util__rename_in_goal(OldVar, NewVar, Cond0, Cond),
	prog_util__rename_in_goal(OldVar, NewVar, Then0, Then).
prog_util__rename_in_goal_expr(OldVar, NewVar, 
		if_then_else(Vars0, StateVars0, Cond0, Then0, Else0),
		if_then_else(Vars, StateVars, Cond, Then, Else)) :-
	prog_util__rename_in_vars(OldVar, NewVar, Vars0, Vars),
	prog_util__rename_in_vars(OldVar, NewVar, StateVars0, StateVars),
	prog_util__rename_in_goal(OldVar, NewVar, Cond0, Cond),
	prog_util__rename_in_goal(OldVar, NewVar, Then0, Then),
	prog_util__rename_in_goal(OldVar, NewVar, Else0, Else).
prog_util__rename_in_goal_expr(OldVar, NewVar, call(SymName, Terms0, Purity),
		call(SymName, Terms, Purity)) :-
	term__substitute_list(Terms0, OldVar, term__variable(NewVar), Terms).
prog_util__rename_in_goal_expr(OldVar, NewVar, unify(TermA0, TermB0, Purity),
		unify(TermA, TermB, Purity)) :-
	term__substitute(TermA0, OldVar, term__variable(NewVar), TermA),
	term__substitute(TermB0, OldVar, term__variable(NewVar), TermB).

:- pred prog_util__rename_in_vars(prog_var::in, prog_var::in,
	list(prog_var)::in, list(prog_var)::out) is det.

prog_util__rename_in_vars(_, _, [], []).
prog_util__rename_in_vars(OldVar, NewVar, [Var0 | Vars0], [Var | Vars]) :-
	( Var0 = OldVar ->
		Var = NewVar
	;
		Var = Var0
	),
	prog_util__rename_in_vars(OldVar, NewVar, Vars0, Vars).

%-----------------------------------------------------------------------------%

% This would be simpler if we had a string__rev_sub_string_search/3 pred.
% With that, we could search for underscores right-to-left,
% and construct the resulting symbol directly.
% Instead, we search for them left-to-right, and then call
% insert_module_qualifier to fix things up.

string_to_sym_name(String, ModuleSeparator, Result) :-
	(
		string__sub_string_search(String, ModuleSeparator, LeftLength),
		LeftLength > 0
	->
		string__left(String, LeftLength, ModuleName),
		string__length(String, StringLength),
		string__length(ModuleSeparator, SeparatorLength),
		RightLength = StringLength - LeftLength - SeparatorLength,
		string__right(String, RightLength, Name),
		string_to_sym_name(Name, ModuleSeparator, NameSym),
		insert_module_qualifier(ModuleName, NameSym, Result)
	;
		Result = unqualified(String)
	).

insert_module_qualifier(ModuleName, unqualified(PlainName),
		qualified(unqualified(ModuleName), PlainName)).
insert_module_qualifier(ModuleName, qualified(ModuleQual0, PlainName),
		qualified(ModuleQual, PlainName)) :-
	insert_module_qualifier(ModuleName, ModuleQual0, ModuleQual).

%-----------------------------------------------------------------------------%

% match_sym_name(PartialSymName, CompleteSymName):
%	succeeds iff there is some sequence of module qualifiers
%	which when prefixed to PartialSymName gives CompleteSymName.

match_sym_name(qualified(Module1, Name), qualified(Module2, Name)) :-
	match_sym_name(Module1, Module2).
match_sym_name(unqualified(Name), unqualified(Name)).
match_sym_name(unqualified(Name), qualified(_, Name)).

%-----------------------------------------------------------------------------%

remove_sym_name_prefix(qualified(Module, Name0), Prefix,
		qualified(Module, Name)) :-
	string__append(Prefix, Name, Name0).
remove_sym_name_prefix(unqualified(Name0), Prefix, unqualified(Name)) :-
	string__append(Prefix, Name, Name0).

remove_sym_name_suffix(qualified(Module, Name0), Suffix,
		qualified(Module, Name)) :-
	string__remove_suffix(Name0, Suffix, Name).
remove_sym_name_suffix(unqualified(Name0), Suffix, unqualified(Name)) :-
	string__remove_suffix(Name0, Suffix, Name).

add_sym_name_suffix(qualified(Module, Name0), Suffix,
		qualified(Module, Name)) :-
	string__append(Name0, Suffix, Name).
add_sym_name_suffix(unqualified(Name0), Suffix, unqualified(Name)) :-
	string__append(Name0, Suffix, Name).

%-----------------------------------------------------------------------------%

make_pred_name_with_context(ModuleName, Prefix,
		PredOrFunc, PredName, Line, Counter, SymName) :-
	make_pred_name(ModuleName, Prefix, yes(PredOrFunc), PredName,
		counter(Line, Counter), SymName).

make_pred_name(ModuleName, Prefix, MaybePredOrFunc, PredName,
		NewPredId, SymName) :-
	(
		MaybePredOrFunc = yes(PredOrFunc),
		(
			PredOrFunc = predicate,
			PFS = "pred"
		;
			PredOrFunc = function,
			PFS = "func"
		)
	;
		MaybePredOrFunc = no,
		PFS = "pred_or_func"
	),
	(
		NewPredId = counter(Line, Counter),
		string__format("%d__%d", [i(Line), i(Counter)], PredIdStr)
	;
		NewPredId = type_subst(VarSet, TypeSubst),
		SubstToString = (pred(SubstElem::in, SubstStr::out) is det :-
			SubstElem = Var - Type,
			varset__lookup_name(VarSet, Var, VarName),
			TypeString = mercury_type_to_string(VarSet, Type),
			string__append_list([VarName, " = ", TypeString],
				SubstStr)
		),
		list_to_string(SubstToString, TypeSubst, PredIdStr)
	;
		NewPredId = unused_args(Args),
		list_to_string(int_to_string, Args, PredIdStr)
	),

	string__format("%s__%s__%s__%s",
		[s(Prefix), s(PFS), s(PredName), s(PredIdStr)], Name),
		SymName = qualified(ModuleName, Name).

:- pred list_to_string(pred(T, string)::in(pred(in, out) is det),
	list(T)::in, string::out) is det.

list_to_string(Pred, List, String) :-
	list_to_string_2(Pred, List, Strings, ["]"]),
	string__append_list(["[" | Strings], String).

:- pred list_to_string_2(pred(T, string)::in(pred(in, out) is det),
	list(T)::in, list(string)::out, list(string)::in) is det.

list_to_string_2(_, []) --> [].
list_to_string_2(Pred, [T | Ts]) -->
	{ call(Pred, T, String) },
	[String],
	( { Ts = [] } ->
		[]
	;
		[", "],
		list_to_string_2(Pred, Ts)
	).

%-----------------------------------------------------------------------------%
