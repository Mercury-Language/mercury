%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% main author: fjh

% various utility predicates acting on the parse tree data
% structure defined in prog_data.m.

:- module prog_util.

:- interface.

:- import_module prog_data, term.
:- import_module std_util, list.

%-----------------------------------------------------------------------------%

	% Returns the name of the module containing public builtins;
	% originally this was "mercury_builtin", but it later became
	% just "builtin", and it may eventually be renamed "std:builtin".
	% This module is automatically imported, as if via `import_module'.

:- pred mercury_public_builtin_module(sym_name).
:- mode mercury_public_builtin_module(out) is det.

	% Returns the name of the module containing private builtins;
	% traditionally this was "mercury_builtin", but it later became
	% "private_builtin", and it may eventually be renamed
	% "std:private_builtin".
	% This module is automatically imported, as if via `use_module'.

:- pred mercury_private_builtin_module(sym_name).
:- mode mercury_private_builtin_module(out) is det.

	% Returns the name of the module containing builtins for tabling;
	% originally these were in "private_builtin", but they
	% may soon be moved into a separate module.
	% This module is automatically imported iff tabling is enabled.

:- pred mercury_table_builtin_module(sym_name).
:- mode mercury_table_builtin_module(out) is det.

	% Returns the name of the module containing the builtins for
	% deep profiling.
	% This module is automatically imported iff deep profiling is
	% enabled.
:- pred mercury_profiling_builtin_module(sym_name).
:- mode mercury_profiling_builtin_module(out) is det.

	% Succeeds iff the specified module is one of the three
	% builtin modules listed above which are automatically imported.

:- pred any_mercury_builtin_module(sym_name).
:- mode any_mercury_builtin_module(in) is semidet.

%-----------------------------------------------------------------------------%

	% Given a symbol name, return its unqualified name.

:- pred unqualify_name(sym_name, string).
:- mode unqualify_name(in, out) is det.

	% sym_name_get_module_name(SymName, DefaultModName, ModName):
	% Given a symbol name, return the module qualifier(s).
	% If the symbol is unqualified, then return the specified default
	% module name.

:- pred sym_name_get_module_name(sym_name, module_name, module_name).
:- mode sym_name_get_module_name(in, in, out) is det.

	% string_to_sym_name(String, Separator, SymName):
	%	Convert a string, possibly prefixed with
	%	module qualifiers (separated by Separator),
	%	into a symbol name.
	%
:- pred string_to_sym_name(string, string, sym_name).
:- mode string_to_sym_name(in, in, out) is det.

	% match_sym_name(PartialSymName, CompleteSymName):
	%	succeeds iff there is some sequence of module qualifiers
	%	which when prefixed to PartialSymName gives CompleteSymName.
	%
:- pred match_sym_name(sym_name, sym_name).
:- mode match_sym_name(in, in) is semidet.

	% remove_sym_name_prefix(SymName0, Prefix, SymName)
	%    succeeds iff
	%	SymName and SymName0 have the same module qualifier
	%	and the unqualified part of SymName0 has the given prefix
	%	and the unqualified part of SymName is the unqualified
	%		part of SymName0 with the prefix removed
:- pred remove_sym_name_prefix(sym_name, string, sym_name).
:- mode remove_sym_name_prefix(in, in, out) is semidet.
:- mode remove_sym_name_prefix(out, in, in) is det.

	% remove_sym_name_suffix(SymName0, Suffix, SymName)
	%    succeeds iff
	%	SymName and SymName0 have the same module qualifier
	%	and the unqualified part of SymName0 has the given suffix
	%	and the unqualified part of SymName is the unqualified
	%		part of SymName0 with the suffix removed
:- pred remove_sym_name_suffix(sym_name, string, sym_name).
:- mode remove_sym_name_suffix(in, in, out) is semidet.

	% add_sym_name_suffix(SymName0, Suffix, SymName)
	%    succeeds iff
	%	SymName and SymName0 have the same module qualifier
	%	and the unqualified part of SymName is the unqualified
	%		part of SymName0 with the suffix added
:- pred add_sym_name_suffix(sym_name, string, sym_name).
:- mode add_sym_name_suffix(in, in, out) is det.

	% insert_module_qualifier(ModuleName, SymName0, SymName):
	%	prepend the specified ModuleName onto the module
	%	qualifiers in SymName0, giving SymName.
:- pred insert_module_qualifier(string, sym_name, sym_name).
:- mode insert_module_qualifier(in, in, out) is det.

        % Given a possible module qualified sym_name and a list of
	% argument types and a context, construct a term. This is
	% used to construct types. 

:- pred construct_qualified_term(sym_name, list(term(T)), term(T)).
:- mode construct_qualified_term(in, in, out) is det.

:- pred construct_qualified_term(sym_name, list(term(T)), prog_context, term(T)).
:- mode construct_qualified_term(in, in, in, out) is det.

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
:- pred make_pred_name(module_name, string, maybe(pred_or_func),
		string, new_pred_id, sym_name).
:- mode make_pred_name(in, in, in, in, in, out) is det.

	% make_pred_name_with_context(ModuleName, Prefix, PredOrFunc, PredName,
	%	Line, Counter, SymName).
	%
	% Create a predicate name with context, e.g. for introduced
	% lambda or deforestation predicates.
:- pred make_pred_name_with_context(module_name, string, pred_or_func,
		string, int, int, sym_name).
:- mode make_pred_name_with_context(in, in, in, in, in, in, out) is det.

:- type new_pred_id
	--->	counter(int, int)		% Line number, Counter
	;	type_subst(tvarset, type_subst)
	.

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

:- pred split_types_and_modes(list(type_and_mode), list(type), maybe_modes).
:- mode split_types_and_modes(in, out, out) is det.

:- pred split_type_and_mode(type_and_mode, type, maybe(mode)).
:- mode split_type_and_mode(in, out, out) is det.

%-----------------------------------------------------------------------------%

	% Perform a substitution on a goal.

:- pred prog_util__rename_in_goal(goal, prog_var, prog_var, goal).
:- mode prog_util__rename_in_goal(in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module mercury_to_mercury, (inst).
:- import_module bool, string, int, map, varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% We may eventually want to put the standard library into a package "std":
%	mercury_public_builtin_module(M) :-
% 		M = qualified(unqualified("std"), "builtin"))).
%	mercury_private_builtin_module(M) :-
% 		M = qualified(unqualified("std"), "private_builtin"))).
mercury_public_builtin_module(unqualified("builtin")).
mercury_private_builtin_module(unqualified("private_builtin")).
mercury_table_builtin_module(unqualified("table_builtin")).
mercury_profiling_builtin_module(unqualified("profiling_builtin")).

any_mercury_builtin_module(Module) :-
	(	mercury_public_builtin_module(Module)
	;	mercury_private_builtin_module(Module)
	;	mercury_table_builtin_module(Module)
	;	mercury_profiling_builtin_module(Module)
	).

unqualify_name(unqualified(PredName), PredName).
unqualify_name(qualified(_ModuleName, PredName), PredName).

sym_name_get_module_name(unqualified(_), ModuleName, ModuleName).
sym_name_get_module_name(qualified(ModuleName, _PredName), _, ModuleName).

construct_qualified_term(qualified(Module, Name), Args, Context, Term) :-
	construct_qualified_term(Module, [], Context, ModuleTerm),
	UnqualifiedTerm = term__functor(term__atom(Name), Args, Context),
	Term = term__functor(term__atom(":"),
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
	(
		Result = yes
	->
		MaybeModes = yes(Modes)
	;
		MaybeModes = no
	).

:- pred split_types_and_modes_2(list(type_and_mode), bool,
				list(type), list(mode), bool).
:- mode split_types_and_modes_2(in, in, out, out, out) is det.

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

:- pred split_type_and_mode(type_and_mode, bool, type, mode, bool).
:- mode split_type_and_mode(in, in, out, out, out) is det.

split_type_and_mode(type_only(T), _, T, (free -> free), no).
split_type_and_mode(type_and_mode(T,M), R, T, M, R).

split_type_and_mode(type_only(T), T, no).
split_type_and_mode(type_and_mode(T,M), T, yes(M)).

%-----------------------------------------------------------------------------%

prog_util__rename_in_goal(Goal0 - Context, OldVar, NewVar, Goal - Context) :-
	prog_util__rename_in_goal_expr(Goal0, OldVar, NewVar, Goal).

:- pred prog_util__rename_in_goal_expr(goal_expr, prog_var, prog_var,
		goal_expr).
:- mode prog_util__rename_in_goal_expr(in, in, in, out) is det.

prog_util__rename_in_goal_expr((GoalA0, GoalB0), OldVar, NewVar,
		(GoalA, GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr((GoalA0 & GoalB0), OldVar, NewVar,
		(GoalA & GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(true, _Var, _NewVar, true).
prog_util__rename_in_goal_expr((GoalA0; GoalB0), OldVar, NewVar,
		(GoalA; GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(fail, _Var, _NewVar, fail).
prog_util__rename_in_goal_expr(not(Goal0), OldVar, NewVar, not(Goal)) :-
	prog_util__rename_in_goal(Goal0, OldVar, NewVar, Goal).
prog_util__rename_in_goal_expr(some(Vars0, Goal0), OldVar, NewVar,
		some(Vars, Goal)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Goal0, OldVar, NewVar, Goal).
prog_util__rename_in_goal_expr(all(Vars0, Goal0), OldVar, NewVar,
		all(Vars, Goal)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Goal0, OldVar, NewVar, Goal).
prog_util__rename_in_goal_expr(implies(GoalA0, GoalB0), OldVar, NewVar,
		implies(GoalA, GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(equivalent(GoalA0, GoalB0), OldVar, NewVar,
		equivalent(GoalA, GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(if_then(Vars0, Cond0, Then0), OldVar, NewVar,
		if_then(Vars, Cond, Then)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Cond0, OldVar, NewVar, Cond),
	prog_util__rename_in_goal(Then0, OldVar, NewVar, Then).
prog_util__rename_in_goal_expr(if_then_else(Vars0, Cond0, Then0, Else0),
		OldVar, NewVar, if_then_else(Vars, Cond, Then, Else)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Cond0, OldVar, NewVar, Cond),
	prog_util__rename_in_goal(Then0, OldVar, NewVar, Then),
	prog_util__rename_in_goal(Else0, OldVar, NewVar, Else).
prog_util__rename_in_goal_expr(call(SymName, Terms0, Purity), OldVar, NewVar,
		call(SymName, Terms, Purity)) :-
	term__substitute_list(Terms0, OldVar, term__variable(NewVar),
		Terms).
prog_util__rename_in_goal_expr(unify(TermA0, TermB0, Purity), OldVar, NewVar,
		unify(TermA, TermB, Purity)) :-
	term__substitute(TermA0, OldVar, term__variable(NewVar),
		TermA),
	term__substitute(TermB0, OldVar, term__variable(NewVar),
		TermB).

:- pred prog_util__rename_in_vars(list(prog_var), prog_var, prog_var,
		list(prog_var)).
:- mode prog_util__rename_in_vars(in, in, in, out) is det.

prog_util__rename_in_vars([], _, _, []).
prog_util__rename_in_vars([Var0 | Vars0], OldVar, NewVar, [Var | Vars]) :-
	( Var0 = OldVar ->
		Var = NewVar
	;
		Var = Var0
	),
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars).

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
	RightLength is StringLength - LeftLength - SeparatorLength,
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
		SubstToString = lambda([SubstElem::in, SubstStr::out] is det, (
			SubstElem = Var - Type,
			varset__lookup_name(VarSet, Var, VarName),
			TypeString = mercury_type_to_string(VarSet, Type),
			string__append_list([VarName, " = ", TypeString],
				SubstStr)
		)),
		list_to_string(SubstToString, TypeSubst, PredIdStr)
	),

	string__format("%s__%s__%s__%s",
		[s(Prefix), s(PFS), s(PredName), s(PredIdStr)], Name),
		SymName = qualified(ModuleName, Name).

:- pred list_to_string(pred(T, string), list(T), string).
:- mode list_to_string(pred(in, out) is det, in, out) is det.

list_to_string(Pred, List, String) :-
	list_to_string_2(Pred, List, Strings, ["]"]),
	string__append_list(["[" | Strings], String).

:- pred list_to_string_2(pred(T, string), list(T), list(string), list(string)).
:- mode list_to_string_2(pred(in, out) is det, in, out, in) is det.

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
