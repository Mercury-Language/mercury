%-----------------------------------------------------------------------------%

% File: interpreter.m.
% Main author: fjh.

% This is an interpreter for definite logic programs
% (i.e. pure Prolog with no negation or if-then-else.)
%
% This is not part of the compiler, it's just intended
% as a demonstration of the use of the meta-programming
% library modules term, varset, and term_io.

%-----------------------------------------------------------------------------%

:- module interpreter.
:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, string, term, varset, term_io, require, std_util.

main -->
	io__write_string("Pure Prolog Interpreter.\n\n"),
	io__command_line_arguments(Args),
	{ database_init(Database0) },
	consult_list(Args, Database0, Database),
	main_loop(Database).

:- pred main_loop(database, io__state, io__state).
:- mode main_loop(in, di, uo) is det.

main_loop(Database) -->
	io__write_string("?- "),
	term_io__read_term(ReadTerm),
	main_loop_2(ReadTerm, Database).

:- pred main_loop_2(read_term, database, io__state, io__state).
:- mode main_loop_2(in, in, di, uo) is det.

main_loop_2(eof, _Database) --> [].
main_loop_2(error(ErrorMessage, LineNumber), Database) -->
	io__write_string("Error reading term at line "),
	io__write_int(LineNumber),
	io__write_string(" of standard input: "),
	io__write_string(ErrorMessage),
	io__write_string("\n"),
	main_loop(Database).
main_loop_2(term(VarSet0, Goal), Database) -->
	%%% It would be a good idea to add some special commands
	%%% with side-effects (such as `consult' and `listing');
	%%% these could be identified and processed here.
/*
	{
	  solutions(
		solve(Database, Goal, VarSet0),
		Solutions
	  )
	},
	write_solutions(Solutions, Goal),
*/
	( { solve(Database, Goal, VarSet0, _VarSet) } ->
		io__write_string("Yes.\n")
	;
		io__write_string("No.\n")
	),

	main_loop(Database).

:- pred write_solutions(list(varset), term, io__state, io__state).
:- mode write_solutions(in, in, di, uo) is det.

write_solutions(Solutions, Goal) -->
	( { Solutions = [] } ->
		io__write_string("No.\n")
	;
		write_solutions_2(Solutions, Goal),
		io__write_string("Yes.\n")
	).

:- pred write_solutions_2(list(varset), term, io__state, io__state).
:- mode write_solutions_2(in, in, di, uo) is det.

write_solutions_2([], _) --> [].
write_solutions_2([VarSet | VarSets], Goal) -->
	term_io__write_term_nl(VarSet, Goal),
	write_solutions_2(VarSets, Goal).

%-----------------------------------------------------------------------------%

:- pred consult_list(list(string), database, database, io__state, io__state).
:- mode consult_list(in, in, out, di, uo) is det.

consult_list([], Database, Database) --> [].
consult_list([File | Files], Database0, Database) -->
	consult(File, Database0, Database1),
	consult_list(Files, Database1, Database).

:- pred consult(string, database, database, io__state, io__state).
:- mode consult(in, in, out, di, uo) is det.

consult(File, Database0, Database) -->
	io__write_string("Consulting file `"),
	io__write_string(File),
	io__write_string("'...\n"),
	io__see(File, Result),
	( { Result = ok } ->
		consult_until_eof(Database0, Database),
		io__seen
	;
		io__write_string("Error opening file `"),
		io__write_string(File),
		io__write_string("' for input.\n"),
		{ Database = Database0 }
	).

:- pred consult_until_eof(database, database, io__state, io__state).
:- mode consult_until_eof(in, out, di, uo) is det.

consult_until_eof(Database0, Database) -->
	term_io__read_term(ReadTerm),
	consult_until_eof_2(ReadTerm, Database0, Database).

:- pred consult_until_eof_2(read_term, database, database,
				io__state, io__state).
:- mode consult_until_eof_2(in, in, out, di, uo) is det.

consult_until_eof_2(eof, Database, Database) --> [].

consult_until_eof_2(error(ErrorMessage, LineNumber), Database0, Database) -->
	io__write_string("Error reading term at line "),
	io__write_int(LineNumber),
	io__write_string(" of standard input: "),
	io__write_string(ErrorMessage),
	io__write_string("\n"),
	consult_until_eof(Database0, Database).

consult_until_eof_2(term(VarSet, Term), Database0, Database) -->
	{ database_assert_clause(Database0, VarSet, Term, Database1) },
	consult_until_eof(Database1, Database).

%-----------------------------------------------------------------------------%

% Solve takes a database of rules and facts, a goal to be solved,
% and a varset (which includes a supply of fresh vars, a substitution,
% and names for [some subset of] the variables).  It updates
% the varset, producing a new substitution and perhaps introducing
% some new vars, and returns the result.

% Goals are stored just as terms.
% (It might be more efficient to parse them 
% before storing them in the database.  Currently we do
% this parsing work every time we interpret a clause.)

:- pred solve(database, term, varset, varset).
:- mode solve(in, in, in, out) is nondet.

solve(_Database, term__functor(term__atom("true"), [], _)) --> [].

solve(Database, term__functor(term__atom(","), [A, B], _)) -->
	solve(Database, A),
	solve(Database, B).

solve(Database, term__functor(term__atom(";"), [A, B], _)) -->
	solve(Database, A)
	;
	solve(Database, B).

solve(_Database, term__functor(term__atom("="), [A, B], _)) -->
	unify(A, B).

solve(Database, Goal) -->
	{ database_lookup_clause(Database, Goal, ClauseVarSet, Head0, Body0) },
	rename_apart(ClauseVarSet, [Head0, Body0], [Head, Body]),
	unify(Goal, Head),
	solve(Database, Body).

%-----------------------------------------------------------------------------%

:- pred rename_apart(varset, list(term), list(term), varset, varset).
:- mode rename_apart(in, in, out, in, out) is det.

rename_apart(NewVarSet, Terms0, Terms, VarSet0, VarSet) :-
	varset__merge(VarSet0, NewVarSet, Terms0, VarSet, Terms).

%-----------------------------------------------------------------------------%

:- pred unify(term, term, varset, varset).
:- mode unify(in, in, in, out) is semidet.

:- unify(X, Y, _, _) when X and Y.		% NU-Prolog indexing

unify(term__variable(X), term__variable(Y), VarSet0, VarSet) :-
	(
		varset__search_var(VarSet0, X, BindingOfX)
	->
		(
			varset__search_var(VarSet0, Y, BindingOfY)
		->
			% both X and Y already have VarSet - just
			% unify the terms they are bound to
			unify(BindingOfX, BindingOfY, VarSet0, VarSet)
		;
			% Y is a variable which hasn't been bound yet
			apply_rec_substitution(BindingOfX, VarSet0,
				SubstBindingOfX),
			( SubstBindingOfX = term__variable(Y) ->
			 	VarSet = VarSet0
			;
				\+ occurs(SubstBindingOfX, Y, VarSet0),
				varset__bind_var(VarSet0, Y, SubstBindingOfX,
					VarSet)
			)
		)
	;
		(
			varset__search_var(VarSet0, Y, BindingOfY2)
		->
			% X is a variable which hasn't been bound yet
			apply_rec_substitution(BindingOfY2, VarSet0,
				SubstBindingOfY2),
			( SubstBindingOfY2 = term__variable(X) ->
				VarSet = VarSet0
			;
				\+ occurs(SubstBindingOfY2, X, VarSet0),
				varset__bind_var(VarSet0, X, SubstBindingOfY2,
					VarSet)
			)
		;
			% both X and Y are unbound variables -
			% bind one to the other
			( X = Y ->
				VarSet = VarSet0
			;
				varset__bind_var(VarSet0, X, term__variable(Y),
					VarSet)
			)
		)
	).

unify(term__variable(X), term__functor(F, As, C), VarSet0, VarSet) :-
	(
		varset__search_var(VarSet0, X, BindingOfX)
	->
		unify(BindingOfX, term__functor(F, As, C), VarSet0,
			VarSet)
	;
		\+ occurs_list(As, X, VarSet0),
		varset__bind_var(VarSet0, X, term__functor(F, As, C), VarSet)
	).

unify(term__functor(F, As, C), term__variable(X), VarSet0, VarSet) :-
	(
		varset__search_var(VarSet0, X, BindingOfX)
	->
		unify(term__functor(F, As, C), BindingOfX, VarSet0,
			VarSet)
	;
		\+ occurs_list(As, X, VarSet0),
		varset__bind_var(VarSet0, X, term__functor(F, As, C), VarSet)
	).

unify(term__functor(F, AsX, _), term__functor(F, AsY, _)) -->
	unify_list(AsX, AsY).

:- pred unify_list(list(term), list(term), varset, varset).
:- mode unify_list(in, in, in, out) is semidet.

unify_list([], []) --> [].
unify_list([X | Xs], [Y | Ys]) -->
	unify(X, Y),
	unify_list(Xs, Ys).

%-----------------------------------------------------------------------------%

	% occurs(Term, Var, Subst) succeeds if Term contains Var,
	% perhaps indirectly via the substitution.  (The variable must
	% not be mapped by the substitution.)

:- pred occurs(term, var, varset).
:- mode occurs(in, in, in) is semidet.

occurs(term__variable(X), Y, VarSet) :-
	X = Y
	;
	varset__search_var(VarSet, X, BindingOfX),
	occurs(BindingOfX, Y, VarSet).
occurs(term__functor(_F, As, _), Y, VarSet) :-
	occurs_list(As, Y, VarSet).

:- pred occurs_list(list(term), var, varset).
:- mode occurs_list(in, in, in) is semidet.

occurs_list([Term | Terms], Y, VarSet) :-
	occurs(Term, Y, VarSet)
	;
	occurs_list(Terms, Y, VarSet).

%-----------------------------------------------------------------------------%

%	apply_rec_substitution(Term0, VarSet, Term) :
%		recursively apply substitution to Term0 until
%		no more substitions can be applied, and then
%		return the result in Term.

:- pred apply_rec_substitution(term, varset, term).
:- mode apply_rec_substitution(in, in, out) is det.

apply_rec_substitution(term__variable(Var), VarSet, Term) :-
	(
		varset__search_var(VarSet, Var, Replacement)
	->
		% recursively apply the substition to the replacement
		apply_rec_substitution(Replacement, VarSet, Term)
	;
		Term = term__variable(Var)
	).
apply_rec_substitution(term__functor(Name, Args0, Context), VarSet,
		 term__functor(Name, Args, Context)) :-
	apply_rec_substitution_to_list(Args0, VarSet, Args).

:- pred apply_rec_substitution_to_list(list(term), varset, list(term)).
:- mode apply_rec_substitution_to_list(in, in, out) is det.

apply_rec_substitution_to_list([], _VarSet, []).
apply_rec_substitution_to_list([Term0 | Terms0], VarSet,
		[Term | Terms]) :-
	apply_rec_substitution(Term0, VarSet, Term),
	apply_rec_substitution_to_list(Terms0, VarSet, Terms).

%-----------------------------------------------------------------------------%

% We store the database just as a list of clauses.
% (It would be more realistic to index this on the predicate name/arity
% and subindex on the name/arity of the first argument.)

:- type database == list(clause).
:- type clause ---> clause(varset, term, term).

:- pred database_init(database).
:- mode database_init(out) is det.

database_init([]).

:- pred database_assert_clause(database, varset, term, database).
:- mode database_assert_clause(in, in, in, out) is det.

database_assert_clause(Database, VarSet, Term, [Clause | Database]) :-
	( Term = term__functor(term__atom(":-"), [H, B], _) ->
		Head = H,
		Body = B
	;
		Head = Term,
		term__context_init(Context),
		Body = term__functor(term__atom("true"), [], Context)
	),
	Clause = clause(VarSet, Head, Body).

:- pred database_lookup_clause(database, term, varset, term, term).
:- mode database_lookup_clause(in, in, out, out, out) is nondet.

database_lookup_clause(Database, _Goal, VarSet, Head, Body) :-
	list__member(Clause, Database),
	Clause = clause(VarSet, Head, Body).

%-----------------------------------------------------------------------------%
