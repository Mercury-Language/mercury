%-----------------------------------------------------------------------------%

% File: interpreter.m.
% Main author: fjh.

% This is an interpreter for definite logic programs
% (i.e. pure Prolog with no negation or if-then-else.)
%
% This is just intended as a demonstration of the use of the
% library module tr_store.m.

% There are many extensions/improvements that could be made;
% they're left as an exercise for the reader.

% This source file is hereby placed in the public domain. -fjh (the author).

%-----------------------------------------------------------------------------%

:- module interpreter.
:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, string, term, varset, term_io, require, std_util.
:- import_module store, tr_store, map, multi_map.
:- import_module unsafe.

main -->
	io__write_string("Pure Prolog Interpreter.\n\n"),
	io__command_line_arguments(Args),
	( { Args = [] } ->
		io__stderr_stream(StdErr),
		io__write_string(StdErr, "Usage: interpreter filename ...\n"),
		io__set_exit_status(1)
	;
		{ database_init(Database0) },
		consult_list(Args, Database0, Database),
		main_loop(Database)
	).

:- pred main_loop(database, io__state, io__state).
:- mode main_loop(in, di, uo) is cc_multi.

main_loop(Database) -->
	io__write_string("?- "),
	term_io__read_term(ReadTerm),
	main_loop_2(ReadTerm, Database).

:- pred main_loop_2(read_term, database, io__state, io__state).
:- mode main_loop_2(in, in, di, uo) is cc_multi.

main_loop_2(eof, _Database) --> [].
main_loop_2(error(ErrorMessage, LineNumber), Database) -->
	io__write_string("Error reading term at line "),
	io__write_int(LineNumber),
	io__write_string(" of standard input: "),
	io__write_string(ErrorMessage),
	io__write_string("\n"),
	main_loop(Database).
main_loop_2(term(VarSet, Goal), Database) -->
	%%% It would be a good idea to add some special commands
	%%% with side-effects (such as `consult' and `listing');
	%%% these could be identified and processed here.
	{ store__new(Store0) },
	{ map__init(VarMap0) },
	{ term_to_my_term(Goal, MyGoal, VarMap0, VarMap, Store0, Store1) },
	print_solutions(VarSet, VarMap, MyGoal, Store1, Database),
	main_loop(Database).

:- pred print_solutions(varset, map(var, my_var(S)), my_term(S),
		store(S), database, io__state, io__state).
:- mode print_solutions(in, in, in, mdi, in, di, uo) is cc_multi.

% The call to unsafe_promise_unique here is needed because without it,
% the following code gets a (spurious) unique mode error,
% because the compiler thinks that `Store0' has inst `ground'
% rather than `mostly_unique' when it is passed as a curried
% argument of a higher-order term.  The compiler doesn't know
% that unsorted_aggregate will only call its higher-order argument
% once per forward execution.
%
% It might be nicer to use do_while rather than unsorted_aggregate,
% so that we can prompt the user after each solution to see if they
% want to see the next solution.
%
print_solutions(VarSet, VarMap, MyGoal, Store0, Database) -->
	unsorted_aggregate(
		(pred(Store::muo) is nondet :-
			solve(Database, MyGoal, unsafe_promise_unique(Store0),
				Store)),
		write_solution(VarSet, VarMap, MyGoal)),
	io__write_string("No (more) solutions.\n").

:- pred write_solution(varset, map(var, my_var(S)), my_term(S), store(S),
			io__state, io__state).
:- mode write_solution(in, in, in, mdi, di, uo) is det.

write_solution(VarSet0, VarToMyVarMap, MyGoal, Store0) -->
	{ map__keys(VarToMyVarMap, Vars) },
	{ map__values(VarToMyVarMap, MyVars) },
	{ map__from_corresponding_lists(MyVars, Vars, VarMap0) },
	{ my_term_to_term(MyGoal, Goal, VarSet0, VarSet, VarMap0, _VarMap,
			Store0, _Store) },
	term_io__write_term_nl(VarSet, Goal).

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

% Here's how we represent terms.
% We don't use the Mercury standard library type `term', because
% that isn't efficient enough; we want variables to be represented
% as mutable variables using the store__mutvar type, so that we
% can implement variable binding as backtrackable destructive update,
% using the tr_store module.

:- type my_var(S)
	==	mutvar(my_term(S), S).

:- type my_term(S)
	--->	var(my_var(S))
	;	free
	;	functor(const, list(my_term(S))).

%-----------------------------------------------------------------------------%

% Convert from the standard Mercury `term' representation to
% our `my_term' representation.

:- pred term_to_my_term(term, my_term(S), store(S), store(S)).
:- mode term_to_my_term(in, out, mdi, muo) is det.

term_to_my_term(Term, MyTerm) -->
	{ map__init(VarMap0) },
	term_to_my_term(Term, MyTerm, VarMap0, _VarMap).

:- pred term_to_my_term_list(list(term), list(my_term(S)), store(S), store(S)).
:- mode term_to_my_term_list(in, out, mdi, muo) is det.

term_to_my_term_list(Terms, MyTerm) -->
	{ map__init(VarMap0) },
	term_to_my_term_list(Terms, MyTerm, VarMap0, _VarMap).

:- pred term_to_my_term(term, my_term(S),
			map(var, my_var(S)), map(var, my_var(S)),
			store(S), store(S)).
:- mode term_to_my_term(in, out, in, out, mdi, muo) is det.

term_to_my_term(variable(Var), var(Ref), VarMap0, VarMap) -->
	( { map__search(VarMap0, Var, Ref1) } ->
		{ Ref = Ref1 },
		{ VarMap = VarMap0 }
	;
		tr_store__new_mutvar(free, Ref),
		{ map__det_insert(VarMap0, Var, Ref, VarMap) }
	).
term_to_my_term(functor(Functor, Args0, _Context), functor(Functor, Args),
		VarMap0, VarMap) -->
	term_to_my_term_list(Args0, Args, VarMap0, VarMap).

:- pred term_to_my_term_list(list(term), list(my_term(S)),
		map(var, my_var(S)), map(var, my_var(S)), store(S), store(S)).
:- mode term_to_my_term_list(in, out, in, out, mdi, muo) is det.

term_to_my_term_list([], [], VarMap, VarMap) --> [].
term_to_my_term_list([Term0|Terms0], [Term|Terms], VarMap0, VarMap) -->
	term_to_my_term(Term0, Term, VarMap0, VarMap1),
	term_to_my_term_list(Terms0, Terms, VarMap1, VarMap).

%-----------------------------------------------------------------------------%

% Convert from our `my_term' representation to
% the standard Mercury `term' representation.

:- pred my_term_to_term(my_term(S), term, store(S), store(S)).
:- mode my_term_to_term(in, out, mdi, muo) is det.

my_term_to_term(MyTerm, Term) -->
	{ varset__init(VarSet0) },
	{ map__init(VarMap0) },
	my_term_to_term(MyTerm, Term, VarSet0, _VarSet, VarMap0, _VarMap).

:- pred my_term_to_term_list(list(my_term(S)), list(term), store(S), store(S)).
:- mode my_term_to_term_list(in, out, mdi, muo) is det.

my_term_to_term_list(MyTerms, Terms) -->
	{ varset__init(VarSet0) },
	{ map__init(VarMap0) },
	my_term_to_term_list(MyTerms, Terms,
		VarSet0, _VarSet, VarMap0, _VarMap).

:- pred my_term_to_term(my_term(S), term, varset, varset,
		map(my_var(S), var), map(my_var(S), var), store(S), store(S)).
:- mode my_term_to_term(in, out, in, out, in, out, mdi, muo) is det.

my_term_to_term(var(MyVar), variable(Var), VarSet0, VarSet, VarMap0, VarMap)
		-->
	%
	% check whether MyVar is in the VarMap;
	% if so, use its corresponding Var,
	% otherwise, create a fresh Var and insert it into the VarMap
	%
	( { map__search(VarMap0, MyVar, Var1) } ->
		{ Var = Var1 },
		{ VarSet1 = VarSet0 },
		{ VarMap1 = VarMap0 }
	;
		{ varset__new_var(VarSet0, Var, VarSet1) },
		{ map__det_insert(VarMap0, MyVar, Var, VarMap1) }
	),
	%
	% check whether MyVar is bound;
	% if so, insert its binding into the VarSet
	%
	tr_store__get_mutvar(MyVar, MyValue),
	( { MyValue \= free } ->
		my_term_to_term(MyValue, Value, VarSet1, VarSet2,
					VarMap1, VarMap),
		{ varset__bind_var(VarSet2, Var, Value, VarSet) }
	;
		{ VarMap = VarMap1 },
		{ VarSet = VarSet1 }
	).
my_term_to_term(free, variable(Var), VarSet0, VarSet, VarMap, VarMap) -->
	{ varset__new_var(VarSet0, Var, VarSet) },
	{ error("my_term_to_term: unexpected free var") }.
my_term_to_term(functor(Functor, Args0), functor(Functor, Args, Context),
		VarSet0, VarSet, VarMap0, VarMap) -->
	{ context_init(Context) },
	my_term_to_term_list(Args0, Args, VarSet0, VarSet, VarMap0, VarMap).

:- pred my_term_to_term_list(list(my_term(S)), list(term),
		varset, varset, map(my_var(S), var), map(my_var(S), var),
		store(S), store(S)).
:- mode my_term_to_term_list(in, out, in, out, in, out, mdi, muo) is det.

my_term_to_term_list([], [], VarSet, VarSet, VarMap, VarMap) --> [].
my_term_to_term_list([Term0|Terms0], [Term|Terms], VarSet0, VarSet,
		VarMap0, VarMap) -->
	my_term_to_term(Term0, Term, VarSet0, VarSet1, VarMap0, VarMap1),
	my_term_to_term_list(Terms0, Terms, VarSet1, VarSet, VarMap1, VarMap).

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

:- pred solve(database, my_term(S), store(S), store(S)).
:- mode solve(in, in, mdi, muo) is nondet.

solve(_Database, functor(atom("true"), [])) --> [].

solve(Database, functor(atom(","), [A, B])) -->
	solve(Database, A),
	solve(Database, B).

solve(Database, functor(atom(";"), [A, B])) -->
	solve(Database, A)
	;
	solve(Database, B).

solve(_Database, functor(atom("="), [A, B])) -->
	unify(A, B).

solve(Database, Goal) -->
	{ database_lookup_clause(Database, Goal, _VarSet, Head0, Body0) },
	term_to_my_term_list([Head0, Body0], [Head, Body]),
	unify(Goal, Head),
	solve(Database, Body).

/*
solve(Database, var(Var)) -->
	get_mutvar(Var, Value),
	solve(Database, Value).
*/
%-----------------------------------------------------------------------------%

:- pred unify(my_term(S), my_term(S), store(S), store(S)).
:- mode unify(in, in, mdi, muo) is semidet.

unify(var(X), var(Y)) -->
	tr_store__get_mutvar(X, BindingOfX),
	tr_store__get_mutvar(Y, BindingOfY),
	(
		{ BindingOfX \= free }
	->
		(
			{ BindingOfY \= free }
		->
			% both X and Y already have bindings - just
			% unify the terms they are bound to
			unify(BindingOfX, BindingOfY)
		;
			% Y is a variable which hasn't been bound yet
			deref(BindingOfX, SubstBindingOfX),
			( { SubstBindingOfX = var(Y) } ->
			 	[]
			;
				not_occurs(SubstBindingOfX, Y),
				tr_store__set_mutvar(Y, SubstBindingOfX)
			)
		)
	;
		(
			{ BindingOfY \= free }
		->
			% X is a variable which hasn't been bound yet
			deref(BindingOfY, SubstBindingOfY),
			( { SubstBindingOfY = var(X) } ->
				[]
			;
				not_occurs(SubstBindingOfY, X),
				tr_store__set_mutvar(X, SubstBindingOfY)
			)
		;
			% both X and Y are unbound variables -
			% bind one to the other
			( { X = Y } ->
				[]
			;
				tr_store__set_mutvar(X, var(Y))
			)
		)
	).

unify(var(X), functor(F, As)) -->
	tr_store__get_mutvar(X, BindingOfX),
	(
		{ BindingOfX \= free }
	->
		unify(BindingOfX, functor(F, As))
	;
		not_occurs_list(As, X),
		tr_store__set_mutvar(X, functor(F, As))
	).

unify(functor(F, As), var(X)) -->
	tr_store__get_mutvar(X, BindingOfX),
	(
		{ BindingOfX \= free }
	->
		unify(functor(F, As), BindingOfX)
	;
		not_occurs_list(As, X),
		tr_store__set_mutvar(X, functor(F, As))
	).

unify(functor(F, AsX), functor(F, AsY)) -->
	unify_list(AsX, AsY).

:- pred unify_list(list(my_term(S)), list(my_term(S)), store(S), store(S)).
:- mode unify_list(in, in, mdi, muo) is semidet.

unify_list([], []) --> [].
unify_list([X | Xs], [Y | Ys]) -->
	unify(X, Y),
	unify_list(Xs, Ys).

%-----------------------------------------------------------------------------%

	% not_occurs(Term, Var, Store0, Store) fails if Term contains Var,
	% perhaps indirectly via the substitution in Store0.
	% (The variable must not be mapped by the substitution.)

:- pred not_occurs(my_term(S), my_var(S), store(S), store(S)).
:- mode not_occurs(in, in, mdi, muo) is semidet.

not_occurs(var(X), Y) -->
	{ X \= Y },
	tr_store__get_mutvar(X, BindingOfX),
	( { BindingOfX = free } ->
		[]
	;
		not_occurs(BindingOfX, Y)
	).
not_occurs(functor(_F, As), Y) -->
	not_occurs_list(As, Y).

:- pred not_occurs_list(list(my_term(S)), my_var(S), store(S), store(S)).
:- mode not_occurs_list(in, in, mdi, muo) is semidet.

not_occurs_list([], _) --> [].
not_occurs_list([Term | Terms], Y) -->
	not_occurs(Term, Y),
	not_occurs_list(Terms, Y).

%-----------------------------------------------------------------------------%

%	deref(Term0, Term, Store0, Store) :
%		recursively apply substitution to Term0 until
%		no more substitions can be applied, and then
%		return the result in Term.

:- pred deref(my_term(S), my_term(S), store(S), store(S)).
:- mode deref(in, out, mdi, muo) is det.

deref(free, _) -->
	{ error("interpreter__deref: unexpected occurence of `free'") }.
deref(var(Var), Term) -->
	tr_store__get_mutvar(Var, Replacement),
	(
		{ Replacement \= free }
	->
		% recursively apply the substition to the replacement
		deref(Replacement, Term)
	;
		{ Term = var(Var) }
	).
deref(functor(Name, Args0), functor(Name, Args)) -->
	deref_list(Args0, Args).

:- pred deref_list(list(my_term(S)), list(my_term(S)), store(S), store(S)).
:- mode deref_list(in, out, mdi, muo) is det.

deref_list([], []) --> [].
deref_list([Term0 | Terms0], [Term | Terms]) -->
	deref(Term0, Term),
	deref_list(Terms0, Terms).

%-----------------------------------------------------------------------------%

% The database of clauses is indexed by predicate name/arity,
% and for each predicate the clauses are indexed according to the
% name/arity of their first argument.

:- type database ---> database(
		list(clause),			% clauses with variable as head
		map(string/int, db_pred)	% preds, indexed on name/arity
	).
:- type db_pred ---> db_pred(
		list(clause),			% unindexed clauses
						% (ones with var as first arg,
						% or with no args)
		multi_map(string/int, clause)	% clauses, indexed on the
						% name/arity of first arg
	).

:- type Name/Arity ---> Name/Arity.

:- type clause ---> clause(varset, term, term). % varset, head, body

:- pred database_init(database).
:- mode database_init(out) is det.

database_init(database([], Preds)) :-
	map__init(Preds).

:- pred database_assert_clause(database, varset, term, database).
:- mode database_assert_clause(in, in, in, out) is det.

database_assert_clause(Database0, VarSet, Term, Database) :-
	%
	% add `:- true' if clause not already in the form `H :- B'
	%
	( Term = functor(atom(":-"), [H, B], _) ->
		Head = H,
		Body = B
	;
		Head = Term,
		context_init(Context),
		Body = functor(atom("true"), [], Context)
	),
	Clause = clause(VarSet, Head, Body),

	%
	% insert clause into database
	%
	Database0 = database(UnindexedClauses, Preds0),
	( Head = functor(atom(PredName), PredArgs, _) ->
		%
		% we can do predicate name/arity indexing
		%
		list__length(PredArgs, PredArity),
		PredId = PredName / PredArity,
		(
			PredArgs = [FirstArg | _],
			FirstArg = functor(atom(FirstArgName), FirstArgArgs, _)
		->
			%
			% we can do first-argument name/arity indexing
			%
			list__length(FirstArgArgs, FirstArgArity),
			FirstArgId = FirstArgName / FirstArgArity,
			( map__search(Preds0, PredId, Pred0) ->
				Pred0 = db_pred(PredUnindexedClauses,
						PredIndexedClauses0),
				multi_map__set(PredIndexedClauses0, FirstArgId,
					Clause, PredIndexedClauses),
				Pred = db_pred(PredUnindexedClauses,
						PredIndexedClauses),
				map__det_update(Preds0, PredId, Pred, Preds)
			;
				multi_map__init(PredIndexedClauses0),
				multi_map__set(PredIndexedClauses0, FirstArgId,
					Clause, PredIndexedClauses),
				Pred = db_pred([], PredIndexedClauses),
				map__det_insert(Preds0, PredId, Pred, Preds)
			)
		;
			%
			% we can't do first-argument indexing -- just
			% insert into the unindexed clauses
			%
			( map__search(Preds0, PredId, Pred0) ->
				Pred0 = db_pred(PredUnindexedClauses,
						PredIndexedClauses),
				Pred = db_pred([Clause | PredUnindexedClauses],
						PredIndexedClauses),
				map__det_update(Preds0, PredId, Pred, Preds)
			;
				multi_map__init(PredIndexedClauses),
				Pred = db_pred([Clause], PredIndexedClauses),
				map__det_insert(Preds0, PredId, Pred, Preds)
			)
		),
		Database = database(UnindexedClauses, Preds)
	;
		Database = database([Clause|UnindexedClauses], Preds0)
	).

:- pred database_lookup_clause(database, my_term(_), varset, term, term).
:- mode database_lookup_clause(in, in, out, out, out) is nondet.

database_lookup_clause(Database, Goal, VarSet, Head, Body) :-
	database_lookup_clause(Database, Goal, Clause),
	Clause = clause(VarSet, Head, Body).

:- pred database_lookup_clause(database, my_term(_), clause).
:- mode database_lookup_clause(in, in, out) is nondet.

database_lookup_clause(database(Clauses, _Preds), _Goal, Clause) :-
	list__member(Clause, Clauses).

database_lookup_clause(database(_Clauses, Preds), Goal, Clause) :-
	Goal = functor(atom(PredName), PredArgs),
	list__length(PredArgs, PredArity),
	map__search(Preds, PredName/PredArity, PredClauses),
	database_lookup_pred_clause(PredClauses, PredArgs, Clause).

:- pred database_lookup_pred_clause(db_pred, list(my_term(_)), clause).
:- mode database_lookup_pred_clause(in, in, out) is nondet.

database_lookup_pred_clause(db_pred(Clauses, _IndexedClauses), _, Clause) :-
	list__member(Clause, Clauses).

database_lookup_pred_clause(db_pred(_, IndexedClauses), PredArgs, Clause) :-
	PredArgs = [FirstArg | _],
	(
		FirstArg = var(_),
		multi_map__member(IndexedClauses, _, Clause)
	;
		FirstArg = functor(atom(FirstArgName), FirstArgArgs),
		list__length(FirstArgArgs, FirstArgArity),
		multi_map__nondet_lookup(IndexedClauses,
			FirstArgName/FirstArgArity, Clause)
	).

%-----------------------------------------------------------------------------%
