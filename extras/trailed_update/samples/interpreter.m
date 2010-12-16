%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: interpreter.m.
% Main author: fjh.
%
% This is an interpreter for definite logic programs
% (i.e. pure Prolog with no negation or if-then-else.)
%
% This is just intended as a demonstration of the use of the
% library module tr_store.m.
%
% There are many extensions/improvements that could be made;
% they're left as an exercise for the reader.
%
% This source file is hereby placed in the public domain. -fjh (the author).
%
%-----------------------------------------------------------------------------%

:- module interpreter.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module tr_store.
:- import_module unsafe.

:- import_module assoc_list.
:- import_module map.
:- import_module multi_map.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module solutions.
:- import_module store.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module term_io.
:- import_module varset.

:- pragma require_feature_set([trailing]).

%-----------------------------------------------------------------------------%

main(!IO) :-
	io.write_string("Pure Prolog Interpreter.\n\n", !IO),
	io.command_line_arguments(Args, !IO),
	(
        Args = [],
		io.stderr_stream(StdErr, !IO),
		io.write_string(StdErr, "Usage: interpreter filename ...\n",
            !IO),
		io.set_exit_status(1, !IO)
	;
        Args = [_ | _],
		database_init(Database0),
		consult_list(Args, Database0, Database, !IO),
		main_loop(Database, !IO)
	).

:- pred main_loop(database::in, io::di, io::uo) is cc_multi.

main_loop(Database, !IO) :-
	io.write_string("?- ", !IO),
	term_io.read_term(ReadTerm, !IO),
	main_loop_2(ReadTerm, Database, !IO).

:- pred main_loop_2(read_term::in, database::in,
    io::di, io::uo) is cc_multi.

main_loop_2(eof, _Database, !IO).
main_loop_2(error(ErrorMessage, LineNumber), Database, !IO) :-
    io.format("Error reading term at line %d of standard input: %s\n",
        [i(LineNumber), s(ErrorMessage)], !IO),
	main_loop(Database, !IO).
main_loop_2(term(VarSet, Goal), Database, !IO) :-
	%%% It would be a good idea to add some special commands
	%%% with side-effects (such as `consult' and `listing');
	%%% these could be identified and processed here.
	store.new(Store0),
	map.init(VarMap0),
	term_to_my_term(Goal, MyGoal, VarMap0, VarMap, Store0, Store1),
	print_solutions(VarSet, VarMap, MyGoal, Store1, Database, !IO),
	main_loop(Database, !IO).

:- pred print_solutions(varset::in, map(var, my_var(S))::in, my_term(S)::in,
    store(S)::mdi, database::in, io::di, io::uo) is cc_multi.

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
print_solutions(VarSet, VarMap, MyGoal, Store0, Database, !IO) :-
    SolvePred = (pred(Store::muo) is nondet :-
	    solve(Database, MyGoal, unsafe_promise_unique(Store0), Store)
    ),
	unsorted_aggregate(SolvePred, write_solution(VarSet, VarMap, MyGoal),
        !IO),
	io.write_string("No (more) solutions.\n", !IO).

:- pred write_solution(varset::in, map(var, my_var(S))::in,
    my_term(S)::in, store(S)::mdi, io::di, io::uo) is det.

write_solution(VarSet0, VarToMyVarMap, MyGoal, Store0, !IO) :-
	map.keys(VarToMyVarMap, Vars),
	map.values(VarToMyVarMap, MyVars),
	assoc_list.from_corresponding_lists(MyVars, Vars, VarMap0),
	my_term_to_term(MyGoal, Goal, VarSet0, VarSet, VarMap0, _VarMap,
	    Store0, _Store),
	term_io.write_term_nl(VarSet, Goal, !IO).

%-----------------------------------------------------------------------------%

:- pred consult_list(list(string)::in, database::in, database::out,
    io::di, io::uo) is det.

consult_list([], !Database, !IO).
consult_list([File | Files], !Database, !IO) :-
	consult(File, !Database, !IO),
	consult_list(Files, !Database, !IO).

:- pred consult(string::in, database::in, database::out,
    io::di, io::uo) is det.

consult(File, !Database, !IO) :-
    io.format("Consulting file `%s'...\n", [s(File)], !IO),
	io.see(File, Result, !IO),
	(
        Result = ok,
		consult_until_eof(!Database, !IO),
		io.seen(!IO)
	;
        Result = error(_),
        io.format("Error opening file `%s' for input.\n", [s(File)], !IO)
	).

:- pred consult_until_eof(database::in, database::out, io::di, io::uo) is det.

consult_until_eof(!Database, !IO) :-
	term_io.read_term(ReadTerm, !IO),
	consult_until_eof_2(ReadTerm, !Database, !IO).

:- pred consult_until_eof_2(read_term::in, database::in, database::out,
    io::di, io::uo) is det.

consult_until_eof_2(eof, !Database, !IO).

consult_until_eof_2(error(ErrorMessage, LineNumber), !Database, !IO) :-
    io.format("Error reading term at line %d of standard input: %s\n",
        [i(LineNumber), s(ErrorMessage)], !IO),
	consult_until_eof(!Database, !IO).

consult_until_eof_2(term(VarSet, Term), !Database, !IO) :-
	database_assert_clause(VarSet, Term, !Database),
	consult_until_eof(!Database, !IO).

%-----------------------------------------------------------------------------%

% Here's how we represent terms.
% We don't use the Mercury standard library type `term', because
% that isn't efficient enough; we want variables to be represented
% as mutable variables using the store.mutvar type, so that we
% can implement variable binding as backtrackable destructive update,
% using the tr_store module.

:- type my_var(S) == generic_mutvar(my_term(S), S).

:- type my_term(S)
	--->	var(my_var(S))
	;	    free
	;	    functor(const, list(my_term(S))).

%-----------------------------------------------------------------------------%

% Convert from the standard Mercury `term' representation to
% our `my_term' representation.

:- pred term_to_my_term(term::in, my_term(S)::out,
    store(S)::mdi, store(S)::muo) is det.

term_to_my_term(Term, MyTerm, !S) :-
	map.init(VarMap0),
	term_to_my_term(Term, MyTerm, VarMap0, _VarMap, !S).

:- pred term_to_my_term_list(list(term)::in, list(my_term(S))::out,
    store(S)::mdi, store(S)::muo) is det.

term_to_my_term_list(Terms, MyTerm, !S) :-
	map.init(VarMap0),
	term_to_my_term_list(Terms, MyTerm, VarMap0, _VarMap, !S).

:- pred term_to_my_term(term::in, my_term(S)::out,
    map(var, my_var(S))::in, map(var, my_var(S))::out,
    store(S)::mdi, store(S)::muo) is det.

term_to_my_term(variable(Var, _), var(Ref), !VarMap, !S) :-
	( map.search(!.VarMap, Var, Ref0) ->
		Ref = Ref0
	;
		tr_store.new_mutvar(free, Ref, !S),
		svmap.det_insert(Var, Ref, !VarMap)
	).
term_to_my_term(functor(Functor, Args0, _Context), functor(Functor, Args),
		!VarMap, !S) :-
	term_to_my_term_list(Args0, Args, !VarMap, !S).

:- pred term_to_my_term_list(list(term)::in, list(my_term(S))::out,
    map(var, my_var(S))::in, map(var, my_var(S))::out,
    store(S)::mdi, store(S)::muo) is det.

term_to_my_term_list([], [], !VarMap, !S).
term_to_my_term_list([Term0 | Terms0], [Term | Terms], !VarMap, !S) :-
	term_to_my_term(Term0, Term, !VarMap, !S),
	term_to_my_term_list(Terms0, Terms, !VarMap, !S).

%-----------------------------------------------------------------------------%

% Convert from our `my_term' representation to
% the standard Mercury `term' representation.

:- pred my_term_to_term(my_term(S)::in, term::out,
    store(S)::mdi, store(S)::muo) is det.

my_term_to_term(MyTerm, Term, !S) :-
	varset.init(VarSet0),
	VarMap0 = [],
	my_term_to_term(MyTerm, Term, VarSet0, _VarSet, VarMap0, _VarMap, !S).

:- pred my_term_to_term_list(list(my_term(S))::in, list(term)::out,
    store(S)::mdi, store(S)::muo) is det.

my_term_to_term_list(MyTerms, Terms, !S) :-
	varset.init(VarSet0),
	VarMap0 = [],
	my_term_to_term_list(MyTerms, Terms, VarSet0, _VarSet,
        VarMap0, _VarMap, !S).

% Note that we need to use an assoc_list here rather than a map,
% because store mutvars can only be tested for equality, not compared
% (this in turn is because in implementations which use copying GC,
% the relative addresses of different mutvars might change after
% a garbage collection).

:- pred my_term_to_term(my_term(S)::in, term::out, varset::in, varset::out,
    assoc_list(my_var(S), var)::in, assoc_list(my_var(S), var)::out,
    store(S)::mdi, store(S)::muo) is det.

my_term_to_term(var(MyVar), variable(Var, Context), !VarSet, !VarMap, !S) :-
	context_init(Context),
	%
	% Check whether MyVar is in the VarMap;
	% if so, use its corresponding Var,
	% otherwise, create a fresh Var and insert it into the VarMap.
	%
	( assoc_list.search(!.VarMap, MyVar, Var0) ->
		Var = Var0
	;
		varset.new_var(!.VarSet, Var, !:VarSet),
		!:VarMap = [MyVar - Var | !.VarMap]
	),
	%
	% Check whether MyVar is bound;
	% if so, insert its binding into the VarSet.
	%
	tr_store.get_mutvar(MyVar, MyValue, !S),
	( MyValue \= free ->
		my_term_to_term(MyValue, Value, !VarSet, !VarMap, !S),
		varset.bind_var(!.VarSet, Var, Value, !:VarSet)
	;
        true
	).
my_term_to_term(free, variable(Var, Context), !VarSet, !VarMap, !S) :-
	context_init(Context),
	varset.new_var(!.VarSet, Var, !:VarSet),
	error("my_term_to_term: unexpected free var").
my_term_to_term(functor(Functor, Args0), functor(Functor, Args, Context),
		!VarSet, !VarMap, !S) :-
	context_init(Context),
	my_term_to_term_list(Args0, Args, !VarSet, !VarMap, !S).

:- pred my_term_to_term_list(list(my_term(S))::in, list(term)::out,
    varset::in, varset::out,
	assoc_list(my_var(S), var)::in, assoc_list(my_var(S), var)::out,
	store(S)::mdi, store(S)::muo) is det.

my_term_to_term_list([], [], !VarSet, !VarMap, !S).
my_term_to_term_list([Term0 | Terms0], [Term | Terms], !VarSet, !VarMap, !S) :-
	my_term_to_term(Term0, Term, !VarSet, !VarMap, !S),
	my_term_to_term_list(Terms0, Terms, !VarSet, !VarMap, !S).

%-----------------------------------------------------------------------------%

% Solve takes a database of rules and facts, a goal to be solved, and a varset
% (which includes a supply of fresh vars, a substitution, and names for [some
% subset of] the variables).  It updates the varset, producing a new
% substitution and perhaps introducing some new vars, and returns the result.

% Goals are stored just as terms.
% (It might be more efficient to parse them before storing them in the
% database.  Currently we do this parsing work every time we interpret a
% clause.)

:- pred solve(database::in, my_term(S)::in,
    store(S)::mdi, store(S)::muo) is nondet.

solve(_Database, functor(atom("true"), []), !S).

solve(Database, functor(atom(","), [A, B]), !S) :-
	solve(Database, A, !S),
	solve(Database, B, !S).

solve(Database, functor(atom(";"), [A, B]), !S) :-
    (
	    solve(Database, A, !S)
	;
	    solve(Database, B, !S)
    ).

solve(_Database, functor(atom("="), [A, B]), !S) :-
	unify(A, B, !S).

solve(Database, Goal, !S) :-
	database_lookup_clause(Database, Goal, _VarSet, Head0, Body0),
	term_to_my_term_list([Head0, Body0], [Head, Body], !S),
	unify(Goal, Head, !S),
	solve(Database, Body, !S).

/*
solve(Database, var(Var)) -->
	get_mutvar(Var, Value),
	solve(Database, Value).
*/
%-----------------------------------------------------------------------------%

:- pred unify(my_term(S)::in, my_term(S)::in, store(S)::mdi, store(S)::muo)
    is semidet.

unify(var(X), var(Y), !S) :-
	tr_store.get_mutvar(X, BindingOfX, !S),
	tr_store.get_mutvar(Y, BindingOfY, !S),
	(
		BindingOfX \= free
	->
		(
			BindingOfY \= free
		->
			% Both X and Y already have bindings - just
			% unify the terms they are bound to.
			unify(BindingOfX, BindingOfY)
		;
			% Y is a variable which hasn't been bound yet.
			deref(BindingOfX, SubstBindingOfX, !S),
			( SubstBindingOfX = var(Y) ->
                true
			;
				not_occurs(SubstBindingOfX, Y, !S),
				tr_store.set_mutvar(Y, SubstBindingOfX, !S)
			)
		)
	;
		(
			BindingOfY \= free
		->
			% X is a variable which hasn't been bound yet.
			deref(BindingOfY, SubstBindingOfY, !S),
			( SubstBindingOfY = var(X) ->
		        true	
			;
				not_occurs(SubstBindingOfY, X, !S),
				tr_store.set_mutvar(X, SubstBindingOfY, !S)
			)
		;
			% Both X and Y are unbound variables -
			% bind one to the other.
			( X = Y ->
		        true	
			;
				tr_store.set_mutvar(X, var(Y), !S)
			)
		)
	).

unify(var(X), functor(F, As), !S) :-
	tr_store.get_mutvar(X, BindingOfX, !S),
	(
		BindingOfX \= free
	->
		unify(BindingOfX, functor(F, As), !S)
	;
		not_occurs_list(As, X, !S),
		tr_store.set_mutvar(X, functor(F, As), !S)
	).

unify(functor(F, As), var(X), !S) :-
	tr_store.get_mutvar(X, BindingOfX, !S),
	(
		BindingOfX \= free
	->
		unify(functor(F, As), BindingOfX, !S)
	;
		not_occurs_list(As, X, !S),
		tr_store.set_mutvar(X, functor(F, As), !S)
	).

unify(functor(F, AsX), functor(F, AsY), !S) :-
	unify_list(AsX, AsY, !S).

:- pred unify_list(list(my_term(S))::in, list(my_term(S))::in,
    store(S)::mdi, store(S)::muo) is semidet.

unify_list([], [], !S).
unify_list([X | Xs], [Y | Ys], !S) :-
	unify(X, Y, !S),
	unify_list(Xs, Ys, !S).

%-----------------------------------------------------------------------------%

	% not_occurs(Term, Var, Store0, Store) fails if Term contains Var,
	% perhaps indirectly via the substitution in Store0.
	% (The variable must not be mapped by the substitution.)

:- pred not_occurs(my_term(S)::in, my_var(S)::in,
    store(S)::mdi, store(S)::muo) is semidet.

not_occurs(var(X), Y, !S) :-
	X \= Y,
	tr_store.get_mutvar(X, BindingOfX, !S),
	( BindingOfX = free ->
	    true	
	;
		not_occurs(BindingOfX, Y, !S)
	).
not_occurs(functor(_F, As), Y, !S)  :-
	not_occurs_list(As, Y, !S).

:- pred not_occurs_list(list(my_term(S))::in, my_var(S)::in,
    store(S)::mdi, store(S)::muo) is semidet.

not_occurs_list([], _, !S).
not_occurs_list([Term | Terms], Y, !S) :-
	not_occurs(Term, Y, !S),
	not_occurs_list(Terms, Y, !S).

%-----------------------------------------------------------------------------%

    % deref(Term0, Term, !Store):
    % Recursively apply substitution to Term0 until no more substitutions can
    % be applied, and then return the result in Term.
    %
:- pred deref(my_term(S)::in, my_term(S)::out, store(S)::mdi, store(S)::muo)
    is det.

deref(free, _, _, _) :-
	error("interpreter.deref: unexpected occurrence of `free'").
deref(var(Var), Term, !S) :-
	tr_store.get_mutvar(Var, Replacement, !S),
	(
		Replacement \= free
	->
		% Recursively apply the substitution to the replacement.
		deref(Replacement, Term, !S)
	;
		Term = var(Var)
	).
deref(functor(Name, Args0), functor(Name, Args), !S) :-
	deref_list(Args0, Args, !S).

:- pred deref_list(list(my_term(S))::in, list(my_term(S))::out,
    store(S)::mdi, store(S)::muo) is det.

deref_list([], [], !S).
deref_list([Term0 | Terms0], [Term | Terms], !S) :-
	deref(Term0, Term, !S),
	deref_list(Terms0, Terms, !S).

%-----------------------------------------------------------------------------%

% The database of clauses is indexed by predicate name/arity,
% and for each predicate the clauses are indexed according to the
% name/arity of their first argument.

:- type database
    --->    database(
		        list(clause),
                % Clauses with variable as head.

		        map(string/int, db_pred)
                % Preds, indexed on name/arity.
	        ).

:- type db_pred
    --->    db_pred(
		        list(clause),
                % Unindexed clauses
                % (ones with var as first arg, or with no args).

		        multi_map(string/int, clause)
                % Clauses, indexed on the  name/arity of first arg.
	        ).

:- type Name/Arity ---> Name/Arity.

:- type clause
    --->    clause(varset, term, term). % varset, head, body

:- pred database_init(database::out) is det.

database_init(database([], Preds)) :-
	map.init(Preds).

:- pred database_assert_clause(varset::in, term::in,
    database::in, database::out) is det.

database_assert_clause(VarSet, Term, !Database) :-
	%
	% Add `:- true' if clause not already in the form `H :- B'.
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
	% Insert clause into database.
	%
	!.Database = database(UnindexedClauses, Preds0),
	( Head = functor(atom(PredName), PredArgs, _) ->
		%
		% We can do predicate name/arity indexing.
		%
		list.length(PredArgs, PredArity),
		PredId = PredName / PredArity,
		(
			PredArgs = [FirstArg | _],
			FirstArg = functor(atom(FirstArgName), FirstArgArgs, _)
		->
			% We can do first-argument name/arity indexing.
			%
			list.length(FirstArgArgs, FirstArgArity),
			FirstArgId = FirstArgName / FirstArgArity,
			( map.search(Preds0, PredId, Pred0) ->
				Pred0 = db_pred(PredUnindexedClauses,
				    PredIndexedClauses0),
				multi_map.set(PredIndexedClauses0, FirstArgId,
					Clause, PredIndexedClauses),
				Pred = db_pred(PredUnindexedClauses,
				    PredIndexedClauses),
				map.det_update(Preds0, PredId, Pred, Preds)
			;
				multi_map.init(PredIndexedClauses0),
				multi_map.set(PredIndexedClauses0, FirstArgId,
					Clause, PredIndexedClauses),
				Pred = db_pred([], PredIndexedClauses),
				map.det_insert(Preds0, PredId, Pred, Preds)
			)
		;
			% We can't do first-argument indexing -- just
			% insert into the unindexed clauses.
			%
			( map.search(Preds0, PredId, Pred0) ->
				Pred0 = db_pred(PredUnindexedClauses,
						PredIndexedClauses),
				Pred = db_pred([Clause | PredUnindexedClauses],
						PredIndexedClauses),
				map.det_update(Preds0, PredId, Pred, Preds)
			;
				multi_map.init(PredIndexedClauses),
				Pred = db_pred([Clause], PredIndexedClauses),
				map.det_insert(Preds0, PredId, Pred, Preds)
			)
		),
		!:Database = database(UnindexedClauses, Preds)
	;
		!:Database = database([Clause | UnindexedClauses], Preds0)
	).

:- pred database_lookup_clause(database::in, my_term(_)::in, varset::out,
    term::out, term::out) is nondet.

database_lookup_clause(Database, Goal, VarSet, Head, Body) :-
	database_lookup_clause(Database, Goal, Clause),
	Clause = clause(VarSet, Head, Body).

:- pred database_lookup_clause(database::in, my_term(_)::in, clause::out) 
    is nondet.

database_lookup_clause(database(Clauses, _Preds), _Goal, Clause) :-
	list.member(Clause, Clauses).

database_lookup_clause(database(_Clauses, Preds), Goal, Clause) :-
	Goal = functor(atom(PredName), PredArgs),
	list.length(PredArgs, PredArity),
	map.search(Preds, PredName/PredArity, PredClauses),
	database_lookup_pred_clause(PredClauses, PredArgs, Clause).

:- pred database_lookup_pred_clause(db_pred::in, list(my_term(_))::in,
    clause::out) is nondet.

database_lookup_pred_clause(db_pred(Clauses, _IndexedClauses), _, Clause) :-
	list.member(Clause, Clauses).

database_lookup_pred_clause(db_pred(_, IndexedClauses), PredArgs, Clause) :-
	PredArgs = [FirstArg | _],
	(
		FirstArg = var(_),
		multi_map.member(IndexedClauses, _, Clause)
	;
		FirstArg = functor(atom(FirstArgName), FirstArgArgs),
		list.length(FirstArgArgs, FirstArgArity),
		multi_map.nondet_lookup(IndexedClauses,
			FirstArgName/FirstArgArity, Clause)
	).

%-----------------------------------------------------------------------------%
:- end_module interpreter.
%-----------------------------------------------------------------------------%
