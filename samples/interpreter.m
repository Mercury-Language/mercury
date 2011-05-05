%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% 
% File: interpreter.m.
% Main author: fjh.
% 
% This is an interpreter for definite logic programs
% (i.e. pure Prolog with no negation or if-then-else.)
%
% This is just intended as a demonstration of the use of the
% meta-programming library modules term, varset, and term_io.
% 
% There are many extensions/improvements that could be made;
% they're left as an exercise for the reader.
% 
% For a more efficient version (using backtrackable destructive update),
% see extras/trailed_update/samples/interpreter.m.
% 
% This source file is hereby placed in the public domain.  -fjh (the author).
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module interpreter.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("Pure Prolog Interpreter.\n\n", !IO),
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        io.stderr_stream(StdErr, !IO),
        io.write_string(StdErr, "Usage: interpreter <filename> ...\n", !IO),
        io.set_exit_status(1, !IO)
    ;
        Args = [_ | _],
        database_init(Database0),
        consult_list(Args, Database0, Database, !IO),
        main_loop(Database, !IO)
    ).

:- pred main_loop(database::in, io::di, io::uo) is det.

main_loop(Database, !IO) :-
    io.write_string("?- ", !IO),
    io.flush_output(!IO),
    term_io.read_term(ReadTerm, !IO),
    main_loop_2(ReadTerm, Database, !IO).

:- pred main_loop_2(read_term::in, database::in, io::di, io::uo) is det.

main_loop_2(eof, _Database, !IO).
main_loop_2(error(ErrorMessage, LineNumber), Database, !IO) :-
    io.write_string("Error reading term at line ", !IO),
    io.write_int(LineNumber, !IO),
    io.write_string(" of standard input: ", !IO),
    io.write_string(ErrorMessage, !IO),
    io.write_string("\n", !IO),
    main_loop(Database, !IO).
main_loop_2(term(VarSet0, Goal), Database, !IO) :-
    %%% It would be a good idea to add some special commands
    %%% with side-effects (such as `consult' and `listing');
    %%% these could be identified and processed here.
    solutions(solve(Database, Goal, VarSet0), Solutions),
    write_solutions(Solutions, Goal, !IO),
    main_loop(Database, !IO).

:- pred write_solutions(list(varset)::in, term::in, io::di, io::uo) is det.

write_solutions(Solutions, Goal, !IO) :-
    (
        Solutions = [],
        io.write_string("No.\n", !IO)
    ;
        Solutions = [_ | _],
        write_solutions_2(Solutions, Goal, !IO),
        io.write_string("Yes.\n", !IO)
    ).

:- pred write_solutions_2(list(varset)::in, term::in, io::di, io::uo) is det.

write_solutions_2([], _, !IO).
write_solutions_2([VarSet | VarSets], Goal, !IO) :-
    term_io.write_term_nl(VarSet, Goal, !IO),
    write_solutions_2(VarSets, Goal, !IO).

%-----------------------------------------------------------------------------%

:- pred consult_list(list(string)::in, database::in, database::out, 
    io::di, io::uo) is det.

consult_list([], !Database, !IO).
consult_list([File | Files], !Database, !IO) :-
    consult(File, !Database, !IO),
    consult_list(Files, !Database, !IO).

:- pred consult(string::in, database::in, database::out, io::di, io::uo)    
    is det.

consult(File, !Database, !IO) :-
    io.write_string("Consulting file `", !IO),
    io.write_string(File, !IO),
    io.write_string("'...\n", !IO),
    io.see(File, Result, !IO),
    (
        Result = ok,
        consult_until_eof(!Database, !IO),
        io.seen(!IO)
    ;
        Result = error(Error),
        io.error_message(Error, ErrorMessage),
        io.write_string("Error opening file `", !IO),
        io.write_string(File, !IO),
        io.write_string("' for input: ", !IO),
        io.write_string(ErrorMessage, !IO),
        io.nl(!IO)
    ).

:- pred consult_until_eof(database::in, database::out, io::di, io::uo) is det.

consult_until_eof(!Database, !IO) :-
    term_io.read_term(ReadTerm, !IO),
    consult_until_eof_2(ReadTerm, !Database, !IO).

:- pred consult_until_eof_2(read_term::in, database::in, database::out,
    io::di, io::uo) is det.

consult_until_eof_2(eof, !Database, !IO).
consult_until_eof_2(error(ErrorMessage, LineNumber), !Database, !IO) :-
    io.write_string("Error reading term at line ", !IO),
    io.write_int(LineNumber, !IO),
    io.write_string(" of standard input: ", !IO),
    io.write_string(ErrorMessage, !IO),
    io.write_string("\n", !IO),
    consult_until_eof(!Database, !IO).
consult_until_eof_2(term(VarSet, Term), !Database, !IO) :-
    database_assert_clause(!.Database, VarSet, Term, !:Database),
    consult_until_eof(!Database, !IO).

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

:- pred solve(database::in, term::in, varset::in, varset::out) is nondet.

solve(_Database, term.functor(term.atom("true"), [], _), !VarSet).
solve(Database, term.functor(term.atom(","), [A, B], _), !VarSet) :-
    solve(Database, A, !VarSet),
    solve(Database, B, !VarSet).
solve(Database, term.functor(term.atom(";"), [A, B], _), !VarSet) :-
    (
        solve(Database, A, !VarSet)
    ;
        solve(Database, B, !VarSet)
    ).
solve(_Database, term.functor(term.atom("="), [A, B], _), !VarSet) :-
    unify(A, B, !VarSet).
solve(Database, Goal, !VarSet) :-
    database_lookup_clause(Database, Goal, ClauseVarSet, Head0, Body0),
    rename_apart(ClauseVarSet, [Head0, Body0], [Head, Body], !VarSet),
    unify(Goal, Head, !VarSet),
    solve(Database, Body, !VarSet).

%-----------------------------------------------------------------------------%

:- pred rename_apart(varset::in, list(term)::in, list(term)::out, 
    varset::in, varset::out) is det.

rename_apart(NewVarSet, Terms0, Terms, VarSet0, VarSet) :-
    varset.merge(VarSet0, NewVarSet, Terms0, VarSet, Terms).

%-----------------------------------------------------------------------------%

% The standard library module `term' contains routines for
% unifying terms based on separate substitutions, but we are
% using the substitutions that are contained in the `varset',
% so we can't use those versions.

:- pred unify(term::in, term::in, varset::in, varset::out) is semidet.

unify(term.variable(X, _), term.variable(Y, _), !VarSet) :-
    ( varset.search_var(!.VarSet, X, BindingOfX) ->
        ( varset.search_var(!.VarSet, Y, BindingOfY) ->
            % Both X and Y already have bindings - just
            % unify the terms they are bound to.
            unify(BindingOfX, BindingOfY, !VarSet)
        ;
            % Y is a variable which hasn't been bound yet
            apply_rec_substitution(BindingOfX, !.VarSet, SubstBindingOfX),
            ( SubstBindingOfX = term.variable(Y, _) ->
                true
            ;
                \+ occurs(SubstBindingOfX, Y, !.VarSet),
                varset.bind_var(Y, SubstBindingOfX, !VarSet)
            )
        )
    ;
        ( varset.search_var(!.VarSet, Y, BindingOfY2) ->
            % X is a variable which hasn't been bound yet.
            apply_rec_substitution(BindingOfY2, !.VarSet, SubstBindingOfY2),
            ( SubstBindingOfY2 = term.variable(X, _) ->
                true
            ;
                \+ occurs(SubstBindingOfY2, X, !.VarSet),
                varset.bind_var(X, SubstBindingOfY2, !VarSet)
            )
        ;
            % Both X and Y are unbound variables -
            % bind one to the other.
            ( X = Y ->
                true
            ;
                varset.bind_var(X, term.variable(Y, context_init), !VarSet)
            )
        )
    ).

unify(term.variable(X, _), term.functor(F, As, C), !VarSet) :-
    ( varset.search_var(!.VarSet, X, BindingOfX) ->
        unify(BindingOfX, term.functor(F, As, C), !VarSet)
    ;
        \+ occurs_list(As, X, !.VarSet),
        varset.bind_var(X, term.functor(F, As, C), !VarSet)
    ).

unify(term.functor(F, As, C), term.variable(X, _), !VarSet) :-
    ( varset.search_var(!.VarSet, X, BindingOfX) ->
        unify(term.functor(F, As, C), BindingOfX, !VarSet)
    ;
        \+ occurs_list(As, X, !.VarSet),
        varset.bind_var(X, term.functor(F, As, C), !VarSet)
    ).

unify(term.functor(F, AsX, _), term.functor(F, AsY, _)) -->
    unify_list(AsX, AsY).

:- pred unify_list(list(term)::in, list(term)::in, varset::in, varset::out)
    is semidet.

unify_list([], [], !IO).
unify_list([X | Xs], [Y | Ys], !IO) :-
    unify(X, Y, !IO),
    unify_list(Xs, Ys, !IO).

%-----------------------------------------------------------------------------%

    % occurs(Term, Var, Subst) succeeds if Term contains Var, perhaps
    % indirectly via the substitution.
    % (The variable must not be mapped by the substitution.)
    % 
:- pred occurs(term::in, var::in, varset::in) is semidet.

occurs(term.variable(X, _), Y, VarSet) :-
    (
        X = Y
    ;
        varset.search_var(VarSet, X, BindingOfX),
        occurs(BindingOfX, Y, VarSet)
    ).
occurs(term.functor(_F, As, _), Y, VarSet) :-
    occurs_list(As, Y, VarSet).

:- pred occurs_list(list(term)::in, var::in, varset::in) is semidet.

occurs_list([Term | Terms], Y, VarSet) :-
    (
        occurs(Term, Y, VarSet)
    ;
        occurs_list(Terms, Y, VarSet)
    ).

%-----------------------------------------------------------------------------%

    % apply_rec_substitution(Term0, VarSet, Term):
    % Recursively apply substitution to Term0 until no more substitutions can be
    % applied, and then return the result in Term.
    %
:- pred apply_rec_substitution(term::in, varset::in, term::out) is det.

apply_rec_substitution(V @ term.variable(Var, _), VarSet, Term) :-
    ( varset.search_var(VarSet, Var, Replacement) ->
        % Recursively apply the substitution to the replacement.
        apply_rec_substitution(Replacement, VarSet, Term)
    ;
        Term = V
    ).
apply_rec_substitution(term.functor(Name, Args0, Context), VarSet,
         term.functor(Name, Args, Context)) :-
    apply_rec_substitution_to_list(Args0, VarSet, Args).

:- pred apply_rec_substitution_to_list(list(term)::in, varset::in, 
    list(term)::out) is det.

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

:- type clause
    --->    clause(
                clause_vars :: varset,
                clause_head :: term,
                clause_body :: term
            ).

:- pred database_init(database::out) is det.

database_init([]).

:- pred database_assert_clause(database::in, varset::in, term::in, 
    database::out) is det.

database_assert_clause(Database, VarSet, Term, [Clause | Database]) :-
    ( Term = term.functor(term.atom(":-"), [H, B], _) ->
        Head = H,
        Body = B
    ;
        Head = Term,
        term.context_init(Context),
        Body = term.functor(term.atom("true"), [], Context)
    ),
    Clause = clause(VarSet, Head, Body).

:- pred database_lookup_clause(database::in, term::in, varset::out, 
    term::out, term::out) is nondet.

database_lookup_clause(Database, _Goal, VarSet, Head, Body) :-
    list.member(Clause, Database),
    Clause = clause(VarSet, Head, Body).

%-----------------------------------------------------------------------------%
:- end_module interpreter.
%-----------------------------------------------------------------------------%
