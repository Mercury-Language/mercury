%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
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
% they are left as an exercise for the reader.
%
% For a more efficient version (using backtrackable destructive update),
% see extras/trailed_update/samples/interpreter.m.
%
% This source file is hereby placed in the public domain. -fjh (the author).
%
%---------------------------------------------------------------------------%
%
% The .exp file is for XXX.
% The .exp2 file is for asm_fast.gc bootchecks.
%
%---------------------------------------------------------------------------%

:- module interpreter.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module mercury_term_parser.
:- import_module require.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    % We *could* check that Args is non-empty, but the version of this test
    % in tests/general is invoked with Args = [], which works because
    % queries it is given do not refer to any predicates (they consist
    % solely of unifications).
    io.write_string("Pure Prolog Interpreter.\n\n", !IO),
    database_init(Database0),
    consult_files(Args, Database0, Database, !IO),
    main_loop(Database, !IO).

:- pred main_loop(database::in, io::di, io::uo) is det.

main_loop(Database, !IO) :-
    io.write_string("?- ", !IO),
    read_term(ReadTerm, !IO),
    (
        ReadTerm = eof
    ;
        ReadTerm = error(ErrorMessage, LineNumber),
        io.format("Error reading term at line %d of standard input: %s\n",
            [i(LineNumber), s(ErrorMessage)], !IO),
        main_loop(Database, !IO)
    ;
        ReadTerm = term(VarSet0, Goal),
        % Any special commands with side-effects (such as `consult'
        % and `listing') could be identified and processed here.
        solutions(solve(Database, Goal, VarSet0), Solutions),
        (
            Solutions = [],
            io.write_string("No.\n", !IO)
        ;
            Solutions = [_ | _],
            write_solutions(Solutions, Goal, !IO),
            io.write_string("Yes.\n", !IO)
        ),
        main_loop(Database, !IO)
    ).

:- pred write_solutions(list(varset)::in, term::in, io::di, io::uo) is det.

write_solutions([], _, !IO).
write_solutions([VarSet | VarSets], Goal, !IO) :-
    term_io.write_term_nl(VarSet, Goal, !IO),
    write_solutions(VarSets, Goal, !IO).

%---------------------------------------------------------------------------%

:- pred consult_files(list(string)::in, database::in, database::out,
    io::di, io::uo) is det.

consult_files([], !Database, !IO).
consult_files([File | Files], !Database, !IO) :-
    consult_file(File, !Database, !IO),
    consult_files(Files, !Database, !IO).

:- pred consult_file(string::in, database::in, database::out,
    io::di, io::uo) is det.

consult_file(File, Database0, Database, !IO) :-
    io.format("Consulting file `%s'...\n", [s(File)], !IO),
    io.open_input(File, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        consult_until_eof(Stream, Database0, Database, !IO),
        io.close_input(Stream, !IO)
    ;
        OpenResult = error(_),
        io.format("Error opening file `%s' for input.\n", [s(File)], !IO),
        Database = Database0
    ).

:- pred consult_until_eof(io.text_input_stream::in,
    database::in, database::out, io::di, io::uo) is det.

consult_until_eof(Stream, !Database, !IO) :-
    read_term(Stream, ReadTerm, !IO),
    (
        ReadTerm = eof
    ;
        ReadTerm = error(ErrorMessage, LineNumber),
        io.format("Error reading term at line %d of standard input: %s\n",
            [i(LineNumber), s(ErrorMessage)], !IO),
        consult_until_eof(Stream, !Database, !IO)
    ;
        ReadTerm = term(VarSet, Term),
        database_assert_clause(VarSet, Term, !Database),
        consult_until_eof(Stream, !Database, !IO)
    ).

%---------------------------------------------------------------------------%

% Solve takes a database of rules and facts, a goal to be solved,
% and a varset (which includes a supply of fresh vars, a substitution,
% and names for [some subset of] the variables). It updates the varset,
% producing a new substitution and perhaps introducing some new vars,
% and returns the result.

% Goals are stored just as terms.
% (It might be more efficient to parse them before storing them
% in the database. Currently we do this parsing work every time
% we interpret a clause.)

:- pred solve(database::in, term::in, varset::in, varset::out) is nondet.

solve(Database, Goal, !VarSet) :-
    (
        Goal = term.functor(term.atom("true"), [], _)
    ;
        Goal = term.functor(term.atom(", "), [A, B], _),
        solve(Database, A, !VarSet),
        solve(Database, B, !VarSet)
    ;
        Goal = term.functor(term.atom(";"), [A, B], _),
        ( solve(Database, A, !VarSet)
        ; solve(Database, B, !VarSet)
        )
    ;
        Goal = term.functor(term.atom("="), [A, B], _),
        unify(A, B, !VarSet)
    ;
        database_lookup_clause(Database, Goal, ClauseVarSet, Head0, Body0),
        rename_apart(ClauseVarSet, [Head0, Body0], [Head, Body], !VarSet),
        unify(Goal, Head, !VarSet),
        solve(Database, Body, !VarSet)
    ).

%---------------------------------------------------------------------------%

:- pred rename_apart(varset::in, list(term)::in, list(term)::out,
    varset::in, varset::out) is det.

rename_apart(NewVarSet, Terms0, Terms, VarSet0, VarSet) :-
    varset.merge(VarSet0, NewVarSet, Terms0, VarSet, Terms).

%---------------------------------------------------------------------------%

% The standard library module `term' contains routines for
% unifying terms based on separate substitutions, but we are
% using the substitutions that are contained in the `varset',
% so we can't use those versions.

:- pred unify(term::in, term::in, varset::in, varset::out) is semidet.

unify(TermX, TermY, !VarSet) :-
    (
        TermX = term.variable(X, _),
        TermY = term.variable(Y, _),
        ( if varset.search_var(!.VarSet, X, BindingOfX) then
            ( if varset.search_var(!.VarSet, Y, BindingOfY) then
                % Both X and Y already have bindings:
                % just unify the terms they are bound to.
                unify(BindingOfX, BindingOfY, !VarSet)
            else
                % Y is an unbound variable.
                apply_rec_substitution(!.VarSet,
                    BindingOfX, SubstBindingOfX),
                ( if SubstBindingOfX = term.variable(Y, _) then
                    true
                else
                    not occurs(SubstBindingOfX, Y, !.VarSet),
                    varset.bind_var(Y, SubstBindingOfX, !VarSet)
                )
            )
        else
            ( if varset.search_var(!.VarSet, Y, BindingOfY2) then
                % X is an unbound variable.
                apply_rec_substitution(!.VarSet,
                    BindingOfY2, SubstBindingOfY2),
                ( if SubstBindingOfY2 = term.variable(X, _) then
                    true
                else
                    not occurs(SubstBindingOfY2, X, !.VarSet),
                    varset.bind_var(X, SubstBindingOfY2, !VarSet)
                )
            else
                % Both X and Y are unbound variables: bind one to the other.
                ( if X = Y then
                    true
                else
                    varset.bind_var(X, term.variable(Y, context_init), !VarSet)
                )
            )
        )
    ;
        TermX = term.variable(X, _),
        TermY = term.functor(F, As, C),
        ( if varset.search_var(!.VarSet, X, BindingOfX) then
            unify(BindingOfX, term.functor(F, As, C), !VarSet)
        else
            not occurs_list(As, X, !.VarSet),
            varset.bind_var(X, term.functor(F, As, C), !VarSet)
        )
    ;
        TermX = term.functor(F, As, C),
        TermY = term.variable(X, _),
        ( if varset.search_var(!.VarSet, X, BindingOfX) then
            unify(term.functor(F, As, C), BindingOfX, !VarSet)
        else
            not occurs_list(As, X, !.VarSet),
            varset.bind_var(X, term.functor(F, As, C), !VarSet)
        )
    ;
        TermX = term.functor(F, AsX, _),
        TermY = term.functor(F, AsY, _),
        unify_list(AsX, AsY, !VarSet)
    ).

:- pred unify_list(list(term)::in, list(term)::in, varset::in, varset::out)
    is semidet.

unify_list([], [], !VarSet).
unify_list([X | Xs], [Y | Ys], !VarSet) :-
    unify(X, Y, !VarSet),
    unify_list(Xs, Ys, !VarSet).

%---------------------------------------------------------------------------%

    % occurs(Term, Var, VarSet) succeeds if Term contains Var,
    % perhaps indirectly via the substitution represented by VarSet.
    % (The variable must not be bound by VarSet.)
    %
:- pred occurs(term::in, var::in, varset::in) is semidet.

occurs(Term, Y, VarSet) :-
    (
        Term = term.variable(X, _),
        (
            X = Y
        ;
            varset.search_var(VarSet, X, BindingOfX),
            occurs(BindingOfX, Y, VarSet)
        )
    ;
        Term = term.functor(_F, As, _),
        occurs_list(As, Y, VarSet)
    ).

:- pred occurs_list(list(term)::in, var::in, varset::in) is semidet.

occurs_list([Term | Terms], Y, VarSet) :-
    ( occurs(Term, Y, VarSet)
    ; occurs_list(Terms, Y, VarSet)
    ).

%---------------------------------------------------------------------------%

    % apply_rec_substitution(VarSet, Term0, Term):
    %
    % Recursively apply substitution to Term0 until no more substitions
    % can be applied, and then return the result in Term.
    %
:- pred apply_rec_substitution(varset::in, term::in, term::out) is det.

apply_rec_substitution(VarSet, Term0, Term) :-
    (
        Term0 = term.variable(Var, _),
        ( if varset.search_var(VarSet, Var, Replacement) then
            % Recursively apply the substitution to the replacement.
            apply_rec_substitution(VarSet, Replacement, Term)
        else
            Term = term.variable(Var, context_init)
        )
    ;
        Term0 = term.functor(Name, ArgTerms0, Context),
        apply_rec_substitution_to_list(VarSet, ArgTerms0, ArgTerms),
        Term = term.functor(Name, ArgTerms, Context)
    ).

:- pred apply_rec_substitution_to_list(varset::in, list(term)::in,
    list(term)::out) is det.

apply_rec_substitution_to_list(_VarSet, [], []).
apply_rec_substitution_to_list(VarSet, [Term0 | Terms0], [Term | Terms]) :-
    apply_rec_substitution(VarSet, Term0, Term),
    apply_rec_substitution_to_list(VarSet, Terms0, Terms).

%---------------------------------------------------------------------------%

% We store the database just as a list of clauses.
% (It would be more realistic to index this on the predicate name/arity
% and subindex on the name/arity of the first argument.)

:- type database == list(clause).
:- type clause
    --->    clause(varset, term, term).

:- pred database_init(database::out) is det.

database_init([]).

:- pred database_assert_clause(varset::in, term::in,
    database::in, database::out) is det.

database_assert_clause(VarSet, Term, Database, [Clause | Database]) :-
    ( if Term = term.functor(term.atom(":-"), [H, B], _) then
        Head = H,
        Body = B
    else
        Head = Term,
        term.context_init(Context),
        Body = term.functor(term.atom("true"), [], Context)
    ),
    Clause = clause(VarSet, Head, Body).

:- pred database_lookup_clause(database::in, term::in,
    varset::out, term::out, term::out) is nondet.

database_lookup_clause(Database, _Goal, VarSet, Head, Body) :-
    list.member(Clause, Database),
    Clause = clause(VarSet, Head, Body).

%---------------------------------------------------------------------------%
