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
% meta-programming library modules term, term_io, and varset.
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
% This module is also used as a test case in two test directories, debugger
% and general.
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
    io.write_string("Pure Prolog Interpreter.\n\n", !IO),
    io.command_line_arguments(Args, !IO),
    database_init(Database0),
    (
        Args = [],
        io.write_string("No files consulted.\n", !IO),
        Database = Database0
    ;
        Args = [_ | _],
        consult_files(Args, Database0, Database, !IO)
    ),
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
    % and returns the varset thus updated as the result.
    %
    % We represent goals simply as terms. We parse (i.e. we discover
    % the structure of) the body of each clause every time we interpret
    % that clause. Definite logic programs do not allow disjunctions
    % in the bodies of clauses, but we do, so for us, each clause is
    % a boolean expression built up (using the conjunction operator ","
    % and/or the disjunction operator ";") from three kinds of primitives:
    % `true', unifications, and calls to user-defined predicates.
    %
    % Parsing each clause just once, before we put it into the database,
    % would be more efficient.
    %
    % Not looking up the database of user-defined predicates on goals
    % whose top-level functor is ,/2, ;/2, true/0 or =/2 would also be
    % more efficient, as well as semantically cleaner.
    %
:- pred solve(database::in, term::in, varset::in, varset::out) is nondet.

solve(Database, Goal, !VarSet) :-
    (
        Goal = term.functor(term.atom(", "), [SubGoalA, SubGoalB], _),
        solve(Database, SubGoalA, !VarSet),
        solve(Database, SubGoalB, !VarSet)
    ;
        Goal = term.functor(term.atom(";"), [SubGoalA, SubGoalB], _),
        ( solve(Database, SubGoalA, !VarSet)
        ; solve(Database, SubGoalB, !VarSet)
        )
    ;
        Goal = term.functor(term.atom("true"), [], _)
    ;
        Goal = term.functor(term.atom("="), [TermA, TermB], _),
        unify_term_pair(TermA, TermB, !VarSet)
    ;
        database_lookup_clause(Database, Goal, ClauseVarSet, Head0, Body0),
        rename_apart(ClauseVarSet, [Head0, Body0], [Head, Body], !VarSet),
        unify_term_pair(Goal, Head, !VarSet),
        solve(Database, Body, !VarSet)
    ).

%---------------------------------------------------------------------------%

:- pred rename_apart(varset::in, list(term)::in, list(term)::out,
    varset::in, varset::out) is det.

rename_apart(NewVarSet, Terms0, Terms, VarSet0, VarSet) :-
    varset.merge(VarSet0, NewVarSet, Terms0, VarSet, Terms).

%---------------------------------------------------------------------------%

    % unify_term_pair(TermX, TermY, !VarSet):
    %
    % Unify TermX with TermY, updating the varset if the unification succeeds.
    %
    % The standard library module `term' contains routines for unifying terms
    % based on separate substitutions (maps from variables to terms),
    % but here we are using substitutions that are contained in the varset
    % itself, so we cannot use those versions.
    %
:- pred unify_term_pair(term::in, term::in, varset::in, varset::out)
    is semidet.

unify_term_pair(TermX, TermY, !VarSet) :-
    (
        TermX = term.variable(VarX, _ContextX),
        TermY = term.variable(VarY, _ContextY),
        ( if varset.search_var(!.VarSet, VarX, BindingOfX) then
            ( if varset.search_var(!.VarSet, VarY, BindingOfY) then
                % Both X and Y already have bindings;
                % unify the terms they are bound to.
                unify_term_pair(BindingOfX, BindingOfY, !VarSet)
            else
                % X is bound, Y is not. Symmetrical with the opposite case.
                apply_rec_substitution(!.VarSet, BindingOfX, SubstBindingOfX),
                ( if SubstBindingOfX = term.variable(VarY, _) then
                    true
                else
                    not var_occurs_in_term(VarY, SubstBindingOfX, !.VarSet),
                    varset.bind_var(VarY, SubstBindingOfX, !VarSet)
                )
            )
        else
            ( if varset.search_var(!.VarSet, VarY, BindingOfY) then
                % Y is bound, X is not. Symmetrical with the opposite case.
                apply_rec_substitution(!.VarSet, BindingOfY, SubstBindingOfY),
                ( if SubstBindingOfY = term.variable(VarX, _) then
                    true
                else
                    not var_occurs_in_term(VarX, SubstBindingOfY, !.VarSet),
                    varset.bind_var(VarX, SubstBindingOfY, !VarSet)
                )
            else
                % Both X and Y are unbound variables; bind one to the other.
                % It does not matter whether we bind X to Y, or Y to X.
                ( if VarX = VarY then
                    true
                else
                    TermY = term.variable(VarY, term.context_init),
                    varset.bind_var(VarX, TermY, !VarSet)
                )
            )
        )
    ;
        TermX = term.variable(VarX, _ContextX),
        TermY = term.functor(_FunctorY, _ArgTermsY, _ContextY),
        unify_var_functor(VarX, TermY, !VarSet)
    ;
        TermX = term.functor(_FunctorX, _ArgTermsX, _ContextX),
        TermY = term.variable(VarY, _ContextY),
        unify_var_functor(VarY, TermX, !VarSet)
    ;
        TermX = term.functor(FunctorX, ArgTermsX, _ContextX),
        TermY = term.functor(FunctorY, ArgTermsY, _ContextY),
        FunctorX = FunctorY,
        unify_term_pairs(ArgTermsX, ArgTermsY, !VarSet)
    ).

:- inst term_functor for term/1
    --->    functor(ground, ground, ground).

    % Unify a variable with a term that is known to be a functor
    % applied to some argument terms.
    %
:- pred unify_var_functor(term.var::in, term::in(term_functor),
    varset::in, varset::out) is semidet.

unify_var_functor(VarX, TermY, !VarSet) :-
    ( if varset.search_var(!.VarSet, VarX, BindingOfX) then
        unify_term_pair(BindingOfX, TermY, !VarSet)
    else
        TermY = term.functor(_FunctorY, ArgTermsY, _ContextY),
        not var_occurs_in_terms(VarX, ArgTermsY, !.VarSet),
        varset.bind_var(VarX, TermY, !VarSet)
    ).

:- pred unify_term_pairs(list(term)::in, list(term)::in,
    varset::in, varset::out) is semidet.

unify_term_pairs([], [], !VarSet).
unify_term_pairs([TermX | TermXs], [TermY | TermYs], !VarSet) :-
    unify_term_pair(TermX, TermY, !VarSet),
    unify_term_pairs(TermXs, TermYs, !VarSet).

%---------------------------------------------------------------------------%

    % var_occurs_in_term(VarX, TermY, VarSet):
    %
    % Succeed iff VarX occurs in TermY, either as is,
    % or after the substitution in VarSet is applied to TermY.
    %
    % VarX must not be mapped by the substitution in VarSet.
    %
:- pred var_occurs_in_term(var::in, term::in, varset::in) is semidet.

var_occurs_in_term(VarX, TermY, VarSet) :-
    (
        TermY = term.variable(VarY, _),
        (
            VarX = VarY
        ;
            varset.search_var(VarSet, VarY, BindingOfY),
            var_occurs_in_term(VarX, BindingOfY, VarSet)
        )
    ;
        TermY = term.functor(_FunctorY, ArgTermsY, _ContextY),
        var_occurs_in_terms(VarX, ArgTermsY, VarSet)
    ).

    % var_occurs_in_terms(VarX, TermsY, VarSet):
    %
    % Succeed iff VarX occurs in any of the TermsY, either as is,
    % or after the substitution in VarSet is applied to TermsY.
    %
    % VarX must not be mapped by the substitution in VarSet.
    %
:- pred var_occurs_in_terms(var::in, list(term)::in, varset::in) is semidet.

var_occurs_in_terms(VarX, [TermY | TermsY], VarSet) :-
    (
        var_occurs_in_term(VarX, TermY, VarSet)
    ;
        var_occurs_in_terms(VarX, TermsY, VarSet)
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
%
% This makes the code simple and readable, but it severely limits its
% performance on anything bigger than toy programs.
%
% It would be more realistic to index the database on the predicate name/arity,
% and subindex on the name/arity of the first argument.

:- type database == list(clause).
:- type clause
    --->    clause(
                clause_vars :: varset,
                clause_head :: term,
                clause_body :: term
            ).

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

    % database_lookup_clause(Database, Goal, VarSet, Head, Body):
    %
    % For each clause in Database whose head may unify with Goal,
    % return the representation of that clause: its varset, head and body.
    %
    % Since our database has no indexing, we ignore the goal, and return
    % *all* the clauses.
    %
:- pred database_lookup_clause(database::in, term::in,
    varset::out, term::out, term::out) is nondet.

database_lookup_clause(Database, _Goal, VarSet, Head, Body) :-
    list.member(Clause, Database),
    Clause = clause(VarSet, Head, Body).

%---------------------------------------------------------------------------%
