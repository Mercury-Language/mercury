%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003, 2006, 2011 The University of Melbourne.
% Copyright (C) 2014, 2020, 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%---------------------------------------------------------------------------%

:- module mercury_syntax.
:- interface.

:- import_module io.
:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type (module) == list(element).

    % Element is a type for pieces of a mercury module.
:- type element
    --->    pred(term, varset)  % Pred declarations
    ;       func(term, varset)  % Func declarations
    ;       type(type, varset)  % Type declarations
    ;       mode(term, varset)  % Mode declarations
                                % (both predicate modes and new modes)
    ;       inst(term, varset)  % Inst declarations
    ;       clause(term, goal, varset)      % Program clauses
    ;       dcg_clause(term, goal, varset)  % DCG clauses
    ;       class(term, varset) % Class declarations
    ;       instance(term, varset)  % Instance declarations
    ;       misc(term, varset). % Anything else

:- type module_result
    --->    module(module, list(module_error)).

:- type module_error
    --->    error(string, int).

:- pred read_module(module_result::out, io::di, io::uo) is det.

:- type lines
    --->    lines
    ;       nolines.

:- pred write_element(lines::in, element::in, io::di, io::uo) is det.

:- pred write_module(lines::in, (module)::in, io::di, io::uo) is det.

:- type (type)
    --->    abstr(term)
    ;       equiv(term, term)
    ;       disj(term, list(term)).

:- pred term_to_type(term::in, (type)::out) is semidet.

:- type goal
    --->    conj(list(goal))
    ;       disj(list(goal))
    ;       ite(goal, goal, goal)
    ;       call(term)
    ;       (=(term, term, context))
    ;       not(goal)
    ;       exists(vars, goal)
    ;       forall(vars, goal)
    %       (goal => goal) % XXX conflicts with type classes
    ;       (goal <= goal)
    ;       (goal <=> goal).

:- pred term_to_goal(term::in, goal::out) is semidet.

:- pred write_goal(varset::in, goal::in, io::di, io::uo) is det.

:- type vars == list(var).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module mercury_term_parser.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term_io.

%---------------------------------------------------------------------------%

read_module(Result, !IO) :-
    read_module([], [], Result0, !IO),
    Result0 = module(Module0, Errors0),
    list.reverse(Module0, Module),
    list.reverse(Errors0, Errors),
    Result = module(Module, Errors).

:- type element_result
    --->    element(element)
    ;       eof
    ;       error(string, int).

:- pred read_module((module)::in, list(module_error)::in, module_result::out,
    io::di, io::uo) is det.

read_module(Module, Errors, Result, !IO) :-
    read_element(Result0, !IO),
    (
        Result0 = eof,
        Result = module(Module, Errors)
    ;
        Result0 = element(Element),
        read_module([Element | Module], Errors, Result, !IO)
    ;
        Result0 = error(Msg, Line),
        read_module(Module, [error(Msg, Line) | Errors], Result, !IO)
    ).

:- pred read_element(element_result::out, io::di, io::uo) is det.

read_element(Result, !IO) :-
    read_term(Result0, !IO),
    (
        Result0 = eof,
        Result = eof
    ;
        Result0 = error(Msg, Line),
        Result = error(Msg, Line)
    ;
        Result0 = term(VarSet, Term),
        ( if classify(Term, VarSet, Element0) then
            Element = Element0
        else
            Element = misc(Term, VarSet)
        ),
        Result = element(Element)
    ).

:- pred classify(term::in, varset::in, element::out) is semidet.

classify(Term, VarSet, Element) :-
    Term = functor(atom(Atom), Args, _),
    ( if Atom = ":-" then
        (
            Args = [functor(atom("pred"), [PredDecl], _)],
            Element = pred(PredDecl, VarSet)
        ;
            Args = [functor(atom("func"), [FuncDecl], _)],
            Element = func(FuncDecl, VarSet)
        ;
            Args = [functor(atom("mode"), [ModeDecl], _)],
            Element = mode(ModeDecl, VarSet)
        ;
            Args = [functor(atom("type"), [TypeTerm], _)],
            ( if mercury_syntax.term_to_type(TypeTerm, TypeDecl) then
                Element = type(TypeDecl, VarSet)
            else
                Element = misc(Term, VarSet)
            )
        ;
            Args = [functor(atom("inst"), [InstDecl], _)],
            Element = inst(InstDecl, VarSet)
        ;
            Args = [functor(atom("class"), [ClassDecl], _)],
            Element = class(ClassDecl, VarSet)
        ;
            Args = [functor(atom("instance"), [InstanceDecl], _)],
            Element = instance(InstanceDecl, VarSet)
        ;
            Args = [Head, Body],
            ( if term_to_goal(Body, Goal) then
                Element = clause(Head, Goal, VarSet)
            else
                Element = misc(Term, VarSet)
            )
        )
    else if Atom = "-->" then
        Args = [Head, Body],
        ( if term_to_goal(Body, Goal) then
            Element = dcg_clause(Head, Goal, VarSet)
        else
            Element = misc(Term, VarSet)
        )
    else
        Element = misc(Term, VarSet)
    ).

%---------------------------------------------------------------------------%

write_module(_Lines, [], !IO).
write_module(Lines, [Element | Module], !IO) :-
    write_element(Lines, Element, !IO),
    io.nl(!IO),
    write_module(Lines, Module, !IO).

write_element(Lines, pred(PredDecl, VarSet), !IO) :-
    cons_decl("pred", PredDecl, Term),
    write_term(Lines, 0, VarSet, Term, !IO),
    dot_nl(!IO).

write_element(Lines, func(FuncDecl, VarSet), !IO) :-
    cons_decl("func", FuncDecl, Term),
    write_term(Lines, 0, VarSet, Term, !IO),
    dot_nl(!IO).

write_element(Lines, type(TypeDecl, VarSet), !IO) :-
    (
        TypeDecl = abstr(AbstrTerm),
        cons_decl("type", AbstrTerm, Term),
        write_term(Lines, 0, VarSet, Term, !IO)
    ;
        TypeDecl = equiv(Head, Body),
        get_context(Head, Context),
        EqivTerm = functor(atom("=="), [Head, Body], Context),
        cons_decl("type", EqivTerm, Term),
        write_term(Lines, 0, VarSet, Term, !IO)
    ;
        TypeDecl = disj(Head, Body),
        get_context(Head, Context),
        cons_type_body(Body, BodyTerm),
        DeclTerm = functor(atom("--->"), [Head, BodyTerm], Context),
        cons_decl("type", DeclTerm, Term),
        write_term(Lines, 0, VarSet, Term, !IO)
    ),
    dot_nl(!IO).

write_element(Lines, mode(ModeDecl, VarSet), !IO) :-
    cons_decl("mode", ModeDecl, Term),
    write_term(Lines, 0, VarSet, Term, !IO),
    dot_nl(!IO).

write_element(Lines, inst(InstDecl, VarSet), !IO) :-
    cons_decl("inst", InstDecl, Term),
    write_term(Lines, 0, VarSet, Term, !IO),
    dot_nl(!IO).

write_element(Lines, class(ClassDecl, VarSet), !IO) :-
    cons_decl("class", ClassDecl, Term),
    write_term(Lines, 0, VarSet, Term, !IO),
    dot_nl(!IO).

write_element(Lines, instance(InstanceDecl, VarSet), !IO) :-
    cons_decl("instance", InstanceDecl, Term),
    write_term(Lines, 0, VarSet, Term, !IO),
    dot_nl(!IO).

write_element(Lines, misc(Term, VarSet), !IO) :-
    write_term(Lines, 0, VarSet, Term, !IO),
    dot_nl(!IO).

write_element(Lines, clause(Head, Goal, VarSet), !IO) :-
    write_term(Lines, 0, VarSet, Head, !IO),
    io.write_string(" :-\n", !IO),
    write_goal(Lines, 1, normal, Goal, VarSet, !IO),
    dot_nl(!IO).

write_element(Lines, dcg_clause(Head, Goal, VarSet), !IO) :-
    write_term(Lines, 0, VarSet, Head, !IO),
    io.write_string(" -->\n", !IO),
    write_goal(Lines, 1, dcg, Goal, VarSet, !IO),
    dot_nl(!IO).

%---------------------------------------------------------------------------%

:- type goal_type
    --->    normal
    ;       dcg.

:- pred term_to_disj(term::in, list(term)::out) is semidet.

term_to_disj(functor(atom(";"), [Head, Term], _), [Head | Tail]) :-
    ( if term_to_disj(Term, Tail0) then
        Tail = Tail0
    else
        Tail = [Term]
    ).

%---------------------------------------------------------------------------%

:- pred cons_decl(string::in, term::in, term::out) is det.

cons_decl(Atom, DeclTerm, Term) :-
    get_context(DeclTerm, Context),
    Term = functor(atom(":-"),
        [functor(atom(Atom), [DeclTerm], Context)],
        Context).

:- pred get_context(term::in, context::out) is det.

get_context(variable(_, Context), Context).
get_context(functor(_, _, Context), Context).

%---------------------------------------------------------------------------%

:- pred write_ind(int::in, io::di, io::uo) is det.

write_ind(N, !IO) :-
    ( if N > 0 then
        io.write_string("    ", !IO),
        write_ind(N - 1, !IO)
    else
        true
    ).

:- pred dot_nl(io::di, io::uo) is det.

dot_nl(!IO) :-
    io.write_string(".\n", !IO).

:- pred write_term(lines::in, int::in, varset::in, term::in,
    io::di, io::uo) is det.

write_term(lines, Ind, VarSet, Term, !IO) :-
    get_context(Term, context(File, Line)),
    ( if File = "", Line = 0 then
        true
    else
        io.format("#%d\n", [i(Line)], !IO)
    ),
    write_ind(Ind, !IO),
    write_term(VarSet, Term, !IO).
write_term(nolines, Ind, VarSet, Term, !IO) :-
    write_ind(Ind, !IO),
    write_term(VarSet, Term, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

term_to_type(functor(atom(Atom), Args, Context), Type) :-
    ( if
        Atom = "==",
        Args = [Head0, Body0]
    then
        Type = equiv(Head0, Body0)
    else if
        Atom = "--->",
        Args = [Head1, Body1]
    then
        ( if term_to_disj(Body1, Terms0) then
            Terms = Terms0
        else
            Terms = [Body1]
        ),
        Type = disj(Head1, Terms)
    else
        Type = abstr(functor(atom(Atom), Args, Context))
    ).

:- pred cons_type_body(list(term)::in, term::out) is det.

cons_type_body([], _) :-
    error("cons_type_body: no disjuncts").
cons_type_body([E], E).
cons_type_body([E | Es], T) :-
    Es = [_ | _],
    cons_type_body(Es, T0),
    get_context(E, Context),
    T = functor(atom(";"), [E, T0], Context).

%---------------------------------------------------------------------------%

term_to_goal(functor(atom(Atom), Args, Context), Goal) :-
    ( if term_to_goal0(Atom, Args, Context, Goal0) then
        Goal = Goal0
    else
        Goal = call(functor(atom(Atom), Args, Context))
    ).

:- pred term_to_goal0(string::in, list(term)::in, context::in, goal::out)
    is semidet.

term_to_goal0("true", [], _, conj([])).
term_to_goal0("fail", [], _, disj([])).
term_to_goal0(",", [A, B], _, conj([GoalA | Conj])) :-
    term_to_goal(A, GoalA),
    term_to_goal(B, GoalB),
    ( if GoalB = conj(Conj0) then
        Conj = Conj0
    else
        Conj = [GoalB]
    ).
term_to_goal0(";", [A, B], _, Goal) :-
    ( if A = functor(atom("->"), [IfTerm, ThenTerm], _) then
        term_to_goal(IfTerm, If),
        term_to_goal(ThenTerm, Then),
        term_to_goal(B, Else),
        Goal = ite(If, Then, Else)
    else
        term_to_goal(A, GoalA),
        term_to_goal(B, GoalB),
        ( if GoalB = disj(Disj0) then
            Goal = disj([GoalA | Disj0])
        else
            Goal = disj([GoalA, GoalB])
        )
    ).
term_to_goal0("else", [functor(atom("if"), [IfThenTerm],  _), ElseTerm], _,
        Goal) :-
    IfThenTerm = functor(atom("then"), [IfTerm, ThenTerm], _),
    term_to_goal(IfTerm, If),
    term_to_goal(ThenTerm, Then),
    term_to_goal(ElseTerm, Else),
    Goal = ite(If, Then, Else).
term_to_goal0("=", [A, B], Context, =(A, B, Context)).
term_to_goal0("not", [A], _, not(Goal)) :-
    term_to_goal(A, Goal).
term_to_goal0("\\+", [A], _, not(Goal)) :-
    term_to_goal(A, Goal).
term_to_goal0("some", [VarsTerm, GoalTerm], _, exists(Vars, Goal)) :-
    vars(VarsTerm, Vars0),
    sort_and_remove_dups(Vars0, Vars),
    term_to_goal(GoalTerm, Goal).
term_to_goal0("all", [VarsTerm, GoalTerm], _, forall(Vars, Goal)) :-
    vars(VarsTerm, Vars0),
    sort_and_remove_dups(Vars0, Vars),
    term_to_goal(GoalTerm, Goal).
% term_to_goal0("=>", [A, B], _, (GoalA => GoalB)) :-
%     term_to_goal(A, GoalA),
%     term_to_goal(B, GoalB).
term_to_goal0("<=", [A, B], _, (GoalA <= GoalB)) :-
    term_to_goal(A, GoalA),
    term_to_goal(B, GoalB).
term_to_goal0("<=>", [A, B], _, (GoalA <=> GoalB)) :-
    term_to_goal(A, GoalA),
    term_to_goal(B, GoalB).

%---------------------------------------------------------------------------%

write_goal(VarSet, Goal, !IO) :-
    write_goal(nolines, 1, normal, Goal, VarSet, !IO).

:- pred write_goal(lines::in, int::in, goal_type::in, goal::in, varset::in,
    io::di, io::uo) is det.

write_goal(Lines, Ind, _GoalType, call(Term), VarSet, !IO) :-
    write_term(Lines, Ind, VarSet, Term, !IO).
write_goal(Lines, Ind, GoalType, =(LHS, RHS, Context), VarSet, !IO) :-
    UnifyTerm = functor(atom("="), [LHS, RHS], Context),
    (
        GoalType = dcg,
        Term = functor(atom("{}"), [UnifyTerm], Context)
    ;
        GoalType = normal,
        Term = UnifyTerm
    ),
    write_term(Lines, Ind, VarSet, Term, !IO).
write_goal(Lines, Ind, GoalType, conj(Goals), VarSet, !IO) :-
    write_conj(Lines, Ind, GoalType, Goals, VarSet, !IO).
write_goal(Lines, Ind, GoalType, disj(Goals), VarSet, !IO) :-
    write_disj(Lines, Ind, GoalType, Goals, VarSet, !IO).
write_goal(Lines, Ind, GoalType, ite(If, Then, Else0), VarSet, !IO) :-
    collect_ite(Else0, IfThens0, Else),
    write_ite(Lines, Ind, GoalType, [If - Then | IfThens0], Else, VarSet, !IO).
write_goal(Lines, Ind, GoalType, not(Goal), VarSet, !IO) :-
    write_ind(Ind, !IO),
    io.write_string("not (\n", !IO),
    write_goal(Lines, Ind + 1, GoalType, Goal, VarSet, !IO),
    io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string(")", !IO).
write_goal(Lines, Ind, GoalType, exists(Vars, Goal), VarSet, !IO) :-
    write_ind(Ind, !IO),
    io.write_string("some [", !IO),
    write_vars(Vars, VarSet, !IO),
    io.write_string("] (\n", !IO),
    write_goal(Lines, Ind + 1, GoalType, Goal, VarSet, !IO),
    io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string(")", !IO).
write_goal(Lines, Ind, GoalType, forall(Vars, Goal), VarSet, !IO) :-
    write_ind(Ind, !IO),
    io.write_string("all [", !IO),
    write_vars(Vars, VarSet, !IO),
    io.write_string("] (\n", !IO),
    write_goal(Lines, Ind + 1, GoalType, Goal, VarSet, !IO),
    io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string(")", !IO).
% write_goal(Lines, Ind, GoalType, (A => B), VarSet, !IO) :-
%     write_ind(Ind, !IO),
%     io.write_string("((\n", !IO),
%     write_goal(Lines, Ind, GoalType, A, VarSet, !IO),
%     io.nl(!IO),
%     write_ind(Ind, !IO),
%     io.write_string(") => (\n", !IO),
%     write_goal(Lines, Ind, GoalType, A, VarSet, !IO),
%     io.nl(!IO),
%     write_ind(Ind, !IO),
%     io.write_string("))", !IO).
write_goal(Lines, Ind, GoalType, (A <= B), VarSet, !IO) :-
    write_ind(Ind, !IO),
    io.write_string("((\n", !IO),
    write_goal(Lines, Ind, GoalType, A, VarSet, !IO),
    io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string(") <= (\n", !IO),
    write_goal(Lines, Ind, GoalType, B, VarSet, !IO),
    io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string("))", !IO).
write_goal(Lines, Ind, GoalType, (A <=> B), VarSet, !IO) :-
    write_ind(Ind, !IO),
    io.write_string("((\n", !IO),
    write_goal(Lines, Ind, GoalType, A, VarSet, !IO),
    io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string(") <=> (\n", !IO),
    write_goal(Lines, Ind, GoalType, B, VarSet, !IO),
    io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string("))", !IO).

%---------------------------------------------------------------------------%

:- pred write_conj(lines::in, int::in, goal_type::in, list(goal)::in,
    varset::in, io::di, io::uo) is det.

write_conj(_Lines, Ind, Type, [], _VarSet, !IO) :-
    write_ind(Ind, !IO),
    (
        Type = normal,
        io.write_string("true", !IO)
    ;
        Type = dcg,
        io.write_string("{ true }", !IO)
    ).
write_conj(Lines, Ind, Type, [Goal], VarSet, !IO) :-
    write_goal(Lines, Ind, Type, Goal, VarSet, !IO).
write_conj(Lines, Ind, Type, [Goal | Goals], VarSet, !IO) :-
    Goals = [_ | _],
    write_goal(Lines, Ind, Type, Goal, VarSet, !IO),
    io.write_string(",\n", !IO),
    write_conj(Lines, Ind, Type, Goals, VarSet, !IO).

%---------------------------------------------------------------------------%

:- pred write_disj(lines::in, int::in, goal_type::in, list(goal)::in,
    varset::in, io::di, io::uo) is det.

write_disj(Lines, Ind, Type, Goals, VarSet, !IO) :-
    write_ind(Ind, !IO),
    io.write_string("(\n", !IO),
    write_disj0(Lines, Ind, Type, Goals, VarSet, !IO), io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string(")", !IO).

:- pred write_disj0(lines::in, int::in, goal_type::in, list(goal)::in,
    varset::in, io::di, io::uo) is det.

write_disj0(_Lines, Ind, Type, [], _VarSet, !IO) :-
    write_ind(Ind + 1, !IO),
    (
        Type = normal,
        io.write_string("fail", !IO)
    ;
        Type = dcg,
        io.write_string("{ fail }", !IO)
    ).
write_disj0(Lines, Ind, Type, [Goal], VarSet, !IO) :-
    write_goal(Lines, Ind + 1, Type, Goal, VarSet, !IO), io.nl(!IO).
write_disj0(Lines, Ind, Type, [Goal | Goals], VarSet, !IO) :-
    Goals = [_ | _],
    write_goal(Lines, Ind + 1, Type, Goal, VarSet, !IO), io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string(";\n", !IO),
    write_disj0(Lines, Ind, Type, Goals, VarSet, !IO).

%---------------------------------------------------------------------------%

:- pred collect_ite(goal::in, list(pair(goal))::out, goal::out) is det.

collect_ite(Goal0, IfThens, Else) :-
    ( if Goal0 = ite(If, Then, Else0) then
        collect_ite(Else0, IfThens0, Else),
        IfThens = [If - Then | IfThens0]
    else
        IfThens = [],
        Else = Goal0
    ).

:- pred write_ite(lines::in, int::in, goal_type::in, list(pair(goal))::in,
    goal::in, varset::in, io::di, io::uo) is det.

write_ite(Lines, Ind, Type, IfThens, Else, VarSet, !IO) :-
    write_ind(Ind, !IO),
    io.write_string("(\n", !IO),
    write_ite0(Lines, Ind, Type, IfThens, VarSet, !IO),
    write_ind(Ind, !IO),
    io.write_string(";\n", !IO),
    write_goal(Lines, Ind + 1, Type, Else, VarSet, !IO),
    io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string(")", !IO).

:- pred write_ite0(lines::in, int::in, goal_type::in, list(pair(goal))::in,
    varset::in, io::di, io::uo) is det.

write_ite0(_Lines, _Ind, _Type, [], _VarSet, !IO) :-
    error("no if-thens").
write_ite0(Lines, Ind, Type, [If - Then], VarSet, !IO) :-
    write_goal(Lines, Ind + 1, Type, If, VarSet, !IO),
    io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string("->\n", !IO),
    write_goal(Lines, Ind + 1, Type, Then, VarSet, !IO),
    io.nl(!IO).
write_ite0(Lines, Ind, Type, [If - Then | Rest], VarSet, !IO) :-
    Rest = [_ | _],
    write_goal(Lines, Ind + 1, Type, If, VarSet, !IO),
    io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string("->\n", !IO),
    write_goal(Lines, Ind + 1, Type, Then, VarSet, !IO),
    io.nl(!IO),
    write_ind(Ind, !IO),
    io.write_string(";\n", !IO),
    write_ite0(Lines, Ind, Type, Rest, VarSet, !IO).

%---------------------------------------------------------------------------%

:- pred write_vars(vars::in, varset::in, io::di, io::uo) is det.

write_vars([], _, !IO).
write_vars([V], VarSet, !IO) :-
    term_io.write_variable(V, VarSet, !IO).
write_vars([V | Vs], VarSet, !IO) :-
    Vs = [_ | _],
    term_io.write_variable(V, VarSet, !IO),
    io.write_string(", ", !IO),
    write_vars(Vs, VarSet, !IO).
