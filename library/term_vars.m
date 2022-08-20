%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2003-2009,2011-2012 The University of Melbourne.
% Copyright (C) 2014-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: term_vars.m.
%
% This file provides ways to find out what variables occur in terms.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module term_vars.
:- interface.

:- import_module list.
:- import_module term.

%---------------------------------------------------------------------------%
%
% Predicates that list the variables in one or more terms.
%

    % vars_in_term(Term, Vars):
    %
    % Vars is the list of variables contained in Term, in the order
    % obtained by traversing the term depth first, left-to-right.
    %
:- func vars_in_term(term(T)) = list(var(T)).
:- pred vars_in_term(term(T)::in, list(var(T))::out) is det.

    % As above, but with an accumulator: add the variables in the term
    % to the front of the initial value of the accumulator.
    %
:- pred vars_in_term_acc(term(T)::in, list(var(T))::in, list(var(T))::out)
    is det.

    % vars_in_terms(Terms, Vars):
    %
    % Vars is the list of variables contained in Terms, in the order
    % obtained by traversing the list of terms depth-first, left-to-right.
    %
:- func vars_in_terms(list(term(T))) = list(var(T)).
:- pred vars_in_terms(list(term(T))::in, list(var(T))::out) is det.

    % term_contains_var(Term, Var):
    %
    % True if Term contains Var. The second mode returns all the variables
    % in Term, one at a time.
    %
:- pred term_contains_var(term(T), var(T)).
:- mode term_contains_var(in, in) is semidet.
:- mode term_contains_var(in, out) is nondet.

    % terms_contain_var(Terms, Var):
    %
    % True if Terms contains Var. The second mode returns all the variables
    % in Terms, one at a time.
    %
:- pred terms_contain_var(list(term(T)), var(T)).
:- mode terms_contain_var(in, in) is semidet.
:- mode terms_contain_var(in, out) is nondet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

vars_in_term(Term) = Vars :-
    vars_in_term(Term, Vars).

vars_in_term(Term, Vars) :-
    vars_in_term_acc(Term, [], Vars).

vars_in_term_acc(Term, !Vars) :-
    (
        Term = variable(Var, _),
        !:Vars = [Var | !.Vars]
    ;
        Term = functor(_, ArgTerms, _),
        vars_in_terms_acc(ArgTerms, !Vars)
    ).

vars_in_terms(Terms) = Vars :-
    vars_in_terms(Terms, Vars).

vars_in_terms(Terms, Vars) :-
    vars_in_terms_acc(Terms, [], Vars).

:- pred vars_in_terms_acc(list(term(T))::in,
    list(var(T))::in, list(var(T))::out) is det.

vars_in_terms_acc([], !Vars).
vars_in_terms_acc([Term | Terms], !Vars) :-
    vars_in_terms_acc(Terms, !Vars),
    vars_in_term_acc(Term, !Vars).

%---------------------------------------------------------------------------%

term_contains_var(variable(Var, _), Var).
term_contains_var(functor(_, ArgTerms, _), Var) :-
    terms_contain_var(ArgTerms, Var).

terms_contain_var([Term | _], Var) :-
    term_contains_var(Term, Var).
terms_contain_var([_ | Terms], Var) :-
    terms_contain_var(Terms, Var).

%---------------------------------------------------------------------------%
:- end_module term_vars.
%---------------------------------------------------------------------------%
