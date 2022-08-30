%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2003-2009,2011-2012 The University of Melbourne.
% Copyright (C) 2014-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: term_unify.m.
% Main author: fjh.
% Stability: medium.
%
% This file provides predicates to unify terms.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module term_unify.
:- interface.

:- import_module list.
:- import_module term.

%---------------------------------------------------------------------------%
%
% Predicates to unify terms.
%

    % unify_terms(TermA, TermB, !Subst):
    %
    % Unify (with occur check) two terms with respect to the current
    % substitution, and update that substitution as necessary.
    %
:- pred unify_terms(term(T)::in, term(T)::in,
    substitution(T)::in, substitution(T)::out) is semidet.

    % unify_term_lists(TermsA, TermsB, !Subst):
    %
    % Unify (with occur check) two lists of terms with respect to the current
    % substitution, and update that substitution as necessary.
    % Fail if the lists are not of equal length.
    %
:- pred unify_term_lists(list(term(T))::in, list(term(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

    % unify_terms_dont_bind(TermA, TermB, DontBindVars, !Subst):
    %
    % Do the same job as unify_term(TermA, TermB, !Subst), but fail
    % if any of the variables in DontBindVars would become bound
    % by the unification.
    %
:- pred unify_terms_dont_bind(term(T)::in, term(T)::in,
    list(var(T))::in, substitution(T)::in, substitution(T)::out) is semidet.

    % unify_term_lists_dont_bind(TermsA, TermsB, DontBindVars, !Subst):
    %
    % Do the same job as unify_term_lists(TermsA, TermsB, !Subst), but fail
    % if any of the variables in DontBindVars would become bound
    % by the unification.
    %
:- pred unify_term_lists_dont_bind(list(term(T))::in, list(term(T))::in,
    list(var(T))::in, substitution(T)::in, substitution(T)::out) is semidet.

%---------------------------------------------------------------------------%
%
% Predicates to test subsumption.
%

    % first_term_list_subsumes_second(TermsA, TermsB, Subst):
    %
    % Succeeds iff the list TermsA subsumes (is more general than) TermsB,
    % producing a substitution which, when applied to TermsA, will give TermsB.
    %
:- pred first_term_list_subsumes_second(list(term(T))::in, list(term(T))::in,
    substitution(T)::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module term_context.
:- import_module term_subst.
:- import_module term_vars.

%---------------------------------------------------------------------------%

unify_terms(TermX, TermY, !Subst) :-
    (
        TermX = variable(X, _),
        TermY = variable(Y, _),
        ( if map.search(!.Subst, X, TermBoundToX) then
            ( if map.search(!.Subst, Y, TermBoundToY) then
                % Both X and Y already have bindings, so just unify
                % the terms they are bound to.
                unify_terms(TermBoundToX, TermBoundToY, !Subst)
            else
                % X is bound, but Y isn't.
                term_subst.apply_rec_substitution_in_term(!.Subst,
                    TermBoundToX, SubstTermBoundToX),
                ( if SubstTermBoundToX = variable(Y, _) then
                    true
                else
                    not var_occurs_in_subst_term(Y, !.Subst,
                        SubstTermBoundToX),
                    map.det_insert(Y, SubstTermBoundToX, !Subst)
                )
            )
        else
            ( if map.search(!.Subst, Y, TermBoundToY) then
                % Y is bound, but X isn't.
                term_subst.apply_rec_substitution_in_term(!.Subst,
                    TermBoundToY, SubstTermBoundToY),
                ( if SubstTermBoundToY = variable(X, _) then
                    true
                else
                    not var_occurs_in_subst_term(X, !.Subst,
                        SubstTermBoundToY),
                    map.det_insert(X, SubstTermBoundToY, !Subst)
                )
            else
                % Neither X nor Y are bound, so bind one to the other.
                ( if X = Y then
                    true
                else
                    map.det_insert(X, TermY, !Subst)
                )
            )
        )
    ;
        TermX = variable(X, _),
        TermY = functor(_, ArgTermsY, _),
        ( if map.search(!.Subst, X, TermBoundToX) then
            unify_terms(TermBoundToX, TermY, !Subst)
        else
            not var_occurs_in_subst_terms(X, !.Subst, ArgTermsY),
            map.det_insert(X, TermY, !Subst)
        )
    ;
        TermX = functor(_, ArgTermsX, _),
        TermY = variable(Y, _),
        ( if map.search(!.Subst, Y, TermBoundToY) then
            unify_terms(TermX, TermBoundToY, !Subst)
        else
            not var_occurs_in_subst_terms(Y, !.Subst, ArgTermsX),
            map.det_insert(Y, TermX, !Subst)
        )
    ;
        TermX = functor(NameX, ArgTermsX, _),
        TermY = functor(NameY, ArgTermsY, _),
        NameX = NameY,
        % We could pretest whether the lengths of the argument lists match.
        unify_term_lists(ArgTermsX, ArgTermsY, !Subst)
    ).

unify_term_lists([], [], !Subst).
unify_term_lists([TermX | TermXs], [TermY | TermYs], !Subst) :-
    unify_terms(TermX, TermY, !Subst),
    unify_term_lists(TermXs, TermYs, !Subst).

%---------------------------------------------------------------------------%

unify_terms_dont_bind(TermX, TermY, DontBindVars, !Subst) :-
    (
        TermX = variable(X, _),
        TermY = variable(Y, _),
        ( if list.member(Y, DontBindVars) then
            unify_terms_bound_var(X, Y, DontBindVars, !Subst)
        else if list.member(X, DontBindVars) then
            unify_terms_bound_var(Y, X, DontBindVars, !Subst)
        else if map.search(!.Subst, X, TermBoundToX) then
            ( if map.search(!.Subst, Y, TermBoundToY) then
                % Both X and Y already have bindings, so just unify
                % the terms they are bound to.
                unify_terms_dont_bind(TermBoundToX, TermBoundToY, DontBindVars,
                    !Subst)
            else
                % X is bound, but Y isn't.
                term_subst.apply_rec_substitution_in_term(!.Subst,
                    TermBoundToX, SubstTermBoundToX),
                ( if SubstTermBoundToX = variable(Y, _) then
                    true
                else
                    not var_occurs_in_subst_term(Y, !.Subst,
                        SubstTermBoundToX),
                    map.det_insert(Y, SubstTermBoundToX, !Subst)
                )
            )
        else
            ( if map.search(!.Subst, Y, TermBoundToY) then
                % Y is bound, but X isn't.
                term_subst.apply_rec_substitution_in_term(!.Subst,
                    TermBoundToY, SubstTermBoundToY),
                ( if SubstTermBoundToY = variable(X, _) then
                    true
                else
                    not var_occurs_in_subst_term(X, !.Subst,
                        SubstTermBoundToY),
                    map.det_insert(X, SubstTermBoundToY, !Subst)
                )
            else
                % Neither X nor Y are bound, so bind one to the other.
                ( if X = Y then
                    true
                else
                    map.det_insert(X, TermY, !Subst)
                )
            )
        )
    ;
        TermX = variable(X, _),
        TermY = functor(_, ArgTermsY, _),
        ( if map.search(!.Subst, X, TermBoundToX) then
            unify_terms_dont_bind(TermBoundToX, TermY, DontBindVars, !Subst)
        else
            not var_occurs_in_subst_terms(X, !.Subst, ArgTermsY),
            not list.member(X, DontBindVars),
            map.det_insert(X, TermY, !Subst)
        )
    ;
        TermX = functor(_, ArgTermsX, _),
        TermY = variable(Y, _),
        ( if map.search(!.Subst, Y, TermBoundToY) then
            unify_terms_dont_bind(TermX, TermBoundToY, DontBindVars, !Subst)
        else
            not var_occurs_in_subst_terms(Y, !.Subst, ArgTermsX),
            not list.member(Y, DontBindVars),
            map.det_insert(Y, TermX, !Subst)
        )
    ;
        TermX = functor(NameX, ArgTermsX, _CX),
        TermY = functor(NameY, ArgTermsY, _CY),
        NameX = NameY,
        list.length(ArgTermsX, ArityX),
        list.length(ArgTermsY, ArityY),
        ArityX = ArityY,
        unify_term_lists_dont_bind(ArgTermsX, ArgTermsY, DontBindVars, !Subst)
    ).

unify_term_lists_dont_bind([], [], _, !Subst).
unify_term_lists_dont_bind([TermX | TermXs], [TermY | TermYs],
        DontBindVars, !Subst) :-
    unify_terms_dont_bind(TermX, TermY, DontBindVars, !Subst),
    unify_term_lists_dont_bind(TermXs, TermYs, DontBindVars, !Subst).

:- pred unify_terms_bound_var(var(T)::in, var(T)::in, list(var(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

unify_terms_bound_var(X, BoundY, DontBindVars, !Subst) :-
    ( if map.search(!.Subst, X, TermBoundToX) then
        TermBoundToX = variable(NewX, _),
        unify_terms_bound_var(NewX, BoundY, DontBindVars, !Subst)
    else
        ( if X = BoundY then
            true
        else
            not list.member(X, DontBindVars),
            map.det_insert(X, variable(BoundY, dummy_context), !Subst)
        )
    ).

%---------------------------------------------------------------------------%

first_term_list_subsumes_second(Terms1, Terms2, Subst) :-
    % Terms1 subsumes Terms2 iff Terms1 can be unified with Terms2
    % without binding any of the variables in Terms2.
    vars_in_terms(Terms2, Terms2Vars),
    map.init(Subst0),
    unify_term_lists_dont_bind(Terms1, Terms2, Terms2Vars, Subst0, Subst).

%---------------------------------------------------------------------------%
:- end_module term_unify.
%---------------------------------------------------------------------------%
