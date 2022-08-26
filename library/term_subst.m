%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2003-2009,2011-2012 The University of Melbourne.
% Copyright (C) 2014-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: term_subst.m.
% Main author: fjh.
% Stability: medium.
%
% This file provides operations that perform substitutions of various kinds
% on terms.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module term_subst.
:- interface.

:- import_module list.
:- import_module term.

%---------------------------------------------------------------------------%
%
% Predicates that look for variables in terms, possibly after a substitution.
%

    % var_occurs_in_subst_term(Var, Substitution, Term):
    %
    % True iff Var occurs in the term resulting after applying Substitution
    % to Term. Var must not be mapped by Substitution.
    %
:- pred var_occurs_in_subst_term(var(T)::in, substitution(T)::in,
    term(T)::in) is semidet.

    % As above, except for a list of terms rather than a single term.
    %
:- pred var_occurs_in_subst_terms(var(T)::in, substitution(T)::in,
    list(term(T))::in)   is semidet.

    % term_is_ground(Term) is true iff Term contains no variables.
    %
:- pred term_is_ground(term(T)::in) is semidet.

    % term_is_ground_in_bindings(Term, Bindings) is true iff
    % all variables contained in Term are mapped to ground terms by Bindings.
    %
:- pred term_is_ground_in_bindings(term(T)::in, substitution(T)::in)
    is semidet.

%---------------------------------------------------------------------------%
%
% Rename predicates that specify the substitution by giving the
% variable/variable pair or pairs directly.
%

    % rename_var_in_term(Var, ReplacementVar, Term0, Term):
    %
    % Replace all occurrences of Var in Term0 with ReplacementVar,
    % and return the result in Term.
    %
:- pred rename_var_in_term(var(T)::in, var(T)::in,
    term(T)::in, term(T)::out) is det.

    % rename_var_in_terms(Var, ReplacementVar, Terms0, Terms):
    %
    % Replace all occurrences of Var in Terms0 with ReplacementVar,
    % and return the result in Terms.
    %
:- pred rename_var_in_terms(var(T)::in, var(T)::in,
    list(term(T))::in, list(term(T))::out) is det.

%---------------------------------------------------------------------------%
%
% Rename predicates that specify the rename by giving an explicit
% variable to variable map.
%

    % apply_renaming_in_var(Renaming, Var0, Var):
    %
    % Apply Renaming in Var0, and return the result as Var.
    %
:- pred apply_renaming_in_var(renaming(T)::in,
    var(T)::in, var(T)::out) is det.

    % apply_renaming_in_vars(Renaming, Vars0, Vars):
    %
    % Apply Renaming in Vars0, and return the result as Vars.
    %
:- pred apply_renaming_in_vars(renaming(T)::in,
    list(var(T))::in, list(var(T))::out) is det.

    % apply_renaming_in_term(Renaming, Term0, Term):
    %
    % Apply Renaming in Term0, and return the result as Term.
    %
:- pred apply_renaming_in_term(renaming(T)::in,
    term(T)::in, term(T)::out) is det.

    % apply_renaming_in_terms(Renaming, Terms0, Terms):
    %
    % Apply Renaming in Terms0, and return the result as Terms.
    %
:- pred apply_renaming_in_terms(renaming(T)::in,
    list(term(T))::in, list(term(T))::out) is det.

%---------------------------------------------------------------------------%
%
% Substitution predicates that specify the substitution by giving the
% variable/term pair or pairs directly.
%

    % substitute_var_in_term(Var, ReplacementTerm, Term0, Term):
    %
    % Replace all occurrences of Var in Term0 with ReplacementTerm,
    % and return the result in Term.
    %
:- pred substitute_var_in_term(var(T)::in, term(T)::in,
    term(T)::in, term(T)::out) is det.

    % substitute_var_in_terms(Var, ReplacementTerm, Terms0, Terms):
    %
    % Replace all occurrences of Var in Terms0 with ReplacementTerm,
    % and return the result in Terms.
    %
:- pred substitute_var_in_terms(var(T)::in, term(T)::in,
    list(term(T))::in, list(term(T))::out) is det.

    % substitute_corresponding_in_term(Vars, ReplacementTerms, Term0, Term):
    %
    % Replace all occurrences of variables in Vars in Term0 with
    % the corresponding term in ReplacementTerms, and return the result
    % as Term. If Vars contains duplicates, or if Vars and ReplacementTerms
    % have different lengths, the behaviour is undefined and probably harmful.
    %
:- pred substitute_corresponding_in_term(list(var(T))::in, list(term(T))::in,
    term(T)::in, term(T)::out) is det.

    % substitute_corresponding_in_terms(Vars, ReplacementTerms, Terms0, Terms):
    %
    % Replace all occurrences of variables in Vars in Terms0 with
    % the corresponding term in ReplacementTerms, and return the result
    % as Terms. If Vars contains duplicates, or if Vars and ReplacementTerms
    % have different lengths, the behaviour is undefined and probably harmful.
    %
:- pred substitute_corresponding_in_terms(list(var(T))::in, list(term(T))::in,
    list(term(T))::in, list(term(T))::out) is det.

%---------------------------------------------------------------------------%
%
% Substitution predicates that specify the substitution by giving
% an explicit variable to term map.
%

    % apply_substitution_in_term(Substitution, Term0, Term):
    %
    % Apply Substitution to Term0 and return the result as Term.
    %
:- pred apply_substitution_in_term(substitution(T)::in,
    term(T)::in, term(T)::out) is det.

    % apply_substitution_in_terms(Substitution, Terms0, Terms):
    %
    % Apply Substitution to Terms0 and return the result as Terms.
    %
:- pred apply_substitution_in_terms(substitution(T)::in,
    list(term(T))::in, list(term(T))::out) is det.

    % apply_rec_substitution_in_term(Substitution, Term0, Term):
    %
    % Recursively apply Substitution to Term0 until no more substitutions
    % can be applied, and then return the result as Term.
    %
:- pred apply_rec_substitution_in_term(substitution(T)::in,
    term(T)::in, term(T)::out) is det.

    % apply_rec_substitution_in_terms(Substitution, Terms0, Terms):
    %
    % Recursively apply Substitution to Terms0 until no more substitutions
    % can be applied, and then return the result as Terms.
    %
:- pred apply_rec_substitution_in_terms(substitution(T)::in,
    list(term(T))::in, list(term(T))::out) is det.

%---------------------------------------------------------------------------%
%
% Conversions between variables and terms.
%

    % Convert a list of terms which are all variables into
    % a list of those variables. Throw an exception if the list contains
    % any terms that are not variables.
    %
:- func term_list_to_var_list(list(term(T))) = list(var(T)).

    % Convert a list of terms which are all variables into
    % a list of those variables.
    %
:- pred term_list_to_var_list(list(term(T))::in, list(var(T))::out) is semidet.

    % Convert a list of variables into a list of terms, each containing
    % one of those variables.
    %
:- func var_list_to_term_list(list(var(T))) = list(term(T)).
:- pred var_list_to_term_list(list(var(T))::in, list(term(T))::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.
:- import_module term_context.

%---------------------------------------------------------------------------%

var_occurs_in_subst_term(Var, Subst, Term) :-
    (
        Term = variable(X, _Context),
        ( if X = Var then
            true
        else
            map.search(Subst, X, TermBoundToX),
            var_occurs_in_subst_term(Var, Subst, TermBoundToX)
        )
    ;
        Term = functor(_Name, ArgTerms, _Context),
        var_occurs_in_subst_terms(Var, Subst, ArgTerms)
    ).

var_occurs_in_subst_terms(Var, Subst, [Term | Terms]) :-
    ( if var_occurs_in_subst_term(Var, Subst, Term) then
        true
    else
        var_occurs_in_subst_terms(Var, Subst, Terms)
    ).

%---------------------------------------------------------------------------%

term_is_ground(functor(_, ArgTerms, _)) :-
    terms_are_ground(ArgTerms).

:- pred terms_are_ground(list(term(T))::in) is semidet.

terms_are_ground([]).
terms_are_ground([Term | Terms]) :-
    term_is_ground(Term),
    terms_are_ground(Terms).

%---------------------------------------------------------------------------%

term_is_ground_in_bindings(Term, Bindings) :-
    (
        Term = variable(Var, _),
        map.search(Bindings, Var, BoundTerm),
        term_is_ground_in_bindings(BoundTerm, Bindings)
    ;
        Term = functor(_, ArgTerms, _),
        terms_are_ground_in_bindings(ArgTerms, Bindings)
    ).

:- pred terms_are_ground_in_bindings(list(term(T))::in, substitution(T)::in)
    is semidet.

terms_are_ground_in_bindings([], _Bindings).
terms_are_ground_in_bindings([Term | Terms], Bindings) :-
    term_is_ground_in_bindings(Term, Bindings),
    terms_are_ground_in_bindings(Terms, Bindings).

%---------------------------------------------------------------------------%

rename_var_in_term(Var, ReplacementVar, Term0, Term) :-
    (
        Term0 = variable(Var0, Context),
        ( if Var0 = Var then
            Term = variable(ReplacementVar, Context)
        else
            Term = Term0
        )
    ;
        Term0 = functor(Name, ArgTerms0, Context),
        term_subst.rename_var_in_terms(Var, ReplacementVar,
            ArgTerms0, ArgTerms),
        Term = functor(Name, ArgTerms, Context)
    ).

rename_var_in_terms(_Var, _ReplacementVar, [], []).
rename_var_in_terms(Var, ReplacementVar, [Term0 | Terms0], [Term | Terms]) :-
    term_subst.rename_var_in_term(Var, ReplacementVar, Term0, Term),
    term_subst.rename_var_in_terms(Var, ReplacementVar, Terms0, Terms).

%---------------------------------------------------------------------------%

apply_renaming_in_var(Renaming, Var0, Var) :-
    ( if map.search(Renaming, Var0, NewVar) then
        Var = NewVar
    else
        Var = Var0
    ).

apply_renaming_in_vars(_Renaming, [], []).
apply_renaming_in_vars(Renaming, [Var0 | Vars0], [Var | Vars]) :-
    term_subst.apply_renaming_in_var(Renaming, Var0, Var),
    term_subst.apply_renaming_in_vars(Renaming, Vars0, Vars).

apply_renaming_in_term(Renaming, Term0, Term) :-
    (
        Term0 = variable(Var0, Context),
        term_subst.apply_renaming_in_var(Renaming, Var0, Var),
        Term = variable(Var, Context)
    ;
        Term0 = functor(Name, ArgTerms0, Context),
        term_subst.apply_renaming_in_terms(Renaming, ArgTerms0, ArgTerms),
        Term = functor(Name, ArgTerms, Context)
    ).

apply_renaming_in_terms(_, [], []).
apply_renaming_in_terms(Renaming, [Term0 | Terms0], [Term | Terms]) :-
    term_subst.apply_renaming_in_term(Renaming, Term0, Term),
    term_subst.apply_renaming_in_terms(Renaming, Terms0, Terms).

%---------------------------------------------------------------------------%

substitute_var_in_term(Var, ReplacementTerm, Term0, Term) :-
    (
        Term0 = variable(Var0, _Context),
        ( if Var0 = Var then
            Term = ReplacementTerm
        else
            Term = Term0
        )
    ;
        Term0 = functor(Name, ArgTerms0, Context),
        term_subst.substitute_var_in_terms(Var, ReplacementTerm,
            ArgTerms0, ArgTerms),
        Term = functor(Name, ArgTerms, Context)
    ).

substitute_var_in_terms(_Var, _ReplacementTerm, [], []).
substitute_var_in_terms(Var, ReplacementTerm,
        [Term0 | Terms0], [Term | Terms]) :-
    term_subst.substitute_var_in_term(Var, ReplacementTerm, Term0, Term),
    term_subst.substitute_var_in_terms(Var, ReplacementTerm, Terms0, Terms).

substitute_corresponding_in_term(Vars, ReplacementTerms, Term0, Term) :-
    map.init(Subst0),
    build_subst(Vars, ReplacementTerms, Subst0, Subst),
    term_subst.apply_substitution_in_term(Subst, Term0, Term).

substitute_corresponding_in_terms(Vars, ReplacementTerms, Terms0, Terms) :-
    map.init(Subst0),
    build_subst(Vars, ReplacementTerms, Subst0, Subst),
    term_subst.apply_substitution_in_terms(Subst, Terms0, Terms).

%---------------------%

:- pred build_subst(list(var(T))::in, list(term(T))::in,
    substitution(T)::in, substitution(T)::out) is det.

build_subst([], [], !Subst).
build_subst([], [_ | _], !Subst) :-
    unexpected($pred, "length mismatch").
build_subst([_ | _], [], !Subst) :-
    unexpected($pred, "length mismatch").
build_subst([Var | Vars], [Term | Terms], !Subst) :-
    map.set(Var, Term, !Subst),
    build_subst(Vars, Terms, !Subst).

%---------------------------------------------------------------------------%

apply_substitution_in_term(Subst, Term0, Term) :-
    (
        Term0 = variable(Var, _),
        ( if map.search(Subst, Var, ReplacementTerm) then
            Term = ReplacementTerm
        else
            Term = Term0
        )
    ;
        Term0 = functor(Name, ArgTerms0, Context),
        term_subst.apply_substitution_in_terms(Subst, ArgTerms0, ArgTerms),
        Term = functor(Name, ArgTerms, Context)
    ).

apply_substitution_in_terms(_Subst, [], []).
apply_substitution_in_terms(Subst, [Term0 | Terms0], [Term | Terms]) :-
    term_subst.apply_substitution_in_term(Subst, Term0, Term),
    term_subst.apply_substitution_in_terms(Subst, Terms0, Terms).

apply_rec_substitution_in_term(Subst, Term0, Term) :-
    (
        Term0 = variable(Var, _),
        ( if map.search(Subst, Var, ReplacementTerm) then
            % Recursively apply the substitution to the replacement.
            term_subst.apply_rec_substitution_in_term(Subst,
                ReplacementTerm, Term)
        else
            Term = Term0
        )
    ;
        Term0 = functor(Name, ArgTerms0, Context),
        term_subst.apply_rec_substitution_in_terms(Subst, ArgTerms0, ArgTerms),
        Term = functor(Name, ArgTerms, Context)
    ).

apply_rec_substitution_in_terms(_Subst, [], []).
apply_rec_substitution_in_terms(Subst, [Term0 | Terms0], [Term | Terms]) :-
    term_subst.apply_rec_substitution_in_term(Subst, Term0, Term),
    term_subst.apply_rec_substitution_in_terms(Subst, Terms0, Terms).

%---------------------------------------------------------------------------%

term_list_to_var_list(Terms) = Vars :-
    ( if term_subst.term_list_to_var_list(Terms, VarsPrime) then
        Vars = VarsPrime
    else
        unexpected($pred, "not all vars")
    ).

term_list_to_var_list([], []).
term_list_to_var_list([variable(Var, _) | Terms], [Var | Vars]) :-
    term_subst.term_list_to_var_list(Terms, Vars).

var_list_to_term_list(Vs) = Ts :-
    term_subst.var_list_to_term_list(Vs, Ts).

var_list_to_term_list([], []).
var_list_to_term_list([Var | Vars], [variable(Var, dummy_context) | Terms]) :-
    term_subst.var_list_to_term_list(Vars, Terms).

%---------------------------------------------------------------------------%
:- end_module term_subst.
%---------------------------------------------------------------------------%
