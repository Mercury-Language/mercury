%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2003-2009,2011-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: term.m.
% Main author: fjh.
% Stability: medium.
%
% This file provides a type `term' used to represent Herbrand terms,
% and various predicates to manipulate terms and substitutions.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module term.
:- interface.

:- import_module enum.
:- import_module integer.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%
%
% The term type represents logic terms (Herbrand terms, in the terminology
% of logic programming theory).
%
% The term type is polymorphic. The intention is to allow terms representing
% different kinds of things to specify a different type parameter. Since
% e.g. term(type_a) is a different type from e.g. term(type_b), this should
% prevent terms of different kinds from being accidentally mixed up.
%
% For the predicates that operate on more than one term, such as unify_term,
% all the terms must use variables from the same varset.
% (You can use varset.merge_renaming to combine two different varsets.)
%

:- type term(T)
    --->    functor(
                const,
                list(term(T)),
                term.context
            )
    ;       variable(
                var(T),
                term.context
            ).

:- type var(T).

:- type const
    --->    atom(string)
    ;       integer(
                integer_base       :: integer_base,
                integer_value      :: integer,
                integer_signedness :: signedness,
                integer_size       :: integer_size
            )
    ;       string(string)
    ;       float(float)
    ;       implementation_defined(string).

:- type integer_base
    --->    base_2
    ;       base_8
    ;       base_10
    ;       base_16.

:- type signedness
    --->    signed
    ;       unsigned.

:- type integer_size
    --->    size_word
    ;       size_8_bit
    ;       size_16_bit
    ;       size_32_bit
    ;       size_64_bit.

:- type generic
    --->    generic.

:- type term ==  term(generic).
:- type var  ==  var(generic).

%---------------------------------------------------------------------------%
%
% These predicates manage the supply of variables.
% NOTE_TO_IMPLEMENTORS We might want to give these predicates unique modes.
%

:- type var_supply(T).

    % init_var_supply(VarSupply):
    %
    % Returns a fresh var_supply for producing fresh variables.
    %
:- func init_var_supply = var_supply(T).
:- pred init_var_supply(var_supply(T)).
:- mode init_var_supply(out) is det.
:- mode init_var_supply(in) is semidet. % implied

    % create_var(Var, !VarSupply):
    %
    % Create a fresh variable (var) and return the updated var_supply.
    %
:- pred create_var(var(T)::out, var_supply(T)::in, var_supply(T)::out) is det.

%---------------------------------------------------------------------------%

    % from_int/1 should only be applied to integers returned by to_int/1.
    % NOTE_TO_IMPLEMENTORS This instance declaration is needed to allow
    % NOTE_TO_IMPLEMENTORS sets of variables to be represented using
    % NOTE_TO_IMPLEMENTORS sparse_bitset.m and the other bitset modules.
:- instance enum(var(_)).

    % var_id(Variable):
    %
    % Returns a unique number associated with this variable w.r.t.
    % its originating var_supply.
    %
:- func var_to_int(var(T)) = int.
:- pred var_to_int(var(T)::in, int::out) is det.

    % var_id(Variable):
    %
    % Returns a unique number associated with this variable w.r.t.
    % its originating var_supply.
    %
    % Obsolete; please use var_to_int instead.
    %
:- func var_id(var(T)) = int.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(var_id/1).

%---------------------------------------------------------------------------%

:- type renaming(T) == map(var(T), var(T)).
:- type renaming    == renaming(generic).

:- type substitution(T) == map(var(T), term(T)).
:- type substitution    == substitution(generic).

%---------------------------------------------------------------------------%

:- pred term_to_int(term(T)::in, int::out) is semidet.

:- pred term_to_int8(term(T)::in, int8::out) is semidet.

:- pred term_to_int16(term(T)::in, int16::out) is semidet.

:- pred term_to_int32(term(T)::in, int32::out) is semidet.

:- pred term_to_int64(term(T)::in, int64::out) is semidet.

:- pred term_to_uint(term(T)::in, uint::out) is semidet.

:- pred term_to_uint8(term(T)::in, uint8::out) is semidet.

:- pred term_to_uint16(term(T)::in, uint16::out) is semidet.

:- pred term_to_uint32(term(T)::in, uint32::out) is semidet.

:- pred term_to_uint64(term(T)::in, uint64::out) is semidet.

:- pred decimal_term_to_int(term(T)::in, int::out) is semidet.

:- func int_to_decimal_term(int, context) = term(T).

:- func int8_to_decimal_term(int8, context) = term(T).

:- func int16_to_decimal_term(int16, context) = term(T).

:- func int32_to_decimal_term(int32, context) = term(T).

:- func int64_to_decimal_term(int64, context) = term(T).

:- func uint_to_decimal_term(uint, context) = term(T).

:- func uint8_to_decimal_term(uint8, context) = term(T).

:- func uint16_to_decimal_term(uint16, context) = term(T).

:- func uint32_to_decimal_term(uint32, context) = term(T).

:- func uint64_to_decimal_term(uint64, context) = term(T).

%---------------------------------------------------------------------------%
%
% Predicates to unify terms.
%

    % unify_term(TermA, TermB, !Subst):
    %
    % Unify (with occur check) two terms with respect to the current
    % substitution, and update that substitution as necessary.
    %
:- pred unify_term(term(T)::in, term(T)::in,
    substitution(T)::in, substitution(T)::out) is semidet.

    % unify_term_list(TermsA, TermsB, !Subst):
    %
    % Unify (with occur check) two lists of terms with respect to the current
    % substitution, and update that substitution as necessary.
    % Fail if the lists are not of equal length.
    %
:- pred unify_term_list(list(term(T))::in, list(term(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

    % unify_term_dont_bind(TermA, TermB, DontBindVars, !Subst):
    %
    % Do the same job as unify_term(TermA, TermB, !Subst), but fail
    % if any of the variables in DontBindVars would become bound
    % by the unification.
    %
:- pred unify_term_dont_bind(term(T)::in, term(T)::in,
    list(var(T))::in, substitution(T)::in, substitution(T)::out) is semidet.

    % unify_term_list_dont_bind(TermsA, TermsB, DontBindVars, !Subst):
    %
    % Do the same job as unify_term_list(TermsA, TermsB, !Subst), but fail
    % if any of the variables in DontBindVars would become bound
    % by the unification.
    %
:- pred unify_term_list_dont_bind(list(term(T))::in, list(term(T))::in,
    list(var(T))::in, substitution(T)::in, substitution(T)::out) is semidet.

%---------------------------------------------------------------------------%
%
% Predicates to test subsumption.
%

    % list_subsumes(TermsA, TermsB, Subst):
    %
    % Succeeds iff the list TermsA subsumes (is more general than) TermsB,
    % producing a substitution which, when applied to TermsA, will give TermsB.
    %
:- pred list_subsumes(list(term(T))::in, list(term(T))::in,
    substitution(T)::out) is semidet.

%---------------------------------------------------------------------------%
%
% Predicates that list the variables in terms.
%

    % vars(Term, Vars):
    %
    % Vars is the list of variables contained in Term, in the order
    % obtained by traversing the term depth first, left-to-right.
    %
:- func vars(term(T)) = list(var(T)).
:- pred vars(term(T)::in, list(var(T))::out) is det.

    % As above, but with an accumulator.
    %
:- func vars_2(term(T), list(var(T))) = list(var(T)).
:- pred vars_2(term(T)::in, list(var(T))::in, list(var(T))::out) is det.

    % vars_list(TermList, Vars):
    %
    % Vars is the list of variables contained in TermList, in the order
    % obtained by traversing the list of terms depth-first, left-to-right.
    %
:- func vars_list(list(term(T))) = list(var(T)).
:- pred vars_list(list(term(T))::in, list(var(T))::out) is det.

    % contains_var(Term, Var):
    %
    % True if Term contains Var. On backtracking returns all the variables
    % contained in Term.
    %
:- pred contains_var(term(T), var(T)).
:- mode contains_var(in, in) is semidet.
:- mode contains_var(in, out) is nondet.

    % contains_var_list(TermList, Var):
    %
    % True if TermList contains Var. On backtracking returns all the variables
    % contained in Term.
    %
:- pred contains_var_list(list(term(T)), var(T)).
:- mode contains_var_list(in, in) is semidet.
:- mode contains_var_list(in, out) is nondet.

%---------------------------------------------------------------------------%
%
% Predicates that look for variables in terms, possibly after a substitution.
%

    % occurs(Term, Var, Substitution):
    %
    % True iff Var occurs in the term resulting after applying Substitution
    % to Term. Var must not be mapped by Substitution.
    %
:- pred occurs(term(T)::in, var(T)::in, substitution(T)::in) is semidet.

    % As above, except for a list of terms rather than a single term.
    %
:- pred occurs_list(list(term(T))::in, var(T)::in, substitution(T)::in)
    is semidet.

    % is_ground(Term) is true iff Term contains no variables.
    %
:- pred is_ground(term(T)::in) is semidet.

    % is_ground_in_bindings(Term, Bindings) is true iff all variables contained
    % in Term are mapped to ground terms by Bindings.
    %
:- pred is_ground_in_bindings(term(T)::in, substitution(T)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Rename predicates that specify the substitution by giving the
% variable/variable pair or pairs directly.
%

    % relabel_variable(Term0, Var, ReplacementVar, Term):
    %
    % Replace all occurrences of Var in Term0 with ReplacementVar and return
    % the result as Term.
    %
    % Obsolete; please use rename_var_in_term instead.
    %
:- func relabel_variable(term(T), var(T), var(T)) = term(T).
:- pred relabel_variable(term(T)::in, var(T)::in, var(T)::in, term(T)::out)
    is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(relabel_variable/3).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(relabel_variable/4).

    % relabel_variables(Terms0, Var, ReplacementVar, Terms):
    %
    % Replace all occurrences of Var in Terms0 with ReplacementVar and return
    % the result as Terms.
    %
    % Obsolete; please use rename_var_in_terms instead.
    %
:- func relabel_variables(list(term(T)), var(T), var(T)) = list(term(T)).
:- pred relabel_variables(list(term(T))::in, var(T)::in, var(T)::in,
    list(term(T))::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(relabel_variables/3).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(relabel_variables/4).

%---------------------%

    % rename(Term0, Var, ReplacementVar, Term):
    %
    % Replace all occurrences of Var in Term0 with ReplacementVar,
    % and return the result in Term.
    %
    % Obsolete; please use rename_var_in_term instead.
    %
:- func rename(term(T), var(T), var(T)) = term(T).
:- pred rename(term(T)::in, var(T)::in, var(T)::in, term(T)::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(rename/3).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(rename/4).

    % rename_list(Terms0, Var, ReplacementVar, Terms):
    %
    % Replace all occurrences of Var in Terms0 with ReplacementVar,
    % and return the result in Terms.
    %
    % Obsolete; please use rename_var_in_terms instead.
    %
:- func rename_list(list(term(T)), var(T), var(T)) = list(term(T)).
:- pred rename_list(list(term(T))::in, var(T)::in, var(T)::in,
    list(term(T))::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(rename_list/3).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(rename_list/4).

%---------------------%

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

    % apply_renaming(Term0, Renaming, Term):
    %
    % Apply renaming to Term0 and return the result in Term.
    %
    % Obsolete; please use apply_renaming_in_term instead.
    %
:- func apply_renaming(term(T), renaming(T)) = term(T).
:- pred apply_renaming(term(T)::in, renaming(T)::in, term(T)::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_renaming/2).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_renaming/3).

    % As above, except applies to a list of terms rather than a single term.
    %
    % Obsolete; please use apply_renaming_in_terms instead.
    %
:- func apply_renaming_to_list(list(term(T)), renaming(T)) = list(term(T)).
:- pred apply_renaming_to_list(list(term(T))::in, renaming(T)::in,
    list(term(T))::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_renaming_to_list/2).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_renaming_to_list/3).

%---------------------%

    % Applies apply_variable_renaming to a var.
    %
    % Obsolete; please use apply_renaming_in_var instead.
    %
:- func apply_variable_renaming_to_var(renaming(T), var(T)) = var(T).
:- pred apply_variable_renaming_to_var(renaming(T)::in,
    var(T)::in, var(T)::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_variable_renaming_to_var/2).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_variable_renaming_to_var/3).

    % Applies apply_variable_renaming to a list of vars.
    %
    % Obsolete; please use apply_renaming_in_vars instead.
    %
:- func apply_variable_renaming_to_vars(renaming(T),
    list(var(T))) = list(var(T)).
:- pred apply_variable_renaming_to_vars(renaming(T)::in,
    list(var(T))::in, list(var(T))::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_variable_renaming_to_vars/2).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_variable_renaming_to_vars/3).

    % Same as relabel_variable, except relabels multiple variables.
    % If a variable is not in the map, it is not replaced.
    %
    % Obsolete; please use apply_renaming_in_term instead.
    %
:- func apply_variable_renaming(term(T), renaming(T)) = term(T).
:- pred apply_variable_renaming(term(T)::in, renaming(T)::in,
    term(T)::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_variable_renaming/2).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_variable_renaming/3).

    % Applies apply_variable_renaming to a list of terms.
    %
    % Obsolete; please use apply_renaming_in_terms instead.
    %
:- func apply_variable_renaming_to_list(list(term(T)), renaming(T)) =
    list(term(T)).
:- pred apply_variable_renaming_to_list(list(term(T))::in, renaming(T)::in,
    list(term(T))::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_variable_renaming_to_list/2).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_variable_renaming_to_list/3).

%---------------------%

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

    % substitute(Term0, Var, ReplacementTerm, Term):
    %
    % Replace all occurrences of Var in Term0 with ReplacementTerm,
    % and return the result as Term.
    %
    % Obsolete; please use substitute_var_in_term instead.
    %
:- func substitute(term(T), var(T), term(T)) = term(T).
:- pred substitute(term(T)::in, var(T)::in, term(T)::in, term(T)::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(substitute/3).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(substitute/4).

    % substitute_list(Var, ReplacementTerm, Terms0, Terms):
    %
    % Replace all occurrences of Var in Terms0 with ReplacementTerm,
    % and return the result as Terms.
    %
    % Obsolete; please use substitute_var_in_terms instead.
    %
:- func substitute_list(list(term(T)), var(T), term(T)) = list(term(T)).
:- pred substitute_list(list(term(T))::in, var(T)::in, term(T)::in,
    list(term(T))::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(substitute_list/3).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(substitute_list/4).

    % substitute_corresponding(Vars, ReplacementTerms, Term0, Term):
    %
    % Replace all occurrences of variables in Vars in Term0 with
    % the corresponding term in ReplacementTerms, and return the result
    % as Term. If Vars contains duplicates, or if Vars and ReplacementTerms
    % have different lengths, the behaviour is undefined and probably harmful.
    %
    % Obsolete; please use substitute_corresponding_in_term instead.
    %
:- func substitute_corresponding(list(var(T)), list(term(T)),
    term(T)) = term(T).
:- pred substitute_corresponding(list(var(T))::in, list(term(T))::in,
    term(T)::in, term(T)::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(substitute_corresponding/3).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(substitute_corresponding/4).

    % substitute_corresponding_list(Vars, ReplacementTerms, Terms0, Terms):
    %
    % Replace all occurrences of variables in Vars in Terms0 with
    % the corresponding term in ReplacementTerms, and return the result
    % as Terms. If Vars contains duplicates, or if Vars and ReplacementTerms
    % have different lengths, the behaviour is undefined and probably harmful.
    %
    % Obsolete; please use substitute_corresponding_in_terms instead.
    %
:- func substitute_corresponding_list(list(var(T)), list(term(T)),
    list(term(T))) = list(term(T)).
:- pred substitute_corresponding_list(list(var(T))::in, list(term(T))::in,
    list(term(T))::in, list(term(T))::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(substitute_corresponding_list/3).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(substitute_corresponding_list/4).

%---------------------%

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

    % apply_substitution(Term0, Substitution, Term):
    %
    % Apply Substitution to Term0 and return the result as Term.
    %
    % Obsolete; please us apply_substitution_in_term instead.
    %
:- func apply_substitution(term(T), substitution(T)) = term(T).
:- pred apply_substitution(term(T)::in, substitution(T)::in,
    term(T)::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_substitution/2).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_substitution/3).

    % apply_substitution_to_list(Term0, Substitution, Term):
    %
    % Apply Substitution to Term0 and return the result as Term.
    %
    % Obsolete; please us apply_substitution_in_terms instead.
    %
:- func apply_substitution_to_list(list(term(T)), substitution(T)) =
    list(term(T)).
:- pred apply_substitution_to_list(list(term(T))::in, substitution(T)::in,
    list(term(T))::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_substitution_to_list/2).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_substitution_to_list/3).

    % apply_rec_substitution(Term0, Substitution, Term):
    %
    % Recursively apply Substitution to Term0 until no more substitutions
    % can be applied, and then return the result as Term.
    %
    % Obsolete; please us apply_rec_substitution_in_term instead.
    %
:- func apply_rec_substitution(term(T), substitution(T)) = term(T).
:- pred apply_rec_substitution(term(T)::in, substitution(T)::in,
    term(T)::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_rec_substitution/2).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_rec_substitution/3).

    % apply_rec_substitution_to_list(Terms0, Substitution, Terms):
    %
    % Recursively apply Substitution to Terms0 until no more substitutions
    % can be applied, and then return the result as Terms.
    %
    % Obsolete; please us apply_rec_substitution_in_terms instead.
    %
:- func apply_rec_substitution_to_list(list(term(T)), substitution(T)) =
    list(term(T)).
:- pred apply_rec_substitution_to_list(list(term(T))::in, substitution(T)::in,
    list(term(T))::out) is det.
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_rec_substitution_to_list/2).
% NOTE_TO_IMPLEMENTORS :- pragma obsolete(apply_rec_substitution_to_list/3).

%---------------------%

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

    % Convert a list of terms which are all vars into a list of vars.
    % Throw an exception if the list contains any non-variables.
    %
:- func term_list_to_var_list(list(term(T))) = list(var(T)).

    % Convert a list of terms which are all vars into a list of vars.
    %
:- pred term_list_to_var_list(list(term(T))::in, list(var(T))::out) is semidet.

    % Convert a list of terms which are all vars into a list of vars
    % (or vice versa).
    %
:- func var_list_to_term_list(list(var(T))) = list(term(T)).
:- pred var_list_to_term_list(list(var(T))::in, list(term(T))::out) is det.

%---------------------------------------------------------------------------%

    % generic_term(Term) is true iff `Term' is a term of type
    % `term' ie `term(generic)'. It is useful because in some instances
    % it doesn't matter what the type of a term is, and passing it to this
    % predicate will ground the type avoiding unbound type variable warnings.
    % NOTE_TO_IMPLEMENTORS XXX This is not all that useful,
    % NOTE_TO_IMPLEMENTORS since we now have with_type.
    %
:- pred generic_term(term::in) is det.

    % Coerce a term of type `T' into a term of type `U'.
    %
:- func coerce(term(T)) = term(U).
:- pred coerce(term(T)::in, term(U)::out) is det.

    % Coerce a var of type `T' into a var of type `U'.
    %
:- func coerce_var(var(T)) = var(U).
:- pred coerce_var(var(T)::in, var(U)::out) is det.

    % Coerce a var_supply of type `T' into a var_supply of type `U'.
    %
:- func coerce_var_supply(var_supply(T)) = var_supply(U).
:- pred coerce_var_supply(var_supply(T)::in, var_supply(U)::out) is det.

%---------------------------------------------------------------------------%

    % NOTE_TO_IMPLEMENTORS: This type should get its own module.
:- type term.context
    --->    context(string, int).
            % file name, line number.

    % Return the context of a term.
    %
:- func get_term_context(term(T)) = term.context.

    % Initialize the term context when reading in (or otherwise constructing)
    % a term.
    %
:- func context_init(string, int) = context.
:- pred context_init(string::in, int::in, context::out) is det.

    % Return a dummy term context.
    %
:- func context_init = context.
:- pred context_init(context::out) is det.

:- pred is_dummy_context(context::in) is semidet.

    % Given a term context, return the source line number.
    %
:- func context_line(context) = int.
:- pred context_line(context::in, int::out) is det.

    % Given a term context, return the source file.
    %
:- func context_file(context) = string.
:- pred context_file(context::in, string::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.
% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.
:- interface.

    % Returns the highest numbered variable returned from this var_supply.
    %
:- func var_supply_max_var(var_supply(T)) = var(T).
:- func var_supply_num_allocated(var_supply(T)) = int.

:- func force_construct_var(int) = var(T).
:- func force_construct_var_supply(int) = var_supply(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

:- type var(T)
    --->    var(int).

:- type var_supply(T)
    --->    var_supply(int).

%---------------------------------------------------------------------------%

init_var_supply = var_supply(0).
init_var_supply(var_supply(0)).

create_var(var(V), var_supply(V0), var_supply(V)) :-
    % We number variables using sequential integers.
    V = V0 + 1.

%---------------------------------------------------------------------------%

:- instance enum(var(_)) where [
    to_int(X) = term.var_to_int(X),
    from_int(X) = term.unsafe_int_to_var(X)
].

    % Cast an integer to a var(T), subverting the type-checking.
    %
:- func unsafe_int_to_var(int) = var(T).

unsafe_int_to_var(VarNum) = var(VarNum).

var_to_int(var(VarNum)) = VarNum.
var_to_int(var(VarNum), VarNum).

var_id(var(VarNum)) = VarNum.

%---------------------------------------------------------------------------%

term_to_int(Term, Int) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, signed, size_word),
    integer.to_int(Integer, Int).

term_to_int8(Term, Int8) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, signed, size_8_bit),
    integer.to_int8(Integer, Int8).

term_to_int16(Term, Int16) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, signed, size_16_bit),
    integer.to_int16(Integer, Int16).

term_to_int32(Term, Int32) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, signed, size_32_bit),
    integer.to_int32(Integer, Int32).

term_to_int64(Term, Int64) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, signed, size_64_bit),
    integer.to_int64(Integer, Int64).

term_to_uint(Term, UInt) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, unsigned, size_word),
    integer.to_uint(Integer, UInt).

term_to_uint8(Term, UInt8) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, unsigned, size_8_bit),
    integer.to_uint8(Integer, UInt8).

term_to_uint16(Term, UInt16) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, unsigned, size_16_bit),
    integer.to_uint16(Integer, UInt16).

term_to_uint32(Term, UInt32) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, unsigned, size_32_bit),
    integer.to_uint32(Integer, UInt32).

term_to_uint64(Term, UInt64) :-
    Term = functor(Const, [], _Context),
    Const = integer(_Base, Integer, unsigned, size_64_bit),
    integer.to_uint64(Integer, UInt64).

decimal_term_to_int(Term, Int) :-
    Term = functor(Const, [], _Context),
    Const = integer(base_10, Integer, signed, size_word),
    integer.to_int(Integer, Int).

int_to_decimal_term(Int, Context) = Term :-
    Const = integer(base_10, integer(Int), signed, size_word),
    Term = functor(Const, [], Context).

int8_to_decimal_term(Int8, Context) = Term :-
    Const = integer(base_10, integer.from_int8(Int8), signed,
        size_8_bit),
    Term = functor(Const, [], Context).

int16_to_decimal_term(Int16, Context) = Term :-
    Const = integer(base_10, integer.from_int16(Int16), signed,
        size_16_bit),
    Term = functor(Const, [], Context).

int32_to_decimal_term(Int32, Context) = Term :-
    Const = integer(base_10, integer.from_int32(Int32), signed,
        size_32_bit),
    Term = functor(Const, [], Context).

int64_to_decimal_term(Int64, Context) = Term :-
    Const = integer(base_10, integer.from_int64(Int64), signed,
        size_64_bit),
    Term = functor(Const, [], Context).

uint_to_decimal_term(UInt, Context) = Term :-
    Const = integer(base_10, integer.from_uint(UInt), unsigned, size_word),
    Term = functor(Const, [], Context).

uint8_to_decimal_term(UInt8, Context) = Term :-
    Const = integer(base_10, integer.from_uint8(UInt8), unsigned,
        size_8_bit),
    Term = functor(Const, [], Context).

uint16_to_decimal_term(UInt16, Context) = Term :-
    Const = integer(base_10, integer.from_uint16(UInt16), unsigned,
        size_16_bit),
    Term = functor(Const, [], Context).

uint32_to_decimal_term(UInt32, Context) = Term :-
    Const = integer(base_10, integer.from_uint32(UInt32), unsigned,
        size_32_bit),
    Term = functor(Const, [], Context).

uint64_to_decimal_term(UInt64, Context) = Term :-
    Const = integer(base_10, integer.from_uint64(UInt64), unsigned,
        size_64_bit),
    Term = functor(Const, [], Context).

%---------------------------------------------------------------------------%

unify_term(TermX, TermY, !Subst) :-
    (
        TermX = variable(X, _),
        TermY = variable(Y, _),
        ( if map.search(!.Subst, X, TermBoundToX) then
            ( if map.search(!.Subst, Y, TermBoundToY) then
                % Both X and Y already have bindings, so just unify
                % the terms they are bound to.
                unify_term(TermBoundToX, TermBoundToY, !Subst)
            else
                % X is bound, but Y isn't.
                apply_rec_substitution_in_term(!.Subst,
                    TermBoundToX, SubstTermBoundToX),
                ( if SubstTermBoundToX = variable(Y, _) then
                    true
                else
                    not occurs(SubstTermBoundToX, Y, !.Subst),
                    map.det_insert(Y, SubstTermBoundToX, !Subst)
                )
            )
        else
            ( if map.search(!.Subst, Y, TermBoundToY) then
                % Y is bound, but X isn't.
                apply_rec_substitution_in_term(!.Subst,
                    TermBoundToY, SubstTermBoundToY),
                ( if SubstTermBoundToY = variable(X, _) then
                    true
                else
                    not occurs(SubstTermBoundToY, X, !.Subst),
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
            unify_term(TermBoundToX, TermY, !Subst)
        else
            not occurs_list(ArgTermsY, X, !.Subst),
            map.det_insert(X, TermY, !Subst)
        )
    ;
        TermX = functor(_, ArgTermsX, _),
        TermY = variable(Y, _),
        ( if map.search(!.Subst, Y, TermBoundToY) then
            unify_term(TermX, TermBoundToY, !Subst)
        else
            not occurs_list(ArgTermsX, Y, !.Subst),
            map.det_insert(Y, TermX, !Subst)
        )
    ;
        TermX = functor(NameX, ArgTermsX, _),
        TermY = functor(NameY, ArgTermsY, _),
        NameX = NameY,
        % ZZZ We could pretest whether the lengths of the argument lists match.
        unify_term_list(ArgTermsX, ArgTermsY, !Subst)
    ).

unify_term_list([], [], !Subst).
unify_term_list([TermX | TermXs], [TermY | TermYs], !Subst) :-
    unify_term(TermX, TermY, !Subst),
    unify_term_list(TermXs, TermYs, !Subst).

%---------------------------------------------------------------------------%

unify_term_dont_bind(TermX, TermY, DontBindVars, !Subst) :-
    (
        TermX = variable(X, _),
        TermY = variable(Y, _),
        ( if list.member(Y, DontBindVars) then
            unify_term_bound_var(X, Y, DontBindVars, !Subst)
        else if list.member(X, DontBindVars) then
            unify_term_bound_var(Y, X, DontBindVars, !Subst)
        else if map.search(!.Subst, X, TermBoundToX) then
            ( if map.search(!.Subst, Y, TermBoundToY) then
                % Both X and Y already have bindings, so just unify
                % the terms they are bound to.
                unify_term_dont_bind(TermBoundToX, TermBoundToY, DontBindVars,
                    !Subst)
            else
                % X is bound, but Y isn't.
                apply_rec_substitution_in_term(!.Subst,
                    TermBoundToX, SubstTermBoundToX),
                ( if SubstTermBoundToX = variable(Y, _) then
                    true
                else
                    not occurs(SubstTermBoundToX, Y, !.Subst),
                    map.det_insert(Y, SubstTermBoundToX, !Subst)
                )
            )
        else
            ( if map.search(!.Subst, Y, TermBoundToY) then
                % Y is bound, but X isn't.
                apply_rec_substitution_in_term(!.Subst,
                    TermBoundToY, SubstTermBoundToY),
                ( if SubstTermBoundToY = variable(X, _) then
                    true
                else
                    not occurs(SubstTermBoundToY, X, !.Subst),
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
            unify_term_dont_bind(TermBoundToX, TermY, DontBindVars, !Subst)
        else
            not occurs_list(ArgTermsY, X, !.Subst),
            not list.member(X, DontBindVars),
            map.det_insert(X, TermY, !Subst)
        )
    ;
        TermX = functor(_, ArgTermsX, _),
        TermY = variable(Y, _),
        ( if map.search(!.Subst, Y, TermBoundToY) then
            unify_term_dont_bind(TermX, TermBoundToY, DontBindVars, !Subst)
        else
            not occurs_list(ArgTermsX, Y, !.Subst),
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
        unify_term_list_dont_bind(ArgTermsX, ArgTermsY, DontBindVars, !Subst)
    ).

unify_term_list_dont_bind([], [], _, !Subst).
unify_term_list_dont_bind([TermX | TermXs], [TermY | TermYs],
        DontBindVars, !Subst) :-
    unify_term_dont_bind(TermX, TermY, DontBindVars, !Subst),
    unify_term_list_dont_bind(TermXs, TermYs, DontBindVars, !Subst).

:- pred unify_term_bound_var(var(T)::in, var(T)::in, list(var(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

unify_term_bound_var(X, BoundY, DontBindVars, !Subst) :-
    ( if map.search(!.Subst, X, TermBoundToX) then
        TermBoundToX = variable(NewX, _),
        unify_term_bound_var(NewX, BoundY, DontBindVars, !Subst)
    else
        ( if X = BoundY then
            true
        else
            not list.member(X, DontBindVars),
            map.det_insert(X, variable(BoundY, context_init), !Subst)
        )
    ).

%---------------------------------------------------------------------------%

list_subsumes(Terms1, Terms2, Subst) :-
    % Terms1 subsumes Terms2 iff Terms1 can be unified with Terms2
    % without binding any of the variables in Terms2.
    vars_list(Terms2, Terms2Vars),
    map.init(Subst0),
    unify_term_list_dont_bind(Terms1, Terms2, Terms2Vars, Subst0, Subst).

%---------------------------------------------------------------------------%

vars(Term) = Vars :-
    vars(Term, Vars).

vars(Term, Vars) :-
    vars_2(Term, [], Vars).

vars_2(Term, Vars0) = Vars :-
    vars_2(Term, Vars0, Vars).

vars_2(Term, !Vars) :-
    (
        Term = variable(Var, _),
        !:Vars = [Var | !.Vars]
    ;
        Term = functor(_, ArgTerms, _),
        vars_2_list(ArgTerms, !Vars)
    ).

vars_list(Terms) = Vars :-
    vars_list(Terms, Vars).

vars_list(Terms, Vars) :-
    vars_2_list(Terms, [], Vars).

:- pred vars_2_list(list(term(T))::in, list(var(T))::in, list(var(T))::out)
    is det.

vars_2_list([], !Vars).
vars_2_list([Term | Terms], !Vars) :-
    vars_2_list(Terms, !Vars),
    vars_2(Term, !Vars).

%---------------------------------------------------------------------------%

contains_var(variable(Var, _), Var).
contains_var(functor(_, ArgTerms, _), Var) :-
    contains_var_list(ArgTerms, Var).

contains_var_list([Term | _], Var) :-
    contains_var(Term, Var).
contains_var_list([_ | Terms], Var) :-
    contains_var_list(Terms, Var).

%---------------------------------------------------------------------------%

occurs(Term, Var, Subst) :-
    (
        Term = variable(X, _Context),
        ( if X = Var then
            true
        else
            map.search(Subst, X, TermBoundToX),
            occurs(TermBoundToX, Var, Subst)
        )
    ;
        Term = functor(_Name, ArgTerms, _Context),
        occurs_list(ArgTerms, Var, Subst)
    ).

occurs_list([Term | Terms], Var, Subst) :-
    ( if occurs(Term, Var, Subst) then
        true
    else
        occurs_list(Terms, Var, Subst)
    ).

%---------------------------------------------------------------------------%

is_ground(functor(_, ArgTerms, _)) :-
    is_ground_list(ArgTerms).

:- pred is_ground_list(list(term(T))::in) is semidet.

is_ground_list([]).
is_ground_list([Term | Terms]) :-
    is_ground(Term),
    is_ground_list(Terms).

%---------------------------------------------------------------------------%

is_ground_in_bindings(Term, Bindings) :-
    (
        Term = variable(Var, _),
        map.search(Bindings, Var, BoundTerm),
        is_ground_in_bindings(BoundTerm, Bindings)
    ;
        Term = functor(_, ArgTerms, _),
        are_ground_in_bindings(ArgTerms, Bindings)
    ).

:- pred are_ground_in_bindings(list(term(T))::in, substitution(T)::in)
    is semidet.

are_ground_in_bindings([], _Bindings).
are_ground_in_bindings([Term | Terms], Bindings) :-
    is_ground_in_bindings(Term, Bindings),
    are_ground_in_bindings(Terms, Bindings).

%---------------------------------------------------------------------------%

% Forwarding functions and predicates, to be obsoleted.
relabel_variable(Term0, Var, ReplacementVar) = Term :-
    rename_var_in_term(Var, ReplacementVar, Term0, Term).
relabel_variable(Term0, Var, ReplacementVar, Term) :-
    rename_var_in_term(Var, ReplacementVar, Term0, Term).
relabel_variables(Terms0, Var, ReplacementVar) = Terms :-
    rename_var_in_terms(Var, ReplacementVar, Terms0, Terms).
relabel_variables(Terms0, Var, ReplacementVar, Terms) :-
    rename_var_in_terms(Var, ReplacementVar, Terms0, Terms).
rename(Term0, Var, ReplacementVar) = Term :-
    rename_var_in_term(Var, ReplacementVar, Term0, Term).
rename(Term0, Var, ReplacementVar, Term) :-
    rename_var_in_term(Var, ReplacementVar, Term0, Term).
rename_list(Terms0, Var, ReplacementVar) = Terms :-
    rename_var_in_terms(Var, ReplacementVar, Terms0, Terms).
rename_list(Terms0, Var, ReplacementVar, Terms) :-
    rename_var_in_terms(Var, ReplacementVar, Terms0, Terms).

%---------------------%

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
        rename_var_in_terms(Var, ReplacementVar, ArgTerms0, ArgTerms),
        Term = functor(Name, ArgTerms, Context)
    ).

rename_var_in_terms(_Var, _ReplacementVar, [], []).
rename_var_in_terms(Var, ReplacementVar, [Term0 | Terms0], [Term | Terms]) :-
    rename_var_in_term(Var, ReplacementVar, Term0, Term),
    rename_var_in_terms(Var, ReplacementVar, Terms0, Terms).

%---------------------------------------------------------------------------%

% Forwarding functions and predicates, to be obsoleted.
apply_renaming(Term0, Renaming) = Term :-
    apply_renaming_in_term(Renaming, Term0, Term).
apply_renaming(Term0, Renaming, Term) :-
    apply_renaming_in_term(Renaming, Term0, Term).
apply_renaming_to_list(Terms0, Renaming) = Terms :-
    apply_renaming_in_terms(Renaming, Terms0, Terms).
apply_renaming_to_list(Terms0, Renaming, Terms) :-
    apply_renaming_in_terms(Renaming, Terms0, Terms).
apply_variable_renaming_to_var(Renaming, Var0) = Var :-
    apply_renaming_in_var(Renaming, Var0, Var).
apply_variable_renaming_to_var(Renaming, Var0, Var) :-
    apply_renaming_in_var(Renaming, Var0, Var).
apply_variable_renaming_to_vars(Renaming, Vars0) = Vars :-
    apply_renaming_in_vars(Renaming, Vars0, Vars).
apply_variable_renaming_to_vars(Renaming, Vars0, Vars) :-
    apply_renaming_in_vars(Renaming, Vars0, Vars).
apply_variable_renaming(Term0, Renaming) = Term :-
    apply_renaming_in_term(Renaming, Term0, Term).
apply_variable_renaming(Term0, Renaming, Term) :-
    apply_renaming_in_term(Renaming, Term0, Term).
apply_variable_renaming_to_list(Terms0, Renaming) = Terms :-
    apply_renaming_in_terms(Renaming, Terms0, Terms).
apply_variable_renaming_to_list(Terms0, Renaming, Terms) :-
    apply_renaming_in_terms(Renaming, Terms0, Terms).

%---------------------%

apply_renaming_in_var(Renaming, Var0, Var) :-
    ( if map.search(Renaming, Var0, NewVar) then
        Var = NewVar
    else
        Var = Var0
    ).

apply_renaming_in_vars(_Renaming, [], []).
apply_renaming_in_vars(Renaming, [Var0 | Vars0], [Var | Vars]) :-
    apply_renaming_in_var(Renaming, Var0, Var),
    apply_renaming_in_vars(Renaming, Vars0, Vars).

apply_renaming_in_term(Renaming, Term0, Term) :-
    (
        Term0 = variable(Var0, Context),
        apply_renaming_in_var(Renaming, Var0, Var),
        Term = variable(Var, Context)
    ;
        Term0 = functor(Name, ArgTerms0, Context),
        apply_renaming_in_terms(Renaming, ArgTerms0, ArgTerms),
        Term = functor(Name, ArgTerms, Context)
    ).

apply_renaming_in_terms(_, [], []).
apply_renaming_in_terms(Renaming, [Term0 | Terms0], [Term | Terms]) :-
    apply_renaming_in_term(Renaming, Term0, Term),
    apply_renaming_in_terms(Renaming, Terms0, Terms).

%---------------------------------------------------------------------------%

% Forwarding functions and predicates, to be obsoleted.
substitute(Term0, Var, ReplacementTerm) = Term :-
    substitute_var_in_term(Var, ReplacementTerm, Term0, Term).
substitute(Term0, Var, ReplacementTerm, Term) :-
    substitute_var_in_term(Var, ReplacementTerm, Term0, Term).
substitute_list(Terms0, Var, ReplacementTerm) = Terms :-
    substitute_var_in_terms(Var, ReplacementTerm, Terms0, Terms).
substitute_list(Terms0, Var, ReplacementTerm, Terms) :-
    substitute_var_in_terms(Var, ReplacementTerm, Terms0, Terms).
substitute_corresponding(Vars, ReplacementTerms, Term0) = Term :-
    substitute_corresponding_in_term(Vars, ReplacementTerms, Term0, Term).
substitute_corresponding(Vars, ReplacementTerms, Term0, Term) :-
    substitute_corresponding_in_term(Vars, ReplacementTerms, Term0, Term).
substitute_corresponding_list(Vars, ReplacementTerms, Terms0) = Terms :-
    substitute_corresponding_in_terms(Vars, ReplacementTerms, Terms0, Terms).
substitute_corresponding_list(Vars, ReplacementTerms, Terms0, Terms) :-
    substitute_corresponding_in_terms(Vars, ReplacementTerms, Terms0, Terms).

%---------------------%

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
        substitute_var_in_terms(Var, ReplacementTerm, ArgTerms0, ArgTerms),
        Term = functor(Name, ArgTerms, Context)
    ).

substitute_var_in_terms(_Var, _ReplacementTerm, [], []).
substitute_var_in_terms(Var, ReplacementTerm,
        [Term0 | Terms0], [Term | Terms]) :-
    substitute_var_in_term(Var, ReplacementTerm, Term0, Term),
    substitute_var_in_terms(Var, ReplacementTerm, Terms0, Terms).

substitute_corresponding_in_term(Vars, ReplacementTerms, Term0, Term) :-
    map.init(Subst0),
    build_subst(Vars, ReplacementTerms, Subst0, Subst),
    apply_substitution_in_term(Subst, Term0, Term).

substitute_corresponding_in_terms(Vars, ReplacementTerms, Terms0, Terms) :-
    map.init(Subst0),
    build_subst(Vars, ReplacementTerms, Subst0, Subst),
    apply_substitution_in_terms(Subst, Terms0, Terms).

%---------------------%

:- pred build_subst(list(var(T))::in, list(term(T))::in,
    substitution(T)::in, substitution(T)::out) is det.

build_subst([], [], !Subst).
build_subst([], [_ | _], !Subst) :-
    unexpected($module, $pred, "length mismatch").
build_subst([_ | _], [], !Subst) :-
    unexpected($module, $pred, "length mismatch").
build_subst([Var | Vars], [Term | Terms], !Subst) :-
    map.set(Var, Term, !Subst),
    build_subst(Vars, Terms, !Subst).

%---------------------------------------------------------------------------%

% Forwarding functions and predicates, to be obsoleted.
apply_substitution(Term0, Subst) = Term :-
    apply_substitution_in_term(Subst, Term0, Term).
apply_substitution(Term0, Subst, Term) :-
    apply_substitution_in_term(Subst, Term0, Term).
apply_substitution_to_list(Terms0, Subst) = Terms :-
    apply_substitution_in_terms(Subst, Terms0, Terms).
apply_substitution_to_list(Terms0, Subst, Terms) :-
    apply_substitution_in_terms(Subst, Terms0, Terms).
apply_rec_substitution(Term0, Subst) = Term :-
    apply_rec_substitution_in_term(Subst, Term0, Term).
apply_rec_substitution(Term0, Subst, Term) :-
    apply_rec_substitution_in_term(Subst, Term0, Term).
apply_rec_substitution_to_list(Terms0, Subst) = Terms :-
    apply_rec_substitution_in_terms(Subst, Terms0, Terms).
apply_rec_substitution_to_list(Terms0, Subst, Terms) :-
    apply_rec_substitution_in_terms(Subst, Terms0, Terms).

%---------------------%

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
        apply_substitution_in_terms(Subst, ArgTerms0, ArgTerms),
        Term = functor(Name, ArgTerms, Context)
    ).

apply_substitution_in_terms(_Subst, [], []).
apply_substitution_in_terms(Subst, [Term0 | Terms0], [Term | Terms]) :-
    apply_substitution_in_term(Subst, Term0, Term),
    apply_substitution_in_terms(Subst, Terms0, Terms).

apply_rec_substitution_in_term(Subst, Term0, Term) :-
    (
        Term0 = variable(Var, _),
        ( if map.search(Subst, Var, ReplacementTerm) then
            % Recursively apply the substitution to the replacement.
            apply_rec_substitution_in_term(Subst, ReplacementTerm, Term)
        else
            Term = Term0
        )
    ;
        Term0 = functor(Name, ArgTerms0, Context),
        apply_rec_substitution_in_terms(Subst, ArgTerms0, ArgTerms),
        Term = functor(Name, ArgTerms, Context)
    ).

apply_rec_substitution_in_terms(_Subst, [], []).
apply_rec_substitution_in_terms(Subst, [Term0 | Terms0], [Term | Terms]) :-
    apply_rec_substitution_in_term(Subst, Term0, Term),
    apply_rec_substitution_in_terms(Subst, Terms0, Terms).

%---------------------------------------------------------------------------%

term_list_to_var_list(Terms) = Vars :-
    ( if term_list_to_var_list(Terms, VarsPrime) then
        Vars = VarsPrime
    else
        unexpected($module, $pred, "not all vars")
    ).

term_list_to_var_list([], []).
term_list_to_var_list([variable(Var, _) | Terms], [Var | Vars]) :-
    term_list_to_var_list(Terms, Vars).

var_list_to_term_list(Vs) = Ts :-
    var_list_to_term_list(Vs, Ts).

var_list_to_term_list([], []).
var_list_to_term_list([Var | Vars], [variable(Var, context_init) | Terms]) :-
    var_list_to_term_list(Vars, Terms).

%---------------------------------------------------------------------------%

generic_term(_).

coerce(TermTypeA) = TermTypeB :-
    coerce(TermTypeA, TermTypeB).

coerce(TermTypeA, TermTypeB) :-
    % Normally calls to this predicate should only be generated by the
    % compiler, but type coercion by copying was taking about 3% of the
    % compiler's runtime.
    private_builtin.unsafe_type_cast(TermTypeA, TermTypeB).

coerce_var(VarTypeA) = VarTypeB :-
    coerce_var(VarTypeA, VarTypeB).

coerce_var(var(VarNum), var(VarNum)).

coerce_var_supply(VarSupplyTypeA) = VarSupplyTypeB :-
    coerce_var_supply(VarSupplyTypeA, VarSupplyTypeB).

coerce_var_supply(var_supply(Supply), var_supply(Supply)).

%---------------------------------------------------------------------------%

get_term_context(Term) = Context :-
    ( Term = functor(_, _, Context)
    ; Term = variable(_, Context)
    ).

context_init(File, LineNumber) = context(File, LineNumber).
context_init(File, LineNumber, context(File, LineNumber)).

context_init = context("", 0).
context_init(context("", 0)).

is_dummy_context(Context) :-
    Context = context("", 0).

context_line(context(_, LineNumber)) = LineNumber.
context_line(context(_, LineNumber), LineNumber).

context_file(context(FileName, _)) = FileName.
context_file(context(FileName, _), FileName).

%---------------------------------------------------------------------------%

var_supply_max_var(var_supply(V)) = var(V).

var_supply_num_allocated(var_supply(V)) = V.

force_construct_var(V) = var(V).

force_construct_var_supply(V) = var_supply(V).

%---------------------------------------------------------------------------%
