%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2003-2009,2011-2012 The University of Melbourne.
% Copyright (C) 2014-2022 The Mercury team.
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

    % In terms constructed by library/mercury_term_parser.m, a functor
    % can have arguments *only* if its const is atom(...). If it is integer,
    % string, float or implementation_defined, then its argument list
    % is guaranteed to be [].
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

:- type term == term(generic).
:- type var  == var(generic).

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

%---------------------------------------------------------------------------%

:- type renaming(T) == map(var(T), var(T)).
:- type renaming    == renaming(generic).

:- type substitution(T) == map(var(T), term(T)).
:- type substitution    == substitution(generic).

%---------------------------------------------------------------------------%

    % generic_term(Term) is true iff Term is a term of type
    % `term' i.e. `term(generic)'. It is useful because in some instances
    % it doesn't matter what the type of a term is, and passing it to this
    % predicate will ground the type avoiding unbound type variable warnings.
    % NOTE_TO_IMPLEMENTORS XXX This is not all that useful,
    % NOTE_TO_IMPLEMENTORS since we now have with_type.
    %
:- pred generic_term(term::in) is det.
:- pragma obsolete(pred(generic_term/1)).

    % Coerce a term of type T into a term of type U.
    %
:- func coerce(term(T)) = term(U).
:- pred coerce(term(T)::in, term(U)::out) is det.

    % Coerce a var of type T into a var of type U.
    %
:- func coerce_var(var(T)) = var(U).
:- pred coerce_var(var(T)::in, var(U)::out) is det.

    % Coerce a var_supply of type T into a var_supply of type U.
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
:- func dummy_context_init = context.
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

:- pred decimal_term_to_int(term(T)::in, int::out) is semidet.
:- pragma obsolete(pred(decimal_term_to_int/2),
    [term_int.decimal_term_to_int/2]).

:- pred term_to_int(term(T)::in, int::out) is semidet.
:- pred term_to_int8(term(T)::in, int8::out) is semidet.
:- pred term_to_int16(term(T)::in, int16::out) is semidet.
:- pred term_to_int32(term(T)::in, int32::out) is semidet.
:- pred term_to_int64(term(T)::in, int64::out) is semidet.
:- pragma obsolete(pred(term_to_int/2),   [term_int.term_to_int/2]).
:- pragma obsolete(pred(term_to_int8/2),  [term_int.term_to_int8/2]).
:- pragma obsolete(pred(term_to_int16/2), [term_int.term_to_int16/2]).
:- pragma obsolete(pred(term_to_int32/2), [term_int.term_to_int32/2]).
:- pragma obsolete(pred(term_to_int64/2), [term_int.term_to_int64/2]).

:- pred term_to_uint(term(T)::in, uint::out) is semidet.
:- pred term_to_uint8(term(T)::in, uint8::out) is semidet.
:- pred term_to_uint16(term(T)::in, uint16::out) is semidet.
:- pred term_to_uint32(term(T)::in, uint32::out) is semidet.
:- pred term_to_uint64(term(T)::in, uint64::out) is semidet.
:- pragma obsolete(pred(term_to_uint/2),   [term_int.term_to_uint/2]).
:- pragma obsolete(pred(term_to_uint8/2),  [term_int.term_to_uint8/2]).
:- pragma obsolete(pred(term_to_uint16/2), [term_int.term_to_uint16/2]).
:- pragma obsolete(pred(term_to_uint32/2), [term_int.term_to_uint32/2]).
:- pragma obsolete(pred(term_to_uint64/2), [term_int.term_to_uint64/2]).

:- func int_to_decimal_term(int, context) = term(T).
:- func int8_to_decimal_term(int8, context) = term(T).
:- func int16_to_decimal_term(int16, context) = term(T).
:- func int32_to_decimal_term(int32, context) = term(T).
:- func int64_to_decimal_term(int64, context) = term(T).
:- pragma obsolete(func(int_to_decimal_term/2),
    [term_int.int_to_decimal_term/2]).
:- pragma obsolete(func(int8_to_decimal_term/2),
    [term_int.int8_to_decimal_term/2]).
:- pragma obsolete(func(int16_to_decimal_term/2),
    [term_int.int16_to_decimal_term/2]).
:- pragma obsolete(func(int32_to_decimal_term/2),
    [term_int.int32_to_decimal_term/2]).
:- pragma obsolete(func(int64_to_decimal_term/2),
    [term_int.int64_to_decimal_term/2]).

:- func uint_to_decimal_term(uint, context) = term(T).
:- func uint8_to_decimal_term(uint8, context) = term(T).
:- func uint16_to_decimal_term(uint16, context) = term(T).
:- func uint32_to_decimal_term(uint32, context) = term(T).
:- func uint64_to_decimal_term(uint64, context) = term(T).
:- pragma obsolete(func(uint_to_decimal_term/2),
    [term_int.uint_to_decimal_term/2]).
:- pragma obsolete(func(uint8_to_decimal_term/2),
    [term_int.uint8_to_decimal_term/2]).
:- pragma obsolete(func(uint16_to_decimal_term/2),
    [term_int.uint16_to_decimal_term/2]).
:- pragma obsolete(func(uint32_to_decimal_term/2),
    [term_int.uint32_to_decimal_term/2]).
:- pragma obsolete(func(uint64_to_decimal_term/2),
    [term_int.uint64_to_decimal_term/2]).

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
:- pragma obsolete(pred(occurs/3),
    [term_subst.var_occurs_in_subst_term/3]).

    % As above, except for a list of terms rather than a single term.
    %
:- pred occurs_list(list(term(T))::in, var(T)::in, substitution(T)::in)
    is semidet.
:- pragma obsolete(pred(occurs_list/3),
    [term_subst.var_occurs_in_subst_terms/3]).

    % is_ground(Term) is true iff Term contains no variables.
    %
:- pred is_ground(term(T)::in) is semidet.
:- pragma obsolete(pred(is_ground/1),
    [term_subst.term_is_ground/1]).

    % is_ground_in_bindings(Term, Bindings) is true iff
    % all variables contained in Term are mapped to ground terms by Bindings.
    %
:- pred is_ground_in_bindings(term(T)::in, substitution(T)::in) is semidet.
:- pragma obsolete(pred(is_ground_in_bindings/2),
    [term_subst.term_is_ground_in_bindings/2]).

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
:- pragma obsolete(pred(rename_var_in_term/4),
    [term_subst.rename_var_in_term/4]).

    % rename_var_in_terms(Var, ReplacementVar, Terms0, Terms):
    %
    % Replace all occurrences of Var in Terms0 with ReplacementVar,
    % and return the result in Terms.
    %
:- pred rename_var_in_terms(var(T)::in, var(T)::in,
    list(term(T))::in, list(term(T))::out) is det.
:- pragma obsolete(pred(rename_var_in_terms/4),
    [term_subst.rename_var_in_terms/4]).

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
:- pragma obsolete(pred(apply_renaming_in_var/3),
    [term_subst.apply_renaming_in_var/3]).

    % apply_renaming_in_vars(Renaming, Vars0, Vars):
    %
    % Apply Renaming in Vars0, and return the result as Vars.
    %
:- pred apply_renaming_in_vars(renaming(T)::in,
    list(var(T))::in, list(var(T))::out) is det.
:- pragma obsolete(pred(apply_renaming_in_vars/3),
    [term_subst.apply_renaming_in_vars/3]).

    % apply_renaming_in_term(Renaming, Term0, Term):
    %
    % Apply Renaming in Term0, and return the result as Term.
    %
:- pred apply_renaming_in_term(renaming(T)::in,
    term(T)::in, term(T)::out) is det.
:- pragma obsolete(pred(apply_renaming_in_term/3),
    [term_subst.apply_renaming_in_term/3]).

    % apply_renaming_in_terms(Renaming, Terms0, Terms):
    %
    % Apply Renaming in Terms0, and return the result as Terms.
    %
:- pred apply_renaming_in_terms(renaming(T)::in,
    list(term(T))::in, list(term(T))::out) is det.
:- pragma obsolete(pred(apply_renaming_in_terms/3),
    [term_subst.apply_renaming_in_terms/3]).

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
:- pragma obsolete(pred(substitute_var_in_term/4),
    [term_subst.substitute_var_in_term/4]).

    % substitute_var_in_terms(Var, ReplacementTerm, Terms0, Terms):
    %
    % Replace all occurrences of Var in Terms0 with ReplacementTerm,
    % and return the result in Terms.
    %
:- pred substitute_var_in_terms(var(T)::in, term(T)::in,
    list(term(T))::in, list(term(T))::out) is det.
:- pragma obsolete(pred(substitute_var_in_terms/4),
    [term_subst.substitute_var_in_terms/4]).

    % substitute_corresponding_in_term(Vars, ReplacementTerms, Term0, Term):
    %
    % Replace all occurrences of variables in Vars in Term0 with
    % the corresponding term in ReplacementTerms, and return the result
    % as Term. If Vars contains duplicates, or if Vars and ReplacementTerms
    % have different lengths, the behaviour is undefined and probably harmful.
    %
:- pred substitute_corresponding_in_term(list(var(T))::in, list(term(T))::in,
    term(T)::in, term(T)::out) is det.
:- pragma obsolete(pred(substitute_corresponding_in_term/4),
    [term_subst.substitute_corresponding_in_term/4]).

    % substitute_corresponding_in_terms(Vars, ReplacementTerms, Terms0, Terms):
    %
    % Replace all occurrences of variables in Vars in Terms0 with
    % the corresponding term in ReplacementTerms, and return the result
    % as Terms. If Vars contains duplicates, or if Vars and ReplacementTerms
    % have different lengths, the behaviour is undefined and probably harmful.
    %
:- pred substitute_corresponding_in_terms(list(var(T))::in, list(term(T))::in,
    list(term(T))::in, list(term(T))::out) is det.
:- pragma obsolete(pred(substitute_corresponding_in_terms/4),
    [term_subst.substitute_corresponding_in_terms/4]).

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
:- pragma obsolete(pred(apply_substitution_in_term/3),
    [term_subst.apply_substitution_in_term/3]).

    % apply_substitution_in_terms(Substitution, Terms0, Terms):
    %
    % Apply Substitution to Terms0 and return the result as Terms.
    %
:- pred apply_substitution_in_terms(substitution(T)::in,
    list(term(T))::in, list(term(T))::out) is det.
:- pragma obsolete(pred(apply_substitution_in_terms/3),
    [term_subst.apply_substitution_in_terms/3]).

    % apply_rec_substitution_in_term(Substitution, Term0, Term):
    %
    % Recursively apply Substitution to Term0 until no more substitutions
    % can be applied, and then return the result as Term.
    %
:- pred apply_rec_substitution_in_term(substitution(T)::in,
    term(T)::in, term(T)::out) is det.
:- pragma obsolete(pred(apply_rec_substitution_in_term/3),
    [term_subst.apply_rec_substitution_in_term/3]).

    % apply_rec_substitution_in_terms(Substitution, Terms0, Terms):
    %
    % Recursively apply Substitution to Terms0 until no more substitutions
    % can be applied, and then return the result as Terms.
    %
:- pred apply_rec_substitution_in_terms(substitution(T)::in,
    list(term(T))::in, list(term(T))::out) is det.
:- pragma obsolete(pred(apply_rec_substitution_in_terms/3),
    [term_subst.apply_rec_substitution_in_terms/3]).

%---------------------------------------------------------------------------%
%
% Conversions between variables and terms.
%

    % Convert a list of terms which are all variables into
    % a list of those variables. Throw an exception if the list contains
    % any terms that are not variables.
    %
:- func term_list_to_var_list(list(term(T))) = list(var(T)).
:- pragma obsolete(func(term_list_to_var_list/1),
    [term_subst.term_list_to_var_list/1]).

    % Convert a list of terms which are all variables into
    % a list of those variables.
    %
:- pred term_list_to_var_list(list(term(T))::in, list(var(T))::out) is semidet.
:- pragma obsolete(pred(term_list_to_var_list/2),
    [term_subst.term_list_to_var_list/2]).

    % Convert a list of variables into a list of terms, each containing
    % one of those variables.
    %
:- func var_list_to_term_list(list(var(T))) = list(term(T)).
:- pred var_list_to_term_list(list(var(T))::in, list(term(T))::out) is det.
:- pragma obsolete(func(var_list_to_term_list/1),
    [term_subst.var_list_to_term_list/1]).
:- pragma obsolete(pred(var_list_to_term_list/2),
    [term_subst.var_list_to_term_list/2]).

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
:- pragma obsolete(pred(unify_term/4), [term_unify.unify_terms/4]).

    % unify_term_list(TermsA, TermsB, !Subst):
    %
    % Unify (with occur check) two lists of terms with respect to the current
    % substitution, and update that substitution as necessary.
    % Fail if the lists are not of equal length.
    %
:- pred unify_term_list(list(term(T))::in, list(term(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.
:- pragma obsolete(pred(unify_term_list/4), [term_unify.unify_term_lists/4]).

    % unify_term_dont_bind(TermA, TermB, DontBindVars, !Subst):
    %
    % Do the same job as unify_term(TermA, TermB, !Subst), but fail
    % if any of the variables in DontBindVars would become bound
    % by the unification.
    %
:- pred unify_term_dont_bind(term(T)::in, term(T)::in,
    list(var(T))::in, substitution(T)::in, substitution(T)::out) is semidet.
:- pragma obsolete(pred(unify_term_dont_bind/5),
    [term_unify.unify_terms_dont_bind/5]).

    % unify_term_list_dont_bind(TermsA, TermsB, DontBindVars, !Subst):
    %
    % Do the same job as unify_term_list(TermsA, TermsB, !Subst), but fail
    % if any of the variables in DontBindVars would become bound
    % by the unification.
    %
:- pred unify_term_list_dont_bind(list(term(T))::in, list(term(T))::in,
    list(var(T))::in, substitution(T)::in, substitution(T)::out) is semidet.
:- pragma obsolete(pred(unify_term_list_dont_bind/5),
    [term_unify.unify_term_lists_dont_bind/5]).

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
:- pragma obsolete(pred(list_subsumes/3),
    [term_unify.first_term_list_subsumes_second/3]).

%---------------------------------------------------------------------------%
%
% Predicates that list the variables in one or more terms.
%

    % vars_in_term(Term, Vars):
    %
    % Vars is the list of variables contained in Term, in the order
    % obtained by traversing the term depth first, left-to-right.
    %
:- func vars(term(T)) = list(var(T)).
:- pred vars(term(T)::in, list(var(T))::out) is det.
:- pragma obsolete(func(vars/1), [term_vars.vars_in_term/1]).
:- pragma obsolete(pred(vars/2), [term_vars.vars_in_term/2]).

    % As above, but with an accumulator: add the variables in the term
    % to the front of the initial value of the accumulator.
    %
:- func vars_2(term(T), list(var(T))) = list(var(T)).
:- pred vars_2(term(T)::in, list(var(T))::in, list(var(T))::out) is det.
:- pragma obsolete(func(vars_2/2), [term_vars.vars_in_term_acc/3]).
:- pragma obsolete(pred(vars_2/3), [term_vars.vars_in_term_acc/3]).

    % vars_in_terms(Terms, Vars):
    %
    % Vars is the list of variables contained in Terms, in the order
    % obtained by traversing the list of terms depth-first, left-to-right.
    %
:- func vars_list(list(term(T))) = list(var(T)).
:- pred vars_list(list(term(T))::in, list(var(T))::out) is det.
:- pragma obsolete(func(vars_list/1), [term_vars.vars_in_terms/1]).
:- pragma obsolete(pred(vars_list/2), [term_vars.vars_in_terms/2]).

    % term_contains_var(Term, Var):
    %
    % True if Term contains Var. The second mode returns all the variables
    % in Term, one at a time.
    %
:- pred contains_var(term(T), var(T)).
:- mode contains_var(in, in) is semidet.
:- mode contains_var(in, out) is nondet.
:- pragma obsolete(pred(contains_var/2), [term_vars.term_contains_var/2]).

    % terms_contain_var(Terms, Var):
    %
    % True if Terms contains Var. The second mode returns all the variables
    % in Terms, one at a time.
    %
:- pred contains_var_list(list(term(T)), var(T)).
:- mode contains_var_list(in, in) is semidet.
:- mode contains_var_list(in, out) is nondet.
:- pragma obsolete(pred(contains_var_list/2), [term_vars.terms_contain_var/2]).

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
:- import_module term_int.
:- import_module term_subst.
:- import_module term_unify.
:- import_module term_vars.

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

dummy_context_init = context("", 0).
context_init = dummy_context_init.
context_init(dummy_context_init).

is_dummy_context(Context) :-
    Context = dummy_context_init.

context_line(context(_, LineNumber)) = LineNumber.
context_line(context(_, LineNumber), LineNumber).

context_file(context(FileName, _)) = FileName.
context_file(context(FileName, _), FileName).

%---------------------------------------------------------------------------%

decimal_term_to_int(Term, Int) :-
    term_int.decimal_term_to_int(Term, Int).

term_to_int(Term, Int) :-
    term_int.term_to_int(Term, Int).
term_to_int8(Term, Int8) :-
    term_int.term_to_int8(Term, Int8).
term_to_int16(Term, Int16) :-
    term_int.term_to_int16(Term, Int16).
term_to_int32(Term, Int32) :-
    term_int.term_to_int32(Term, Int32).
term_to_int64(Term, Int64) :-
    term_int.term_to_int64(Term, Int64).

term_to_uint(Term, UInt) :-
    term_int.term_to_uint(Term, UInt).
term_to_uint8(Term, UInt8) :-
    term_int.term_to_uint8(Term, UInt8).
term_to_uint16(Term, UInt16) :-
    term_int.term_to_uint16(Term, UInt16).
term_to_uint32(Term, UInt32) :-
    term_int.term_to_uint32(Term, UInt32).
term_to_uint64(Term, UInt64) :-
    term_int.term_to_uint64(Term, UInt64).

int_to_decimal_term(Int, Context) =
    term_int.int_to_decimal_term(Int, Context).
int8_to_decimal_term(Int8, Context) =
    term_int.int8_to_decimal_term(Int8, Context).
int16_to_decimal_term(Int16, Context) =
    term_int.int16_to_decimal_term(Int16, Context).
int32_to_decimal_term(Int32, Context) =
    term_int.int32_to_decimal_term(Int32, Context).
int64_to_decimal_term(Int64, Context) =
    term_int.int64_to_decimal_term(Int64, Context).

uint_to_decimal_term(UInt, Context) =
    term_int.uint_to_decimal_term(UInt, Context).
uint8_to_decimal_term(UInt8, Context) =
    term_int.uint8_to_decimal_term(UInt8, Context).
uint16_to_decimal_term(UInt16, Context) =
    term_int.uint16_to_decimal_term(UInt16, Context).
uint32_to_decimal_term(UInt32, Context) =
    term_int.uint32_to_decimal_term(UInt32, Context).
uint64_to_decimal_term(UInt64, Context) =
    term_int.uint64_to_decimal_term(UInt64, Context).

%---------------------------------------------------------------------------%

occurs(Term, Var, Subst) :-
    term_subst.var_occurs_in_subst_term(Var, Subst, Term).

occurs_list(Terms, Var, Subst) :-
    term_subst.var_occurs_in_subst_terms(Var, Subst, Terms).

is_ground(Term) :-
    term_subst.term_is_ground(Term).

is_ground_in_bindings(Term, Subst) :-
    term_subst.term_is_ground_in_bindings(Term, Subst).

rename_var_in_term(Var, ReplacementVar, !Term) :-
    term_subst.rename_var_in_term(Var, ReplacementVar, !Term).

rename_var_in_terms(Var, ReplacementVar, !Terms) :-
    term_subst.rename_var_in_terms(Var, ReplacementVar, !Terms).

apply_renaming_in_var(Renaming, !Var) :-
    term_subst.apply_renaming_in_var(Renaming, !Var).

apply_renaming_in_vars(Renaming, !Vars) :-
    term_subst.apply_renaming_in_vars(Renaming, !Vars).

apply_renaming_in_term(Renaming, !Term) :-
    term_subst.apply_renaming_in_term(Renaming, !Term).

apply_renaming_in_terms(Renaming, !Terms) :-
    term_subst.apply_renaming_in_terms(Renaming, !Terms).

substitute_var_in_term(Var, ReplacementTerm, !Term) :-
    term_subst.substitute_var_in_term(Var, ReplacementTerm, !Term).

substitute_var_in_terms(Var, ReplacementTerm, !Terms) :-
    term_subst.substitute_var_in_terms(Var, ReplacementTerm, !Terms).

substitute_corresponding_in_term(Var, ReplacementTerm, !Term) :-
    term_subst.substitute_corresponding_in_term(Var, ReplacementTerm, !Term).

substitute_corresponding_in_terms(Var, ReplacementTerm, !Terms) :-
    term_subst.substitute_corresponding_in_terms(Var, ReplacementTerm, !Terms).

apply_substitution_in_term(Subst, !Term) :-
    term_subst.apply_substitution_in_term(Subst, !Term).

apply_substitution_in_terms(Subst, !Terms) :-
    term_subst.apply_substitution_in_terms(Subst, !Terms).

apply_rec_substitution_in_term(Subst, !Term) :-
    term_subst.apply_rec_substitution_in_term(Subst, !Term).

apply_rec_substitution_in_terms(Subst, !Terms) :-
    term_subst.apply_rec_substitution_in_terms(Subst, !Terms).

term_list_to_var_list(Terms) =
    term_subst.term_list_to_var_list(Terms).

term_list_to_var_list(Terms, Vars) :-
    term_subst.term_list_to_var_list(Terms, Vars).

var_list_to_term_list(Vars) =
    term_subst.var_list_to_term_list(Vars).

var_list_to_term_list(Vars, Terms) :-
    term_subst.var_list_to_term_list(Vars, Terms).

%---------------------------------------------------------------------------%

unify_term(TermX, TermY, !Subst) :-
    term_unify.unify_terms(TermX, TermY, !Subst).

unify_term_list(TermsX, TermsY, !Subst) :-
    term_unify.unify_term_lists(TermsX, TermsY, !Subst).

unify_term_dont_bind(TermX, TermY, DontBindVars, !Subst) :-
    term_unify.unify_terms_dont_bind(TermX, TermY, DontBindVars, !Subst).

unify_term_list_dont_bind(TermsX, TermsY, DontBindVars, !Subst) :-
    term_unify.unify_term_lists_dont_bind(TermsX, TermsY, DontBindVars,
        !Subst).

list_subsumes(TermsX, TermsY, Subst) :-
    term_unify.first_term_list_subsumes_second(TermsX, TermsY, Subst).

%---------------------------------------------------------------------------%

vars(Term) =
    term_vars.vars_in_term(Term).

vars(Term, Vars) :-
    term_vars.vars_in_term(Term, Vars).

vars_2(Term, !.Acc) = !:Acc :-
    term_vars.vars_in_term_acc(Term, !Acc).

vars_2(Term, !Acc) :-
    term_vars.vars_in_term_acc(Term, !Acc).

vars_list(Terms) =
    term_vars.vars_in_terms(Terms).

vars_list(Terms, Vars) :-
    term_vars.vars_in_terms(Terms, Vars).

contains_var(Term, Var) :-
    term_vars.term_contains_var(Term, Var).

contains_var_list(Terms, Var) :-
    term_vars.terms_contain_var(Terms, Var).

%---------------------------------------------------------------------------%

var_supply_max_var(var_supply(V)) = var(V).

var_supply_num_allocated(var_supply(V)) = V.

force_construct_var(V) = var(V).

force_construct_var_supply(V) = var_supply(V).

%---------------------------------------------------------------------------%
:- end_module term.
%---------------------------------------------------------------------------%
