%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

% This file contains a bunch of test cases, all of which should result
% in errors of one kind or another.

:- module errors1.

%---------------------------------------------------------------------------%

:- pred pred_with_no_clauses_or_mode_declaration(int).

%---------------------------------------------------------------------------%

:- pred pred_with_no_clauses(int).
:- mode pred_with_no_clauses(input).

%---------------------------------------------------------------------------%

/*
:- pred pred_with_invalid_goal.

pred_with_invalid_goal :-
    5.
*/

%---------------------------------------------------------------------------%

:- mode mode_declaration_without_pred_declaration.

%---------------------------------------------------------------------------%

:- mode missing_pred_declaration.

missing_pred_declaration.

%---------------------------------------------------------------------------%

clause_without_pred_or_mode_declaration.

%---------------------------------------------------------------------------%

:- pred pred_which_calls_non_existant_pred.

pred_which_calls_non_existant_pred :-
    non_existant_pred.

%---------------------------------------------------------------------------%

:- type type_with_multiply_defined_ctors
    --->    a
    ;       a
    ;       f(int)
    ;       f(int).

:- type du_type_which_references_undefined_type
    --->    f(undefined_type).

:- type eqv_type_which_references_undefined_type == undefined_type.

:- type circular_eqv_type == circular_eqv_type.

:- type indirectly_circular_eqv_type_1 == indirectly_circular_eqv_type_2.
:- type indirectly_circular_eqv_type_2 == indirectly_circular_eqv_type_1.

%---------------------------------------------------------------------------%

:- pred pred_which_binds_type_param(TypeParam::input).

pred_which_binds_type_param(Argument) :-
    Argument = 0.

%---------------------------------------------------------------------------%

:- pred pred_with_unresolved_polymorphism.

pred_with_unresolved_polymorphism :-
    pred_which_binds_type_param(Arg).

%---------------------------------------------------------------------------%

:- pred pred_producing_string(string).

:- pred pred_expecting_int(int).

:- pred pred_with_type_error.

pred_with_type_error :-
    pred_producing_string(X),
    pred_expecting_int(X).

:- pred pred_with_singleton_vars(T).

pred_with_singleton_vars(X).

%---------------------------------------------------------------------------%
