%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module func_default_mode_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- type term
    --->    parameter(parameter)
    ;       value(value).

:- type bidon
    --->    bidon(term).

:- type value
    --->    value(float, float).

:- type parameter
    --->    parameter(string).

% We declare a typeclass for the constants used in the expressions.
% The user has to provide a null element for the mult operator
% and a null element for the plus operator.
% The user also has to provide a way to test whether term are null
% or not.
% Finally, the user has to provide a way of turning a minus operator
% to a plus. Typically, this is done by doing :
% A - B -> A + (B * -1)
% where the constant -1 is up to the user to define...

:- typeclass constant(C) where [
    func null_for_mult = C,

    pred is_null_for_mult(C),
    mode is_null_for_mult(in) is semidet,

    func null_for_plus = C,

    pred is_null_for_plus(C),
    mode is_null_for_plus(in) is semidet,

    func minus_one = C
].

%---------------------------------------------------------------------------%

:- implementation.

% Declares the predicates needed to handle the constants in
% the expressions.

:- instance constant(term) where [
    func(null_for_mult/0) is my_null_for_mult,
    pred(is_null_for_mult/1) is my_is_null_for_mult,
    func(null_for_plus/0) is my_null_for_plus,
    pred(is_null_for_plus/1) is my_is_null_for_plus,
    func(minus_one/0) is my_minus_one
].

    % This function generates a null element for the multiplication.
    %
:- func my_null_for_mult = term.
:- mode my_null_for_mult = out is det.

my_null_for_mult = value(definition_value(1.0)).

    % This predicate succeeds if the argument is the 1.0 value.
    %
:- pred my_is_null_for_mult(term).
:- mode my_is_null_for_mult(in) is semidet.

my_is_null_for_mult(value(definition_value(1.0))).

    % This function generates a null element for the addition.
    %
:- func my_null_for_plus = term.
:- mode my_null_for_plus = out is det.

my_null_for_plus = value(definition_value(0.0)).

    % This predicate succeeds if the argument is the 0.0 value.
    %
:- pred my_is_null_for_plus(term).
:- mode my_is_null_for_plus(in) is semidet.

my_is_null_for_plus(value(definition_value(0.0))).

    % This function generates a null element for the addition.
    %
:- func my_minus_one = term.
:- mode my_minus_one = out is det.

my_minus_one = value(definition_value(-1.0)).

    % This predicate is used to turn a float or a pair of floats
    % into the type value.
    %
:- func definition_value(float) = value.
:- mode definition_value(in) = out is det.

definition_value(X) = value(X, X).

:- func definition_value(float, float) = value.
:- mode definition_value(in, in) = out is det.

definition_value(X, Y) = value(X, Y).

main(In, Out) :-
    io.write(bidon(minus_one), In, Int3),
    io.nl(Int3, Out).
