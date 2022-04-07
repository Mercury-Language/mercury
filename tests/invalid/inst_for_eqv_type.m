%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A new test case to show that we report inst definitions that specify
% they are for a user-defined equivalence type.
%

:- module inst_for_eqv_type.
:- interface.

:- type et == int.

:- inst one_two for et/0
    --->    1
    ;       2.

:- pred test_12(et::in(one_two), string::out) is det.

%---------------------------------------------------------------------------%
:- implementation.
%---------------------------------------------------------------------------%

test_12(ET, Str) :-
    ( ET = 1, Str = "1"
    ; ET = 2, Str = "2"
    ).
