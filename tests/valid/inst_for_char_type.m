%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A new test case to show that we *don't* report errors for inst definitions
% that specify they are for "char.char".
%

:- module inst_for_char_type.
:- interface.

:- import_module char.

:- type et == int.

:- inst ab for char/0
    --->    ('a')
    ;       ('b').

:- pred test_ab(char::in(ab), string::out) is det.

%---------------------------------------------------------------------------%
:- implementation.
%---------------------------------------------------------------------------%

test_ab(Char, Str) :-
    ( Char = 'a', Str = "a"
    ; Char = 'b', Str = "b"
    ).
