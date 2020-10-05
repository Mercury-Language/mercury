%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Small test of functions and predicates that have the same name.
%---------------------------------------------------------------------------%

:- module func_and_pred.

:- interface.

:- import_module list.
:- import_module io.

:- func concat(list(X), list(X)) = list(X).
:- mode concat(in, in) = out is det.

:- pred concat(list(X), list(X), list(X)).
:- mode concat(in, in, out) is det.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.

concat([], L) = L.
concat([E | R], L) = [E | concat(R, L)].

concat([], L, L).
concat([E | R], L, [E | Z]) :-
    concat(R, L, Z).

main(!IO) :-
    concat(['H', 'e'], ['l', 'l', 'o', ' '], Hello),
    World = concat(['w', 'o'], ['r', 'l', 'd', '!', '\n']),
    string.from_char_list(concat(Hello, World), HelloWorld),
    io.write_string(HelloWorld, !IO).

%-----------------------------
