%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Uncaught exception:
% Software Error: map__lookup: key not found
% Key Type: prog_data:class_constraint
% Key Functor: constraint/2
% Value Type: hlds_data:constraint_proof

:- module superclass_bug2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    foobar(t1(1)),
    io.write_string("Hi.\n", !IO).

:- typeclass g(T) where [ ].
:- typeclass h(T) <= g(T) where [ ].

% Bottom level instance.

:- instance g(int) where [ ].

:- type t1(S)
    --->    t1(S).

:- instance g(t1(S)) <= g(S) where [ ].
:- instance h(t1(S)) <= g(S) where [ ].

:- pred foobar(T) <= h(T).
:- mode foobar(in) is det.

foobar(_).

%---------------------------------------------------------------------------%
