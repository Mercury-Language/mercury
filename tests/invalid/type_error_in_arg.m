%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 expandtab ft=mercury
%---------------------------------------------------------------------------%

:- module type_error_in_arg.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module maybe.

main(!IO) :-
    p(42, X),
    io.write(X, !IO),
    io.nl(!IO).

:- type t
    --->    t(
                f1          ::  bool,
                f2          ::  maybe(int),
                f3          ::  cord(int)
            ).

:- pred p(int::in, t::out) is det.

p(N, X) :-
    A = no,
    B = yes(N),
    C = [],
    X = t(
        A,
        B,
        C
    ).
