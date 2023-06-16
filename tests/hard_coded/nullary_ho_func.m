%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for use of zero-arity higher-order function terms.
%
% Author: fjh

:- module nullary_ho_func.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module std_util.
:- import_module type_desc.

:- type nullary_func(T) == ((func) = T).
:- inst nullary_func == ((func) = out is det).

:- func apply_nullary_func(nullary_func(T)) = T.
:- mode apply_nullary_func(in(nullary_func)) = out is det.

apply_nullary_func(F) = apply(F).

:- func apply_func((func) = T) = T.
:- mode apply_func((func) = out is semidet) = out is semidet.
:- mode apply_func((func) = out is det) = out is det.

apply_func(F) = apply(F).

main(!IO) :-
    F = ((func) = 42),
    X = apply(F),
    G = ((func) = (_ :: out) is semidet :- fail),
    H = ((func) = (R :: out) is semidet :- semidet_succeed, R = X),
    io.print("X = ", !IO), io.print_line(X, !IO),
    io.print("apply(F) = ", !IO), io.print_line(X, !IO),
    io.print("apply_func(F) = ", !IO), io.print_line(X, !IO),
    io.print("apply_nullary_func(F) = ", !IO), io.print_line(X, !IO),
    ( if Y = apply(G) then
        io.print("Y = ", !IO), io.print_line(Y, !IO)
    else
        io.print_line("Y = apply(G) failed", !IO)
    ),
    ( if Z = apply(H) then
        io.print("Z = ", !IO), io.print_line(Z, !IO)
    else
        io.print_line("Y = apply(G) failed", !IO)
    ),
    print("type_of(F) = ", !IO),
    print_line(type_of(F), !IO),
    print("type_name(type_of(F)) = ", !IO),
    print_line(type_name(type_of(F)), !IO).
