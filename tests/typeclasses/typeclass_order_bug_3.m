%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests class methods with type variables which are
% not constrained in the head of the `:- typeclass' declaration.
%
% The code generated by the compiler of 13/1/2001 for this test case segfaults.
% The type_infos and typeclass_infos were being passed in the wrong order.

:- module typeclass_order_bug_3.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

:- typeclass class(T) where [
    pred p(U::in, V::in, T::in, io::di, io::uo) is det <= writeable(U)
].

:- typeclass writeable(T) where [
    pred write_t(T::in, io::di, io::uo) is det
].

:- instance class(list(T)) <= class(T) where [
    ( p(U, V, List) -->
        list.foldl(p(U, V), List),
        write_t(U),
        io.nl,
        io.write(V),
        io.nl
    )
].

:- instance class(int) where [
    ( p(_, _, Int) -->
        io.write_int(Int + 1),
        io.nl
    )
].

:- instance writeable(string) where [
    write_t(String) -->
        io.write_string(String)
].

main(!IO) :-
    p("string", "string2", [1, 2, 3], !IO).