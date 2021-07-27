%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module typeclass_test_5.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- typeclass c1(T) where [].
:- typeclass c2(T) <= c1(T) where [
    pred p(T), mode p(out) is det
].

:- pred get_int(int::out) is det.
:- pred get_int2(int::out) is det.

:- implementation.

:- instance c2(int) where [
    pred(p/1) is get_int,
    pred(p/1) is get_int2
].

main(!IO).

get_int(42).
get_int2(43).
