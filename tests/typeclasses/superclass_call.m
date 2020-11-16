%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module superclass_call.

:- interface.

:- pred main(io::di, io::uo) is det.

:- import_module io.

:- implementation.

:- typeclass printable(A) where [
    pred p(A::in, io::di, io::uo) is det
].

:- typeclass foo(A) <= printable(A) where [
    pred b(A::in) is semidet
].

:- instance printable(int) where [
    pred(p/3) is io.write_int
].

:- instance foo(int) where [
    pred(b/1) is foo_b
].

main(!IO) :-
    p(42, !IO),
    io.write_string("\n", !IO),
    blah(101, !IO),
    io.write_string("\n", !IO).

:- pred foo_b(int::in) is semidet.

foo_b(1).

:- pred blah(T::in, io::di, io::uo) is det <= foo(T).

blah(X, !IO) :-
    ( if
        % This also tests the semidet class method call mechanism.
        b(X)
    then
        io.write_string("true\n", !IO)
    else
        io.write_string("false\n", !IO)
    ),

    % At this call to the superclass method, the printable typeclass_info
    % gets extracted from the foo typeclass_info.
    p(X, !IO).
