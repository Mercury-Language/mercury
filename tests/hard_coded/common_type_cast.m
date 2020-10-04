%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test for the case where common.m must insert a type cast
% because two equivalent cells have different types.

:- module common_type_cast.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- type foo(T)
    --->    ok(T)
    ;       error(string).

:- implementation.

:- import_module int.
:- import_module float.

main(!IO) :-
    Pred =
        ( pred(Int::in, Float::out) is det :-
            Float = float(Int)
        ),
    test(Pred, error("error"), Output1),
    test(Pred, ok(1), Output2),
    io.write_line(Output1, !IO),
    io.write_line(Output2, !IO).

:- pred test(pred(T, U), foo(T), foo(U)).
:- mode test(pred(in, out) is det, in, out) is det.

test(P, ok(T), ok(U)) :-
    call(P, T, U).
test(_, error(Err), error(Err)).
