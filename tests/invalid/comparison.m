%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module comparison.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    ( if test(1.0, 2.0) then
        io.write_string("ok\n", !IO)
    else
        io.write_string("not ok\n", !IO)
    ).

:- pred test(float::in, float::in) is semidet.

test(A, B) :-
    A < B.
