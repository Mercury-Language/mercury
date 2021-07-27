%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
% The compiler aborted on this module with --trace shallow due to an incorrect
% fix.
%
%---------------------------------------------------------------------------%

:- module livevars_shallow2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- type ps
    --->    ps(int).

%---------------------------------------------------------------------------%

main(!IO) :-
    P = stringify_state(int_with_state),
    io.write_line(P, !IO).

:- pred int_with_state(int::out, ps::in, ps::out) is semidet.

int_with_state(1, !PS) :-
    semidet_true.

:- pred stringify_state(pred(T, ps, ps)::in(pred(out, in, out) is semidet),
    string::out, ps::in, ps::out) is semidet.

stringify_state(P, String, !PS) :-
    P(_, !PS),
    String = xfoo(-1).

:- func xfoo(T) = string.

xfoo(_) = "xfoo".
