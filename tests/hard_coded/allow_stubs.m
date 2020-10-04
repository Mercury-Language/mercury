%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% test case for the `--allow-stubs' option.
%

:- module allow_stubs.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.
:- import_module univ.

main(!IO) :-
    hello(!IO),
    trap_exceptions(how_are_you, !IO),
    trap_exceptions(going_today, !IO),
    goodbye(!IO).

hello(!IO) :-
    print("hello world", !IO),
    nl(!IO).

:- pred goodbye(io::di, io::uo) is det.

goodbye -->
    print("goodbye"), nl.

:- pred how_are_you(io::di, io::uo) is det.

:- mode going_today(di, uo) is det.

going_today(!IO) :-
    print("going ", !IO),
    today(!IO).

:- pred today(io::di, io::uo) is det.

:- pred unused1(T::di, T::uo) is det.
:- pred unused2(T::di, T::uo) is det.

unused1(IO0, IO) :-
    unused2(IO0, IO).

:- impure pred imp(io::di, io::uo) is det.

:- mode trap_exceptions(pred(di, uo) is det, di, uo) is cc_multi.

trap_exceptions(IOGoal, !IO) :-
    try_io((pred({}::out, di, uo) is det --> IOGoal), Res, !IO),
    (
        Res = succeeded({})
    ;
        Res = exception(Exception),
        print("[caught exception: ", !IO),
        print(univ_value(Exception), !IO),
        print("]\n", !IO)
    ).
