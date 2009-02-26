%
% Basic test of or_else attached to a nested transaction
%

:- module nested_or_else.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module stm_builtin.

:- pred det_retry(int::in, stm::ui) is det.

det_retry(X, Stm) :-
    trace [io(!IO)] (
        write(X, !IO), nl(!IO)
    ),
    retry(Stm).

%------------------------------------------------------------------------------%

main(IO0, IO) :-
    atomic [outer(IO0, IO1), inner(STM0, STM1)] (
        atomic [outer(STM0, STM1), inner(INNER0, INNER1)] (
            X = 1,
            INNER0 = INNER1,
            det_retry(X, INNER1)
        or_else
            X = 2,
            INNER0 = INNER1,
            det_retry(X, INNER1)
        ),
        det_retry(X, STM1)
    or_else
        X = 3,
        STM0 = STM1
    ),

    write(X, IO1, IO2),
    nl(IO2, IO).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
