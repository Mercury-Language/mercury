% From orig/stm-compiler/test9
%
% Basic test of or_else: that it gets executed when the atomic scope it is
% attached to retries, and that it is able to bind a variable that was
% bound in the atomic scope before the retry.
%

:- module atomic_or_else.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module stm_builtin.

:- pred det_retry(stm::ui) is det.

det_retry(Stm) :-
    retry(Stm).

%------------------------------------------------------------------------------%

main(IO0, IO) :-
    atomic [outer(IO0, IO1), inner(STM0, STM1)] (
        X = 1,
        STM0 = STM1,
        det_retry(STM1)
    or_else
        STM0 = STM1,
        X = 5
    ),

    write(X, IO1, IO2),
    nl(IO2, IO).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
