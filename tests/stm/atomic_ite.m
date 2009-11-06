:- module atomic_ite.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module stm_builtin.

%-----------------------------------------------------------------------------%

main(!IO) :-
    new_stm_var(10, Var, !IO),
    atomic [outer(!IO), inner(!STM)] (
        countdown(Var, !STM)
    ).

:- pred countdown(stm_var(int)::in, stm::di, stm::uo) is det.

countdown(Var, !IO) :-
    atomic [outer(!IO), inner(!STM)] (
        read_stm_var(Var, X, !STM),
        ( X < 0 ->
            retry(!.STM)
        ;
            trace [io(!IO)] (
                io.write_int(X, !IO),
                io.nl(!IO)
            ),
            write_stm_var(Var, X - 1, !STM)
        )
    ),
    ( X = 0 ->
        true
    ;
        countdown(Var, !IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
