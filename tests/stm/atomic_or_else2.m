%-----------------------------------------------------------------------------%

:- module atomic_or_else2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module stm_builtin.

%-----------------------------------------------------------------------------%

main(!IO) :-
    new_stm_var(0, TVar1, !IO),
    new_stm_var(1, TVar2, !IO),
    atomic [outer(!IO), inner(!STM)] (
        nonzero(TVar1, !STM),
        Msg = "TVar1 is non-zero"
    or_else
        nonzero(TVar2, !STM),
        Msg = "TVar2 is non-zero"
    ),
    io.write_string(Msg, !IO),
    io.nl(!IO).

:- pred nonzero(stm_var(int)::in, stm::di, stm::uo) is det.

nonzero(TVar, !STM) :-
    read_stm_var(TVar, X, !STM),
    ( X = 0 ->
        retry(!.STM)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
