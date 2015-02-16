% Test that the state variable transformation works on an atomic goal that
% doesn't mention the inner state variable (e.g. try_put_mvar).

:- module atomic_mvar.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module maybe.
:- import_module stm_builtin.

:- type mvar(T)
    --->    mvar(
                stm_var(maybe(T))
            ).

%---------------------------------------------------------------------------%

main(!IO) :-
    new_mvar(MVar, !IO),
    atomic [outer(!IO), inner(!STM)] (
        put_mvar(MVar, "one", !STM)
    ),
    atomic [outer(!IO), inner(!STM)] (
        try_put_mvar(MVar, "two", Success, !STM)
    ),
    io.write(Success, !IO),
    io.nl(!IO).

:- pred new_mvar(mvar(T)::out, io::di, io::uo) is det.

new_mvar(mvar(TVar), !IO) :-
    new_stm_var(no, TVar, !IO).

:- pred take_mvar(mvar(T)::in, T::out, stm::di, stm::uo) is det.

take_mvar(mvar(TVar), T, !STM) :-
    read_stm_var(TVar, Maybe, !STM),
    (
        Maybe = yes(T),
        write_stm_var(TVar, no, !STM)
    ;
        Maybe = no,
        retry(!.STM)
    ).

:- pred put_mvar(mvar(T)::in, T::in, stm::di, stm::uo) is det.

put_mvar(mvar(TVar), T, !STM) :-
    read_stm_var(TVar, Maybe, !STM),
    (
        Maybe = yes(_),
        retry(!.STM)
    ;
        Maybe = no,
        write_stm_var(TVar, yes(T), !STM)
    ).

:- pred try_put_mvar(mvar(T)::in, T::in, bool::out, stm::di, stm::uo) is cc_multi.

try_put_mvar(MVar, T, Success, !STM) :-
    atomic [outer(!STM), inner(!STM1)] (
        put_mvar(MVar, T, !STM1),
        Success = yes
    or_else
        % !STM1 not mentioned.
        Success = no
    ).

:- pred nonsense(io::di, io::uo) is det.

nonsense(!IO) :-
    atomic [outer(!IO), inner(!STM)] (
        % !STM not mentioned
        true
    ).

%---------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
