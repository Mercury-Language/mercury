:- module atomic_scope.
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
        foo(Var, !STM)
    ).

:- pred foo(stm_var(int)::in, stm::di, stm::uo) is det.

foo(Var, !IO) :-
    atomic [outer(!IO), inner(!STM)] (
        read_stm_var(Var, X, !STM)
    ),
    ( X = 1 ->
        atomic [outer(!IO), inner(!STM)] (
            write_stm_var(Var, 2, !STM)
        )
    ;
        % Unification for !:STM = !.STM was being added here.
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
