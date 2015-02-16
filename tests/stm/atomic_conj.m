%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Two atomic scopes in a conjunction.

:- module atomic_conj.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module stm_builtin.

%---------------------------------------------------------------------------%

main(IO0, IO) :-
    new_stm_var(1, Var, IO0, IO1),
    atomic [outer(IO1, IO2), inner(STM0, STM2)] (
        read_stm_var(Var, A, STM0, STM1),
        write_stm_var(Var, A + 1, STM1, STM2)
    ),
    atomic [outer(IO2, IO3), inner(STM3, STM)] (
        read_stm_var(Var, B, STM3, STM)
    ),

    io.write_int(A, IO3, IO4),
    io.nl(IO4, IO5),
    io.write_int(B, IO5, IO6),
    io.nl(IO6, IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

