%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% From orig/stm-compiler/test1
%
% About as basic as it gets. Just tests that an atomic scope that doesn't
% actually do any transacting is executed as normal.
%

:- module basic.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module stm_builtin.
:- import_module int.

main(IO0, IO) :-
    atomic [outer(IO0, IO1), inner(STM, STM)]
    (
        trace [io(!BLA)] (
            io.write_string("This is inside the atomic goal\n",
                !BLA)
        )
    ),
    io.write_string("Hello world\n", IO1, IO).
