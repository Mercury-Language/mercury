% From orig/stm-compiler/test7
%
% Most basic test of nested transactions: simply that they compile.
%

:- module nested.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module stm_builtin.

%------------------------------------------------------------------------------%

main(IO0, IO) :-
    atomic [outer(IO0, IO), inner(STM0, STM)] (
        atomic [outer(STM0, STM), inner(INNER_STM0, INNER_STM)] (
            INNER_STM0 = INNER_STM
        ),
        % There is a bug that prevents a nested transaction being used
        % as the goal directly inside a transaction; adding the line below
        % makes it a conjunction instead.
        % In future I hope to remove this by optimizing the outer transaction
        % away. -- Ben Mellor
        X = 1
    ).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
