%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test an optimisation to the subterm dependency tracking where
% the tracking algorithm checks to see if the subterm it is tracking in
% an output argument appears in the same position in an input argument.  If
% it doesm then the algorithm can continue tracking the subterm in the
% input and needn't generated any descendent nodes.
% The algorithm checks only input variables that differ from the output
% variable by a numerical suffix.
%

:- module nodescend_tracking.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

:- type t
    --->    reverse(int, int)
    ;       leave(int, int).

main(!IO) :-
    run_test(List, !IO),
    io.write_int(list.length(List), !IO),
    io.nl(!IO).

:- pred run_test(list(t)::out, io::di, io::uo) is det.

run_test(List, !IO) :-
    make_test_list(10000) = List0,
    change(List0, List, !IO).

:- pred change(list(t)::in, list(t)::out, io::di, io::uo) is det.

change(!List, !IO) :-
    (
        !.List = []
    ;
        !.List = [Head | Tail],
        change(Tail, Rest, !IO),
        (
            Head = reverse(X, Y),
            !:List = [reverse(Y, X) | Rest]
        ;
            Head = leave(_, _),
            % The test case will track the leave/2 term, so if the
            % optimisation works, then this code should not be reexecuted
            % by the debugger when tracking the term.
            untabled_print(!IO),
            !:List = [Head | Rest]
        )
    ).

:- func make_test_list(int) = list(t).

make_test_list(NumElements) = List :-
    ( if NumElements =< 1 then
        List = [leave(1, 2)]
    else
        List = [reverse(1, 2) | make_test_list(NumElements - 1)]
    ).

:- pred untabled_print(io::di, io::uo) is det.

    % untabled_print is intentionally not tabled.
    % We use it to check when a piece of code is reexecuted by the debugger.
    %
:- pragma foreign_proc("C",
    untabled_print(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    printf(\"\\n*** called untabled_print ***\\n\");
    IO = IO0;
").
