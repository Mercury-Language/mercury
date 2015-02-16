%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module det_condition.

:- interface.

    % Check that we get determinism analysis and code generation right
    % for code which has an if-then-else with a deterministic condition.
    %
:- pred p is det.

:- implementation.

p :-
    if true then true else not true.
