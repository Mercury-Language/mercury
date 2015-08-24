%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module state_vars_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO).

:- pred p(int::out) is det.

    % Warning about reference to "uninitialized" !.X.
    %
p(!:X) :-
    !.X = 1.
