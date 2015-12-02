%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module warn_dead_procs_trace.
:- interface.
:- pred p(int::in, int::out) is det.
:- implementation.

:- import_module int.
:- import_module require.

p(A, Z) :-
    trace [compiletime(flag("not_given"))] (
        ( if q(A) then
            true
        else
            unexpected($pred, "not q")
        )
    ),
    r(A, Z).

    % This procedure has one call to it which is deleted when simplification
    % deletes the trace goal scope it is in. Dead proc elimination may
    % therefore delete this procedure, but the fact that the source code
    % contains a call to it means that we should NOT generate a warning for it.
    %
:- pred q(int::in) is semidet.

q(A) :-
    A < 10.

:- pred r(int::in, int::out) is det.

r(A, Z) :-
    Z = A + 1.

    % Totally unused procedure.
    %
:- pred t(int::in, int::out) is det.

t(A, Z) :-
    Z = A + 1.
