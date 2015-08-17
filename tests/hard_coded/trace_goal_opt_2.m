%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module trace_goal_opt_2.
:- interface.

:- pred require_lt(int::in, int::in) is det.

:- implementation.

:- import_module exception.
:- import_module int.

:- mutable(x, int, 0, ground, [untrailed]).

:- pragma inline(require_lt/2).

require_lt(A, B):-
    trace [
        compiletime(flag("abc") or flag("\"xyz\"") and grade(debug)),
        runtime(env("TRACE_ABC")),
        state(x, !X)
        % io(!IO) prevents opt-exporting?
    ] (
        require_lt0(A, B)
    ).

:- pred require_lt0(int::in, int::in) is det.

require_lt0(A, B):-
    ( if A < B then
        true
    else
        throw("require_lt0")
    ).
