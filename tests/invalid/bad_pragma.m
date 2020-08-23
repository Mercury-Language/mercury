%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_pragma.

:- interface.

:- pred foo1(int::in, int::out) is det.
:- pred foo2(int::in, int::out) is det.
:- pred foo3(int::in, int::out) is det.
:- pred foo4(int::in, int::out) is det.

:- implementation.

:- pragma memo(foo1/2, [xyzzy, abracadabra]).

foo1(N, N).

:- pragma minimal_model(foo2/2,
    [fast_loose,
    size_limit(42),
    disable_warning_if_ignored]).

foo2(N, N).

:- pragma loop_check(foo3/2,
    [allow_reset,
    statistics,
    statistics,
    allow_reset]).

foo3(N, N).

:- pragma memo(foo4/2,
    [size_limit(12.3)]).

foo4(N, N).
