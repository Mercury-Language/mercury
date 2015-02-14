%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module copy_pred.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.
:- import_module map.
:- import_module string.
:- import_module univ.

:- type phloat == float.

main -->
    { F = foo(10, 20.2, 30, 40.4) },
    { copy(F, F2) },
    io__set_globals(univ(F2)),
    io__get_globals(Univ),
    { det_univ_to_type(Univ, F3) },
    { inst_cast(F3, F4) },
    { F4("blah", S) },
    print(S), nl.

:- pred inst_cast(pred(string, string), pred(string, string)).
:- mode inst_cast(in, out(pred(in, out) is det)) is det.

:- pragma foreign_proc("C",
    inst_cast(X::in, Y::out(pred(in, out) is det)),
    [will_not_call_mercury, thread_safe, promise_pure], "Y = X").
:- pragma foreign_proc("C#",
    inst_cast(X::in, Y::out(pred(in, out) is det)),
    [will_not_call_mercury, thread_safe, promise_pure], "Y = X;").
:- pragma foreign_proc("Java",
    inst_cast(X::in, Y::out(pred(in, out) is det)),
    [will_not_call_mercury, thread_safe, promise_pure], "Y = X;").
:- pragma foreign_proc("Erlang",
    inst_cast(X::in, Y::out(pred(in, out) is det)),
    [will_not_call_mercury, thread_safe, promise_pure], "Y = X").

:- pred foo(int, float, int, phloat, string, string) is det.
:- mode foo(in, in, in, in, in, out) is det.

foo(A, B, C, D, S0, S) :-
    string__format("%d, %g, %d, %g, %s", [i(A), f(B), i(C), f(D), s(S0)], S).
