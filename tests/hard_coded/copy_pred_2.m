%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module copy_pred_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module deconstruct.
:- import_module list.
:- import_module map.
:- import_module string.

main(!IO) :-
    make_closure(10, 20, P0),
    io.write_string("copying\n", !IO),
    copy(P0, P1),
    inst_cast(P1, P),
    io.write_string("calling\n", !IO),
    P("blah", S),
    io.write_string("printing\n", !IO),
    io.print_line(S, !IO).

:- pred make_closure(T, T, pred(string, string)).
:- mode make_closure(in, in, out(pred(in, out) is det)) is det.
:- pragma no_inline(make_closure/3).

make_closure(A, B, foo(A, B)).

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

:- pred foo(T, T, string, string).
:- mode foo(in, in, in, out) is det.

foo(A, B, S0, S) :-
    functor(A, canonicalize, FA, _),
    functor(B, canonicalize, FB, _),
    string.format("%s, %s, %s", [s(FA), s(FB), s(S0)], S).
