%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module mode_choice.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- pragma foreign_decl("C", "
#include ""mercury_string.h""
").

main(!IO) :-
    ( if test1("foo", T0, T0b) then
        print("T0: ", !IO), print(T0, !IO), nl(!IO),
        print("T0b: ", !IO), print(T0b, !IO), nl(!IO)
    else
        print("test1: failed", !IO)
    ),
    ( if test1("foo", "fooie", T1) then
        print("T1: ", !IO), print(T1, !IO), nl(!IO)
    else
        print("test1: failed", !IO)
    ),
    test2("bar", T2),
    print("T2: ", !IO), print(T2, !IO), nl(!IO),
    Z = "z",
    test2(Z, T3),
    print("T3: ", !IO), print(T3, !IO), nl(!IO),
    test2(Z, T4),
    print("T4: ", !IO), print(T4, !IO), nl(!IO),
    mkany(Any),
    test3(Any, T5),
    print("T5: ", !IO), print(T5, !IO), nl(!IO),
    test3(_, T6),
    print("T6: ", !IO), print(T6, !IO), nl(!IO),
    ( if test4("", "", T7) then
        print("T7: ", !IO), print(T7, !IO), nl(!IO)
    else
        print("T7 failed\n", !IO)
    ),
    test4("", T8, T9),
    print("T8: ", !IO), print(T8, !IO), nl(!IO),
    print("T9: ", !IO), print(T9, !IO), nl(!IO),
    test4(T10, "", T11),
    print("T10: ", !IO), print(T10, !IO), nl(!IO),
    print("T11: ", !IO), print(T11, !IO), nl(!IO),
    test5("a", "b", T12),
    print("T12: ", !IO), print(T12, !IO), nl(!IO),
    test5("a", "a", T13),
    print("T13: ", !IO), print(T13, !IO), nl(!IO),
    test5("b", "b", T14),
    print("T14: ", !IO), print(T14, !IO), nl(!IO).

% prefer `in' to `out'

:- pred test1(string, string, string).
:- mode test1(in, out, out) is semidet.
:- mode test1(in, in, out) is semidet.

:- pragma promise_pure(test1/3).
:- pragma foreign_proc("C",
    test1(_A::in, B::out, C::out),
    [promise_pure, will_not_call_mercury],
"
    MR_make_aligned_string_copy(B, ""test1(in, out, out)"");
    C = B;
    SUCCESS_INDICATOR = MR_TRUE;
").
test1(_A::in, B::out, C::out) :-
    B = C,
    B = "test1(in, out, out)".

:- pragma foreign_proc("C",
    test1(_A::in, _B::in, C::out),
    [promise_pure, will_not_call_mercury],
"
    MR_make_aligned_string_copy(C, ""test1(in, in, out)"");
    SUCCESS_INDICATOR = MR_TRUE;
").
test1(_A::in, _B::in, C::out) :-
    C = "test1(in, in, out)".

% prefer `di' to `uo'

:- pred test2(string, string).
:- mode test2(in, out) is det.
:- mode test2(di, uo) is det.

:- pragma promise_pure(test2/2).
:- pragma foreign_proc("C",
    test2(_A::in, B::out),
    [promise_pure, will_not_call_mercury],
"
    MR_make_aligned_string_copy(B, ""test2(in, out)"");
").
test2(_A::in, B::out) :-
    B = "test2(in, out)".

:- pragma foreign_proc("C",
    test2(_A::di, B::uo),
    [promise_pure, will_not_call_mercury],
"
    MR_make_aligned_string_copy(B, ""test2(di, uo)"");
").
test2(_A::di, B::uo) :-
    B = "test2(di, uo)".

/******* `ui' modes not yet supported
% prefer `ui' to `in'

:- pred test2b(string, string).
:- mode test2b(in, out) is det.
:- mode test2b(ui, uo) is det.

:- pragma c_code(test2b(_A::in, B::out), will_not_call_mercury, "
    B = ""test2b(in, out)"";
").

:- pragma c_code(test2(_A::ui, B::out), will_not_call_mercury, "
    B = ""test2b(ui, out)"";
").
*******/

:- pred mkany(string::out(any)) is det.
:- pragma foreign_proc("C",
    mkany(S::out(any)),
    [promise_pure, will_not_call_mercury],
"
    S = NULL;
").
:- pragma foreign_proc("C#",
    mkany(S::out(any)),
    [promise_pure],
"
    S = null;
").
:- pragma foreign_proc("Java",
    mkany(S::out(any)),
    [promise_pure],
"
    S = null;
").

% prefer in(any) over out(any)
% [i.e. any -> any beats free -> any]

:- pred test3(string, string).
:- mode test3(in(any), out) is det.
:- mode test3(out(any), out) is det.

:- pragma promise_pure(test3/2).
:- pragma foreign_proc("C",
    test3(_A::in(any), B::out),
    [promise_pure, will_not_call_mercury],
"
    MR_make_aligned_string_copy(B, ""test3(in(any), out)"");
").
test3(_A::in(any), B::out) :-
    B = "test3(in(any), out)".

:- pragma foreign_proc("C",
    test3(A::out(any), B::out),
    [promise_pure,  will_not_call_mercury],
"
    A = NULL;
    MR_make_aligned_string_copy(B, ""test3(out(any), out)"");
").
test3(A::out(any), B::out) :-
    mkany(A),
    B = "test3(out(any), out)".

% for non-comparable modes, pick the first one

:- pred test4(string, string, string).
:- mode test4(in, out, out) is det.
:- mode test4(out, in, out) is det.

:- pragma promise_pure(test4/3).
:- pragma foreign_proc("C",
    test4(_A::in, B::out, C::out),
    [promise_pure, will_not_call_mercury],
"
    MR_make_aligned_string_copy(B, """");
    MR_make_aligned_string_copy(C, ""test4(in, out, out)"");
").
test4(_A::in, B::out, C::out) :-
    B = "",
    C = "test4(in, out, out)".

:- pragma foreign_proc("C",
    test4(A::out, _B::in, C::out),
    [promise_pure, will_not_call_mercury],
"
    MR_make_aligned_string_copy(A, """");
    MR_make_aligned_string_copy(C, ""test4(out, in, out)"");
").
test4(A::out, _B::in, C::out) :-
    A = "",
    C = "test4(out, in, out)".

% for non-comparable modes, pick the first one

:- inst a == bound("a").
:- inst b == bound("b").
:- inst ab == bound("a" ; "b").

:- pred test5(string, string, string).
:- mode test5(in(a), in(ab), out) is det.
:- mode test5(in(ab), in(b), out) is det.

:- pragma promise_pure(test5/3).
:- pragma foreign_proc("C",
    test5(_A::in(a), _B::in(ab), C::out),
    [promise_pure, will_not_call_mercury],
"
    MR_make_aligned_string_copy(C, ""test5(in(a), in(ab), out)"");
").
test5(_A::in(a), _B::in(ab), C::out) :-
    C = "test5(in(a), in(ab), out)".

:- pragma foreign_proc("C",
    test5(_A::in(ab), _B::in(b), C::out),
    [promise_pure, will_not_call_mercury],
"
    MR_make_aligned_string_copy(C, ""test5(in(ab), in(b), out)"");
").
test5(_A::in(ab), _B::in(b), C::out) :-
    C = "test5(in(ab), in(b), out)".
