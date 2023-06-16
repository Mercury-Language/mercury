%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multimode_missing_impure.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    In = 42,
    test0,
    test1(In),
    test1(_Out0),
    test2(In, In),
    test2(In, _Out1),
    test2(_Out2, In),
    test2(_Out3, _Out4).

:- pred test0.
:- mode test0 is det.
test0 :-
    puts("test0").

% This should be declared impure, or promised pure.
:- pred test1(int).
:- mode test1(in) is det.
:- mode test1(out) is det.
test1(_::in) :-
    puts("test1(in)").
test1(0::out) :-
    puts("test1(out)").

% This should be declared impure, or promised pure.
:- pred test2(int, int).
:- mode test2(in, in) is det.
:- mode test2(in, out) is det.
:- mode test2(out, in) is det.
:- mode test2(out, out) is det.
test2(_::in, _::in) :-
    puts("test2(in, in)").
test2(_::in, 0::out) :-
    puts("test2(in, out)").
test2(0::out, _::in) :-
    puts("test2(out, in)").
test2(0::out, 0::out) :-
    puts("test2(out, out)").

:- pred puts(string::in) is det.
puts(_).
:- pragma foreign_proc("C",
    puts(S::in),
    [promise_pure, will_not_call_mercury],
"
    puts(S);
").
