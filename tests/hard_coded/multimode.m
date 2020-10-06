%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multimode.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- pragma promise_pure(main/2).
main(!IO) :-
    In = 42,
    In2 = In,       % this line (and the use of `In2' below,
                    % rather than `In') is needed to avoid
                    % triggering an unrelated bug -- see
                    % tests/valid/mode_selection.m.

    % test pure functions
    print_line(func0, !IO),
    print_line(func1(In), !IO),
    print_line(func1(_Out0), !IO),
    print_line(func2(In, In2), !IO),
    print_line(func2(In, _Out1), !IO),
    print_line(func2(_Out2, In), !IO),
    print_line(func2(_Out3, _Out4), !IO),

    % test impure predicates
    impure test0,
    impure test1(In),
    impure test1(_Out10),
    impure test2(In, In),
    impure test2(In, _Out11),
    impure test2(_Out12, In),
    impure test2(_Out13, _Out14).

:- func func0 = string.
:- mode func0 = out is det.
func0 = ("func0 = out" :: out).

:- pragma promise_pure(func1/1). % XXX technically this is a lie
:- func func1(int) = string.
:- mode func1(in) = out is det.
:- mode func1(out) = out is det.
func1(_::in) = ("func1(in) = out"::out).
func1(0::out) = ("func1(out) = out"::out).

:- pragma promise_pure(func2/2). % XXX technically this is a lie
:- func func2(int, int) = string.
:- mode func2(in, in) = out is det.
:- mode func2(in, out) = out is det.
:- mode func2(out, in) = out is det.
:- mode func2(out, out) = out is det.
func2(_::in, _::in) = (R::out) :-
    R = "func2(in, in) = out".
func2(_::in, 0::out) = (R::out) :-
    R = "func2(in, out) = out".
func2(0::out, _::in) = (R::out) :-
    R = "func2(out, in) = out".
func2(0::out, 0::out) = (R::out) :-
    R = "func2(out, out) = out".

:- impure pred test0.
:- mode test0 is det.
test0 :-
    impure puts("test0").

:- impure pred test1(int).
:- mode test1(in) is det.
:- mode test1(out) is det.
test1(_::in) :-
    impure puts("test1(in)").
test1(0::out) :-
    impure puts("test1(out)").

:- impure pred test2(int, int).
:- mode test2(in, in) is det.
:- mode test2(in, out) is det.
:- mode test2(out, in) is det.
:- mode test2(out, out) is det.
test2(_::in, _::in) :-
    impure puts("test2(in, in)").
test2(_::in, 0::out) :-
    impure puts("test2(in, out)").
test2(0::out, _::in) :-
    impure puts("test2(out, in)").
test2(0::out, 0::out) :-
    impure puts("test2(out, out)").

:- impure pred puts(string::in) is det.
:- pragma foreign_proc("C",
    puts(S::in),
    [will_not_call_mercury],
"
    puts(S)
").
:- pragma foreign_proc("C#",
    puts(S::in),
    [will_not_call_mercury],
"
    System.Console.WriteLine(S);
").
:- pragma foreign_proc("Java",
    puts(S::in),
    [will_not_call_mercury],
"
    System.out.println(S);
").
:- pragma foreign_proc("Erlang",
    puts(S::in),
    [will_not_call_mercury],
"
    io:put_chars(S),
    io:nl()
").
