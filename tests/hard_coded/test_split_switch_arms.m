%-----------------------------------------------------------------------------% % vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% This test case exercises the code of compiler/split_switch_arms.
%

:- module test_split_switch_arms.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

:- type t
    --->    f0
    ;       f1
    ;       f2
    ;       f3
    ;       f4
    ;       f5(int)
    ;       f6(int)
    ;       f7(int)
    ;       f8(int)
    ;       f9(int).

main(!IO) :-
    List = [f0, f1, f2, f3, f4, f5(50), f6(60), f7(70), f8(80), f9(90)],
    run_tests(List, !IO).

:- pred run_tests(list(t)::in, io::di, io::uo) is det.

run_tests([], !IO).
run_tests([T | Ts], !IO) :-
    test(T, f5(5000), Str5),
    io.format("test(%s, f5(5000), \"%s\")\n",
        [s(string.string(T)), s(Str5)], !IO),
    test(T, f6(6000), Str6),
    io.format("test(%s, f6(6000), \"%s\")\n",
        [s(string.string(T)), s(Str6)], !IO),
    run_tests(Ts, !IO).

:- pred test(t::in, t::in, string::out) is det.

test(T, U, Str) :-
    (
        ( T = f0
        ; T = f1
        ; T = f2
        ; T = f3
        ; T = f4
        ; T = f5(_)
        ),
        Str0 = "a",
        (
            ( T = f0
            ; T = f1
            ; T = f2
            ),
            Str1 = "a",
            (
                ( T = f0
                ; T = f1
                ),
                string.format("%s %s <%s>",
                    [s(Str0), s(Str1), s(string.string(T))], Str)
            ;
                T = f2,
                string.format("%s %s [%s]",
                    [s(Str0), s(Str1), s(string.string(T))], Str)
            )
        ;
            ( T = f3
            ; T = f4
            ; T = f5(_)
            ),
            Str1 = "c",
            (
                ( T = f3
                ; T = f4
                ),
                string.format("%s %s {%s}",
                    [s(Str0), s(Str1), s(string.string(T))], Str)
            ;
                ( T = f5(N)
                % ; T = f6(N)   % This would get a "cannot succeed" error.
                ),
                string.format("%s %s (%s) %d",
                    [s(Str0), s(Str1), s(string.string(T)), i(N)], Str)
            )
        )
    ;
        ( T = f6(_)
        ; T = f7(_)
        ; T = f8(_)
        ; T = f9(_)
        ),
        Str0 = "d",
        (
            ( U = f0
            ; U = f1
            ; U = f2
            ; U = f3
            ; U = f4
            ; U = f5(_)
            ),
            Str1 = "e",
            (
                ( T = f6(TN)
                ; T = f7(TN)
                ),
                string.format("%s %s /%d/",
                    [s(Str0), s(Str1), i(TN)], Str)
            ;
                ( T = f8(TN)
                ; T = f9(TN)
                ),
                string.format("%s %s !%d!",
                    [s(Str0), s(Str1), i(TN)], Str)
            )
        ;
            ( U = f6(UN)
            ; U = f7(UN)
            ; U = f8(UN)
            ; U = f9(UN)
            ),
            Str1 = "f",
            (
                ( T = f6(TN)
                ; T = f7(TN)
                ),
                string.format("%s %s *%d*",
                    [s(Str0), s(Str1), i(UN + TN)], Str)
            ;
                ( T = f8(TN)
                ; T = f9(TN)
                ),
                string.format("%s %s -%d-",
                    [s(Str0), s(Str1), i(UN + TN)], Str)
            )
        )
    ).
