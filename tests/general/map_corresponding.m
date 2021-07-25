%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% map_corresponding.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Thu Dec 13 14:36:55 EST 2001
%
%---------------------------------------------------------------------------%

:- module map_corresponding.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module pprint.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_test2, solutions(test_case2), !IO),
    io.nl(!IO),
    list.foldl(run_test3, solutions(test_case3), !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred run_test2({bool, list(int), list(int), list(int)}::in,
    io::di, io::uo) is cc_multi.

run_test2({ShouldSucceed, As, Bs, Cs}, !IO) :-
    P =
        ( pred(Zs::out) is det :-
            Zs = list.map_corresponding(sum2, As, Bs)
        ),
    try(P, ExceptionResult),
    Inputs = to_doc(As) `<>` space `<>` to_doc(Bs),
    (
        ExceptionResult = succeeded(Ws),
        (
            ShouldSucceed = yes,
            Result =
                ( if Ws = Cs then
                    text("succeeded correctly on inputs ") `<>` Inputs
                else
                    text("*** succeeded incorrectly on inputs ") `<>` Inputs
                )
        ;
            ShouldSucceed = no,
            Result =
                text("*** succeeded unexpectedly on inputs ") `<>` Inputs `<>`
                text(" with result ") `<>` to_doc(Ws)
        )
    ;
        ExceptionResult = exception(_),
        (
            ShouldSucceed = yes,
            Result =
                text("*** failed unexpectedly on inputs ") `<>` Inputs
        ;
            ShouldSucceed = no,
            Result =
                text("failed as expected on inputs ") `<>` Inputs
        )
    ),
    io.print("\nlist__map_corresponding/3: ", !IO),
    pprint.write(80, Result, !IO).

%---------------------------------------------------------------------------%

:- func sum2(int, int) = int.

sum2(X, Y) = X + Y.

%---------------------------------------------------------------------------%

:- pred run_test3({bool, list(int), list(int), list(int), list(int)}::in,
    io::di, io::uo) is cc_multi.

run_test3({ShouldSucceed, As, Bs, Cs, Ds}, !IO) :-
    P =
        ( pred(Zs::out) is det :-
            Zs = list.map_corresponding3(sum3, As, Bs, Cs)
        ),
    try(P, ExceptionResult),
    Inputs = to_doc(As) `<>` space `<>` to_doc(Bs) `<>` space `<>` to_doc(Cs),
    (
        ExceptionResult = succeeded(Ws),
        (
            ShouldSucceed = yes,
            Result =
                ( if Ws = Ds then
                    text("succeeded correctly on inputs ") `<>` Inputs
                else
                    text("*** succeeded incorrectly on inputs ") `<>` Inputs
                )
        ;
            ShouldSucceed = no,
            Result =
                text("*** succeeded unexpectedly on inputs ") `<>` Inputs `<>`
                text(" with result ") `<>` to_doc(Ws)
        )
    ;
        ExceptionResult = exception(_),
        (
            ShouldSucceed = yes,
            Result =
                text("*** failed unexpectedly on inputs ") `<>` Inputs
        ;
            ShouldSucceed = no,
            Result =
                text("failed as expected on inputs ") `<>` Inputs
        )
    ),
    io.print("\nlist__map_corresponding3/4: ", !IO),
    pprint.write(80, Result, !IO).

%---------------------------------------------------------------------------%

:- func sum3(int, int, int) = int.

sum3(X, Y, Z) = X + Y + Z.

%---------------------------------------------------------------------------%

:- pred test_case2({bool, list(int), list(int), list(int)}::out) is multi.

test_case2({yes, [], [], []}).
test_case2({yes, [1, 2, 3], [4, 5, 6], [5, 7, 9]}).
test_case2({no,  [], [1], []}).
test_case2({no,  [1], [], []}).

%---------------------------------------------------------------------------%

:- pred test_case3({bool, list(int), list(int), list(int), list(int)}::out)
    is multi.

test_case3({yes, [], [], [], []}).
test_case3({yes, [1, 2, 3], [4, 5, 6], [7, 8, 9], [12, 15, 18]}).
test_case3({no,  [], [1], [1], []}).
test_case3({no,  [1], [], [1], []}).
test_case3({no,  [], [], [1], []}).
test_case3({no,  [1], [1], [], []}).
test_case3({no,  [], [1], [], []}).
test_case3({no,  [1], [], [], []}).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
