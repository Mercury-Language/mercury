%------------------------------------------------------------------------------%
% map_corresponding.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Thu Dec 13 14:36:55 EST 2001
% vim: ft=mercury ff=unix ts=4 sw=4 et tw=0 wm=0
%
%------------------------------------------------------------------------------%

:- module map_corresponding.

:- interface.

:- import_module io.



:- pred main(io::di, io::uo) is cc_multi.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module list, bool, int, string, pprint, std_util, exception.

%------------------------------------------------------------------------------%

main -->
    list__foldl(run_test2, solutions(test_case2)),
    io__nl,
    list__foldl(run_test3, solutions(test_case3)),
    io__nl.

%------------------------------------------------------------------------------%

:- pred run_test2({bool, list(int), list(int), list(int)}, io, io).
:- mode run_test2(in, di, uo) is cc_multi.

run_test2({ShouldSucceed, As, Bs, Cs}, IO0, IO) :-
    P = ( pred(Zs::out) is det :- Zs = list__map_corresponding(sum2, As, Bs)),
    try(P, ExceptionResult),
    Inputs = to_doc(As) `<>` space `<>` to_doc(Bs),
    (
        ExceptionResult = succeeded(Ws),
        (
            ShouldSucceed = yes,
            Result =
                ( if   Ws = Cs
                  then text("succeeded correctly on inputs ") `<>` Inputs
                  else text("*** succeeded incorrectly on inputs ") `<>` Inputs
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
    io__print("\nlist__map_corresponding/3: ", IO0, IO1),
    pprint__write(80, Result, IO1, IO).

%------------------------------------------------------------------------------%

:- func sum2(int, int) = int.

sum2(X, Y) = X + Y.

%------------------------------------------------------------------------------%

:- pred run_test3({bool, list(int), list(int), list(int), list(int)}, io, io).
:- mode run_test3(in, di, uo) is cc_multi.

run_test3({ShouldSucceed, As, Bs, Cs, Ds}, IO0, IO) :-
    P = ( pred(Zs::out) is det :- Zs = list__map_corresponding3(sum3, As, Bs, Cs)),
    try(P, ExceptionResult),
    Inputs = to_doc(As) `<>` space `<>` to_doc(Bs) `<>` space `<>` to_doc(Cs),
    (
        ExceptionResult = succeeded(Ws),
        (
            ShouldSucceed = yes,
            Result =
                ( if   Ws = Ds
                  then text("succeeded correctly on inputs ") `<>` Inputs
                  else text("*** succeeded incorrectly on inputs ") `<>` Inputs
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
    io__print("\nlist__map_corresponding3/4: ", IO0, IO1),
    pprint__write(80, Result, IO1, IO).

%------------------------------------------------------------------------------%

:- func sum3(int, int, int) = int.

sum3(X, Y, Z) = X + Y + Z.

%------------------------------------------------------------------------------%

:- pred test_case2({bool, list(int), list(int), list(int)}).
:- mode test_case2(out) is multi.

test_case2({yes, [], [], []}).
test_case2({yes, [1, 2, 3], [4, 5, 6], [5, 7, 9]}).
test_case2({no,  [], [1], []}).
test_case2({no,  [1], [], []}).

%------------------------------------------------------------------------------%

:- pred test_case3({bool, list(int), list(int), list(int), list(int)}).
:- mode test_case3(out) is multi.

test_case3({yes, [], [], [], []}).
test_case3({yes, [1, 2, 3], [4, 5, 6], [7, 8, 9], [12, 15, 18]}).
test_case3({no,  [], [1], [1], []}).
test_case3({no,  [1], [], [1], []}).
test_case3({no,  [], [], [1], []}).
test_case3({no,  [1], [1], [], []}).
test_case3({no,  [], [1], [], []}).
test_case3({no,  [1], [], [], []}).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
