%---------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%---------------------------------------------------------------------------%
% state_vars_tests.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Wed Apr 3 14:19:02 EST 2002
%---------------------------------------------------------------------------%

:- module state_vars_tests.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    unsorted_solutions(test, Solns),
    list.reverse(Solns, RevSolns),
    io.print_line(RevSolns, !IO).

%---------------------------------------------------------------------------%

:- pred test(int::out) is multi.

test(X) :-
    add(1, 0, X).

test(X) :-
    some [!A] (
        add(2, 0, !:A), X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0, add(3, !A), X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 4, not fail, X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 5, not (add(1, !A), !.A = 5), X = !.A
    ).

test(X) :-
    some [!A, !B] (
        !:A = 1, !:B = 1, add(1, !A), add(2, !B), X = !.A * !.B
    ).

test(X) :-
    some [!A] (
        ( if true then !:A = 7 else !:A = -1 ), X = !.A
    ).

test(X) :-
    some [!A] (
        ( if fail then !:A = -1 else !:A = 8 ), X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0, ( if t(!.A, _) then !:A = 9 else !:A = -1 ), X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0, ( if f(!.A, _) then !:A = -1 else !:A = 10 ), X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        ( if ( f(!A) ; t(!A) ), !.A = 1 then !:A = 11 else !:A = -1 ),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        ( if ( t(!A) ; f(!A) ), !.A = 1 then !:A = 12 else !:A = -1 ),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        ( add(13, !A) ; add(14, !A) ),
        X = !.A
    ).

test(X) :-
    some [!A, !B] (
        !:A = 1,
        !:B = 1,
        ( add(14, !A) ; add(15, !B) ),
        X = !.A * !.B
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        !:A = fn_a(17, !.A),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        !:A = fn_b(18, !.A),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        F = ( func(!.B) = !:B :- !:B = !.B + 19 ),
        !:A = F(!.A),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        F = ( func(!.B) = !.B + 20 ),
        !:A = F(!.A),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        F = ( func(!.A) = !:A :- !:A = !.A + 21 ),
        !:A = F(!.A),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        F = ( func(!.A) = !.A + 22 ),
        !:A = F(!.A),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        P = ( pred(!.B :: in, !:B :: out) is det :- !:B = !.B + 23 ),
        P(!A),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        P = ( pred(!.B :: in, (!.B + 24) :: out) is det ),
        P(!A),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        P = ( pred(!.A :: in, !:A :: out) is det :- !:A = !.A + 25 ),
        P(!A),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        P = ( pred(!.A :: in, (!.A + 26) :: out) is det ),
        P(!A),
        X = !.A
    ).

test(!:A * !:B) :-
    !:A = 2,
    add(1, !A),
    !:B = 8,
    add(1, !B).

test(X) :-
    some [!A] (
        !:A = 0,
        ( if add(28, !A) then
            true
        else
            !:A = -1
        ),
        X = !.A
    ).

test(X) :-
    some [!A] (
        !:A = 0,
        ( if add(0, !A) then
            !:A = !.A + 29
        else
            true
        ),
        X = !.A
    ).

% This use of state variables is no longer considered valid.
% test(X) :-
%      X =
%         ( if ( some [!A] !:A = 30 ) then !.A else 0 ).

test(X) :-
    ( if ( some [!A] !:A = 31 ) then
        X = !.A
    else
        X = 0
    ).

%---------------------------------------------------------------------------%

:- pred add(int::in, int::in, int::out) is det.

add(N, X, X + N).

:- pred t(int::in, int::out) is semidet.

t(!X) :-
    !:X = !.X + 1,
    semidet_succeed.

:- pred f(int::in, int::out) is semidet.

f(!X) :-
    X0 = !.X,
    !:X = !.X + 1,
    !.X = X0.

:- func fn_a(int, int) = int.

fn_a(N, !.X) = !:X :-
    !:X = !.X + N.

:- func fn_b(int, int) = int.

fn_b(N, !.X) = !.X + N.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
