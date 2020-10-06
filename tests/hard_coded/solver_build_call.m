%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% solver_build_call.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Tue Oct 18 15:30:39 EST 2005
%
% This detects a bug where modes.build_call was using out-of-date versions
% of the varset and vartypes, and then overwriting these fields in the
% mode_info, leading to a compiler abort:
%
%   Uncaught Mercury exception:
%   Software Error: mode analysis: rechecking extra goals adds more extra goals
%
%---------------------------------------------------------------------------%

:- module solver_build_call.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module std_util.

%---------------------------------------------------------------------------%

main(!IO) :-
    promise_pure (
        ( if solve_problem(Solution) then
            io.print("solution found: ", !IO),
            io.print(Solution, !IO),
            io.nl(!IO)
        else
            io.print("no solution found\n", !IO)
        )
    ).

:- pred solve_problem(int::out) is semidet.

solve_problem(Solution) :-
    init(B),
    init(C),
    post_constraint( B \/ -C),
    post_constraint(-B \/  C),
    solve([B, C], Solution).

:- solver type st
    where   representation is int.

:- pred init(st::oa) is det.

init(A) :-
    promise_pure(impure A = 'representation to any st/0'(123)).

:- func -(st::ia) = (st::oa) is det.

-(A) = A.

:- func (st::ia) \/ (st::ia) = (st::oa) is det.

A \/ _ = A.

:- pred post_constraint(st::ia) is semidet.

post_constraint(_) :-
    semidet_succeed.

:- pred solve(list(st)::ia, int::out) is semidet.

solve(_, 42) :-
    semidet_succeed.
