%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the code that generates warnings about unused final values of statevars,
% and the heuristics for when we should *not* generate such warnings.
%
% There are two such heuristics.
%
% - The first heuristic says "do not generate unused final statevar warnings
%   for a predicate if its defining clauses are all facts".
%
% - The second heuristic says "do not generate unused final statevar warnings
%   for a predicate if
%   - the initial value of the statevar is used, *and*
%   - the mode of either the initial or the final statevar contains
%     uniqueness".
%
% Test for
%
% - the presence of the first heuristic
% - the presence of the second heuristic
% - the presence of neither heuristic
%
% for final state vars in both clause heads and lambda expression heads.
%

:- module unneeded_final_statevar.
:- interface.
:- import_module io.

:- pred p1(io::di, io::uo) is det.
:- pred p2(int::mdi, int::muo) is semidet.
:- pred p3(int::mdi, int::muo) is semidet.
:- pred q1(io::di, io::uo) is det.
:- pred q2(int::mdi, int::muo, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

% p1 should not get a warning due to the first heuristic.
p1(!IO).

%---------------------%

% p2 should not get a warning due to the second heuristic.
p2(!State) :-
    mui(!.State),
    semidet_succeed.

:- pred mui(int::mui) is det.
:- pragma external_pred(mui/1).

%---------------------%

% p3 *should* get a warning, because neither heuristic applies to it.
p3(!State) :-
    semidet_fail.

%---------------------------------------------------------------------------%

q1(!IO) :-
    % We should not get a warning for !:Sum due to the first heuristic.
    % We should not get a warning for !:I due to the second heuristic.
    P = (
            pred(_N::in, !.Sum::in, !:Sum::out, !.I::di, !:I::uo) is det :-
                true
        ),
    Nums = [1, 2, 3],
    list.foldl2(P, Nums, 0, SumP, !IO),
    io.format("%d\n", [i(SumP)], !IO).

%---------------------------------------------------------------------------%

% We should get a warning for !State because neither heuristic applies to it.
% The !IO statevar is of course used.
q2(!State, !IO) :-
    % This is a repeat of q1, but with a nonempty lambda body, which makes
    % the first heuristic (empty clause body) inapplicable.
    % The second heuristic applies to !:I, but not to !:Sum,
    % though !I is also used.
    Q = (
            pred(N::in, !.Sum::in, !:Sum::out, !.I::di, !:I::uo) is det :-
                io.format("q: %d\n", [i(N + !.Sum)], !I)
        ),
    Nums = [1, 2, 3],
    list.foldl2(Q, Nums, 0, SumQ, !IO),
    io.format("%d\n", [i(SumQ)], !IO).

%---------------------------------------------------------------------------%
