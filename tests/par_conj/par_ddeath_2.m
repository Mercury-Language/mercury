% With --trace deep --parallel
% The incorrect deadness analysis of the parallel conjunction caused Result to
% be added to the pre-death set of the else branch of the if-then-else.
% When --delay-death is enabled (with --trace deep) it would result in the
% following exception:
%
% Uncaught Mercury exception:
% Software Error: liveness.m: Unexpected: branches of if-then-else disagree on liveness
% First: Low High Result
% Rest:  Low High

:- module par_ddeath_2.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

main(!IO) :-
    integers(0, 5, R),
    io.print(R, !IO),
    io.nl(!IO).

:- pred integers(int::in, int::in, list(int)::out) is det.

integers(Low, High, Result) :- 
    ( Low =< High ->
	integers(Low+1, High, Rest) &
	Result = [Low | Rest]
    ;
	Result = []
    ).
