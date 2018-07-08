%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test is a variant of coup, with the commits around the recursive
% calls wrapped up inside explicit predicates. This means that when a
% subgoal is suspended, coup_det_frame will require a non-empty det
% stack segment to be saved, whereas in coup the saved det stack segment
% is empty. Both need to be tested.
%
% In case there are any problems with the interaction of the commits
% and tabling, this version is more likely to be easy to debug, since
% putting breakpoints on any_p and any_q effectively puts breakpoint
% on the commits, which otherwise you can't easily do.

:- module coup_det_frame.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

:- pragma require_feature_set([memo]).

main(!IO) :-
    solutions(p, Solns),
    io.write(Solns, !IO),
    io.write_string("\n", !IO).

:- pragma minimal_model(p/1).
:- pred p(int).
:- mode p(out) is nondet.

p(X) :-
    q(X).
p(X) :-
    X = 1.

:- pragma minimal_model(q/1).
:- pred q(int).
:- mode q(out) is nondet.

q(3) :-
    any_q.
q(4) :-
    any_p.

:- pred any_q is semidet.

any_q :-
    q(_).

:- pred any_p is semidet.

any_p :-
    p(_).
