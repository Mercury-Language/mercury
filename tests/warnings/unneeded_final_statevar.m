%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unneeded_final_statevar.
:- interface.
:- import_module io.

:- pred p1(io::di, io::uo) is det.
:- pred p2(int::mdi, int::muo) is semidet.
:- pred p3(int::mdi, int::muo) is semidet.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module int.

%---------------------------------------------------------------------------%

% p1 should not get a warning, due to the heuristic rule that says
% "do not generate unused final statevar warnings for a predicate
% if it is defining clauses are all facts".
p1(!IO).

%---------------------%

% p2 should not get a warning, due to the heuristic rule that says
% "do not generate unused final statevar warnings for a predicate if
% - the initial value of the statevar is used, and
% - the mode of either the initial or the final statevar contains uniquness".
p2(!State) :-
    p2a(!.State),
    semidet_succeed.

:- pred p2a(int::mui) is det.
:- pragma external_pred(p2a/1).

%---------------------%

% p3 *should* get a warning, because neither of those heuristics apply to it.
p3(!State) :-
    semidet_fail.

%---------------------------------------------------------------------------%
