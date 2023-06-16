%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Compiling this on rotd-2004-12-01 and before in any grade and with inlining
% enabled results in the following assertion failure.
%
%   Uncaught Mercury exception:
%   Software Error: inappropriate determinism inside a negation
%
% The problem goes away when inlining is disabled.
%
% The cause of the problem is that the recomputation of instmap_deltas after
% inlining generates incorrect results. See the XXXs in the predicate
% merge_instmapping_delta_2 in instmap.m and in recompute_instmap_delta_unify
% in mode_util.m. There is no easy fix, since there seems to be no existing
% predicate that takes two insts and computes the intersections of all the
% bound insts inside them.
%
%---------------------------------------------------------------------------%

:- module puzzle_detism_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    ( if solve(james, spanner, library) then
        Result = "committed"
    else
        Result = "did not commit"
    ),
    io.write_string("James " ++ Result ++
        " the murder with the spanner in the library.\n", !IO).

:- type suspect
    --->    george
    ;       katherine
    ;       james.

:- type weapon
    --->    knife
    ;       spanner
    ;       candlestick.

:- type room
    --->    library
    ;       lounge
    ;       conservatory.

:- pred solve(suspect::in, weapon::in, room::in) is semidet.

solve(Suspect, Weapon, Room) :-
    ( Weapon = spanner => ( Room = library ; Room = lounge )),
    ( Weapon = candlestick =>
        ( Suspect = katherine ; Room = conservatory )).
