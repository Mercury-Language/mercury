%----------------------------------------------------------------------------%
%
% Compiling this on rotd-2004-12-01 and before in any grade and with inlining 
% enabled results in the following assertion failure. 
%
% 	Uncaught Mercury exception:
% 	Software Error: inappropriate determinism inside a negation
%
% The problem goes away when `--no-inlining' is enabled.
%
%----------------------------------------------------------------------------%

:- module puzzle_detism_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, string.

main(!IO) :-
	( solve(james, spanner, library) ->
		Result = "committed"
	;
		Result = "did not commit"
	),
	io.write_string("James " ++ Result ++
		" the murder with the spanner in the library.\n", !IO).

:- type suspect
	--->	george
	;	katherine
	;	james.

:- type weapon
	--->	knife
	;	spanner
	;	candlestick.

:- type room
	--->	library
	;	lounge
	;	conservatory.

:- pred solve(suspect::in, weapon::in, room::in) is semidet.

solve(Suspect, Weapon, Room) :-
	( Weapon = spanner => ( Room = library ; Room = lounge )),
	( Weapon = candlestick => 
		( Suspect = katherine ; Room = conservatory )).
