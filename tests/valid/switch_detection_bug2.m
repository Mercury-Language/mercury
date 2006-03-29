%-----------------------------------------------------------------------------%
% Fix a bug where cse_detection wasn't hoisting the common deconstruction
% for a one-arm switch on argument 2 of display_diff_side_by_side_2.
% The symptom was a determinism error.
:- module switch_detection_bug2.

:- interface.
:- import_module io, int, list, pair.

%-----------------------------------------------------------------------------%

:- type pos == int.

:- type segment == pair(pos,pos).

:- type edit --->
		add(pos,segment)
	;	delete(segment,pos)
	;	change(segment,segment).

:- type diff == list(edit).


:- pred display_diff_side_by_side_2(int, side_by_side_info, diff,
		io__state, io__state).
:- mode display_diff_side_by_side_2(in, in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, std_util, int, list, char, string, bool.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Parameters to pass around.
:- type side_by_side_info
	--->	side_by_side_info(
			int,		% Half width
			int,		% Column 2 offset
			bool,		% Left column only
			bool,		% Suppress common lines
			bool		% Help sdiff
	).

display_diff_side_by_side_2(_Prev, SBS, []) -->
	{ SBS = side_by_side_info(_, _, _, Suppress, _) },
	( { Suppress = no } ->
		[]
	;
		[]
	).
display_diff_side_by_side_2(Prev, SBS, [Edit | Diff]) -->
	{ first_mentioned_positions(Edit, _, _) },
	{ SBS = side_by_side_info(_, _, _, Suppress, _) },
	( { Suppress = no } ->
		[]
	;
		[]
	),
	display_diff_side_by_side_2(Prev, SBS, Diff).

:- pred first_mentioned_positions(edit :: in, pos :: out, pos :: out) is det.
:- external(first_mentioned_positions/3).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

