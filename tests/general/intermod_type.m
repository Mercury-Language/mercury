% Regression test for a bug in intermodule optimization - base_type_infos
% for types used by abstract exported predicates in `.opt' files were
% not being exported, causing a link error.
% 
:- module intermod_type.

%------------------------------------------------------------------------------%

:- interface.

:- import_module intermod_type2.
:- import_module int, io, list, std_util.

:- pred main(io__state::di, io__state::uo) is det.

:- type dungeon.

:- type player	== who.

:- type level
	--->	lev(
			int, int, % width, height (== 80 * 22)
			list(who),
			win
		).

:- type pos
	--->	pos(int, int).

:- type who.

:- type runq	== list(pair(int, list(who))).

%------------------------------------------------------------------------------%

:- type rnd == int.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module store.

main -->
	io__write_string("OK\n").

%------------------------------------------------------------------------------%

:- type who == int.

:- type dungeon
	--->	dun(
			win,		% Root window, for messages
			win,		% main window
			win,		% message window
			player,
			list(level),	% levels above me
			level,		% current level
			list(level),	% levels below me
			rnd,
			int,		% current cycle
			runq
		).

%------------------------------------------------------------------------------%
