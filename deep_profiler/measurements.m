%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module defines the data structures that store deep profiling
% measurements and the operations on them.

:- module measurements.

:- interface.

:- import_module list.

:- type own_prof_info.
:- type inherit_prof_info.

:- func calls(own_prof_info) = int.
:- func exits(own_prof_info) = int.
:- func fails(own_prof_info) = int.
:- func redos(own_prof_info) = int.
:- func quanta(own_prof_info) = int.
:- func mallocs(own_prof_info) = int.
:- func words(own_prof_info) = int.

:- func zero_own_prof_info = own_prof_info.

:- func inherit_quanta(inherit_prof_info) = int.
:- func inherit_mallocs(inherit_prof_info) = int.
:- func inherit_words(inherit_prof_info) = int.

:- func zero_inherit_prof_info = inherit_prof_info.

:- func add_inherit_to_inherit(inherit_prof_info, inherit_prof_info)
	= inherit_prof_info.
:- func add_own_to_inherit(own_prof_info, inherit_prof_info)
	= inherit_prof_info.
:- func subtract_own_from_inherit(own_prof_info, inherit_prof_info)
	= inherit_prof_info.
:- func add_inherit_to_own(inherit_prof_info, own_prof_info) = own_prof_info.
:- func add_own_to_own(own_prof_info, own_prof_info) = own_prof_info.

:- func sum_own_infos(list(own_prof_info)) = own_prof_info.
:- func sum_inherit_infos(list(inherit_prof_info)) = inherit_prof_info.

:- func compress_profile(int, int, int, int, int, int, int) = own_prof_info.
:- func compress_profile(own_prof_info) = own_prof_info.

:- func own_to_string(own_prof_info) = string.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

:- type own_prof_info
	--->	all(int, int, int, int, int, int, int)
					% calls, exits, fails, redos, quanta,
					% memory_mallocs, memory_words
	;	det(int, int, int, int)	% calls, quanta, mallocs, words;
					% implicit exits == calls,
					% implicit fails == redos == 0
	;	zdet(int, int, int).	% calls, mallocs, words;
					% implicit exits == calls,
					% implicit fails == redos == 0
					% implicit quanta == 0

:- type inherit_prof_info
	--->	inherit_prof_info(
			int, 		% quanta
			int, 		% memory_mallocs
			int 		% memory_words
		).

calls(zdet(Calls, _, _)) = Calls.
exits(zdet(Calls, _, _)) = Calls.
fails(zdet(_, _, _)) = 0.
redos(zdet(_, _, _)) = 0.
quanta(zdet(_, _, _)) = 0.
mallocs(zdet(_, Mallocs, _)) = Mallocs.
words(zdet(_, _, Words)) = Words.

calls(det(Calls, _, _, _)) = Calls.
exits(det(Calls, _, _, _)) = Calls.
fails(det(_, _, _, _)) = 0.
redos(det(_, _, _, _)) = 0.
quanta(det(_, Quanta, _, _)) = Quanta.
mallocs(det(_, _, Mallocs, _)) = Mallocs.
words(det(_, _, _, Words)) = Words.

calls(all(Calls, _, _, _, _, _, _)) = Calls.
exits(all(_, Exits, _, _, _, _, _)) = Exits.
fails(all(_, _, Fails, _, _, _, _)) = Fails.
redos(all(_, _, _, Redos, _, _, _)) = Redos.
quanta(all(_, _, _, _, Quanta, _, _)) = Quanta.
mallocs(all(_, _, _, _, _, Mallocs, _)) = Mallocs.
words(all(_, _, _, _, _, _, Words)) = Words.

zero_own_prof_info = zdet(0, 0, 0).

inherit_quanta(inherit_prof_info(Quanta, _, _)) = Quanta.
inherit_mallocs(inherit_prof_info(_, Mallocs, _)) = Mallocs.
inherit_words(inherit_prof_info(_, _, Words)) = Words.

zero_inherit_prof_info = inherit_prof_info(0, 0, 0).

add_inherit_to_inherit(PI1, PI2) = SumPI :-
	Quanta = inherit_quanta(PI1) + inherit_quanta(PI2),
	Mallocs = inherit_mallocs(PI1) + inherit_mallocs(PI2),
	Words = inherit_words(PI1) + inherit_words(PI2),
	SumPI = inherit_prof_info(Quanta, Mallocs, Words).

add_own_to_inherit(PI1, PI2) = SumPI :-
	Quanta = quanta(PI1) + inherit_quanta(PI2),
	Mallocs = mallocs(PI1) + inherit_mallocs(PI2),
	Words = words(PI1) + inherit_words(PI2),
	SumPI = inherit_prof_info(Quanta, Mallocs, Words).

subtract_own_from_inherit(PI1, PI2) = SumPI :-
	Quanta = inherit_quanta(PI2) - quanta(PI1),
	Mallocs = inherit_mallocs(PI2) - mallocs(PI1),
	Words = inherit_words(PI2) - words(PI1),
	SumPI = inherit_prof_info(Quanta, Mallocs, Words).

add_inherit_to_own(PI1, PI2) = SumPI :-
	Calls = calls(PI2),
	Exits = exits(PI2),
	Fails = fails(PI2),
	Redos = redos(PI2),
	Quanta = inherit_quanta(PI1) + quanta(PI2),
	Mallocs = inherit_mallocs(PI1) + mallocs(PI2),
	Words = inherit_words(PI1) + words(PI2),
	SumPI = compress_profile(Calls, Exits, Fails, Redos,
		Quanta, Mallocs, Words).

add_own_to_own(PI1, PI2) = SumPI :-
	Calls = calls(PI1) + calls(PI2),
	Exits = exits(PI1) + exits(PI2),
	Fails = fails(PI1) + fails(PI2),
	Redos = redos(PI1) + redos(PI2),
	Quanta = quanta(PI1) + quanta(PI2),
	Mallocs = mallocs(PI1) + mallocs(PI2),
	Words = words(PI1) + words(PI2),
	SumPI = compress_profile(Calls, Exits, Fails, Redos,
		Quanta, Mallocs, Words).

sum_own_infos(Owns) =
	list__foldl(add_own_to_own, Owns, zero_own_prof_info).

sum_inherit_infos(Inherits) =
	list__foldl(add_inherit_to_inherit, Inherits, zero_inherit_prof_info).

compress_profile(Calls, Exits, Fails, Redos, Quanta, Mallocs, Words) = PI :-
	(
		Calls = Exits,
		Fails = 0,
		Redos = 0
	->
		(
			Quanta = 0
		->
			PI = zdet(Calls, Mallocs, Words)
		;
			PI = det(Calls, Quanta, Mallocs, Words)
		)
	;
		PI = all(Calls, Exits, Fails, Redos, Quanta, Mallocs, Words)
	).

compress_profile(PI0) = PI :-
	(
		PI0 = all(Calls, Exits, Fails, Redos, Quanta, Mallocs, Words),
		(
			Calls = Exits,
			Fails = 0,
			Redos = 0
		->
			(
				Quanta = 0
			->
				PI = zdet(Calls, Mallocs, Words)
			;
				PI = det(Calls, Quanta, Mallocs, Words)
			)
		;
			PI = PI0
		)
	;
		PI0 = det(Calls, Quanta, Mallocs, Words),
		(
			Quanta = 0
		->
			PI = zdet(Calls, Mallocs, Words)
		;
			PI = PI0
		)
	;
		PI0 = zdet(_, _, _),
		PI = PI0
	).

%-----------------------------------------------------------------------------%

own_to_string(all(Calls, Exits, Fails, Redos, Quanta, Allocs, Words)) =
	"all(" ++
	string__int_to_string(Calls) ++ ", " ++
	string__int_to_string(Exits) ++ ", " ++
	string__int_to_string(Fails) ++ ", " ++
	string__int_to_string(Redos) ++ ", " ++
	string__int_to_string(Quanta) ++ ", " ++
	string__int_to_string(Allocs) ++ ", " ++
	string__int_to_string(Words) ++
	")".
own_to_string(det(Calls, Quanta, Allocs, Words)) =
	"det(" ++
	string__int_to_string(Calls) ++ ", " ++
	string__int_to_string(Quanta) ++ ", " ++
	string__int_to_string(Allocs) ++ ", " ++
	string__int_to_string(Words) ++
	")".
own_to_string(zdet(Calls, Allocs, Words)) =
	"det(" ++
	string__int_to_string(Calls) ++ ", " ++
	string__int_to_string(Allocs) ++ ", " ++
	string__int_to_string(Words) ++
	")".
