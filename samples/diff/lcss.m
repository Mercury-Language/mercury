%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: bromage
% Simplified by Marnix Klooster <marnix@worldonline.nl>

% The only predicate exported from this module is given two lists, and
% it generates a 'longest common subsequence.'  A 'common subsequence'
% of two lists List1 and List2 is a list of pairs of integers I-J,
% where I and J refer to items I and J (numbering from 0) in List1 and
% List2, respectively.  For a pair I-J we always have 0=<I<L1 and
% 0=<J<L2 and List1[I]=List2[J], where L1 and L2 are the lengths of
% List1 and List2.  For two consecutive pairs I1-J1 and I2-J2 we
% always have I1<I2 and J1<J2.

% This algorithm chooses one of the longest common subsequences, and
% appends the pair L1-L2.  (This is done to find the differing part
% after the last match in the lcss easily.  This pair is essentially
% the match between 'end-of-list' markers appended to both List1 and
% List2.)  [To do: How to describe which one is picked?  Is it the
% 'earliest'?]

% [Idea: A nice exercise might be to formulate and implement
% alternative strategies for choosing a lcss.  E.g. one resulting in
% the smallest diff (how measured?), or using a more symmetric
% algorithm.  GNU diff uses an algorithm not based on lcss's, whic is
% detailed in section 4.2 of "An O(ND) Difference Algorithm and its
% Variations", Eugene Myers, Algorithmica Vol. 1 No. 2, 1986,
% pp. 251-266.]

%-----------------------------------------------------------------------------%

:- module lcss.

:- interface.
:- import_module difftype, file.

:- pred diff_by_lcss(file, file, diff).
:- mode diff_by_lcss(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, require, std_util, int, list, char, array.

diff_by_lcss(File1, File2, Diff) :-
	file__to_list(File1, File1list),
	file__to_list(File2, File2list),
	file__get_numlines(File1, Length1),
	file__get_numlines(File2, Length2),
	lcss__find_diff(File1list, File2list, Length1, Length2, Diff).

%-----------------------------------------------------------------------------%

	% The longest common subsequence of two lists can be
	% represented as an ordered list of "matches".  A match is a
	% pair of the form I-J where I is the number of an item in
	% item 1 and J is the number of an item in list 2.  (Note that
	% an item is numbered by the position immediately preceding
	% it, i.e., numbering starts at 0.)
:- type lcss == list(pair(pos,pos)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Find a longest common subsequence.  The algorithm
	% used is very similar to that in:
	%
	%	Hunt & Szymanski, "A fast algorithm for computing
	%	longest common subsequences", CACM 20:5, pp 350--353,
	%	1977.

	% The essence of the algorithm is simple.  A 'match' is pair
	% I-J so that List1[I]=List2[J].  For every length (>=0) it
	% keeps a lcss of that length that has been found, under the
	% matches considered so far.  Initially, only length 0 has a
	% lcss: the empty one.  All matches between the two lists are
	% gone through in lexicographical order.  For every match the
	% shortest stored lcss is found to which it could be appended.
	% If appending the match results in a lcss that 'ends before'
	% the current lcss of that length, it is replaced.  Here, the
	% 'end' of a lcss is the J component of its last pair I-J (or
	% 'minus infinity' for the empty lcss).

	% Note that always Thresh is increasing, and strictly
	% increasing for all K such that 0 =< Thresh[K] < 'infinity'.
	% Note also that if Link[I] = [P-Q | _], then Thresh[I] = Q.

	% If a lcss of length I has been found, Link[I] contains it in
	% reverse, and Thresh[I] contains its 'end' (see above).
	% Otherwise, Thresh[I] contains 'infinity.'

:- pred lcss__find_diff(list(string) :: in, list(string) :: in,
			int :: in, int :: in, diff :: out) is det.

lcss__find_diff(List1, List2, L1, L2, Diff) :-

	% The original version swapped List1 and List2, so that the
	% first was the largest.  Is this swapping really worthwile?
	% It doesn't seem to be faster, nor does it consume less
	% memory.  Therefore I removed it.  -- MK

	% The consequence is that build_thresh and build_lcss need a
	% value representing 'infinity'; previously we could use the
	% length of the first list for this.  Now we use length of
	% the longest list plus one.  The reason for the plus one is
	% that it should be greater than any other threshold, and
	% on identical files, thresholds may get as large as L1=L2.

	int__max(L1, L2, Inf0),
	Inf is Inf0 + 1,

	% The original version uses arrays of the same length as the
	% longest list.  But it is sufficient to use the length of the
	% shortest list, since a lcss cannot be longer than that.

	int__min(L1, L2, N),

	% Calculate the LCSS.  To run through all matches in
	% lexicographical order, build_matchlist, build_thresh
	% build_thresh2 are used; the processing of a match I-J is
	% done in build_thresh3.  When all matches have been
	% processed, build_lcss extracts the longest lcss found.

	lcss__build_matchlist(List1, List2, MatchList),
	lcss__build_thresh(N, MatchList, Inf, Thresh, Link),
	lcss__build_diff(N, Inf, Thresh, Link, L1, L2, Diff).

%-----------------------------------------------------------------------------%

	% The matchlist represents the set of all matchings I-J
	% between F1 and F2.  It is stored as a list of lists of
	% integers where the Ith element of the list is the list of
	% all J such that F1[I]=F2[J].  Every list in the matchlist is
	% in increasing order, since this is required by build_thresh
	% to go through the matches in lexicographical order.

:- pred lcss__build_matchlist(list(string), list(string), list(list(int))).
:- mode lcss__build_matchlist(in, in, out) is det.
lcss__build_matchlist(List1, List2, MatchList) :-

	% First, invert List2.  The inverted list is a
	% mapping from strings to lists of integers where
	% a given string maps to the list of strings in List2
	% which match that string, in reverse order.  (The
	% reversal is for efficiency reasons.)

	lcss__build_match_map(0, List2, Map),

	% Now match each line in List1 with those in List2,
	% reversing the matches as we go.

	lcss__match_map_to_matchlist(List1, Map, MatchList).

:- pred lcss__build_match_map(int, list(string), map(string,list(int))).
:- mode lcss__build_match_map(in, in, out) is det.
lcss__build_match_map(_, [], Map) :-
	map__init(Map).
lcss__build_match_map(N, [S | Ss], MapOut) :-
	N1 is N + 1,
	lcss__build_match_map(N1, Ss, MapIn),
	( map__search(MapIn, S, Ns0) ->
		Ns1 = [N | Ns0]
	;
		Ns1 = [ N ]
	),
	map__set(MapIn, S, Ns1, MapOut).

:- pred lcss__match_map_to_matchlist(list(string), map(string,list(int)), 
		list(list(int))).
:- mode lcss__match_map_to_matchlist(in, in, out) is det.
lcss__match_map_to_matchlist([], _, []).
lcss__match_map_to_matchlist([S | Ss], Map, [M | Ms]) :-
	lcss__match_map_to_matchlist(Ss, Map, Ms),
	( map__search(Map, S, Ns0) ->
		list__reverse(Ns0, M)
	;
		M = []
	).

%-----------------------------------------------------------------------------%

	% This is the heart of the lcss procedure.  The inputs are the
	% length of the shortest list, and the matchlist of both lists
	% (together with a value to be used as 'infinity').

	% The algorithm maintains and outputs the arrays Thresh and
	% Link (see above).  The values -1 and Inf are used for 'minus
	% infinity' and 'infinity,' respectively.

:- pred lcss__build_thresh(int, list(list(int)), int,
		array(int), array(lcss)).
:- mode lcss__build_thresh(in, in, in, array_uo, array_uo) is det.
lcss__build_thresh(N, MatchList, Inf, Thresh, Link) :-

	% Initialize Thresh and Link.

	N1 is N + 1,	% Why this size?  Suppose we have two identical
			% files of length N.  Then the links will be
			% [], [0-0], [0-0,1-1], ... [0-0..N-N], which
			% makes N+1 links in total.  We could use N
			% if we pre-screened the files to see if they
			% were identical or not.

	array__init(N1, Inf, Thresh0),		% Thresh[0..N] := Inf
	array__set(Thresh0, 0, -1, Thresh1),	% Thresh[0] := -1
	array__init(N1, [], Link1),		% Link[0..N] := []

	% Process all matches in Matchlist in lexicographical order.
	lcss__build_thresh2(N, 0, MatchList, Thresh1, Link1, Thresh, Link).


:- pred lcss__build_thresh2(int, int, list(list(int)),
		array(int), array(lcss), array(int), array(lcss)).
:- mode lcss__build_thresh2(in, in, in,
		array_di, array_di, array_uo, array_uo) is det.
lcss__build_thresh2(_N, _I, [], Thresh0, Link0, Thresh0, Link0).
lcss__build_thresh2(N, I, [Matches | MatchRest], Thresh0, Link0,
						Thresh1, Link1) :-
	I1 is I + 1,
	lcss__build_thresh3(N, I, Matches, Thresh0, Link0, Thresh2, Link2),
	lcss__build_thresh2(N, I1, MatchRest, Thresh2, Link2,
						Thresh1, Link1).


:- pred lcss__build_thresh3(int, int, list(int),
		array(int), array(lcss),
	 	array(int), array(lcss)).
:- mode lcss__build_thresh3(in, in, in,
		array_di, array_di, array_uo, array_uo) is det.
lcss__build_thresh3(_, _, [], Thresh, Link, Thresh, Link).
lcss__build_thresh3(N, I, [ J | Js ], Thresh0, Link0, Thresh1, Link1) :-

	% Process the match I-J, and find which Thresh entry we should
	% attach this match to.  Do this by finding the longest
	% subsequence in Link that 'ends before' J (see above), and
	% set K to its length plus one.

	lcss__build_thresh4(0, N, J, K, Thresh0),

	% Does (reversed) common subsequence [I-J | Link[K-1]] 'end
	% before' Link[K]?  In other words, is J<Thresh[K]?

	array__lookup(Thresh0, K, ThreshK),
	( J < ThreshK ->
		% Yes, so make this match part of a new entry, by
		% doing Link[K] := [I-J | Link[K-1]]

		K1 is K - 1,
		array__set(Thresh0, K, J, Thresh2),
		array__lookup(Link0, K1, LinkK1),
		array__set(Link0, K, [I - J | LinkK1], Link2)
	;
		% Otherwise forget it.
		Link0 = Link2, Thresh0 = Thresh2
	),

	% Process the remaining matches that have I as their first
	% element.
	lcss__build_thresh3(N, I, Js, Thresh2, Link2, Thresh1, Link1).


	% lcss__build_thresh4 performs a binary search
	% through Thresh to find the value of K such
	% that Thresh[K-1] < J =< Thresh[K].
:- pred lcss__build_thresh4(int, int, int, int, array(int)).
:- mode lcss__build_thresh4(in, in, in, out, in) is det.
lcss__build_thresh4(Lo, Hi, J, K, Thresh) :-
	Width is Hi - Lo,
	( Width < 1 ->
		error("lcss__build_thresh4")
	; Width = 1 ->
		K = Hi
	;
		% Use the middle element of the range.
		Mid is (Lo + Hi) // 2,
		array__lookup(Thresh, Mid, ThreshMid),
		( ThreshMid < J ->
			lcss__build_thresh4(Mid, Hi, J, K, Thresh)
		;
			lcss__build_thresh4(Lo, Mid, J, K, Thresh)
		)
	).

%-----------------------------------------------------------------------------%

	% Now that we have the array Thresh, it is a simple exercise
	% to recover the Lcss: Simply find the largest value of K such
	% that Thresh[K]<Inf, and Link[K] should be the Lcss in
	% reverse.

:- pred lcss__build_diff(int, int, array(int), array(lcss), int, int, diff).
:- mode lcss__build_diff(in, in, in, in, in, in, out) is det.
lcss__build_diff(N, Inf, Thresh, Link, L1, L2, Diff) :-
	lcss__build_lcss2(N, Inf, Thresh, K),
	array__lookup(Link, K, Lcss),
	lcss__to_diff(L1, L2, Lcss, [], Diff).


	% Find the largest K<=N for which Thresh[K]<Inf.

	% A simple linear search should be sufficient.  On
	% "normal" input, the number of changes is expected
	% to be small compared to the size of the file, so
	% a full-blown binary search is not necessary.
	% (Even if this were not the case, this predicate
	% is not the bottleneck at the moment.)

:- pred lcss__build_lcss2(int, int, array(int), int).
:- mode lcss__build_lcss2(in, in, in, out) is det.
lcss__build_lcss2(N, Inf, Thresh, K) :-
	( array__lookup(Thresh, N, Inf) ->
		N1 is N - 1,
		lcss__build_lcss2(N1, Inf, Thresh, K)
	;
		K = N
	).

	% lcss__to_diff turns the (reversed) longest common subsequence
	% of two lists into a list of edits.
:- pred lcss__to_diff(int, int, lcss, diff, diff).
:- mode lcss__to_diff(in, in, in, in, out) is det.
lcss__to_diff(_X, _Y, [], Diff, Diff).
lcss__to_diff(X, Y, [X1 - Y1 | Lcss], Diff0, Diff) :-
	X2 is X - 1, Y2 is Y - 1,
	X3 is X1 + 1, Y3 is Y1 + 1,
	( X1 = X2 ->
		( Y1 = Y2 ->
			Diff1 = Diff0
		;
			Diff1 = [add(X, Y3 - Y) | Diff0]
		)
	;
		( Y1 = Y2 ->
			Diff1 = [delete(X3 - X, Y) | Diff0]
		;
			Diff1 = [change(X3 - X, Y3 - Y) | Diff0]
		)
	),
	lcss__to_diff(X1, Y1, Lcss, Diff1, Diff).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
