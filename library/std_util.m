%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: std_util.nl.
% Main author: fjh.

% This file is intended for all the useful standard utilities
% that don't belong elsewhere, like <stdlib.h> in C.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module std_util.
:- interface.
:- import_module list.

%-----------------------------------------------------------------------------%

% The universal type.
% Note that the current NU-Prolog implementation of univ_to_type
% is buggy in that it always succeeds, even if the types didn't
% match, so until this gets implemented correctly, don't use
% univ_to_type unless you are sure that the types will definely match.

:- type univ.

:- pred type_to_univ(_T, univ).
:- mode type_to_univ(in, out) is det.
:- mode type_to_univ(out, in) is semidet.

:- pred univ_to_type(univ, _T).
:- mode univ_to_type(in, out) is semidet.
:- mode univ_to_type(out, in) is det.

%-----------------------------------------------------------------------------%

% The boolean type.
% Unlike most languages, we use `yes' and `no' as boolean constants
% rather than `true' and `false'.  This is to avoid confusion
% with the predicates `true' and `fail'.

:- type bool ---> yes ; no.

:- pred bool__or(bool, bool, bool).
:- mode bool__or(in, in, out) is det.

:- pred bool__and(bool, bool, bool).
:- mode bool__and(in, in, out) is det.

%-----------------------------------------------------------------------------%

% The "maybe" type.

:- type maybe(T) ---> yes(T) ; no.

%-----------------------------------------------------------------------------%

:- type unit		--->	unit.

:- type pair(T1, T2)	--->	(T1 - T2).
:- type pair(T)		==	pair(T,T).

:- type assoc_list(K,V)	==	list(pair(K,V)).

:- pred assoc_list__reverse_members(assoc_list(K, V), assoc_list(V, K)).
:- mode assoc_list__reverse_members(in, out) is det.

:- pred assoc_list__from_corresponding_lists(list(K), list(V),
							assoc_list(K, V)).
:- mode assoc_list__from_corresponding_lists(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred gc_call(pred).
:- mode gc_call(in) is nondet.

:- pred solutions(pred(T), list(T)).
:- mode solutions(complicated_mode, out) is det.

% The following is a temporary hack until we implement higher-order
% modes.

:- mode complicated_mode :: input.

%-----------------------------------------------------------------------------%

% Declaratively, `report_stats' is the same as `true'.
% It has the side-effect of reporting some memory and time usage statistics
% to stdout.  (Technically, every Mercury implementation must offer
% a mode of invokation which disables this side-effect.)

:- pred report_stats is det.

%-----------------------------------------------------------------------------%

	% `semidet_succeed' is exactly the same as `true', except that
	% the compiler thinks that it is semi-deterministic.  You can
	% use calls to `semidet_succeed' to suppress warnings about
	% determinism declarations which could be stricter.

:- pred semidet_succeed is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- external(report_stats/0).
:- external(type_to_univ/2).
:- external(gc_call/1).		% currently only implemented for Prolog
:- external(solutions/2).	% currently only implemented for Prolog

assoc_list__reverse_members([], []).
assoc_list__reverse_members([K-V|KVs], [V-K|VKs]) :-
	assoc_list__reverse_members(KVs, VKs).

assoc_list__from_corresponding_lists(As, Bs, ABs) :-
	(
		assoc_list__from_corresponding_lists_2(As, Bs, ABs0)
	->
		ABs = ABs0
	;
		error("Lists have different lengths.")
	).

:- pred assoc_list__from_corresponding_lists_2(list(K), list(V), 
							assoc_list(K, V)).
:- mode assoc_list__from_corresponding_lists_2(in, in, out) is semidet.

assoc_list__from_corresponding_lists_2([], [], []).
assoc_list__from_corresponding_lists_2([A|As], [B|Bs], [A - B|ABs]) :-
	assoc_list__from_corresponding_lists_2(As, Bs, ABs).

univ_to_type(Univ, X) :- type_to_univ(X, Univ).

bool__or(yes, _, yes).
bool__or(no, Bool, Bool).

bool__and(no, _, no).
bool__and(yes, Bool, Bool).

%-----------------------------------------------------------------------------%

	% Some hacks to prevent compiler warnings.

semidet_succeed :-
	dummy(1).

:- pred dummy(int).
:- mode dummy(in) is semidet.

dummy(1).

%-----------------------------------------------------------------------------%
