%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: higher_order.m
% main author: philip

:- module higher_order.
:- interface.
:- import_module list, string. % std_util, term.

% :- func univ(string, list(term)) = term.
% 
% :- func const(string) = term.
% 
% :- func term_from_list(func(X) = term, list(X)) = term.
% :- mode term_from_list(func(in) = out is det, in) = out is det.
% 
% :- func term_from_string(string) = term.
% 
% :- func term_from_int(int) = term.
% 
% :- func maybe_to_term(func(X) = term, maybe(X)) = term.
% :- mode maybe_to_term(func(in) = out is det, in) = out is det.
% 
% :- func pair_to_term(func(X) = term, func(Y) = term, pair(X, Y)) = term.
% :- mode pair_to_term(func(in) = out is det, func(in) = out is det, in) = out
%                                                                        is det.

:- func string_portray_list(func(X) = string, list(X)) = string.
:- mode string_portray_list(func(in) = out is det, in) = out is det.

:- func string_portray_list(func(X) = string, string, string, string, list(X))
								= string.
:- mode string_portray_list(func(in) = out is det, in, in, in, in)
								= out is det.

:- implementation.
:- import_module builtin, int.

% univ(S, L) = term__functor(term__atom(S), L, C) :- term__context_init(C).
% 
% const(S) = term__functor(term__atom(S), [], C) :- term__context_init(C).
% 
% term_from_list(_, []) = const("[]").
% term_from_list(P, [H|L]) = univ("[|]", [apply(P, H), term_from_list(P, L)]).
% 
% term_from_string(S) = term__functor(term__string(S), [], C) :-
%         term__context_init(C).
% 
% term_from_int(I) = term__functor(term__integer(I), [], C) :-
%         term__context_init(C).
% 
% maybe_to_term(_, no) = const("no").
% maybe_to_term(P, yes(X)) = univ("yes", [apply(P, X)]).
% 
% pair_to_term(P1, P2, X1-X2) = univ("-", [apply(P1, X1), apply(P2, X2)]).

string_portray_list(F, L) = string_portray_list(F, "[", ", ", "]", L).

string_portray_list(F, Pre, In, Post, L) = S :-
	list__map((pred(I::in, O::out) is det :- O = F(I)), L, SL),
	list__length(SL, N),
	list__duplicate(N - 1, In, InL),
	list__append(InL, [Post], InL1),
	list__zip(SL, InL1, SL1),
	string__append_list([Pre|SL1], S).

