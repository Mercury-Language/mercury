%---------------------------------------------------------------------------%
% Copyright (C) 2002-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% Set_cc is an implementation of sets which uses compare_representation
% instead of builtin comparison, hence it is suitable for use with terms
% that don't have a canonical representation.  It is implemented using
% tree234_cc; see that module for further discussion about the implications
% of using compare_representation.
%
% Author: Mark Brown (dougl)

%---------------------------------------------------------------------------%

:- module mdb__set_cc.

:- interface.
:- import_module bool.

:- type set_cc(T).

:- pred set_cc__init(set_cc(T)).
:- mode set_cc__init(uo) is det.

:- pred set_cc__is_empty(set_cc(T)).
:- mode set_cc__is_empty(in) is semidet.

:- pred set_cc__member(T, set_cc(T), bool).
:- mode set_cc__member(in, in, out) is cc_multi.

:- pred set_cc__insert(set_cc(T), T, set_cc(T)).
:- mode set_cc__insert(in, in, out) is cc_multi.

:- pred set_cc__delete(set_cc(T), T, set_cc(T)).
:- mode set_cc__delete(in, in, out) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb__tree234_cc.

:- import_module std_util.

:- type set_cc(T) == tree234_cc(T, unit).

set_cc__init(S) :-
	tree234_cc__init(S).

set_cc__is_empty(S) :-
	tree234_cc__is_empty(S).

set_cc__member(T, S, Bool) :-
	tree234_cc__search(S, T, Maybe),
	(
		Maybe = yes(_),
		Bool = yes
	;
		Maybe = no,
		Bool = no
	).

set_cc__insert(S0, T, S) :-
	tree234_cc__set(S0, T, unit, S).

set_cc__delete(S0, T, S) :-
	tree234_cc__delete(S0, T, S).

