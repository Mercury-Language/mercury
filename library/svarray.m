%-----------------------------------------------------------------------------%
% Copyright (C) 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: svarray.m

% This file provides an interface to the 'array' ADT that is conducive to the
% use of state variable notation. The predicates here do the same thing as
% their counterparts in the array module; the only difference is the order
% of the arguments.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module svarray.

:- interface.

:- import_module array.

	% svarray__set sets the nth element of an array, and returns the
	% resulting array (good opportunity for destructive update ;-).
	% Throws an exception if the index is out of bounds.
:- pred svarray__set(int::in, T::in, array(T)::array_di, array(T)::array_uo)
	is det.

	% svarray__semidet_set sets the nth element of an array,
	% and returns the resulting array.
	% It fails if the index is out of bounds.
:- pred svarray__semidet_set(int::in, T::in,
	array(T)::array_di, array(T)::array_uo) is semidet.

	% svarray__slow_set sets the nth element of an array,
	% and returns the resulting array.  The initial array is not
	% required to be unique, so the implementation may not be able to use
	% destructive update.
	% It is an error if the index is out of bounds.
:- pred svarray__slow_set(int, T, array(T), array(T)).
:- mode svarray__slow_set(in, in, array_ui, array_uo) is det.
:- mode svarray__slow_set(in, in, in, array_uo) is det.

	% svarray__semidet_slow_set sets the nth element of an array,
	% and returns the resulting array.  The initial array is not
	% required to be unique, so the implementation may not be able to use
	% destructive update.
	% It fails if the index is out of bounds.
:- pred svarray__semidet_slow_set(int, T, array(T), array(T)).
:- mode svarray__semidet_slow_set(in, in, array_ui, array_uo) is semidet.
:- mode svarray__semidet_slow_set(in, in, in, array_uo) is semidet.

	% svarray__resize(Size, Init, Array0, Array):
	% The array is expanded or shrunk to make it fit
	% the new size `Size'.  Any new entries are filled
	% with `Init'.
:- pred svarray__resize(int::in, T::in, array(T)::array_di, array(T)::array_uo)
	is det.

	% svarray__shrink(Size, Array0, Array):
	% The array is shrunk to make it fit the new size `Size'.
	% Throws an exception if `Size' is larger than the size of `Array0'.
:- pred svarray__shrink(int::in, array(T)::array_di, array(T)::array_uo)
	is det.

%-----------------------------------------------------------------------------%

:- implementation.

svarray__set(Index, Value, !Array) :-
	array__set(!.Array, Index, Value, !:Array).

svarray__semidet_set(Index, Value, !Array) :-
	array__semidet_set(!.Array, Index, Value, !:Array).

svarray__slow_set(Index, Value, !Array) :-
	array__slow_set(!.Array, Index, Value, !:Array).

svarray__semidet_slow_set(Index, Value, !Array) :-
	array__semidet_slow_set(!.Array, Index, Value, !:Array).

svarray__resize(Size, Init, !Array) :-
	array__resize(!.Array, Size, Init, !:Array).

svarray__shrink(Size, !Array) :-
	array__shrink(!.Array, Size, !:Array).

%-----------------------------------------------------------------------------%
