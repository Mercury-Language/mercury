%-----------------------------------------------------------------------------%
% Copyright (C) 1993-1995, 1997, 1999, 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: tr_array.m
% Main authors: fjh
% Stability: medium

% This module provides backtrackable destructive update operations on arrays.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module tr_array.
:- interface.
:- import_module array, list, std_util.

%-----------------------------------------------------------------------------%

%
% Operations that perform backtrackable destructive update.
%

	% tr_array__set sets the nth element of an array, and returns the
	% resulting array (good opportunity for destructive update ;-).  
	% It is an error if the index is out of bounds.
:- pred tr_array__set(array(T), int, T, array(T)).
:- mode tr_array__set(array_mdi, in, in, array_muo) is det.

	% tr_array__semidet_set sets the nth element of an array,
	% and returns the resulting array.
	% It fails if the index is out of bounds.
:- pred tr_array__semidet_set(array(T), int, T, array(T)).
:- mode tr_array__semidet_set(array_mdi, in, in, array_muo) is semidet.

%-----------------------------------------------------------------------------%

%
% "mui" (Mostly Unique Input) versions of standard array operations.
% It is necessary to use these ones rather than the standard array
% operations when dealing with mostly-unique values, to preserve
% "mostly_unique"-ness.
%

	% tr_array__min returns the lower bound of the array.
	% Note: in this implementation, the lower bound is always zero.
:- pred tr_array__min(array(_T), int).
:- mode tr_array__min(array_mui, out) is det.
:- mode tr_array__min(in, out) is det.

	% tr_array__max returns the upper bound of the array.
:- pred tr_array__max(array(_T), int).
:- mode tr_array__max(array_mui, out) is det.
:- mode tr_array__max(in, out) is det.

	% tr_array__size returns the length of the array,
	% i.e. upper bound - lower bound + 1.
:- pred tr_array__size(array(_T), int).
:- mode tr_array__size(array_mui, out) is det.
:- mode tr_array__size(in, out) is det.

	% tr_array__bounds returns the upper and lower bounds of an array.
	% Note: in this implementation, the lower bound is always zero.
:- pred tr_array__bounds(array(_T), int, int).
:- mode tr_array__bounds(array_mui, out, out) is det.
:- mode tr_array__bounds(in, out, out) is det.

	% tr_array__in_bounds checks whether an index is in the bounds
	% of an array.
:- pred tr_array__in_bounds(array(_T), int).
:- mode tr_array__in_bounds(array_mui, in) is semidet.
:- mode tr_array__in_bounds(in, in) is semidet.

%-----------------------------------------------------------------------------%

	% tr_array__lookup returns the Nth element of an array.
	% It is an error if the index is out of bounds.
:- pred tr_array__lookup(array(T), int, T).
:- mode tr_array__lookup(array_mui, in, out) is det.
:- mode tr_array__lookup(in, in, out) is det.

	% tr_array__semidet_lookup returns the Nth element of an array.
	% It fails if the index is out of bounds.
:- pred tr_array__semidet_lookup(array(T), int, T).
:- mode tr_array__semidet_lookup(array_mui, in, out) is semidet.
:- mode tr_array__semidet_lookup(in, in, out) is semidet.

	% tr_array__slow_set sets the nth element of an array,
	% and returns the resulting array.  The initial array is not
	% required to be unique, so the implementation may not be able to use
	% destructive update.
	% It is an error if the index is out of bounds.
:- pred tr_array__slow_set(array(T), int, T, array(T)).
:- mode tr_array__slow_set(array_mui, in, in, array_uo) is det.
:- mode tr_array__slow_set(in, in, in, array_uo) is det.

	% tr_array__semidet_slow_set sets the nth element of an array,
	% and returns the resulting array.  The initial array is not
	% required to be unique, so the implementation may not be able to use
	% destructive update.
	% It fails if the index is out of bounds.
:- pred tr_array__semidet_slow_set(array(T), int, T, array(T)).
:- mode tr_array__semidet_slow_set(array_mui, in, in, array_uo) is semidet.
:- mode tr_array__semidet_slow_set(in, in, in, array_uo) is semidet.

	% tr_array__copy(Array0, Array):
	% Makes a new unique copy of an array.
:- pred tr_array__copy(array(T), array(T)).
:- mode tr_array__copy(array_mui, array_uo) is det.
:- mode tr_array__copy(in, array_uo) is det.

	% tr_array__resize(Array0, Size, Init, Array):
	% The array is expanded or shrunk to make it fit
	% the new size `Size'.  Any new entries are filled
	% with `Init'.
:- pred tr_array__resize(array(T), int, T, array(T)).
:- mode tr_array__resize(array_mui, in, in, array_uo) is det.
:- mode tr_array__resize(in, in, in, array_uo) is det.

	% tr_array__shrink(Array0, Size, Array):
	% The array is shrunk to make it fit the new size `Size'.
	% It is an error if `Size' is larger than the size of `Array0'.
:- pred tr_array__shrink(array(T), int, array(T)).
:- mode tr_array__shrink(array_mui, in, array_uo) is det.
:- mode tr_array__shrink(in, in, array_uo) is det.

	% tr_array__to_list takes an array and returns a list containing
	% the elements of the array in the same order that they
	% occurred in the array.
:- pred tr_array__to_list(array(T), list(T)).
:- mode tr_array__to_list(array_mui, out) is det.
:- mode tr_array__to_list(in, out) is det.

	% tr_array__fetch_items takes an array and a lower and upper
	% index, and places those items in the array between these
	% indices into a list.  It is an error if either index is
	% out of bounds.
:- pred tr_array__fetch_items(array(T), int, int, list(T)).
:- mode tr_array__fetch_items(array_mui, in, in, out) is det.
:- mode tr_array__fetch_items(in, in, in, out) is det.

	% tr_array__bsearch takes an array, an element to be found
	% and a comparison predicate and returns the position of
	% the element in the array.  Assumes the array is in sorted
	% order.  Fails if the element is not present.  If the
	% element to be found appears multiple times, the index of
	% the first occurrence is returned.
:- pred tr_array__bsearch(array(T), T, pred(T, T, comparison_result),
			maybe(int)).
:- mode tr_array__bsearch(array_mui, in, pred(in, in, out) is det, out)
			is det.
:- mode tr_array__bsearch(in, in, pred(in, in, out) is det, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int.

/****
lower bounds other than zero are not supported
	% tr_array__resize takes an array and new lower and upper bounds.
	% the array is expanded or shrunk at each end to make it fit
	% the new bounds.
:- pred tr_array__resize(array(T), int, int, array(T)).
:- mode tr_array__resize(in, in, in, out) is det.
****/

%-----------------------------------------------------------------------------%

% Arrays are implemented in array.m using the C interface.

% The C type which defines the representation of arrays is
% MR_ArrayType; it is defined in runtime/mercury_library_types.h.

:- pragma c_header_code("
	#include ""mercury_library_types.h""
").

%-----------------------------------------------------------------------------%

:- pragma c_code(tr_array__set(Array0::array_mdi, Index::in, Item::in,
		Array::array_muo),
	will_not_call_mercury,
"{
	MR_ArrayType *array = (MR_ArrayType *) Array0;
	if ((MR_Unsigned) Index >= (MR_Unsigned) array->size) {
		MR_fatal_error(""tr_array__set: array index out of bounds"");
	}
	MR_trail_current_value(&array->elements[Index]);
	array->elements[Index] = Item;	/* destructive update! */
	Array = Array0;
}").

tr_array__semidet_set(Array0, Index, Item, Array) :-
	tr_array__in_bounds(Array0, Index),
	tr_array__set(Array0, Index, Item, Array).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma c_code(tr_array__min(Array::array_mui, Min::out),
	will_not_call_mercury,
"
	/* Array not used */
	Min = 0;
").
:- pragma c_code(tr_array__min(Array::in, Min::out),
	will_not_call_mercury,
"
	/* Array not used */
	Min = 0;
").

:- pragma c_code(tr_array__max(Array::array_mui, Max::out),
	will_not_call_mercury,
"
	Max = ((MR_ArrayType *)Array)->size - 1;
").
:- pragma c_code(tr_array__max(Array::in, Max::out),
	will_not_call_mercury,
"
	Max = ((MR_ArrayType *)Array)->size - 1;
").

tr_array__bounds(Array, Min, Max) :-
	tr_array__min(Array, Min),
	tr_array__max(Array, Max).

%-----------------------------------------------------------------------------%

:- pragma c_code(tr_array__size(Array::array_mui, Max::out),
	will_not_call_mercury,
"
	Max = ((MR_ArrayType *)Array)->size;
").
:- pragma c_code(tr_array__size(Array::in, Max::out),
	will_not_call_mercury,
"
	Max = ((MR_ArrayType *)Array)->size;
").

%-----------------------------------------------------------------------------%

tr_array__in_bounds(Array, Index) :-
	tr_array__bounds(Array, Min, Max),
	Min =< Index, Index =< Max.

tr_array__semidet_lookup(Array, Index, Item) :-
	tr_array__in_bounds(Array, Index),
	tr_array__lookup(Array, Index, Item).

tr_array__semidet_slow_set(Array0, Index, Item, Array) :-
	tr_array__in_bounds(Array0, Index),
	tr_array__slow_set(Array0, Index, Item, Array).

tr_array__slow_set(Array0, Index, Item, Array) :-
	tr_array__copy(Array0, Array1),
	array__set(Array1, Index, Item, Array).

%-----------------------------------------------------------------------------%

:- pragma c_code(tr_array__lookup(Array::array_mui, Index::in, Item::out),
	will_not_call_mercury,
"{
	MR_ArrayType *array = (MR_ArrayType *) Array;
	if ((MR_Unsigned) Index >= (MR_Unsigned) array->size) {
		MR_fatal_error(""tr_array__lookup: ""
			""array index out of bounds"");
	}
	Item = array->elements[Index];
}").
:- pragma c_code(tr_array__lookup(Array::in, Index::in, Item::out),
	will_not_call_mercury,
"{
	MR_ArrayType *array = (MR_ArrayType *) Array;
	if ((MR_Unsigned) Index >= (MR_Unsigned) array->size) {
		MR_fatal_error(""tr_array__lookup: array index out of bounds"");
	}
	Item = array->elements[Index];
}").

%-----------------------------------------------------------------------------%

:- pragma c_header_code("
void ML_tr_resize_array(MR_ArrayType *array, const MR_ArrayType *old_array,
					MR_Integer array_size, MR_Word item);
").

:- pragma c_code("
void
ML_tr_resize_array(MR_ArrayType *array, const MR_ArrayType *old_array,
	MR_Integer array_size, MR_Word item)
{
	MR_Integer i;
	MR_Integer elements_to_copy;

	elements_to_copy = old_array->size;
	if (elements_to_copy > array_size) {
		elements_to_copy = array_size;
	}

	array->size = array_size;
	for (i = 0; i < elements_to_copy; i++) {
		array->elements[i] = old_array->elements[i];
	}
	for (; i < array_size; i++) {
		array->elements[i] = item;
	}

	/*
	** since the mode on the old array is `array_mdi', it is NOT safe to
	** deallocate the storage for it
	*/
}
").

:- pragma c_code(
	tr_array__resize(Array0::array_mui, Size::in, Item::in,
		Array::array_uo),
	will_not_call_mercury,
"
	MR_incr_hp_msg(Array, Size + 1, MR_PROC_LABEL, ""array:array/1"");
	ML_tr_resize_array((MR_ArrayType *)Array, (const MR_ArrayType *)Array0,
		Size, Item);
").

:- pragma c_code(
	tr_array__resize(Array0::in, Size::in, Item::in,
		Array::array_uo),
	will_not_call_mercury,
"
	MR_incr_hp_msg(Array, Size + 1, MR_PROC_LABEL, ""array:array/1"");
	ML_tr_resize_array((MR_ArrayType *)Array, (const MR_ArrayType *)Array0,
		Size, Item);
").


%-----------------------------------------------------------------------------%

:- pragma c_header_code("
void ML_tr_shrink_array(MR_ArrayType *, const MR_ArrayType *old_array,
					MR_Integer array_size);
").

:- pragma c_code("
void
ML_tr_shrink_array(MR_ArrayType *array, const MR_ArrayType *old_array,
	MR_Integer array_size)
{
	MR_Integer i;
	MR_Integer old_array_size;

	old_array_size = old_array->size;
	if (old_array_size < array_size) {
		MR_fatal_error(
			""tr_array__shrink: can't shrink to a larger size"");
	}

	array->size = array_size;
	for (i = 0; i < array_size; i++) {
		array->elements[i] = old_array->elements[i];
	}

	/*
	** since the mode on the old array is `array_mdi', it is NOT safe to
	** deallocate the storage for it
	*/
}
").

:- pragma c_code(
	tr_array__shrink(Array0::array_mui, Size::in, Array::array_uo),
	will_not_call_mercury,
"
	MR_incr_hp_msg(Array, Size + 1, MR_PROC_LABEL, ""array:array/1"");
	ML_tr_shrink_array((MR_ArrayType *) Array,
		(const MR_ArrayType *) Array0, Size);
").

:- pragma c_code(
	tr_array__shrink(Array0::in, Size::in, Array::array_uo),
	will_not_call_mercury,
"
	MR_incr_hp_msg(Array, Size + 1, MR_PROC_LABEL, ""array:array/1"");
	ML_tr_shrink_array((MR_ArrayType *) Array,
		(const MR_ArrayType *) Array0, Size);
").

%-----------------------------------------------------------------------------%

:- pragma c_header_code("
void ML_tr_copy_array(MR_ArrayType *array, const MR_ArrayType *old_array);
").

:- pragma c_code("
void
ML_tr_copy_array(MR_ArrayType *array, const MR_ArrayType *old_array)
{
	/*
	** Any changes to this function will probably also require
	** changes to deepcopy() in runtime/deep_copy.c.
	*/

	MR_Integer i;
	MR_Integer array_size;

	array_size = old_array->size;
	array->size = array_size;
	for (i = 0; i < array_size; i++) {
		array->elements[i] = old_array->elements[i];
	}
}
").

:- pragma c_code(
	tr_array__copy(Array0::array_mui, Array::array_uo),
	will_not_call_mercury,
"
	MR_incr_hp_msg(Array, ((const MR_ArrayType *)Array0)->size + 1,
		MR_PROC_LABEL, ""array:array/1"");
	ML_tr_copy_array((MR_ArrayType *)Array, (const MR_ArrayType *)Array0);
").

:- pragma c_code(
	tr_array__copy(Array0::in, Array::array_uo),
	will_not_call_mercury,
"
	MR_incr_hp_msg(Array, ((const MR_ArrayType *)Array0)->size + 1,
		MR_PROC_LABEL, ""array:array/1"");
	ML_tr_copy_array((MR_ArrayType *)Array, (const MR_ArrayType *)Array0);
").

%-----------------------------------------------------------------------------%

tr_array__to_list(Array, List) :-
        tr_array__bounds(Array, Low, High),
        tr_array__fetch_items(Array, Low, High, List).

%-----------------------------------------------------------------------------%

tr_array__fetch_items(Array, Low, High, List) :-
        (
                Low > High
        ->
                List = []
        ;
                Low1 is Low + 1,
                tr_array__fetch_items(Array, Low1, High, List0),
                tr_array__lookup(Array, Low, Item),
                List = [Item|List0]
        ).

%-----------------------------------------------------------------------------%

tr_array__bsearch(A, El, Compare, Result) :-
	tr_array__bounds(A, Lo, Hi),
	tr_array__bsearch_2(A, Lo, Hi, El, Compare, Result).

:- pred tr_array__bsearch_2(array(T), int, int, T,
			pred(T, T, comparison_result), maybe(int)).
:- mode tr_array__bsearch_2(in, in, in, in, pred(in, in, out) is det,
				out) is det.
tr_array__bsearch_2(Array, Lo, Hi, El, Compare, Result) :-
	Width is Hi - Lo,

	% If Width < 0, there is no range left.
	( Width < 0 ->
	    Result = no
	;
	    % If Width == 0, we may just have found our element.
	    % Do a Compare to check.
	    ( Width = 0 ->
	        tr_array__lookup(Array, Lo, X),
	        ( call(Compare, El, X, (=)) ->
		    Result = yes(Lo)
	        ;
		    Result = no
	        )
	    ;
	        % Otherwise find the middle element of the range
	        % and check against that.
	        Mid is (Lo + Hi) >> 1,	% `>> 1' is hand-optimized `div 2'.
	        tr_array__lookup(Array, Mid, XMid),
	        call(Compare, XMid, El, Comp),
	        ( Comp = (<),
		    Mid1 is Mid + 1,
		    tr_array__bsearch_2(Array, Mid1, Hi, El, Compare, Result)
	        ; Comp = (=),
		    tr_array__bsearch_2(Array, Lo, Mid, El, Compare, Result)
	        ; Comp = (>),
		    Mid1 is Mid - 1,
		    tr_array__bsearch_2(Array, Lo, Mid1, El, Compare, Result)
	        )
	    )
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
