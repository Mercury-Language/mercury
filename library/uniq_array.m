%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: uniq_array.m
% Main author: fjh
% Stability: VERY LOW

% This module provides dynamically-sized one-dimensional arrays.
% Array indices start at zero.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module uniq_array.
:- interface.
:- import_module int, list.

:- type uniq_array(T).

:- inst uniq_array(I) ---> uniq_array(I).
:- inst uniq_array == uniq_array(ground).
:- inst uniq_array_skel == uniq_array(free).

:- mode uniq_array_di == di(uniq_array).
:- mode uniq_array_uo == out(uniq_array).
:- mode uniq_array_ui == in(uniq_array).

	% uniq_array__make_empty_array(Array) creates an array of size zero
	% with bounds from 0 to 0.

:- pred uniq_array__make_empty_array(uniq_array(T)).
:- mode uniq_array__make_empty_array(uniq_array_uo) is det.

	% uniq_array__init(Size, Init, Array) creates a uniq_array
	% with bounds from 0 to Size-1, with each element initialized to Init.
:- pred uniq_array__init(int, T, uniq_array(T)).
:- mode uniq_array__init(in, in, uniq_array_uo) is det.

	% uniq_array__max returns the upper bound of the array
:- pred uniq_array__max(uniq_array(_T), int).
:- mode uniq_array__max(uniq_array_ui, out) is det.
:- mode uniq_array__max(in, out) is det.

	% uniq_array__size returns the length of the array,
	% i.e. upper bound + 1.
:- pred uniq_array__size(uniq_array(_T), int).
:- mode uniq_array__size(uniq_array_ui, out) is det.
:- mode uniq_array__size(in, out) is det.

	% uniq_array__lookup returns the Nth element of an uniq_array.
	% It is an error if the index is out of bounds.
:- pred uniq_array__lookup(uniq_array(T), int, T).
:- mode uniq_array__lookup(uniq_array_ui, in, out) is det.
:- mode uniq_array__lookup(in, in, out) is det.

	% uniq_array__set sets the nth element of an uniq_array, and returns the
	% resulting uniq_array (good opportunity for destructive update ;-).  
	% It is an error if the index is out of bounds.
:- pred uniq_array__set(uniq_array(T), int, T, uniq_array(T)).
:- mode uniq_array__set(uniq_array_di, in, in, uniq_array_uo) is det.

	% uniq_array__copy(Array0, Array):
	% Makes a new unique copy of a uniq_array.
:- pred uniq_array__copy(uniq_array(T), uniq_array(T)).
:- mode uniq_array__copy(uniq_array_ui, uniq_array_uo) is det.
:- mode uniq_array__copy(in, uniq_array_uo) is det.

	% uniq_array__resize(Array0, Size, Init, Array):
	% The uniq_array is expanded or shrunk to make it fit
	% the new size `Size'.  Any new entries are filled
	% with `Init'.
:- pred uniq_array__resize(uniq_array(T), int, T, uniq_array(T)).
:- mode uniq_array__resize(uniq_array_di, in, in, uniq_array_uo) is det.

	% uniq_array__from_list takes a list (of nonzero length),
	% and returns an uniq_array containing those elements in
	% the same order that they occured in the list.
:- pred uniq_array__from_list(list(T), uniq_array(T)).
:- mode uniq_array__from_list(in, uniq_array_uo) is det.

	% uniq_array__to_list takes an uniq_array and returns a list containing
	% the elements of the uniq_array in the same order that they
	% occurred in the uniq_array.
:- pred uniq_array__to_list(uniq_array(T), list(T)).
:- mode uniq_array__to_list(uniq_array_ui, out) is det.

	% uniq_array__fetch_items takes an uniq_array and a lower and upper
	% index, and places those items in the uniq_array between these
	% indices into a list.  It is an error if either index is
	% out of bounds.
:- pred uniq_array__fetch_items(uniq_array(T), int, int, list(T)).
:- mode uniq_array__fetch_items(in, in, in, out) is det.

	% uniq_array__bsearch takes an uniq_array, an element to be found
	% and a comparison predicate and returns the position of
	% the element in the uniq_array.  Assumes the uniq_array is in sorted
	% order.  Fails if the element is not present.  If the
	% element to be found appears multiple times, the index of
	% the first occurrence is returned.
	% call/N currently does not allow output arguments to come
	% before input arguments, so you can't just pass compare/3
	% in here. :-(
:- pred uniq_array__bsearch(uniq_array(T), T, pred(T, T, comparison_result),
		maybe(int)).
:- mode uniq_array__bsearch(uniq_array_ui, in, pred(in, in, out) is det,
		out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util.

:- type uniq_array(T).

/****
lower bounds other than zero are not supported
	% uniq_array__resize takes an uniq_array and new lower and upper bounds.
	% the uniq_array is expanded or shrunk at each end to make it fit
	% the new bounds.
:- pred uniq_array__resize(uniq_array(T), int, int, uniq_array(T)).
:- mode uniq_array__resize(in, in, in, out) is det.
****/

	% uniq_array__bounds returns the upper and lower bounds of an
	% uniq_array
	% Note: in this implementation, the lower bound is always zero.
:- pred uniq_array__bounds(uniq_array(_T), int, int).
:- mode uniq_array__bounds(in, out, out) is det.

	% uniq_array__min returns the lower bound of the array
	% Note: in this implementation, the lower bound is always zero.
:- pred uniq_array__min(uniq_array(_T), int).
:- mode uniq_array__min(uniq_array_ui, out) is det.
:- mode uniq_array__min(in, out) is det.

%-----------------------------------------------------------------------------%

:- pragma(c_header_code, "

	typedef struct {
		Integer size;
		Word *elements;
	} UniqArrayType;

").

%-----------------------------------------------------------------------------%

:- pragma(c_header_code, "
static UniqArrayType *
make_uniq_array(Integer size, Word item)
{
	Integer i;
	Word *array_elements;
	UniqArrayType *array;

	array_elements = make_many(Word, size);
	for (i = 0; i < size; i++) {
		array_elements[i] = item;
	}
	array = make(UniqArrayType);
	array->elements = array_elements;
	array->size = size;
	return array;
}
").

:- pragma(c_code,
	uniq_array__init(Size::in, Item::in, UniqArray::uniq_array_uo),
"
	UniqArray = (Word) make_uniq_array(Size, Item);
").

:- pragma(c_code,
	uniq_array__make_empty_array(UniqArray::uniq_array_uo),
"
	UniqArray = (Word) make_uniq_array(0, 0);
").

%-----------------------------------------------------------------------------%

:- pragma(c_code, uniq_array__min(UniqArray::uniq_array_ui, Min::out), "
	/* UniqArray not used */
	Min = 0;
").
:- pragma(c_code, uniq_array__min(UniqArray::in, Min::out), "
	/* UniqArray not used */
	Min = 0;
").

:- pragma(c_code, uniq_array__max(UniqArray::uniq_array_ui, Max::out), "
	Max = ((UniqArrayType *)UniqArray)->size - 1;
").
:- pragma(c_code, uniq_array__max(UniqArray::in, Max::out), "
	Max = ((UniqArrayType *)UniqArray)->size - 1;
").

uniq_array__bounds(Array, Min, Max) :-
	uniq_array__min(Array, Min),
	uniq_array__max(Array, Max).

%-----------------------------------------------------------------------------%

:- pragma(c_code, uniq_array__size(UniqArray::uniq_array_ui, Max::out), "
	Max = ((UniqArrayType *)UniqArray)->size;
").
:- pragma(c_code, uniq_array__size(UniqArray::in, Max::out), "
	Max = ((UniqArrayType *)UniqArray)->size;
").

%-----------------------------------------------------------------------------%

:- pragma(c_code, uniq_array__lookup(UniqArray::uniq_array_ui, Index::in,
		Item::out), "{
	UniqArrayType *uniq_array = (UniqArrayType *)UniqArray;
	if ((Unsigned) Index >= uniq_array->size) {
		fatal_error(""uniq_array__lookup: array index out of bounds"");
	}
	Item = uniq_array->elements[Index];
}").
:- pragma(c_code, uniq_array__lookup(UniqArray::in, Index::in, Item::out), "{
	UniqArrayType *uniq_array = (UniqArrayType *)UniqArray;
	if ((Unsigned) Index >= uniq_array->size) {
		fatal_error(""uniq_array__lookup: array index out of bounds"");
	}
	Item = uniq_array->elements[Index];
}").

%-----------------------------------------------------------------------------%

:- pragma(c_code, uniq_array__set(UniqArray0::uniq_array_di, Index::in,
		Item::in, UniqArray::uniq_array_uo), "{
	UniqArrayType *uniq_array = (UniqArrayType *)UniqArray0;
	if ((Unsigned) Index >= uniq_array->size) {
		fatal_error(""uniq_array__set: array index out of bounds"");
	}
	uniq_array->elements[Index] = Item;	/* destructive update! */
	UniqArray = UniqArray0;
}").

%-----------------------------------------------------------------------------%

:- pragma(c_header_code, "
static UniqArrayType *
resize_uniq_array(UniqArrayType *old_array, Integer array_size, Word item)
{
	Integer i;
	Word *array_elements;
	UniqArrayType* array;
	Integer old_array_size;
	Word *old_array_elements;

	old_array_size = old_array->size;
	if (old_array_size == array_size) return old_array;
	old_array_elements = old_array->elements;

	array_elements = make_many(Word, array_size);
	for (i = 0; i < old_array_size; i++) {
		array_elements[i] = old_array_elements[i];
	}
	for (; i < array_size; i++) {
		array_elements[i] = item;
	}

	/*
	** since the mode on the old array is `uniq_array_di', it is safe to
	** deallocate the storage for it
	*/
	oldmem(old_array->elements);
	oldmem(old_array);

	array = make(UniqArrayType);
	array->elements = array_elements;
	array->size = array_size;
	return array;
}
").

:- pragma(c_code,
	uniq_array__resize(UniqArray0::uniq_array_di, Size::in, Item::in,
		UniqArray::uniq_array_uo),
"
	UniqArray = (Word) resize_uniq_array((UniqArrayType *) UniqArray0,
						Size, Item);
").

%-----------------------------------------------------------------------------%

:- pragma(c_header_code, "
static UniqArrayType *
copy_uniq_array(UniqArrayType *old_array)
{
	Integer i;
	Word *array_elements;
	UniqArrayType* array;
	Integer array_size;
	Word *old_array_elements;

	array_size = old_array->size;
	old_array_elements = old_array->elements;

	array_elements = make_many(Word, array_size);
	for (i = 0; i < array_size; i++) {
		array_elements[i] = old_array_elements[i];
	}

	array = make(UniqArrayType);
	array->elements = array_elements;
	array->size = array_size;
	return array;
}
").

:- pragma(c_code,
	uniq_array__copy(UniqArray0::uniq_array_ui, UniqArray::uniq_array_uo),
"
	UniqArray = (Word) copy_uniq_array((UniqArrayType *) UniqArray0);
").

:- pragma(c_code,
	uniq_array__copy(UniqArray0::in, UniqArray::uniq_array_uo),
"
	UniqArray = (Word) copy_uniq_array((UniqArrayType *) UniqArray0);
").

%-----------------------------------------------------------------------------%

uniq_array__from_list([], Array) :-
	uniq_array__make_empty_array(Array).
uniq_array__from_list(List, Array) :-
        List = [ Head | Tail ],
        list__length(List, Len),
        uniq_array__init(Len, Head, Array0),
        uniq_array__insert_items(Tail, 1, Array0, Array).

%-----------------------------------------------------------------------------%

:- pred uniq_array__insert_items(list(T), int, uniq_array(T), uniq_array(T)).
:- mode uniq_array__insert_items(in, in, uniq_array_di, uniq_array_uo) is det.

uniq_array__insert_items([], _N, Array, Array).
uniq_array__insert_items([Head|Tail], N, Array0, Array) :-
        uniq_array__set(Array0, N, Head, Array1),
        N1 is N + 1,
        uniq_array__insert_items(Tail, N1, Array1, Array).

%-----------------------------------------------------------------------------%

uniq_array__to_list(Array, List) :-
        uniq_array__bounds(Array, Low, High),
        uniq_array__fetch_items(Array, Low, High, List).

%-----------------------------------------------------------------------------%

uniq_array__fetch_items(Array, Low, High, List) :-
        (
                Low > High
        ->
                List = []
        ;
                Low1 is Low + 1,
                uniq_array__fetch_items(Array, Low1, High, List0),
                uniq_array__lookup(Array, Low, Item),
                List = [Item|List0]
        ).

%-----------------------------------------------------------------------------%

uniq_array__bsearch(A, El, Compare, Result) :-
	uniq_array__bounds(A, Lo, Hi),
	uniq_array__bsearch_2(A, Lo, Hi, El, Compare, Result).

:- pred uniq_array__bsearch_2(uniq_array(T), int, int, T,
			pred(T, T, comparison_result), maybe(int)).
:- mode uniq_array__bsearch_2(in, in, in, in, pred(in, in, out) is det,
				out) is det.
uniq_array__bsearch_2(Array, Lo, Hi, El, Compare, Result) :-
	Width is Hi - Lo,

	% If Width < 0, there is no range left.
	( Width < 0 ->
	    Result = no
	;
	    % If Width == 0, we may just have found our element.
	    % Do a Compare to check.
	    ( Width = 0 ->
	        uniq_array__lookup(Array, Lo, X),
	        ( call(Compare, El, X, (=)) ->
		    Result = yes(Lo)
	        ;
		    Result = no
	        )
	    ;
	        % Otherwise find the middle element of the range
	        % and check against that.  NOTE: I used ">> 1"
	        % rather than "// 2" because division always
	        % rounds towards zero whereas shift right always
	        % rounds down.  (Indices can be negative.)
	        Mid is (Lo + Hi) >> 1,
	        uniq_array__lookup(Array, Mid, XMid),
	        call(Compare, XMid, El, Comp),
	        ( Comp = (<),
		    Mid1 is Mid + 1,
		    uniq_array__bsearch_2(Array, Mid1, Hi, El, Compare, Result)
	        ; Comp = (=),
		    uniq_array__bsearch_2(Array, Lo, Mid, El, Compare, Result)
	        ; Comp = (>),
		    Mid1 is Mid - 1,
		    uniq_array__bsearch_2(Array, Lo, Mid1, El, Compare, Result)
	        )
	    )
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
