%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: uniq_array.m
% Main author: fjh
% Stability: low
%	This module will probably be renamed as `array'.

% This module provides dynamically-sized one-dimensional arrays.
% Array indices start at zero.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module uniq_array.
:- interface.
:- import_module list, term.

:- type uniq_array(T).

	% XXX the current Mercury compiler doesn't support `ui' modes,
	% so to work-around that problem, we currently don't use
	% unique modes in this module.

% :- inst uniq_array(I) = unique(uniq_array(I)).
:- inst uniq_array(I) = bound(uniq_array(I)).
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

	% uniq_array/1 is a function that constructs an array from a list.
	% (It does the same thing as the predicate uniq_array__from_list/2.)
	% The syntax `uniq_array([...])' is used to represent uniq_arrays
	% for io__read, io__write, term_to_type, and type_to_term.
:- func uniq_array(list(T)) = uniq_array(T).
:- mode uniq_array(in) = uniq_array_uo is det.

	% uniq_array__max returns the upper bound of the array
:- pred uniq_array__max(uniq_array(_T), int).
:- mode uniq_array__max(uniq_array_ui, out) is det.
:- mode uniq_array__max(in, out) is det.

	% uniq_array__size returns the length of the array,
	% i.e. upper bound + 1.
:- pred uniq_array__size(uniq_array(_T), int).
:- mode uniq_array__size(uniq_array_ui, out) is det.
:- mode uniq_array__size(in, out) is det.

	% uniq_array__in_bounds checks whether an index is in the bounds
	% of a uniq_array
:- pred uniq_array__in_bounds(uniq_array(_T), int).
:- mode uniq_array__in_bounds(uniq_array_ui, in) is semidet.
:- mode uniq_array__in_bounds(in, in) is semidet.

	% uniq_array__lookup returns the Nth element of a uniq_array.
	% It is an error if the index is out of bounds.
:- pred uniq_array__lookup(uniq_array(T), int, T).
:- mode uniq_array__lookup(uniq_array_ui, in, out) is det.
:- mode uniq_array__lookup(in, in, out) is det.

	% uniq_array__semidet_lookup returns the Nth element of a uniq_array.
	% It fails if the index is out of bounds.
:- pred uniq_array__semidet_lookup(uniq_array(T), int, T).
:- mode uniq_array__semidet_lookup(uniq_array_ui, in, out) is semidet.
:- mode uniq_array__semidet_lookup(in, in, out) is semidet.

	% uniq_array__set sets the nth element of a uniq_array, and returns the
	% resulting uniq_array (good opportunity for destructive update ;-).  
	% It is an error if the index is out of bounds.
:- pred uniq_array__set(uniq_array(T), int, T, uniq_array(T)).
:- mode uniq_array__set(uniq_array_di, in, in, uniq_array_uo) is det.

	% uniq_array__semidet_set sets the nth element of a uniq_array,
	% and returns the resulting uniq_array.
	% It fails if the index is out of bounds.
:- pred uniq_array__semidet_set(uniq_array(T), int, T, uniq_array(T)).
:- mode uniq_array__semidet_set(uniq_array_di, in, in, uniq_array_uo)
				is semidet.

	% uniq_array__slow_set sets the nth element of a uniq_array,
	% and returns the resulting uniq_array.  The initial array is not
	% required to be unique, so the implementation may not be able to use
	% destructive update.
	% It is an error if the index is out of bounds.
:- pred uniq_array__slow_set(uniq_array(T), int, T, uniq_array(T)).
:- mode uniq_array__slow_set(uniq_array_ui, in, in, uniq_array_uo) is det.
:- mode uniq_array__slow_set(in, in, in, uniq_array_uo) is det.

	% uniq_array__semidet_slow_set sets the nth element of a uniq_array,
	% and returns the resulting uniq_array.  The initial array is not
	% required to be unique, so the implementation may not be able to use
	% destructive update.
	% It fails if the index is out of bounds.
:- pred uniq_array__semidet_slow_set(uniq_array(T), int, T, uniq_array(T)).
:- mode uniq_array__semidet_slow_set(uniq_array_ui, in, in, uniq_array_uo)
				is semidet.
:- mode uniq_array__semidet_slow_set(in, in, in, uniq_array_uo) is semidet.

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

	% uniq_array__from_list takes a list,
	% and returns a uniq_array containing those elements in
	% the same order that they occured in the list.
:- pred uniq_array__from_list(list(T), uniq_array(T)).
:- mode uniq_array__from_list(in, uniq_array_uo) is det.

	% uniq_array__to_list takes a uniq_array and returns a list containing
	% the elements of the uniq_array in the same order that they
	% occurred in the uniq_array.
:- pred uniq_array__to_list(uniq_array(T), list(T)).
:- mode uniq_array__to_list(uniq_array_ui, out) is det.
:- mode uniq_array__to_list(in, out) is det.

	% uniq_array__fetch_items takes a uniq_array and a lower and upper
	% index, and places those items in the uniq_array between these
	% indices into a list.  It is an error if either index is
	% out of bounds.
:- pred uniq_array__fetch_items(uniq_array(T), int, int, list(T)).
:- mode uniq_array__fetch_items(in, in, in, out) is det.

	% uniq_array__bsearch takes a uniq_array, an element to be found
	% and a comparison predicate and returns the position of
	% the element in the uniq_array.  Assumes the uniq_array is in sorted
	% order.  Fails if the element is not present.  If the
	% element to be found appears multiple times, the index of
	% the first occurrence is returned.
:- pred uniq_array__bsearch(uniq_array(T), T, pred(T, T, comparison_result),
		maybe(int)).
:- mode uniq_array__bsearch(uniq_array_ui, in, pred(in, in, out) is det,
		out) is det.
:- mode uniq_array__bsearch(in, in, pred(in, in, out) is det, out) is det.

%-----------------------------------------------------------------------------%
:- implementation.

% Everything beyond here is not intended as part of the public interface,
% and will not appear in the Mercury Library Reference Manual.

%-----------------------------------------------------------------------------%
:- interface.

	% The following predicates have to be declared in the interface,
	% otherwise dead code elimination will remove them.
	% But they're an implementation detail; user code should just
	% use the generic versions.

	% unify/2 for uniq_arrays

:- pred uniq_array_equal(uniq_array(T), uniq_array(T)).
:- mode uniq_array_equal(in, in) is semidet.

	% compare/3 for uniq_arrays

:- pred uniq_array_compare(comparison_result, uniq_array(T), uniq_array(T)).
:- mode uniq_array_compare(out, in, in) is det.

	% type_to_term/2 for uniq_arrays

:- pred uniq_array_to_term(uniq_array(T), term).
:- mode uniq_array_to_term(in, out) is det.

	% term_to_type/2 for uniq_arrays

:- pred uniq_array_from_term(term, uniq_array(T)).
:- mode uniq_array_from_term(in, out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, int.

:- type uniq_array(T).

/****
lower bounds other than zero are not supported
	% uniq_array__resize takes a uniq_array and new lower and upper bounds.
	% the uniq_array is expanded or shrunk at each end to make it fit
	% the new bounds.
:- pred uniq_array__resize(uniq_array(T), int, int, uniq_array(T)).
:- mode uniq_array__resize(in, in, in, out) is det.
****/

	% uniq_array__bounds returns the upper and lower bounds of an
	% uniq_array
	% Note: in this implementation, the lower bound is always zero.
:- pred uniq_array__bounds(uniq_array(_T), int, int).
:- mode uniq_array__bounds(uniq_array_ui, out, out) is det.
:- mode uniq_array__bounds(in, out, out) is det.

	% uniq_array__min returns the lower bound of the array
	% Note: in this implementation, the lower bound is always zero.
:- pred uniq_array__min(uniq_array(_T), int).
:- mode uniq_array__min(uniq_array_ui, out) is det.
:- mode uniq_array__min(in, out) is det.

%-----------------------------------------------------------------------------%

% Arrays are implemented using the C interface.

% The C type which defines the representation of arrays is
% MR_UniqArrayType; it is defined in runtime/type_info.h.

%-----------------------------------------------------------------------------%

:- pragma(c_code, "

Define_extern_entry(mercury____Unify___uniq_array__uniq_array_1_0);
Define_extern_entry(mercury____Index___uniq_array__uniq_array_1_0);
Define_extern_entry(mercury____Compare___uniq_array__uniq_array_1_0);
Define_extern_entry(mercury____TermToType___uniq_array__uniq_array_1_0);
Define_extern_entry(mercury____TypeToTerm___uniq_array__uniq_array_1_0);

#ifdef  USE_TYPE_LAYOUT

const struct mercury_data_uniq_array__base_type_layout_uniq_array_1_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data_uniq_array__base_type_layout_uniq_array_1 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_UNIQ_ARRAY_VALUE))
};

const struct mercury_data_uniq_array__base_type_functors_uniq_array_1_struct {
	Integer f1;
} mercury_data_uniq_array__base_type_functors_uniq_array_1 = {
	MR_TYPEFUNCTORS_SPECIAL
};

#endif

Declare_entry(mercury__uniq_array__uniq_array_equal_2_0);
Declare_entry(mercury__uniq_array__uniq_array_compare_3_0);
Declare_entry(mercury__uniq_array__uniq_array_to_term_2_0);
Declare_entry(mercury__uniq_array__uniq_array_from_term_2_0);

BEGIN_MODULE(uniq_array_module)
	init_entry(mercury____Unify___uniq_array__uniq_array_1_0);
	init_entry(mercury____Index___uniq_array__uniq_array_1_0);
	init_entry(mercury____Compare___uniq_array__uniq_array_1_0);
	init_entry(mercury____TermToType___uniq_array__uniq_array_1_0);
	init_entry(mercury____TypeToTerm___uniq_array__uniq_array_1_0);
BEGIN_CODE

Define_entry(mercury____Unify___uniq_array__uniq_array_1_0);
	/* this is implemented in Mercury, not hand-coded low-level C */
	tailcall(ENTRY(mercury__uniq_array__uniq_array_equal_2_0),
		ENTRY(mercury____Unify___uniq_array__uniq_array_1_0));

Define_entry(mercury____Index___uniq_array__uniq_array_1_0);
	index_output = -1;
	proceed();

Define_entry(mercury____Compare___uniq_array__uniq_array_1_0);
	/* this is implemented in Mercury, not hand-coded low-level C */
	tailcall(ENTRY(mercury__uniq_array__uniq_array_compare_3_0),
		ENTRY(mercury____Compare___uniq_array__uniq_array_1_0));

Define_entry(mercury____TermToType___uniq_array__uniq_array_1_0);
	/* this is implemented in Mercury, not hand-coded low-level C */
	tailcall(ENTRY(mercury__uniq_array__uniq_array_from_term_2_0),
		ENTRY(mercury____TermToType___uniq_array__uniq_array_1_0));

Define_entry(mercury____TypeToTerm___uniq_array__uniq_array_1_0);
	/* this is implemented in Mercury, not hand-coded low-level C */
	tailcall(ENTRY(mercury__uniq_array__uniq_array_to_term_2_0),
		ENTRY(mercury____TypeToTerm___uniq_array__uniq_array_1_0));

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_uniq_array_module
*/
void sys_init_uniq_array_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_uniq_array_module(void) {
	extern ModuleFunc uniq_array_module;
	uniq_array_module();
}

").

%-----------------------------------------------------------------------------%

	% unify/2 for uniq_arrays

uniq_array_equal(Array1, Array2) :-
	uniq_array__size(Array1, Size),
	uniq_array__size(Array2, Size),
	uniq_array__equal_elements(0, Size, Array1, Array2).

:- pred uniq_array__equal_elements(int, int, uniq_array(T), uniq_array(T)).
:- mode uniq_array__equal_elements(in, in, in, in) is semidet.

uniq_array__equal_elements(N, Size, Array1, Array2) :-
	( N = Size ->
		true
	;
		uniq_array__lookup(Array1, N, Elem),
		uniq_array__lookup(Array2, N, Elem),
		N1 is N + 1,
		uniq_array__equal_elements(N1, Size, Array1, Array2)
	).

	% compare/3 for uniq_arrays

uniq_array_compare(Result, Array1, Array2) :-
	uniq_array__size(Array1, Size1),
	uniq_array__size(Array2, Size2),
	compare(SizeResult, Size1, Size2),
	( SizeResult = (=) ->
		uniq_array__compare_elements(0, Size1, Array1, Array2, Result)
	;
		Result = SizeResult
	).

:- pred uniq_array__compare_elements(int, int, uniq_array(T), uniq_array(T),
			comparison_result).
:- mode uniq_array__compare_elements(in, in, in, in, out) is det.

uniq_array__compare_elements(N, Size, Array1, Array2, Result) :-
	( N = Size ->
		Result = (=)
	;
		uniq_array__lookup(Array1, N, Elem1),
		uniq_array__lookup(Array2, N, Elem2),
		compare(ElemResult, Elem1, Elem2),
		( ElemResult = (=) ->
			N1 is N + 1,
			uniq_array__compare_elements(N1, Size, Array1, Array2,
				Result)
		;
			Result = ElemResult
		)
	).


	% type_to_term for uniq_arrays

uniq_array_to_term(Array, Term) :-
	uniq_array__to_list(Array, List),
	type_to_term(List, ListTerm),
	term__context_init(Context),
	Term = term__functor(term__atom("uniq_array"), [ListTerm], Context).


	% term_to_type for uniq_arrays

uniq_array_from_term(Term, Array) :-
	Term = term__functor(term__atom("uniq_array"), [ListTerm], _),
	term_to_type(ListTerm, List),
	uniq_array__from_list(List, Array).

%-----------------------------------------------------------------------------%

:- pragma(c_header_code, "
MR_UniqArrayType *ML_make_uniq_array(Integer size, Word item);
").

:- pragma(c_code, "
MR_UniqArrayType *
ML_make_uniq_array(Integer size, Word item)
{
	Integer i;
	MR_UniqArrayType *array;

	array = MR_make_uniq_array(size);
	array->size = size;
	for (i = 0; i < size; i++) {
		array->elements[i] = item;
	}
	return array;
}
").

:- pragma(c_code,
	uniq_array__init(Size::in, Item::in, UniqArray::uniq_array_uo),
"
	UniqArray = (Word) ML_make_uniq_array(Size, Item);
").

:- pragma(c_code,
	uniq_array__make_empty_array(UniqArray::uniq_array_uo),
"
	UniqArray = (Word) ML_make_uniq_array(0, 0);
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
	Max = ((MR_UniqArrayType *)UniqArray)->size - 1;
").
:- pragma(c_code, uniq_array__max(UniqArray::in, Max::out), "
	Max = ((MR_UniqArrayType *)UniqArray)->size - 1;
").

uniq_array__bounds(Array, Min, Max) :-
	uniq_array__min(Array, Min),
	uniq_array__max(Array, Max).

%-----------------------------------------------------------------------------%

:- pragma(c_code, uniq_array__size(UniqArray::uniq_array_ui, Max::out), "
	Max = ((MR_UniqArrayType *)UniqArray)->size;
").
:- pragma(c_code, uniq_array__size(UniqArray::in, Max::out), "
	Max = ((MR_UniqArrayType *)UniqArray)->size;
").

%-----------------------------------------------------------------------------%

uniq_array__in_bounds(Array, Index) :-
	uniq_array__bounds(Array, Min, Max),
	Min =< Index, Index =< Max.

uniq_array__semidet_lookup(UniqArray, Index, Item) :-
	uniq_array__in_bounds(UniqArray, Index),
	uniq_array__lookup(UniqArray, Index, Item).

uniq_array__semidet_set(UniqArray0, Index, Item, UniqArray) :-
	uniq_array__in_bounds(UniqArray0, Index),
	uniq_array__set(UniqArray0, Index, Item, UniqArray).

uniq_array__semidet_slow_set(UniqArray0, Index, Item, UniqArray) :-
	uniq_array__in_bounds(UniqArray0, Index),
	uniq_array__slow_set(UniqArray0, Index, Item, UniqArray).

uniq_array__slow_set(UniqArray0, Index, Item, UniqArray) :-
	uniq_array__copy(UniqArray0, UniqArray1),
	uniq_array__set(UniqArray1, Index, Item, UniqArray).

%-----------------------------------------------------------------------------%

:- pragma(c_code, uniq_array__lookup(UniqArray::uniq_array_ui, Index::in,
		Item::out), "{
	MR_UniqArrayType *uniq_array = (MR_UniqArrayType *)UniqArray;
	if ((Unsigned) Index >= (Unsigned) uniq_array->size) {
		fatal_error(""uniq_array__lookup: array index out of bounds"");
	}
	Item = uniq_array->elements[Index];
}").
:- pragma(c_code, uniq_array__lookup(UniqArray::in, Index::in, Item::out), "{
	MR_UniqArrayType *uniq_array = (MR_UniqArrayType *)UniqArray;
	if ((Unsigned) Index >= (Unsigned) uniq_array->size) {
		fatal_error(""uniq_array__lookup: array index out of bounds"");
	}
	Item = uniq_array->elements[Index];
}").

%-----------------------------------------------------------------------------%

:- pragma(c_code, uniq_array__set(UniqArray0::uniq_array_di, Index::in,
		Item::in, UniqArray::uniq_array_uo), "{
	MR_UniqArrayType *uniq_array = (MR_UniqArrayType *)UniqArray0;
	if ((Unsigned) Index >= (Unsigned) uniq_array->size) {
		fatal_error(""uniq_array__set: array index out of bounds"");
	}
	uniq_array->elements[Index] = Item;	/* destructive update! */
	UniqArray = UniqArray0;
}").

%-----------------------------------------------------------------------------%

:- pragma(c_header_code, "
MR_UniqArrayType * ML_resize_uniq_array(MR_UniqArrayType *old_array,
					Integer array_size, Word item);
").

:- pragma(c_code, "
MR_UniqArrayType *
ML_resize_uniq_array(MR_UniqArrayType *old_array, Integer array_size,
				Word item)
{
	Integer i;
	MR_UniqArrayType* array;
	Integer old_array_size;

	old_array_size = old_array->size;
	if (old_array_size == array_size) return old_array;

	array = (MR_UniqArrayType *) make_many(Word, array_size + 1);
	array->size = array_size;
	for (i = 0; i < old_array_size; i++) {
		array->elements[i] = old_array->elements[i];
	}
	for (; i < array_size; i++) {
		array->elements[i] = item;
	}

	/*
	** since the mode on the old array is `uniq_array_di', it is safe to
	** deallocate the storage for it
	*/
	oldmem(old_array);

	return array;
}
").

:- pragma(c_code,
	uniq_array__resize(UniqArray0::uniq_array_di, Size::in, Item::in,
		UniqArray::uniq_array_uo),
"
	UniqArray = (Word) ML_resize_uniq_array(
				(MR_UniqArrayType *) UniqArray0, Size, Item);
").

%-----------------------------------------------------------------------------%

:- pragma(c_header_code, "
MR_UniqArrayType *ML_copy_uniq_array(MR_UniqArrayType *old_array);
").

:- pragma(c_code, "
MR_UniqArrayType *
ML_copy_uniq_array(MR_UniqArrayType *old_array)
{
	/*
	** Any changes to this function will probably also require
	** changes to deepcopy() in runtime/deep_copy.c.
	*/

	Integer i;
	MR_UniqArrayType* array;
	Integer array_size;

	array_size = old_array->size;
	array = MR_make_uniq_array(array_size);
	array->size = array_size;
	for (i = 0; i < array_size; i++) {
		array->elements[i] = old_array->elements[i];
	}
	return array;
}
").

:- pragma(c_code,
	uniq_array__copy(UniqArray0::uniq_array_ui, UniqArray::uniq_array_uo),
"
	UniqArray =
		(Word) ML_copy_uniq_array((MR_UniqArrayType *) UniqArray0);
").

:- pragma(c_code,
	uniq_array__copy(UniqArray0::in, UniqArray::uniq_array_uo),
"
	UniqArray =
		(Word) ML_copy_uniq_array((MR_UniqArrayType *) UniqArray0);
").

%-----------------------------------------------------------------------------%

uniq_array(List) = Array :-
	uniq_array__from_list(List, Array).

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
	        % and check against that.
	        Mid is (Lo + Hi) >> 1,	% `>> 1' is hand-optimized `div 2'.
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
