%-----------------------------------------------------------------------------%
% Copyright (C) 1993-1995, 1997-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: array.m
% Main authors: fjh, bromage
% Stability: medium-low

% This module provides dynamically-sized one-dimensional arrays.
% Array indices start at zero.

% By default, the array__set and array__lookup procedures will check
% for bounds errors.  But for better performance, it is possible to
% disable some of the checking by compiling with `--intermodule-optimization'
% and with the C macro symbol `ML_OMIT_ARRAY_BOUNDS_CHECKS'
% defined, e.g. by using `MCFLAGS=--intermodule-optimization' and
% `CFLAGS=-DML_OMIT_ARRAY_BOUNDS_CHECKS' in your Mmakefile,
% or by compiling with the command
% `mmc --intermodule-optimization --cflags -DML_OMIT_ARRAY_BOUNDS_CHECKS'.
%
% For maximum performance, all bounds checking can be disabled by
% recompiling this module using `CFLAGS=-DML_OMIT_ARRAY_BOUNDS_CHECKS'
% or `mmc --cflags -DML_OMIT_ARRAY_BOUNDS_CHECKS' as above. You can
% either recompile the entire library, or just copy `array.m' to your
% application's source directory and link with it directly instead of as
% part of the library.
%

% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%	Function forms added.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module array.
:- interface.
:- import_module list, std_util.

:- type array(T).

:- inst array(I) = bound(array(I)).
:- inst array == array(ground).
:- inst array_skel == array(free).

	% XXX the current Mercury compiler doesn't support `ui' modes,
	% so to work-around that problem, we currently don't use
	% unique modes in this module.

% :- inst uniq_array(I) = unique(array(I)).
% :- inst uniq_array == uniq_array(unique).
:- inst uniq_array(I) = bound(array(I)). % XXX work-around
:- inst uniq_array == uniq_array(ground). % XXX work-around
:- inst uniq_array_skel == uniq_array(free).

:- mode array_di == di(uniq_array).
:- mode array_uo == out(uniq_array).
:- mode array_ui == in(uniq_array).

% :- inst mostly_uniq_array(I) = mostly_unique(array(I)).
% :- inst mostly_uniq_array == mostly_uniq_array(mostly_unique).
:- inst mostly_uniq_array(I) = bound(array(I)).	% XXX work-around
:- inst mostly_uniq_array == mostly_uniq_array(ground).	% XXX work-around
:- inst mostly_uniq_array_skel == mostly_uniq_array(free).

:- mode array_mdi == mdi(mostly_uniq_array).
:- mode array_muo == out(mostly_uniq_array).
:- mode array_mui == in(mostly_uniq_array).

%-----------------------------------------------------------------------------%

	% array__make_empty_array(Array) creates an array of size zero
	% starting at lower bound 0.
:- pred array__make_empty_array(array(T)).
:- mode array__make_empty_array(array_uo) is det.

	% array__init(Size, Init, Array) creates an array
	% with bounds from 0 to Size-1, with each element initialized to Init.
:- pred array__init(int, T, array(T)).
:- mode array__init(in, in, array_uo) is det.

	% array/1 is a function that constructs an array from a list.
	% (It does the same thing as the predicate array__from_list/2.)
	% The syntax `array([...])' is used to represent arrays
	% for io__read, io__write, term_to_type, and type_to_term.
:- func array(list(T)) = array(T).
:- mode array(in) = array_uo is det.

%-----------------------------------------------------------------------------%

	% array__min returns the lower bound of the array.
	% Note: in this implementation, the lower bound is always zero.
:- pred array__min(array(_T), int).
:- mode array__min(array_ui, out) is det.
:- mode array__min(in, out) is det.

	% array__max returns the upper bound of the array.
:- pred array__max(array(_T), int).
:- mode array__max(array_ui, out) is det.
:- mode array__max(in, out) is det.

	% array__size returns the length of the array,
	% i.e. upper bound - lower bound + 1.
:- pred array__size(array(_T), int).
:- mode array__size(array_ui, out) is det.
:- mode array__size(in, out) is det.

	% array__bounds returns the upper and lower bounds of an array.
	% Note: in this implementation, the lower bound is always zero.
:- pred array__bounds(array(_T), int, int).
:- mode array__bounds(array_ui, out, out) is det.
:- mode array__bounds(in, out, out) is det.

	% array__in_bounds checks whether an index is in the bounds
	% of an array.
:- pred array__in_bounds(array(_T), int).
:- mode array__in_bounds(array_ui, in) is semidet.
:- mode array__in_bounds(in, in) is semidet.

%-----------------------------------------------------------------------------%

	% array__lookup returns the Nth element of an array.
	% It is an error if the index is out of bounds.
:- pred array__lookup(array(T), int, T).
:- mode array__lookup(array_ui, in, out) is det.
:- mode array__lookup(in, in, out) is det.

	% array__semidet_lookup returns the Nth element of an array.
	% It fails if the index is out of bounds.
:- pred array__semidet_lookup(array(T), int, T).
:- mode array__semidet_lookup(array_ui, in, out) is semidet.
:- mode array__semidet_lookup(in, in, out) is semidet.

	% array__set sets the nth element of an array, and returns the
	% resulting array (good opportunity for destructive update ;-).  
	% It is an error if the index is out of bounds.
:- pred array__set(array(T), int, T, array(T)).
:- mode array__set(array_di, in, in, array_uo) is det.


	% array__semidet_set sets the nth element of an array,
	% and returns the resulting array.
	% It fails if the index is out of bounds.
:- pred array__semidet_set(array(T), int, T, array(T)).
:- mode array__semidet_set(array_di, in, in, array_uo) is semidet.

	% array__slow_set sets the nth element of an array,
	% and returns the resulting array.  The initial array is not
	% required to be unique, so the implementation may not be able to use
	% destructive update.
	% It is an error if the index is out of bounds.
:- pred array__slow_set(array(T), int, T, array(T)).
:- mode array__slow_set(array_ui, in, in, array_uo) is det.
:- mode array__slow_set(in, in, in, array_uo) is det.

	% array__semidet_slow_set sets the nth element of an array,
	% and returns the resulting array.  The initial array is not
	% required to be unique, so the implementation may not be able to use
	% destructive update.
	% It fails if the index is out of bounds.
:- pred array__semidet_slow_set(array(T), int, T, array(T)).
:- mode array__semidet_slow_set(array_ui, in, in, array_uo) is semidet.
:- mode array__semidet_slow_set(in, in, in, array_uo) is semidet.

	% array__copy(Array0, Array):
	% Makes a new unique copy of an array.
:- pred array__copy(array(T), array(T)).
:- mode array__copy(array_ui, array_uo) is det.
:- mode array__copy(in, array_uo) is det.

	% array__resize(Array0, Size, Init, Array):
	% The array is expanded or shrunk to make it fit
	% the new size `Size'.  Any new entries are filled
	% with `Init'.
:- pred array__resize(array(T), int, T, array(T)).
:- mode array__resize(array_di, in, in, array_uo) is det.

	% array__shrink(Array0, Size, Array):
	% The array is shrunk to make it fit the new size `Size'.
	% It is an error if `Size' is larger than the size of `Array0'.
:- pred array__shrink(array(T), int, array(T)).
:- mode array__shrink(array_di, in, array_uo) is det.


	% array__from_list takes a list,
	% and returns an array containing those elements in
	% the same order that they occured in the list.
:- pred array__from_list(list(T), array(T)).
:- mode array__from_list(in, array_uo) is det.

	% array__to_list takes an array and returns a list containing
	% the elements of the array in the same order that they
	% occurred in the array.
:- pred array__to_list(array(T), list(T)).
:- mode array__to_list(array_ui, out) is det.
:- mode array__to_list(in, out) is det.

	% array__fetch_items takes an array and a lower and upper
	% index, and places those items in the array between these
	% indices into a list.  It is an error if either index is
	% out of bounds.
:- pred array__fetch_items(array(T), int, int, list(T)).
:- mode array__fetch_items(in, in, in, out) is det.


	% array__bsearch takes an array, an element to be found
	% and a comparison predicate and returns the position of
	% the element in the array.  Assumes the array is in sorted
	% order.  Fails if the element is not present.  If the
	% element to be found appears multiple times, the index of
	% the first occurrence is returned.
:- pred array__bsearch(array(T), T, pred(T, T, comparison_result),
			maybe(int)).
:- mode array__bsearch(array_ui, in, pred(in, in, out) is det, out) is det.
:- mode array__bsearch(in, in, pred(in, in, out) is det, out) is det.

	% array__map(Closure, OldArray, NewArray) applys `Closure' to
	% each of the elements of `OldArray' to create `NewArray'.
:- pred array__map(pred(T1, T2), array(T1), array(T2)).
:- mode array__map(pred(in, out) is det, array_di, array_uo) is det.

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

	% unify/2 for arrays

:- pred array_equal(array(T), array(T)).
:- mode array_equal(in, in) is semidet.

	% compare/3 for arrays

:- pred array_compare(comparison_result, array(T), array(T)).
:- mode array_compare(out, in, in) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int.

:- type array(T).

/****
lower bounds other than zero are not supported
	% array__resize takes an array and new lower and upper bounds.
	% the array is expanded or shrunk at each end to make it fit
	% the new bounds.
:- pred array__resize(array(T), int, int, array(T)).
:- mode array__resize(in, in, in, out) is det.
****/

%-----------------------------------------------------------------------------%

% Arrays are implemented using the C interface.

% The C type which defines the representation of arrays is
% MR_ArrayType; it is defined in runtime/mercury_library_types.h.

%-----------------------------------------------------------------------------%

:- pragma c_code("

Define_extern_entry(mercury____Unify___array__array_1_0);
Define_extern_entry(mercury____Index___array__array_1_0);
Define_extern_entry(mercury____Compare___array__array_1_0);

MR_MODULE_STATIC_OR_EXTERN 
const struct mercury_data_array__type_ctor_functors_array_1_struct
	mercury_data_array__type_ctor_functors_array_1;
MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data_array__type_ctor_layout_array_1_struct
	mercury_data_array__type_ctor_layout_array_1;
MR_STATIC_CODE_CONST struct MR_TypeCtorInfo_struct
mercury_data_array__type_ctor_info_array_1 = {
	(Integer) 1,
	MR_MAYBE_STATIC_CODE(ENTRY(mercury____Unify___array__array_1_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury____Index___array__array_1_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury____Compare___array__array_1_0)),
	MR_TYPECTOR_REP_ARRAY,
	(Word *) &mercury_data_array__type_ctor_functors_array_1,
	(Word *) &mercury_data_array__type_ctor_layout_array_1,
	string_const(""array"", 5),
	string_const(""array"", 5),
	MR_RTTI_VERSION
};

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data_array__type_ctor_layout_array_1_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data_array__type_ctor_layout_array_1 = {
	make_typelayout_for_all_tags(TYPE_CTOR_LAYOUT_CONST_TAG, 
		MR_mkbody(MR_TYPE_CTOR_LAYOUT_ARRAY_VALUE))
};

MR_MODULE_STATIC_OR_EXTERN
const struct mercury_data_array__type_ctor_functors_array_1_struct {
	Integer f1;
} mercury_data_array__type_ctor_functors_array_1 = {
	MR_TYPE_CTOR_FUNCTORS_SPECIAL
};

Declare_entry(mercury__array__array_equal_2_0);
Declare_entry(mercury__array__array_compare_3_0);

BEGIN_MODULE(array_module_builtins)
	init_entry(mercury____Unify___array__array_1_0);
	init_entry(mercury____Index___array__array_1_0);
	init_entry(mercury____Compare___array__array_1_0);
BEGIN_CODE

Define_entry(mercury____Unify___array__array_1_0);
	/* this is implemented in Mercury, not hand-coded low-level C */
	tailcall(ENTRY(mercury__array__array_equal_2_0),
		ENTRY(mercury____Unify___array__array_1_0));

Define_entry(mercury____Index___array__array_1_0);
	r1 = -1;
	proceed();

Define_entry(mercury____Compare___array__array_1_0);
	/* this is implemented in Mercury, not hand-coded low-level C */
	tailcall(ENTRY(mercury__array__array_compare_3_0),
		ENTRY(mercury____Compare___array__array_1_0));

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_array_module_builtins
*/

MR_MODULE_STATIC_OR_EXTERN ModuleFunc array_module_builtins;

void sys_init_array_module_builtins(void);
		/* suppress gcc -Wmissing-decl warning */
void sys_init_array_module_builtins(void) {
	array_module_builtins();
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_array__type_ctor_info_array_1,
		array__array_1_0);
}

").

%-----------------------------------------------------------------------------%

	% unify/2 for arrays

array_equal(Array1, Array2) :-
	array__size(Array1, Size),
	array__size(Array2, Size),
	array__equal_elements(0, Size, Array1, Array2).

:- pred array__equal_elements(int, int, array(T), array(T)).
:- mode array__equal_elements(in, in, in, in) is semidet.

array__equal_elements(N, Size, Array1, Array2) :-
	( N = Size ->
		true
	;
		array__lookup(Array1, N, Elem),
		array__lookup(Array2, N, Elem),
		N1 is N + 1,
		array__equal_elements(N1, Size, Array1, Array2)
	).

	% compare/3 for arrays

array_compare(Result, Array1, Array2) :-
	array__size(Array1, Size1),
	array__size(Array2, Size2),
	compare(SizeResult, Size1, Size2),
	( SizeResult = (=) ->
		array__compare_elements(0, Size1, Array1, Array2, Result)
	;
		Result = SizeResult
	).

:- pred array__compare_elements(int, int, array(T), array(T),
			comparison_result).
:- mode array__compare_elements(in, in, in, in, out) is det.

array__compare_elements(N, Size, Array1, Array2, Result) :-
	( N = Size ->
		Result = (=)
	;
		array__lookup(Array1, N, Elem1),
		array__lookup(Array2, N, Elem2),
		compare(ElemResult, Elem1, Elem2),
		( ElemResult = (=) ->
			N1 is N + 1,
			array__compare_elements(N1, Size, Array1, Array2,
				Result)
		;
			Result = ElemResult
		)
	).

%-----------------------------------------------------------------------------%

:- pragma c_header_code("
MR_ArrayType *ML_make_array(Integer size, Word item);
").

:- pragma c_code("
MR_ArrayType *
ML_make_array(Integer size, Word item)
{
	Integer i;
	MR_ArrayType *array;

	array = MR_make_array(size);
	array->size = size;
	for (i = 0; i < size; i++) {
		array->elements[i] = item;
	}
	return array;
}
").

:- pragma c_code(array__init(Size::in, Item::in, Array::array_uo),
		[will_not_call_mercury, thread_safe], "
	Array = (Word) ML_make_array(Size, Item);
").

:- pragma c_code(array__make_empty_array(Array::array_uo),
		[will_not_call_mercury, thread_safe], "
	Array = (Word) ML_make_array(0, 0);
").

%-----------------------------------------------------------------------------%

:- pragma c_code(array__min(Array::array_ui, Min::out),
		[will_not_call_mercury, thread_safe], "
	/* Array not used */
	Min = 0;
").
:- pragma c_code(array__min(Array::in, Min::out),
		[will_not_call_mercury, thread_safe], "
	/* Array not used */
	Min = 0;
").

:- pragma c_code(array__max(Array::array_ui, Max::out), 
		[will_not_call_mercury, thread_safe], "
	Max = ((MR_ArrayType *)Array)->size - 1;
").
:- pragma c_code(array__max(Array::in, Max::out), 
		[will_not_call_mercury, thread_safe], "
	Max = ((MR_ArrayType *)Array)->size - 1;
").

array__bounds(Array, Min, Max) :-
	array__min(Array, Min),
	array__max(Array, Max).

%-----------------------------------------------------------------------------%

:- pragma c_code(array__size(Array::array_ui, Max::out),
		[will_not_call_mercury, thread_safe], "
	Max = ((MR_ArrayType *)Array)->size;
").
:- pragma c_code(array__size(Array::in, Max::out),
		[will_not_call_mercury, thread_safe], "
	Max = ((MR_ArrayType *)Array)->size;
").

%-----------------------------------------------------------------------------%

array__in_bounds(Array, Index) :-
	array__bounds(Array, Min, Max),
	Min =< Index, Index =< Max.

array__semidet_lookup(Array, Index, Item) :-
	array__in_bounds(Array, Index),
	array__lookup(Array, Index, Item).

array__semidet_set(Array0, Index, Item, Array) :-
	array__in_bounds(Array0, Index),
	array__set(Array0, Index, Item, Array).

array__semidet_slow_set(Array0, Index, Item, Array) :-
	array__in_bounds(Array0, Index),
	array__slow_set(Array0, Index, Item, Array).

array__slow_set(Array0, Index, Item, Array) :-
	array__copy(Array0, Array1),
	array__set(Array1, Index, Item, Array).

%-----------------------------------------------------------------------------%

:- pragma c_code(array__lookup(Array::array_ui, Index::in, Item::out),
		[will_not_call_mercury, thread_safe], "{
	MR_ArrayType *array = (MR_ArrayType *)Array;
#ifndef ML_OMIT_ARRAY_BOUNDS_CHECKS
	if ((Unsigned) Index >= (Unsigned) array->size) {
		fatal_error(""array__lookup: array index out of bounds"");
	}
#endif
	Item = array->elements[Index];
}").
:- pragma c_code(array__lookup(Array::in, Index::in, Item::out),
		[will_not_call_mercury, thread_safe], "{
	MR_ArrayType *array = (MR_ArrayType *)Array;
#ifndef ML_OMIT_ARRAY_BOUNDS_CHECKS
	if ((Unsigned) Index >= (Unsigned) array->size) {
		fatal_error(""array__lookup: array index out of bounds"");
	}
#endif
	Item = array->elements[Index];
}").

%-----------------------------------------------------------------------------%

:- pragma c_code(array__set(Array0::array_di, Index::in,
		Item::in, Array::array_uo),
		[will_not_call_mercury, thread_safe], "{
	MR_ArrayType *array = (MR_ArrayType *)Array0;
#ifndef ML_OMIT_ARRAY_BOUNDS_CHECKS
	if ((Unsigned) Index >= (Unsigned) array->size) {
		fatal_error(""array__set: array index out of bounds"");
	}
#endif
	array->elements[Index] = Item;	/* destructive update! */
	Array = Array0;
}").

%-----------------------------------------------------------------------------%

:- pragma c_header_code("
MR_ArrayType * ML_resize_array(MR_ArrayType *old_array,
					Integer array_size, Word item);
").

:- pragma c_code("
MR_ArrayType *
ML_resize_array(MR_ArrayType *old_array, Integer array_size,
				Word item)
{
	Integer i;
	MR_ArrayType* array;
	Integer elements_to_copy;

	elements_to_copy = old_array->size;
	if (elements_to_copy == array_size) return old_array;
	if (elements_to_copy > array_size) {
		elements_to_copy = array_size;
	}

	array = (MR_ArrayType *) MR_GC_NEW_ARRAY(Word, array_size + 1);
	array->size = array_size;
	for (i = 0; i < elements_to_copy; i++) {
		array->elements[i] = old_array->elements[i];
	}
	for (; i < array_size; i++) {
		array->elements[i] = item;
	}

	/*
	** since the mode on the old array is `array_di', it is safe to
	** deallocate the storage for it
	*/
	MR_GC_free(old_array);

	return array;
}
").

:- pragma c_code(array__resize(Array0::array_di, Size::in, Item::in,
		Array::array_uo), [will_not_call_mercury, thread_safe], "
	Array = (Word) ML_resize_array(
				(MR_ArrayType *) Array0, Size, Item);
").

%-----------------------------------------------------------------------------%

:- pragma c_header_code("
MR_ArrayType * ML_shrink_array(MR_ArrayType *old_array,
					Integer array_size);
").

:- pragma c_code("
MR_ArrayType *
ML_shrink_array(MR_ArrayType *old_array, Integer array_size)
{
	Integer i;
	MR_ArrayType* array;
	Integer old_array_size;

	old_array_size = old_array->size;
	if (old_array_size == array_size) return old_array;
	if (old_array_size < array_size) {
		fatal_error(""array__shrink: can't shrink to a larger size"");
	}

	array = (MR_ArrayType *) MR_GC_NEW_ARRAY(Word, array_size + 1);
	array->size = array_size;
	for (i = 0; i < array_size; i++) {
		array->elements[i] = old_array->elements[i];
	}

	/*
	** since the mode on the old array is `array_di', it is safe to
	** deallocate the storage for it
	*/
	MR_GC_free(old_array);

	return array;
}
").

:- pragma c_code(array__shrink(Array0::array_di, Size::in, Array::array_uo),
		[will_not_call_mercury, thread_safe], "
	Array = (Word) ML_shrink_array(
				(MR_ArrayType *) Array0, Size);
").

%-----------------------------------------------------------------------------%

:- pragma c_header_code("
MR_ArrayType *ML_copy_array(MR_ArrayType *old_array);
").

:- pragma c_code("
MR_ArrayType *
ML_copy_array(MR_ArrayType *old_array)
{
	/*
	** Any changes to this function will probably also require
	** changes to deepcopy() in runtime/deep_copy.c.
	*/

	Integer i;
	MR_ArrayType* array;
	Integer array_size;

	array_size = old_array->size;
	array = MR_make_array(array_size);
	array->size = array_size;
	for (i = 0; i < array_size; i++) {
		array->elements[i] = old_array->elements[i];
	}
	return array;
}
").

:- pragma c_code(array__copy(Array0::array_ui, Array::array_uo),
		[will_not_call_mercury, thread_safe], "
	Array = (Word) ML_copy_array((MR_ArrayType *) Array0);
").

:- pragma c_code(array__copy(Array0::in, Array::array_uo),
		[will_not_call_mercury, thread_safe], "
	Array = (Word) ML_copy_array((MR_ArrayType *) Array0);
").

%-----------------------------------------------------------------------------%

array(List) = Array :-
	array__from_list(List, Array).

array__from_list([], Array) :-
	array__make_empty_array(Array).
array__from_list(List, Array) :-
        List = [ Head | Tail ],
        list__length(List, Len),
        array__init(Len, Head, Array0),
        array__insert_items(Tail, 1, Array0, Array).

%-----------------------------------------------------------------------------%

:- pred array__insert_items(list(T), int, array(T), array(T)).
:- mode array__insert_items(in, in, array_di, array_uo) is det.

array__insert_items([], _N, Array, Array).
array__insert_items([Head|Tail], N, Array0, Array) :-
        array__set(Array0, N, Head, Array1),
        N1 is N + 1,
        array__insert_items(Tail, N1, Array1, Array).

%-----------------------------------------------------------------------------%

array__to_list(Array, List) :-
        array__bounds(Array, Low, High),
        array__fetch_items(Array, Low, High, List).

%-----------------------------------------------------------------------------%

array__fetch_items(Array, Low, High, List) :-
        (
                Low > High
        ->
                List = []
        ;
                Low1 is Low + 1,
                array__fetch_items(Array, Low1, High, List0),
                array__lookup(Array, Low, Item),
                List = [Item|List0]
        ).

%-----------------------------------------------------------------------------%

array__bsearch(A, El, Compare, Result) :-
	array__bounds(A, Lo, Hi),
	array__bsearch_2(A, Lo, Hi, El, Compare, Result).

:- pred array__bsearch_2(array(T), int, int, T,
			pred(T, T, comparison_result), maybe(int)).
:- mode array__bsearch_2(in, in, in, in, pred(in, in, out) is det,
				out) is det.
array__bsearch_2(Array, Lo, Hi, El, Compare, Result) :-
	Width is Hi - Lo,

	% If Width < 0, there is no range left.
	( Width < 0 ->
	    Result = no
	;
	    % If Width == 0, we may just have found our element.
	    % Do a Compare to check.
	    ( Width = 0 ->
	        array__lookup(Array, Lo, X),
	        ( call(Compare, El, X, (=)) ->
		    Result = yes(Lo)
	        ;
		    Result = no
	        )
	    ;
	        % Otherwise find the middle element of the range
	        % and check against that.
	        Mid is (Lo + Hi) >> 1,	% `>> 1' is hand-optimized `div 2'.
	        array__lookup(Array, Mid, XMid),
	        call(Compare, XMid, El, Comp),
	        ( Comp = (<),
		    Mid1 is Mid + 1,
		    array__bsearch_2(Array, Mid1, Hi, El, Compare, Result)
	        ; Comp = (=),
		    array__bsearch_2(Array, Lo, Mid, El, Compare, Result)
	        ; Comp = (>),
		    Mid1 is Mid - 1,
		    array__bsearch_2(Array, Lo, Mid1, El, Compare, Result)
	        )
	    )
	).

%-----------------------------------------------------------------------------%

array__map(Closure, OldArray, NewArray) :-
	( array__semidet_lookup(OldArray, 0, Elem0) ->
		array__size(OldArray, Size),
		call(Closure, Elem0, Elem),
		array__init(Size, Elem, NewArray0),
		array__map_2(1, Size, Closure, OldArray,
		NewArray0, NewArray)
	;
		array__make_empty_array(NewArray)
	).

:- pred array__map_2(int, int, pred(T1, T2), array(T1), array(T2), array(T2)).
:- mode array__map_2(in, in, pred(in, out) is det, in, array_di, array_uo)
		is det.

array__map_2(N, Size, Closure, OldArray, NewArray0, NewArray) :-
	( N >= Size ->
		NewArray = NewArray0
	;
		array__lookup(OldArray, N, OldElem),
		Closure(OldElem, NewElem),
		array__set(NewArray0, N, NewElem, NewArray1),
		array__map_2(N + 1, Size, Closure, OldArray,
		NewArray1, NewArray)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%	Function forms added.

:- interface.

:- func array__make_empty_array = array(T).
:- mode array__make_empty_array = array_uo is det.

:- func array__init(int, T) = array(T).
:- mode array__init(in, in) = array_uo is det.

:- func array__min(array(_T)) = int.
:- mode array__min(array_ui) = out is det.

:- func array__max(array(_T)) = int.
:- mode array__max(array_ui) = out is det.

:- func array__size(array(_T)) = int.
:- mode array__size(array_ui) = out is det.

:- func array__lookup(array(T), int) = T.
:- mode array__lookup(array_ui, in) = out is det.

:- func array__set(array(T), int, T) = array(T).
:- mode array__set(array_di, in, in) = array_uo is det.

:- func array__slow_set(array(T), int, T) = array(T).
:- mode array__slow_set(array_ui, in, in) = array_uo is det.
:- mode array__slow_set(in, in, in) = array_uo is det.

:- func array__copy(array(T)) = array(T).
:- mode array__copy(array_ui) = array_uo is det.

:- func array__resize(array(T), int, T) = array(T).
:- mode array__resize(array_di, in, in) = array_uo is det.

:- func array__shrink(array(T), int) = array(T).
:- mode array__shrink(array_di, in) = array_uo is det.

:- func array__from_list(list(T)) = array(T).
:- mode array__from_list(in) = array_uo is det.

:- func array__to_list(array(T)) = list(T).
:- mode array__to_list(array_ui) = out is det.

:- func array__fetch_items(array(T), int, int) = list(T).
:- mode array__fetch_items(array_ui, in, in) = out is det.

:- func array__bsearch(array(T), T, func(T,T) = comparison_result) = maybe(int).
:- mode array__bsearch(array_ui, in, func(in,in) = out is det) = out is det.

:- func array__map(func(T1) = T2, array(T1)) = array(T2).
:- mode array__map(func(in) = out is det, array_di) = array_uo is det.

:- func array_compare(array(T), array(T)) = comparison_result.
:- mode array_compare(in, in) = out is det.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

array__make_empty_array = A :-
	array__make_empty_array(A).

array__init(N, X) = A :-
	array__init(N, X, A).

array__min(A) = N :-
	array__min(A, N).

array__max(A) = N :-
	array__max(A, N).

array__size(A) = N :-
	array__size(A, N).

array__lookup(A, N) = X :-
	array__lookup(A, N, X).

array__set(A1, N, X) = A2 :-
	array__set(A1, N, X, A2).

array__slow_set(A1, N, X) = A2 :-
	array__slow_set(A1, N, X, A2).

array__copy(A1) = A2 :-
	array__copy(A1, A2).

array__resize(A1, N, X) = A2 :-
	array__resize(A1, N, X, A2).

array__shrink(A1, N) = A2 :-
	array__shrink(A1, N, A2).

array__from_list(Xs) = A :-
	array__from_list(Xs, A).

array__to_list(A) = Xs :-
	array__to_list(A, Xs).

array__fetch_items(A, N1, N2) = Xs :-
	array__fetch_items(A, N1, N2, Xs).

array__bsearch(A, X, F) = MN :-
	P = ( pred(X1::in, X2::in, C::out) is det :- C = F(X1, X2) ),
	array__bsearch(A, X, P, MN).

array__map(F, A1) = A2 :-
	P = ( pred(X::in, Y::out) is det :- Y = F(X) ),
	array__map(P, A1, A2).

array_compare(A1, A2) = C :-
	array_compare(C, A1, A2).

