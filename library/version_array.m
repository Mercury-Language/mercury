%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury Team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: version_array.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>.
% Stability: medium.
%
% Version types are efficient pure implementations of typically imperative
% structures, subject to the following caveat: efficient access is only
% guaranteed for the "latest" version of a given structure. An older version
% incurs an access cost proportional to the number of its descendants.
%
% For example, if A0 is a version array, and A1 is created by updating A0,
% and A2 is created by updating A1, ..., and An is created by updating An-1,
% then accesses to An cost O(1) (assuming no further versions of the array
% have been created from An), but accesses to A0 cost O(n).
%
% Updates to older versions of the structure (for example A(n-1)) may have
% additional costs, for arrays this cost is O(m) where m is the size of the
% array, as the whole array is copied to make a new version array.
%
% Most version data structures come with impure, unsafe means to "rewind"
% to an earlier version, restoring that version's O(1) access times, but
% leaving later versions undefined (i.e. only do this if you are discarding
% all later versions of the structure).
%
% The motivation for using version types is that they are ordinary ground
% structures and do not depend upon uniqueness, while in many circumstances
% offering similar levels of performance.
%
% This module implements version arrays. A version array provides O(1)
% access and update for the "latest" version of the array. "Older"
% versions of the array incur an O(k) penalty on accesses where k is
% the number of updates that have been made since.
%
% The advantage of version arrays is that in the common, singly-threaded,
% case, they are almost as fast as unique arrays, but can be treated as
% ordinary ground values rather than unique values.
%
% Version arrays are zero based.
%
% NOTE_TO_IMPLEMENTORS XXX This implementation is not yet guaranteed to work
% NOTE_TO_IMPLEMENTORS with the agc (accurate garbage collection) grades.
% NOTE_TO_IMPLEMENTORS Specifically, MR_deep_copy and MR_agc_deep_copy
% NOTE_TO_IMPLEMENTORS currently do not recognise version arrays.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module version_array.
:- interface.

:- import_module list.
:- import_module pretty_printer.

%---------------------------------------------------------------------------%

:- type version_array(T).

    % An `index_out_of_bounds' is the exception thrown on out-of-bounds
    % array accesses. The string describes the predicate or function
    % reporting the error.
    %
:- type index_out_of_bounds
    --->    index_out_of_bounds(string).

%---------------------------------------------------------------------------%

    % empty returns the empty array.
    %
:- func empty = version_array(T).

    % init(Size, DefaultValue):
    % uinit(Size, DefaultValue):
    %
    % Return an array of size Size, with each item initialised to DefaultValue.
    %
:- func init(int, T) = version_array(T).
:- func uinit(uint, T) = version_array(T).

    % Same as empty/0, except the resulting version_array is not thread safe.
    %
    % That is, your program can crash or behave strangely if you attempt to
    % concurrently access or update the array from different threads, or any
    % two arrays produced from operations on the same original array.
    % However this version is much quicker if you guarantee that you never
    % concurrently access the version array.
    %
:- func unsafe_empty = version_array(T).

    % Same as init/uint, except the resulting version_array is not thread safe.
    %
    % The downside of this difference is that your program can crash,
    % or behave strangely, if you ever attempt to concurrently access
    % or update a non-thread-safe version_array from different threads.
    % This is also true for any two version_arrays that were generated
    % from operations on the same original non-thread-safe version_array.
    %
    % The upside is that if you can guarantee that your program will do
    % none of the above, then non-thread-safe version arrays are much quicker
    % to access and to update in multi-threaded programs.
    %
:- func unsafe_init(int, T) = version_array(T).
:- func unsafe_uinit(uint, T) = version_array(T).

    % version_array(Items):
    %
    % Returns an array constructed from the items in Items.
    %
:- func version_array(list(T)) = version_array(T).

    % A synonym for the above.
    %
:- func from_list(list(T)) = version_array(T).

    % from_reverse_list(Items):
    %
    % Returns an array constructed from the reverse of Items.
    %
:- func from_reverse_list(list(T)) = version_array(T).

    % lookup(VersionArray, Index) = Item:
    % ulookup(VersionArray, Index) = Item:
    %
    % Return, as Item, the Index'th member of VersionArray.
    % (The first item has index 0).
    %
:- func lookup(version_array(T), int) = T.
:- pred lookup(version_array(T)::in, int::in, T::out) is det.
:- func ulookup(version_array(T), uint) = T.
:- pred ulookup(version_array(T)::in, uint::in, T::out) is det.

    % VersionArray ^ elem(Index) = lookup(VersionArray, Index)
    % VersionArray ^ uelem(Index) = ulookup(VersionArray, Index)
    %
:- func elem(int, version_array(T)) = T.
:- func uelem(uint, version_array(T)) = T.

    % set(Index, Item, VersionArray0, VersionArray):
    % uset(Index, Item, VersionArray0, VersionArray):
    %
    % VersionArray is a copy of array VersionArray0,
    % with the item at Index replaced with Item.
    % Throws an exception if Index is out of bounds.
    %
:- pred set(int::in, T::in,
    version_array(T)::in, version_array(T)::out) is det.
:- pred uset(uint::in, T::in,
    version_array(T)::in, version_array(T)::out) is det.

    % (VersionArray0 ^ elem(Index) := Item) = VersionArray:
    % (VersionArray0 ^ uelem(Index) := Item) = VersionArray:
    %
    % is equivalent to set(Index, Item, VersionArray0, VersionArray).
    %
:- func 'elem :='(int, version_array(T), T) = version_array(T).
:- func 'uelem :='(uint, version_array(T), T) = version_array(T).

    % size(VersionArray) = Size:
    % usize(VersionArray) = Size:
    %
    % Return in Size the number of items in VersionArray (meaning that
    % the valid indices for VersionArray range from 0 to Size - 1).
    %
:- func size(version_array(T)) = int.
:- func usize(version_array(T)) = uint.

    % max(VersionArray) = size(VersionArray) - 1.
    %
    % Returns -1 for an empty array.
    %
:- func max(version_array(T)) = int.

    % is_empty(VersionArray):
    %
    % Succeeds if-and-only-if VersionArray is the empty array.
    %
:- pred is_empty(version_array(T)::in) is semidet.

    % resize(VersionArray0, NewSize, DefaultValue) = VersionArray:
    % resize(NewSize, DefaultValue, VersionArray0, VersionArray):
    % uresize(VersionArray0, NewSize, DefaultValue) = VersionArray:
    % uresize(NewSize, DefaultValue, VersionArray0, VersionArray):
    %
    % Return in VersionArray a new array whose size is NewSize.
    %
    % Each slot in VersionArray will be filled with the value from
    % the corresponding slot in VersionArray0 (if there is such a slot),
    % or with DefaultValue (if there is no such slot, due to NewSize
    % being greater than size(VersionArray0)).
    %
:- func resize(version_array(T), int, T) = version_array(T).
:- pred resize(int::in, T::in,
    version_array(T)::in, version_array(T)::out) is det.
:- func uresize(version_array(T), uint, T) = version_array(T).
:- pred uresize(uint::in, T::in,
    version_array(T)::in, version_array(T)::out) is det.

    % copy(VersionArray0) = VersionArray:
    %
    % Return in VersionArray a copy of VersionArray0.
    %
    % Access to the copy is O(1).
    %
:- func copy(version_array(T)) = version_array(T).

    % list(VersionArray) = Items:
    %
    % Return in Items the list of all items in VersionArray.
    % (Meaning that VersionArray = version_array(Items)).
    %
:- func list(version_array(T)) = list(T).

    % A synonym for the above.
    %
:- func to_list(version_array(T)) = list(T).

    % foldl(Func, VersionArray, Acc0) = Acc:
    %
    % Equivalent to list.foldl(Func, list(VersionArray), Acc0) = Acc,
    % but more efficient.
    %
:- func foldl(func(T, A) = A, version_array(T), A) = A.

    % foldl(Pred, VersionArray, !Acc):
    %
    % Equivalent to list.foldl(Pred, list(VersionArray), !Acc),
    % but more efficient.
    %
:- pred foldl(pred(T, A, A), version_array(T), A, A).
:- mode foldl(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldl(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldl(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldl(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

    % foldl2(Pred, VersionArray, !AccA, !AccB):
    %
    % Equivalent to list.foldl2(Pred, list(VersionArray), !AccA, !AccB),
    % but more efficient.
    %
:- pred foldl2(pred(T, A, A, B, B), version_array(T), A, A, B, B).
:- mode foldl2(in(pred(in, in, out, in, out) is det), in,
    in, out, in, out) is det.
:- mode foldl2(in(pred(in, in, out, mdi, muo) is det), in,
    in, out, mdi, muo) is det.
:- mode foldl2(in(pred(in, in, out, di, uo) is det), in,
    in, out, di, uo) is det.
:- mode foldl2(in(pred(in, in, out, in, out) is semidet), in,
    in, out, in, out) is semidet.
:- mode foldl2(in(pred(in, in, out, mdi, muo) is semidet), in,
    in, out, mdi, muo) is semidet.
:- mode foldl2(in(pred(in, in, out, di, uo) is semidet), in,
    in, out, di, uo) is semidet.

    % foldr(Func, VersionArray, Acc0) = Acc:
    %
    % Equivalent to list.foldr(Func, list(VersionArray), Acc0) = Acc,
    % but more efficient.
    %
:- func foldr(func(T, A) = A, version_array(T), A) = A.

:- pred foldr(pred(T, A, A), version_array(T), A, A).
:- mode foldr(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldr(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldr(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldr(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldr(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldr(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

:- pred foldr2(pred(T, A, A, B, B), version_array(T), A, A, B, B).
:- mode foldr2(in(pred(in, in, out, in, out) is det), in,
    in, out, in, out) is det.
:- mode foldr2(in(pred(in, in, out, mdi, muo) is det), in,
    in, out, mdi, muo) is det.
:- mode foldr2(in(pred(in, in, out, di, uo) is det), in,
    in, out, di, uo) is det.
:- mode foldr2(in(pred(in, in, out, in, out) is semidet), in,
    in, out, in, out) is semidet.
:- mode foldr2(in(pred(in, in, out, mdi, muo) is semidet), in,
    in, out, mdi, muo) is semidet.
:- mode foldr2(in(pred(in, in, out, di, uo) is semidet), in,
    in, out, di, uo) is semidet.

    % all_true(Pred, VersionArray):
    %
    % True if-and-only-if Pred is true for every element of VersionArray.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), version_array(T)::in)
    is semidet.

    % all_false(Pred, VersionArray):
    %
    % True if-and-only-if Pred is false for every element of VersionArray.
    %
:- pred all_false(pred(T)::in(pred(in) is semidet), version_array(T)::in)
    is semidet.

    % unsafe_rewind(VersionArray0) = VersionArray:
    % unsafe_rewind(VersionArray0, VersionArray):

    % Return as VersionArray a version of VersionArray0 for which
    % all accesses are O(1).
    %
    % Invoking this predicate renders undefined both VersionArray0 and
    % all later versions derived from it by performing individual updates.
    % Use this *only* when you are absolutely certain that there are
    % no live references to either.
    %
:- func unsafe_rewind(version_array(T)) = version_array(T).
:- pred unsafe_rewind(version_array(T)::in, version_array(T)::out) is det.

    % Convert a version_array to a pretty_printer.doc for formatting.
    %
:- func version_array_to_doc(version_array(T)) = pretty_printer.doc.
:- pragma obsolete(func(version_array_to_doc/1),
    [pretty_printer.version_array_to_doc/1]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

%---------------------------------------------------------------------------%

:- interface.

    % Succeeds if the version array has an internal lock to protect against
    % concurrent updates. This predicate is privately exported so that
    % version_hash_table does not need an extra flag to record whether the
    % version_array was created with a lock or not.
    %
:- pred has_lock(version_array(T)::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% The first implementation of version arrays used nb_references.
% This incurred three memory allocations for every update. This version
% works at a lower level, but only performs one allocation per update.

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    empty = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    MR_Word array;

    MR_incr_hp_type_msg(VA, struct ML_va,
        MR_ALLOC_ID, ""version_array.version_array/1"");
    MR_incr_hp_msg(array, 1,
        MR_ALLOC_ID, ""version_array.version_array/1"");

    VA->index            = -1;
    VA->value            = (MR_Word) NULL;
    VA->rest.array       = (MR_ArrayPtr) array;
    VA->rest.array->size = 0;

#ifdef MR_THREAD_SAFE
    MR_incr_hp_type_msg(VA->lock, MercuryLock, MR_ALLOC_ID, NULL);
    pthread_mutex_init(VA->lock, MR_MUTEX_ATTR);
#endif
").

:- pragma foreign_proc("C#",
    empty = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VA = new version_array.ML_sva(version_array.ML_uva.empty());
").

:- pragma foreign_proc("Java",
    empty = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VA = new jmercury.version_array.ML_sva(
        jmercury.version_array.ML_uva.empty());
").

:- pragma foreign_proc("C",
    unsafe_empty = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    MR_Word array;

    MR_incr_hp_type_msg(VA, struct ML_va,
        MR_ALLOC_ID, ""version_array.version_array/1"");
    MR_incr_hp_msg(array, 1,
        MR_ALLOC_ID, ""version_array.version_array/1"");

    VA->index            = -1;
    VA->value            = (MR_Word) NULL;
    VA->rest.array       = (MR_ArrayPtr) array;
    VA->rest.array->size = 0;

#ifdef MR_THREAD_SAFE
    VA->lock             = NULL;
#endif
").

:- pragma foreign_proc("C#",
    unsafe_empty = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VA = version_array.ML_uva.empty();
").

:- pragma foreign_proc("Java",
    unsafe_empty = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VA = jmercury.version_array.ML_uva.empty();
").

:- pragma foreign_proc("C",
    init(N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate],
"
    MR_Integer  i;
    MR_Word     array;

    MR_incr_hp_type_msg(VA, struct ML_va,
        MR_ALLOC_ID, ""version_array.version_array/1"");
    MR_incr_hp_msg(array, N + 1,
        MR_ALLOC_ID, ""version_array.version_array/1"");

    VA->index            = -1;
    VA->value            = (MR_Word) NULL;
    VA->rest.array       = (MR_ArrayPtr) array;
    VA->rest.array->size = N;

    for (i = 0; i < N; i++) {
        VA->rest.array->elements[i] = X;
    }

#ifdef MR_THREAD_SAFE
    MR_incr_hp_type_msg(VA->lock, MercuryLock, MR_ALLOC_ID, NULL);
    pthread_mutex_init(VA->lock, MR_MUTEX_ATTR);
#endif
").

:- pragma foreign_proc("C#",
    init(N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    VA = new version_array.ML_sva(version_array.ML_uva.init(N, X));
").

:- pragma foreign_proc("Java",
    init(N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    VA = new jmercury.version_array.ML_sva(
        jmercury.version_array.ML_uva.init(N, X));
").

:- pragma foreign_proc("C",
    unsafe_init(N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate],
"
    MR_Integer  i;
    MR_Word     array;

    MR_incr_hp_type_msg(VA, struct ML_va,
        MR_ALLOC_ID, ""version_array.version_array/1"");
    MR_incr_hp_msg(array,  N + 1,
        MR_ALLOC_ID, ""version_array.version_array/1"");

    VA->index            = -1;
    VA->value            = (MR_Word) NULL;
    VA->rest.array       = (MR_ArrayPtr) array;
    VA->rest.array->size = N;

    for (i = 0; i < N; i++) {
        VA->rest.array->elements[i] = X;
    }

#ifdef MR_THREAD_SAFE
    VA->lock             = NULL;
#endif
").

:- pragma foreign_proc("C#",
    unsafe_init(N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    VA = version_array.ML_uva.init(N, X);
").

:- pragma foreign_proc("Java",
    unsafe_init(N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    VA = jmercury.version_array.ML_uva.init(N, X);
").

uinit(Size, DefaultValue) = VersionArray :-
    init(uint.cast_to_int(Size), DefaultValue) = VersionArray.

unsafe_uinit(Size, DefaultValue) = VersionArray :-
    unsafe_init(uint.cast_to_int(Size), DefaultValue) = VersionArray.

%---------------------------------------------------------------------------%

version_array([]) = version_array.empty.
version_array([X | Xs]) = VA :-
    NumElems = 1 + list.length(Xs),
    VA0 = version_array.init(NumElems, X),
    version_array_init_loop(1, Xs, VA0, VA).

:- pred version_array_init_loop(int::in, list(T)::in,
    version_array(T)::in, version_array(T)::out) is det.

version_array_init_loop(_, [], !VA).
version_array_init_loop(I, [X | Xs], !VA) :-
    set(I, X, !VA),
    version_array_init_loop(I + 1, Xs, !VA).

from_list(Xs) = version_array(Xs).

from_reverse_list([]) = version_array.empty.
from_reverse_list([X | Xs]) = VA :-
    NumElems = 1 + list.length(Xs),
    VA0 = version_array.init(NumElems, X),
    from_reverse_list_init_loop(NumElems - 2, Xs, VA0, VA).

:- pred from_reverse_list_init_loop(int::in, list(T)::in,
    version_array(T)::in, version_array(T)::out) is det.

from_reverse_list_init_loop(_, [], !VA).
from_reverse_list_init_loop(I, [X | Xs], !VA) :-
    set(I, X, !VA),
    from_reverse_list_init_loop(I - 1, Xs, !VA).

%---------------------------------------------------------------------------%

lookup(VersionArray, I) = X :-
    lookup(VersionArray, I, X).

:- pragma inline(pred(lookup/3)).
lookup(VersionArray, I, X) :-
    ( if get_if_in_range(VersionArray, I, X0) then
        X = X0
    else
        out_of_bounds_error(I, max(VersionArray), "version_array.lookup")
    ).

ulookup(VersionArray, Index) = Item :-
    lookup(VersionArray, uint.cast_to_int(Index), Item).

ulookup(VersionArray, Index, Item) :-
    lookup(VersionArray, uint.cast_to_int(Index), Item).

:- pragma inline(func(elem/2)).
elem(Index, VersionArray) = Item :-
    lookup(VersionArray, Index, Item).

uelem(Index, VersionArray) = Item :-
    ulookup(VersionArray, Index, Item).

%---------------------------------------------------------------------------%

:- pragma inline(pred(set/4)).
set(Index, NewItem, !VersionArray) :-
    ( if set_if_in_range(Index, NewItem, !VersionArray) then
        true
    else
        out_of_bounds_error(Index, max(!.VersionArray), "version_array.set")
    ).

uset(Index, NewItem, !VersionArray) :-
    IndexI = uint.cast_to_int(Index),
    ( if set_if_in_range(IndexI, NewItem, !VersionArray) then
        true
    else
        out_of_bounds_error(IndexI, max(!.VersionArray), "version_array.uset")
    ).

:- pragma inline(func('elem :='/3)).
'elem :='(Index, VersionArray0, NewItem) = VersionArray :-
    set(Index, NewItem, VersionArray0, VersionArray).

'uelem :='(Index, VersionArray0, NewItem) = VersionArray :-
    uset(Index, NewItem, VersionArray0, VersionArray).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    size(VA::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    N = ML_va_size_dolock(VA);
").

:- pragma foreign_proc("C#",
    size(VA::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = VA.size();
").

:- pragma foreign_proc("Java",
    size(VA::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = VA.size();
").

usize(VersionArray) = Size :-
    SizeI = size(VersionArray),
    Size = uint.cast_from_int(SizeI).

max(VA) = size(VA) - 1.

is_empty(VA) :-
    size(VA) = 0.

:- pragma foreign_proc("C",
    resize(VA0::in, N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    VA = ML_va_resize_dolock(VA0, N, X, MR_ALLOC_ID);
").

:- pragma foreign_proc("C#",
    resize(VA0::in, N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    VA = VA0.resize(N, X);
").

:- pragma foreign_proc("Java",
    resize(VA0::in, N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    VA = VA0.resize(N, X);
").

resize(NewSize, DefaultValue, VersionArray0, VersionArray) :-
    resize(VersionArray0, NewSize, DefaultValue) = VersionArray.

uresize(VersionArray0, NewSize, DefaultValue) = VersionArray :-
    resize(uint.cast_to_int(NewSize), DefaultValue,
        VersionArray0, VersionArray).

uresize(NewSize, DefaultValue, VersionArray0, VersionArray) :-
    resize(uint.cast_to_int(NewSize), DefaultValue,
        VersionArray0, VersionArray).

%---------------------------------------------------------------------------%

copy(VA0) = VA :-
    Size = size(VA0),
    ( if Size = 0 then
        VA = VA0
    else
        lookup(VA0, 0, Init),
        resize(Size, Init, VA0, VA)
    ).

%---------------------------------------------------------------------------%

list(VA) = foldr(list.cons, VA, []).

to_list(VA) = list(VA).

%---------------------------------------------------------------------------%

foldl(F, VA, Acc0) = Acc :-
    do_foldl_func(F, VA, 0, size(VA), Acc0, Acc).

:- pred do_foldl_func((func(T, A) = A)::in,
    version_array(T)::in, int::in, int::in, A::in, A::out) is det.

do_foldl_func(F, VA, I, Size, !Acc) :-
    ( if I < Size then
        lookup(VA, I, X),
        !:Acc = F(X, !.Acc),
        do_foldl_func(F, VA, I + 1, Size, !Acc)
    else
        true
    ).

%---------------------------------------------------------------------------%

foldl(P, VA, !Acc) :-
    do_foldl_pred(P, VA, 0, size(VA), !Acc).

:- pred do_foldl_pred(pred(T, A, A), version_array(T), int, int, A, A).
:- mode do_foldl_pred(in(pred(in, in, out) is det),
    in, in, in, in, out) is det.
:- mode do_foldl_pred(in(pred(in, mdi, muo) is det),
    in, in, in, mdi, muo) is det.
:- mode do_foldl_pred(in(pred(in, di, uo) is det),
    in, in, in, di, uo) is det.
:- mode do_foldl_pred(in(pred(in, in, out) is semidet),
    in, in, in, in, out) is semidet.
:- mode do_foldl_pred(in(pred(in, mdi, muo) is semidet),
    in, in, in, mdi, muo) is semidet.
:- mode do_foldl_pred(in(pred(in, di, uo) is semidet),
    in, in, in, di, uo) is semidet.

do_foldl_pred(P, VA, I, Size, !Acc) :-
    ( if I < Size then
        lookup(VA, I, X),
        P(X, !Acc),
        do_foldl_pred(P, VA, I + 1, Size, !Acc)
    else
        true
    ).

%---------------------------------------------------------------------------%

foldl2(P, VA, !AccA, !AccB) :-
    do_foldl2(P, VA, 0, size(VA), !AccA, !AccB).

:- pred do_foldl2(pred(T, A, A, B, B), version_array(T), int, int,
    A, A, B, B).
:- mode do_foldl2(in(pred(in, in, out, in, out) is det), in, in, in,
    in, out, in, out) is det.
:- mode do_foldl2(in(pred(in, in, out, mdi, muo) is det), in, in, in,
    in, out, mdi, muo) is det.
:- mode do_foldl2(in(pred(in, in, out, di, uo) is det), in, in, in,
    in, out, di, uo) is det.
:- mode do_foldl2(in(pred(in, in, out, in, out) is semidet), in, in, in,
    in, out, in, out) is semidet.
:- mode do_foldl2(in(pred(in, in, out, mdi, muo) is semidet), in, in, in,
    in, out, mdi, muo) is semidet.
:- mode do_foldl2(in(pred(in, in, out, di, uo) is semidet), in, in, in,
    in, out, di, uo) is semidet.

do_foldl2(P, VA, I, Size, !AccA, !AccB) :-
    ( if I < Size then
        lookup(VA, I, X),
        P(X, !AccA, !AccB),
        do_foldl2(P, VA, I + 1, Size, !AccA, !AccB)
    else
        true
    ).

%---------------------------------------------------------------------------%

foldr(F, VA, Acc0) = Acc :-
    do_foldr_func(F, VA, size(VA) - 1, Acc0, Acc).

:- pred do_foldr_func((func(T, A) = A)::in, version_array(T)::in,
    int::in, A::in, A::out) is det.

do_foldr_func(F, VA, I, !Acc) :-
    ( if I >= 0 then
        lookup(VA, I, X),
        !:Acc = F(X, !.Acc),
        do_foldr_func(F, VA, I - 1, !Acc)
    else
        true
    ).

%---------------------------------------------------------------------------%

foldr(P, VA, !Acc) :-
    do_foldr_pred(P, VA, size(VA) - 1, !Acc).

:- pred do_foldr_pred(pred(T, A, A), version_array(T), int, A, A).
:- mode do_foldr_pred(in(pred(in, in, out) is det), in, in, in, out) is det.
:- mode do_foldr_pred(in(pred(in, mdi, muo) is det), in, in, mdi, muo) is det.
:- mode do_foldr_pred(in(pred(in, di, uo) is det),  in, in, di, uo) is det.
:- mode do_foldr_pred(in(pred(in, in, out) is semidet), in, in, in, out)
    is semidet.
:- mode do_foldr_pred(in(pred(in, mdi, muo) is semidet), in, in, mdi, muo)
    is semidet.
:- mode do_foldr_pred(in(pred(in, di, uo) is semidet), in, in, di, uo)
    is semidet.

do_foldr_pred(P, VA, I, !Acc) :-
    ( if I >= 0 then
        lookup(VA, I, X),
        P(X, !Acc),
        do_foldr_pred(P, VA, I - 1, !Acc)
    else
        true
    ).

%---------------------------------------------------------------------------%

foldr2(P, VA, !AccA, !AccB) :-
    do_foldr2(P, VA, size(VA) - 1, !AccA, !AccB).

:- pred do_foldr2(pred(T, A, A, B, B), version_array(T), int,
    A, A, B, B).
:- mode do_foldr2(in(pred(in, in, out, in, out) is det), in, in,
    in, out, in, out) is det.
:- mode do_foldr2(in(pred(in, in, out, mdi, muo) is det), in, in,
    in, out, mdi, muo) is det.
:- mode do_foldr2(in(pred(in, in, out, di, uo) is det), in, in,
    in, out, di, uo) is det.
:- mode do_foldr2(in(pred(in, in, out, in, out) is semidet), in, in,
    in, out, in, out) is semidet.
:- mode do_foldr2(in(pred(in, in, out, mdi, muo) is semidet), in, in,
    in, out, mdi, muo) is semidet.
:- mode do_foldr2(in(pred(in, in, out, di, uo) is semidet), in, in,
    in, out, di, uo) is semidet.

do_foldr2(P, VA, I, !AccA, !AccB) :-
    ( if I >= 0 then
        lookup(VA, I, X),
        P(X, !AccA, !AccB),
        do_foldr2(P, VA, I - 1, !AccA, !AccB)
    else
        true
    ).

%---------------------------------------------------------------------------%

all_true(Pred, VA) :-
    do_all_true(Pred, 0, size(VA), VA).

:- pred do_all_true(pred(T)::in(pred(in) is semidet), int::in, int::in,
    version_array(T)::in) is semidet.

do_all_true(Pred, I, N, VA) :-
    ( if I < N then
        lookup(VA, I, Elem),
        Pred(Elem),
        do_all_true(Pred, I + 1, N, VA)
    else
        true
    ).

all_false(Pred, VA) :-
    do_all_false(Pred, 0, size(VA), VA).

:- pred do_all_false(pred(T)::in(pred(in) is semidet), int::in, int::in,
    version_array(T)::in) is semidet.

do_all_false(Pred, I, N, VA) :-
    ( if I < N then
        lookup(VA, I, Elem),
        not Pred(Elem),
        do_all_false(Pred, I + 1, N, VA)
    else
        true
    ).

%---------------------------------------------------------------------------%

unsafe_rewind(VA, unsafe_rewind(VA)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% Sordid stuff below this point...
%
% The `thread_safe' attributes are justified:
% - creating new version arrays is thread-safe
% - thread-safe version arrays are protected by their own locks so do not need
%   the global lock
% - the whole point of providing non-thread-safe version arrays is to avoid
%   locking when the user "knows", and supposedly guarantees, that it is safe
%   to do so.

:- pragma foreign_type("C", version_array(T), "struct ML_va *",
        [can_pass_as_mercury_type])
    where
        equality   is eq_version_array,
        comparison is cmp_version_array.

:- pragma foreign_type("C#", version_array(T), "version_array.ML_va")
    where
        equality   is eq_version_array,
        comparison is cmp_version_array.

:- pragma foreign_type("Java", version_array(T),
        "jmercury.version_array.ML_va")
    where
        equality   is eq_version_array,
        comparison is cmp_version_array.

:- pred eq_version_array(version_array(T)::in, version_array(T)::in)
    is semidet.
:- pragma terminates(pred(eq_version_array/2)).

eq_version_array(VAa, VAb) :-
    N = size(VAa),
    N = size(VAb),
    eq_version_array_2(N - 1, VAa, VAb).

:- pred eq_version_array_2(int::in,
    version_array(T)::in, version_array(T)::in) is semidet.

eq_version_array_2(I, VAa, VAb) :-
    ( if I >= 0 then
        lookup(VAa, I, Elem),
        lookup(VAb, I, Elem),
        eq_version_array_2(I - 1, VAa, VAb)
    else
        true
    ).

:- pred cmp_version_array(comparison_result::uo,
    version_array(T)::in, version_array(T)::in) is det.
:- pragma terminates(pred(cmp_version_array/3)).

cmp_version_array(R, VAa, VAb) :-
    SizeA = VAa ^ size,
    SizeB = VAb ^ size,
    compare(SizeResult, SizeA, SizeB),
    (
        SizeResult = (=),
        cmp_version_array_2(0, SizeA, VAa, VAb, R)
    ;
        ( SizeResult = (<)
        ; SizeResult = (>)
        ),
        R = SizeResult
    ).

:- pred cmp_version_array_2(int::in, int::in, version_array(T)::in,
    version_array(T)::in, comparison_result::uo) is det.

cmp_version_array_2(I, Size, VAa, VAb, R) :-
    ( if I >= Size then
        R = (=)
      else
        lookup(VAa, I, ElemA),
        lookup(VAb, I, ElemB),
        compare(R0, ElemA, ElemB),
        (
            R0 = (=),
            cmp_version_array_2(I + 1, Size, VAa, VAb, R)
        ;
            ( R0 = (<)
            ; R0 = (>)
            ),
            R  = R0
        )
    ).

:- pred get_if_in_range(version_array(T)::in, int::in, T::out) is semidet.

:- pragma foreign_proc("C",
    get_if_in_range(VA::in, I::in, X::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = ML_va_get_dolock(VA, I, &X);
").

:- pragma foreign_proc("C#",
    get_if_in_range(VA::in, I::in, X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        X = VA.get(I);
        SUCCESS_INDICATOR = true;
    } catch (System.IndexOutOfRangeException) {
        X = null;
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("Java",
    get_if_in_range(VA::in, I::in, X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        X = VA.get(I);
        SUCCESS_INDICATOR = true;
    } catch (ArrayIndexOutOfBoundsException e) {
        X = null;
        SUCCESS_INDICATOR = false;
    }
").

:- pred set_if_in_range(int::in, T::in,
    version_array(T)::in, version_array(T)::out) is semidet.

:- pragma foreign_proc("C",
    set_if_in_range(I::in, X::in, VA0::in, VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = ML_va_set_dolock(VA0, I, X, &VA, MR_ALLOC_ID);
").

:- pragma foreign_proc("C#",
    set_if_in_range(I::in, X::in, VA0::in, VA::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        VA = VA0.set(I, X);
        SUCCESS_INDICATOR = true;
    } catch (System.IndexOutOfRangeException) {
        VA = null;
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("Java",
    set_if_in_range(I::in, X::in, VA0::in, VA::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        VA = VA0.set(I, X);
        SUCCESS_INDICATOR = true;
    } catch (ArrayIndexOutOfBoundsException e) {
        VA = null;
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("C",
    unsafe_rewind(VA0::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    VA = ML_va_rewind_dolock(VA0, MR_ALLOC_ID);
").

:- pragma foreign_proc("C#",
    unsafe_rewind(VA0::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VA = VA0.rewind();
").

:- pragma foreign_proc("Java",
    unsafe_rewind(VA0::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VA = VA0.rewind();
").

:- pragma foreign_decl("C", "
// If index is -1 then value is undefined and rest is the latest
// array value.
//
// Otherwise value is the overwritten value at index and rest is
// a pointer to the next version in the chain.

typedef struct ML_va        *ML_va_ptr;
typedef const struct ML_va  *ML_const_va_ptr;

struct ML_va {
    MR_Integer          index;  // -1 for latest, >= 0 for older
    MR_Word             value;  // Valid if index >= 0
    union {
        MR_ArrayPtr     array;  // Valid if index == -1
        ML_va_ptr       next;   // Valid if index >= 0
    } rest;
#ifdef MR_THREAD_SAFE
    MercuryLock         *lock;  // NULL or lock
#endif
};

// Returns a pointer to the latest version of the array.
extern ML_va_ptr
ML_va_get_latest(ML_const_va_ptr VA);

// Returns the number of items in a version array.
extern MR_Integer
ML_va_size_dolock(ML_const_va_ptr);

// If I is in range then ML_va_get(VA, I, &X) sets X to the I'th item
// in VA (counting from zero) and returns MR_TRUE. Otherwise it
// returns MR_FALSE.
extern MR_bool
ML_va_get_dolock(ML_const_va_ptr, MR_Integer, MR_Word *);

// If I is in range then ML_va_set(VA0, I, X, VA) sets VA to be VA0
// updated with the I'th item as X (counting from zero) and
// returns MR_TRUE. Otherwise it returns MR_FALSE.
extern MR_bool
ML_va_set_dolock(ML_va_ptr, MR_Integer, MR_Word, ML_va_ptr *,
    MR_AllocSiteInfoPtr);

// `Rewinds' a version array, invalidating all extant successors
// including the argument.
extern ML_va_ptr
ML_va_rewind_dolock(ML_va_ptr, MR_AllocSiteInfoPtr);

// Resize a version array.
extern ML_va_ptr
ML_va_resize_dolock(ML_va_ptr, MR_Integer, MR_Word, MR_AllocSiteInfoPtr);

").

:- pragma foreign_decl("C", local, "

#include ""mercury_types.h""
#include ""mercury_bitmap.h""

// Returns the number of items in a version array.
static MR_Integer
ML_va_size(ML_const_va_ptr);

// If I is in range then ML_va_get(VA, I, &X) sets X to the I'th item
// in VA (counting from zero) and returns MR_TRUE. Otherwise it
// returns MR_FALSE.
static MR_bool
ML_va_get(ML_const_va_ptr VA, MR_Integer I, MR_Word *Xptr);

// If I is in range then ML_va_set(VA0, I, X, VA) sets VA to be VA0
// updated with the I'th item as X (counting from zero) and
// returns MR_TRUE. Otherwise it returns MR_FALSE.
static MR_bool
ML_va_set(ML_va_ptr, MR_Integer, MR_Word, ML_va_ptr *,
    MR_AllocSiteInfoPtr alloc_id);

// Create a copy of VA0 as a new array.
static ML_va_ptr
ML_va_flat_copy(ML_const_va_ptr VA0, MR_AllocSiteInfoPtr alloc_id);

// Update the array VA using the override values in VA0
// i.e. recreate the state of the version array as captured in VA0.
static void
ML_va_rewind_into(ML_va_ptr VA, ML_const_va_ptr VA0,
    MR_AllocSiteInfoPtr alloc_id);

// `Rewinds' a version array, invalidating all extant successors
// including the argument.
static ML_va_ptr
ML_va_rewind(ML_va_ptr VA, MR_AllocSiteInfoPtr alloc_id);

// Resize a version array.
static ML_va_ptr
ML_va_resize(ML_va_ptr, MR_Integer, MR_Word, MR_AllocSiteInfoPtr);

").

:- pragma foreign_code("C", "

#define ML_va_latest_version(VA)   ((VA)->index == -1)

#ifdef MR_THREAD_SAFE
    #define ML_maybe_lock(lock)                         \\
        do {                                            \\
            if (lock) {                                 \\
                MR_LOCK(lock, ""ML_maybe_lock"");       \\
            }                                           \\
        } while (0)

    #define ML_maybe_unlock(lock)                       \\
        do {                                            \\
            if (lock) {                                 \\
                MR_UNLOCK(lock, ""ML_maybe_unlock"");   \\
            }                                           \\
        } while (0)
#else
    #define ML_maybe_lock(lock)     ((void) 0)
    #define ML_maybe_unlock(lock)   ((void) 0)
#endif

ML_va_ptr
ML_va_get_latest(ML_const_va_ptr VA)
{
    while (!ML_va_latest_version(VA)) {
        VA = VA->rest.next;
    }

    // Cast away the 'const'.
    return (ML_va_ptr)VA;
}

MR_Integer
ML_va_size_dolock(ML_const_va_ptr VA)
{
#ifdef MR_THREAD_SAFE
    MercuryLock *lock = VA->lock;
#endif
    MR_Integer  size;

    ML_maybe_lock(lock);

    size = ML_va_size(VA);

    ML_maybe_unlock(lock);

    return size;
}

static MR_Integer
ML_va_size(ML_const_va_ptr VA)
{
    VA = ML_va_get_latest(VA);

    return VA->rest.array->size;
}

int
ML_va_get_dolock(ML_const_va_ptr VA, MR_Integer I, MR_Word *Xptr)
{
#ifdef MR_THREAD_SAFE
    MercuryLock *lock = VA->lock;
#endif
    int         ret;

    ML_maybe_lock(lock);

    ret = ML_va_get(VA, I, Xptr);

    ML_maybe_unlock(lock);

    return ret;
}

static int
ML_va_get(ML_const_va_ptr VA, MR_Integer I, MR_Word *Xptr)
{
    while (!ML_va_latest_version(VA)) {
        if (I == VA->index) {
            *Xptr = VA->value;
            return MR_TRUE;
        }

        VA = VA->rest.next;
    }

    if (0 <= I && I < VA->rest.array->size) {
        *Xptr = VA->rest.array->elements[I];
        return MR_TRUE;
    } else {
        return MR_FALSE;
    }
}

int
ML_va_set_dolock(ML_va_ptr VA0, MR_Integer I, MR_Word X, ML_va_ptr *VAptr,
    MR_AllocSiteInfoPtr alloc_id)
{
#ifdef MR_THREAD_SAFE
    MercuryLock *lock = VA0->lock;
#endif
    int         ret;

    ML_maybe_lock(lock);

    ret = ML_va_set(VA0, I, X, VAptr, alloc_id);

    ML_maybe_unlock(lock);

    return ret;
}

static int
ML_va_set(ML_va_ptr VA0, MR_Integer I, MR_Word X, ML_va_ptr *VAptr,
    MR_AllocSiteInfoPtr alloc_id)
{
    ML_va_ptr VA1;

    if (ML_va_latest_version(VA0)) {
        if (I < 0 || I >= VA0->rest.array->size) {
            return MR_FALSE;
        }

        MR_incr_hp_type_msg(VA1, struct ML_va, alloc_id,
            ""version_array.version_array/1"");
        VA1->index      = -1;
        VA1->value      = (MR_Word) NULL;
        VA1->rest.array = VA0->rest.array;
#ifdef MR_THREAD_SAFE
        VA1->lock       = VA0->lock;
#endif

        VA0->index     = I;
        VA0->value     = VA0->rest.array->elements[I];
        VA0->rest.next = VA1;

        VA1->rest.array->elements[I] = X;
    } else {
        VA1 = ML_va_flat_copy(VA0, alloc_id);

        if (I < 0 || I >= VA1->rest.array->size) {
            return MR_FALSE;
        }

        VA1->rest.array->elements[I] = X;
    }

    *VAptr = VA1;
    return MR_TRUE;
}

static ML_va_ptr
ML_va_flat_copy(ML_const_va_ptr VA0, MR_AllocSiteInfoPtr alloc_id)
{
    ML_va_ptr   latest;
    ML_va_ptr   VA;
    MR_Word     array;
    MR_Integer  N;
    MR_Integer  i;

    latest = ML_va_get_latest(VA0);
    N = latest->rest.array->size;

    MR_incr_hp_type_msg(VA, struct ML_va,
        alloc_id, ""version_array.version_array/1"");
    MR_incr_hp_msg(array, N + 1,
        alloc_id, ""version_array.version_array/1"");

    VA->index               = -1;
    VA->value               = (MR_Word) NULL;
    VA->rest.array          = (MR_ArrayPtr) array;
    VA->rest.array->size    = N;

    for (i = 0; i < N; i++) {
        VA->rest.array->elements[i] = latest->rest.array->elements[i];
    }

#ifdef MR_THREAD_SAFE
    if (VA0->lock != NULL) {
        MR_incr_hp_type_msg(VA->lock, MercuryLock, alloc_id, NULL);
        pthread_mutex_init(VA->lock, MR_MUTEX_ATTR);
    } else {
        VA->lock = NULL;
    }
#endif

    ML_va_rewind_into(VA, VA0, alloc_id);

    return VA;
}

static void
ML_va_rewind_into(ML_va_ptr VA_dest, ML_const_va_ptr VA_src,
    MR_AllocSiteInfoPtr alloc_id)
{
    MR_Integer      I;
    MR_Word         X;
    ML_const_va_ptr cur;
    MR_BitmapPtr    bitmap;

    if (ML_va_latest_version(VA_src)) {
        // Shortcut.
        return;
    }

    // Rewind elements from the oldest to the newest, undoing their changes.
    // So that we undo elements in the correct order, we use a bitmap to
    // ensure that we never update an array slot twice.
    cur = VA_src;
    MR_allocate_bitmap_msg(bitmap, VA_dest->rest.array->size, alloc_id);
    MR_bitmap_zero(bitmap);
    while (!ML_va_latest_version(cur)) {
        I = cur->index;
        X = cur->value;
        if (I < VA_dest->rest.array->size && !MR_bitmap_get_bit(bitmap, I)) {
            VA_dest->rest.array->elements[I] = X;
            MR_bitmap_set_bit(bitmap, I);
        }

        cur = cur->rest.next;
    }
}

ML_va_ptr
ML_va_rewind_dolock(ML_va_ptr VA, MR_AllocSiteInfoPtr alloc_id)
{
#ifdef MR_THREAD_SAFE
    MercuryLock *lock = VA->lock;
#endif
    ML_maybe_lock(lock);

    VA = ML_va_rewind(VA, alloc_id);

    ML_maybe_unlock(lock);

    return VA;
}

static ML_va_ptr
ML_va_rewind(ML_va_ptr VA, MR_AllocSiteInfoPtr alloc_id)
{
    MR_Integer      I;
    MR_Word         X;
    ML_va_ptr       cur;
    MR_ArrayPtr     array;
    MR_BitmapPtr    bitmap;

    if (ML_va_latest_version(VA)) {
        // Shortcut.
        return VA;
    }

    // Rewind elements from the oldest to the newest, undoing their changes.
    // So that we undo elements in the correct order we use a bitmap to
    // ensure that we never update an array slot twice.
    cur = VA;
    array = ML_va_get_latest(VA)->rest.array;
    MR_allocate_bitmap_msg(bitmap, array->size, alloc_id);
    while (!ML_va_latest_version(cur)) {
        I = cur->index;
        X = cur->value;

        if (!MR_bitmap_get_bit(bitmap, I)) {
            array->elements[I] = X;
            MR_bitmap_set_bit(bitmap, I);
        }

        cur = cur->rest.next;
    }
    VA->rest.array = array;

    // This element is no longer an update element.
    VA->index = -1;
    VA->value = 0;
    return VA;
}

ML_va_ptr
ML_va_resize_dolock(ML_va_ptr VA0, MR_Integer N, MR_Word X,
    MR_AllocSiteInfoPtr alloc_id)
{
#ifdef MR_THREAD_SAFE
    MercuryLock *lock = VA0->lock;
#endif
    ML_va_ptr   VA;

    ML_maybe_lock(lock);

    VA = ML_va_resize(VA0, N, X, alloc_id);

    ML_maybe_unlock(lock);

    return VA;
}

static ML_va_ptr
ML_va_resize(ML_va_ptr VA0, MR_Integer N, MR_Word X,
    MR_AllocSiteInfoPtr alloc_id)
{
    ML_va_ptr   latest;
    ML_va_ptr   VA;
    MR_Integer  i;
    MR_Integer  size_VA0;
    MR_Integer  min;
    MR_Word     array;

    latest = ML_va_get_latest(VA0);

    size_VA0 = ML_va_size(latest);
    min      = (N <= size_VA0 ? N : size_VA0);
    MR_incr_hp_type_msg(VA, struct ML_va,
        alloc_id, ""version_array.version_array/1"");
    MR_incr_hp_msg(array, N + 1,
        alloc_id, ""version_array.version_array/1"");

    VA->index               = -1;
    VA->value               = (MR_Word) NULL;
    VA->rest.array          = (MR_ArrayPtr) array;
    VA->rest.array->size    = N;

    for (i = 0; i < min; i++) {
        VA->rest.array->elements[i] = latest->rest.array->elements[i];
    }

#ifdef MR_THREAD_SAFE
    if (VA0->lock != NULL) {
        MR_incr_hp_type_msg(VA->lock, MercuryLock, alloc_id, NULL);
        pthread_mutex_init(VA->lock, MR_MUTEX_ATTR);
    } else {
        VA->lock = NULL;
    }
#endif

    ML_va_rewind_into(VA, VA0, alloc_id);

    for (i = min; i < N; i++) {
        VA->rest.array->elements[i] = X;
    }

    return VA;
}

").

:- pragma foreign_decl("C#", local, "
using System;
").

:- pragma foreign_code("C#", "

public interface ML_va {
    bool has_lock();
    object get(int I);
    ML_va set(int I, object X);
    ML_va resize(int N, object X);
    ML_va rewind();
    int size();
}

// An implementation of version arrays that is safe when used in multiple
// threads.
//
// It just wraps the unsafe version in some synchronization logic
// so that only one thread can be accessing the array at one instant.
[System.Serializable]
public class ML_sva : ML_va {
    private ML_uva version_array;
    private object va_lock;

    public ML_sva(ML_uva va) {
        version_array = va;
        va_lock = new object();
    }

    private ML_sva() {}

    public bool has_lock() {
        return true;
    }

    public object get(int I) {
        lock (va_lock) {
            return version_array.get(I);
        }
    }

    public ML_va set(int I, object X) {
        lock (va_lock) {
            ML_sva result = new ML_sva();

            result.version_array = version_array.set_uva(I, X);

            if (result.version_array.isClone()) {
                result.version_array.resetIsClone();
                result.va_lock = new object();
            } else {
                result.va_lock = this.va_lock;
            }

            return result;
        }
    }

    public ML_va resize(int N, object X) {
        lock (va_lock) {
            ML_sva result = new ML_sva();
            result.version_array = version_array.resize_uva(N, X);
            result.va_lock = new object();
            return result;
        }
    }

    public ML_va rewind()
    {
        lock (va_lock) {
            ML_sva result = new ML_sva();
            result.version_array = version_array.rewind_uva();
            result.va_lock = this.va_lock;
            return result;
        }
    }

    public int size()
    {
        lock (va_lock) {
            return version_array.size();
        }
    }
}

// An implementation of version arrays that is only safe when used from
// a single thread, but *much* faster than the synchronized version.
[System.Serializable]
public class ML_uva : ML_va {
    private int                 index;  // -1 for latest, >= 0 for older
    private object              value;  // Valid if index >= 0
    private object              rest;   // array if index == -1
                                        // next if index >= 0

    // True if this is a fresh clone of another ML_uva.
    private bool                clone = false;

    public ML_uva() {}

    public static ML_uva empty() {
        ML_uva va = new ML_uva();
        va.index = -1;
        va.value = null;
        va.rest  = new object[0];
        return va;
    }

    public static ML_uva init(int N, object X) {
        ML_uva va = new ML_uva();
        va.index = -1;
        va.value = null;
        va.rest  = new object[N];
        for (int i = 0; i < N; i++) {
            va.array()[i] = X;
        }
        return va;
    }

    public bool has_lock() {
        return false;
    }

    public ML_va resize(int N, object X) {
        return resize_uva(N, X);
    }

    public ML_uva resize_uva(int N, object X) {
        ML_uva  VA0 = this;
        ML_uva  latest;
        int     size_VA0;
        int     min;

        latest = VA0.latest();

        size_VA0 = latest.size();
        min      = (N <= size_VA0 ? N : size_VA0);
        ML_uva VA = new ML_uva();

        VA.index = -1;
        VA.value = null;
        VA.rest  = new object[N];

        System.Array.Copy(latest.array(), 0, VA.array(), 0, min);

        VA0.rewind_into(VA);

        for (int i = min; i < N; i++) {
            VA.array()[i] = X;
        }
        return VA;
    }

    private bool is_latest()
    {
        return index == -1;
    }

    private ML_uva latest()
    {
        ML_uva VA = this;
        while (!VA.is_latest()) {
            VA = VA.next();
        }
        return VA;
    }

    private object[] array()
    {
        return (object[]) rest;
    }

    private ML_uva next()
    {
        return (ML_uva) rest;
    }

    public int size()
    {
        return latest().array().Length;
    }

    public object get(int I)
    {
        ML_uva VA = this;

        while (!VA.is_latest()) {
            if (I == VA.index) {
                return VA.value;
            }

            VA = VA.next();
        }

        return VA.array()[I];
    }

    public ML_va set(int I, object X)
    {
        return set_uva(I, X);
    }

    public ML_uva set_uva(int I, object X)
    {
        ML_uva VA0 = this;
        ML_uva VA1;

        if (VA0.is_latest()) {
            VA1 = new ML_uva();
            VA1.index   = -1;
            VA1.value   = null;
            VA1.rest    = VA0.array();

            VA0.index   = I;
            VA0.value   = VA0.array()[I];
            VA0.rest    = VA1;

            VA1.array()[I] = X;
        } else {
            VA1 = VA0.flat_copy();

            VA1.array()[I] = X;
        }

        return VA1;
    }

    private ML_uva flat_copy()
    {
        ML_uva  VA0 = this;
        ML_uva  latest;
        ML_uva  VA;

        latest = VA0.latest();

        VA = new ML_uva();
        VA.index = -1;
        VA.value = null;
        VA.rest  = latest.array().Clone();
        VA.clone = true;

        VA0.rewind_into(VA);

        return VA;
    }

    public bool isClone() {
        return clone;
    }

    public void resetIsClone() {
        this.clone = false;
    }

    private void rewind_into(ML_uva VA)
    {
        int                             I;
        object                          X;
        ML_uva                          cur;
        mercury.runtime.MercuryBitmap   bitmap;

        if (this.is_latest()) {
            // Shortcut.
            return;
        }

        // Rewind elements from the oldest to the newest, undoing their
        // changes. So that we undo elements in the correct order,
        // we use a bitmap to ensure that we never update an array slot twice.
        cur = this;
        bitmap = new mercury.runtime.MercuryBitmap(cur.size());
        while (!cur.is_latest()) {
            I = cur.index;
            X = cur.value;
            if (I < VA.size() && !bitmap.GetBit(I)) {
                VA.array()[I] = X;
                bitmap.SetBit(I);
            }

            cur = cur.next();
        }
    }

    public ML_va rewind()
    {
        return rewind_uva();
    }

    public ML_uva rewind_uva()
    {
        int                             I;
        object                          X;
        ML_uva                          cur;
        mercury.runtime.MercuryBitmap   bitmap;
        object[]                        array;

        if (is_latest()) {
            return this;
        }

        // Rewind elements from the oldest to the newest, undoing their
        // changes. So that we undo elements in the correct order,
        // we use a bitmap to ensure that we never update an array slot twice.
        cur = this;
        array = latest().array();
        bitmap = new mercury.runtime.MercuryBitmap(array.Length);
        while (!cur.is_latest()) {
            I = cur.index;
            X = cur.value;

            if (!bitmap.GetBit(I)) {
                array[I] = X;
                bitmap.SetBit(I);
            }

            cur = cur.next();
        }
        rest = array;

        // This element is no-longer an update element.
        index = -1;
        value = 0;
        return this;
    }
}

").

:- pragma foreign_decl("Java", local, "
import jmercury.runtime.MercuryBitmap;
").

:- pragma foreign_code("Java", "

public interface ML_va {
    public boolean has_lock();
    public Object get(int I) throws ArrayIndexOutOfBoundsException;
    public ML_va set(int I, Object X);
    public ML_va resize(int N, Object X);
    public ML_va rewind();
    public int size();
}

public static class Lock implements java.io.Serializable {
    public Lock() { return; }
}

// An implementation of version arrays that is safe when used in multiple
// threads.
//
// It just wraps the unsafe version in some synchronization logic
// so that only one thread can be accessing the array at one instant.
public static class ML_sva implements ML_va, java.io.Serializable {
    private ML_uva version_array;
    private Lock lock;

    public ML_sva(ML_uva va) {
        version_array = va;
        lock = new Lock();
    }

    private ML_sva() {}

    public boolean has_lock() {
        return true;
    }

    public Object get(int I) throws ArrayIndexOutOfBoundsException {
        synchronized (lock) {
            return version_array.get(I);
        }
    }

    public ML_sva set(int I, Object X) {
        synchronized (lock) {
            ML_sva result = new ML_sva();

            result.version_array = version_array.set(I, X);

            if (result.version_array.isClone()) {
                result.version_array.resetIsClone();
                result.lock = new Lock();
            } else {
                result.lock = this.lock;
            }

            return result;
        }
    }

    public ML_sva resize(int N, Object X) {
        synchronized (lock) {
            ML_sva result = new ML_sva();
            result.version_array = version_array.resize(N, X);
            result.lock = new Lock();
            return result;
        }
    }

    public ML_sva rewind()
    {
        synchronized (lock) {
            ML_sva result = new ML_sva();
            result.version_array = version_array.rewind();
            result.lock = this.lock;
            return result;
        }
    }

    public int size()
    {
        synchronized (lock) {
            return version_array.size();
        }
    }
}

// An implementation of version arrays that is only safe when used from
// a single thread, but *much* faster than the synchronized version.
public static class ML_uva implements ML_va, java.io.Serializable {
    private int                 index;  // -1 for latest, >= 0 for older
    private Object              value;  // Valid if index >= 0
    private Object              rest;   // array if index == -1
                                        // next if index >= 0

    private boolean             clone = false;

    public ML_uva() {}

    public static ML_uva empty() {
        ML_uva va = new ML_uva();
        va.index = -1;
        va.value = null;
        va.rest  = new Object[0];
        return va;
    }

    public static ML_uva init(int N, Object X) {
        ML_uva va = new ML_uva();
        va.index = -1;
        va.value = null;
        va.rest  = new Object[N];
        java.util.Arrays.fill(va.array(), X);
        return va;
    }

    public boolean has_lock() {
        return false;
    }

    public ML_uva resize(int N, Object X) {
        ML_uva  VA0 = this;
        ML_uva  latest;
        int     size_VA0;
        int     min;

        latest = VA0.latest();

        size_VA0 = latest.size();
        min      = (N <= size_VA0 ? N : size_VA0);
        ML_uva VA = new ML_uva();

        VA.index = -1;
        VA.value = null;
        VA.rest  = new Object[N];

        System.arraycopy(latest.array(), 0, VA.array(), 0, min);

        VA0.rewind_into(VA);

        java.util.Arrays.fill(VA.array(), min, N, X);
        return VA;
    }

    private boolean is_latest()
    {
        return index == -1;
    }

    private ML_uva latest()
    {
        ML_uva VA = this;
        while (!VA.is_latest()) {
            VA = VA.next();
        }
        return VA;
    }

    private Object[] array()
    {
        return (Object[]) rest;
    }

    private ML_uva next()
    {
        return (ML_uva) rest;
    }

    public int size()
    {
        return latest().array().length;
    }

    public Object get(int I)
        throws ArrayIndexOutOfBoundsException
    {
        ML_uva VA = this;

        while (!VA.is_latest()) {
            if (I == VA.index) {
                return VA.value;
            }

            VA = VA.next();
        }

        return VA.array()[I];
    }

    public ML_uva set(int I, Object X)
    {
        ML_uva VA0 = this;
        ML_uva VA1;

        if (VA0.is_latest()) {
            VA1 = new ML_uva();
            VA1.index   = -1;
            VA1.value   = null;
            VA1.rest    = VA0.array();

            VA0.index   = I;
            VA0.value   = VA0.array()[I];
            VA0.rest    = VA1;

            VA1.array()[I] = X;
        } else {
            VA1 = VA0.flat_copy();

            VA1.array()[I] = X;
        }

        return VA1;
    }

    private ML_uva flat_copy()
    {
        ML_uva  VA0 = this;
        ML_uva  latest;
        ML_uva  VA;

        latest = VA0.latest();

        VA = new ML_uva();
        VA.index = -1;
        VA.value = null;
        VA.rest  = latest.array().clone();
        VA.clone = true;

        VA0.rewind_into(VA);

        return VA;
    }

    public boolean isClone() {
        return clone;
    }

    public void resetIsClone() {
        this.clone = false;
    }

    private void rewind_into(ML_uva VA)
    {
        int             I;
        Object          X;
        ML_uva          cur;
        MercuryBitmap   bitmap;

        if (this.is_latest()) {
            return;
        }

        // Rewind elements from the oldest to the newest, undoing their
        // changes. So that we undo elements in the correct order,
        // we use a bitmap to ensure that we never update an array slot twice.
        cur = this;
        bitmap = new MercuryBitmap(cur.size());
        while (!cur.is_latest()) {
            I = cur.index;
            X = cur.value;
            if (I < VA.size() && !bitmap.getBit(I)) {
                VA.array()[I] = X;
                bitmap.setBit(I);
            }

            cur = cur.next();
        }
    }

    public ML_uva rewind()
    {
        int             I;
        Object          X;
        ML_uva          cur;
        MercuryBitmap   bitmap;
        Object[]        array;

        if (is_latest()) {
            return this;
        }

        // Rewind elements from the oldest to the newest, undoing their
        // changes. So that we undo elements in the correct order,
        // we use a bitmap to ensure that we never update an array slot twice.
        cur = this;
        array = latest().array();
        bitmap = new MercuryBitmap(array.length);
        while (!cur.is_latest()) {
            I = cur.index;
            X = cur.value;

            if (!bitmap.getBit(I)) {
                array[I] = X;
                bitmap.setBit(I);
            }

            cur = cur.next();
        }
        rest = array;

        // This element is no-longer an update element.
        index = -1;
        value = 0;
        return this;
    }
}

").

%---------------------------------------------------------------------------%

    % Throw an exception indicating an array bounds error.
    %
:- pred out_of_bounds_error(int, int, string).
:- mode out_of_bounds_error(in, in, in) is erroneous.

out_of_bounds_error(Index, Max, PredName) :-
    % Note: we deliberately do not include the array element type name in the
    % error message here, for performance reasons: using the type name could
    % prevent the compiler from optimizing away the construction of the
    % type_info in the caller, because it would prevent unused argument
    % elimination.
    string.format("%s: index %d not in range [0, %d]",
        [s(PredName), i(Index), i(Max)], Msg),
    throw(index_out_of_bounds(Msg)).

%---------------------------------------------------------------------------%

version_array_to_doc(A) = pretty_printer.version_array_to_doc(A).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    has_lock(VA::in),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#ifdef MR_THREAD_SAFE
    SUCCESS_INDICATOR = (VA->lock != NULL) ? MR_TRUE : MR_FALSE;
#else
    // The following means has_lock(VA) will fail in non-.par C grades even if
    // VA was created with a 'safe' init function. That is acceptable for the
    // use in version_hash_table.m, but if we wanted to publicly export
    // has_lock, it is something we might want to change.
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

:- pragma foreign_proc("C#",
    has_lock(VA::in),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    SUCCESS_INDICATOR = VA.has_lock();
").

:- pragma foreign_proc("Java",
    has_lock(VA::in),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    SUCCESS_INDICATOR = VA.has_lock();
").

%---------------------------------------------------------------------------%
:- end_module version_array.
%---------------------------------------------------------------------------%
