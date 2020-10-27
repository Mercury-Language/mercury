%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2012 The University of Melbourne.
% Copyright (C) 2014-2020 The Mercury Team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: version_array.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>.
% Stability: low.
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
% all later versions of the structure.)
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
% The advantage of version arrays is that in the common, singly threaded,
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

    % An `version_array.index_out_of_bounds' is the exception thrown
    % on out-of-bounds array accesses. The string describes
    % the predicate or function reporting the error.
    %
:- type version_array.index_out_of_bounds
    --->    version_array.index_out_of_bounds(string).

%---------------------------------------------------------------------------%

    % empty_array returns the empty array.
    %
:- func empty = version_array(T).

    % init(N, X) returns an array of size N with each item initialised to X.
    %
:- func init(int, T) = version_array(T).

    % Same as empty/0 except the resulting version_array is not thread safe.
    %
    % That is your program can crash or behave strangely if you attempt to
    % concurrently access or update the array from different threads, or any
    % two arrays produced from operations on the same original array.
    % However this version is much quicker if you guarantee that you never
    % concurrently access the version array.
    %
:- func unsafe_empty = version_array(T).

    % Same as init(N, X) except the resulting version_array is not thread safe.
    %
    % That is your program can crash or behave strangely if you attempt to
    % concurrently access or update the array from different threads, or any
    % two arrays produced from operations on the same original array.
    % However this version is much quicker if you guarantee that you never
    % concurrently access the version array.
    %
:- func unsafe_init(int, T) = version_array(T).

    % version_array(Xs) returns an array constructed from the items in the list
    % Xs.
    %
:- func version_array(list(T)) = version_array(T).

    % A synonym for the above.
    %
:- func from_list(list(T)) = version_array(T).

    % from_reverse_list(Xs) returns an array constructed from the items in the
    % list Xs in reverse order.
    %
:- func from_reverse_list(list(T)) = version_array(T).

    % lookup(A, I) = X iff the I'th member of A is X.
    % (The first item has index 0).
    %
:- func lookup(version_array(T), int) = T.

    % A ^ elem(I) = lookup(A, I)
    %
:- func version_array(T) ^ elem(int) = T.

    % set(I, X, A0, A): A is a copy of array A0 with item I updated to be X.
    % An exception is thrown if I is out of bounds.
    %
:- pred set(int::in, T::in, version_array(T)::in, version_array(T)::out)
    is det.

    % (A0 ^ elem(I) := X) = A is equivalent to set(I, X, A0, A).
    %
:- func (version_array(T) ^ elem(int) := T) = version_array(T).

    % size(A) = N if A contains N items (i.e. the valid indices for A
    % range from 0 to N - 1).
    %
:- func size(version_array(T)) = int.

    % max(Z) = size(A) - 1.
    % Returns -1 for an empty array.
    %
:- func max(version_array(T)) = int.

    % is_empty(Array) is true iff Array is the empty array.
    %
:- pred is_empty(version_array(T)::in) is semidet.

    % resize(A, N, X) returns a new array whose items from
    % 0..min(size(A), N - 1) are taken from A and whose items
    % from min(size(A), N - 1)..(N - 1) (if any) are initialised to X.
    % A predicate version is also provided.
    %
:- func resize(version_array(T), int, T) = version_array(T).
:- pred resize(int::in, T::in, version_array(T)::in, version_array(T)::out)
    is det.

    % copy(A) is a copy of array A. Access to the copy is O(1).
    %
:- func copy(version_array(T)) = version_array(T).

    % list(A) = Xs where Xs is the list of items in A
    % (i.e. A = version_array(Xs)).
    %
:- func list(version_array(T)) = list(T).

    % A synonym for the above.
    %
:- func to_list(version_array(T)) = list(T).

    % foldl(F, A, X) is equivalent to list.foldl(F, list(A), X).
    %
:- func foldl(func(T1, T2) = T2, version_array(T1), T2) = T2.

    % foldl(P, A, !X) is equivalent to list.foldl(P, list(A), !X).
    %
:- pred foldl(pred(T1, T2, T2), version_array(T1), T2, T2).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.

    % foldl2(P, A, !Acc1, !Acc2) is equivalent to
    % list.foldl2(P, list(A), !Acc1, !Acc2) but more efficient.
    %
:- pred foldl2(pred(T1, T2, T2, T3, T3), version_array(T1), T2, T2, T3, T3).
:- mode foldl2(pred(in, in, out, in, out) is det, in, in, out, in, out)
    is det.
:- mode foldl2(pred(in, in, out, mdi, muo) is det, in, in, out, mdi, muo)
    is det.
:- mode foldl2(pred(in, in, out, di, uo) is det, in, in, out, di, uo)
    is det.
:- mode foldl2(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode foldl2(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode foldl2(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.

    % foldr(F, A, X) is equivalent to list.foldr(F, list(A), Xs).
    %
:- func foldr(func(T1, T2) = T2, version_array(T1), T2) = T2.

:- pred foldr(pred(T1, T2, T2), version_array(T1), T2, T2).
:- mode foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldr(pred(in, di, uo) is semidet, in, di, uo) is semidet.

:- pred foldr2(pred(T1, T2, T2, T3, T3), version_array(T1), T2, T2, T3, T3).
:- mode foldr2(pred(in, in, out, in, out) is det, in, in, out, in, out)
    is det.
:- mode foldr2(pred(in, in, out, mdi, muo) is det, in, in, out, mdi, muo)
    is det.
:- mode foldr2(pred(in, in, out, di, uo) is det, in, in, out, di, uo)
    is det.
:- mode foldr2(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode foldr2(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode foldr2(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.

    % version_array.all_true(Pred, Array):
    % True iff Pred is true for every element of Array.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), version_array(T)::in)
    is semidet.

    % version_array.all_false(Pred, Array):
    % True iff Pred is false for every element of Array.
    %
:- pred all_false(pred(T)::in(pred(in) is semidet), version_array(T)::in)
    is semidet.

    % unsafe_rewind(A) produces a version of A for which all accesses are O(1).
    % Invoking this predicate renders A and all later versions undefined that
    % were derived by performing individual updates. Only use this when you are
    % absolutely certain there are no live references to A or later versions
    % of A. (A predicate version is also provided.)
    %
:- func unsafe_rewind(version_array(T)) = version_array(T).
:- pred unsafe_rewind(version_array(T)::in, version_array(T)::out) is det.

    % Convert a version_array to a pretty_printer.doc for formatting.
    %
:- func version_array_to_doc(version_array(T)) = pretty_printer.doc.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% The first implementation of version arrays used nb_references.
% This incurred three memory allocations for every update. This version
% works at a lower level, but only performs one allocation per update.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module exception.
:- import_module string.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    version_array.empty = (VA::out),
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
    version_array.empty = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    VA = new version_array.ML_sva(version_array.ML_uva.empty());
").

:- pragma foreign_proc("Java",
    version_array.empty = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    VA = new ML_sva(ML_uva.empty());
").

:- pragma foreign_proc("C",
    version_array.unsafe_empty = (VA::out),
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
    version_array.unsafe_empty = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    VA = version_array.ML_uva.empty();
").

:- pragma foreign_proc("Java",
    version_array.unsafe_empty = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    VA = ML_uva.empty();
").

:- pragma foreign_proc("C",
    version_array.init(N::in, X::in) = (VA::out),
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
    version_array.init(N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate],
"
    VA = new version_array.ML_sva(version_array.ML_uva.init(N, X));
").

:- pragma foreign_proc("Java",
    version_array.init(N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate],
"
    VA = new ML_sva(ML_uva.init(N, X));
").

:- pragma foreign_proc("C",
    version_array.unsafe_init(N::in, X::in) = (VA::out),
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
    version_array.unsafe_init(N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate],
"
    VA = version_array.ML_uva.init(N, X);
").

:- pragma foreign_proc("Java",
    version_array.unsafe_init(N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate],
"
    VA = ML_uva.init(N, X);
").

%---------------------------------------------------------------------------%

version_array([]) = version_array.empty.
version_array([X | Xs]) = VA :-
    VA0 = version_array.init(1 + list.length(Xs), X),
    version_array_loop(1, Xs, VA0, VA).

:- pred version_array_loop(int::in, list(T)::in,
    version_array(T)::in, version_array(T)::out) is det.

version_array_loop(_, [], !VA).
version_array_loop(I, [X | Xs], !VA) :-
    set(I, X, !VA),
    version_array_loop(I + 1, Xs, !VA).

from_list(Xs) = version_array(Xs).

from_reverse_list([]) = version_array.empty.
from_reverse_list([X | Xs]) = VA :-
    NumElems = 1 + list.length(Xs),
    VA0 = version_array.init(NumElems, X),
    from_reverse_list_loop(NumElems - 2, Xs, VA0, VA).

:- pred from_reverse_list_loop(int::in, list(T)::in,
    version_array(T)::in, version_array(T)::out) is det.

from_reverse_list_loop(_, [], !VA).
from_reverse_list_loop(I, [X | Xs], !VA) :-
    set(I, X, !VA),
    from_reverse_list_loop(I - 1, Xs, !VA).

%---------------------------------------------------------------------------%

lookup(VA, I) = X :-
    ( if get_if_in_range(VA, I, X0) then
        X = X0
    else
        out_of_bounds_error(I, max(VA), "version_array.lookup")
    ).

:- pragma inline(version_array.elem/2).
VA ^ elem(I) =
    lookup(VA, I).

%---------------------------------------------------------------------------%

set(I, X, !VA) :-
    ( if set_if_in_range(I, X, !VA) then
        true
    else
        out_of_bounds_error(I, max(!.VA), "version_array.set")
    ).

:- pragma inline(version_array.'elem :='/3).
(VA0 ^ elem(I) := X) = VA :-
    set(I, X, VA0, VA).

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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    N = VA.size();
").

:- pragma foreign_proc("Java",
    size(VA::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    N = VA.size();
").

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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate],
"
    VA = VA0.resize(N, X);
").

:- pragma foreign_proc("Java",
    resize(VA0::in, N::in, X::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate],
"
    VA = VA0.resize(N, X);
").

resize(N, X, VA, resize(VA, N, X)).

%---------------------------------------------------------------------------%

copy(VA) =
    ( if size(VA) = 0 then
        VA
    else
        resize(VA, size(VA), lookup(VA, 0))
    ).

%---------------------------------------------------------------------------%

list(VA) = foldr(list.cons, VA, []).

to_list(VA) = list(VA).

%---------------------------------------------------------------------------%

foldl(F, VA, Acc) = do_foldl_func(F, VA, Acc, 0, size(VA)).

:- func do_foldl_func(func(T1, T2) = T2, version_array(T1), T2, int, int) = T2.

do_foldl_func(F, VA, Acc, Lo, Hi) =
    ( if Lo < Hi then
        do_foldl_func(F, VA, F(lookup(VA, Lo), Acc), Lo + 1, Hi)
    else
        Acc
    ).

%---------------------------------------------------------------------------%

foldl(P, VA, !Acc) :-
    do_foldl_pred(P, VA, 0, size(VA), !Acc).

:- pred do_foldl_pred(pred(T1, T2, T2), version_array(T1), int, int, T2, T2).
:- mode do_foldl_pred(pred(in, in, out) is det, in, in, in, in, out) is det.
:- mode do_foldl_pred(pred(in, mdi, muo) is det, in, in, in, mdi, muo) is det.
:- mode do_foldl_pred(pred(in, di, uo) is det, in, in, in, di, uo) is det.
:- mode do_foldl_pred(pred(in, in, out) is semidet, in, in, in, in, out)
    is semidet.
:- mode do_foldl_pred(pred(in, mdi, muo) is semidet, in, in, in, mdi, muo)
    is semidet.
:- mode do_foldl_pred(pred(in, di, uo) is semidet, in, in, in, di, uo)
    is semidet.

do_foldl_pred(P, VA, Lo, Hi, !Acc) :-
    ( if Lo < Hi then
        P(lookup(VA, Lo), !Acc),
        do_foldl_pred(P, VA, Lo + 1, Hi, !Acc)
    else
        true
    ).

%---------------------------------------------------------------------------%

foldl2(P, VA, !Acc1, !Acc2) :-
    do_foldl2(P, VA, 0, size(VA), !Acc1, !Acc2).

:- pred do_foldl2(pred(T1, T2, T2, T3, T3), version_array(T1), int, int,
    T2, T2, T3, T3).
:- mode do_foldl2(pred(in, in, out, in, out) is det, in, in, in,
    in, out, in, out) is det.
:- mode do_foldl2(pred(in, in, out, mdi, muo) is det, in, in, in,
    in, out, mdi, muo) is det.
:- mode do_foldl2(pred(in, in, out, di, uo) is det, in, in, in,
    in, out, di, uo) is det.
:- mode do_foldl2(pred(in, in, out, in, out) is semidet, in, in, in,
    in, out, in, out) is semidet.
:- mode do_foldl2(pred(in, in, out, mdi, muo) is semidet, in, in, in,
    in, out, mdi, muo) is semidet.
:- mode do_foldl2(pred(in, in, out, di, uo) is semidet, in, in, in,
    in, out, di, uo) is semidet.

do_foldl2(P, VA, Lo, Hi, !Acc1, !Acc2) :-
    ( if Lo < Hi then
        P(lookup(VA, Lo), !Acc1, !Acc2),
        do_foldl2(P, VA, Lo + 1, Hi, !Acc1, !Acc2)
    else
        true
    ).

%---------------------------------------------------------------------------%

foldr(F, VA, Acc) = do_foldr_func(F, VA, Acc, version_array.max(VA)).

:- func do_foldr_func(func(T1, T2) = T2, version_array(T1), T2, int) = T2.

do_foldr_func(F, VA, Acc, Hi) =
    ( if 0 =< Hi then
        do_foldr_func(F, VA, F(lookup(VA, Hi), Acc), Hi - 1)
    else
        Acc
    ).

%---------------------------------------------------------------------------%

foldr(P, VA, !Acc) :-
    do_foldr_pred(P, VA, version_array.max(VA), !Acc).

:- pred do_foldr_pred(pred(T1, T2, T2), version_array(T1), int, T2, T2).
:- mode do_foldr_pred(pred(in, in, out) is det, in, in, in, out) is det.
:- mode do_foldr_pred(pred(in, mdi, muo) is det, in, in, mdi, muo) is det.
:- mode do_foldr_pred(pred(in, di, uo) is det,  in, in, di, uo) is det.
:- mode do_foldr_pred(pred(in, in, out) is semidet, in, in, in, out)
    is semidet.
:- mode do_foldr_pred(pred(in, mdi, muo) is semidet, in, in, mdi, muo)
    is semidet.
:- mode do_foldr_pred(pred(in, di, uo) is semidet, in, in, di, uo)
    is semidet.

do_foldr_pred(P, VA, I, !Acc) :-
    ( if I >= 0 then
        P(lookup(VA, I), !Acc),
        do_foldr_pred(P, VA, I - 1, !Acc)
    else
        true
    ).

%---------------------------------------------------------------------------%

foldr2(P, VA, !Acc1, !Acc2) :-
    do_foldr2(P, VA, version_array.max(VA), !Acc1, !Acc2).

:- pred do_foldr2(pred(T1, T2, T2, T3, T3), version_array(T1), int,
    T2, T2, T3, T3).
:- mode do_foldr2(pred(in, in, out, in, out) is det, in, in,
    in, out, in, out) is det.
:- mode do_foldr2(pred(in, in, out, mdi, muo) is det, in, in,
    in, out, mdi, muo) is det.
:- mode do_foldr2(pred(in, in, out, di, uo) is det, in, in,
    in, out, di, uo) is det.
:- mode do_foldr2(pred(in, in, out, in, out) is semidet, in, in,
    in, out, in, out) is semidet.
:- mode do_foldr2(pred(in, in, out, mdi, muo) is semidet, in, in,
    in, out, mdi, muo) is semidet.
:- mode do_foldr2(pred(in, in, out, di, uo) is semidet, in, in,
    in, out, di, uo) is semidet.

do_foldr2(P, VA, I, !Acc1, !Acc2) :-
    ( if I >= 0 then
        P(lookup(VA, I), !Acc1, !Acc2),
        do_foldr2(P, VA, I - 1, !Acc1, !Acc2)
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
        Elem = VA ^ elem(I),
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
        Elem = VA ^ elem(I),
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

:- pragma foreign_type("C", version_array(T), "struct ML_va *")
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
:- pragma terminates(eq_version_array/2).

eq_version_array(VAa, VAb) :-
    N = max(VAa),
    N = max(VAb),
    eq_version_array_2(N, VAa, VAb).

:- pred eq_version_array_2(int::in,
    version_array(T)::in, version_array(T)::in) is semidet.

eq_version_array_2(I, VAa, VAb) :-
    ( if I >= 0 then
        lookup(VAa, I) = lookup(VAb, I),
        eq_version_array_2(I - 1, VAa, VAb)
    else
        true
    ).

:- pred cmp_version_array(comparison_result::uo,
    version_array(T)::in, version_array(T)::in) is det.
:- pragma terminates(cmp_version_array/3).

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
        compare(R0, lookup(VAa, I), lookup(VAb, I)),
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    VA = VA0.rewind();
").

:- pragma foreign_proc("Java",
    unsafe_rewind(VA0::in) = (VA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
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
    // So that we undo elements in the correct order we use a bitmap to
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

    // This element is no-longer an update element.
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
    object get(int I);
    ML_va set(int I, object X);
    ML_va resize(int N, object X);
    ML_va rewind();
    int size();
}

// An implementation of version arrays that is safe when used in multiple
// threads.
//
// It just wraps the unsafe version is some synchronization logic so
// that only one thread can be accessing the array at one instant.
[System.Serializable]
public class ML_sva : ML_va {
    private ML_uva version_array;
    private object va_lock;

    public ML_sva(ML_uva va) {
        version_array = va;
        va_lock = new object();
    }

    private ML_sva() {}

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
// It just wraps the unsafe version is some synchronization logic so
// that only one thread can be accessing the array at one instant.
public static class ML_sva implements ML_va, java.io.Serializable {
    private ML_uva version_array;
    private Lock lock;

    public ML_sva(ML_uva va) {
        version_array = va;
        lock = new Lock();
    }

    private ML_sva() {};

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
    throw(version_array.index_out_of_bounds(Msg)).

%---------------------------------------------------------------------------%

version_array_to_doc(A) =
    indent([str("version_array(["), version_array_to_doc_2(0, A), str("])")]).

:- func version_array_to_doc_2(int, version_array(T)) = doc.

version_array_to_doc_2(I, VA) =
    ( if I > version_array.max(VA) then
        str("")
    else
        docs([
            format_arg(format(lookup(VA, I))),
            ( if I = version_array.max(VA) then
                str("")
            else
                group([str(", "), nl])
            ),
            format_susp((func) = version_array_to_doc_2(I + 1, VA))
        ])
    ).

%---------------------------------------------------------------------------%
:- end_module version_array.
%---------------------------------------------------------------------------%
