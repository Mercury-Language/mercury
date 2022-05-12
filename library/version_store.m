%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2011 The University of Melbourne.
% Copyright (C) 2013-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: version_store.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>
% Stability: low.
%
% See the header comments in version_array.m for an explanation of version
% types.
%
% A version_store is similar to, albeit slightly slower than, an ordinary
% store, but does not depend upon uniqueness.
%
% Note that, unlike ordinary stores, liveness of data is via the version store
% rather than the mutvars. This means that dead data (i.e. data whose mutvar
% is out of scope) in a version_store may not be garbage collected.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module version_store.
:- interface.

%---------------------------------------------------------------------------%

:- type version_store(S).

:- type mutvar(T, S).

    % Construct a new version store. This is distinguished from other
    % version stores by its existentially quantified type. This means
    % the compiler can automatically detect any attempt to use a mutvar
    % with the wrong version store.
    %
:- some [S] func init = version_store(S).

    % new_mutvar(X, Mutvar, VS0, VS) adds a new mutvar with value reference X
    % to the version store.
    %
:- pred new_mutvar(T::in, mutvar(T, S)::out,
    version_store(S)::in, version_store(S)::out) is det.

    % new_cyclic_mutvar(F, Mutvar, VS0, VS) adds a new mutvar with value
    % reference F(Mutvar) to the version store. This can be used to
    % construct cyclic terms.
    %
:- pred new_cyclic_mutvar((func(mutvar(T, S)) = T)::in, mutvar(T, S)::out,
    version_store(S)::in, version_store(S)::out) is det.

    % copy_mutvar(Mutvar, NewMutvar, VS0, VS) constructs NewMutvar
    % with the same value reference as Mutvar.
    %
:- pred copy_mutvar(mutvar(T, S)::in, mutvar(T, S)::out,
    version_store(S)::in, version_store(S)::out) is det.

    % VS ^ elem(Mutvar) returns the element referenced by Mutvar in
    % the version store.
    %
:- func version_store(S) ^ elem(mutvar(T, S)) = T.

    % lookup(VS, Mutvar) = VS ^ elem(Mutvar).
    %
    % A predicate version is also provided.
    %
:- func lookup(version_store(S), mutvar(T, S)) = T.
:- pred get_mutvar(mutvar(T, S)::in, T::out,
    version_store(S)::in, version_store(S)::out) is det.

    % ( VS ^ elem(Mutvar) := X ) updates the version store so that
    % Mutvar now refers to value X.
    %
:- func ( version_store(S) ^ elem(mutvar(T, S)) := T ) = version_store(S).

    % set(VS, Mutvar, X) = ( VS ^ elem(Mutvar) := X ).
    %
    % A predicate version is also provided.
    %
:- func set(version_store(S), mutvar(T, S), T) = version_store(S).
:- pred set_mutvar(mutvar(T, S)::in, T::in,
    version_store(S)::in, version_store(S)::out) is det.

    % unsafe_rewind(VS) produces a version of VS for which all accesses
    % are O(1). Invoking this predicate renders undefined VS and all later
    % versions undefined that were derived by performing individual updates.
    % Only use this when you are absolutely certain there are no live
    % references to VS or later versions of VS.
    %
    % A predicate version is also provided.
    %
:- func unsafe_rewind(version_store(T)) = version_store(T).
:- pred unsafe_rewind(version_store(T)::in, version_store(T)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module counter.
:- import_module int.
:- import_module unit.
:- import_module univ.
:- import_module version_array.

%---------------------------------------------------------------------------%

    % Each mutvar contains an index into the version_array;
    % its value is stored in the indicated slot in the version_array.
    %
    % We maintain a counter that, when allocated from, tells us the number
    % of the next free slot in the array. This is what we use to allocate
    % new mutvars. We store this counter at index 0 of the version_array,
    % so all mutvars contain a strictly positive index.
    %
    % Whenever the version_array is about to run out of slots, we double
    % its size. We never shrink the version_array.
    %
    % The slots in the version_array beyond the slot of belonging to
    % the last allocated mutvar are not meaningful.
    %
:- type version_store(S)
    --->    version_store(version_array(univ)).

:- type mutvar(T, S)
    --->    mutvar(int).
            % This integer must be strictly greater than zero.

:- type some_version_store_type
    --->    some_version_store_type.

%---------------------------------------------------------------------------%

init = version_store(VA) `with_type` version_store(some_version_store_type) :-
    counter.init(1, Counter),
    % Using 256 as the initial size of the array is a compromise between
    % - wasting too much memory in small version_stores
    %   (which would happen with larger initial sizes), and
    % - requiring too many reallocations for large version stores
    %   (which would happen with smaller initial sizes).
    VA = version_array.init(256, univ(Counter)).

%---------------------------------------------------------------------------%

new_mutvar(X, Mutvar, !VS) :-
    new_cyclic_mutvar(func(_) = X, Mutvar, !VS).

%---------------------------------------------------------------------------%

new_cyclic_mutvar(F, Mutvar, !VS) :-
    CounterMutvar = mutvar(0),
    Counter0 = version_store.lookup(!.VS, CounterMutvar),
    counter.allocate(I, Counter0, Counter),
    Mutvar = mutvar(I),
    Size0 = size(!.VS),
    ( if I >= Size0 then
        resize(Size0 + Size0, !VS)
    else
        true
    ),
    set_mutvar(CounterMutvar, Counter, !VS),
    set_mutvar(Mutvar, F(Mutvar), !VS).

:- func size(version_store(S)) = int.

size(version_store(VA)) = size(VA).

:- pred resize(int::in, version_store(S)::in, version_store(S)::out) is det.

resize(N, version_store(VA0), version_store(VA)) :-
    version_array.resize(N, univ(unit), VA0, VA).

%---------------------------------------------------------------------------%

copy_mutvar(Mutvar0, Mutvar, VS0, VS) :-
    get_mutvar(Mutvar0, Value, VS0, _VS),
    new_mutvar(Value, Mutvar, VS0, VS).

%---------------------------------------------------------------------------%

VS ^ elem(Mutvar) = Value :-
    version_store.get_mutvar(Mutvar, Value, VS, _VS).

lookup(VS, Mutvar) = Value :-
    version_store.get_mutvar(Mutvar, Value, VS, _VS).

get_mutvar(mutvar(I), Value, VS, VS) :-
    VS = version_store(VA),
    UnivValue = version_array.lookup(VA, I),
    det_univ_to_type(UnivValue, Value).

%---------------------------------------------------------------------------%

VS0 ^ elem(Mutvar) := Value = VS :-
    version_store.set_mutvar(Mutvar, Value, VS0, VS).

set(VS0, Mutvar, Value) = VS :-
    version_store.set_mutvar(Mutvar, Value, VS0, VS).

set_mutvar(mutvar(I), Value, version_store(VA0), version_store(VA)) :-
    version_array.set(I, univ(Value), VA0, VA).

%---------------------------------------------------------------------------%

unsafe_rewind(VS0) = VS :-
    version_store.unsafe_rewind(VS0, VS).

unsafe_rewind(version_store(VA0), version_store(VA)) :-
    version_array.unsafe_rewind(VA0, VA).

%---------------------------------------------------------------------------%
:- end_module version_store.
%---------------------------------------------------------------------------%
