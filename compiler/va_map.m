%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2022 YesLogic Pty. Ltd.
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: va_map.m.
% Main author: wangp.
%
% This module defines a data structure on top of version arrays, with a
% map-like interface. Keys are restricted to values that can be converted to
% and from a uint.
%
% Like a plain version array, it provides O(1) indexing and update when
% accessing the latest version of the data structure.
%
% Unlike a plain version array, the data structure will grow to accomodate keys
% outside of the existing bounds. This is more like a map.
%
% Unlike a map, keys that were not explicitly set are implicitly associated
% with a default value. This is more like an array.
%
% Note: this implementation of va_map is backed by a single contiguous array,
% so the keys should be mapped to a relatively small range of values starting
% from zero.
%
%---------------------------------------------------------------------------%

:- module libs.va_map.
:- interface.

:- type va_map(K, V).

    % The keys of a va_map must be convertible to/from a uint.
    %
:- typeclass va_map_key(K) where [
    func from_key(K) = uint,
    func to_key(uint) = K
].

    % The value type of a va_map must have a default value.
    %
:- typeclass va_map_value(V) where [
    func default_value = V
].

    % Create a new va_map.
    % All keys are implicitly associated with the default value.
    %
    % NOTE: the implementation uses "thread-unsafe" version_arrays. It is NOT
    % safe to access or update copies of the same va_map simultaneously.
    %
:- func init = va_map(K, V).

    % Return the value associated with the given key in the map.
    %
:- pred lookup(va_map(K, V)::in, K::in, V::out) is det
    <= (va_map_key(K), va_map_value(V)).

    % Insert or update the key with the given value.
    %
:- pred set(K::in, V::in, va_map(K, V)::in, va_map(K, V)::out) is det
    <= (va_map_key(K), va_map_value(V)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module uint.
:- import_module version_array.

:- type va_map(K, V)
    --->    va_map(version_array(V)).

%---------------------------------------------------------------------------%

:- pred key_to_slot(K::in, int::out) is det <= va_map_key(K).

key_to_slot(K, Slot) :-
    Slot = uint.cast_to_int(from_key(K)).

:- pred slot_to_key(int::in, K::out) is det <= va_map_key(K).
:- pragma consider_used(pred(slot_to_key/2)).

slot_to_key(Slot, K) :-
    U = uint.cast_from_int(Slot),
    K = to_key(U).

%---------------------------------------------------------------------------%

init = va_map(Array) :-
    Array = version_array.unsafe_empty.

%---------------------------------------------------------------------------%

lookup(VAMap, Key, Value) :-
    VAMap = va_map(Array),
    key_to_slot(Key, Slot),
    ( if Slot < size(Array) then
        Value = version_array.lookup(Array, Slot)
    else
        Value = default_value
    ).

%---------------------------------------------------------------------------%

set(Key, Value, VAMap0, VAMap) :-
    VAMap0 = va_map(Array0),
    key_to_slot(Key, Slot),
    maybe_grow_array(Slot, Array0, Array1),
    version_array.set(Slot, Value, Array1, Array),
    VAMap = va_map(Array).

:- pred maybe_grow_array(int::in, version_array(V)::in, version_array(V)::out)
    is det <= va_map_value(V).

maybe_grow_array(Slot, !Array) :-
    Size = size(!.Array),
    ( if Slot >= Size then
        NewSize = roundup_array_size(Slot + 1),
        version_array.resize(NewSize, default_value, !Array)
    else
        true
    ).

:- func roundup_array_size(int) = int.

roundup_array_size(N) =
    % Round up to next multiple of 512 (arbitrarily).
    (N + 511) // 512 * 512.

%---------------------------------------------------------------------------%
:- end_module libs.va_map.
%---------------------------------------------------------------------------%
