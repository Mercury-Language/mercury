%---------------------------------------------------------------------------%
% Copyright (C) 2004-2007, 2010-2011 The University of Melbourne
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% File: version_bitmap.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>.
% Stability: low.
%
% (See the header comments in version_array.m for an explanation of version
% types.)
%
% Version bitmaps: an implementation of bitmaps using version arrays.
%
% The advantage of version bitmaps is that in the common, singly threaded,
% case, they are almost as fast as unique bitmaps, but can be treated as
% ordinary ground values rather than unique values.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module version_bitmap.
:- interface.

:- import_module bool.

%---------------------------------------------------------------------------%

:- type version_bitmap.

    % init(N, B) creates a version_bitmap of size N (indexed 0 .. N-1)
    % setting each bit if B = yes and clearing each bit if B = no.
    % An exception is thrown if N is negative.
    %
:- func init(int, bool) = version_bitmap.

    % resize(BM, N, B) resizes version_bitmap BM to have N bits;
    % if N is smaller than the current number of bits in BM, then
    % the excess are discarded. If N is larger than the current number
    % of bits in BM then the new bits are set if B = yes and cleared if
    % B = no.
    %
:- func resize(version_bitmap, int, bool) = version_bitmap.

    % Version of the above suitable for use with state variables.
    %
:- pred resize(int::in, bool::in, version_bitmap::in, version_bitmap::out)
    is det.

    % Returns the number of bits in a version_bitmap.
    %
:- func num_bits(version_bitmap) = int.

    % Get the given bit.
    %
:- func version_bitmap ^ bit(int) = bool.

    % Set the given bit.
    %
:- func (version_bitmap ^ bit(int) := bool) = version_bitmap.

    % set(BM, I), clear(BM, I) and flip(BM, I) set, clear and flip
    % bit I in BM respectively. An exception is thrown if I is out
    % of range. Predicate versions are also provided.
    %
:- func set(version_bitmap, int) = version_bitmap.
:- pred set(int::in, version_bitmap::in, version_bitmap::out) is det.

:- func clear(version_bitmap, int) = version_bitmap.
:- pred clear(int::in, version_bitmap::in, version_bitmap::out) is det.

:- func flip(version_bitmap, int) = version_bitmap.
:- pred flip(int::in, version_bitmap::in, version_bitmap::out) is det.

    % is_set(BM, I) and is_clear(BM, I) succeed iff bit I in BM
    % is set or clear respectively.
    %
:- pred is_set(version_bitmap::in, int::in) is semidet.
:- pred is_clear(version_bitmap::in, int::in) is semidet.

    % Create a new copy of a version_bitmap.
    %
:- func copy(version_bitmap) = version_bitmap.

    % Set operations; the second argument is altered in all cases.
    %
:- func complement(version_bitmap) = version_bitmap.

:- func union(version_bitmap, version_bitmap) = version_bitmap.

:- func intersect(version_bitmap, version_bitmap) = version_bitmap.

:- func difference(version_bitmap, version_bitmap) = version_bitmap.

:- func xor(version_bitmap, version_bitmap) = version_bitmap.

    % unsafe_rewind(B) produces a version of B for which all accesses are
    % O(1). Invoking this predicate renders B and all later versions undefined
    % that were derived by performing individual updates. Only use this when
    % you are absolutely certain there are no live references to B or later
    % versions of B.
    %
:- func unsafe_rewind(version_bitmap) = version_bitmap.

    % A version of the above suitable for use with state variables.
    %
:- pred unsafe_rewind(version_bitmap::in, version_bitmap::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module version_array.

    % A version_bitmap is represented as an array of ints where each int stores
    % int.bits_per_int bits. The first element of the array (index 0)
    % is used to hold the number of bits in the version_bitmap. This avoids
    % having to create a new version_bitmap cell on each update.
    %
    % NOTE: the `filler' bits in the last element of the array *must* be clear
    % (i.e. zero). This makes the set operations simpler to implement.
    %
:- type version_bitmap == version_array(int).

%---------------------------------------------------------------------------%

init(N, B) = BM :-
    ( if N < 0 then
        throw(software_error("version_bitmap.init: negative size"))
    else
        X    = initializer(B),
        BM0  = (version_array.init(num_ints_required(N), X) ^ elem(0) := N),
        BM   = clear_filler_bits(BM0)
    ).

%---------------------------------------------------------------------------%

resize(BM0, N, B) = BM :-
    ( if N =< 0 then
        BM = init(N, B)
    else
        X = initializer(B),
        NumInts = num_ints_required(N),
        BM1 = version_array.resize(BM0, NumInts, X),

        % Now we need to ensure that bits N, N+1, N+2, ... up to
        % the word boundary are initialized properly.
        int.min(num_bits(BM0), N, M),
        Offset  = int_offset(M - 1),
        Mask    = bitsmask(M - 1),          % For bits we need to preserve.
        Bits    = \(Mask) /\ X,             % Bits we need to fill in.
        BM2     = (( BM1
                        ^ elem(0)      := N )
                        ^ elem(Offset) := (BM1 ^ elem(Offset) /\ Mask) \/ Bits),
        BM      = clear_filler_bits(BM2)
    ).

resize(N, B, BM, resize(BM, N, B)).

%---------------------------------------------------------------------------%

:- func clear_filler_bits(version_bitmap) = version_bitmap.

clear_filler_bits(BM0) = BM :-
    N = num_bits(BM0),
    ( if N > 0 then
        Last = int_offset(N - 1),       % Offset of last bit.
        Ksam = bitsmask(N - 1),         % Masks off the filler bits.
        BM   = BM0 ^ elem(Last) := BM0 ^ elem(Last) /\ Ksam
    else
        BM   = BM0
    ).

%---------------------------------------------------------------------------%

:- func initializer(bool) = int.

initializer(no)  = 0.
initializer(yes) = \(0).

%---------------------------------------------------------------------------%

num_bits(BM) = BM ^ elem(0).

%---------------------------------------------------------------------------%

:- pred in_range(version_bitmap::in, int::in) is semidet.

in_range(BM, I) :- 0 =< I, I < num_bits(BM).

%---------------------------------------------------------------------------%

BM ^ bit(I) = ( if is_set(BM, I) then yes else no ).

(BM ^ bit(I) := yes) = set(BM, I).
(BM ^ bit(I) := no) = clear(BM, I).

%---------------------------------------------------------------------------%

set(BM, I) =
    ( if in_range(BM, I) then
        BM ^ elem(int_offset(I)) :=
            BM ^ elem(int_offset(I)) \/ bitmask(I)
    else
        throw(software_error("version_bitmap.set: out of range"))
    ).

set(I, BM, set(BM, I)).

clear(BM, I) =
    ( if in_range(BM, I) then
        BM ^ elem(int_offset(I)) :=
            BM ^ elem(int_offset(I)) /\ \bitmask(I)
    else
        throw(software_error("version_bitmap.clear: out of range"))
    ).

clear(I, BM, clear(BM, I)).

flip(BM, I) =
    ( if in_range(BM, I) then
        BM ^ elem(int_offset(I)) :=
            BM ^ elem(int_offset(I)) `xor` bitmask(I)
    else
        throw(software_error("version_bitmap.flip: out of range"))
    ).

flip(I, BM, flip(BM, I)).

%---------------------------------------------------------------------------%

is_set(BM, I) :-
    ( if in_range(BM, I) then
        BM ^ elem(int_offset(I)) /\ bitmask(I) \= 0
    else
        throw(software_error("version_bitmap.is_set: out of range"))
    ).

is_clear(BM, I) :-
    ( if in_range(BM, I) then
        BM ^ elem(int_offset(I)) /\ bitmask(I) = 0
    else
        throw(software_error("version_bitmap.is_clear: out of range"))
    ).

%---------------------------------------------------------------------------%

copy(BM) = version_array.copy(BM).

%---------------------------------------------------------------------------%

complement(BM) =
    clear_filler_bits(complement_2(BM ^ elem(0) - 1, BM)).

:- func complement_2(int, version_bitmap) = version_bitmap.

complement_2(WordI, BM) =
    ( if WordI =< 0 then
        BM
    else
        complement_2(WordI - 1, BM ^ elem(WordI) := \(BM ^ elem(WordI)))
    ).

%---------------------------------------------------------------------------%

union(BMa, BMb) =
    ( if num_bits(BMa) = num_bits(BMb) then
        zip(int_offset(num_bits(BMb) - 1), (\/), BMa, BMb)
    else
        throw(software_error(
            "version_bitmap.union: version_bitmaps not the same size"))
    ).

%---------------------------------------------------------------------------%

intersect(BMa, BMb) =
    ( if num_bits(BMa) = num_bits(BMb) then
        zip(int_offset(num_bits(BMb) - 1), (/\), BMa, BMb)
    else
        throw(software_error(
            "version_bitmap.intersect: version_bitmaps not the same size"))
    ).

%---------------------------------------------------------------------------%

difference(BMa, BMb) =
    ( if num_bits(BMa) = num_bits(BMb) then
        zip(int_offset(num_bits(BMb) - 1), (func(X, Y) = X /\ \Y), BMa, BMb)
    else
        throw(software_error(
            "version_bitmap.difference: version_bitmaps not the same size"))
    ).

%---------------------------------------------------------------------------%

xor(BMa, BMb) =
    ( if num_bits(BMa) = num_bits(BMb) then
        zip(int_offset(num_bits(BMb) - 1), (func(X, Y) = X `xor` Y), BMa, BMb)
    else
        throw(software_error(
            "version_bitmap.xor: version_bitmaps not the same size"))
    ).

%---------------------------------------------------------------------------%

    % Applies a function to every corresponding element between +ve I
    % and 1 inclusive, destructively updating the second version_bitmap.
    %
:- func zip(int, func(int, int) = int, version_bitmap, version_bitmap) =
    version_bitmap.

zip(I, Fn, BMa, BMb) =
    ( if I > 0 then
        zip(I - 1, Fn, BMa, BMb ^ elem(I) := Fn(BMb ^ elem(I), BMa ^ elem(I)))
    else
        BMb
    ).

%---------------------------------------------------------------------------%

    % The size of the version_array required to hold an N-bit version_bitmap.
    %
:- func num_ints_required(int) = int.

    % We add the 1 on because version_arrays of size N are indexed 0 .. N - 1.
    %
num_ints_required(N) = 1 + ( if N > 0 then int_offset(N) else 0 ).

%---------------------------------------------------------------------------%

    % The version_array index containing the given bit.
    %
:- func int_offset(int) = int.

    % We add the extra 1 on because elem(0) is used to store the
    % number of bits in the version_bitmap; the data are stored
    % in the following elements.
    %
int_offset(I) = 1 + int.quot_bits_per_int(I).

%---------------------------------------------------------------------------%

    % Construct the bitmask for a given bit in a word.
    %
    % E.g. assuming int.bits_per_int = 8 and I = 11 then
    % bitmask(I) = 2'00001000
    %
:- func bitmask(int) = int.

    % NOTE: it would be nicer to use /\ with a bitmask here rather
    % than rem. Do modern back-ends do the decent thing here if
    % int.bits_per_int is the expected power of two?
    %
bitmask(I) = 1 `unchecked_left_shift` int.rem_bits_per_int(I).

%---------------------------------------------------------------------------%

    % Construct the bitmask for all the bits up to and including
    % the given bit in a word.
    %
    % E.g. assuming int.bits_per_int = 8 and I = 11 then
    % bitmask(I) = 2'00001111
    %
:- func bitsmask(int) = int.

bitsmask(I) = BitsMask :-
    BitMask  = bitmask(I),
    BitsMask = BitMask \/ (BitMask - 1).

%---------------------------------------------------------------------------%

unsafe_rewind(BM) = version_array.unsafe_rewind(BM).

unsafe_rewind(BM, version_bitmap.unsafe_rewind(BM)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
