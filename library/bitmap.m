% ---------------------------------------------------------------------------- %
% bitmap.m
% Copyright (C) 2001-2002 Ralph Becket <rbeck@microsoft.com>
% Thu Feb  1 14:38:31 GMT 2001
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
% THIS MODULE IS HEREBY DONATED TO THE MERCURY PROJECT TO BE
% RELEASED UNDER WHATEVER LICENCES ARE DEEMED APPROPRIATE BY
% THE MERCURY PROJECT TEAM.
%
% Efficient bitmap implementation.
%
% CAVEAT: the user is referred to the documentation in the header
% of array.m regarding programming with unique objects (the compiler
% does not currently recognise them, hence we are forced to use
% non-unique modes until the situation is rectified; this places
% a small burden on the programmer to ensure the correctness of his
% code that would otherwise be assured by the compiler.)
%
% ---------------------------------------------------------------------------- %

:- module bitmap.

:- interface.

:- import_module array, int, bool.



:- type bitmap.

:- inst bitmap    == array.
:- inst uniq_bitmap == uniq_array.
:- mode bitmap_ui == array_ui.
:- mode bitmap_di == array_di.
:- mode bitmap_uo == array_uo.

    % new(N, B) creates a bitmap of size N (indexed 0 .. N-1)
    % setting each bit if B = yes and clearing each bit if B = no.
    % An exception is thrown if N is negative.
    %
:- func new(int, bool) = bitmap.
:- mode new(in, in) = bitmap_uo is det.

    % Returns the number of bits in a bitmap.
    %
:- func num_bits(bitmap) = int.
:- mode num_bits(bitmap_ui) = out is det.
:- mode num_bits(in) = out is det.

    % set(BM, I), clear(BM, I) and flip(BM, I) set, clear and flip
    % bit I in BM respectively.
    %
:- func set(bitmap, int) = bitmap.
:- mode set(bitmap_di, in) = bitmap_uo is det.

:- func clear(bitmap, int) = bitmap.
:- mode clear(bitmap_di, in) = bitmap_uo is det.

:- func flip(bitmap, int) = bitmap.
:- mode flip(bitmap_di, in) = bitmap_uo is det.

    % Unsafe versions of the above: if the index is out of range
    % then behaviour is undefined and bad things are likely to happen.
    %
:- func unsafe_set(bitmap, int) = bitmap.
:- mode unsafe_set(bitmap_di, in) = bitmap_uo is det.

:- func unsafe_clear(bitmap, int) = bitmap.
:- mode unsafe_clear(bitmap_di, in) = bitmap_uo is det.

:- func unsafe_flip(bitmap, int) = bitmap.
:- mode unsafe_flip(bitmap_di, in) = bitmap_uo is det.

    % is_set(BM, I) and is_clear(BM, I) succeed iff bit I in BM
    % is set or clear respectively.
    %
:- pred is_set(bitmap, int).
:- mode is_set(bitmap_ui, in) is semidet.
:- mode is_set(in, in) is semidet.

:- pred is_clear(bitmap, int).
:- mode is_clear(bitmap_ui, in) is semidet.
:- mode is_clear(in, in) is semidet.

    % Unsafe versions of the above: if the index is out of range
    % then behaviour is undefined and bad things are likely to happen.
    %
:- pred unsafe_is_set(bitmap, int).
:- mode unsafe_is_set(bitmap_ui, in) is semidet.
:- mode unsafe_is_set(in, in) is semidet.

:- pred unsafe_is_clear(bitmap, int).
:- mode unsafe_is_clear(bitmap_ui, in) is semidet.
:- mode unsafe_is_clear(in, in) is semidet.

    % get(BM, I) returns `yes' if is_set(BM, I) and `no' otherwise.
    %
:- func get(bitmap, int) = bool.
:- mode get(bitmap_ui, in) = out is det.
:- mode get(in, in) = out is det.

    % Unsafe versions of the above: if the index is out of range
    % then behaviour is undefined and bad things are likely to happen.
    %
:- func unsafe_get(bitmap, int) = bool.
:- mode unsafe_get(bitmap_ui, in) = out is det.
:- mode unsafe_get(in, in) = out is det.

    % Create a new copy of a bitmap.
    %
:- func copy(bitmap) = bitmap.
:- mode copy(bitmap_ui) = bitmap_uo is det.

    % Set operations; the second argument is altered in all cases.
    %
:- func complement(bitmap) = bitmap.
:- mode complement(bitmap_di) = bitmap_uo is det.

:- func union(bitmap, bitmap) = bitmap.
:- mode union(bitmap_ui, bitmap_di) = bitmap_uo is det.

:- func intersect(bitmap, bitmap) = bitmap.
:- mode intersect(bitmap_ui, bitmap_di) = bitmap_uo is det.

:- func difference(bitmap, bitmap) = bitmap.
:- mode difference(bitmap_ui, bitmap_di) = bitmap_uo is det.

    % resize(BM, N, B) resizes bitmap BM to have N bits; if N is
    % smaller than the current number of bits in BM then the excess
    % are discarded.  If N is larger than the current number of bits
    % in BM then the new bits are set if B = yes and cleared if
    % B = no.
    %
:- func resize(bitmap, int, bool) = bitmap.
:- mode resize(bitmap_di, in, in) = bitmap_uo is det.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

:- import_module exception, require.

    % A bitmap is represented as an array of ints where each int stores
    % int__bits_per_int bits.  The first element of the array (index 0)
    % is used to hold the number of bits in the bitmap.  This avoids
    % having to create a new bitmap cell on each update.
    %
    % NOTE: the `filler' bits in the last element of the array *must*
    % be clear (i.e. zero).  This makes the set operations simpler to
    % implement.

:- type bitmap == array(int).

% ---------------------------------------------------------------------------- %

new(N, B) = BM :-
    ( if N < 0 then
        throw(software_error("bitmap__new: negative size"))
      else
        X    = initializer(B),
        BM0  = (array__init(num_ints_required(N), X) ^ elem(0) := N),
        BM   = clear_filler_bits(BM0)
    ).

% ---------------------------------------------------------------------------- %

resize(BM0, N, B) = BM :-
    ( if N =< 0 then
        BM      = new(N, B)
      else
        X       = initializer(B),
        NumInts = num_ints_required(N),
        BM1     = array__resize(BM0, NumInts, X),

            % Now we need to ensure that bits N, N+1, N+2, ... up to
            % the word boundary are initialized properly.
            %
        int__min(num_bits(BM0), N, M),
        Offset  = int_offset(M - 1),
        Mask    = bitsmask(M - 1),          % For bits we need to preserve.
        Bits    = \(Mask) /\ X,             % Bits we need to fill in.
        BM2     = (( BM1
                        ^ elem(0)      := N )
                        ^ elem(Offset) := (BM1 ^ elem(Offset) /\ Mask) \/ Bits),
        BM      = clear_filler_bits(BM2)
    ).

% ---------------------------------------------------------------------------- %

:- func clear_filler_bits(bitmap) = bitmap.
:- mode clear_filler_bits(bitmap_di) = bitmap_uo is det.

clear_filler_bits(BM0) = BM :-
    N = num_bits(BM0),
    ( if N > 0 then
        Last = int_offset(N - 1),       % Offset of last bit.
        Ksam = bitsmask(N - 1),         % Masks off the filler bits.
        BM   = BM0 ^ elem(Last) := BM0 ^ elem(Last) /\ Ksam
      else
        BM   = BM0
    ).

% ---------------------------------------------------------------------------- %

:- func initializer(bool) = int.

initializer(no)  = 0.
initializer(yes) = \(0).

% ---------------------------------------------------------------------------- %

num_bits(BM) = BM ^ elem(0).

% ---------------------------------------------------------------------------- %

:- pred in_range(bitmap, int).
:- mode in_range(bitmap_ui, in) is semidet.
:- mode in_range(in, in) is semidet.

in_range(BM, I) :- 0 =< I, I < num_bits(BM).

% ---------------------------------------------------------------------------- %

set(BM, I) =
    ( if in_range(BM, I)
      then BM ^ elem(int_offset(I)) := BM ^ elem(int_offset(I)) \/ bitmask(I)
      else throw(software_error("bitmap__set: out of range"))
    ).

clear(BM, I) =
    ( if in_range(BM, I)
      then BM ^ elem(int_offset(I)) := BM ^ elem(int_offset(I)) /\ \bitmask(I)
      else throw(software_error("bitmap__clear: out of range"))
    ).

flip(BM, I) =
    ( if in_range(BM, I)
      then BM ^ elem(int_offset(I)) := BM ^ elem(int_offset(I)) `xor` bitmask(I)
      else throw(software_error("bitmap__flip: out of range"))
    ).

% ---------------------------------------------------------------------------- %

unsafe_set(BM, I) =
    BM ^ elem(int_offset(I)) := BM ^ elem(int_offset(I)) \/ bitmask(I).

unsafe_clear(BM, I) =
    BM ^ elem(int_offset(I)) := BM ^ elem(int_offset(I)) /\ \bitmask(I).

unsafe_flip(BM, I) =
    BM ^ elem(int_offset(I)) := BM ^ elem(int_offset(I)) `xor` bitmask(I).

% ---------------------------------------------------------------------------- %

is_set(BM, I) :-
    ( if in_range(BM, I)
      then BM ^ elem(int_offset(I)) /\ bitmask(I) \= 0
      else throw(software_error("bitmap__is_set: out of range"))
    ).

is_clear(BM, I) :-
    ( if in_range(BM, I)
      then BM ^ elem(int_offset(I)) /\ bitmask(I) = 0
      else throw(software_error("bitmap__is_clear: out of range"))
    ).

% ---------------------------------------------------------------------------- %

unsafe_is_set(BM, I) :-
    BM ^ elem(int_offset(I)) /\ bitmask(I) \= 0.

unsafe_is_clear(BM, I) :-
    BM ^ elem(int_offset(I)) /\ bitmask(I) = 0.

% ---------------------------------------------------------------------------- %

get(BM, I) = ( if is_clear(BM, I) then no else yes ).

%------------------------------------------------------------------------------%

unsafe_get(BM, I) = ( if unsafe_is_clear(BM, I) then no else yes ).

% ---------------------------------------------------------------------------- %

copy(BM) = array__copy(BM).

% ---------------------------------------------------------------------------- %

complement(BM) =
    clear_filler_bits(complement_2(BM ^ elem(0) - 1, BM)).



:- func complement_2(int, bitmap) = bitmap.
:- mode complement_2(in, bitmap_di) = bitmap_uo is det.

complement_2(WordI, BM) =
    ( if WordI =< 0
      then BM
      else complement_2(WordI - 1, BM ^ elem(WordI) := \(BM ^ elem(WordI)))
    ).

% ---------------------------------------------------------------------------- %

union(BMa, BMb) =
    ( if num_bits(BMa) > num_bits(BMb) then
        zip(int_offset(num_bits(BMb) - 1), (\/), BMb, bitmap__copy(BMa))
      else
        zip(int_offset(num_bits(BMa) - 1), (\/), BMa, BMb)
    ).

% ---------------------------------------------------------------------------- %

intersect(BMa, BMb) =
    ( if num_bits(BMa) > num_bits(BMb) then
        zip(int_offset(num_bits(BMb) - 1), (/\), BMb, bitmap__copy(BMa))
      else
        zip(int_offset(num_bits(BMa) - 1), (/\), BMa, BMb)
    ).

% ---------------------------------------------------------------------------- %

difference(BMa, BMb) =
    ( if num_bits(BMa) > num_bits(BMb) then
        zip(int_offset(num_bits(BMb) - 1), Xor, BMb, bitmap__copy(BMa))
      else
        zip(int_offset(num_bits(BMa) - 1), Xor, BMa, BMb)
    )
 :-
    Xor = ( func(X, Y) = (X `xor` Y) ).

% ---------------------------------------------------------------------------- %

    % Applies a function to every corresponding element between +ve I
    % and 1 inclusive, destructively updating the second bitmap.
    %
:- func zip(int, func(int, int) = int, bitmap, bitmap) = bitmap.
:- mode zip(in, func(in, in) = out is det, bitmap_ui, bitmap_di) = bitmap_uo
            is det.

zip(I, Fn, BMa, BMb) =
    ( if I > 0 then
        zip(I - 1, Fn, BMa, BMb ^ elem(I) := Fn(BMb ^ elem(I), BMa ^ elem(I)))
      else
        BMb
    ).

% ---------------------------------------------------------------------------- %

    % The size of the array required to hold an N-bit bitmap.
    %
:- func num_ints_required(int) = int.

    % We add the 1 on because arrays of size N are indexed 0 .. N - 1.
    %
num_ints_required(N) = 1 + ( if N > 0 then int_offset(N) else 0 ).

% ---------------------------------------------------------------------------- %

    % The array index containing the given bit.
    %
:- func int_offset(int) = int.

    % We add the extra 1 on because elem(0) is used to store the number
    % of bits in the bitmap; the data are stored in the following elements.
    %
int_offset(I) = 1 + int__quot_bits_per_int(I).

% ---------------------------------------------------------------------------- %

    % Construct the bitmask for a given bit in a word.
    %
    % E.g. assuming int__bits_per_int = 8 and I = 11 then
    % bitmask(I) = 2'00001000
    %
:- func bitmask(int) = int.

    % NOTE: it would be nicer to use /\ with a bitmask here rather
    % than rem.  Do modern back-ends do the decent thing here if
    % int__bits_per_int is the expected power of two?
    %
bitmask(I) = 1 `unchecked_left_shift` int__rem_bits_per_int(I).

% ---------------------------------------------------------------------------- %

    % Construct the bitmask for all the bits up to and including
    % the given bit in a word.
    %
    % E.g. assuming int__bits_per_int = 8 and I = 11 then
    % bitmask(I) = 2'00001111
    %
:- func bitsmask(int) = int.

bitsmask(I) = BitsMask :-
    BitMask  = bitmask(I),
    BitsMask = BitMask \/ (BitMask - 1).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
