%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2007, 2009-2011 The University of Melbourne
% Copyright (C) 2013-2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: bitmap.m.
% Main author: rafe, stayl.
% Stability: low.
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
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module bitmap.
:- interface.

:- import_module bool.
:- import_module list.

%---------------------------------------------------------------------------%

    % Type `bitmap' is equivalent to `array(bool)', but is implemented
    % much more efficiently. Accessing bitmaps as if they were an array
    % of eight bit bytes is especially efficient.
    %
    % See runtime/mercury_types.h for the definition of MR_BitmapPtr for
    % use in foreign code.
    %
    % Comparison of bitmaps first compares the size, if the size is equal
    % then each bit in turn is compared starting from bit zero.
    %
:- type bitmap.

:- inst bitmap == ground.
:- inst uniq_bitmap == bitmap.  % XXX should be unique
:- mode bitmap_di == in(uniq_bitmap). % XXX should be di
:- mode bitmap_uo == out(uniq_bitmap).
:- mode bitmap_ui == in(uniq_bitmap).

    % The exception thrown for any error.
    %
:- type bitmap_error
    --->    bitmap_error(string).

%---------------------------------------------------------------------------%

:- type bit_index == int.
:- type byte_index == int.
:- type num_bits == int.
:- type num_bytes == int.

    % 8 bits stored in the least significant bits of the integer.
    %
:- type byte == int.

    % An integer interpreted as a vector of int.bits_per_int bits.
    %
:- type word == int.

%---------------------------------------------------------------------------%

    % init(N, B) creates a bitmap of size N (indexed 0 .. N-1)
    % setting each bit if B = yes and clearing each bit if B = no.
    % An exception is thrown if N is negative.
    %
:- func init(num_bits::in, bool::in) = (bitmap::bitmap_uo) is det.

    % A synonym for init(N, no).
    %
:- func init(num_bits::in) = (bitmap::bitmap_uo) is det.

    % Is the given bit number in range?
    %
:- pred in_range(bitmap, bit_index).
% :- mode in_range(bitmap_ui, in) is semidet.
:- mode in_range(in, in) is semidet.

    % Is the given byte number in range?
    %
:- pred byte_in_range(bitmap, byte_index).
% :- mode byte_in_range(bitmap_ui, in) is semidet.
:- mode byte_in_range(in, in) is semidet.

    % Return the number of bits in a bitmap.
    %
:- func num_bits(bitmap) = num_bits.
% :- mode num_bits(bitmap_ui) = out is det.
:- mode num_bits(in) = out is det.

    % Return the number of bytes in a bitmap, failing if the bitmap
    % has a partial final byte.
    %
:- func num_bytes(bitmap) = num_bytes.
% :- mode num_bytes(bitmap_ui) = out is semidet.
:- mode num_bytes(in) = out is semidet.

    % As above, but throw an exception if the bitmap has a partial final byte.
    %
:- func det_num_bytes(bitmap) = num_bytes.
% :- mode det_num_bytes(bitmap_ui) = out is det.
:- mode det_num_bytes(in) = out is det.

    % Return the number of bits in a byte (always 8).
    %
:- func bits_per_byte = int.

    % is_empty(Bitmap):
    % True iff Bitmap is a bitmap containing zero bits.
    %
:- pred is_empty(bitmap).
%:- mode is_empty(bitmap_ui) is semidet.
:- mode is_empty(in) is semidet.

%---------------------------------------------------------------------------%
%
% Get or set the given bit.
% The unsafe versions do not check whether the bit is in range.
%

:- func bitmap      ^ bit(bit_index)    = bool.
% :- mode bitmap_ui ^ bit(in)           = out is det.
:- mode in          ^ bit(in)           = out is det.

:- func bitmap      ^ unsafe_bit(bit_index) = bool.
% :- mode bitmap_ui ^ unsafe_bit(in)        = out is det.
:- mode in          ^ unsafe_bit(in)        = out is det.

:- func (bitmap     ^ bit(bit_index)    := bool)    = bitmap.
:- mode (bitmap_di  ^ bit(in)           := in)      = bitmap_uo is det.

:- func (bitmap     ^ unsafe_bit(bit_index) := bool) = bitmap.
:- mode (bitmap_di  ^ unsafe_bit(in)        := in)   = bitmap_uo is det.

%---------------------------------------------------------------------------%
%
% Bitmap ^ bits(OffSet, NumBits) = Word.
% The low order bits of Word contain the NumBits bits of BitMap
% starting at OffSet.
% 0 =< NumBits =< int.bits_per_int.
%

:- func bitmap      ^ bits(bit_index, num_bits) = word.
% :- mode bitmap_ui ^ bits(in, in)              = out is det.
:- mode in          ^ bits(in, in)              = out is det.

:- func bitmap      ^ unsafe_bits(bit_index, num_bits)  = word.
% :- mode bitmap_ui ^ unsafe_bits(in, in)               = out is det.
:- mode in          ^ unsafe_bits(in, in)               = out is det.

:- func (bitmap     ^ bits(bit_index, num_bits) := word) = bitmap.
:- mode (bitmap_di  ^ bits(in, in)              := in)   = bitmap_uo is det.

:- func (bitmap     ^ unsafe_bits(bit_index, num_bits) := word) = bitmap.
:- mode (bitmap_di  ^ unsafe_bits(in, in)              := in)   = bitmap_uo
    is det.

%---------------------------------------------------------------------------%
%
% BM ^ byte(ByteNumber)
% Get or set the given numbered byte (multiply ByteNumber by
% bits_per_byte to get the bit index of the start of the byte).
%
% The bits are stored in or taken from the least significant bits of an int.
% The safe versions will throw an exception if the given ByteNumber is out of
% bounds.  Final partial bytes are out of bounds.  The unsafe versions do not
% check whether the byte is in range.
%

:- func bitmap      ^ byte(byte_index) = byte.
% :- mode bitmap_ui ^ byte(in) = out is det.
:- mode in          ^ byte(in) = out is det.

:- func bitmap      ^ unsafe_byte(byte_index)   = byte.
% :- mode bitmap_ui ^ unsafe_byte(in)           = out is det.
:- mode in          ^ unsafe_byte(in)           = out is det.

:- func (bitmap     ^ byte(byte_index)  := byte) = bitmap.
:- mode (bitmap_di  ^ byte(in)          := in)   = bitmap_uo is det.

:- func (bitmap     ^ unsafe_byte(byte_index)   := byte) = bitmap.
:- mode (bitmap_di  ^ unsafe_byte(in)           := in)   = bitmap_uo is det.

%
% Versions of the above that set or take uint8 values instead of a byte stored
% in the least significant bits of an int.
%

:- func get_uint8(bitmap, byte_index) = uint8.
%:- mode get_uint8(bitmap_ui, in) = out is det.
:- mode get_uint8(in, in) = out is det.

:- func unsafe_get_uint8(bitmap, byte_index) = uint8.
%:- mode unsafe_get_uint8(bitmap_ui, in) = out is det.
:- mode unsafe_get_uint8(in, in) = out is det.

:- pred set_uint8(byte_index::in, uint8::in,
    bitmap::bitmap_di, bitmap::bitmap_uo) is det.

:- pred unsafe_set_uint8(byte_index::in, uint8::in,
    bitmap::bitmap_di, bitmap::bitmap_uo) is det.

%---------------------------------------------------------------------------%

    % Flip the given bit.
    %
:- func flip(bitmap, bit_index) = bitmap.
:- mode flip(bitmap_di, in) = bitmap_uo is det.

:- pred flip(bit_index, bitmap, bitmap).
:- mode flip(in, bitmap_di, bitmap_uo) is det.

%---------------------------------------------------------------------------%
%
% Variations that might be slightly more efficient by not
% converting bits to bool.
%

:- func set(bitmap, bit_index) = bitmap.
:- mode set(bitmap_di, in) = bitmap_uo is det.

:- pred set(bit_index, bitmap, bitmap).
:- mode set(in, bitmap_di, bitmap_uo) is det.

:- func clear(bitmap, bit_index) = bitmap.
:- mode clear(bitmap_di, in) = bitmap_uo is det.

:- pred clear(bit_index, bitmap, bitmap).
:- mode clear(in, bitmap_di, bitmap_uo) is det.

    % is_set(BM, I) and is_clear(BM, I) succeed iff bit I in BM
    % is set or clear respectively.
    %
:- pred is_set(bitmap, bit_index).
% :- mode is_set(bitmap_ui, in) is semidet.
:- mode is_set(in, in) is semidet.

:- pred is_clear(bitmap, bit_index).
% :- mode is_clear(bitmap_ui, in) is semidet.
:- mode is_clear(in, in) is semidet.

%---------------------------------------------------------------------------%
%
% Unsafe versions of the above: if the index is out of range
% then behaviour is undefined and bad things are likely to happen.
%

:- func unsafe_flip(bitmap, bit_index) = bitmap.
:- mode unsafe_flip(bitmap_di, in) = bitmap_uo is det.

:- pred unsafe_flip(bit_index, bitmap, bitmap).
:- mode unsafe_flip(in, bitmap_di, bitmap_uo) is det.

:- func unsafe_set(bitmap, bit_index) = bitmap.
:- mode unsafe_set(bitmap_di, in) = bitmap_uo is det.

:- pred unsafe_set(bit_index, bitmap, bitmap).
:- mode unsafe_set(in, bitmap_di, bitmap_uo) is det.

:- func unsafe_clear(bitmap, bit_index) = bitmap.
:- mode unsafe_clear(bitmap_di, in) = bitmap_uo is det.

:- pred unsafe_clear(bit_index, bitmap, bitmap).
:- mode unsafe_clear(in, bitmap_di, bitmap_uo) is det.

:- pred unsafe_is_set(bitmap, bit_index).
% :- mode unsafe_is_set(bitmap_ui, in) is semidet.
:- mode unsafe_is_set(in, in) is semidet.

:- pred unsafe_is_clear(bitmap, bit_index).
% :- mode unsafe_is_clear(bitmap_ui, in) is semidet.
:- mode unsafe_is_clear(in, in) is semidet.

%---------------------------------------------------------------------------%

    % Create a new copy of a bitmap.
    %
:- func copy(bitmap) = bitmap.
% :- mode copy(bitmap_ui) = bitmap_uo is det.
:- mode copy(in) = bitmap_uo is det.

    % resize(BM, N, B) resizes bitmap BM to have N bits; if N is
    % smaller than the current number of bits in BM then the excess
    % are discarded. If N is larger than the current number of bits
    % in BM then the new bits are set if B = yes and cleared if
    % B = no.
    %
:- func resize(bitmap, num_bits, bool) = bitmap.
:- mode resize(bitmap_di, in, in) = bitmap_uo is det.

    % Shrink a bitmap without copying it into a smaller memory allocation.
    %
:- func shrink_without_copying(bitmap, num_bits) = bitmap.
:- mode shrink_without_copying(bitmap_di, in) = bitmap_uo is det.

%---------------------------------------------------------------------------%

    % Slice = slice(BM, StartIndex, NumBits)
    %
    % A bitmap slice represents the sub-range of a bitmap of NumBits bits
    % starting at bit index StartIndex. Throws an exception if the slice
    % is not within the bounds of the bitmap.
    %
:- type slice.
:- func slice(bitmap, bit_index, num_bits) = bitmap.slice.

    % As above, but use byte indices.
    %
:- func byte_slice(bitmap, byte_index, num_bytes) = slice.

    % Access functions for slices.
    %
:- func slice ^ slice_bitmap = bitmap.
:- func slice ^ slice_start_bit_index = bit_index.
:- func slice ^ slice_num_bits = num_bits.

    % As above, but return byte indices, throwing an exception if
    % the slice doesn't start and end on a byte boundary.
    %
:- func slice ^ slice_start_byte_index = byte_index.
:- func slice ^ slice_num_bytes = num_bytes.

%---------------------------------------------------------------------------%
%
% Set operations; for binary operations the second argument is altered
% in all cases. The input bitmaps must have the same size.
%

:- func complement(bitmap) = bitmap.
:- mode complement(bitmap_di) = bitmap_uo is det.

:- func union(bitmap, bitmap) = bitmap.
% :- mode union(bitmap_ui, bitmap_di) = bitmap_uo is det.
:- mode union(in, bitmap_di) = bitmap_uo is det.

:- func intersect(bitmap, bitmap) = bitmap.
% :- mode intersect(bitmap_ui, bitmap_di) = bitmap_uo is det.
:- mode intersect(in, bitmap_di) = bitmap_uo is det.

:- func difference(bitmap, bitmap) = bitmap.
% :- mode difference(bitmap_ui, bitmap_di) = bitmap_uo is det.
:- mode difference(in, bitmap_di) = bitmap_uo is det.

:- func xor(bitmap, bitmap) = bitmap.
% :- mode xor(bitmap_ui, bitmap_di) = bitmap_uo is det.
:- mode xor(in, bitmap_di) = bitmap_uo is det.

%---------------------------------------------------------------------------%

    % Condense a list of bitmaps into a single bitmap.
    %
:- func append_list(list(bitmap)) = bitmap.
:- mode append_list(in) = bitmap_uo is det.

%---------------------------------------------------------------------------%
%
% Operations to copy part of a bitmap.
%

    % copy_bits(SrcBM, SrcStartBit, DestBM, DestStartBit, NumBits)
    %
    % Overwrite NumBits bits in DestBM starting at DestStartBit with
    % the NumBits bits starting at SrcStartBit in SrcBM.
    %
:- func copy_bits(bitmap, bit_index, bitmap, bit_index, num_bits) = bitmap.
% :- mode copy_bits(bitmap_ui, in, bitmap_di, in, in) = bitmap_uo is det.
:- mode copy_bits(in, in, bitmap_di, in, in) = bitmap_uo is det.

    % copy_bits_in_bitmap(BM, SrcStartBit, DestStartBit, NumBits)
    %
    % Overwrite NumBits bits starting at DestStartBit with the NumBits
    % bits starting at SrcStartBit in the same bitmap.
    %
:- func copy_bits_in_bitmap(bitmap, bit_index, bit_index, num_bits) = bitmap.
:- mode copy_bits_in_bitmap(bitmap_di, in, in, in) = bitmap_uo is det.

    % copy_bytes(SrcBM, SrcStartByte, DestBM, DestStartByte, NumBytes)
    %
    % Overwrite NumBytes bytes in DestBM starting at DestStartByte with
    % the NumBytes bytes starting at SrcStartByte in SrcBM.
    %
:- func copy_bytes(bitmap, byte_index, bitmap, byte_index, num_bytes) = bitmap.
% :- mode copy_bytes(bitmap_ui, in, bitmap_di, in, in) = bitmap_uo is det.
:- mode copy_bytes(in, in, bitmap_di, in, in) = bitmap_uo is det.

    % copy_bytes_in_bitmap(BM, SrcStartByte, DestStartByte, NumBytes)
    %
    % Overwrite NumBytes bytes starting at DestStartByte with the NumBytes
    % bytes starting at SrcStartByte in the same bitmap.
    %
:- func copy_bytes_in_bitmap(bitmap, byte_index,
    byte_index, num_bytes) = bitmap.
:- mode copy_bytes_in_bitmap(bitmap_di, in, in, in) = bitmap_uo is det.

%---------------------------------------------------------------------------%

    % Convert a bitmap to a string of the form "<length:hex digits>",
    % e.g. "<24:10AFBD>".
    %
:- func to_string(bitmap) = string.
% :- mode to_string(bitmap_ui) = out is det.
:- mode to_string(in) = out is det.

    % Convert a string created by to_string back into a bitmap.
    % Fails if the string is not of the form created by to_string.
    %
:- func from_string(string) = bitmap.
:- mode from_string(in) = bitmap_uo is semidet.

    % As above, but throws an exception instead of failing.
    %
:- func det_from_string(string) = bitmap.
:- mode det_from_string(in) = bitmap_uo is det.

    % Convert a bitmap to a string of `1' and `0' characters, where
    % the bytes are separated by `.'.
    %
:- func to_byte_string(bitmap) = string.
% :- mode to_byte_string(bitmap_ui) = out is det.
:- mode to_byte_string(in) = out is det.

%---------------------------------------------------------------------------%

    % Compute a hash function for a bitmap.
    %
:- func hash(bitmap) = int.
% :- mode hash(bitmap_ui) = out is det.
:- mode hash(in) = out is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.
:- interface.

    % Used by io.m.

    % throw_bounds_error(BM, PredName, Index, NumBits):
    %
:- pred throw_bounds_error(bitmap::in, string::in, bit_index::in, num_bits::in)
    is erroneous.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

init(N, B) = BM :-
    ( if N < 0 then
        throw_bitmap_error("bitmap.init: negative size")
    else
        X    = initializer(B),
        BM0  = initialize_bitmap(allocate_bitmap(N), N, X),
        BM   = clear_filler_bits(BM0)
    ).

init(N) = init(N, no).

:- func clear_filler_bits(bitmap) = bitmap.
:- mode clear_filler_bits(bitmap_di) = bitmap_uo is det.

clear_filler_bits(BM) = set_trailing_bits_in_byte(BM, num_bits(BM) - 1, 0).

:- func set_trailing_bits_in_byte(bitmap, bit_index, byte) = bitmap.
:- mode set_trailing_bits_in_byte(bitmap_di, in, in) = bitmap_uo is det.

set_trailing_bits_in_byte(!.BM, Bit, Initializer) = !:BM :-
    FirstTrailingBit = Bit + 1,
    FirstTrailingBitIndex = bit_index_in_byte(FirstTrailingBit),
    ( if FirstTrailingBitIndex \= 0 then
        ByteIndex = byte_index_for_bit(FirstTrailingBit),
        NumBitsToSet = bits_per_byte - FirstTrailingBitIndex,
        !:BM = !.BM ^ unsafe_byte(ByteIndex) :=
            set_bits_in_byte(!.BM ^ unsafe_byte(ByteIndex),
                FirstTrailingBitIndex, NumBitsToSet, Initializer)
    else
        true
    ).

:- func initializer(bool) = byte.

initializer(no)  = 0.
initializer(yes) = \0.

:- func initialize_bitmap(bitmap, num_bits, byte) = bitmap.
:- mode initialize_bitmap(bitmap_di, in, in) = bitmap_uo is det.

initialize_bitmap(Bitmap, N, Init) =
    initialize_bitmap_bytes(Bitmap, 0, byte_index_for_bit(N - 1), Init).

:- func initialize_bitmap_bytes(bitmap, byte_index, byte_index,
    byte) = bitmap.
:- mode initialize_bitmap_bytes(bitmap_di, in, in, in) = bitmap_uo is det.

initialize_bitmap_bytes(BM, ByteIndex, LastByteIndex, Init) =
    ( if ByteIndex > LastByteIndex then
        BM
    else
        initialize_bitmap_bytes(BM ^ unsafe_byte(ByteIndex) := Init,
            ByteIndex + 1, LastByteIndex, Init)
    ).

%---------------------------------------------------------------------------%

in_range(BM, I) :-
    0 =< I, I < num_bits(BM).

:- pred in_range_rexcl(bitmap, bit_index).
:- mode in_range_rexcl(in, in) is semidet.

in_range_rexcl(BM, I) :-
    0 =< I, I =< num_bits(BM).

byte_in_range(BM, I) :-
    in_range(BM, I * bits_per_byte + bits_per_byte - 1).

%---------------------------------------------------------------------------%

num_bits(_) = _ :-
    private_builtin.sorry("bitmap.num_bits").

:- pragma foreign_proc("C",
    num_bits(BM::in) = (NumBits::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    NumBits = BM->num_bits;
").

:- pragma foreign_proc("Java",
    num_bits(BM::in) = (NumBits::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    NumBits = BM.num_bits;
").

:- pragma foreign_proc("C#",
    num_bits(BM::in) = (NumBits::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    NumBits = BM.num_bits;
").

:- pragma foreign_proc("Erlang",
    num_bits(BM::in) = (NumBits::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    {_, NumBits} = BM
").

%---------------------------------------------------------------------------%

num_bytes(BM) = Bytes :-
    NumBits = BM ^ num_bits,
    NumBits `unchecked_rem` bits_per_byte = 0,
    Bytes = NumBits `unchecked_quotient` bits_per_byte.

det_num_bytes(BM) = Bytes :-
    ( if Bytes0 = num_bytes(BM) then
        Bytes = Bytes0
    else
        throw_bitmap_error(
            "bitmap.det_num_bytes: bitmap has a partial final byte")
    ).

bits_per_byte = 8.

%---------------------------------------------------------------------------%

is_empty(BM) :-
    BM ^ num_bits = 0.

%---------------------------------------------------------------------------%

BM ^ bit(I) = B :-
    ( if in_range(BM, I) then
        B = BM ^ unsafe_bit(I)
    else
        throw_bit_bounds_error(BM, "bitmap.bit", I)
    ).

BM ^ unsafe_bit(I) =
    ( if unsafe_is_set(BM, I) then yes else no ).

(!.BM ^ bit(I) := B) = !:BM :-
    ( if in_range(!.BM, I) then
        !BM ^ unsafe_bit(I) := B
    else
        throw_bit_bounds_error(!.BM, "bitmap.'bit :='", I)
    ).

(BM ^ unsafe_bit(I) := yes) = unsafe_set(BM, I).
(BM ^ unsafe_bit(I) := no) = unsafe_clear(BM, I).

%---------------------------------------------------------------------------%

BM ^ bits(FirstBit, NumBits) = Word :-
    ( if
        FirstBit >= 0,
        NumBits >= 0,
        NumBits =< int.bits_per_int,
        in_range_rexcl(BM, FirstBit + NumBits)
    then
        Word = BM ^ unsafe_bits(FirstBit, NumBits)
    else if
        ( NumBits < 0
        ; NumBits > int.bits_per_int
        )
    then
        throw_bitmap_error("bitmap.bits: number of bits must be between " ++
            "0 and `int.bits_per_int'.")
    else
        throw_bit_bounds_error(BM, "bitmap.bits", FirstBit)
    ).

BM ^ unsafe_bits(FirstBit, NumBits) = Bits :-
    FirstByteIndex = byte_index_for_bit(FirstBit),
    FirstBitIndex = bit_index_in_byte(FirstBit),
    extract_bits_from_bytes(FirstByteIndex, FirstBitIndex,
        NumBits, BM, 0, Bits).

    % Extract the given number of bits starting at the most significant.
    %
:- pred extract_bits_from_bytes(byte_index, bit_index_in_byte, num_bits,
    bitmap, word, word).
% :- mode extract_bits_from_bytes(in, in, in, bitmap_ui, in, out) is det.
:- mode extract_bits_from_bytes(in, in, in, in, in, out) is det.

extract_bits_from_bytes(FirstByteIndex, FirstBitIndex, NumBits, BM, !Bits) :-
    RemainingBitsInByte = bits_per_byte - FirstBitIndex,
    ( if
        NumBits > RemainingBitsInByte
    then
        NumBitsThisByte = RemainingBitsInByte,
        extract_bits_from_byte_index(FirstByteIndex, FirstBitIndex,
            NumBitsThisByte, BM, !Bits),
        extract_bits_from_bytes(FirstByteIndex + 1, 0,
            NumBits - NumBitsThisByte, BM, !Bits)
    else if
        NumBits > 0
    then
        extract_bits_from_byte_index(FirstByteIndex, FirstBitIndex,
            NumBits, BM, !Bits)
    else
        true
    ).

:- pred extract_bits_from_byte_index(byte_index, bit_index_in_byte, num_bits,
    bitmap, word, word).
% :- mode extract_bits_from_byte_index(in, in, in, bitmap_ui, in, out) is det.
:- mode extract_bits_from_byte_index(in, in, in, in, in, out) is det.

extract_bits_from_byte_index(ByteIndex, FirstBitIndex,
        NumBitsThisByte, BM, !Bits) :-
    BitsThisByte = extract_bits_from_byte(BM ^ unsafe_byte(ByteIndex),
        FirstBitIndex, NumBitsThisByte),
    !:Bits = (!.Bits `unchecked_left_shift` NumBitsThisByte) \/ BitsThisByte.

%---------------------------------------------------------------------------%

(!.BM ^ bits(FirstBit, NumBits) := Bits) = !:BM :-
    ( if
        FirstBit >= 0,
        NumBits >= 0,
        NumBits =< int.bits_per_int,
        in_range_rexcl(!.BM, FirstBit + NumBits)
    then
        !BM ^ unsafe_bits(FirstBit, NumBits) := Bits
    else if
        ( NumBits < 0
        ; NumBits > int.bits_per_int
        )
    then
        throw_bitmap_error(
            "bitmap.'bits :=': number of bits must be between " ++
            "0 and `int.bits_per_int'.")
    else
        throw_bit_bounds_error(!.BM, "bitmap.'bits :='", FirstBit)
    ).

(BM0 ^ unsafe_bits(FirstBit, NumBits) := Bits) = BM :-
    LastBit = FirstBit + NumBits - 1,
    LastByteIndex = byte_index_for_bit(LastBit),
    LastBitIndex = bit_index_in_byte(LastBit),
    set_bits_in_bytes(LastByteIndex, LastBitIndex, NumBits, Bits, BM0, BM).

    % Set the given number of bits starting at the least significant.
    %
:- pred set_bits_in_bytes(byte_index, bit_index_in_byte, num_bits,
    word, bitmap, bitmap).
:- mode set_bits_in_bytes(in, in, in, in, bitmap_di, bitmap_uo) is det.

set_bits_in_bytes(LastByteIndex, LastBitIndex, NumBits, Bits, !BM) :-
    RemainingBitsInByte = LastBitIndex + 1,
    ( if NumBits > RemainingBitsInByte then
        NumBitsThisByte = RemainingBitsInByte,
        set_bits_in_byte_index(LastByteIndex, LastBitIndex, NumBitsThisByte,
            Bits, !BM),
        set_bits_in_bytes(LastByteIndex - 1, bits_per_byte - 1,
            NumBits - NumBitsThisByte,
            Bits `unchecked_right_shift` NumBitsThisByte, !BM)
    else if NumBits > 0 then
        set_bits_in_byte_index(LastByteIndex, LastBitIndex, NumBits, Bits, !BM)
    else
        true
    ).

:- pred set_bits_in_byte_index(byte_index, bit_index_in_byte, num_bits,
    word, bitmap, bitmap).
:- mode set_bits_in_byte_index(in, in, in, in, bitmap_di, bitmap_uo) is det.

set_bits_in_byte_index(ByteIndex, LastBitIndex, NumBitsThisByte, Bits, !BM) :-
    FirstBitInByte = LastBitIndex - NumBitsThisByte + 1,
    !:BM = !.BM ^ unsafe_byte(ByteIndex) :=
        set_bits_in_byte(!.BM ^ unsafe_byte(ByteIndex),
            FirstBitInByte, NumBitsThisByte, Bits).

%---------------------------------------------------------------------------%

BM ^ byte(N) = Byte :-
    ( if byte_in_range(BM, N) then
        Byte = BM ^ unsafe_byte(N)
    else
        throw_byte_bounds_error(BM, "bitmap.byte", N)
    ).

_ ^ unsafe_byte(_) = _ :-
    private_builtin.sorry("bitmap.unsafe_byte").

:- pragma foreign_proc("C",
    unsafe_byte(N::in, BM::in) = (Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Byte = (MR_Integer) BM->elements[N];
").

:- pragma foreign_proc("Java",
    unsafe_byte(N::in, BM::in) = (Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    // Mask off sign bits so Byte is in range 0-255.
    Byte = ((int) BM.elements[N]) & 0xff;
").

:- pragma foreign_proc("C#",
    unsafe_byte(N::in, BM::in) = (Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Byte = BM.elements[N];
").

:- pragma foreign_proc("Erlang",
    unsafe_byte(N::in, BM::in) = (Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    {Bin, _} = BM,
    <<_:N/binary, Byte/integer, _/binary>> = Bin
").

%---------------------------------------------------------------------------%

get_uint8(BM, N) = U8 :-
    ( if byte_in_range(BM, N) then
        U8 = unsafe_get_uint8(BM, N)
    else
        throw_byte_bounds_error(BM, "bitmap.get_uint8", N)
    ).

:- pragma foreign_proc("C",
    unsafe_get_uint8(BM::in, N::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U8 = (uint8_t) BM->elements[N];
").

:- pragma foreign_proc("Java",
    unsafe_get_uint8(BM::in, N::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U8 = BM.elements[N];
").

:- pragma foreign_proc("C#",
    unsafe_get_uint8(BM::in, N::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U8 = BM.elements[N];
").

:- pragma foreign_proc("Erlang",
    unsafe_get_uint8(BM::in, N::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    {Bin, _} = BM,
    <<_:N/binary, U8/integer, _/binary>> = Bin
").

%---------------------------------------------------------------------------%

(!.BM ^ byte(N) := Byte) = !:BM :-
    ( if byte_in_range(!.BM, N) then
        !BM ^ unsafe_byte(N) := Byte
    else
        throw_byte_bounds_error(!.BM, "bitmap.'byte :='", N)
    ).

(_ ^ unsafe_byte(_) := _) = _ :-
    private_builtin.sorry("bitmap.'unsafe_byte :='").

:- pragma foreign_proc("C",
    'unsafe_byte :='(N::in, BM0::bitmap_di, Byte::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = BM0;
    BM->elements[N] = (MR_uint_least8_t) Byte;
").

:- pragma foreign_proc("Java",
    'unsafe_byte :='(N::in, BM0::bitmap_di, Byte::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = BM0;
    BM.elements[N] = (byte) Byte;
").

:- pragma foreign_proc("C#",
    'unsafe_byte :='(N::in, BM0::bitmap_di, Byte::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = BM0;
    BM.elements[N] = (byte) Byte;
").

:- pragma foreign_proc("Erlang",
    'unsafe_byte :='(N::in, BM0::bitmap_di, Byte::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    {Bin0, NumBits} = BM0,
    <<Left:N/binary, _/integer, Right/binary>> = Bin0,
    Bin = <<Left/binary, Byte/integer, Right/binary>>,
    BM = {Bin, NumBits}
").

%---------------------------------------------------------------------------%

set_uint8(N, U8, !BM) :-
    ( if byte_in_range(!.BM, N) then
        unsafe_set_uint8(N, U8, !BM)
    else
        throw_byte_bounds_error(!.BM, "bitmap.set_uint", N)
    ).

:- pragma foreign_proc("C",
    unsafe_set_uint8(N::in, U8::in, BM0::bitmap_di, BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = BM0;
    BM->elements[N] = (MR_uint_least8_t) U8;
").

:- pragma foreign_proc("Java",
    unsafe_set_uint8(N::in, U8::in, BM0::bitmap_di, BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BM = BM0;
    BM.elements[N] = (byte) U8;
").

:- pragma foreign_proc("C#",
    unsafe_set_uint8(N::in, U8::in, BM0::bitmap_di, BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BM = BM0;
    BM.elements[N] = (byte) U8;
").

:- pragma foreign_proc("Erlang",
    unsafe_set_uint8(N::in, U8::in, BM0::bitmap_di, BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    {Bin0, NumBits} = BM0,
    <<Left:N/binary, _/integer, Right/binary>> = Bin0,
    Bin = <<Left/binary, U8/integer, Right/binary>>,
    BM = {Bin, NumBits}
").

%---------------------------------------------------------------------------%

flip(!.BM, I) = !:BM :-
    flip(I, !BM).

flip(I, !BM) :-
    ( if in_range(!.BM, I) then
        !:BM = unsafe_flip(!.BM, I)
    else
        throw_bit_bounds_error(!.BM, "bitmap.flip", I)
    ).

set(!.BM, I) = !:BM :-
    set(I, !BM).

set(I, !BM) :-
    ( if in_range(!.BM, I) then
        !:BM = unsafe_set(!.BM, I)
    else
        throw_bit_bounds_error(!.BM, "bitmap.set", I)
    ).

clear(!.BM, I) = !:BM :-
    clear(I, !BM).

clear(I, !BM) :-
    ( if in_range(!.BM, I) then
        !:BM = unsafe_clear(!.BM, I)
    else
        throw_bit_bounds_error(!.BM, "bitmap.clear", I)
    ).

is_set(BM, I) :-
    ( if in_range(BM, I) then
        unsafe_is_set(BM, I)
    else
        throw_bit_bounds_error(BM, "bitmap.is_set", I)
    ).

is_clear(BM, I) :-
    ( if in_range(BM, I) then
        unsafe_is_clear(BM, I)
    else
        throw_bit_bounds_error(BM, "bitmap.is_clear", I)
    ).

%---------------------------------------------------------------------------%

unsafe_flip(BM0, I) = BM :-
    ByteIndex = byte_index_for_bit(I),
    Byte0 = BM0 ^ unsafe_byte(ByteIndex),
    Byte = Byte0 `xor` bitmask(I),
    BM = BM0 ^ unsafe_byte(ByteIndex) := Byte.

unsafe_flip(I, BM, unsafe_flip(BM, I)).

unsafe_set(BM0, I) = BM :-
    ByteIndex = byte_index_for_bit(I),
    Byte0 = BM0 ^ unsafe_byte(ByteIndex),
    Byte = Byte0 \/ bitmask(I),
    BM = BM0 ^ unsafe_byte(ByteIndex) := Byte.

unsafe_set(I, BM, unsafe_set(BM, I)).

unsafe_clear(BM0, I) = BM :-
    ByteIndex = byte_index_for_bit(I),
    Byte0 = BM0 ^ unsafe_byte(ByteIndex),
    Byte = Byte0 /\ \bitmask(I),
    BM = BM0 ^ unsafe_byte(ByteIndex) := Byte.

unsafe_clear(I, BM, unsafe_clear(BM, I)).

unsafe_is_set(BM, I) :-
    not unsafe_is_clear(BM, I).

unsafe_is_clear(BM, I) :-
    BM ^ unsafe_byte(byte_index_for_bit(I)) /\ bitmask(I) = 0.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    copy(BM0::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    MR_allocate_bitmap_msg(BM, BM0->num_bits, MR_ALLOC_ID);
    MR_copy_bitmap(BM, BM0);
").

:- pragma foreign_proc("C#",
    copy(BM0::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = new mercury.runtime.MercuryBitmap(BM0.num_bits);
    System.Array.Copy(BM0.elements, 0, BM.elements, 0, BM0.elements.Length);
").

:- pragma foreign_proc("Java",
    copy(BM0::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = new MercuryBitmap(BM0.num_bits);
    System.arraycopy(BM0.elements, 0, BM.elements, 0, BM0.elements.length);
").

:- pragma foreign_proc("Erlang",
    copy(BM0::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = BM0
").

copy(BM0) = BM :-
    NumBits = BM0 ^ num_bits,
    BM = clear_filler_bits(
        unsafe_copy_bits(0, BM0, 0, allocate_bitmap(NumBits), 0, NumBits)).

%---------------------------------------------------------------------------%

resize(!.BM, NewSize, InitializerBit) = !:BM :-
    ( if NewSize =< 0 then
        !:BM = init(NewSize, InitializerBit)
    else
        OldSize = num_bits(!.BM),
        InitializerByte = initializer(InitializerBit),
        !:BM = resize_bitmap(!.BM, NewSize),
        ( if NewSize > OldSize then
            % Fill in the trailing bits in the previous final byte.
            !:BM = set_trailing_bits_in_byte(!.BM, OldSize - 1,
                InitializerByte),
            OldLastByteIndex = byte_index_for_bit(OldSize - 1),
            NewLastByteIndex = byte_index_for_bit(NewSize - 1),
            ( if NewLastByteIndex > OldLastByteIndex then
                !:BM = initialize_bitmap_bytes(!.BM, OldLastByteIndex + 1,
                    NewLastByteIndex, InitializerByte)
            else
                true
            )
        else
            true
        ),
        !:BM = clear_filler_bits(!.BM)
    ).

%---------------------------------------------------------------------------%

shrink_without_copying(!.BM, NewSize) = !:BM :-
    ( if 0 =< NewSize, NewSize =< !.BM ^ num_bits then
        !:BM = !.BM ^ num_bits := NewSize
    else
        throw_bit_bounds_error(!.BM,
            "bitmap.shrink_without_copying", NewSize)
    ).

:- func 'num_bits :='(bitmap, num_bits) = bitmap.
:- mode 'num_bits :='(bitmap_di, in) = bitmap_uo is det.

'num_bits :='(_, _) = _
    :- private_builtin.sorry("bitmap.'num_bits :='").

:- pragma foreign_proc("C",
    'num_bits :='(BM0::bitmap_di, NumBits::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = BM0;
    BM->num_bits = NumBits;
").
:- pragma foreign_proc("Java",
    'num_bits :='(BM0::bitmap_di, NumBits::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = BM0;
    BM.num_bits = NumBits;
").
:- pragma foreign_proc("C#",
    'num_bits :='(BM0::bitmap_di, NumBits::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = BM0;
    BM.num_bits = NumBits;
").

:- pragma foreign_proc("Erlang",
    'num_bits :='(BM0::bitmap_di, NumBits::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    {Bin, _} = BM0,
    BM = {Bin, NumBits}
").

%---------------------------------------------------------------------------%

:- type bitmap.slice
    --->    bitmap.slice_ctor(
                slice_bitmap_field          :: bitmap,
                slice_start_bit_index_field :: bit_index,
                slice_num_bits_field        :: num_bits
            ).

slice(BM, StartBit, NumBits) = Slice :-
    ( if
        NumBits >= 0,
        StartBit >= 0,
        in_range_rexcl(BM, StartBit + NumBits)
    then
        Slice = bitmap.slice_ctor(BM, StartBit, NumBits)
    else
        throw_bounds_error(BM, "bitmap.slice", StartBit, NumBits)
    ).

byte_slice(BM, StartByte, NumBytes) =
    slice(BM, StartByte * bits_per_byte, NumBytes * bits_per_byte).

Slice ^ slice_bitmap = Slice ^ slice_bitmap_field.
Slice ^ slice_start_bit_index  = Slice ^ slice_start_bit_index_field.
Slice ^ slice_num_bits = Slice ^ slice_num_bits_field.

Slice ^ slice_start_byte_index =
    quotient_bits_per_byte_with_rem_zero("bitmap.slice_start_byte_index",
        Slice ^ slice_start_bit_index).
Slice ^ slice_num_bytes =
    quotient_bits_per_byte_with_rem_zero("bitmap.slice_num_bytes",
        Slice ^ slice_num_bits).

:- func quotient_bits_per_byte_with_rem_zero(string, int) = int is det.

quotient_bits_per_byte_with_rem_zero(Pred, Int) = Quotient :-
    ( if unchecked_rem(Int, bits_per_byte) = 0 then
        Quotient = unchecked_quotient(Int, bits_per_byte)
    else
        throw_bitmap_error(Pred ++ ": not a byte slice.")
    ).

%---------------------------------------------------------------------------%

complement(BM) =
    clear_filler_bits(complement_2(byte_index_for_bit(num_bits(BM) - 1), BM)).

:- func complement_2(int, bitmap) = bitmap.
:- mode complement_2(in, bitmap_di) = bitmap_uo is det.

complement_2(ByteI, BM0) = BM :-
    ( if
        ByteI < 0
    then
        BM = BM0
    else
        X = BM0 ^ unsafe_byte(ByteI),
        BM1 = BM0 ^ unsafe_byte(ByteI) := \ X,
        BM = complement_2(ByteI - 1, BM1)
    ).

%---------------------------------------------------------------------------%

union(BMa, BMb) = BMc :-
    ( if num_bits(BMa) = num_bits(BMb) then
        BMc = zip((\/), BMa, BMb)
    else
        throw_bitmap_error("bitmap.union: bitmaps not the same size")
    ).

intersect(BMa, BMb) = BMc :-
    ( if num_bits(BMa) = num_bits(BMb) then
        BMc = zip((/\), BMa, BMb)
    else
        throw_bitmap_error("bitmap.intersect: bitmaps not the same size")
    ).

difference(BMa, BMb) = BMc :-
    ( if num_bits(BMa) = num_bits(BMb) then
        BMc = zip((func(X, Y) = (X /\ \Y)), BMa, BMb)
    else
        throw_bitmap_error("bitmap.difference: bitmaps not the same size")
    ).

xor(BMa, BMb) = BMc :-
    ( if num_bits(BMa) = num_bits(BMb) then
        BMc = zip((func(X, Y) = (X `xor` Y)), BMa, BMb)
    else
        throw_bitmap_error("bitmap.xor: bitmaps not the same size")
    ).

%---------------------------------------------------------------------------%

    % Applies a function to every corresponding element between +ve I
    % and 0 inclusive, destructively updating the second bitmap.
    %
:- func zip(func(byte, byte) = byte,
    bitmap, bitmap) = bitmap.
% :- mode zip(func(in, in) = out is det,
%    bitmap_ui, bitmap_di) = bitmap_uo is det.
:- mode zip(func(in, in) = out is det,
    in, bitmap_di) = bitmap_uo is det.

zip(Fn, BMa, BMb) =
    ( if num_bits(BMb) = 0 then
        BMb
    else
        zip2(byte_index_for_bit(num_bits(BMb) - 1), Fn, BMa, BMb)
    ).

:- func zip2(byte_index, func(byte, byte) = byte,
    bitmap, bitmap) = bitmap.
% :- mode zip2(in, func(in, in) = out is det,
%    bitmap_ui, bitmap_di) = bitmap_uo is det.
:- mode zip2(in, func(in, in) = out is det,
    in, bitmap_di) = bitmap_uo is det.

zip2(I, Fn, BMa, BMb) = BM :-
    ( if I >= 0 then
        Xa = BMa ^ unsafe_byte(I),
        Xb = BMb ^ unsafe_byte(I),
        BMc = BMb ^ unsafe_byte(I) := Fn(Xa, Xb),
        BM = zip2(I - 1, Fn, BMa, BMc)
    else
        BM = BMb
    ).

%---------------------------------------------------------------------------%

append_list(BMs) = !:BM :-
    BMSize = list.foldl((func(BM, Size) = Size + BM ^ num_bits), BMs, 0),
    !:BM = init(BMSize),
    list.foldl2(copy_bitmap_into_place, BMs, 0, _, !BM).

:- pred copy_bitmap_into_place(bitmap::in, int::in, int::out,
    bitmap::bitmap_di, bitmap::bitmap_uo) is det.

copy_bitmap_into_place(ThisBM, !Index, !BM) :-
    !:BM = unsafe_copy_bits(0, ThisBM, 0, !.BM, !.Index, ThisBM ^ num_bits),
    !:Index = !.Index + ThisBM ^ num_bits.

%---------------------------------------------------------------------------%

copy_bits(SrcBM, SrcStartBit, DestBM, DestStartBit, NumBits) =
    copy_bits(0, SrcBM, SrcStartBit, DestBM, DestStartBit, NumBits).

copy_bits_in_bitmap(SrcBM, SrcStartBit, DestStartBit, NumBits) =
    copy_bits(1, SrcBM, SrcStartBit, SrcBM, DestStartBit, NumBits).

:- func copy_bits(int, bitmap, bit_index,
    bitmap, bit_index, num_bits) = bitmap.
% :- mode copy_bits(in, bitmap_ui, in, bitmap_di, in, in) = bitmap_uo is det.
:- mode copy_bits(in, in, in, bitmap_di, in, in) = bitmap_uo is det.

copy_bits(SameBM, SrcBM, SrcStartBit, !.DestBM, DestStartBit, NumBits)
        = !:DestBM :-
    ( if
        NumBits >= 0,
        SrcStartBit >= 0,
        DestStartBit >= 0,
        in_range_rexcl(SrcBM, SrcStartBit + NumBits),
        in_range_rexcl(!.DestBM, DestStartBit + NumBits)
    then
        !:DestBM = unsafe_copy_bits(SameBM, SrcBM, SrcStartBit,
            !.DestBM, DestStartBit, NumBits)
    else
        ( if
            ( NumBits < 0
            ; SrcStartBit < 0
            ; not in_range(SrcBM, SrcStartBit + NumBits - 1)
            )
        then
            throw_bounds_error(SrcBM, "copy_bits (source)",
                SrcStartBit, NumBits)
        else if
            ( DestStartBit < 0
            ; not in_range(!.DestBM, DestStartBit + NumBits - 1)
            )
        then
            throw_bounds_error(!.DestBM, "copy_bits (destination)",
                DestStartBit, NumBits)
        else
            throw_bitmap_error("bitmap.copy_bits: failed to diagnose error")
        )
    ).

:- func unsafe_copy_bits(int, bitmap, bit_index,
    bitmap, bit_index, num_bits) = bitmap.
% :- mode unsafe_copy_bits(in, bitmap_ui, in,
%    bitmap_di, in, in) = bitmap_uo is det.
:- mode unsafe_copy_bits(in, in, in,
    bitmap_di, in, in) = bitmap_uo is det.

unsafe_copy_bits(SameBM, SrcBM, SrcStartBit, !.DestBM, DestStartBit,
        !.NumBits) = !:DestBM :-
    SrcStartIndex = bit_index_in_byte(SrcStartBit),
    DestStartIndex = bit_index_in_byte(DestStartBit),
    ( if !.NumBits < 2 * bits_per_byte then
        % The alternatives below don't handle ranges that don't span
        % a byte boundary.
        !:DestBM = !.DestBM ^ unsafe_bits(DestStartBit, !.NumBits) :=
            SrcBM ^ unsafe_bits(SrcStartBit, !.NumBits)
      else if SrcStartIndex = DestStartIndex then
        % Handle the common case where the bits to be moved have
        % the same offsets in each byte, so we can do a block byte copy.
        StartIndex = SrcStartIndex,
        SrcEndBit = SrcStartBit + !.NumBits - 1,
        EndIndex = bit_index_in_byte(SrcEndBit),
        ( if
            StartIndex = 0,
            EndIndex = bits_per_byte - 1
       then
            % It's an aligned block of bytes, move it.
            NumBytes = !.NumBits `unchecked_quotient` bits_per_byte,
            SrcStartByteIndex = SrcStartBit `unchecked_quotient` bits_per_byte,
            DestStartByteIndex =
                DestStartBit `unchecked_quotient` bits_per_byte,
            !:DestBM = unsafe_copy_bytes(SameBM, SrcBM, SrcStartByteIndex,
                !.DestBM, DestStartByteIndex, NumBytes)
       else
            % Grab the odd bits at each end of the block to move,
            % leaving a block of aligned bytes to move.
            ( if StartIndex = 0 then
                NumBitsAtStart = 0,
                StartBitsToSet = 0
            else
                NumBitsAtStart = bits_per_byte - StartIndex,
                SrcPartialStartByteIndex = byte_index_for_bit(SrcStartBit),
                StartBitsToSet =
                    extract_bits_from_byte(
                        SrcBM ^ unsafe_byte(SrcPartialStartByteIndex),
                        StartIndex, NumBitsAtStart),

                !:NumBits = !.NumBits - NumBitsAtStart
            ),

            ( if EndIndex = bits_per_byte - 1 then
                NumBitsAtEnd = 0,
                EndBitsToSet = 0
            else
                NumBitsAtEnd = EndIndex + 1,
                SrcPartialEndByteIndex = byte_index_for_bit(SrcEndBit),
                EndBitsToSet =
                    extract_bits_from_byte(
                        SrcBM ^ unsafe_byte(SrcPartialEndByteIndex),
                        0, NumBitsAtEnd),

                !:NumBits = !.NumBits - NumBitsAtEnd
            ),

            % Do the block copy.
            NumBytes = !.NumBits `unchecked_quotient` bits_per_byte,
            SrcStartByteIndex = (SrcStartBit + NumBitsAtStart)
                `unchecked_quotient` bits_per_byte,
            DestStartByteIndex = (DestStartBit + NumBitsAtStart)
                `unchecked_quotient` bits_per_byte,
            !:DestBM = unsafe_copy_bytes(SameBM, SrcBM, SrcStartByteIndex,
                !.DestBM, DestStartByteIndex, NumBytes),

            % Fill in the partial bytes at the start and end of the range.
            ( if NumBitsAtStart = 0 then
                true
            else
                DestPartialStartByteIndex = DestStartByteIndex - 1,
                !:DestBM =
                    !.DestBM ^ unsafe_byte(DestPartialStartByteIndex) :=
                        set_bits_in_byte(
                            !.DestBM ^ unsafe_byte(DestPartialStartByteIndex),
                            StartIndex, NumBitsAtStart, StartBitsToSet)
            ),

            ( if NumBitsAtEnd = 0 then
                true
            else
                DestPartialEndByteIndex = DestStartByteIndex + NumBytes,
                !:DestBM =
                    !.DestBM ^ unsafe_byte(DestPartialEndByteIndex) :=
                        set_bits_in_byte(
                            !.DestBM ^ unsafe_byte(DestPartialEndByteIndex),
                            0, NumBitsAtEnd, EndBitsToSet)
            )
        )
    else
        !:DestBM = unsafe_copy_unaligned_bits(SameBM, SrcBM, SrcStartBit,
            !.DestBM, DestStartBit, !.NumBits)
    ).

copy_bytes(SrcBM, SrcStartByteIndex, DestBM, DestStartByteIndex, NumBytes) =
    copy_bytes(0, SrcBM, SrcStartByteIndex,
        DestBM, DestStartByteIndex, NumBytes).

copy_bytes_in_bitmap(SrcBM, SrcStartByteIndex, DestStartByteIndex, NumBytes) =
    copy_bytes(1, SrcBM, SrcStartByteIndex,
        SrcBM, DestStartByteIndex, NumBytes).

    % The SameBM parameter is 1 if we are copying within the same bitmap
    % We use an `int' rather than a `bool' for easier interfacing with C.
    %
:- func copy_bytes(int, bitmap, bit_index,
    bitmap, bit_index, num_bits) = bitmap.
% :- mode copy_bytes(in, bitmap_ui, in,
%    bitmap_di, in, in) = bitmap_uo is det.
:- mode copy_bytes(in, in, in,
    bitmap_di, in, in) = bitmap_uo is det.

copy_bytes(SameBM, SrcBM, SrcStartByte, !.DestBM, DestStartByte, NumBytes)
        = !:DestBM :-
    ( if
        NumBytes = 0
    then
        true
    else if
        NumBytes > 0,
        SrcStartByte >= 0,
        byte_in_range(SrcBM, SrcStartByte + NumBytes - 1),
        DestStartByte >= 0,
        byte_in_range(!.DestBM, DestStartByte + NumBytes - 1)
    then
        !:DestBM = unsafe_copy_bytes(SameBM, SrcBM, SrcStartByte, !.DestBM,
            DestStartByte, NumBytes)
    else
        throw_bitmap_error("bitmap.copy_bytes: out of range")
    ).

:- func unsafe_copy_bytes(int, bitmap, byte_index,
    bitmap, byte_index, num_bytes) = bitmap.
% :- mode unsafe_copy_bytes(in, bitmap_ui, in,
%    bitmap_di, in, in) = bitmap_uo is det.
:- mode unsafe_copy_bytes(in, in, in,
    bitmap_di, in, in) = bitmap_uo is det.

:- pragma foreign_proc("C",
    unsafe_copy_bytes(SameBM::in, SrcBM::in, SrcFirstByteIndex::in,
        DestBM0::bitmap_di, DestFirstByteIndex::in,
        NumBytes::in) = (DestBM::bitmap_uo),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    DestBM = DestBM0;
    if (SameBM) {
        memmove(DestBM->elements + DestFirstByteIndex,
            SrcBM->elements + SrcFirstByteIndex, NumBytes);
    } else {
        MR_memcpy(DestBM->elements + DestFirstByteIndex,
            SrcBM->elements + SrcFirstByteIndex, NumBytes);
    }
").

:- pragma foreign_proc("C#",
    unsafe_copy_bytes(_SameBM::in, SrcBM::in, SrcFirstByteIndex::in,
        DestBM0::bitmap_di, DestFirstByteIndex::in,
        NumBytes::in) = (DestBM::bitmap_uo),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    DestBM = DestBM0;
    System.Array.Copy(SrcBM.elements, SrcFirstByteIndex,
        DestBM.elements, DestFirstByteIndex, NumBytes);
").

:- pragma foreign_proc("Java",
    unsafe_copy_bytes(_SameBM::in, SrcBM::in, SrcFirstByteIndex::in,
        DestBM0::bitmap_di, DestFirstByteIndex::in, NumBytes::in)
        = (DestBM::bitmap_uo),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    DestBM = DestBM0;
    System.arraycopy(SrcBM.elements, SrcFirstByteIndex,
        DestBM.elements, DestFirstByteIndex, NumBytes);
").

unsafe_copy_bytes(SameBM, SrcBM, SrcFirstByteIndex,
        !.DestBM, DestFirstByteIndex, NumBytes) = !:DestBM :-
    Direction = choose_copy_direction(SameBM, SrcFirstByteIndex,
        DestFirstByteIndex),
    (
        Direction = left_to_right,
        !:DestBM = unsafe_do_copy_bytes(SrcBM, SrcFirstByteIndex,
            !.DestBM, DestFirstByteIndex, NumBytes, 1)
    ;
        Direction = right_to_left,
        !:DestBM = unsafe_do_copy_bytes(SrcBM,
            SrcFirstByteIndex + NumBytes - 1,
            !.DestBM, DestFirstByteIndex + NumBytes - 1, NumBytes, -1)
    ).

:- func unsafe_do_copy_bytes(bitmap, byte_index,
    bitmap, byte_index, num_bytes, num_bytes) = bitmap.
% :- mode unsafe_do_copy_bytes(bitmap_ui, in,
%    bitmap_di, in, in, in) = bitmap_uo is det.
:- mode unsafe_do_copy_bytes(in, in,
    bitmap_di, in, in, in) = bitmap_uo is det.

unsafe_do_copy_bytes(SrcBM, SrcByteIndex, DestBM, DestByteIndex,
        NumBytes, AddForNext) =
    ( if NumBytes = 0 then
        DestBM
    else
        unsafe_do_copy_bytes(SrcBM, SrcByteIndex + AddForNext,
            DestBM ^ unsafe_byte(DestByteIndex) :=
                SrcBM ^ unsafe_byte(SrcByteIndex),
            DestByteIndex + AddForNext, NumBytes - 1, AddForNext)
    ).

    % General case. Reduce the number of writes by aligning to the next
    % byte boundary in the destination bitmap so each byte is written once.
    %
:- func unsafe_copy_unaligned_bits(int, bitmap, bit_index,
    bitmap, bit_index, num_bits) = bitmap.
% :- mode unsafe_copy_unaligned_bits(in, bitmap_ui, in,
%    bitmap_di, in, in) = bitmap_uo is det.
:- mode unsafe_copy_unaligned_bits(in, in, in,
    bitmap_di, in, in) = bitmap_uo is det.

unsafe_copy_unaligned_bits(SameBM, SrcBM, SrcStartBit,
        !.DestBM, DestStartBit, !.NumBits) = !:DestBM :-
    % Grab the odd bits at each end of the block in the destination,
    % leaving a block of aligned bytes to copy.
    DestStartIndex = bit_index_in_byte(DestStartBit),
    DestEndBit = DestStartBit + !.NumBits - 1,
    ( if DestStartIndex = 0 then
        NumBitsAtStart = 0,
        StartBits = 0
    else
        NumBitsAtStart = bits_per_byte - DestStartIndex,
        StartBits = SrcBM ^ unsafe_bits(SrcStartBit, NumBitsAtStart),
        !:NumBits = !.NumBits - NumBitsAtStart
    ),

    NewSrcStartBit = (SrcStartBit + NumBitsAtStart),
    NewDestStartBit = (DestStartBit + NumBitsAtStart),

    DestEndIndex = bit_index_in_byte(DestEndBit),
    ( if DestEndIndex = bits_per_byte - 1 then
        NumBitsAtEnd = 0,
        EndBits = 0
    else
        NumBitsAtEnd = DestEndIndex + 1,
        SrcEndBit = NewSrcStartBit + !.NumBits - 1,
        EndBits =
            SrcBM ^ unsafe_bits(SrcEndBit - NumBitsAtEnd + 1, NumBitsAtEnd),
        !:NumBits = !.NumBits - NumBitsAtEnd
    ),

    % Do the block copy.
    NumBytes = !.NumBits `unchecked_quotient` bits_per_byte,
    Direction = choose_copy_direction(SameBM, NewSrcStartBit, NewDestStartBit),
    SrcBitIndex = bit_index_in_byte(NewSrcStartBit),

    (
        Direction = left_to_right,
        SrcStartByteIndex = byte_index_for_bit(NewSrcStartBit),
        DestStartByteIndex = byte_index_for_bit(NewDestStartBit),
        !:DestBM = unsafe_copy_unaligned_bytes_ltor(SrcBM,
            SrcStartByteIndex + 1, SrcBitIndex,
            SrcBM ^ unsafe_byte(SrcStartByteIndex),
            !.DestBM, DestStartByteIndex, NumBytes)
    ;
        Direction = right_to_left,
        SrcStartByteIndex = byte_index_for_bit(NewSrcStartBit + !.NumBits - 1),
        DestStartByteIndex =
            byte_index_for_bit(NewDestStartBit + !.NumBits - 1),
        !:DestBM = unsafe_copy_unaligned_bytes_rtol(SrcBM,
            SrcStartByteIndex - 1, SrcBitIndex,
            SrcBM ^ unsafe_byte(SrcStartByteIndex),
            !.DestBM, DestStartByteIndex, NumBytes)
    ),

    % Fill in the partial bytes at the start and end of the range.
    ( if NumBitsAtStart = 0 then
        true
    else
        PartialDestStartByteIndex = byte_index_for_bit(DestStartBit),
        !:DestBM =
            !.DestBM ^ unsafe_byte(PartialDestStartByteIndex) :=
                set_bits_in_byte(
                    !.DestBM ^ unsafe_byte(PartialDestStartByteIndex),
                    DestStartIndex, NumBitsAtStart, StartBits)
    ),

    ( if NumBitsAtEnd = 0 then
        true
    else
        PartialDestEndByteIndex = byte_index_for_bit(DestEndBit),
        !:DestBM =
            !.DestBM ^ unsafe_byte(PartialDestEndByteIndex) :=
                set_bits_in_byte(
                    !.DestBM ^ unsafe_byte(PartialDestEndByteIndex),
                    0, NumBitsAtEnd, EndBits)
    ).

:- func unsafe_copy_unaligned_bytes_ltor(bitmap, byte_index, bit_index_in_byte,
    byte, bitmap, byte_index, num_bytes) = bitmap.
:- mode unsafe_copy_unaligned_bytes_ltor(in, in, in, in,
    bitmap_di, in, in) = bitmap_uo is det.
% :- mode unsafe_copy_unaligned_bytes_ltor(bitmap_ui, in, in, in,
%    bitmap_di, in, in) = bitmap_uo is det.

unsafe_copy_unaligned_bytes_ltor(SrcBM, SrcByteIndex, SrcBitIndex,
        PrevSrcByteBits, !.DestBM, DestByteIndex, NumBytes) = !:DestBM :-
    ( if NumBytes =< 0 then
        true
    else
        % Combine parts of two adjacent bytes in the source bitmap
        % into one byte of the destination.
        %
        % For example, for the call to `unsafe_copy_unaligned_bytes_ltor'
        % that would result from a call:
        % `unsafe_copy_bits(SrcBM, 1, DestBM, 0, 8)',
        % we construct the first byte in the destination by pasting
        % together the last seven bits of `SrcBM ^ byte(0)'
        % (from PrevSrcByteBits) with the first bit of `SrcBM ^ byte(1)'.
        % SrcBM: |0 1234567|0 1234567|01234567|...
        %          |   \/    |
        % DestBM:  |0123456 7|...
        %
        % PrevSrcByteBits will contain the initial contents of `Src ^ byte(0)'
        % (we can't look it up here because in the general case it may be
        % overwritten by previous recursive calls).

        SrcByteBits = SrcBM ^ unsafe_byte(SrcByteIndex),
        DestByteBits = (PrevSrcByteBits `unchecked_left_shift` SrcBitIndex)
            \/ (SrcByteBits `unchecked_right_shift`
                (bits_per_byte - SrcBitIndex)),
        !:DestBM = !.DestBM ^ unsafe_byte(DestByteIndex) := DestByteBits,

        unsafe_copy_unaligned_bytes_ltor(SrcBM, SrcByteIndex + 1, SrcBitIndex,
            SrcByteBits, !.DestBM, DestByteIndex + 1, NumBytes - 1) = !:DestBM
    ).

:- func unsafe_copy_unaligned_bytes_rtol(bitmap, byte_index, bit_index_in_byte,
    byte, bitmap, byte_index, num_bytes) = bitmap.
:- mode unsafe_copy_unaligned_bytes_rtol(in, in, in, in,
    bitmap_di, in, in) = bitmap_uo is det.
% :- mode unsafe_copy_unaligned_bytes_rtol(bitmap_ui, in, in, in,
%    bitmap_di, in, in) = bitmap_uo is det.

unsafe_copy_unaligned_bytes_rtol(SrcBM, SrcByteIndex, SrcBitIndex,
        PrevSrcByteBits, !.DestBM, DestByteIndex, NumBytes) = !:DestBM :-
    ( if NumBytes =< 0 then
        true
    else
        % Combine parts of two adjacent bytes in the source bitmap
        % into one byte of the destination.
        %
        % For example, for the first call to `unsafe_copy_unaligned_bytes_ltor'
        % resulting from a call `unsafe_copy_bits_in_bitmap(SrcBM, 7, 8, 8)'
        % we construct the second byte in the destination by pasting together
        % the last bit of `SrcBM ^ byte(0)' with the first seven bits of
        % `SrcBM ^ byte(1)' (from PrevSrcByteBits).
        % SrcBM:     |0123456 7|0123456 7|01234567|
        %                    |   \/    |
        % DestBM:   |01234567|0 1234567|
        %
        % PrevSrcByteBits will contain the initial contents of `Src ^ byte(1)'
        % (we can't look it up here because in the general case it may be
        % overwritten by previous recursive calls).

        SrcByteBits = SrcBM ^ unsafe_byte(SrcByteIndex),
        DestByteBits = (SrcByteBits `unchecked_left_shift` SrcBitIndex)
            \/ (PrevSrcByteBits `unchecked_right_shift`
                (bits_per_byte - SrcBitIndex)),
        !:DestBM = !.DestBM ^ unsafe_byte(DestByteIndex) := DestByteBits,

        !:DestBM = unsafe_copy_unaligned_bytes_rtol(SrcBM, SrcByteIndex - 1,
            SrcBitIndex, SrcByteBits, !.DestBM,
            DestByteIndex - 1, NumBytes - 1)
    ).

:- type copy_direction
    --->    left_to_right
    ;       right_to_left.

    % choose_copy_direction(SameBM, SrcStartBit, DestStartBit)
    %
    % Choose a direction that will avoid overwriting data
    % before it has been copied.
    % Where it doesn't matter, prefer left_to_right for better performance.
    %
:- func choose_copy_direction(int, bit_index, bit_index) = copy_direction.

choose_copy_direction(SameBM, SrcStartBit, DestStartBit) =
    ( if SameBM = 1, SrcStartBit < DestStartBit then
        right_to_left
    else
        left_to_right
    ).

%---------------------------------------------------------------------------%

to_string(BM) = Str :-
    % Note: this should be kept in sync with MR_bitmap_to_string in
    % runtime/mercury_bitmap.c.
    %
    NumBits = BM ^ num_bits,
    to_string_chars(byte_index_for_bit(NumBits - 1), BM, ['>'], BitChars),
    LenChars = to_char_list(int_to_string(NumBits)),
    Chars = ['<' | LenChars] ++ [':' | BitChars],
    Str = string.from_char_list(Chars).

:- pred to_string_chars(int, bitmap, list(char), list(char)).
% :- mode to_string_chars(in, bitmap_ui, in, out) is det.
:- mode to_string_chars(in, in, in, out) is det.

to_string_chars(Index, BM, !Chars) :-
    ( if Index < 0 then
        true
    else
        Byte = BM ^ unsafe_byte(Index),
        Mask = n_bit_mask(4),
        ( if
            char.int_to_hex_digit((Byte `unchecked_right_shift` 4) /\ Mask,
                HighChar),
            char.int_to_hex_digit(Byte /\ Mask, LowChar)
        then
            !:Chars = [HighChar, LowChar | !.Chars],
            to_string_chars(Index - 1, BM, !Chars)
        else
            throw(software_error("bitmap.to_string: internal error"))
        )
    ).

from_string(Str) = BM :-
    string.unsafe_index_next(Str, 0, Start, '<'),
    string.unsafe_index(Str, Start, Char),
    char.is_digit(Char),
    string.unsafe_prev_index(Str, length(Str), End, '>'),
    string.sub_string_search_start(Str, ":", Start, Colon),
    SizeStr = string.unsafe_between(Str, Start, Colon),
    string.to_int(SizeStr, Size),
    ( if Size >= 0 then
        BM0 = allocate_bitmap(Size),
        string.unsafe_index_next(Str, Colon, AfterColon, _),
        hex_chars_to_bitmap(Str, AfterColon, End, 0, BM0, BM)
    else
        fail
    ).

det_from_string(Str) =
    ( if BM = from_string(Str) then
        BM
    else
        unexpected($pred, "bitmap.from_string failed")
    ).

:- pred hex_chars_to_bitmap(string::in, int::in, int::in, byte_index::in,
    bitmap::bitmap_di, bitmap::bitmap_uo) is semidet.

hex_chars_to_bitmap(Str, Index, End, ByteIndex, !BM) :-
    ( if Index = End then
        true
    else if Index + 1 = End then
        % Each byte of the bitmap should have mapped to a pair of characters.
        fail
    else
        char.hex_digit_to_int(Str ^ unsafe_elem(Index), HighNibble),
        char.hex_digit_to_int(Str ^ unsafe_elem(Index + 1), LowNibble),
        Byte = (HighNibble `unchecked_left_shift` 4) \/ LowNibble,
        !:BM = !.BM ^ unsafe_byte(ByteIndex) := Byte,
        hex_chars_to_bitmap(Str, Index + 2, End, ByteIndex + 1, !BM)
    ).

%---------------------------------------------------------------------------%

to_byte_string(BM) = string.join_list(".", bitmap_to_byte_strings(BM)).

:- func bitmap_to_byte_strings(bitmap) = list(string).
% :- mode bitmap_to_byte_strings(bitmap_ui) = out is det.
:- mode bitmap_to_byte_strings(in) = out is det.

bitmap_to_byte_strings(BM) = Strs :-
    NumBits = BM ^ num_bits,
    Strs = bitmap_to_byte_strings(BM, NumBits, []).

:- func bitmap_to_byte_strings(bitmap, int, list(string)) = list(string).
% :- mode bitmap_to_byte_strings(bitmap_ui, in, in) = out is det.
:- mode bitmap_to_byte_strings(in, in, in) = out is det.

bitmap_to_byte_strings(BM, NumBits, !.Strs) = !:Strs :-
    ( if NumBits =< 0 then
        true
    else
        ThisByte0 = BM ^ unsafe_byte(byte_index_for_bit(NumBits - 1)),
        LastBitIndex = bit_index_in_byte(NumBits - 1),
        ( if LastBitIndex = bits_per_byte - 1 then
            BitsThisByte = bits_per_byte,
            ThisByte = ThisByte0
        else
            BitsThisByte = LastBitIndex + 1,
            ThisByte = ThisByte0 `unchecked_right_shift`
                (bits_per_byte - BitsThisByte)
        ),
        ThisByteStr = string.pad_left(string.int_to_base_string(ThisByte, 2),
            '0', BitsThisByte),
        !:Strs = [ThisByteStr | !.Strs],
        !:Strs = bitmap_to_byte_strings(BM, NumBits - BitsThisByte, !.Strs)
    ).

%---------------------------------------------------------------------------%

hash(BM) = HashVal :-
    % NOTE: bitmap.hash is also defined as MR_hash_bitmap in
    % runtime/mercury_bitmap.h. The two definitions must be kept identical.
    %
    NumBits = BM ^ num_bits,
    NumBytes0 = NumBits `unchecked_quotient` bits_per_byte,
    ( if NumBits `unchecked_rem` bits_per_byte = 0 then
        NumBytes = NumBytes0
    else
        NumBytes = NumBytes0 + 1
    ),
    hash_2(BM, 0, NumBytes, 0, HashVal0),
    HashVal = HashVal0 `xor` NumBits.

:- pred hash_2(bitmap::in, byte_index::in, int::in, int::in, int::out) is det.

hash_2(BM, Index, Length, !HashVal) :-
    ( if Index < Length then
        combine_hash(BM ^ unsafe_byte(Index), !HashVal),
        hash_2(BM, Index + 1, Length, !HashVal)
    else
        true
    ).

:- pred combine_hash(int::in, int::in, int::out) is det.

combine_hash(X, H0, H) :-
    H1 = H0 `xor` (H0 << 5),
    H = H1 `xor` X.

%---------------------------------------------------------------------------%
%
% A bitmap is represented in C as a size (in bits) and an array of bytes.
%
% NOTE: the `filler' bits in the last element of the array *must* be clear
% (i.e. zero). This makes the unification, comparison and the set operations
% simpler to implement.
%

:- pragma foreign_decl("C", "
#include ""mercury_types.h""
#include ""mercury_bitmap.h""
#include ""mercury_type_info.h""
").

:- pragma foreign_decl("Java", "
import jmercury.runtime.MercuryBitmap;
").

:- pragma foreign_type("C", bitmap, "MR_BitmapPtr",
        [can_pass_as_mercury_type])
    where equality is bitmap_equal, comparison is bitmap_compare.
:- pragma foreign_type("Java", bitmap, "jmercury.runtime.MercuryBitmap")
    where equality is bitmap_equal, comparison is bitmap_compare.
:- pragma foreign_type("C#", bitmap, "mercury.runtime.MercuryBitmap")
    where equality is bitmap_equal, comparison is bitmap_compare.
:- pragma foreign_type("Erlang", bitmap, "")
    where equality is bitmap_equal, comparison is bitmap_compare.

:- pred bitmap_equal(bitmap::in, bitmap::in) is semidet.
:- pragma terminates(bitmap_equal/2).

:- pragma foreign_proc("C",
    bitmap_equal(BM1::in, BM2::in),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    SUCCESS_INDICATOR = MR_bitmap_eq(BM1, BM2);
").

:- pragma foreign_proc("C#",
    bitmap_equal(BM1::in, BM2::in),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    SUCCESS_INDICATOR = BM1.Equals(BM2);
").

:- pragma foreign_proc("Java",
    bitmap_equal(BM1::in, BM2::in),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    SUCCESS_INDICATOR = BM1.equals(BM2);
").

:- pragma foreign_proc("Erlang",
    bitmap_equal(BM1::in, BM2::in),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (BM1 =:= BM2)
").

bitmap_equal(BM1, BM2) :-
    BM1 ^ num_bits = (BM2 ^ num_bits) @ NumBits,
    bytes_equal(0, byte_index_for_bit(NumBits), BM1, BM2).

:- pred bytes_equal(byte_index::in, byte_index::in,
    bitmap::in, bitmap::in) is semidet.

bytes_equal(Index, MaxIndex, BM1, BM2) :-
    ( if Index =< MaxIndex then
        BM1 ^ unsafe_byte(Index) = BM2 ^ unsafe_byte(Index),
        bytes_equal(Index + 1, MaxIndex, BM1, BM2)
    else
        true
    ).

:- pred bitmap_compare(comparison_result::uo, bitmap::in, bitmap::in) is det.
:- pragma terminates(bitmap_compare/3).

:- pragma foreign_proc("C",
    bitmap_compare(Result::uo, BM1::in, BM2::in),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    MR_Integer  res;
    res = MR_bitmap_cmp(BM1, BM2);
    Result = ((res < 0) ? MR_COMPARE_LESS
        : (res == 0) ? MR_COMPARE_EQUAL
        : MR_COMPARE_GREATER);
").

:- pragma foreign_proc("Java",
    bitmap_compare(Result::uo, BM1::in, BM2::in),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail,
        may_not_duplicate],
"
    if (BM1.num_bits < BM2.num_bits) {
        Result = builtin.COMPARE_LESS;
    } else if (BM1.num_bits > BM2.num_bits) {
        Result = builtin.COMPARE_GREATER;
    } else {
        Result = builtin.COMPARE_EQUAL;
        for (int i = 0; i < BM1.elements.length; i++) {
            // Mask off sign bits.
            int b1 = ((int) BM1.elements[i]) & 0xff;
            int b2 = ((int) BM2.elements[i]) & 0xff;
            if (b1 < b2) {
                Result = builtin.COMPARE_LESS;
                break;
            }
            if (b1 > b2) {
                Result = builtin.COMPARE_GREATER;
                break;
            }
        }
    }
").

:- pragma foreign_proc("C#",
    bitmap_compare(Result::uo, BM1::in, BM2::in),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail,
        may_not_duplicate],
"
    if (BM1.num_bits < BM2.num_bits) {
        Result = builtin.COMPARE_LESS;
    } else if (BM1.num_bits > BM2.num_bits) {
        Result = builtin.COMPARE_GREATER;
    } else {
        Result = builtin.COMPARE_EQUAL;
        for (int i = 0; i < BM1.elements.Length; i++) {
            // Mask off sign bits.
            int b1 = ((int) BM1.elements[i]) & 0xff;
            int b2 = ((int) BM2.elements[i]) & 0xff;
            if (b1 < b2) {
                Result = builtin.COMPARE_LESS;
                break;
            }
            if (b1 > b2) {
                Result = builtin.COMPARE_GREATER;
                break;
            }
        }
    }
").

:- pragma foreign_proc("Erlang",
    bitmap_compare(Result::uo, BM1::in, BM2::in),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    if
        BM1 =:= BM2 ->
            Result = {'='};
        BM1 < BM2 ->
            Result = {'<'};
        true ->
            Result = {'>'}
    end
").

bitmap_compare(Result, BM1, BM2) :-
    NumBits1 = BM1 ^ num_bits,
    NumBits2 = BM2 ^ num_bits,
    compare(Result0, NumBits1, NumBits2),
    (
        Result0 = (=),
        MaxIndex = byte_index_for_bit(NumBits2),
        bytes_compare(Result, 0, MaxIndex, BM1, BM2)
    ;
        ( Result0 = (<)
        ; Result0 = (>)
        ),
        Result = Result0
    ).

:- pred bytes_compare(comparison_result, byte_index, byte_index,
    bitmap, bitmap).
:- mode bytes_compare(uo, in, in, in, in) is det.

bytes_compare(Result, Index, MaxIndex, BM1, BM2) :-
    ( if Index =< MaxIndex then
        Byte1 = BM1 ^ unsafe_byte(Index),
        Byte2 = BM2 ^ unsafe_byte(Index),
        compare(Result0, Byte1, Byte2),
        (
            Result0 = (=),
            bytes_compare(Result, Index + 1, MaxIndex, BM1, BM2)
        ;
            ( Result0 = (<)
            ; Result0 = (>)
            ),
            Result = Result0
        )
    else
        Result = (=)
    ).

%---------------------------------------------------------------------------%

:- func allocate_bitmap(num_bits) = bitmap.
:- mode allocate_bitmap(in) = bitmap_uo is det.

:- pragma foreign_proc("C",
    allocate_bitmap(N::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    MR_allocate_bitmap_msg(BM, N, MR_ALLOC_ID);
").

:- pragma foreign_proc("Java",
    allocate_bitmap(N::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = new MercuryBitmap(N);
").

:- pragma foreign_proc("C#",
    allocate_bitmap(N::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    BM = new mercury.runtime.MercuryBitmap(N);
").

:- pragma foreign_proc("Erlang",
    allocate_bitmap(N::in) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Roundup = 8 * ((N + 7) div 8),
    Bin = <<0:Roundup>>,
    BM = {Bin, N}
").

:- func resize_bitmap(bitmap, num_bits) = bitmap.
:- mode resize_bitmap(bitmap_di, in) = bitmap_uo is det.

resize_bitmap(OldBM, N) =
    copy_bits(OldBM, 0, allocate_bitmap(N), 0, int.min(OldBM ^ num_bits, N)).

%---------------------------------------------------------------------------%

    % The byte index containing the given bit.
    %
:- func byte_index_for_bit(bit_index) = byte_index.

byte_index_for_bit(I) =
    ( if I < 0 then
        -1
    else
        unchecked_quotient(I, bits_per_byte)
    ).

%---------------------------------------------------------------------------%

:- type bit_index_in_byte == int.

    % Convert a bit index for a bitmap into a bit index into a
    % byte in the bitmap.
    %
:- func bit_index_in_byte(bit_index) = bit_index_in_byte.

bit_index_in_byte(I) = I `unchecked_rem` bits_per_byte.

%---------------------------------------------------------------------------%

    % Construct the bitmask for a given bit in a byte. Bits are numbered
    % from most significant to least significant (starting at zero).
    %
    % E.g. assuming bits_per_byte = 8 and I = 3 then
    % bitmask(I) = 2'00010000
    %
:- func bitmask(bit_index_in_byte) = byte.

bitmask(I) = 1 `unchecked_left_shift`
    (bits_per_byte - 1 - bit_index_in_byte(I)).

%---------------------------------------------------------------------------%

    % Construct a bitmask containing the N least significant bits set.
    %
    % E.g. assuming bits_per_byte = 8 and I = 4 then
    % n_bit_mask(I) = 2'00001111
    %
:- func n_bit_mask(num_bits) = byte.

n_bit_mask(N) = BitsMask :-
    BitMask  = 1 `unchecked_left_shift` (N - 1),
    BitsMask = BitMask \/ (BitMask - 1).

%---------------------------------------------------------------------------%

    % extract_bits_from_byte(Byte, FirstBit, NumBits)
    % Return an integer whose NumBits least significant bits contain
    % bits FirstBit, FirstBit + 1, ... FirstBit + NumBits - 1,
    % in order from most significant to least significant.
    %
:- func extract_bits_from_byte(byte, bit_index_in_byte, num_bits) = byte.

extract_bits_from_byte(Byte, FirstBit, NumBits) = Bits :-
    % Shift the last bit in the selected bit range
    % to the least significant position.
    LastBit = FirstBit + NumBits - 1,
    Shift = bits_per_byte - 1 - LastBit,
    Bits = (Byte `unchecked_right_shift` Shift) /\ n_bit_mask(NumBits).

    % set_bits_in_byte(Byte, FirstBit, NumBits, Bits)
    %
    % Replace bits FirstBit, FirstBit + 1, ... FirstBit + NumBits - 1,
    % with the NumBits least significant bits of Bits, replacing FirstBit
    % with the most significant of those bits.
    %
:- func set_bits_in_byte(byte, bit_index_in_byte, num_bits, byte) = byte.

set_bits_in_byte(Byte0, FirstBit, NumBits, Bits) = Byte :-
    LastBit = FirstBit + NumBits - 1,
    Shift = bits_per_byte - 1 - LastBit,
    Mask = n_bit_mask(NumBits),
    BitsToSet = Mask /\ Bits,
    Byte = (Byte0 /\ \ (Mask `unchecked_left_shift` Shift))
        \/ (BitsToSet `unchecked_left_shift` Shift).

%---------------------------------------------------------------------------%

    % throw_bit_bounds_error(BM, PredName, BitIndex):
    %
:- pred throw_bit_bounds_error(bitmap::in, string::in, bit_index::in)
    is erroneous.

throw_bit_bounds_error(BM, Pred, BitIndex) :-
    string.format(
        "%s: bit index %d is out of bounds [0, %d).",
        [s(Pred), i(BitIndex), i(BM ^ num_bits)], Msg),
    throw_bitmap_error(Msg).

    % throw_byte_bounds_error(BM, PredName, ByteIndex):
    %
:- pred throw_byte_bounds_error(bitmap::in, string::in, byte_index::in)
    is erroneous.

throw_byte_bounds_error(BM, Pred, ByteIndex) :-
    string.format(
        "%s: byte index %d is out of bounds [0, %d).",
        [s(Pred), i(ByteIndex), i(BM ^ num_bits / bits_per_byte)], Msg),
    throw_bitmap_error(Msg).

throw_bounds_error(BM, Pred, Index, NumBits) :-
    ( if NumBits < 0 then
        string.format("%s: negative number of bits: %d.",
            [s(Pred), i(NumBits)], Msg)
    else
        string.format(
            "%s: %d bits starting at bit %d is out of bounds [0, %d).",
            [s(Pred), i(NumBits), i(Index), i(BM ^ num_bits)], Msg)
    ),
    throw_bitmap_error(Msg).

:- pred throw_bitmap_error(string::in) is erroneous.

throw_bitmap_error(Msg) :-
    throw(bitmap_error(Msg)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
