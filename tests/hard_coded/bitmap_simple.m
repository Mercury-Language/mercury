%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Very inefficient, but simple sbitmap implementation,
% used for testing bitmap.m.

:- module bitmap_simple.

:- interface.

:- import_module bool.
:- import_module version_array.

:- type sbitmap == version_array(bool).

:- inst sbitmap == ground.
:- inst uniq_sbitmap == sbitmap. % XXX should be unique
:- mode sbitmap_di == di(uniq_sbitmap). % XXX should be di
:- mode sbitmap_uo == out(uniq_sbitmap).
:- mode sbitmap_ui == in(uniq_sbitmap).

    % The exception thrown for any error.
:- type sbitmap_error
    ---> sbitmap_error(string).

%---------------------------------------------------------------------------%

    % new(N, B) creates a sbitmap of size N (indexed 0 .. N-1)
    % setting each bit if B = yes and clearing each bit if B = no.
    % An exception is thrown if N is negative.
    %
:- func new(int, bool) = sbitmap.
:- mode new(in, in) = sbitmap_uo is det.

    % Create a new copy of a sbitmap.
    %
:- func copy(sbitmap) = sbitmap.
:- mode copy(sbitmap_ui) = sbitmap_uo is det.
%:- mode copy(in) = sbitmap_uo is det.

    % resize(BM, N, B) resizes sbitmap BM to have N bits; if N is
    % smaller than the current number of bits in BM then the excess
    % are discarded.  If N is larger than the current number of bits
    % in BM then the new bits are set if B = yes and cleared if
    % B = no.
    %
:- func resize(sbitmap, int, bool) = sbitmap.
:- mode resize(sbitmap_di, in, in) = sbitmap_uo is det.

    % Is the given bit number in range.
    %
:- pred in_range(sbitmap, int).
:- mode in_range(sbitmap_ui, in) is semidet.
%:- mode in_range(in, in) is semidet.

    % Is the given byte number in range.
    %
:- pred byte_in_range(sbitmap, int).
:- mode byte_in_range(sbitmap_ui, in) is semidet.
%:- mode byte_in_range(in, in) is semidet.

    % Returns the number of bits in a sbitmap.
    %
:- func num_bits(sbitmap) = int.
:- mode num_bits(sbitmap_ui) = out is det.
%:- mode num_bits(in) = out is det.

    % Returns the number of bytes in a sbitmap, failing if the sbitmap
    % has a partial final byte.
    %
:- func num_bytes(sbitmap) = int.
:- mode num_bytes(sbitmap_ui) = out is semidet.
%:- mode num_bytes(in) = out is semidet.

    % As above, but throw an exception if the sbitmap has a partial final byte.
:- func det_num_bytes(sbitmap) = int.
:- mode det_num_bytes(sbitmap_ui) = out is det.
%:- mode det_num_bytes(in) = out is det.

%---------------------------------------------------------------------------%

    % Get or set the given bit.
    % The unsafe versions do not check whether the bit is in range.
    %
:- func bit(int, sbitmap) = bool.
:- mode bit(in, sbitmap_ui) = out is det.
%:- mode bit(in, in) = out is det.

:- func unsafe_bit(int, sbitmap) = bool.
:- mode unsafe_bit(in, sbitmap_ui) = out is det.
%:- mode unsafe_bit(in, in) = out is det.

:- func 'bit :='(int, sbitmap, bool) = sbitmap.
:- mode 'bit :='(in, sbitmap_di, in) = sbitmap_uo is det.

:- func 'unsafe_bit :='(int, sbitmap, bool) = sbitmap.
:- mode 'unsafe_bit :='(in, sbitmap_di, in) = sbitmap_uo is det.

%---------------------------------------------------------------------------%

    % Bitmap ^ bits(OffSet, NumBits) = Word.
    % The low order bits of Word contain the NumBits bits of BitMap
    % starting at OffSet.
    % NumBits must be less than int.bits_per_int.
    %
:- func bits(int, int, sbitmap) = int.
:- mode bits(in, in, sbitmap_ui) = out is det.
%:- mode bits(in, in, in) = out is det.

:- func unsafe_bits(int, int, sbitmap) = int.
:- mode unsafe_bits(in, in, sbitmap_ui) = out is det.
%:- mode unsafe_bits(in, in, in) = out is det.

:- func 'bits :='(int, int, sbitmap, int) = sbitmap.
:- mode 'bits :='(in, in, sbitmap_di, in) = sbitmap_uo is det.

:- func 'unsafe_bits :='(int, int, sbitmap, int) = sbitmap.
:- mode 'unsafe_bits :='(in, in, sbitmap_di, in) = sbitmap_uo is det.

%---------------------------------------------------------------------------%

    % BM ^ byte(ByteNumber)
    % Get or set the given numbered byte (multiply ByteNumber by
    % bits_per_int to get the bit index of the start of the byte).
    %
    % The bits are stored in or taken from the least significant bits
    % of the integer.
    % The unsafe versions do not check whether the byte is in range.
    %
:- func byte(int, sbitmap) = int.
:- mode byte(in, sbitmap_ui) = out is det.
%:- mode byte(in, in) = out is det.

:- func unsafe_byte(int, sbitmap) = int.
:- mode unsafe_byte(in, sbitmap_ui) = out is det.
%:- mode unsafe_byte(in, in) = out is det.

:- func 'byte :='(int, sbitmap, int) = sbitmap.
:- mode 'byte :='(in, sbitmap_di, in) = sbitmap_uo is det.

:- func 'unsafe_byte :='(int, sbitmap, int) = sbitmap.
:- mode 'unsafe_byte :='(in, sbitmap_di, in) = sbitmap_uo is det.

%---------------------------------------------------------------------------%

    % Flip the given bit.
    %
:- func flip(sbitmap, int) = sbitmap.
:- mode flip(sbitmap_di, in) = sbitmap_uo is det.

:- func unsafe_flip(sbitmap, int) = sbitmap.
:- mode unsafe_flip(sbitmap_di, in) = sbitmap_uo is det.

%---------------------------------------------------------------------------%

    % Set operations; for binary operations the second argument is altered
    % in all cases.  The input sbitmaps must have the same size.
    %

:- func complement(sbitmap) = sbitmap.
:- mode complement(sbitmap_di) = sbitmap_uo is det.

:- func union(sbitmap, sbitmap) = sbitmap.
:- mode union(sbitmap_ui, sbitmap_di) = sbitmap_uo is det.
%:- mode union(in, sbitmap_di) = sbitmap_uo is det.

:- func intersect(sbitmap, sbitmap) = sbitmap.
:- mode intersect(sbitmap_ui, sbitmap_di) = sbitmap_uo is det.
%:- mode intersect(in, sbitmap_di) = sbitmap_uo is det.

:- func difference(sbitmap, sbitmap) = sbitmap.
:- mode difference(sbitmap_ui, sbitmap_di) = sbitmap_uo is det.
%:- mode difference(in, sbitmap_di) = sbitmap_uo is det.

:- func xor(sbitmap, sbitmap) = sbitmap.
:- mode xor(sbitmap_ui, sbitmap_di) = sbitmap_uo is det.
%:- mode xor(in, sbitmap_di) = sbitmap_uo is det.

%---------------------------------------------------------------------------%

    % copy_bits(SrcBM, SrcStartBit, DestBM, DestStartBit, NumBits)
    %
    % Overwrite NumBits bits in DestBM starting at DestStartBit with
    % the NumBits bits starting at SrcStartBit in SrcBM.
    %
:- func copy_bits(sbitmap, int, sbitmap, int, int) = sbitmap.
:- mode copy_bits(sbitmap_ui, in, sbitmap_di, in, in) = sbitmap_uo is det.
%:- mode copy_bits(in, in, sbitmap_di, in, in) = sbitmap_uo is det.

    % copy_bits_in_sbitmap(BM, SrcStartBit, DestStartBit, NumBits)
    %
    % Overwrite NumBits bits starting at DestStartBit with the NumBits
    % bits starting at SrcStartBit in the same sbitmap.
    %
:- func copy_bits_in_bitmap(sbitmap, int, int, int) = sbitmap.
:- mode copy_bits_in_bitmap(sbitmap_di, in, in, in) = sbitmap_uo is det.

    % copy_bytes(SrcBM, SrcStartByte, DestBM, DestStartByte, NumBytes)
    %
    % Overwrite NumBytes bytes in DestBM starting at DestStartByte with
    % the NumBytes bytes starting at SrcStartByte in SrcBM.
    %
:- func copy_bytes(sbitmap, int, sbitmap, int, int) = sbitmap.
:- mode copy_bytes(sbitmap_ui, in, sbitmap_di, in, in) = sbitmap_uo is det.
%:- mode copy_bytes(in, in, sbitmap_di, in, in) = sbitmap_uo is det.

    % copy_bytes_in_sbitmap(BM, SrcStartByte, DestStartByte, NumBytes)
    %
    % Overwrite NumBytes bytes starting at DestStartByte with the NumBytes
    % bytes starting at SrcStartByte in the same sbitmap.
    %
:- func copy_bytes_in_bitmap(sbitmap, int, int, int) = sbitmap.
:- mode copy_bytes_in_bitmap(sbitmap_di, in, in, in) = sbitmap_uo is det.

%---------------------------------------------------------------------------%

    % Variations that might be slightly more efficient by not
    % converting bits to bool.
    %

:- func set(sbitmap, int) = sbitmap.
:- mode set(sbitmap_di, in) = sbitmap_uo is det.

:- func clear(sbitmap, int) = sbitmap.
:- mode clear(sbitmap_di, in) = sbitmap_uo is det.

    % is_set(BM, I) and is_clear(BM, I) succeed iff bit I in BM
    % is set or clear respectively.
    %
:- pred is_set(sbitmap, int).
:- mode is_set(sbitmap_ui, in) is semidet.
%:- mode is_set(in, in) is semidet.

:- pred is_clear(sbitmap, int).
:- mode is_clear(sbitmap_ui, in) is semidet.
%:- mode is_clear(in, in) is semidet.

    % Unsafe versions of the above: if the index is out of range
    % then behaviour is undefined and bad things are likely to happen.
    %
:- func unsafe_set(sbitmap, int) = sbitmap.
:- mode unsafe_set(sbitmap_di, in) = sbitmap_uo is det.

:- func unsafe_clear(sbitmap, int) = sbitmap.
:- mode unsafe_clear(sbitmap_di, in) = sbitmap_uo is det.

:- pred unsafe_set(int, sbitmap, sbitmap).
:- mode unsafe_set(in, sbitmap_di, sbitmap_uo) is det.

:- pred unsafe_clear(int, sbitmap, sbitmap).
:- mode unsafe_clear(in, sbitmap_di, sbitmap_uo) is det.

:- pred unsafe_flip(int, sbitmap, sbitmap).
:- mode unsafe_flip(in, sbitmap_di, sbitmap_uo) is det.

:- pred unsafe_is_set(sbitmap, int).
:- mode unsafe_is_set(sbitmap_ui, in) is semidet.
%:- mode unsafe_is_set(in, in) is semidet.

:- pred unsafe_is_clear(sbitmap, int).
:- mode unsafe_is_clear(sbitmap_ui, in) is semidet.
%:- mode unsafe_is_clear(in, in) is semidet.

    % Predicate versions, for use with state variables.
    %

:- pred set(int, sbitmap, sbitmap).
:- mode set(in, sbitmap_di, sbitmap_uo) is det.

:- pred clear(int, sbitmap, sbitmap).
:- mode clear(in, sbitmap_di, sbitmap_uo) is det.

:- pred flip(int, sbitmap, sbitmap).
:- mode flip(in, sbitmap_di, sbitmap_uo) is det.

:- func to_byte_string(sbitmap) = string.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module enum.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

new(N, B) = BM :-
    ( if N < 0 then
        throw_sbitmap_error("sbitmap.new: negative size") = _ : int
    else
        BM  = version_array.init(N, B)
    ).

%---------------------------------------------------------------------------%

resize(BM, NewSize, InitializerBit) =
    ( if NewSize =< 0 then
        bitmap_simple.new(NewSize, InitializerBit)
    else
        version_array.resize(BM, NewSize, InitializerBit)
    ).

%---------------------------------------------------------------------------%

% Use the underlying version_array's bounds checking.
%
% in_range(BM, I) :- 0 =< I, I < num_bits(BM).
:- pragma no_determinism_warning(pred(in_range/2)).
in_range(_, _) :-
    true.

% byte_in_range(BM, I) :-
%    in_range(BM, I * bits_per_int + bits_per_int - 1).
:- pragma no_determinism_warning(byte_in_range/2).
byte_in_range(_, _).

%---------------------------------------------------------------------------%

bit(I, BM) =
    ( if in_range(BM, I)
    then BM ^ unsafe_bit(I)
    else throw_sbitmap_error("sbitmap.bit: out of range")
    ).

unsafe_bit(I, BM) =
    ( if unsafe_is_set(BM, I) then yes else no ).

'bit :='(I, BM, B) =
    ( if in_range(BM, I) then
        BM ^ unsafe_bit(I) := B
    else
        throw_sbitmap_error("sbitmap.'bit :=': out of range")
    ).

'unsafe_bit :='(I, BM, yes) = unsafe_set(BM, I).
'unsafe_bit :='(I, BM, no) = unsafe_clear(BM, I).

%---------------------------------------------------------------------------%

bits(FirstBit, NumBits, BM) =
    ( if FirstBit >= 0, in_range(BM, FirstBit + NumBits - 1)
    then BM ^ unsafe_bits(FirstBit, NumBits)
    else throw_sbitmap_error("sbitmap.bits: out of range")
    ).

unsafe_bits(FirstBit, NumBits, BM) = Bits :-
    extract_bits(FirstBit, NumBits, BM, 0, Bits).

    % Extract the given number of bits starting at the most significant.
    %
:- pred extract_bits(int, int, sbitmap, int, int).
:- mode extract_bits(in, in, sbitmap_ui, in, out) is det.
%:- mode extract_bits(in, in, in, in, out) is det.

extract_bits(FirstBit, NumBits, BM, !Bits) :-
    ( if NumBits =< 0 then
        true
    else
        !:Bits = (!.Bits `unchecked_left_shift` 1)
            \/ to_int(BM ^ elem(FirstBit)),
        extract_bits(FirstBit + 1, NumBits - 1, BM, !Bits)
    ).

'bits :='(FirstBit, NumBits, BM, Bits) =
    ( if FirstBit >= 0, in_range(BM, FirstBit + NumBits - 1) then
        BM ^ unsafe_bits(FirstBit, NumBits) := Bits
    else
        throw_sbitmap_error("sbitmap.bits: out of range")
    ).

'unsafe_bits :='(FirstBit, NumBits, BM0, Bits) = BM :-
    LastBit = FirstBit + NumBits - 1,
    set_bits(LastBit, NumBits, Bits, BM0, BM).

    % Set the given number of bits starting at the least significant
:- pred set_bits(int, int, int, sbitmap, sbitmap).
:- mode set_bits(in, in, in, sbitmap_di, sbitmap_uo) is det.

set_bits(LastBit, NumBits, Bits, !BM) :-
    ( if NumBits =< 0 then
        true
    else
        !:BM = !.BM ^ elem(LastBit) := det_from_int(Bits /\ 1),
        set_bits(LastBit - 1, NumBits - 1,
            Bits `unchecked_right_shift` 1, !BM)
    ).

%---------------------------------------------------------------------------%

set(BM, I) =
    ( if in_range(BM, I) then
        unsafe_set(BM, I)
    else
        throw_sbitmap_error("sbitmap.set: out of range")
    ).

clear(BM, I) =
    ( if in_range(BM, I) then
        unsafe_clear(BM, I)
    else
        throw_sbitmap_error("sbitmap.clear: out of range")
    ).

flip(BM, I) =
    ( if in_range(BM, I) then
        unsafe_flip(BM, I)
    else
        throw_sbitmap_error("sbitmap.flip: out of range")
    ).

set(I, BM, set(BM, I)).

clear(I, BM, clear(BM, I)).

flip(I, BM, flip(BM, I)).

%---------------------------------------------------------------------------%

unsafe_set(BM, I) =
    BM ^ elem(I) := yes.

unsafe_clear(BM, I) =
    BM ^ elem(I) := no.

unsafe_flip(BM, I) =
    BM ^ elem(I) := bool.not(BM ^ elem(I)).

unsafe_set(I, BM, unsafe_set(BM, I)).

unsafe_clear(I, BM, unsafe_clear(BM, I)).

unsafe_flip(I, BM, unsafe_flip(BM, I)).

%---------------------------------------------------------------------------%

is_set(BM, I) :-
    ( if in_range(BM, I) then
        unsafe_is_set(BM, I)
    else
        throw_sbitmap_error("sbitmap.is_set: out of range") = _ : int
    ).

is_clear(BM, I) :-
    ( if in_range(BM, I) then
        unsafe_is_clear(BM, I)
    else
        throw_sbitmap_error("sbitmap.is_clear: out of range") = _ : int
    ).

%---------------------------------------------------------------------------%

unsafe_is_set(BM, I) :-
    BM ^ elem(I) = yes.

unsafe_is_clear(BM, I) :-
    BM ^ elem(I) = no.

%---------------------------------------------------------------------------%

complement(BM) = from_list(map(not, to_list(BM))).

%---------------------------------------------------------------------------%

union(BMa, BMb) =
    ( if num_bits(BMa) = num_bits(BMb) then
        zip(or, BMa, BMb)
    else
        throw_sbitmap_error("sbitmap.union: sbitmaps not the same size")
    ).

%---------------------------------------------------------------------------%

intersect(BMa, BMb) =
    ( if num_bits(BMa) = num_bits(BMb) then
        zip(and, BMa, BMb)
    else
        throw_sbitmap_error("sbitmap.intersect: sbitmaps not the same size")
    ).

%---------------------------------------------------------------------------%

difference(BMa, BMb) =
    ( if num_bits(BMa) = num_bits(BMb) then
        zip((func(X, Y) = (X `and` not(Y))), BMa, BMb)
    else
        throw_sbitmap_error("sbitmap.difference: sbitmaps not the same size")
    ).

%---------------------------------------------------------------------------%

xor(BMa, BMb) =
    ( if num_bits(BMa) = num_bits(BMb) then
        zip((func(X, Y) = (X `xor` Y)), BMa, BMb)
    else
        throw_sbitmap_error("sbitmap.xor: sbitmaps not the same size")
    ).

%---------------------------------------------------------------------------%

    % Applies a function to every corresponding element between +ve I
    % and 1 inclusive, destructively updating the second sbitmap.
    %
:- func zip(func(bool, bool) = bool, sbitmap, sbitmap) = sbitmap.
:- mode zip(func(in, in) = out is det, sbitmap_ui, sbitmap_di) =
    sbitmap_uo is det.
%:- mode zip(func(in, in) = out is det, in, sbitmap_di) = sbitmap_uo is det.

zip(Fn, BMa, BMb) =
    ( if num_bits(BMb) = 0 then
        BMb
    else
        zip2(num_bits(BMb) - 1, Fn, BMa, BMb)
    ).

:- func zip2(int, func(bool, bool) = bool, sbitmap, sbitmap) = sbitmap.
:- mode zip2(in, func(in, in) = out is det, sbitmap_ui, sbitmap_di)
    = sbitmap_uo is det.
%:- mode zip2(in, func(in, in) = out is det, in, sbitmap_di) = sbitmap_uo
%    is det.

zip2(I, Fn, BMa, BMb) =
    ( if I >= 0 then
        zip2(I - 1, Fn, BMa,
            BMb ^ elem(I) := Fn(BMa ^ elem(I), BMb ^ elem(I)))
     else
        BMb
    ).

%---------------------------------------------------------------------------%

copy_bits(SrcBM, SrcStartBit, DestBM, DestStartBit, NumBits) =
    copy_bits(no, SrcBM, SrcStartBit, DestBM, DestStartBit, NumBits).

copy_bits_in_bitmap(BM, SrcStartBit, DestStartBit, NumBits) =
    copy_bits(yes, BM, SrcStartBit, BM, DestStartBit, NumBits).

:- func copy_bits(bool, sbitmap, int, sbitmap, int, int) = sbitmap.
:- mode copy_bits(in, sbitmap_ui, in, sbitmap_di, in, in) = sbitmap_uo is det.
%:- mode copy_bits(in, in, in, sbitmap_di, in, in) = sbitmap_uo is det.

copy_bits(SameBM, SrcBM, SrcStartBit, DestBM, DestStartBit, NumBits) =
    ( if
        in_range(SrcBM, SrcStartBit),
        in_range(SrcBM, SrcStartBit + NumBits - 1),
        in_range(DestBM, DestStartBit),
        in_range(DestBM, DestStartBit + NumBits - 1)
    then
        unsafe_copy_bits(SameBM, SrcBM, SrcStartBit,
            DestBM, DestStartBit, NumBits)
    else
        throw_sbitmap_error("sbitmap.copy_bits: out of range")
    ).

:- func unsafe_copy_bits(bool, sbitmap, int, sbitmap, int, int) = sbitmap.
:- mode unsafe_copy_bits(in, sbitmap_ui, in,
    sbitmap_di, in, in) = sbitmap_uo is det.
%:- mode unsafe_copy_bits(in, in, in, sbitmap_di, in, in) = sbitmap_uo is det.

unsafe_copy_bits(SameBM, SrcBM, SrcStartBit, !.DestBM, DestStartBit,
        NumBits) = !:DestBM :-
    CopyDirection = choose_copy_direction(SameBM, SrcStartBit, DestStartBit),
    (
        CopyDirection = left_to_right,
        AddForNextBit = 1,
        SrcFirstBit = SrcStartBit,
        DestFirstBit = DestStartBit
    ;
        CopyDirection = right_to_left,
        AddForNextBit = -1,
        SrcFirstBit = SrcStartBit + NumBits - 1,
        DestFirstBit = DestStartBit + NumBits - 1
    ),
    !:DestBM = unsafe_do_copy_bits(SrcBM, SrcFirstBit,
        !.DestBM, DestFirstBit, AddForNextBit, NumBits).

:- func unsafe_do_copy_bits(sbitmap, int, sbitmap, int, int, int) = sbitmap.
:- mode unsafe_do_copy_bits(sbitmap_ui, in,
    sbitmap_di, in, in, in) = sbitmap_uo is det.
%:- mode unsafe_do_copy_bits(in, in,
%    sbitmap_di, in, in, in) = sbitmap_uo is det.

unsafe_do_copy_bits(SrcBM, SrcFirstBit, DestBM, DestFirstBit,
        AddForNextBit, NumBits) =
    ( if NumBits =< 0 then
        DestBM
    else
        unsafe_do_copy_bits(SrcBM, SrcFirstBit + AddForNextBit,
            DestBM ^ elem(DestFirstBit) := SrcBM ^ elem(SrcFirstBit),
            DestFirstBit + AddForNextBit,
            AddForNextBit, NumBits - 1)
    ).

copy_bytes(SrcBM, SrcStartByte, DestBM, DestStartByte, NumBytes) =
    copy_bits(SrcBM, SrcStartByte * bits_per_byte,
        DestBM, DestStartByte * bits_per_byte, NumBytes * bits_per_byte).

copy_bytes_in_bitmap(BM, SrcStartByte, DestStartByte, NumBytes) =
    copy_bits_in_bitmap(BM, SrcStartByte * bits_per_byte,
        DestStartByte * bits_per_byte, NumBytes * bits_per_byte).

:- type copy_direction
    --->    left_to_right
    ;       right_to_left.

    % choose_copy_direction(SameBM, SrcStartBit, DestStartBit)
    %
    % Choose a direction that will avoid overwriting data
    % before it has been copied.
:- func choose_copy_direction(bool, int, int) = copy_direction.

choose_copy_direction(yes, SrcStartBit, DestStartBit) =
    ( if SrcStartBit < DestStartBit then right_to_left else left_to_right ).
choose_copy_direction(no, _, _) = left_to_right.
    % Doesn't matter for correctness, but performance is likely
    % to be better left_to_right.

%---------------------------------------------------------------------------%

num_bytes(BM) = Bytes :-
    NumBits = BM ^ num_bits,
    NumBits rem bits_per_byte = 0,
    Bytes = NumBits `unchecked_quotient` bits_per_byte.

det_num_bytes(BM) = Bytes :-
    ( if Bytes0 = num_bytes(BM) then
        Bytes = Bytes0
    else
        throw_sbitmap_error("det_num_bytes: sbitmap has a partial final byte")
    ).

%---------------------------------------------------------------------------%

num_bits(BM) = version_array.size(BM).

%---------------------------------------------------------------------------%

byte(N, BM) =
    ( if N >= 0, in_range(BM, N * bits_per_byte + bits_per_byte - 1) then
        BM ^ unsafe_byte(N)
    else
        throw_sbitmap_error("sbitmap.byte: out of range")
    ).

unsafe_byte(N, BM) = BM ^ bits(N * bits_per_byte, bits_per_byte).

%---------------------------------------------------------------------------%

'byte :='(N, BM, Byte) =
    ( if N >= 0, in_range(BM, N * bits_per_byte + bits_per_byte - 1) then
        BM ^ unsafe_byte(N) := Byte
    else
        throw_sbitmap_error("sbitmap.'byte :=': out of range")
    ).

'unsafe_byte :='(N, BM, Byte) =
    BM ^ bits(N * bits_per_byte, bits_per_byte) := Byte.

%---------------------------------------------------------------------------%

copy(BM) = version_array.copy(BM).

%---------------------------------------------------------------------------%

    % Return the number of bits in a byte (always 8).
    %
:- func bits_per_byte = int.

bits_per_byte = 8.

%---------------------------------------------------------------------------%

:- func throw_sbitmap_error(string) = _ is erroneous.

throw_sbitmap_error(Msg) = _ :-
    throw_sbitmap_error(Msg).

:- pred throw_sbitmap_error(string::in) is erroneous.

throw_sbitmap_error(Msg) :- throw(sbitmap_error(Msg)).

%---------------------------------------------------------------------------%

to_byte_string(BM) =
    string.join_list(".", sbitmap_to_byte_strings(BM)).

:- func sbitmap_to_byte_strings(sbitmap) = list(string).
:- mode sbitmap_to_byte_strings(in) = out is det.

sbitmap_to_byte_strings(BM) = Strs :-
    NumBits = BM ^ num_bits,
    Strs = sbitmap_to_byte_strings(BM, NumBits, []).

:- func sbitmap_to_byte_strings(sbitmap, int, list(string)) = list(string).
:- mode sbitmap_to_byte_strings(in, in, in) = out is det.

sbitmap_to_byte_strings(BM, NumBits, !.Strs) = !:Strs :-
    ( if NumBits =< 0 then
        true
    else
        % Check for the incomplete last byte.
        NumBitsThisByte0 = NumBits rem bits_per_byte,
        ( if NumBitsThisByte0 = 0 then
            NumBitsThisByte = bits_per_byte
        else
            NumBitsThisByte = NumBitsThisByte0
        ),
        ThisByte =
            (BM ^ unsafe_bits(NumBits - NumBitsThisByte, NumBitsThisByte)),
        ThisByteStr = string.pad_left(int_to_base_string(ThisByte, 2),
            '0', NumBitsThisByte),
        !:Strs = [ThisByteStr | !.Strs],
        !:Strs = sbitmap_to_byte_strings(BM, NumBits - NumBitsThisByte, !.Strs)
    ).
