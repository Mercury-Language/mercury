%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test bitmaps by checking the output against a simpler implementation.

:- module bitmap_tester.

:- interface.

:- import_module bitmap.
:- import_module bitmap_simple.
:- import_module bool.

%---------------------------------------------------------------------------%

:- type mypair(T, U)
    --->    (fst::T) - (snd::U).
:- type tbitmap == mypair(bitmap, sbitmap).

:- inst tbitmap == bound(bitmap - sbitmap).
:- inst uniq_tbitmap == tbitmap. % XXX should be unique
:- mode tbitmap_di == in(uniq_tbitmap). % XXX should be di
:- mode tbitmap_uo == out(uniq_tbitmap).
:- mode tbitmap_ui == in(uniq_tbitmap).

:- type bitmap_result_error
    --->    some [OtherArgs] one_argument(string, sbitmap, OtherArgs, tbitmap)
    ;       some [OtherArgs] two_arguments(string, sbitmap, sbitmap,
                    OtherArgs, tbitmap)
    ;       some [OtherArgs, Result] query(string, sbitmap, OtherArgs,
                    mypair(Result, Result))
    ;       some [OtherArgs, Result] binary_query(string, sbitmap, sbitmap,
                    OtherArgs, mypair(Result, Result)).

:- type bitmap_verify_error
    --->    bitmap_verify_error(bitmap, bitmap_verify_error_type).

:- type bitmap_verify_error_type
    --->    hash
    ;       trailing_bits_not_empty
    ;       to_string(string, string).

%---------------------------------------------------------------------------%

    % new(N, B) creates a bitmap of size N (indexed 0 .. N-1)
    % setting each bit if B = yes and clearing each bit if B = no.
    % An exception is thrown if N is negative.
    %
:- func new(num_bits, bool) = tbitmap.
:- mode new(in, in) = tbitmap_uo is det.

    % Create a new copy of a bitmap.
    %
:- func copy(tbitmap) = tbitmap.
%:- mode copy(tbitmap_ui) = tbitmap_uo is det.
:- mode copy(in) = tbitmap_uo is det.

    % resize(BM, N, B) resizes bitmap BM to have N bits; if N is
    % smaller than the current number of bits in BM then the excess
    % are discarded.  If N is larger than the current number of bits
    % in BM then the new bits are set if B = yes and cleared if
    % B = no.
    %
:- func resize(tbitmap, num_bits, bool) = tbitmap.
:- mode resize(tbitmap_di, in, in) = tbitmap_uo is det.

%---------------------------------------------------------------------------%

    % Get or set the given bit.
    % The unsafe versions do not check whether the bit is in range.
    %
:- func bit(bit_index, tbitmap) = bool.
%:- mode bit(in, tbitmap_ui) = out is det.
:- mode bit(in, in) = out is det.

:- func 'bit :='(bit_index, tbitmap, bool) = tbitmap.
:- mode 'bit :='(in, tbitmap_di, in) = tbitmap_uo is det.

%---------------------------------------------------------------------------%

    % Bitmap ^ bits(OffSet, NumBits) = Word.
    % The low order bits of Word contain the NumBits bits of BM
    % starting at OffSet.
    % NumBits must be less than int.bits_per_int.
    %
:- func bits(bit_index, num_bits, tbitmap) = word.
%:- mode bits(in, in, tbitmap_ui) = out is det.
:- mode bits(in, in, in) = out is det.

:- func 'bits :='(bit_index, num_bits, tbitmap, word) = tbitmap.
:- mode 'bits :='(in, in, tbitmap_di, in) = tbitmap_uo is det.

%---------------------------------------------------------------------------%

:- func byte(int, tbitmap) = int.
%:- mode byte(in, tbitmap_ui) = out is det.
:- mode byte(in, in) = out is det.

:- func 'byte :='(int, tbitmap, int) = tbitmap.
:- mode 'byte :='(in, tbitmap_di, in) = tbitmap_uo is det.

%---------------------------------------------------------------------------%

    % Flip the given bit.
    %
:- func flip(tbitmap, bit_index) = tbitmap.
:- mode flip(tbitmap_di, in) = tbitmap_uo is det.

%---------------------------------------------------------------------------%

    % Set operations; for binary operations the second argument is altered
    % in all cases.  The input bitmaps must have the same size.
    %

:- func complement(tbitmap) = tbitmap.
:- mode complement(tbitmap_di) = tbitmap_uo is det.

:- func union(tbitmap, tbitmap) = tbitmap.
%:- mode union(tbitmap_ui, tbitmap_di) = tbitmap_uo is det.
:- mode union(in, tbitmap_di) = tbitmap_uo is det.

:- func intersect(tbitmap, tbitmap) = tbitmap.
%:- mode intersect(tbitmap_ui, tbitmap_di) = tbitmap_uo is det.
:- mode intersect(in, tbitmap_di) = tbitmap_uo is det.

:- func difference(tbitmap, tbitmap) = tbitmap.
%:- mode difference(tbitmap_ui, tbitmap_di) = tbitmap_uo is det.
:- mode difference(in, tbitmap_di) = tbitmap_uo is det.

:- func xor(tbitmap, tbitmap) = tbitmap.
%:- mode xor(tbitmap_ui, tbitmap_di) = tbitmap_uo is det.
:- mode xor(in, tbitmap_di) = tbitmap_uo is det.

%---------------------------------------------------------------------------%

    % copy_bits(SrcBM, SrcStartBit, DestBM, DestStartBit, NumBits)
    %
    % Overwrite NumBits bits in DestBM starting at DestStartBit with
    % the NumBits bits starting at SrcStartBit in SrcBM.
    %
:- func copy_bits(tbitmap, bit_index, tbitmap, bit_index, num_bits) = tbitmap.
%:- mode copy_bits(tbitmap_ui, in, tbitmap_di, in, in) = tbitmap_uo is det.
:- mode copy_bits(in, in, tbitmap_di, in, in) = tbitmap_uo is det.

    % copy_bits_in_bitmap(BM, SrcStartBit, DestStartBit, NumBits)
    %
    % Overwrite NumBits bits starting at DestStartBit with the NumBits
    % bits starting at SrcStartBit in the same bitmap.
    %
:- func copy_bits_in_bitmap(tbitmap, bit_index, bit_index, num_bits) = tbitmap.
:- mode copy_bits_in_bitmap(tbitmap_di, in, in, in) = tbitmap_uo is det.

    % copy_bytes(SrcBM, SrcStartByte, DestBM, DestStartByte, NumBytes)
    %
    % Overwrite NumBytes bytes in DestBM starting at DestStartByte with
    % the NumBytes bytes starting at SrcStartByte in SrcBM.
    %
:- func copy_bytes(tbitmap, byte_index, tbitmap, byte_index,
    num_bytes) = tbitmap.
%:- mode copy_bytes(tbitmap_ui, in, tbitmap_di, in, in) = tbitmap_uo is det.
:- mode copy_bytes(in, in, tbitmap_di, in, in) = tbitmap_uo is det.

    % copy_bytes_in_bitmap(BM, SrcStartByte, DestStartByte, NumBytes)
    %
    % Overwrite NumBytes bytes starting at DestStartByte with the NumBytes
    % bytes starting at SrcStartByte in the same bitmap.
    %
:- func copy_bytes_in_bitmap(tbitmap, byte_index, byte_index,
    num_bytes) = tbitmap.
:- mode copy_bytes_in_bitmap(tbitmap_di, in, in, in) = tbitmap_uo is det.

:- func to_byte_string(tbitmap) = string.
%:- mode to_byte_string(tbitmap_ui) = out is det.
:- mode to_byte_string(in) = out is det.

:- func ordering(tbitmap, tbitmap) = comparison_result.
:- mode ordering(tbitmap_ui, tbitmap_ui) = out is det.

:- func test_unify(tbitmap, tbitmap) = bool.
:- mode test_unify(tbitmap_ui, tbitmap_ui) = out is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module deconstruct.
:- import_module enum.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

new(N, B) = bitmap.init(N, B) - new(N, B).

%---------------------------------------------------------------------------%

resize(BM, NewSize, InitializerBit) =
    check("resize", BM, {NewSize, InitializerBit},
        resize(BM ^ fst, NewSize, InitializerBit)
            - resize(BM ^ snd, NewSize, InitializerBit)).

copy(BM) =
    check("copy", BM, {}, copy(BM ^ fst) - copy(BM ^ snd)).

%---------------------------------------------------------------------------%

bit(I, BM) =
    check_query("bit", BM, I, BM ^ fst ^ bit(I) - BM ^ snd ^ bit(I)).

'bit :='(I, BM, B) =
    check("bit :=", BM, {I, B},
        (BM ^ fst ^ bit(I) := B)
            ^ snd ^ bit(I) := B).

%---------------------------------------------------------------------------%

bits(FirstBit, NumBits, BM) =
    check_query("bits", BM, {FirstBit, NumBits},
        BM ^ fst ^ bits(FirstBit, NumBits) -
            BM ^ snd ^ bits(FirstBit, NumBits)).

'bits :='(FirstBit, NumBits, BM, Bits) =
    check("bits :=", BM, {FirstBit, NumBits},
        (BM ^ fst ^ bits(FirstBit, NumBits) := Bits)
            ^ snd ^ bits(FirstBit, NumBits) := Bits).

%---------------------------------------------------------------------------%

byte(I, BM) =
    check_query("byte", BM, I, BM ^ fst ^ byte(I) - BM ^ snd ^ byte(I)).

'byte :='(I, BM, B) =
    check("byte :=", BM, {I, B},
        (BM ^ fst ^ byte(I) := B)
            ^ snd ^ byte(I) := B).

%---------------------------------------------------------------------------%

flip(BM, I) =
    check("bits :=", BM, I, flip(BM ^ fst, I) - flip(BM ^ snd, I)).

%---------------------------------------------------------------------------%

complement(BM) =
    check("complement", BM, {}, complement(BM ^ fst) - complement(BM ^ snd)).

%---------------------------------------------------------------------------%

union(BMa, BMb) =
    check2("union", BMa, BMb, {},
        union(BMa ^ fst, BMb ^ fst)
            - union(BMa ^ snd, BMb ^ snd)).

%---------------------------------------------------------------------------%

intersect(BMa, BMb) =
    check2("intersect", BMa, BMb, {},
        intersect(BMa ^ fst, BMb ^ fst)
            - intersect(BMa ^ snd, BMb ^ snd)).

%---------------------------------------------------------------------------%

difference(BMa, BMb) =
    check2("difference", BMa, BMb, {},
        difference(BMa ^ fst, BMb ^ fst)
            - difference(BMa ^ snd, BMb ^ snd)).

%---------------------------------------------------------------------------%

xor(BMa, BMb) =
    check2("xor", BMa, BMb, {},
        xor(BMa ^ fst, BMb ^ fst)
            - xor(BMa ^ snd, BMb ^ snd)).

%---------------------------------------------------------------------------%

copy_bits(SrcBM, SrcStartBit, DestBM, DestStartBit, NumBits) =
    check2("copy_bits", SrcBM, DestBM, {SrcStartBit, DestStartBit, NumBits},
        copy_bits(SrcBM ^ fst, SrcStartBit,
            DestBM ^ fst, DestStartBit, NumBits) -
        copy_bits(SrcBM ^ snd, SrcStartBit,
            DestBM ^ snd, DestStartBit, NumBits)).

copy_bits_in_bitmap(SrcBM, SrcStartBit, DestStartBit, NumBits) =
    check("copy_bits_in_bitmap", SrcBM, {SrcStartBit, DestStartBit, NumBits},
        copy_bits_in_bitmap(SrcBM ^ fst, SrcStartBit, DestStartBit, NumBits) -
        copy_bits_in_bitmap(SrcBM ^ snd, SrcStartBit, DestStartBit, NumBits)).

copy_bytes(SrcBM, SrcStartByte, DestBM, DestStartByte, NumBytes) =
    check2("copy_bytes", SrcBM, DestBM,
        {SrcStartByte, DestStartByte, NumBytes},
        copy_bytes(SrcBM ^ fst, SrcStartByte,
            DestBM ^ fst, DestStartByte, NumBytes) -
        copy_bytes(SrcBM ^ snd, SrcStartByte,
            DestBM ^ snd, DestStartByte, NumBytes)).

copy_bytes_in_bitmap(SrcBM, SrcStartByte, DestStartByte, NumBytes) =
    check("copy_bytes_in_bitmap", SrcBM,
        {SrcStartByte, DestStartByte, NumBytes},
        copy_bytes_in_bitmap(SrcBM ^ fst, SrcStartByte,
            DestStartByte, NumBytes) -
        copy_bytes_in_bitmap(SrcBM ^ snd, SrcStartByte,
            DestStartByte, NumBytes)).

%---------------------------------------------------------------------------%

ordering(BM1, BM2) =
    check_query2("ordering", BM1, BM2, {},
        builtin.ordering(BM1 ^ fst, BM2 ^ fst) -
        builtin.ordering(BM1 ^ snd, BM2 ^ snd)).

test_unify(BM1, BM2) =
    check_query2("test_unify", BM1, BM2, {},
        pred_to_bool(unify(BM1 ^ fst, BM2 ^ fst)) -
        pred_to_bool(unify(BM1 ^ snd, BM2 ^ snd))).

%---------------------------------------------------------------------------%

:- func check(string, tbitmap, OtherArgs, tbitmap) = tbitmap.
%:- mode check(in, tbitmap_ui, in, tbitmap_di) = tbitmap_uo is det.
:- mode check(in, in, in, tbitmap_di) = tbitmap_uo is det.

check(Op, Tester0, OtherArgs, Tester) = Tester :-
    Tester = BM - SBM,
    BMArray = to_sbitmap(BM),
    ( if verify(BM), BMArray = SBM then
        true
    else
        throw('new one_argument'(Op, Tester0 ^ snd, OtherArgs, Tester))
    ).

:- func check2(string, tbitmap, tbitmap, OtherArgs, tbitmap) = tbitmap.
%:- mode check2(in, tbitmap_ui, tbitmap_ui, in, tbitmap_di) = tbitmap_uo is det.
:- mode check2(in, in, in, in, tbitmap_di) = tbitmap_uo is det.

check2(Op, Tester1, Tester2, OtherArgs, Tester) = Result :-
    Tester = BM - SBM,
    BMArray = to_sbitmap(BM),
    ( if verify(BM), SBM = BMArray then
        Result = Tester
    else
        throw('new two_arguments'(Op, Tester1 ^ snd, Tester2 ^ snd,
            OtherArgs, Tester))
    ).

:- func check_query(string, tbitmap, OtherArgs, mypair(T, T)) = T.
%:- mode check_query(in, tbitmap_ui, in, in) = out is det.
:- mode check_query(in, in, in, in) = out is det.

check_query(Op, Tester1, OtherArgs, Res) = TheRes :-
    Res = Res1 - Res2,
    ( if Res1 = Res2 then
        TheRes = Res1
    else
        throw('new query'(Op, Tester1 ^ snd, OtherArgs, Res))
    ).

:- func check_query2(string, tbitmap, tbitmap,
    OtherArgs, mypair(T, T)) = T.
%:- mode check_query2(in, tbitmap_ui, tbitmap_ui, in, in) = out is det.
:- mode check_query2(in, in, in, in, in) = out is det.

check_query2(Op, Tester1, Tester2, OtherArgs, Res) = TheRes :-
    Res = Res1 - Res2,
    ( if Res1 = Res2 then
        TheRes = Res1
    else
        throw('new binary_query'(Op, Tester1 ^ snd, Tester2 ^ snd,
            OtherArgs, Res))
    ).

:- func to_sbitmap(bitmap) = sbitmap.
%:- mode to_sbitmap(bitmap_ui) = sbitmap_uo.
:- mode to_sbitmap(in) = sbitmap_uo.

to_sbitmap(BM) =
    to_sbitmap_2(0, BM ^ num_bits, BM, new(BM ^ num_bits, no)).

:- func to_sbitmap_2(int, int, bitmap, sbitmap) = sbitmap.
%:- mode to_sbitmap_2(in, in, bitmap_ui, sbitmap_di) = sbitmap_uo.
:- mode to_sbitmap_2(in, in, in, sbitmap_di) = sbitmap_uo.

to_sbitmap_2(Index, NumBits, BM, SBM) =
    ( if Index < NumBits then
        to_sbitmap_2(Index + 1, NumBits, BM,
            SBM ^ bit(Index) := BM ^ bit(Index))
    else
        SBM
    ).

to_byte_string(BM) = to_byte_string(BM ^ fst).

    % At each step we check that the output bitmap can be converted to
    % and from a string, and that the hash produced by the Mercury code
    % in bitmap.m matches that produced by the C code in mercury_bitmap.h.
    % We also check that any trailing bits in the final byte are empty.
:- pred verify(bitmap).
%:- mode verify(bitmap_ui) is semidet.
:- mode verify(in) is semidet.

verify(BM) :-
    % functor/4 uses MR_bitmap_to_string in runtime/mercury_bitmap.h.
    functor(BM, do_not_allow, Functor, _Arity),
    Str = bitmap.to_string(BM),
    ( if
        Functor = "\"" ++ Str ++ "\"",
        BM = bitmap.from_string(Str)
    then
        semidet_succeed
    else
        throw(bitmap_verify_error(BM, to_string(Functor, Str)))
    ),
    ( if bitmap.hash(BM) = foreign_hash(BM) then
        semidet_succeed
    else
        throw(bitmap_verify_error(BM, hash))
    ),
    NumBits = BM ^ num_bits,
    BitsInLastByte = NumBits `rem` bitmap.bits_per_byte,
    ( if
        BitsInLastByte = 0
    then
        true
    else if
        0 = BM ^ unsafe_bits(NumBits, bitmap.bits_per_byte - BitsInLastByte)
    then
        true
    else
        throw(bitmap_verify_error(BM, trailing_bits_not_empty))
    ).

:- pragma foreign_decl("C", "#include ""mercury_bitmap.h""").

:- func foreign_hash(bitmap) = int.
%:- mode foreign_hash(bitmap_ui) = out is det.
:- mode foreign_hash(in) = out is det.
:- pragma promise_pure(foreign_hash/1).

foreign_hash(BM) = bitmap.hash(BM).

:- pragma foreign_proc("C",
    foreign_hash(BM::in) = (Hash::out),
    [will_not_call_mercury, promise_pure],
    "Hash = MR_hash_bitmap(BM);"
).
