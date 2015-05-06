:- module mp_int_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, mp_int, list, string.

main(!IO) :-
    A = mp_int(-10),
    B = mp_int(3),
    C = A * B,
    BinaryX = det_from_base_string("1101101101110011100110101000111101010100111001011010101010101101011", 2),
    BinaryY = det_from_base_string("01111000101100101101101101110011100110101000111101010100111001110010", 2),
    BinaryZ_AND = BinaryX /\ BinaryY,
    BinaryZ_OR  = BinaryX \/ BinaryY,
    BinaryZ_XOR = xor(BinaryX, BinaryY),
    BSX = to_base_string(BinaryX, 2),
    BSY = to_base_string(BinaryY, 2),
    BSZ_AND = to_base_string(BinaryZ_AND, 2),
    BSZ_OR = to_base_string(BinaryZ_OR, 2),
    BSZ_XOR = to_base_string(BinaryZ_XOR, 2),
    BinaryZ_COMPL = \ BinaryX,
    BSZ_COMPL = to_base_string(BinaryZ_COMPL, 2),
    io.format("bitwise ops of\n%s\n%s\n/\\:  %s\n\\/:  %s\nXOR: %s\n\\ %s:\n  %s\n",
              [s(BSX), s(BSY), s(BSZ_AND), s(BSZ_OR), s(BSZ_XOR),
               s(BSX), s(BSZ_COMPL)], !IO),
    divide_with_rem(A, B, Quot, Rem),
    Large  = det_from_string("123456789123456789123456789"),
    Larger = det_from_string("123456789123456789123456789123456789"),
    QResult = Larger // Large,
    RResult = Larger rem Large,
    divide_with_rem(Larger, Large, QResult0, RResult0),
    io.format("Large: %s // %s = (%s, %s)\n",
              [s(to_string(Larger)), s(to_string(Large)),
               s(to_string(QResult)), s(to_string(RResult))],
              !IO),
    io.format("divide_with_rem(%s::in, %s::in, %s::out, %s::out)\n",
              [s(to_string(Larger)), s(to_string(Large)),
               s(to_string(QResult0)), s(to_string(RResult0))],
              !IO),
    AS = to_string(A),
    BS = to_string(B),
    CS = to_string(C),
    QuotS = to_string(Quot),
    RemS = to_string(Rem),
    multiply_by_2(A, A2),
    A2S = to_string(A2),
    io.format("%s * %s = %s\n%s // %s = (%s, %s)\ncompare(%s, %s) = ",
             [s(AS), s(BS), s(CS),
              s(AS), s(BS), s(QuotS), s(RemS),
              s(AS), s(BS)], !IO),
    ( A < B ->
        io.print("<", !IO)
    ;
        io.print(">=", !IO)
    ),
    io.nl(!IO),
    io.format("%s << 1 = %s\n",
              [s(AS), s(A2S)],
              !IO),
    MIN_INT = mp_int(int.min_int),
    io.format("min_int = %s\n", [s(to_string(MIN_INT))], !IO)
    .
