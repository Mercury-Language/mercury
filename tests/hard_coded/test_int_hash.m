%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module test_int_hash.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hash_table.
:- import_module int.
:- import_module list.
:- import_module uint32.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    RNG0 = 128738123u32,
    NumTests = 10000,
    int.fold_up3(do_test, 1, NumTests, RNG0, _, 0, NumFails, !IO),
    ( if NumFails = 0 then
        io.write_string("ALL TESTS PASSED\n", !IO)
    else
        true
    ).

:- pred do_test(int::in, uint32::in, uint32::out, int::in, int::out,
    io::di, io::uo) is det.

do_test(_, !RNG, !NumFails, !IO) :-
    next(Num, !RNG),
    int.hash(Num, LibHash),
    cint_hash(Num, OrigHash),
    %io.format("hash(%d) = %d\n", [i(Num), i(LibHash)], !IO),
    ( if LibHash = OrigHash then
        true
    else
        io.format("FAILED: hash(%d), lib = %d, orig = %d\n",
            [i(Num), i(LibHash), i(OrigHash)], !IO),
        !:NumFails = !.NumFails + 1
    ).

    % A xorshift RNG.
:- pred next(int::out, uint32::in, uint32::out) is det.

next(N, !X) :-
    !:X = !.X `xor` (!.X << 13),
    !:X = !.X `xor` (!.X >> 17),
    !:X = !.X `xor` (!.X << 5),
    N = uint32.cast_to_int(!.X).

%---------------------------------------------------------------------------%

    % Ralph's original implementation.
    %
:- pred cint_hash(int::in, int::out) is det.

:- pragma foreign_proc("C",
    cint_hash(N::in, H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    const int c2 = 0x27d4eb2d; // a prime or an odd constant
    MR_Unsigned key;

    key = N;

    if (sizeof(MR_Word) == 4) {
        key = (key ^ 61) ^ (key >> 16);
        key = key + (key << 3);
        key = key ^ (key >> 4);
        key = key * c2;
        key = key ^ (key >> 15);
    } else {
        key = (~key) + (key << 21); // key = (key << 21) - key - 1;
        key = key ^ (key >> 24);
        key = (key + (key << 3)) + (key << 8);  //  key * 265
        key = key ^ (key >> 14);
        key = (key + (key << 2)) + (key << 4);  // key * 21
        key = key ^ (key >> 28);
        key = key + (key << 31);
    }

    H = key;
").

    % This is the original Java implementation from Thomas Wang's webpage.
    % We only need to provide a 32-bit implementation since Mercury's int type
    % corresponds to a Java int.
    %
:- pragma foreign_proc("Java",
    cint_hash(N::in, H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int c2=0x27d4eb2d; // a prime or an odd constant
    N = (N ^ 61) ^ (N >>> 16);
    N = N + (N << 3);
    N = N ^ (N >>> 4);
    N = N * c2;
    N = N ^ (N >>> 15);
    H = N;
").

:- pragma foreign_proc("C#",
    cint_hash(N::in, H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    uint key = (uint) N;
    uint c2=0x27d4eb2d; // a prime or an odd constant
    key = (key ^ 61) ^ (key >> 16);
    key = key + (key << 3);
    key = key ^ (key >> 4);
    key = key * c2;
    key = key ^ (key >> 15);
    H = (int)key;
").

%---------------------------------------------------------------------------%
:- end_module test_int_hash.
%---------------------------------------------------------------------------%
