%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test comparison operations for unsigned 64-bit integers.

:- module cmp_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint64.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_cmp_test(unify, "==", !IO),
    io.nl(!IO),
    run_cmp_test(uint64.(<), "<", !IO),
    io.nl(!IO),
    run_cmp_test(uint64.(=<), "=<", !IO),
    io.nl(!IO),
    run_cmp_test(uint64.(>), ">", !IO),
    io.nl(!IO),
    run_cmp_test(uint64.(>=), ">=", !IO).

:- pred run_cmp_test(pred(uint64, uint64)::in(pred(in, in) is semidet),
    string::in, io::di, io::uo) is det.

run_cmp_test(CmpPred, Desc, !IO) :-
    io.format("*** Testing uint64.'%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_cmp_test_2(CmpPred, Desc, Bs), As, !IO).

:- pred run_cmp_test_2(pred(uint64, uint64)::in(pred(in, in) is semidet),
    string::in, list(uint64)::in, uint64::in, io::di, io::uo) is det.

run_cmp_test_2(CmpPred, Desc, Bs, A, !IO) :-
    list.foldl(run_cmp_test_3(CmpPred, Desc, A), Bs, !IO).

:- pred run_cmp_test_3(pred(uint64, uint64)::in(pred(in, in) is semidet),
    string::in, uint64::in, uint64::in, io::di, io::uo) is det.

run_cmp_test_3(CmpPred, Desc, A, B, !IO) :-
    Result = ( if CmpPred(A, B) then "true" else "false" ),
    io.format("%s %s %s = %s\n",
        [s(uint64_to_string(A)), s(Desc), s(uint64_to_string(B)), s(Result)],
        !IO).

:- func numbers = list(uint64).

numbers = [
    0_u64,
    1_u64,
    2_u64,
    8_u64,
    16_u64,
    255_u64,
    65_535_u64,
    2_147_483_647_u64,
    4_294_967_295_u64,
    18_446_744_073_709_551_615_u64
].

%---------------------------------------------------------------------------%
:- end_module cmp_uint64.
%---------------------------------------------------------------------------%
