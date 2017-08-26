%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test comparison operations for unsigned 32-bit integers.

:- module cmp_uint32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint32.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_cmp_test(unify, "==", !IO),
    io.nl(!IO),
    run_cmp_test(uint32.(<), "<", !IO),
    io.nl(!IO),
    run_cmp_test(uint32.(=<), "=<", !IO),
    io.nl(!IO),
    run_cmp_test(uint32.(>), ">", !IO),
    io.nl(!IO),
    run_cmp_test(uint32.(>=), ">=", !IO).

:- pred run_cmp_test(pred(uint32, uint32)::in(pred(in, in) is semidet),
    string::in, io::di, io::uo) is det.

run_cmp_test(CmpPred, Desc, !IO) :-
    io.format("*** Testing uint32.'%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_cmp_test_2(CmpPred, Desc, Bs), As, !IO).

:- pred run_cmp_test_2(pred(uint32, uint32)::in(pred(in, in) is semidet),
    string::in, list(uint32)::in, uint32::in, io::di, io::uo) is det.

run_cmp_test_2(CmpPred, Desc, Bs, A, !IO) :-
    list.foldl(run_cmp_test_3(CmpPred, Desc, A), Bs, !IO).

:- pred run_cmp_test_3(pred(uint32, uint32)::in(pred(in, in) is semidet), string::in,
    uint32::in, uint32::in, io::di, io::uo) is det.

run_cmp_test_3(CmpPred, Desc, A, B, !IO) :-
    Result = ( if CmpPred(A, B) then "true" else "false" ),
    io.format("%s %s %s = %s\n",
        [s(uint32_to_string(A)), s(Desc), s(uint32_to_string(B)), s(Result)],
        !IO).

:- func numbers = list(uint32).

numbers = [
    0_u32,
    1_u32,
    2_u32,
    8_u32,
    16_u32,
    255_u32,
    65_535_u32,
    2_147_483_647_u32,
    4_294_967_295_u32
].

%---------------------------------------------------------------------------%
:- end_module cmp_uint32.
%---------------------------------------------------------------------------%
