%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test comparison operations for signed 16-bit integers.

:- module cmp_int16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int16.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_cmp_test(unify, "==", !IO),
    io.nl(!IO),
    run_cmp_test(int16.(<), "<", !IO),
    io.nl(!IO),
    run_cmp_test(int16.(=<), "=<", !IO),
    io.nl(!IO),
    run_cmp_test(int16.(>), ">", !IO),
    io.nl(!IO),
    run_cmp_test(int16.(>=), ">=", !IO).

:- pred run_cmp_test(pred(int16, int16)::in(pred(in, in) is semidet),
    string::in, io::di, io::uo) is det.

run_cmp_test(CmpPred, Desc, !IO) :-
    io.format("*** Testing int16.'%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_cmp_test_2(CmpPred, Desc, Bs), As, !IO).

:- pred run_cmp_test_2(pred(int16, int16)::in(pred(in, in) is semidet),
    string::in, list(int16)::in, int16::in, io::di, io::uo) is det.

run_cmp_test_2(CmpPred, Desc, Bs, A, !IO) :-
    list.foldl(run_cmp_test_3(CmpPred, Desc, A), Bs, !IO).

:- pred run_cmp_test_3(pred(int16, int16)::in(pred(in, in) is semidet), string::in,
    int16::in, int16::in, io::di, io::uo) is det.

run_cmp_test_3(CmpPred, Desc, A, B, !IO) :-
    Result = ( if CmpPred(A, B) then "true" else "false" ),
    io.format("%s %s %s = %s\n",
        [s(int16_to_string(A)), s(Desc), s(int16_to_string(B)), s(Result)], !IO).

:- func numbers = list(int16).

numbers = [
    -32_768_i16,
    -128_i16,
    0_i16,
    1_i16,
    2_i16,
    8_i16,
    10_i16,
    16_i16,
    127_i16,
    32_767_i16
].

%---------------------------------------------------------------------------%
:- end_module cmp_int16.
%---------------------------------------------------------------------------%
