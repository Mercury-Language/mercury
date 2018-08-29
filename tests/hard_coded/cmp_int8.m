%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test comparison operations for signed 8-bit integers.

:- module cmp_int8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int8.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_cmp_test(unify, "==", !IO),
    io.nl(!IO),
    run_cmp_test(int8.(<), "<", !IO),
    io.nl(!IO),
    run_cmp_test(int8.(=<), "=<", !IO),
    io.nl(!IO),
    run_cmp_test(int8.(>), ">", !IO),
    io.nl(!IO),
    run_cmp_test(int8.(>=), ">=", !IO).

:- pred run_cmp_test(pred(int8, int8)::in(pred(in, in) is semidet),
    string::in, io::di, io::uo) is det.

run_cmp_test(CmpPred, Desc, !IO) :-
    io.format("*** Testing int8.'%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_cmp_test_2(CmpPred, Desc, Bs), As, !IO).

:- pred run_cmp_test_2(pred(int8, int8)::in(pred(in, in) is semidet),
    string::in, list(int8)::in, int8::in, io::di, io::uo) is det.

run_cmp_test_2(CmpPred, Desc, Bs, A, !IO) :-
    list.foldl(run_cmp_test_3(CmpPred, Desc, A), Bs, !IO).

:- pred run_cmp_test_3(pred(int8, int8)::in(pred(in, in) is semidet),
    string::in, int8::in, int8::in, io::di, io::uo) is det.

run_cmp_test_3(CmpPred, Desc, A, B, !IO) :-
    Result = ( if CmpPred(A, B) then "true" else "false" ),
    io.format("%s %s %s = %s\n",
        [s(int8_to_string(A)), s(Desc), s(int8_to_string(B)), s(Result)],
        !IO).

:- func numbers = list(int8).

numbers = [
    -128_i8,
    0_i8,
    1_i8,
    2_i8,
    8_i8,
    10_i8,
    16_i8,
    127_i8
].

%---------------------------------------------------------------------------%
:- end_module cmp_int8.
%---------------------------------------------------------------------------%
