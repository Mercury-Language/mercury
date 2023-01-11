%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module func_exp.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    do_test(-1, 1, !IO),
    do_test(0, 1, !IO),
    do_test(1, 1, !IO),
    do_test(10, 1, !IO).

%---------------------------------------------------------------------------%

:- pred do_test(int::in, int::in, io::di, io::uo) is cc_multi.

do_test(N, X, !IO) :-
    io.format("pow(double, %d, %d) = ", [i(N), i(X)], !IO),
    ( try []
        Result = std_util.pow(double, N, X)
    then
        io.print_line(Result, !IO)
    catch_any S ->
        io.print_line(S, !IO)
    ).

%---------------------------------------------------------------------------%

:- func double(int) = int.

double(X) = 2 * X.

%---------------------------------------------------------------------------%
:- end_module func_exp.
%---------------------------------------------------------------------------%
