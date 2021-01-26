%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
% harness.m
% Ralph Becket <rbeck@microsoft.com>
% Mon Nov 13 13:12:09 GMT 2000
%---------------------------------------------------------------------------%

:- module harness.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bmio.
% :- import_module compress1.
% :- import_module compress2.
% :- import_module compress3.
% :- import_module compress4.
:- import_module compress5.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- func num_iterations = int.
num_iterations = 10.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(ArgV, !IO),
    ( if
        ArgV = [FileName, NumBytesStr],
        string.to_int(NumBytesStr, NumBytes)
    then
        bmio.init(FileName, NumBytes, !IO),
        %test("compress1", compress1.go),
        %test("compress2", compress2.go),
        %test("compress3", compress3.go),
        %test("compress4", compress4.go),
        test("compress5", compress5.go, !IO)
    else
        error("usage: compress <infile> <nbytes>")
    ).

%---------------------------------------------------------------------------%

:- pred test(string::in, pred(io, io)::in(pred(di, uo) is det),
    io::di, io::uo) is det.

test(_Name, Go, !IO) :-
    % io.format("\n\n******* %s x %d *******\n",
    %   [s(Name), i(num_iterations)], !IO),
    % io.report_stats(!IO),
    test_loop(num_iterations, Go, !IO).
    % io.report_stats(!IO).

%---------------------------------------------------------------------------%

:- pred test_loop(int::in, pred(io, io)::in(pred(di, uo) is det),
    io::di, io::uo) is det.

test_loop(N, Go, !IO) :-
    ( if N > 0 then
        bmio.use_compression_io(!IO),
        Go(!IO),
        test_loop(N - 1, Go, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module harness.
%---------------------------------------------------------------------------%
