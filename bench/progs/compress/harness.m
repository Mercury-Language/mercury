% ---------------------------------------------------------------------------- %
% harness.m
% Ralph Becket <rbeck@microsoft.com>
% Mon Nov 13 13:12:09 GMT 2000
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%
% ---------------------------------------------------------------------------- %

:- module harness.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

:- import_module bmio, int, list, string, require.
% :- import_module compress1, compress2, compress3, compress4, compress5.
:- import_module compress5.

:- func num_iterations = int.
num_iterations = 10.

% ---------------------------------------------------------------------------- %

main -->
    io__command_line_arguments(ArgV),

    ( if
        { ArgV = [FileName, NumBytesStr] },
        { string__to_int(NumBytesStr, NumBytes) }
      then
        bmio__init(FileName, NumBytes),
        %test("compress1", compress1__go),
        %test("compress2", compress2__go),
        %test("compress3", compress3__go),
        %test("compress4", compress4__go),
        test("compress5", compress5__go)
      else
        { error("usage: compress <infile> <nbytes>") }
    ).

% ---------------------------------------------------------------------------- %

:- pred test(string, pred(io__state, io__state), io__state, io__state).
:- mode test(in, pred(di, uo) is det, di, uo) is det.

test(_Name, Go) -->
    % io__format("\n\n******* %s x %d *******\n", [s(Name), i(num_iterations)]),
    % io__report_stats,
    test_loop(num_iterations, Go).
    % io__report_stats.

% ---------------------------------------------------------------------------- %

:- pred test_loop(int, pred(io__state, io__state), io__state, io__state).
:- mode test_loop(in, pred(di, uo) is det, di, uo) is det.

test_loop(N, Go) -->
    ( if { N > 0 } then
        bmio__use_compression_io,
        Go,
        test_loop(N - 1, Go)
      else []
    ).

% ---------------------------------------------------------------------------- %
