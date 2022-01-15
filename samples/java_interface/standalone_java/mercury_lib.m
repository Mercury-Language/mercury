%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module mercury_lib.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

    % Write "Hello World" to the current Mercury text output stream.
    %
:- pred write_hello(io::di, io::uo) is det.

    % cube(X) returns X * X * X.
    %
:- func cube(int) = int.

    % fibs(N) returns the Nth Fibonacci number using a parallelised naive
    % algorithm.
    %
:- func fibs(int) = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.
:- import_module thread.
:- import_module thread.future.

%-----------------------------------------------------------------------------%

:- pragma foreign_export("Java", write_hello(di, uo), "writeHello").

write_hello(!IO) :-
    io.print_line("Hello World", !IO).

%-----------------------------------------------------------------------------%

:- pragma foreign_export("Java", cube(in) = out, "cube").

cube(X) = X * X * X.

%-----------------------------------------------------------------------------%
%
% Trivial concurrency test.
%

% No one would normally write fibs this way.

:- pragma foreign_export("Java", fibs(in) = out, "fibs").

fibs(N) = fibs_par(N).

:- func fibs_par(int) = int.

fibs_par(N) = F :-
    ( if N < 2 then
        F = 1
    else if N > fibs_thresh then
        F2 = future((func) = fibs_par(N - 2)),
        F1 = fibs_par(N - 1),
        F = F1 + wait(F2)
    else
        F = fibs_seq(N - 1) + fibs_seq(N - 2)
    ).

:- func fibs_seq(int) = int.

fibs_seq(N) =
    ( if N < 2 then
        1
    else
        fibs_seq(N-2) + fibs_seq(N-1)
    ).

:- func fibs_thresh = int.

fibs_thresh = 20.

%-----------------------------------------------------------------------------%
%
% Initialiser for this library.
%

:- initialise initialiser/2.

:- pred initialiser(io::di, io::uo) is det.

initialiser(!IO) :-
    io.print_line("mercury_lib: the initialiser has now been invoked.", !IO).

%-----------------------------------------------------------------------------%
%
% Finaliser for this library.
%

:- finalise finaliser/2.

:- pred finaliser(io::di, io::uo) is det.

finaliser(!IO) :-
    io.print_line("mercury_lib: the finaliser has now been invoked.", !IO).

%-----------------------------------------------------------------------------%
:- end_module mercury_lib.
%-----------------------------------------------------------------------------%
