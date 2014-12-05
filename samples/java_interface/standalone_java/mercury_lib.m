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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

:- pragma foreign_export("Java", write_hello(di, uo),
    "writeHello").

write_hello(!IO) :-
    io.print_line("Hello World", !IO).

%-----------------------------------------------------------------------------%

:- pragma foreign_export("Java", cube(in) = out,
    "cube").

cube(X) = X * X * X.

%-----------------------------------------------------------------------------%
%
% Initialiser for this library
%

:- initialise initialiser/2.

:- pred initialiser(io::di, io::uo) is det.

initialiser(!IO) :-
    io.print_line("mercury_lib: the initialiser has now been invoked.", !IO).

%-----------------------------------------------------------------------------%
%
% Finaliser for this library
%

:- finalise finaliser/2.

:- pred finaliser(io::di, io::uo) is det.

finaliser(!IO) :-
    io.print_line("mercury_lib: the finaliser has now been invoked.", !IO).

%-----------------------------------------------------------------------------%
:- end_module mercury_lib.
%-----------------------------------------------------------------------------%
