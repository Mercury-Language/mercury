%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module mercury_lib.
:- interface.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Write "Hello World" to the current Mercury text output stream.
    %
:- pred write_hello(io::di, io::uo) is det.

    % cube(X) returns X * X * X.
    %
:- func cube(int) = int.

    % Write the given list of ints to the current Mercury text output stream.
    %
:- pred print_list(list(int)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

    % Succeeds if the given fruit is a citrus fruit.
    %
:- pred is_citrus(fruit::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------------%

    % Make the data constructors of the fruit/0 type visible to C#.
    %
:- pragma foreign_export_enum("C#", fruit/0, [uppercase]).

:- pragma foreign_export("C#", is_citrus(in), "IsCitrus").

is_citrus(orange).
is_citrus(lemon).

%-----------------------------------------------------------------------------%

:- pragma foreign_export("C#", write_hello(di, uo), "WriteHello").

write_hello(!IO) :-
    io.print_line("Hello World", !IO).

%-----------------------------------------------------------------------------%

:- pragma foreign_export("C#", cube(in) = out, "Cube").

cube(X) = X * X * X.

%-----------------------------------------------------------------------------%

:- pragma foreign_export("C#", print_list(in, di, uo), "PrintList").

print_list(List, !IO) :-
    io.print_line(List, !IO).

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
