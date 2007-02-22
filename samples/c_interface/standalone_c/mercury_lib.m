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

    % Write the current value of the mutable `global' to the current
    % Mercury text output stream.
    %
:- pred write_global_value(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

:- mutable(global, int, 561, ground, [untrailed,
    foreign_name("C", "GLOBAL"), attach_to_io_state]).

%-----------------------------------------------------------------------------%

:- pragma foreign_export("C", write_hello(di, uo),
    "write_hello").

write_hello(!IO) :-
    io.write_string("Hello World\n", !IO).

:- pragma foreign_export("C", write_global_value(di, uo),
    "write_global_value").

write_global_value(!IO) :-
    get_global(Value, !IO),
    io.format("The new value of global is %d.\n", [i(Value)], !IO).

%-----------------------------------------------------------------------------%
%
% Initialiser for this library
%

:- initialise initialiser/2.

:- pred initialiser(io::di, io::uo) is det.

initialiser(!IO) :-
    io.write_string("mercury_lib: the initialiser has now been invoked.\n",
        !IO).

%-----------------------------------------------------------------------------%
%
% Finaliser for this library
%

:- finalise finaliser/2.

:- pred finaliser(io::di, io::uo) is det.

finaliser(!IO) :-
    io.write_string("mercury_lib: the finaliser has now been invoked.\n",
        !IO).

%-----------------------------------------------------------------------------%
:- end_module mercury_lib.
%-----------------------------------------------------------------------------%
