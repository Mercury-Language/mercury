
:- module qs_utils.

:- interface.

:- import_module io.
:- import_module list.
:- import_module string.

:- inst list_skel_unique for list/1 == unique(
        [] ;
        [ground | list_skel_unique]
    ).

:- mode list_skel_di == list_skel_unique >> dead.
:- mode list_skel_uo == free >> list_skel_unique.

:- pred partition(string, list(string), list(string), list(string)).
:- mode partition(in, in, out, out) is det.
:- mode partition(in, list_skel_di, list_skel_uo, list_skel_uo) is det.

:- pred read_input(list(string)::uo, io::di, io::uo) is det.

:- pred write_output(list(string)::in, io::di, io::uo) is det.

:- pred sink_output(T::in, io::di, io::uo) is det.

:- pred open_log(output_stream::out, io::di, io::uo) is det.

:- pred my_append(list(T), list(T), list(T)).
:- mode my_append(in, in, out) is det.
:- mode my_append(list_skel_di, list_skel_di, list_skel_uo) is det.

:- implementation.

:- import_module exception.
:- import_module maybe.

partition(_, [], [], []).
partition(P, [X | Xs], Bigs, Littles) :-
    partition(P, Xs, Bigs0, Littles0),
    ( compare(>, X, P) ->
        Bigs = [X | Bigs0],
        Littles = Littles0
    ;
        Bigs = Bigs0,
        Littles = [ X | Littles0 ]
    ).

read_input(Lines, !IO) :-
    io.command_line_arguments(Files, !IO),
    foldl2(read_lines_from_file, Files, [], Lines, !IO).

:- pred read_lines_from_file(string::in, list(string)::di, list(string)::uo,
    io::di, io::uo) is det.

read_lines_from_file(Filename, !Lines, !IO) :-
    io.open_input(Filename, Result, !IO),
    (
        Result = ok(Stream),
        read_lines_as_strings(Stream, !Lines, !IO),
        io.close_input(Stream, !IO)
    ;
        Result = error(Error),
        error(error_message(Error))
    ).

:- pred read_lines_as_strings(input_stream::in, 
    list(string)::di, list(string)::uo, io::di, io::uo) is det.

read_lines_as_strings(InputStream, !Lines, !IO) :-
    io.read_line_as_string(InputStream, Result, !IO),
    (
        Result = ok(Line0),
        % Copy makes this unique.
        copy(Line0, Line),
        !:Lines = [ Line | !.Lines ],
        read_lines_as_strings(InputStream, !Lines, !IO)
    ;
        Result = eof
    ;
        Result = error(Error),
        error(error_message(Error))
    ).

write_output([], !IO).
write_output([ String | Strings ], !IO) :-
    io.write_string(String, !IO),
    write_output(Strings, !IO).

:- pragma foreign_proc("C", sink_output(_Input::in, IO0::di, IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury],
    "
        IO = IO0;
    ").

open_log(Log, !IO) :-
    io.get_environment_var("LOG", MaybeLogFile, !IO),
    (
        MaybeLogFile = yes(LogFile)
    ;
        MaybeLogFile = no,
        error("LOG environment variable was unset.")
    ),
    io.open_append(LogFile, OpenLogfileResult, !IO),
    (
        OpenLogfileResult = ok(Log)
    ;
        OpenLogfileResult = error(Error),
        error(error_message(Error))
    ).

my_append([], !L).
my_append([X | Xs], List0, [X | List]) :-
    my_append(Xs, List0, List).

:- pred error(string::in) is erroneous.

error(String) :- 
    throw(String).

