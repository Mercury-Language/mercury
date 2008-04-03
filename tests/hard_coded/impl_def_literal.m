% Test implementation-defined literals, e.g. $file.

:- module impl_def_literal.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module impl_def_literal.sub.
:- import_module string.

main(!IO) :-
    io.write_string($file, !IO),
    io.nl(!IO),
    io.write_int($line, !IO),
    io.nl(!IO),
    io.write_string($module, !IO),
    io.nl(!IO),
    io.write_string($pred, !IO),
    io.nl(!IO),
    io.write_string(a_function, !IO),
    io.nl(!IO),

    fun_with_lines(!IO),
    fun_with_lines_2(!IO),

    % We don't actually write out the grade string so as not to make the
    % expected output grade-dependent.
    ( string.length($grade) = 0 ->
        io.write_string("huh?\n", !IO)
    ;
        io.write_string("have $grade\n", !IO)
    ),

    in_submodule(!IO).

:- func a_function = string.

a_function = $pred.

:- pred fun_with_lines(io::di, io::uo) is det.

fun_with_lines(!IO) :-
    X = $line,
    Y = $line,
    ( X = Y ->
        io.write_string("fun_with_lines: equal\n", !IO)
    ;
        io.write_string("fun_with_lines: unequal\n", !IO)
    ).

:- pred fun_with_lines_2(io::di, io::uo) is det.

fun_with_lines_2(!IO) :-
    % The user probably expects the two occurrences of $line to be replaced
    % by two different numbers, but that doesn't happen.
    (
        $line =
        $line
    ->
        io.write_string("fun_with_lines_2: equal\n", !IO)
    ;
        io.write_string("fun_with_lines_2: unequal\n", !IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module sub.
:- interface.

:- pred in_submodule(io::di, io::uo) is det.

:- implementation.

in_submodule(!IO) :-
    io.write_string($module, !IO),
    io.nl(!IO),
    io.write_string($pred, !IO),
    io.nl(!IO),
    io.write_string($file, !IO),
    io.nl(!IO),
#10101
    io.write_int($line, !IO),
    io.nl(!IO).

:- end_module sub.

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
