%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for targets using UTF-8 encoding.
% The .exp2 file is for the Java backend.
% The .exp3 file is for the C# backend.
%
% XXX The Java and C# backends currently differ in whether the null byte on
% line 71 is considered an error.
%
%---------------------------------------------------------------------------%

:- module string_well_formed_utf8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if count_code_units("\U0001F600") = 4 then
        io.write_string("string encoding is UTF-8\n", !IO)
    else
        io.write_string("string encoding is UTF-16\n", !IO)
    ),
    loop([], AccResults, !IO),
    list.reverse(AccResults, Results0),
    list.remove_adjacent_dups(Results0, Results),
    io.write_list(Results, "\n", io.write_string, !IO).

:- pred loop(list(string)::in, list(string)::out, io::di, io::uo) is det.

loop(!Acc, !IO) :-
    io.get_line_number(LineNr, !IO),
    io.read_line_as_string(RLAS, !IO),
    (
        RLAS = ok(Line),
        check_line(LineNr, strip(Line), !Acc),
        loop(!Acc, !IO)
    ;
        RLAS = eof
    ;
        RLAS = error(Error),
        Msg = format("line %d: %s", [i(LineNr), s(io.error_message(Error))]),
        !:Acc = [Msg | !.Acc],
        loop(!Acc, !IO)
    ).

:- pred check_line(int::in, string::in, list(string)::in, list(string)::out)
    is det.

check_line(LineNr, S, !Acc) :-
    ( if string.is_well_formed(S) then
        ( if string.all_match(is_ascii, S) then
            Msg = ""
        else if string.contains_char(S, '\uFFFD') then
            Msg = format("line %d: contains replacement char", [i(LineNr)])
        else
            Msg = format("line %d: well-formed", [i(LineNr)])
        )
    else
        Msg = format("line %d: not well-formed", [i(LineNr)])
    ),
    !:Acc = [Msg | !.Acc].
