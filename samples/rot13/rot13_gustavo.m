% I have another version of rot13.
%
% Gustavo A. Ospina <g-ospina@uniandes.edu.co>
%
% This version reads a line and prints the line "roted". I think it is as
% declarative as Jurgen's version. Also I handle error and I used predicates
% on your char library. Maybe my version is slower, but it can be discussed.
%
% This source file is hereby placed in the public domain.
%	- Gustavo Ospina

:- module rot13_gustavo.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.

main(!IO) :-
    io.read_line(Result, !IO),
    (
        Result = ok(Line),
        print_rot_chars(Line, !IO),
        main(!IO)
    ;
        Result = eof
    ;
        Result = error(Error),
        io.error_message(Error, Message),
        io.stderr_stream(Stderr, !IO),
        io.write_string(Stderr, Message, !IO)
    ).

:- pred print_rot_chars(list(char)::in, io::di, io::uo) is det.

print_rot_chars([], !IO).
print_rot_chars([Ch | Chs], !IO) :-
    rot13(Ch, RotCh),
    io.write_char(RotCh, !IO),
    print_rot_chars(Chs, !IO).

:- pred rot13(char::in, char::out) is det.

rot13(Char, RotChar) :-
    ( if char.is_upper(Char) then
        rot13(Char, 0'A, RotChar)
    else if char.is_lower(Char) then
        rot13(Char, 0'a, RotChar)
    else
        RotChar = Char
    ).

:- pred rot13(char::in, int::in, char::out) is det.

rot13(Char, CodeLetterA, RotChar) :-
    char.to_int(Char, CodeChar),
    RotCode = (CodeChar - CodeLetterA + 13) mod 26 + CodeLetterA,
    ( if char.to_int(RChar, RotCode) then
        RotChar = RChar
    else
        RotChar = '\a'
        % Alert character (Error case. To satisfy mode check)
    ).
