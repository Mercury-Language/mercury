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

:- pred main(io__state::di,io__state::uo) is det.

:- implementation.

:- import_module char,int,list.

:- pred rot13(char::in,char::out) is det.

rot13(Char,RotChar) :-
    char__is_upper(Char) ->
        rot13(Char,0'A,RotChar)
    ;
    char__is_lower(Char) ->
        rot13(Char,0'a,RotChar)
    ;
    RotChar = Char.

:- pred rot13(char::in,int::in,char::out) is det.

rot13(Char,CodeLetterA,RotChar) :-
    char__to_int(Char,CodeChar),
    RotCode = (CodeChar - CodeLetterA + 13) mod 26 + CodeLetterA,
    char__to_int(RChar,RotCode) ->
        RotChar = RChar
    ;
        RotChar = '\a'.   
        /* Alert character (Error case. To satisfy mode check) */

:- pred printRotChars(list(char)::in,io__state::di,io__state::uo) is det.

printRotChars([]) -->
    [].

printRotChars([Ch|Chs]) -->
    {rot13(Ch,RotCh)},
    io__write_char(RotCh),
    printRotChars(Chs).

% Main Program

main -->
    io__read_line(Result),
    (
        {Result = ok(Line)},
        printRotChars(Line),
	main
    ;
        {Result = eof,
        true}
    ;
        {Result = error(Error),
        io__error_message(Error,Message)},
        io__stderr_stream(Stderr),
        io__write_string(Stderr,Message)
    ).
