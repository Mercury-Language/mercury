%
% Copyright (C) 1998 Jürgen Stuber <juergen@mpi-sb.mpg.de>
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%
% I couldn't resist:
% 	Jürgen Stuber <juergen@mpi-sb.mpg.de>
% 	http://www.mpi-sb.mpg.de/~juergen/

:- module rot13_juergen.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module char, int, require.

:- pred rot13( char::in, char::out) is det.

main -->
    io__read_char( Result ),
    ( { Result = ok( Char ) } ->
        { rot13( Char, Rot13Char ) },
	io__write_char( Rot13Char ),
        main
      
    ; { Result = eof } ->
        { true }
    ;
	{ error( "read failed" ) }
    ).

rot13( Char, Rot13Char ) :-
    char__to_int( Char, Code ),
    ( 0'A =< Code, Code =< 0'Z ->
        Rot13Code = (Code - 0'A + 13) mod 26 + 0'A
    ; 0'a =< Code, Code =< 0'z ->
        Rot13Code = (Code - 0'a + 13) mod 26 + 0'a
    ; 
        Rot13Code = Code
    ),
    ( char__to_int( Ch, Rot13Code ) ->
                Rot13Char = Ch
    ;
                error("too offensive, censored")
    ).

