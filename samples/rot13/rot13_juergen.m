% Copyright (C) 1998 Jürgen Stuber <juergen@mpi-sb.mpg.de>
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%
% I couldn't resist:
%   Jürgen Stuber <juergen@mpi-sb.mpg.de>
%   http://www.mpi-sb.mpg.de/~juergen/

:- module rot13_juergen.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module require.

main(!IO) :-
    io.read_char(Result, !IO),
    ( if Result = ok(Char) then
        rot13(Char, Rot13Char),
        io.write_char(Rot13Char, !IO),
        main(!IO)
    else if Result = eof then
        true
    else
        error("read failed")
    ).

:- pred rot13(char::in, char::out) is det.

rot13(Char, Rot13Char) :-
    char.to_int(Char, Code),
    ( if 0'A =< Code, Code =< 0'Z then
        Rot13Code = (Code - 0'A + 13) mod 26 + 0'A
    else if 0'a =< Code, Code =< 0'z then
        Rot13Code = (Code - 0'a + 13) mod 26 + 0'a
    else
        Rot13Code = Code
    ),
    ( if char.to_int(Ch, Rot13Code) then
        Rot13Char = Ch
    else
        error("too offensive, censored")
    ).

