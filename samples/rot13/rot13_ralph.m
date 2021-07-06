% ---------------------------------------------------------------------------- %
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
% ---------------------------------------------------------------------------- %
% rot13_ralph.m
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Tue Jan  9 18:10:44 GMT 2001
%
% Short and sweet.
%
% ---------------------------------------------------------------------------- %

:- module rot13_ralph.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module require.

% ---------------------------------------------------------------------------- %

main(!IO) :-
    io.read_byte(Result, !IO),
    (
        Result = ok(X),
        io.write_byte(rot13(X), !IO),
        main(!IO)
    ;
        Result = eof
    ;
        Result = error(ErrNo),
        error(io.error_message(ErrNo))
    ).

% ---------------------------------------------------------------------------- %

:- func rot13(int) = int.

rot13(X) = RotX :-
    Rot13 = ( func(C, A) = ((13 + C - A) `rem` 26) + A ),
    RotX =
        (    if 0'a =< X, X =< 0'z then Rot13(X, 0'a)
        else if 0'A =< X, X =< 0'Z then Rot13(X, 0'A)
        else X
        ).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
