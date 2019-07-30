%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test handling of null characters (any null character is an error).

:- module null_char.

:- interface.

:- type foo == int.

:- implementation.

:- import_module char.

:- pred 'wr\0\ng1'(int).

'wr\0\ng'(1).

:- pred wrong2(string).

wrong2("wr\0\ng").

:- pred wrong3(string).

wrong3(_String) :-'wr\0\ng'.

:- pred wrong4(char).

wrong4('\0\').

:- pred 'wr\0\ng5'.

'wr\0\ng5'.

% wrong6 used to have a raw NUL byte.

:- pred wrong7(string).

wrong7("wr\x0\ng").

:- pred wrong8(string).

wrong8("wr\u0000ng").

% wrong9 used to have a raw NUL byte.
