:- module intermod_char2.
:- interface.

:- import_module char.

:- pred p(char::in) is semidet.

:- implementation.

p('\r').
p('\n').

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
