%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module char_escape_opt_helper_1.
:- interface.

:- import_module char.

:- pred p(char::in) is semidet.

:- implementation.

p('\r').
p('\n').
