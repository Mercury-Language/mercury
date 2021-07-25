%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module trust_2.

:- interface.

:- import_module trust_1.

:- pred concat(w::in, w::in, w::out) is cc_multi.

:- implementation.

:- import_module string.

concat(w(S), w(T), w(S ++ T)).
