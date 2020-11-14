%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Character constants in clause heads were doubly-escaped in .opt files.
%

:- module intermod_char.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module intermod_char2.

main(!IO) :-
    ( if p('\r') then
        true
    else
        true
    ).
