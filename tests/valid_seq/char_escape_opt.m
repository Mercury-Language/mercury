%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Character constants in clause heads were doubly-escaped in .opt files.
%
% This test was originally called intermod_char.
%

:- module char_escape_opt.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char_escape_opt_helper_1.

main(!IO) :-
    ( if p('\r') then
        true
    else
        true
    ).
