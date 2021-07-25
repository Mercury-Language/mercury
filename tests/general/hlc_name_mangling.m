%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for a bug in the hlc grade where
% function names were not mangled correctly.
%

:- module hlc_name_mangling.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module 'hlc_name_mangling-helper-module'.

main(!IO) :-
    io.write_line(foo, !IO).
