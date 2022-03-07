%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case is to ensure that stdin/stdout are redirected correctly for
% system commands.
%

:- module system_sort.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module io.call_system.

main(!IO) :-
    io.call_system.call_system("sort", _, !IO).
