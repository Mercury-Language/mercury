%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Mercury 0.9 had a bug where a program compiled with tracing
% could not have a module with the same name as one of the
% browser library modules, which are not user-visible.
%

:- module parse.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_string("ok\n", !IO).
