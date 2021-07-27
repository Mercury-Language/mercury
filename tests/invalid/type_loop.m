%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. Some versions of the compiler loop on this
% until they run out stack space.

:- module type_loop.

:- interface.

:- import_module io.
:- type foo.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module map.

:- type foo == map(int, foo).   % ps, this looks a bit suspect.

main(!IO) :-
    io.write_string("Hi", !IO).
