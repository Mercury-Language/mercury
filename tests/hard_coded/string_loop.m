%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tom says "The following module loops forever on mundook".
%

:- module string_loop.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module float.
:- import_module string.

main(IO0, IO) :-
    string.format("%ei\n", [f(0.0)], Str),
    io.write_string(Str, IO0, IO).
