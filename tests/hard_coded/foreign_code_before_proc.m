%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test whether the contents of a foreign_code pragma are put into
% the generated .c file *before* the foreign_procs that use its contents.
%

:- module foreign_code_before_proc.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_line(xyzzy, !IO).

:- func xyzzy = int.
:- pragma foreign_proc("C",
    xyzzy = (X::out), [promise_pure],
"
    X = xyzzy;
").
xyzzy = 42.

:- pragma foreign_code("C", "
int xyzzy = 42;
").
