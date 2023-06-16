%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Ensure that foreign_proc_make.dll and foreign_proc_make2.dll are built
% before attempting to build foreign_proc_make__cpp_code.dll and
% foreign_proc_make2__csharp_code.dll.
%

:- module foreign_proc_make.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module foreign_proc_make_helper_1.

main(!IO) :-
    io.write_int(f2 + f3, !IO),
    io.nl(!IO).

:- func f2 = int.
:- pragma foreign_proc("C#", f2 = (X::out), [promise_pure], "X=5;").

f2 = 5.
