%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug441.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bug441_sub.

main(!IO) :-
    do_fmain(hello, !IO).

:- func hello : fmain `with_inst` fmain.

hello(FIO) = print_fio_line("Hello World!", FIO).
