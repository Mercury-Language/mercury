%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug441_helper_1.
:- interface.

:- import_module io.
:- import_module unit.

%---------------------------------------------------------------------------%

:- type fio(T).

:- type fio == fio(unit).

:- type fmain == (func(fio) = fio).
:- inst fmain == (func(di) = uo is det).

:- pred do_fmain(fmain::in(fmain), io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func print_fio_line(T::in, fio(_)::di) = (fio::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- type fio(T)
    --->    fio(T).

do_fmain(FMain, !IO) :-
    FIO0 = fio_from_io(!.IO),
    FIO = FMain(FIO0),
    !:IO = io_from_fio(FIO).

:- func fio_from_io(io::di) = (fio::uo) is det.

fio_from_io(_) = FIO :-
    FIO = unsafe_promise_unique(fio(unit)).

:- func io_from_fio(fio(_)::di) = (io::uo) is det.

:- pragma foreign_proc("C",
    io_from_fio(_FIO::di) = (IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    IO = 0;
").

print_fio_line(Line, _) = FIO :-
    trace [io(!IO)] (
        io.print_line(Line, !IO)
    ),
    FIO = unsafe_promise_unique(fio(unit)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
