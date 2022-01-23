%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%-----------------------------------------------------------------------------%

:- module sleep.
:- interface.

:- import_module io.

    % usleep(MSec, !IO)
    %
    % Sleep for MSec microseconds.
    % Only implemented for Unix-like systems so far.
    %
:- pred usleep(int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C",
"
    #include <sys/time.h>
    #include <sys/types.h>
    #include <unistd.h>
").

:- pragma foreign_proc("C",
    usleep(N::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct timeval tv = {0, N};
    select(0, NULL, NULL, NULL, &tv);
").

%-----------------------------------------------------------------------------%
:- end_module sleep.
%-----------------------------------------------------------------------------%
