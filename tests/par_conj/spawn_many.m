% vim: ft=mercury ts=4 sw=4 et
%
% This program spawns many threads very quickly.  In an old implementation of
% thread.spawn, each new Mercury thread would push a large C stack frame on the
% executing Mercury engine's C stack.  When the engine hit a blocking call
% (e.g. channel.get) it would switch to the next Mercury context waiting, which
% would usually be the start of another Mercury thread, which pushes another
% large C stack frame.  Soon the C stack would be exhausted.
%
:- module spawn_many.
:- interface.

:- import_module io.

:- impure pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module thread.
:- import_module thread.channel.
:- import_module unit.

main(!IO) :-
    % Set a signal to go off if the program is taking too long.
    % The default SIGALRM handler will abort the program.
    impure alarm(10),

    NumSpawn = 5000,
    channel.init(Channel, !IO),
    loop(Channel, NumSpawn, !IO),
    wait(Channel, NumSpawn, !IO),
    io.write_string("ok\n", !IO).

:- pred loop(channel(unit)::in, int::in, io::di, io::uo) is cc_multi.

loop(Channel, N, !IO) :-
    ( if N = 0 then
        true
    else
        thread.spawn((pred(!.IO::di, !:IO::uo) is cc_multi :-
            channel.put(Channel, unit, !IO)
        ), !IO),
        loop(Channel, N-1, !IO)
    ).

:- pred wait(channel(unit)::in, int::in, io::di, io::uo) is det.

wait(Channel, Num, !IO) :-
    ( if Num = 0 then
        true
    else
        channel.take(Channel, _Unit, !IO),
        wait(Channel, Num - 1, !IO)
    ).

:- pragma foreign_decl("C", "#include <unistd.h>").
:- impure pred alarm(int::in) is det.

:- pragma foreign_proc("C",
    alarm(Seconds::in),
    [will_not_call_mercury],
"
    alarm(Seconds);
").
