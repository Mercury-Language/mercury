% vim: ft=mercury ts=4 sw=4 et
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

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module thread.
:- import_module thread.channel.
:- import_module unit.

main(!IO) :-
    NumSpawn = 5000,
    channel.init(Channel, !IO),
    loop(Channel, NumSpawn, !IO),
    count(Channel, 0, NumExit, !IO),
    (if NumSpawn = NumExit then
        io.write_string("ok\n", !IO)
    else
        io.format("not ok: %d != %d\n", [i(NumSpawn), i(NumExit)], !IO)
    ).

:- pred loop(channel(unit)::in, int::in, io::di, io::uo) is cc_multi.

loop(Channel, N, !IO) :-
    ( if N = 0 then
        true
    else
        thread.spawn((pred(!.IO::di, !:IO::uo) is cc_multi :-
            foo(Channel, !IO)
        ), !IO),
        loop(Channel, N-1, !IO)
    ).

:- pred foo(channel(unit)::in, io::di, io::uo) is det.

foo(Channel, !IO) :-
    channel.put(Channel, unit, !IO).

:- pred count(channel(unit)::in, int::in, int::out, io::di, io::uo) is det.

count(Channel, Num0, Num, !IO) :-
    channel.try_take(Channel, MaybeUnit, !IO),
    (
        MaybeUnit = yes(_),
        count(Channel, Num0 + 1, Num, !IO)
    ;
        MaybeUnit = no,
        Num = Num0
    ).
