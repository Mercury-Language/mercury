% vim: ft=mercury ts=8 sw=4 et
% Test that a multithread program doesn't terminate until all threads complete.

:- module thread_barrier.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module thread.
:- import_module thread.channel.
:- import_module unit.

main(!IO) :-
    channel.init(Channel, !IO),
    loop(Channel, 10, !IO).

:- pred loop(channel(unit)::in, int::in, io::di, io::uo) is cc_multi.

loop(Channel, N, !IO) :-
    ( if N > 0 then
        io.write_string("loop\n", !IO),
        thread.spawn(loop(Channel, N-1), !IO),
        % Give the current thread something to do.
        channel.put(Channel, unit, !IO)
    else
        true
    ).

:- finalize fin/2.
:- pred fin(io::di, io::uo) is det.

fin(!IO) :-
    % This should appear last.
    io.write_string("fin.\n", !IO).
