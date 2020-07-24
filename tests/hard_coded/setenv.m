:- module setenv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module gc.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

main(!IO) :-
    io.set_environment_var("foo", "bar", !IO),

    % Earlier versions of the Mercury library relied on putenv, which
    % on many platforms requires that we don't garbage collect the string
    % we pass it. This code tests whether we can handle that.
    use_mem(1_000_000, !IO),
    gc.garbage_collect(!IO),
    use_mem(1_000_000, !IO),

    io.get_environment_var("foo", Res, !IO),
    (
        Res = yes(Value),
        io.write_string("Got value: " ++ Value ++ "\n", !IO)
    ;
        Res = no,
        io.write_string("Failure!\n", !IO)
    ).

:- pred use_mem(int::in, io::di, io::uo) is det.

use_mem(N, !IO) :-
    io.write_string("Use mem: ", !IO),
    ( if list.length(1 `..` N) = N then
        io.write_string("ok\n", !IO)
    else
        io.write_string("hmm\n", !IO)
    ).
