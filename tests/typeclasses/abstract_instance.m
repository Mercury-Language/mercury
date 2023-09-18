%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module abstract_instance.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module abstract_instance_helper_1.
:- import_module list.

main(!IO) :-
    run(42, !IO),
    run("hello world", !IO),
    run([5, 4, 3, 2, 1], !IO),
    run(["hello", "world"], !IO),
    run([[[[0]]]], !IO).
