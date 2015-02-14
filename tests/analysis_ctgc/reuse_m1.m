%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module reuse_m1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module reuse_m2.

main(!IO) :-
    F = foo(1, 2),
    G = foo(3, 4),
    fiddle2(F, G, H),      % F can't be reused
    use(F, !IO),
    use(H, !IO).

:- pred use(T::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    use(T::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, no_sharing],
"
    (void) T;
    IO = IO0;
").
