%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% When only a small part of a large expected-to-be-ground inst
% is not ground, we used to print the entire inst. Test whether
% we can generate more precisely targeted diagnostics.
%
%---------------------------------------------------------------------------%

:- module small_part_free.

:- interface.

:- type info
    --->    info(
                int,
                int,
                int,
                int,
                int,
                int,
                sub,
                int,
                int,
                int
            ).

:- type sub
    --->    sub(
                int,
                int,
                int,
                int,
                int,
                int,
                int,
                int,
                int
            ).

:- pred foo(int::in, sub::out) is det.
:- pred bar(int::in, info::out) is det.

:- implementation.

foo(N, Sub) :-
    Sub = sub(11, 12, 13, 14, 15, _, 17, 18, N).

bar(N, Info) :-
    Sub = sub(11, 12, _, 14, 15, _, 17, 18, N),
    Info = info(1, 2, _, 4, 5, 6, Sub, 8, 9, N).
