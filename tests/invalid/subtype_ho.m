%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_ho.
:- interface.

:- import_module io.

:- type tasks(T)
    --->    task1(pred(T, io, io))
    ;       task2(pred(T::in, io::di, io::uo) is det)
    ;       fun(func(int) = int).

:- type task1(T) =< tasks(T)
    --->    task1(pred(T, io, io)).

:- type task2_or_fun(T) =< tasks(T)
    --->    task2(pred(T::in, io::di, io::uo) is det)
    ;       fun(func(int) = int).

:- type extra_mode_info(T) =< tasks(T)
    --->    task1(pred(T::in, io::di, io::uo) is det).

:- type missing_mode_info(T) =< tasks(T)
    --->    task2(pred(T, io, io)).

:- type mismatch_arg_mode(T) =< tasks(T)
    --->    task2(pred(T::out, io::di, io::uo) is det).

:- type mismatch_detism(T) =< tasks(T)
    --->    task2(pred(T::in, io::di, io::uo) is cc_multi).

:- type mismatch_purity(T) =< tasks(T)
    --->    task1(impure pred(T, io, io))
    ;       task2(impure pred(T::in, io::di, io::uo) is det).
