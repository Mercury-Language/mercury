%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_vars.
:- interface.

:- typeclass c(T) where [].

:- type t1 --->     some [T1, T2] foo(T1) => c(T2).
                                                % T2 doesn't occur in ctor args
:- type t2 --->     some [T1, T2] bar(T1).      % T2 occurs only in quantifier
:- type t3(T1) ---> some [T1] bar(T1).          % T1 has overlapping scope
:- type t4 --->     some [T1] bar(T1) => c(T2). % T2 occurs only in constraint
