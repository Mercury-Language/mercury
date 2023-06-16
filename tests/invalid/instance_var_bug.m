%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This program cause the following assertion failure in rotd-2007-11-11:
%
% Software Error: hlds_code_util.m: Unexpected: type_to_string: invalid type
%
% The problem was that the name mangling scheme used for the names of method
% wrapper predicates cannot handle type variables. The compiler should have
% been reporting the instance tc(V) as an error anyway, but it wasn't doing
% that check until *after* it had attempted to add the method wrapper.

:- module instance_var_bug.
:- interface.
:- typeclass tc(T) where [ pred p(T::in) is semidet ].
:- implementation.
:- instance tc(V) where [ p(_) :- semidet_true ].
