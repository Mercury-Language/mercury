%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case should produce _no_ warnings. One version of the compiler
% complained that the recursive call to return_me inside the lambda
% would cause infinite recursion. -- 28/7/1997 bromage
%
%---------------------------------------------------------------------------%

:- module inf_recursion_lambda.
:- interface.

:- type closure
    --->    closure((func) = closure).
:- inst closure == bound(closure((func) = out(closure) is det)).

:- func return_me = (closure :: out(closure)) is det.

:- implementation.

return_me = closure((func) = return_me).
