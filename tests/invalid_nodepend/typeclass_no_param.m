%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% rotd-2015-11-04 and before(!) did not report an error
% for typeclasses with no parameters.
%
%---------------------------------------------------------------------------%

:- module typeclass_no_param.
:- interface.

:- typeclass foo where [].
