%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%
% This test is disabled, because automatic initialization of solver variables
% is no longer supported.
%
%---------------------------------------------------------------------------%

:- module missing_init_pred.

:- interface.

:- solver type t.

:- implementation.

:- solver type t
    where   representation is int,
            initialisation is init,
            ground is ground,
            any is ground.
