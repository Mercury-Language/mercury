%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_no_arrow.
:- interface.

:- type t
    ;   f1      % This ";" should have been "--->".
    ;   f2
    ;   f3.

    % Don't warn about the module not exporting anything.
:- type shutup
    --->    shutup.
