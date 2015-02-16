%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% In rotd-2009-06-03 and earlier the Erlang backend would generate code to
% check two dummy type variables are equal, but those variables might not
% exist.

:- module unify_dummy.
:- interface.

:- type blah
    --->    blah(int, dum).

:- type dum
    --->    dum.

:- pred same_dumdum(blah::in, dum::in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

same_dumdum(blah(_, DumDum), DumDum).

%---------------------------------------------------------------------------%
