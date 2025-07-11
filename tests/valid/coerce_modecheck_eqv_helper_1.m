%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_modecheck_eqv_helper_1.
:- interface.

% We really need foo(T) to be abstractly exported from this module,
% with the type equivalence defined in the implementation section.
% Then, we can run:
%
%   mmc --make-int coerce_modecheck_eqv_helper_1.m
%   mmc --errorcheck-only coerce_modecheck_eqv.m
%
% If we try to include and run the test as part of the test suite,
% the compiler reports:
%
%   Error: the type `foo'/1 is a polymorphic equivalence type with a
%   monomorphic definition. The export of such types as abstract types
%   is not yet implemented.
%
% Therefore, we export the type definition in the interface section,
% but it won't actually test the handling of an unexpanded equivalence type
% in the modechecker for coercions.
:- type foo(T).
:- type foo(T) == int.  % should be in the implementation section

:- type bar.

:- implementation.

:- type bar == int.
