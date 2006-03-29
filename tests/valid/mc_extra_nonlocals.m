
% This is a regression test. Some nonlocals sets in the compare predicate
% generated for the polymorphic type below contained variables that
% didn't appear in the goal. Constraints based mode analysis using the
% propagation solver was incorrectly assuming that the goal consumed the
% variable, and analysis was failing because of this.
%
% Constraints based mode analysis has been changed to ignore nonlocals
% that don't appear in the goal, however the compare predicate for
% this type may still have inaccurate nonlocals sets at the time of mode
% analysis.

:- module mc_extra_nonlocals.
:- interface.

:- type polymorphic(K)
       --->    empty
       ;       node(polymorphic(K)).

