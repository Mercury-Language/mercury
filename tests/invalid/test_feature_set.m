%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_feature_set.
:- interface.

:- type foo
    --->    foo.

:- implementation.

:- pragma require_feature_set([trailing, single_prec_float, concurrency, memo]).
