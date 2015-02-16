%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module conflicting_fs.
:- interface.

:- type foo
    --->    foo.

:- implementation.

:- pragma require_feature_set([single_prec_float, double_prec_float]).
:- pragma require_feature_set([trailing, parallel_conj]).
