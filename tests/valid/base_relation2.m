%-----------------------------------------------------------------------------%
% File: base_relation2.m
% Author: stayl
%
% Test calling an imported Aditi procedure from a module containing
% no Aditi predicates. The compiler of 6/4/2000 aborted on this
% test case because the `aditi' marker was not removed from the
% imported predicate, resulting in arg_infos not being computed for it.
%-----------------------------------------------------------------------------%
:- module base_relation2.

:- interface.

:- import_module aditi.

:- pred query(aditi__state::aditi_mui, int::in, int::out) is nondet.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module base_relation.

query(DB, X, Y) :-
	base(DB, X, Y).
