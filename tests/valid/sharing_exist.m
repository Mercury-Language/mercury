% Regression test. Structure sharing widening caused compiler aborts in the
% presence of existential types.
%
% Uncaught Mercury exception:
% Software Error: ctgc.selector.m: Unexpected: get_type_of_node: existential
% type.

:- module sharing_exist.
:- interface.

:- type quux
    --->    some [T] quux(T, string).

:- pred replace(quux::in, T::in, quux::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

replace(Q0, X, Q) :-
    Q0 = quux(_, Y),
    Q = 'new quux'(X, Y).

% vi:ft=mercury:ts=8:sts=4:sw=4:et
