% vim: ft=mercury ts=8 sts=4 sw=4 et

% We were generating table reset/statistics predicates for `:- pragma memo's
% with no attributes.  This happened to cause the compiler to abort when
% structure sharing was enabled, but is otherwise unrelated to that analysis.

:- module table_no_attr.
:- interface.

:- pred baboon(int::in, int::out) is det.

:- implementation.

:- pragma memo(baboon/2).

baboon(X, X).
