	% this is invalid because the interface refers to stuff defined
	% only in the implementation
:- module tricky_assert1.
:- interface.
:- assertion tricky_assert1__local.

:- implementation.
:- pred tricky_assert1__local is semidet.
:- external(tricky_assert1__local/0).
