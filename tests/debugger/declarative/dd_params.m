% Invoking the commands `params' and `actions' from within the
% declarative debugger was broken in rotd-2006-06-04.
%
:- module dd_params.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

main(!IO) :-
	p(1, X),
	io.write(X, !IO),
	io.nl(!IO).

:- type foo
	--->	bar
	;	baz(int, foo).

:- pred p(int, foo).
:- mode p(in, out) is det.

p(N, baz(N, bar)).

