:- module info.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
	p(X),
	io.write(X, !IO).
	
:- pred p(int::out) is det.

p(1).
