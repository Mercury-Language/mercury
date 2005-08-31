:- module trigraphs.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	io.write_string("??(  ??)  ??<  ??>  ??=  ??/n  ??'  ??!  ??-", !IO),
	nl(!IO).
