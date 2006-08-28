:- module runtime_opt.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

main(!IO) :-
	io.write_string("Hello world (with non-standard options).\n", !IO).

