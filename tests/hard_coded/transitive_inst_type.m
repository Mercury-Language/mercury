:- module transitive_inst_type.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module transitive_inst_type2.

main -->
	{ new_sequence(Seq) },
	{ sequence_length(Seq, Size) },
	io__write_int(Size),
	io__nl.
