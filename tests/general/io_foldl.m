:- module io_foldl.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module require.

main --> 
	io__input_stream_foldl_io(io__write_char, Res),
	{ require(unify(Res, ok), "Error reading file.") }.
