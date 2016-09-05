
:- module read_dir_regression.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.
:- import_module list.

main(!IO) :-
    open_input(".", FileRes, !IO),
    ( FileRes = ok(File),
	read_line(File, LineRes, !IO),
	( LineRes = ok(_),
	    write_string("ok\n", !IO)
	; LineRes = eof,
	    write_string("eof\n", !IO)
	; LineRes = error(Error),
	    format("read failed: %s\n", [s(error_message(Error))], !IO)
	),
	close_input(File, !IO)
    ; FileRes = error(Error),
	format("open failed: %s\n", [s(error_message(Error))], !IO)
    ).

