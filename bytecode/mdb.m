
:- module mdb.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	call_mbi.

:- pred call_mbi(io__state::di, io__state::uo) is det.

:- pragma c_header_code("#include \"mbi.h\"").

:- pragma c_code(call_mbi(IO_in::di, IO_out::uo),
	"BC_call_mbi(); IO_out = IO_in;").

