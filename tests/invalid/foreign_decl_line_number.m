% Check that we report the correct line number for the error in
% foreign_decl.
:- module foreign_decl_line_number.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main -->
	io__write_string("Hello\n").

:- pragma foreign_decl("C", "
	/* Missing ; in struct def */
typedef struct {
	int	missing_semicolon_here
	int	x;
} bug;
").
	
