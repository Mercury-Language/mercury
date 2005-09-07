% Test compilation of a file with MS DOS style newlines.
%

:- module dos.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.    % A comment

:- implementation.

:- type t --->
	'escaped functor'.

main(!IO) :-
        io.write_string("multi
                line string\n", !IO),
        io.write_string("should be all on one \
line\n", !IO),
	io.write('escaped fu\
nctor', !IO).
