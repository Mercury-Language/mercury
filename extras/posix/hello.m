:- module hello.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module posix, posix__open, posix__write, text.
:- import_module list, string.

main -->
	open("/dev/tty", [wronly], Res0),
	(
		{ Res0 = ok(Fd) },
		{ Str = "hello world.\n" },
		{ length(Str, Len) },
		write(Fd, Len, text(Str), Res1),
		(
			{ Res1 = ok(NWritten) },
			( { NWritten \= Len } ->
				% We didn't write all of it!
				write("failed to write it all\n")
			;
				[]
			)
		;
			{ Res1 = error(Err) },
			write(Err), nl
		)
	;
		{ Res0 = error(Err) },
		write(Err), nl
	).

