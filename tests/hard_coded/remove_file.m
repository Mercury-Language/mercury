:- module remove_file.
:- interface.
:- use_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	io__tmpnam(Name),
%%%%%%%	io__print("Temp file name = "), io__print(Name), io__nl,
	io__tell(Name, TellResult),
	( { TellResult = io__ok },
		io__print("Just testing"), io__nl,
		io__told,
		io__remove_file(Name, RemoveResult),
		( { RemoveResult = io__ok },
			io__see(Name, SeeResult),
			( { SeeResult = io__ok } ->
				io__print("Remove didn't remove file\n"),
				io__set_exit_status(1)
			;
				io__print("Test passed\n")
			)
		; { RemoveResult = io__error(RemoveError) },
			io__print("Remove failed: "),
			{ io__error_message(RemoveError, RemoveErrorMsg) },
			io__print(RemoveErrorMsg),
			io__nl,
			io__set_exit_status(1)
		),
		io__remove_file(Name, RemoveAgainResult),
		( { RemoveAgainResult = io__ok },
			io__print("Second remove didn't report failure\n"),
			io__set_exit_status(1)
		; { RemoveAgainResult = io__error(RemoveAgainError) },
			io__print("Second remove failed, as expected: "),
			{ io__error_message(RemoveAgainError,
				RemoveAgainErrorMsg) },
			io__print(RemoveAgainErrorMsg),
			io__nl
		)
	; { TellResult = io__error(TellError) },
		io__print("Tell failed: "),
		{ io__error_message(TellError, TellErrorMsg) },
		io__print(TellErrorMsg),
		io__nl,
		io__set_exit_status(1)
	).

