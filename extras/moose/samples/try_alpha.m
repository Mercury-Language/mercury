:- module try_alpha.

:- interface.

:- import_module io.

:- pred main(io:state, io:state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module alpha, list.

:- type token_list == list(token).

:- instance parser_state(token_list) where [

	get_token(eof, [],       []),
	get_token(T,   [T | Ts], Ts),

	unget_token(T, Ts) = [T | Ts]
].

main --> 
	read_line(Res0),
	(
		{ Res0 = ok(Chars) },
		(	{ scan(Chars, Toks) },
			{ parse(Res, Toks, RemainingToks) },
			write(Res), nl,
			write(RemainingToks), nl
		),
		main
	;
		{ Res0 = eof }
	;
		{ Res0 = error(Err) },
		{ io__error_message(Err, Msg) },
		write_string(Msg), nl
	).

