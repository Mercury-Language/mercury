:- module try_alpha.

:- interface.

:- import_module io.

:- pred main(io:state, io:state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module alpha, list.

:- type token_list == list(token).

:- instance parser_state(token_list) where [
	pred(get_token/3) is uncons
].

:- pred uncons(T::out, list(T)::in, list(T)::out) is semidet.

uncons(X, Xs, Xs0) :- Xs = [X | Xs0].

main --> 
	read_line(Res0),
	(
		{ Res0 = ok(Chars) },
		( { scan(Chars, Toks) } ->
			{ parse(Toks, Res, RemainingToks) },
			write(Res), nl,
			write(RemainingToks), nl
		;
			write_string("scanning error.\n")
		),
		main
	;
		{ Res0 = eof }
	;
		{ Res0 = error(Err) },
		{ io__error_message(Err, Msg) },
		write_string(Msg), nl
	).

