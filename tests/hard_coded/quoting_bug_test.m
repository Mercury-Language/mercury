:- module quoting_bug_test.
:- interface.
:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module quoting_bug, list.

main -->
	write_token(*), nl,
	write_token(&&), nl,
	write_token(-=), nl,
	write_token(+=), nl,
	write_token(?), nl,
	test([*, &&, -=, +=, ?]).

:- pred write_token(token::in, state::di, state::uo) is det.
write_token(T) --> write(T).

:- pred test(list(token)::in, state::di, state::uo) is det.
test(List) -->
	write_list(List, " ", write_token), nl.

