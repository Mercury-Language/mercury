%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% browse_test - Driver to test the browser.
%
% authors: aet
% stability: low

:- module browse_test.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, string, int, std_util, tree234, assoc_list.
:- import_module mdb, mdb__browse.

main -->
	{ Filename = "/etc/hosts" },
	{ EXIT_FAILURE = 1 },
	{ EXIT_SUCCESS = 0 },
	io__see(Filename, Result),
	( { Result = error(_) } ->
		io__write_string("Can't open input file.\n"),
		io__set_exit_status(EXIT_FAILURE)
	;
		read_words(Words),
		io__seen,
		{ assoc_list__from_corresponding_lists(Words, Words,
			AssocList) },
		{ tree234__assoc_list_to_tree234(AssocList, Tree) },
		io__stdin_stream(StdIn),
		io__stdout_stream(StdOut),
		browse__init_state(State),
		browse__browse(Tree, StdIn, StdOut, State, _),
		io__set_exit_status(EXIT_SUCCESS)
	).

:- pred read_words(list(string), io__state, io__state).
:- mode read_words(out, di, uo) is det.
read_words(Words) -->
	io__read_word(Result),
	( { Result = ok(Chars) } ->
		{ string__from_char_list(Chars, Word) },
		read_words(Rest),
		{ Words = [Word|Rest] }
	;
		{ Words = [] }
	).

%---------------------------------------------------------------------------%
