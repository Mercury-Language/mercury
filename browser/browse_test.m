%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003 The University of Melbourne.
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
:- mode main(di, uo) is cc_multi.

:- implementation.

:- import_module mdb.
:- import_module mdb__browse.
:- import_module mdb__browser_info.

:- import_module list, string, int, std_util, tree234, assoc_list.

main -->
	{ Filename = "/etc/fstab" },
	{ EXIT_FAILURE = 1 },
	{ EXIT_SUCCESS = 0 },
	io__open_input(Filename, Result),
	( { Result = ok(WordsStream) } ->
		read_words(WordsStream, Words),
		io__close_input(WordsStream),
		{ assoc_list__from_corresponding_lists(Words, Words,
			AssocList) },
		{ tree234__assoc_list_to_tree234(AssocList, Tree) },
		io__stdin_stream(StdIn),
		io__stdout_stream(StdOut),
		{ browser_info__init_persistent_state(State0) },
		io__write_string("list:"),
		io__nl,
		browse__browse(AssocList, StdIn, StdOut, _, State0, State1),
		io__write_string("tree:"),
		io__nl,
		browse__browse(Tree, StdIn, StdOut, _, State1, State2),
		io__write_string("stream:"),
		io__nl,
		browse__browse(StdIn, StdIn, StdOut, _, State2, _),
		io__set_exit_status(EXIT_SUCCESS)
	;
		io__write_string("Can't open input file.\n"),
		io__set_exit_status(EXIT_FAILURE)
	).

:- pred read_words(io__input_stream::in, list(string)::out,
	io__state::di, io__state::uo) is det.

read_words(Stream, Words) -->
	io__read_word(Stream, Result),
	( { Result = ok(Chars) } ->
		{ string__from_char_list(Chars, Word) },
		read_words(Stream, Rest),
		{ Words = [Word | Rest] }
	;
		{ Words = [] }
	).

%---------------------------------------------------------------------------%
