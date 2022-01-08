%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003, 2005-2006 The University of Melbourne.
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

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module mdb.
:- import_module mdb.browse.
:- import_module mdb.browser_info.
:- import_module mdb.browser_term.

:- import_module assoc_list.
:- import_module list.
:- import_module string.
:- import_module tree234.
:- import_module univ.

main(!IO) :-
    Filename = "/etc/fstab",
    EXIT_FAILURE = 1,
    EXIT_SUCCESS = 0,
    io.open_input(Filename, Result, !IO),
    ( if Result = ok(WordsStream) then
        read_words(WordsStream, Words, !IO),
        io.close_input(WordsStream, !IO),
        assoc_list.from_corresponding_lists(Words, Words, AssocList),
        tree234.assoc_list_to_tree234(AssocList, Tree),
        io.stdin_stream(StdIn, !IO),
        io.stdout_stream(StdOut, !IO),
        browser_info.init_persistent_state(State0),
        io.write_string("list:", !IO),
        io.nl(!IO),
        browse_browser_term_no_modes(plain_term(univ(AssocList)),
            StdIn, StdOut, _, State0, State1, !IO),
        io.write_string("tree:", !IO),
        io.nl(!IO),
        browse_browser_term_no_modes(plain_term(univ(Tree)),
            StdIn, StdOut, _, State1, State2, !IO),
        io.write_string("stream:", !IO),
        io.nl(!IO),
        browse_browser_term_no_modes(plain_term(univ(StdIn)),
            StdIn, StdOut, _, State2, _, !IO),
        io.set_exit_status(EXIT_SUCCESS, !IO)
    else
        io.write_string("Can't open input file.\n", !IO),
        io.set_exit_status(EXIT_FAILURE, !IO)
    ).

:- pred read_words(io.input_stream::in, list(string)::out,
    io.state::di, io.state::uo) is det.

read_words(Stream, Words, !IO) :-
    io.read_word(Stream, Result, !IO),
    ( if Result = ok(Chars) then
        string.from_char_list(Chars, Word),
        read_words(Stream, Rest, !IO),
        Words = [Word | Rest]
    else
        Words = []
    ).

%---------------------------------------------------------------------------%
:- end_module browse_test.
%---------------------------------------------------------------------------%
