%-----------------------------------------------------------------------------%

:- module expand_terms.

% Emulation of Prolog's expand_term/term_expansion mechanism.
% This program provides pre-processing of Mercury programs,
% using an arbitrary term-to-term translation given by the
% `term_expansion' predicate.

% To use, copy this file to the directory containing your source code, and
% modify the `term_expansion' predicate at the end of this file to provide your
% own term expansion.  Then add
%
%	*.m: expand_terms
%	%.m: %.m.in
%		expand_terms $*.m.in > $*.m
%
% to your Mmake file, and rename your `.m' files with the suffix `.m.in'.

% This source file is hereby placed in the public domain.  -fjh (the author).

%-----------------------------------------------------------------------------%

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module string, list, term, varset, term_io.

main -->
	io__command_line_arguments(Args),
	( { Args = [] } ->
		expand_terms
	;
		expand_terms_file_list(Args)
	).
		
:- pred expand_terms_file_list(list(string)::in, io__state::di, io__state::uo)
	is det.

expand_terms_file_list([]) --> [].
expand_terms_file_list([File | Files]) -->
	expand_terms_file(File),
	expand_terms_file_list(Files).

:- pred expand_terms_file(string::in, io__state::di, io__state::uo) is det.

expand_terms_file(File) -->
	io__open_input(File, Result),
	( { Result = ok(Stream) },
		expand_terms_stream(Stream)
	; { Result = error(Error) },
		io__progname("expand_terms", Progname),
		{ io__error_message(Error, Message) },
		io__write_strings([
			Progname, ": ",
			"error opening file `", File, "' for input:\n\t",
			Message, "\n"
		]),
		io__set_exit_status(1)
	).

:- pred expand_terms_stream(io__input_stream::in, io__state::di, io__state::uo)
	is det.

expand_terms_stream(Stream) -->
	io__set_input_stream(Stream, _OldStream),
	expand_terms.

:- pred expand_terms(io__state::di, io__state::uo) is det.

expand_terms -->
	term_io__read_term(Result),
	expand_terms_2(Result).

:- pred expand_terms_2(read_term::in, io__state::di, io__state::uo)
	is det.

expand_terms_2(Result) -->
	( { Result = term(VarSet0, Term0) },
		{ expand_term(Term0, VarSet0, Term, VarSet) },
		term_io__write_term(VarSet, Term),
		io__write_string(".\n"),
		term_io__read_term(NextResult),
		expand_terms_2(NextResult)
	; { Result = eof }
	; { Result = error(Message, LineNum) },
		io__input_stream_name(StreamName),
		{ string__format("%s:%03d: %s\n", [s(StreamName), i(LineNum),
			s(Message)], FullMessage) },
		io__write_string(FullMessage),
		io__set_exit_status(1)
	).

%-----------------------------------------------------------------------------%

:- pred expand_term(term, varset, term, varset).
:- mode expand_term(in, in, out, out) is det.

expand_term(Term0, VarSet0, Term, VarSet) :-
	( term_expansion(Term0, VarSet0, Term1, VarSet1) ->
		Term = Term1,
		VarSet = VarSet1
	;
		Term = Term0,
		VarSet = VarSet0
	).

%-----------------------------------------------------------------------------%

:- pred term_expansion(term, varset, term, varset).
:- mode term_expansion(in, in, out, out) is semidet.

% Insert your clauses for term_expansion here.
% As a trivial example, here is one which replaces
% `A <=> B' with `A :- B'.

term_expansion(Term0, VarSet, Term, VarSet) :-
	Term0 = term__functor(term__atom("<=>"), [A, B], Context),
	Term = term__functor(term__atom(":-"), [A, B], Context).

%-----------------------------------------------------------------------------%
