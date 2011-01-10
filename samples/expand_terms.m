%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% Emulation of Prolog's expand_term/term_expansion mechanism.
% This program provides pre-processing of Mercury programs,
% using an arbitrary term-to-term translation given by the
% `term_expansion' predicate.
%
% To use, copy this file to the directory containing your source code, and
% modify the `term_expansion' predicate at the end of this file to provide your
% own term expansion.  Then add
%
%   *.m: expand_terms
%   %.m: %.m.in
%       expand_terms $*.m.in > $*.m
%
% to your Mmake file, and rename your `.m' files with the suffix `.m.in'.
%
% This source file is hereby placed in the public domain.  -fjh (the author).
%
%-----------------------------------------------------------------------------%

:- module expand_terms.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (   
        Args = [],
        expand_terms(!IO)
    ;
        Args = [_ | _],
        expand_terms_file_list(Args, !IO)
    ).
        
:- pred expand_terms_file_list(list(string)::in, io::di, io::uo) is det.

expand_terms_file_list([], !IO).
expand_terms_file_list([File | Files], !IO) :-
    expand_terms_file(File, !IO),
    expand_terms_file_list(Files, !IO).

:- pred expand_terms_file(string::in, io::di, io::uo) is det.

expand_terms_file(File, !IO) :-
    io.open_input(File, Result, !IO),
    ( Result = ok(Stream),
        expand_terms_stream(Stream, !IO)
    ; Result = error(Error),
        io.progname("expand_terms", Progname, !IO),
        io.error_message(Error, Message),
        io.write_strings([
            Progname, ": ",
            "error opening file `", File, "' for input:\n\t",
            Message, "\n"
        ], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred expand_terms_stream(io.input_stream::in, io::di, io::uo) is det.

expand_terms_stream(Stream, !IO) :-
    io.set_input_stream(Stream, _OldStream, !IO),
    expand_terms(!IO).

:- pred expand_terms(io::di, io::uo) is det.

expand_terms(!IO) :-
    term_io.read_term(Result, !IO),
    expand_terms_2(Result, !IO).

:- pred expand_terms_2(read_term::in, io::di, io::uo) is det.

expand_terms_2(Result, !IO) :-
    (
        Result = term(VarSet0, Term0),
        expand_term(Term0, VarSet0, Term, VarSet),
        term_io.write_term(VarSet, Term, !IO),
        io.write_string(".\n", !IO),
        term_io.read_term(NextResult, !IO),
        expand_terms_2(NextResult, !IO)
    ;
        Result = eof
    ;
        Result = error(Message, LineNum),
        io.input_stream_name(StreamName, !IO),
        string.format("%s:%03d: %s\n", [s(StreamName), i(LineNum),
            s(Message)], FullMessage),
        io.write_string(FullMessage, !IO),
        io.set_exit_status(1, !IO)
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

% Insert your clauses for term_expansion here.
% As a trivial example, here is one which replaces
% `A <=> B' with `A :- B'.

:- pred term_expansion(term, varset, term, varset).
:- mode term_expansion(in, in, out, out) is semidet.

term_expansion(Term0, VarSet, Term, VarSet) :-
    Term0 = term.functor(term.atom("<=>"), [A, B], Context),
    Term = term.functor(term.atom(":-"), [A, B], Context).

%-----------------------------------------------------------------------------%
:- end_module expand_terms.
%-----------------------------------------------------------------------------%
