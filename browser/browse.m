%---------------------------------------------------------------------------%
% Copyright (C) 1998-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% browse - implements a very simple term browser.
% There are a number of features that haven't been incorporated:
%	- Scripting language that allows precise control over
%	  how types are printed.
%	- User preferences, which use the scripting language
%	  to allow user control beyond the provided defaults.
%	- Node expansion and contraction in the style of
%	  Windows Explorer.
%
% authors: aet
% stability: low

:- module mdb__browse.

:- interface.

:- import_module mdb__browser_info.
:- import_module io, bool, std_util, list.

	% The interactive term browser.  The caller type will be `browse', and
	% the default format for the `browse' caller type will be used.
	%
:- pred browse__browse(T::in, io__input_stream::in,
	io__output_stream::in, maybe(list(dir))::out,
	browser_persistent_state::in, browser_persistent_state::out,
	io__state::di, io__state::uo) is cc_multi.

	% As above, except that the supplied format will override the default.
	%
:- pred browse__browse_format(T::in, io__input_stream::in,
	io__output_stream::in, portray_format::in,
	browser_persistent_state::in, browser_persistent_state::out,
	io__state::di, io__state::uo) is cc_multi.

	% A version of browse__browse that works on synthetic terms
	% in the sense of browser_info:browser_term.
	%
:- pred browse__browse_synthetic(string::in, list(univ)::in, bool::in,
	io__input_stream::in, io__output_stream::in, maybe(list(dir))::out,
	browser_persistent_state::in, browser_persistent_state::out,
	io__state::di, io__state::uo) is cc_multi.

	% A version of browse__browse_format that works on synthetic terms
	% in the sense of browser_info:browser_term.
	%
:- pred browse__browse_format_synthetic(string::in, list(univ)::in, bool::in,
	io__input_stream::in, io__output_stream::in, portray_format::in,
	browser_persistent_state::in, browser_persistent_state::out,
	io__state::di, io__state::uo) is cc_multi.

	% The browser interface for the external debugger.  The caller type
	% will be `browse', and the default format will be used.
	%
:- pred browse__browse_external(T::in, io__input_stream::in,
	io__output_stream::in,
	browser_persistent_state::in, browser_persistent_state::out,
	io__state::di, io__state::uo) is cc_multi.

	% The non-interactive term browser.  The caller type should be either
	% `print' or `print_all'.  The default portray format for that
	% caller type is used.
	%
:- pred browse__print(T::in, io__output_stream::in, browse_caller_type::in,
	browser_persistent_state::in, io__state::di, io__state::uo)
	is cc_multi.

	% A version of browse__print that works on synthetic terms
	% in the sense of browser_info:browser_term.
	%
:- pred browse__print_synthetic(string::in, list(univ)::in, bool::in,
	io__output_stream::in, browse_caller_type::in,
	browser_persistent_state::in, io__state::di, io__state::uo)
	is cc_multi.

	% As above, except that the supplied format will override the default.
	%
:- pred browse__print_format(T::in, io__output_stream::in,
	browse_caller_type::in, portray_format::in,
	browser_persistent_state::in, io__state::di, io__state::uo)
	is cc_multi.

	% A version of browse__print_format that works on synthetic terms
	% in the sense of browser_info:browser_term.
	%
:- pred browse__print_format_synthetic(string::in, list(univ)::in, bool::in,
	io__output_stream::in, browse_caller_type::in, portray_format::in,
	browser_persistent_state::in, io__state::di, io__state::uo)
	is cc_multi.

	% Estimate the total term size, in characters,
	% We count the number of characters in the functor,
	% plus two characters for each argument: "(" and ")"
	% for the first, and ", " for each of the rest,
	% plus the sizes of the arguments themselves.
	% This is only approximate since it doesn't take into
	% account all the special cases such as operators.
	%
	% This predicate returns not the estimated total term size,
	% but the difference between the given maximum size the caller
	% is interested in and the estimated total term size.
	% This difference is positive if the term is smaller than the
	% maximum and negative if it is bigger. If the difference is
	% negative, term_size_left_from_max will return a negative difference
	% but the value will usually not be accurate, since in such cases
	% by definition the caller is not interested in the accurate value.
:- pred term_size_left_from_max(univ::in, int::in, int::out) is cc_multi.

:- pred browser_term_size_left_from_max(browser_term::in,
	int::in, int::out) is cc_multi.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module mdb__parse, mdb__util, mdb__frame, mdb__sized_pretty.
:- import_module string, int, char, std_util.
:- import_module parser, require, pprint, deconstruct.

%---------------------------------------------------------------------------%
%
% We export these predicates to C for use by the tracer:
% they are used in trace/mercury_trace_browser.c.
%

:- pragma export(browse__browse(in, in, in, out, in, out, di, uo),
	"ML_BROWSE_browse").
:- pragma export(browse__browse_format(in, in, in, in, in, out, di, uo),
	"ML_BROWSE_browse_format").
:- pragma export(browse__browse_synthetic(in, in, in, in, in, out,
	in, out, di, uo), "ML_BROWSE_browse_synthetic").
:- pragma export(browse__browse_format_synthetic(in, in, in, in, in, in,
	in, out, di, uo), "ML_BROWSE_browse_format_synthetic").
:- pragma export(browse__browse_external(in, in, in, in, out, di, uo),
	"ML_BROWSE_browse_external").
:- pragma export(browse__print(in, in, in, in, di, uo),
	"ML_BROWSE_print").
:- pragma export(browse__print_format(in, in, in, in, in, di, uo),
	"ML_BROWSE_print_format").
:- pragma export(browse__print_synthetic(in, in, in, in, in, in, di, uo),
	"ML_BROWSE_print_synthetic").
:- pragma export(browse__print_format_synthetic(in, in, in, in, in, in, in,
	di, uo), "ML_BROWSE_print_format_synthetic").

%---------------------------------------------------------------------------%
% If the term browser is called from the internal debugger, input is
% done via a call to the readline library (if available), using streams
% MR_mdb_in and MR_mdb_out.  If it is called from the external debugger,
% Input/Output are done via MR_debugger_socket_in/MR_debugger_socket_out. 
% In the latter case we need to output terms; their type is 
% term_browser_response.


:- type term_browser_response 
	--->	browser_str(string)
	;	browser_int(int)
	;	browser_nl
	;	browser_end_command
	;	browser_quit.

:- type debugger 
	--->	internal
	;	external.

%---------------------------------------------------------------------------%
%
% Non-interactive display
%

browse__print(Term, OutputStream, Caller, State) -->
	browse__print_common(plain_term(univ(Term)), OutputStream,
		Caller, no, State).

browse__print_format(Term, OutputStream, Caller, Format, State) -->
	browse__print_common(plain_term(univ(Term)), OutputStream,
		Caller, yes(Format), State).

browse__print_synthetic(FunctorString, Args, IsFunc, OutputStream,
		Caller, State) -->
	{ synthetic_term_to_browser_term(FunctorString, Args, IsFunc,
		BrowserTerm) },
	browse__print_common(BrowserTerm, OutputStream, Caller, no, State).

browse__print_format_synthetic(FunctorString, Args, IsFunc, OutputStream,
		Caller, Format, State) -->
	{ synthetic_term_to_browser_term(FunctorString, Args, IsFunc,
		BrowserTerm) },
	browse__print_common(BrowserTerm, OutputStream,
		Caller, yes(Format), State).

:- pred browse__print_common(browser_term::in, io__output_stream::in,
	browse_caller_type::in, maybe(portray_format)::in,
	browser_persistent_state::in, io__state::di, io__state::uo)
	is cc_multi.

browse__print_common(BrowserTerm, OutputStream, Caller, MaybeFormat, State) -->
	{ Info = browser_info__init(BrowserTerm, Caller, MaybeFormat, State) },
	io__set_output_stream(OutputStream, OldStream),
	{ browser_info__get_format(Info, Caller, MaybeFormat, Format) },
	%
	% For plain terms, we assume that the variable name has been printed
	% on the first part of the line.  If the format is something other than
	% `flat', then we need to start on the next line.
	%
	(
		{ BrowserTerm = plain_term(_) },
		{ Format \= flat }
	->
		io__nl
	;
		[]
	),
	portray(internal, Caller, no, Info),
	io__set_output_stream(OldStream, _).

%---------------------------------------------------------------------------%
%
% Interactive display
%

browse__browse(Object, InputStream, OutputStream, MaybeMark, State0, State) -->
	browse_common(internal, plain_term(univ(Object)),
		InputStream, OutputStream, no, MaybeMark, State0, State).

browse__browse_format(Object, InputStream, OutputStream, Format,
		State0, State) -->
	browse_common(internal, plain_term(univ(Object)),
		InputStream, OutputStream, yes(Format), _, State0, State).

browse__browse_synthetic(FunctorString, Args, IsFunc,
		InputStream, OutputStream, MaybeMark, State0, State) -->
	{ synthetic_term_to_browser_term(FunctorString, Args, IsFunc,
		BrowserTerm) },
	browse_common(internal, BrowserTerm,
		InputStream, OutputStream, no, MaybeMark, State0, State).

browse__browse_format_synthetic(FunctorString, Args, IsFunc,
		InputStream, OutputStream, Format, State0, State) -->
	{ synthetic_term_to_browser_term(FunctorString, Args, IsFunc,
		BrowserTerm) },
	browse_common(internal, BrowserTerm,
		InputStream, OutputStream, yes(Format), _, State0, State).

browse__browse_external(Object, InputStream, OutputStream, State0, State) -->
	browse_common(external, plain_term(univ(Object)),
		InputStream, OutputStream, no, _, State0, State).

:- pred browse_common(debugger::in, browser_term::in, io__input_stream::in,
	io__output_stream::in, maybe(portray_format)::in,
	maybe(list(dir))::out, browser_persistent_state::in,
	browser_persistent_state::out, io__state::di, io__state::uo)
	is cc_multi.

browse_common(Debugger, Object, InputStream, OutputStream, MaybeFormat,
		MaybeMark, State0, State) -->
	{ Info0 = browser_info__init(Object, browse, MaybeFormat, State0) },
	io__set_input_stream(InputStream, OldInputStream),
	io__set_output_stream(OutputStream, OldOutputStream),
	% startup_message,
	browse_main_loop(Debugger, Info0, Info),
	io__set_input_stream(OldInputStream, _),
	io__set_output_stream(OldOutputStream, _),
	{ MaybeMark = Info ^ maybe_mark },
	{ State = Info ^ state }.

% This predicate converts synthetic terms from the representation used in the
% trace directory (as a list of arguments, the last of which represents the
% return value for function calls) to the representation used in the browser
% directory, in which a function call's return value is stored separately from
% the other arguments.
%
% The reason why the trace directory does not use the latter representation
% is that it would require C code to construct values of type maybe(T).

:- pred synthetic_term_to_browser_term(string::in, list(univ)::in, bool::in,
	browser_term::out) is det.

synthetic_term_to_browser_term(FunctorString, Args, IsFunc, BrowserTerm) :-
	(
		IsFunc = no,
		BrowserTerm = synthetic_term(FunctorString, Args, no)
	;
		IsFunc = yes,
		list__split_last_det(Args, FuncArgs, Return),
		BrowserTerm = synthetic_term(FunctorString, FuncArgs,
			yes(Return))
	).

:- pred browse_main_loop(debugger::in, browser_info::in, browser_info::out, 
	io__state::di, io__state::uo) is cc_multi.

browse_main_loop(Debugger, Info0, Info) -->
	(
		{ Debugger = internal },
		{ prompt(Prompt) },
		parse__read_command(Prompt, Command)
	;
		{ Debugger = external },
		parse__read_command_external(Command)
	),
	run_command(Debugger, Command, Quit, Info0, Info1),
	(
		{ Quit = yes },
		% write_string_debugger(Debugger, "quitting...\n")
		(
			{ Debugger = external },
			send_term_to_socket(browser_quit)
		;
			{ Debugger = internal }
		),
		{ Info = Info1 }
	;
		{ Quit = no },
		browse_main_loop(Debugger, Info1, Info)
	).

:- pred startup_message(debugger::in, io__state::di, io__state::uo) is det.

startup_message(Debugger) -->
	write_string_debugger(Debugger, "-- Simple Mercury Term Browser.\n"),
	write_string_debugger(Debugger, "-- Type \"help\" for help.\n\n").

:- pred prompt(string::out) is det.

prompt("browser> ").

:- pred run_command(debugger::in, command::in, bool::out, browser_info::in,
	browser_info::out, io__state::di, io__state::uo) is cc_multi.

run_command(Debugger, Command, Quit, Info0, Info) -->
	% XXX The commands `set', `ls' and `print' should allow the format
	% to be specified by an option.  In each case we instead pass `no' to
	% the respective handler.
	( { Command = empty },
		{ Quit = no },
		{ Info = Info0 }
	; { Command = unknown },
		write_string_debugger(Debugger, 
			"Error: unknown command or syntax error.\n"),
		write_string_debugger(Debugger, "Type \"help\" for help.\n"),
		{ Quit = no },
		{ Info = Info0 }
	; { Command = help },
		help(Debugger),
		{ Quit = no },
		{ Info = Info0 }
	; { Command = set },
		show_settings(Debugger, Info0, no),
		{ Quit = no },
		{ Info = Info0 }
	; { Command = set(Setting) },
		{ set_browse_param(Info0 ^ caller_type, Setting,
			Info0, Info) },
		{ Quit = no }
	; { Command = ls },
		portray(Debugger, browse, no, Info0),
		{ Quit = no },
		{ Info = Info0 }
	; { Command = ls(Path) },
		portray_path(Debugger, browse, no, Info0, Path),
		{ Quit = no },
		{ Info = Info0 }
	; { Command = cd },
		{ set_path(root_rel([]), Info0, Info) },
		{ Quit = no }
	; { Command = cd(Path) },
		{ change_dir(Info0 ^ dirs, Path, NewPwd) },
		( { deref_subterm(Info0 ^ term, NewPwd, _SubUniv) } ->
			{ Info = Info0 ^ dirs := NewPwd }
		;
			write_string_debugger(Debugger, 
				"error: cannot change to subterm\n"),
			{ Info = Info0 }
		),
		{ Quit = no }
	; { Command = print },
		portray(Debugger, print, no, Info0),
		{ Quit = no },
		{ Info = Info0 }
	; { Command = pwd },
		write_path(Debugger, Info0 ^ dirs),
		nl_debugger(Debugger),
		{ Quit = no },
		{ Info = Info0 }
	; { Command = mark },
		{ Quit = yes },
		{ Info = Info0 ^ maybe_mark := yes(Info0 ^ dirs) }
	; { Command = mark(Path) },
		{ change_dir(Info0 ^ dirs, Path, NewPwd) },
		( { deref_subterm(Info0 ^ term, NewPwd, _SubUniv) } ->
			{ Quit = yes },
			{ Info = Info0 ^ maybe_mark := yes(NewPwd) }
		;
			write_string_debugger(Debugger, 
				"error: cannot mark subterm\n"),
			{ Quit = no },
			{ Info = Info0 }
		)
	; { Command = quit },
		{ Quit = yes },
		{ Info = Info0 }
	; { Command = display },
		write_string_debugger(Debugger,
				"command not yet implemented\n"),
		{ Quit = no },
		{ Info = Info0 }
	; { Command = write },
		write_string_debugger(Debugger,
				"command not yet implemented\n"),
		{ Quit = no },
		{ Info = Info0 }
	),
	( { Debugger = external } ->
		send_term_to_socket(browser_end_command)
	;
		{ true }
	).

:- pred set_browse_param(browse_caller_type::in, setting::in,
	browser_info::in, browser_info::out) is det.

set_browse_param(CallerType, Setting, Info0, Info) :-
	%
	% XXX We can't yet give options to the `set' command.
	%
	No = bool__no,
	browser_info__set_param(yes(CallerType), No, No, No, No, Setting, 
			Info0 ^ state, NewState),
	Info = Info0 ^ state := NewState.

:- pred help(debugger::in, io__state::di, io__state::uo) is det.
help(Debugger) -->
	{ string__append_list([
"Commands are:\n",
"\tls [path]      -- list subterm (expanded)\n",
"\tcd [path]      -- cd current subterm (default is root)\n",
"\thelp           -- show this help message\n",
"\tset var value  -- set a setting\n",
"\tset            -- show settings\n",
"\tprint          -- show single line representation of current term\n",
"\tquit           -- quit browser\n",
"\tmark [path]    -- mark the given subterm (default is current) and quit\n",
"SICStus Prolog style commands are:\n",
"\tp              -- print\n",
"\t< n            -- set depth\n",
"\t^ [path]       -- cd [path] (default is root)\n",
"\t?              -- help\n",
"\th              -- help\n",
"\n",
"-- settings:\n",
"--    size; depth; path; format (flat raw_pretty verbose pretty); width; ",
"lines\n",
"--    Paths can be Unix-style or SICStus-style: /2/3/1 or ^2^3^1\n",
"\n"],
		HelpMessage) },
	write_string_debugger(Debugger, HelpMessage).

%---------------------------------------------------------------------------%
%
% Various pretty-print routines
%

:- pred portray(debugger::in, browse_caller_type::in,
	maybe(portray_format)::in, browser_info::in,
	io__state::di, io__state::uo) is cc_multi.

portray(Debugger, Caller, MaybeFormat, Info) -->
	{ browser_info__get_format(Info, Caller, MaybeFormat, Format) },
	{ browser_info__get_format_params(Info, Caller, Format, Params) },
	(
		{ deref_subterm(Info ^ term, Info ^ dirs, SubUniv) }
	->
		(
			{ Format = flat },
			portray_flat(Debugger, SubUniv, Params)
		;
			{ Format = raw_pretty },
			portray_raw_pretty(Debugger, SubUniv, Params)
		;
			{ Format = verbose },
			portray_verbose(Debugger, SubUniv, Params)
		;
			{ Format = pretty },
			portray_pretty(Debugger, SubUniv, Params)
		)
	;
		write_string_debugger(Debugger, "error: no such subterm")
	),
	nl_debugger(Debugger).

:- pred portray_path(debugger::in, browse_caller_type::in,
	maybe(portray_format)::in, browser_info::in, path::in,
	io__state::di, io__state::uo) is cc_multi.

portray_path(Debugger, Caller, MaybeFormat, Info0, Path) -->
	{ set_path(Path, Info0, Info) },
	portray(Debugger, Caller, MaybeFormat, Info).

:- pred portray_flat(debugger::in, browser_term::in, format_params::in,
	io__state::di, io__state::uo) is cc_multi.

portray_flat(Debugger, BrowserTerm, Params) -->
	%
	% io__write handles the special cases such as lists,
	% operators, etc. better, so we prefer to use it if we
	% can.  However, io__write doesn't have a depth or size limit,
	% so we need to check the size first; if the term is small
	% enough, we use io__write (actually io__write_univ), otherwise
	% we use term_to_string/4.
	%
	% XXX this ignores the maximum number of lines
	%
	{ browser_term_size_left_from_max(BrowserTerm, max_print_size,
		RemainingSize) },
	( { RemainingSize >= 0 } ->
		portray_flat_write_browser_term(BrowserTerm)
	;
		{ browser_term_to_string(BrowserTerm, Params ^ size,
			Params ^ depth, Str) },
		write_string_debugger(Debugger, Str)
	).

:- pred portray_flat_write_browser_term(browser_term::in,
	io__state::di, io__state::uo) is cc_multi.

portray_flat_write_browser_term(plain_term(Univ)) -->
	io__current_output_stream(Stream),
	io__write_univ(Stream, include_details_cc, Univ).
portray_flat_write_browser_term(synthetic_term(Functor, Args, MaybeReturn)) -->
	io__write_string(Functor),
	io__current_output_stream(Stream),
	( { Args = [] } ->
		[]
	;
		io__write_string("("),
		io__write_list(Args, ", ", pred(U::in, di, uo) is cc_multi -->
			io__write_univ(Stream, include_details_cc, U)),
		io__write_string(")")
	),
	(
		{ MaybeReturn = yes(Return) },
		io__write_string(" = "),
		io__write_univ(Stream, include_details_cc, Return)
	;
		{ MaybeReturn = no }
	).

:- pred portray_verbose(debugger::in, browser_term::in, format_params::in,
	io__state::di, io__state::uo) is cc_multi.

portray_verbose(Debugger, BrowserTerm, Params) -->
	{ browser_term_to_string_verbose(BrowserTerm, Params ^ size,
		Params ^ depth, Params ^ width, Params ^ lines, Str) },
	write_string_debugger(Debugger, Str).

:- pred portray_raw_pretty(debugger::in, browser_term::in, format_params::in,
	io__state::di, io__state::uo) is det.

portray_raw_pretty(Debugger, BrowserTerm, Params) -->
	{ browser_term_to_string_raw_pretty(BrowserTerm, Params ^ width, 
		Params ^ depth, Str) },
	write_string_debugger(Debugger, Str).

:- pred portray_pretty(debugger::in, browser_term::in, format_params::in,
	io__state::di, io__state::uo) is cc_multi.

portray_pretty(Debugger, BrowserTerm, Params) -->
	{ sized_pretty__browser_term_to_string_line(BrowserTerm,
		Params ^ width, Params ^ lines, Str) },
	write_string_debugger(Debugger, Str).

	% The maximum estimated size for which we use `io__write'.
:- func max_print_size = int.

max_print_size = 60.

term_size_left_from_max(Univ, MaxSize, RemainingSize) :-
	(
		MaxSize < 0
	->
		RemainingSize = MaxSize
	;
		limited_deconstruct_cc(univ_value(Univ), MaxSize,
			Functor, Arity, Args)
	->
		string__length(Functor, FunctorSize),
		% "()", plus Arity-1 times ", "
		PrincipalSize = FunctorSize + Arity * 2,
		MaxArgsSize = MaxSize - PrincipalSize,
		list__foldl(term_size_left_from_max,
			Args, MaxArgsSize, RemainingSize)
	;
		RemainingSize = -1
	).

browser_term_size_left_from_max(BrowserTerm, MaxSize, RemainingSize) :-
	(
		BrowserTerm = plain_term(Univ),
		term_size_left_from_max(Univ, MaxSize, RemainingSize)
	;
		BrowserTerm = synthetic_term(Functor, Args, MaybeReturn),
		string__length(Functor, FunctorSize),
		list__length(Args, Arity),
		(
			MaybeReturn = yes(_),
			% "()", " = ", plus Arity-1 times ", "
			PrincipalSize = FunctorSize + Arity * 2 + 3
		;
			MaybeReturn = no,
			% "()", plus Arity-1 times ", "
			PrincipalSize = FunctorSize + Arity * 2
		),
		MaxArgsSize = MaxSize - PrincipalSize,
		list__foldl(term_size_left_from_max,
			Args, MaxArgsSize, RemainingSize)
	).

%---------------------------------------------------------------------------%
%
% Single-line representation of a term.
%

:- pred browser_term_to_string(browser_term::in, int::in, int::in,
	string::out) is cc_multi.

browser_term_to_string(BrowserTerm, MaxSize, MaxDepth, Str) :-
	CurSize = 0,
	CurDepth = 0,
	browser_term_to_string_2(BrowserTerm, MaxSize, CurSize, _NewSize,
		MaxDepth, CurDepth, Str).

	% Note: When the size limit is reached, we simply display
	% further subterms compressed. We don't just stop printing.
	% XXX: Is this reasonable?
:- pred browser_term_to_string_2(browser_term::in, int::in, int::in, int::out,
	int::in, int::in, string::out) is cc_multi.

browser_term_to_string_2(BrowserTerm, MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, Str) :-
	(
		CurSize < MaxSize,
		CurDepth < MaxDepth,
		limited_deconstruct_browser_term_cc(BrowserTerm, MaxSize,
			Functor, _Arity, Args, MaybeReturn)
	->
		CurSize1 = CurSize + 1,
		CurDepth1 = CurDepth + 1,
		args_to_string_list(Args, MaxSize, CurSize1, NewSize1,
			MaxDepth, CurDepth1, ArgStrs),
		BracketedArgsStr = bracket_string_list(ArgStrs),
		(
			MaybeReturn = yes(Return),
			browser_term_to_string_2(plain_term(Return), MaxSize,
				NewSize1, NewSize, MaxDepth, CurDepth1,
				ReturnStr),
			string__append_list([Functor, BracketedArgsStr,
				" = ", ReturnStr], Str)
		;
			MaybeReturn = no,
			NewSize = NewSize1,
			string__append_list([Functor, BracketedArgsStr], Str)
		)
	;
		% Str = "...",
		browser_term_compress(BrowserTerm, Str),
		NewSize = CurSize
	).

:- pred args_to_string_list(list(univ)::in, int::in, int::in, int::out,
	int::in, int::in, list(string)::out) is cc_multi.

args_to_string_list([], _MaxSize, CurSize, NewSize,
		_MaxDepth, _CurDepth, Strs) :-
	Strs = [],
	NewSize = CurSize.
args_to_string_list([Univ | Univs], MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, Strs) :-
	browser_term_to_string_2(plain_term(Univ), MaxSize, CurSize, NewSize1,
		MaxDepth, CurDepth, Str),
	args_to_string_list(Univs, MaxSize, NewSize1, NewSize,
		MaxDepth, CurDepth, RestStrs),
	Strs = [Str | RestStrs].

:- func bracket_string_list(list(string)) = string.

bracket_string_list(Args) = Str :-
	( Args = [] ->
		Str = ""
	;
		string__append_list(["(", comma_string_list(Args), ")"], Str)
	).

:- func comma_string_list(list(string)) = string.

comma_string_list(Args) = Str :-
	(
		Args = [],
		Str = ""
	;
		Args = [S],
		Str = S
	;
		Args = [S1, S2 | Ss],
		Rest = comma_string_list([S2 | Ss]),
		string__append_list([S1, ", ", Rest], Str)
	).

:- pred browser_term_compress(browser_term::in, string::out) is cc_multi.

browser_term_compress(BrowserTerm, Str) :-
	functor_browser_term_cc(BrowserTerm, Functor, Arity, IsFunc),
	( Arity = 0 ->
		Str = Functor
	;
		int_to_string(Arity, ArityStr),
		(
			IsFunc = yes,
			append_list([Functor, "/", ArityStr, "+1"], Str)
		;
			IsFunc = no,
			append_list([Functor, "/", ArityStr], Str)
		)
	).
	
%---------------------------------------------------------------------------%
%
% Print using the pretty printer from the standard library.
% XXX the size of the term is not limited---the pretty printer
% provides no way of doing this.
%

:- pred browser_term_to_string_raw_pretty(browser_term::in, int::in, int::in,
	string::out) is det.

browser_term_to_string_raw_pretty(plain_term(Univ), Width, MaxDepth, Str) :-
	Value = univ_value(Univ),
	Doc = to_doc(MaxDepth, Value),
	Str = to_string(Width, Doc).
browser_term_to_string_raw_pretty(synthetic_term(Functor, Args, MaybeReturn),
		Width, MaxDepth, Str) :-
	Doc = synthetic_term_to_doc(MaxDepth, Functor, Args, MaybeReturn),
	Str = to_string(Width, Doc).

%---------------------------------------------------------------------------%
%
% Verbose printing. Tree layout with numbered branches.
% Numbering makes it easier to change to subterms.
%

:- pred browser_term_to_string_verbose(browser_term::in, int::in, int::in,
	int::in, int::in, string::out) is cc_multi.

browser_term_to_string_verbose(BrowserTerm, MaxSize, MaxDepth, X, Y, Str) :-
	CurSize = 0,
	CurDepth = 0,
	browser_term_to_string_verbose_2(BrowserTerm, MaxSize, CurSize,
		_NewSize, MaxDepth, CurDepth, Frame),
	frame__clip(X-Y, Frame, ClippedFrame),
	unlines(ClippedFrame, Str).

:- pred browser_term_to_string_verbose_2(browser_term::in, int::in, int::in,
	int::out, int::in, int::in, frame::out) is cc_multi.

browser_term_to_string_verbose_2(BrowserTerm, MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, Frame) :-
	(
		CurSize < MaxSize,
		CurDepth < MaxDepth,
		limited_deconstruct_browser_term_cc(BrowserTerm, MaxSize,
			Functor, _Arity, Args0, MaybeReturn)
	->
		% XXX we should consider formatting function terms differently.
		(
			MaybeReturn = yes(Return),
			list__append(Args0, [Return], Args)
		;
			MaybeReturn = no,
			Args = Args0
		),
		CurSize1 is CurSize + 1,
		CurDepth1 is CurDepth + 1,
		ArgNum = 1,
		args_to_string_verbose_list(Args, ArgNum, MaxSize, CurSize1,
			NewSize, MaxDepth, CurDepth1, ArgsFrame),
		frame__vglue([Functor], ArgsFrame, Frame)
	;
		browser_term_compress(BrowserTerm, Line),
		Frame = [Line],
		NewSize = CurSize
	).

:- pred args_to_string_verbose_list(list(univ)::in, int::in, int::in,
	int::in, int::out, int::in, int::in, frame::out) is cc_multi.

args_to_string_verbose_list([], _ArgNum, _MaxSize, CurSize, NewSize,
		_MaxDepth, _CurDepth, []) :-
	NewSize = CurSize.
args_to_string_verbose_list([Univ], ArgNum, MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, Frame) :-
	browser_term_to_string_verbose_2(plain_term(Univ), MaxSize,
		CurSize, NewSize, MaxDepth, CurDepth, TreeFrame),
	% XXX: ArgNumS must have fixed length 2.
	string__int_to_string(ArgNum, ArgNumS),
	string__append_list([ArgNumS, "-"], LastBranchS),
	frame__hglue([LastBranchS], TreeFrame, Frame).
args_to_string_verbose_list([Univ1, Univ2 | Univs], ArgNum, MaxSize,
		CurSize, NewSize, MaxDepth, CurDepth, Frame) :-
	browser_term_to_string_verbose_2(plain_term(Univ1), MaxSize, CurSize,
		NewSize1, MaxDepth, CurDepth, TreeFrame),
	ArgNum1 is ArgNum + 1,
	args_to_string_verbose_list([Univ2 | Univs], ArgNum1, MaxSize,
		NewSize1, NewSize2, MaxDepth, CurDepth, RestTreesFrame),
	NewSize = NewSize2,
	% XXX: ArgNumS must have fixed length 2.
	string__int_to_string(ArgNum, ArgNumS),
	string__append_list([ArgNumS, "-"], BranchFrameS),
	frame__vsize(TreeFrame, Height),
	Height1 is Height - 1,
	list__duplicate(Height1, "|", VBranchFrame),
	frame__vglue([BranchFrameS], VBranchFrame, LeftFrame),
	frame__hglue(LeftFrame, TreeFrame, TopFrame),
	frame__vglue(TopFrame, RestTreesFrame, Frame).

:- pred unlines(list(string)::in, string::out) is det.

unlines([], "").
unlines([Line | Lines], Str) :-
	string__append(Line, "\n", NLine),
	unlines(Lines, Strs),
	string__append(NLine, Strs, Str).

%---------------------------------------------------------------------------%
%
% Miscellaneous path handling
%

:- pred write_path(debugger, list(dir), io__state, io__state).
:- mode write_path(in, in, di, uo) is det.
write_path(Debugger, []) -->
	write_string_debugger(Debugger, "/").
write_path(Debugger, [Dir]) -->
	(
		{ Dir = parent },
		write_string_debugger(Debugger, "/")
	;
		{ Dir = child_num(N) },
		write_string_debugger(Debugger, "/"), 
		write_int_debugger(Debugger, N)
	;
		{ Dir = child_name(Name) },
		write_string_debugger(Debugger, "/"), 
		write_string_debugger(Debugger, Name)
	).
write_path(Debugger, [Dir, Dir2 | Dirs]) -->
	write_path_2(Debugger, [Dir, Dir2 | Dirs]).


:- pred write_path_2(debugger, list(dir), io__state, io__state).
:- mode write_path_2(in, in, di, uo) is det.
write_path_2(Debugger, []) -->
	write_string_debugger(Debugger, "/").
write_path_2(Debugger, [Dir]) -->
	(
		{ Dir = parent },
		write_string_debugger(Debugger, "/..")
	;
		{ Dir = child_num(N) },
		write_string_debugger(Debugger, "/"), 
		write_int_debugger(Debugger, N)
	;
		{ Dir = child_name(Name) },
		write_string_debugger(Debugger, "/"), 
		write_string_debugger(Debugger, Name)
	).
write_path_2(Debugger, [Dir, Dir2 | Dirs]) -->
	(
		{ Dir = parent },
		write_string_debugger(Debugger, "/.."),
		write_path_2(Debugger, [Dir2 | Dirs])
	;
		{ Dir = child_num(N) },
		write_string_debugger(Debugger, "/"), 
		write_int_debugger(Debugger, N),
		write_path_2(Debugger, [Dir2 | Dirs])
	;
		{ Dir = child_name(Name) },
		write_string_debugger(Debugger, "/"), 
		write_string_debugger(Debugger, Name),
		write_path_2(Debugger, [Dir2 | Dirs])
	).

	% We assume a root-relative path. We assume Term is the entire term
	% passed into browse/3, not a subterm.
:- pred deref_subterm(browser_term::in, list(dir)::in, browser_term::out)
	is semidet.

deref_subterm(BrowserTerm, Path, SubBrowserTerm) :-
	simplify_dirs(Path, SimplifiedPath),
	(
		BrowserTerm = plain_term(Univ),
		deref_subterm_2(Univ, SimplifiedPath, SubUniv),
		SubBrowserTerm = plain_term(SubUniv)
	;
		BrowserTerm = synthetic_term(_Functor, Args, MaybeReturn),
		(
			SimplifiedPath = [],
			SubBrowserTerm = BrowserTerm
		;
			SimplifiedPath = [Step | SimplifiedPathTail],
			(
				Step = child_num(N),
				% The first argument of a non-array is numbered
				% argument 1.
				list__index1(Args, N, ArgUniv)
			;
				Step = child_name(Name),
				(
					MaybeReturn = yes(ArgUnivPrime),
					( Name = "r"
					; Name = "res"
					; Name = "result"
					)
				->
					ArgUniv = ArgUnivPrime
				;
					fail
				)
			;
				Step = parent,
				error("deref_subterm: found parent")
			),
			deref_subterm_2(ArgUniv, SimplifiedPathTail, SubUniv),
			SubBrowserTerm = plain_term(SubUniv)
		)
	).

:- pred deref_subterm_2(univ::in, list(dir)::in, univ::out) is semidet.

deref_subterm_2(Univ, Path, SubUniv) :-
	(
		Path = [],
		Univ = SubUniv
	; 
		Path = [Dir | Dirs],
		(
			Dir = child_num(N),
			(
				TypeCtor = type_ctor(univ_type(Univ)),
				type_ctor_name(TypeCtor) = "array",
				type_ctor_module_name(TypeCtor) = "array"
			->
					% The first element of an array is at
					% index zero.
				ArgN = argument(univ_value(Univ), N)
			;
				% The first argument of a non-array is numbered
				% argument 1 by the user but argument 0 by
				% std_util:argument.
				ArgN = argument(univ_value(Univ), N - 1)
			)
		;
			Dir = child_name(Name),
			ArgN = named_argument(univ_value(Univ), Name)
		;
			Dir = parent,
			error("deref_subterm_2: found parent")
		),
		deref_subterm_2(ArgN, Dirs, SubUniv)
	).

%---------------------------------------------------------------------------%

:- pred get_path(browser_info, path).
:- mode get_path(in, out) is det.
get_path(Info, root_rel(Info ^ dirs)).

:- pred set_path(path, browser_info, browser_info).
:- mode set_path(in, in, out) is det.
set_path(NewPath, Info0, Info) :-
	change_dir(Info0 ^ dirs, NewPath, NewDirs),
	Info = Info0 ^ dirs := NewDirs.

:- pred change_dir(list(dir), path, list(dir)).
:- mode change_dir(in, in, out) is det.
change_dir(PwdDirs, Path, RootRelDirs) :-
	(
		Path = root_rel(Dirs),
		NewDirs = Dirs
	;
		Path = dot_rel(Dirs),
		list__append(PwdDirs, Dirs, NewDirs)
	),
	simplify_dirs(NewDirs, RootRelDirs).

:- pred set_term(univ::in, browser_info::in, browser_info::out) is det.

set_term(Term, Info0, Info) :-
	set_browser_term(plain_term(Term), Info0, Info1),
	% Display from the root term.
	% This avoid errors due to dereferencing non-existent subterms.
	set_path(root_rel([]), Info1, Info).

:- pred set_browser_term(browser_term::in, browser_info::in, browser_info::out)
	is det.

set_browser_term(BrowserTerm, Info, Info ^ term := BrowserTerm).

%---------------------------------------------------------------------------%
%
% Display predicates.
%

:- pred show_settings(debugger, browser_info, maybe(portray_format),
		io__state, io__state).
:- mode show_settings(in, in, in, di, uo) is det.

show_settings(Debugger, Info, MaybeFormat) -->
	{ browser_info__get_format(Info, browse, MaybeFormat, Format) },
	{ browser_info__get_format_params(Info, browse, Format, Params) },
	write_string_debugger(Debugger, "Max depth is: "), 
		write_int_debugger(Debugger, Params ^ depth), 
		nl_debugger(Debugger),
	write_string_debugger(Debugger, "Max size is: "), 
		write_int_debugger(Debugger, Params ^ size), 
		nl_debugger(Debugger),
	write_string_debugger(Debugger, "X clip is: "), 
		write_int_debugger(Debugger, Params ^ width), 
		nl_debugger(Debugger),
	write_string_debugger(Debugger, "Y clip is: "), 
		write_int_debugger(Debugger, Params ^ lines),
		nl_debugger(Debugger),
	write_string_debugger(Debugger, "Current path is: "),
		write_path(Debugger, Info ^ dirs),
		nl_debugger(Debugger),
	{ browser_info__get_format(Info, browse, no, LsFormat) },
	write_string_debugger(Debugger, "Ls format is "),
		print_format_debugger(Debugger, LsFormat),
		nl_debugger(Debugger),
	{ browser_info__get_format(Info, print, no, PrintFormat) },
	write_string_debugger(Debugger, "Print format is "),
		print_format_debugger(Debugger, PrintFormat),
		nl_debugger(Debugger).

:- pred string_to_path(string, path).
:- mode string_to_path(in, out) is semidet.
string_to_path(Str, Path) :-
	string__to_char_list(Str, Cs),
	chars_to_path(Cs, Path).

:- pred chars_to_path(list(char), path).
:- mode chars_to_path(in, out) is semidet.
chars_to_path([C | Cs], Path) :-
	( C = ('/') ->
		Path = root_rel(Dirs),
		chars_to_dirs(Cs, Dirs)
	;
		Path = dot_rel(Dirs),
		chars_to_dirs([C | Cs], Dirs)
	).

:- pred chars_to_dirs(list(char), list(dir)).
:- mode chars_to_dirs(in, out) is semidet.
chars_to_dirs(Cs, Dirs) :-
	split_dirs(Cs, Names),
	names_to_dirs(Names, Dirs).

:- pred names_to_dirs(list(string), list(dir)).
:- mode names_to_dirs(in, out) is semidet.
names_to_dirs([], []).
names_to_dirs([Name | Names], Dirs) :-
	( Name = ".." ->
		Dirs = [parent | RestDirs],
		names_to_dirs(Names, RestDirs)
	; Name = "." ->
		names_to_dirs(Names, Dirs)
	; string__to_int(Name, Num) ->
		Dirs = [child_num(Num) | RestDirs],
		names_to_dirs(Names, RestDirs)
	;
		Dirs = [child_name(Name) | RestDirs],
		names_to_dirs(Names, RestDirs)
	).


:- pred split_dirs(list(char), list(string)).
:- mode split_dirs(in, out) is det.
split_dirs(Cs, Names) :-
	takewhile(not_slash, Cs, NameCs, Rest),
	string__from_char_list(NameCs, Name),
	( NameCs = [] ->
		Names = []
	; Rest = [] ->
		Names = [Name]
	; Rest = [_Slash | RestCs] ->
		split_dirs(RestCs, RestNames),
		Names = [Name | RestNames]
	;
		error("split_dirs: software error")
	).
		
		
:- pred not_slash(char).
:- mode not_slash(in) is semidet.
not_slash(C) :-
	C \= ('/').

	% Remove "/dir/../" sequences from a list of directories to yield
	% a form that lacks ".." entries.
	% NB: This can be done more efficiently than simple iteration
	% to a limit.
:- pred simplify_dirs(list(dir), list(dir)).
:- mode simplify_dirs(in, out) is det.
simplify_dirs(Dirs, SimpleDirs) :-
	util__limit(simplify, Dirs, SimpleDirs).

	% If possible, remove a single occurence of
	% either:
	%	- "dir/../"
	% or:
	%	- "/.." (parent of root is root)
	%
:- pred simplify(list(dir), list(dir)).
:- mode simplify(in, out) is det.
simplify([], []).
simplify([First | Rest], Simplified) :-
	( First = parent ->
		Simplified = Rest
	; Rest = [] ->
		Simplified = [First]
	; Rest = [parent | Tail] ->
		Simplified = Tail
	;
		simplify(Rest, SimplifiedRest),
		Simplified = [First | SimplifiedRest]
	).

%---------------------------------------------------------------------------%

:- pred write_string_debugger(debugger, string, io__state, io__state).
:- mode write_string_debugger(in, in, di, uo) is det.
write_string_debugger(internal, String) -->
	io__write_string(String).
write_string_debugger(external, String) -->
	send_term_to_socket(browser_str(String)).

:- pred nl_debugger(debugger, io__state, io__state).
:- mode nl_debugger(in, di, uo) is det.
nl_debugger(internal) -->
	io__nl.
nl_debugger(external) -->
	send_term_to_socket(browser_nl).

:- pred write_int_debugger(debugger, int, io__state, io__state).
:- mode write_int_debugger(in, in, di, uo) is det.
write_int_debugger(internal, Int) -->
	io__write_int(Int).
write_int_debugger(external, Int) -->
	send_term_to_socket(browser_int(Int)).


:- pred print_format_debugger(debugger, portray_format, io__state, io__state).
:- mode print_format_debugger(in, in, di, uo) is det.
print_format_debugger(internal, X) -->
	io__print(X).
print_format_debugger(external, X) -->
	(
		{ X = flat },
		send_term_to_socket(browser_str("flat"))
	;
		{ X = raw_pretty },
		send_term_to_socket(browser_str("raw_pretty"))
	;
		{ X = verbose },
		send_term_to_socket(browser_str("verbose"))
	;
		{ X = pretty },
		send_term_to_socket(browser_str("pretty"))
	).

:- pred send_term_to_socket(term_browser_response, io__state, io__state).
:- mode send_term_to_socket(in, di, uo) is det.
send_term_to_socket(Term) -->
	write(Term),
	print(".\n"),
	flush_output.

%---------------------------------------------------------------------------%

    % These two functions are just like like pprint:to_doc, except their input
    % is not a natural term, but a synthetic term defined by a functor, a list
    % of arguments, and if the synthetic term is a function application, then
    % the result of that function application.

:- func synthetic_term_to_doc(string, list(univ), maybe(univ))      = doc.
:- func synthetic_term_to_doc(int, string, list(univ), maybe(univ)) = doc.

synthetic_term_to_doc(Functor, Args, MaybeReturn) =
	synthetic_term_to_doc(int__max_int, Functor, Args, MaybeReturn).

synthetic_term_to_doc(Depth, Functor, Args, MaybeReturn) = Doc :-
	Arity = list__length(Args),
	( Depth =< 0 ->
		( Arity = 0 ->
			Doc = text(Functor)
		;
			(
				MaybeReturn = yes(_),
				Doc = text(Functor) `<>` text("/") `<>`
					poly(i(Arity)) `<>` text("+1") 
			;
				MaybeReturn = no,
				Doc = text(Functor) `<>` text("/") `<>`
					poly(i(Arity))
			)
		)
	;
		( Arity = 0 ->
			Doc = text(Functor)
		;
			ArgDocs = packed_cs_univ_args(Depth - 1, Args),
			(
				MaybeReturn = yes(Return),
				Doc = group(
					text(Functor) `<>`
					parentheses(
						nest(2, ArgDocs)
					) `<>`
					nest(2, text(" = ") `<>`
						to_doc(Depth - 1, Return)
					)
				)
			;
				MaybeReturn = no,
				Doc = group(
					text(Functor) `<>` parentheses(
						nest(2, ArgDocs)
					)
				)
			)
		)
	).

%---------------------------------------------------------------------------%
