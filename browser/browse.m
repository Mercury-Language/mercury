%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000 The University of Melbourne.
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

:- import_module io, std_util.
:- import_module mdb__browser_info.

	% The interactive term browser.  The caller type will be `browse', and
	% the default format for the `browse' caller type will be used.
	%
:- pred browse__browse(T, io__input_stream, io__output_stream,
			browser_persistent_state, browser_persistent_state,
			io__state, io__state).
:- mode browse__browse(in, in, in, in, out, di, uo) is det.

	% As above, except that the supplied format will override the default.
	%
:- pred browse__browse_format(T, io__input_stream, io__output_stream,
			portray_format, browser_persistent_state,
			browser_persistent_state, io__state, io__state).
:- mode browse__browse_format(in, in, in, in, in, out, di, uo) is det.

	% The browser interface for the external debugger.  The caller type
	% will be `browse', and the default format will be used.
	%
:- pred browse__browse_external(T, io__input_stream, io__output_stream,
			browser_persistent_state, browser_persistent_state,
			io__state, io__state).
:- mode browse__browse_external(in, in, in, in, out, di, uo) is det.

	% The non-interactive term browser.  The caller type should be either
	% `print' or `print_all'.  The default portray format for that
	% caller type is used.
	%
:- pred browse__print(T, io__output_stream, browse_caller_type,
			browser_persistent_state, io__state, io__state).
:- mode browse__print(in, in, in, in, di, uo) is det.

	% As above, except that the supplied format will override the default.
	%
:- pred browse__print_format(T, io__output_stream, browse_caller_type,
			portray_format, browser_persistent_state,
			io__state, io__state).
:- mode browse__print_format(in, in, in, in, in, di, uo) is det.

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
:- pred term_size_left_from_max(univ::in, int::in, int::out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module mdb__parse, mdb__util, mdb__frame.
:- import_module string, list, parser, require, std_util, int, char, pprint.
:- import_module bool.

%---------------------------------------------------------------------------%
%
% We export these predicates to C for use by the tracer:
% they are used in trace/mercury_trace_browser.c.
%

:- pragma export(browse__browse(in, in, in, in, out, di, uo),
	"ML_BROWSE_browse").
:- pragma export(browse__browse_format(in, in, in, in, in, out, di, uo),
	"ML_BROWSE_browse_format").
:- pragma export(browse__browse_external(in, in, in, in, out, di, uo),
	"ML_BROWSE_browse_external").
:- pragma export(browse__print(in, in, in, in, di, uo),
	"ML_BROWSE_print").
:- pragma export(browse__print_format(in, in, in, in, in, di, uo),
	"ML_BROWSE_print_format").

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
	browse__print_common(Term, OutputStream, Caller, no, State).

browse__print_format(Term, OutputStream, Caller, Format, State) -->
	browse__print_common(Term, OutputStream, Caller, yes(Format), State).

:- pred browse__print_common(T, io__output_stream, browse_caller_type,
		maybe(portray_format), browser_persistent_state,
		io__state, io__state).
:- mode browse__print_common(in, in, in, in, in, di, uo) is det.

browse__print_common(Term, OutputStream, Caller, MaybeFormat, State) -->
	{ browser_info__init(Term, MaybeFormat, State, Info) },
	io__set_output_stream(OutputStream, OldStream),
	{ browser_info__get_format(Info, Caller, MaybeFormat, Format) },
	%
	% We assume that the variable name has been printed on the
	% first part of the line.  If the format is something other than
	% `flat', then we need to start on the next line.
	%
	(
		{ Format = flat }
	->
		[]
	;
		io__nl
	),
	portray(internal, Caller, no, Info),
	io__set_output_stream(OldStream, _).

%---------------------------------------------------------------------------%
%
% Interactive display
%

browse__browse(Object, InputStream, OutputStream, State0, State) -->
	browse_common(internal, Object, InputStream, OutputStream, 
		no, State0, State).

browse__browse_format(Object, InputStream, OutputStream, Format,
		State0, State) -->

	browse_common(internal, Object, InputStream, OutputStream,
		yes(Format), State0, State).

browse__browse_external(Object, InputStream, OutputStream, State0, State) -->
	browse_common(external, Object, InputStream, OutputStream, 
		no, State0, State).

:- pred browse_common(debugger, T, io__input_stream, io__output_stream,
		maybe(portray_format), browser_persistent_state,
		browser_persistent_state, io__state, io__state).
:- mode browse_common(in, in, in, in, in, in, out, di, uo) is det.

browse_common(Debugger, Object, InputStream, OutputStream, MaybeFormat,
		State0, State) -->
	
	{ browser_info__init(Object, MaybeFormat, State0, Info0) },
	io__set_input_stream(InputStream, OldInputStream),
	io__set_output_stream(OutputStream, OldOutputStream),
	% startup_message,
	browse_main_loop(Debugger, Info0, Info),
	io__set_input_stream(OldInputStream, _),
	io__set_output_stream(OldOutputStream, _),
	{ State = Info ^ state }.

:- pred browse_main_loop(debugger, browser_info, browser_info, 
		io__state, io__state).
:- mode browse_main_loop(in, in, out, di, uo) is det.

browse_main_loop(Debugger, Info0, Info) -->
	(
		{ Debugger = internal },
		{ prompt(Prompt) },
		parse__read_command(Prompt, Command)
	;
		{ Debugger = external },
		parse__read_command_external(Command)
	),
	( { Command = quit } ->
		% write_string_debugger(Debugger, "quitting...\n")
		(
			{ Debugger = external },
			send_term_to_socket(browser_quit)
		;
			{ Debugger = internal }
		),
		{ Info = Info0 }
	;
		run_command(Debugger, Command, Info0, Info1),
		browse_main_loop(Debugger, Info1, Info)
	).

:- pred startup_message(debugger::in, io__state::di, io__state::uo) is det.
startup_message(Debugger) -->
	write_string_debugger(Debugger, "-- Simple Mercury Term Browser.\n"),
	write_string_debugger(Debugger, "-- Type \"help\" for help.\n\n").

:- pred prompt(string::out) is det.
prompt("browser> ").


:- pred run_command(debugger, command, browser_info, browser_info,
		io__state, io__state).
:- mode run_command(in, in, in, out, di, uo) is det.

run_command(Debugger, Command, Info0, Info) -->
	% XXX The commands `set', `ls' and `print' should allow the format
	% to be specified by an option.  In each case we instead pass `no' to
	% the respective handler.
	( { Command = unknown } ->
		write_string_debugger(Debugger, 
			"Error: unknown command or syntax error.\n"),
		write_string_debugger(Debugger, "Type \"help\" for help.\n"),
		{ Info = Info0 }
	; { Command = help } ->
		help(Debugger),
		{ Info = Info0 }
	; { Command = set } ->
		show_settings(Debugger, Info0, no),
		{ Info = Info0 }
	; { Command = set(Setting) } ->
		{ set_browse_param(Setting, Info0, Info) }
	; { Command = ls } ->
		portray(Debugger, browse, no, Info0),
		{ Info = Info0 }
	; { Command = ls(Path) } ->
		portray_path(Debugger, browse, no, Info0, Path),
		{ Info = Info0 }
	; { Command = cd } ->
		{ set_path(root_rel([]), Info0, Info) }
	; { Command = cd(Path) } ->
		{ change_dir(Info0 ^ dirs, Path, NewPwd) },
		( { deref_subterm(Info0 ^ term, NewPwd, _SubUniv) } ->
			{ Info = Info0 ^ dirs := NewPwd }
		;
			write_string_debugger(Debugger, 
				"error: cannot change to subterm\n"),
			{ Info = Info0 }
		)
	; { Command = print } ->
		portray(Debugger, print, no, Info0),
		{ Info = Info0 }
	; { Command = pwd } ->
		write_path(Debugger, Info0 ^ dirs),
		nl_debugger(Debugger),
		{ Info = Info0 }
	;	
		write_string_debugger(Debugger,
				"command not yet implemented\n"),
		{ Info = Info0 }
	),
	( { Debugger = external } ->
		send_term_to_socket(browser_end_command)
	;
		{ true }
	).

:- pred set_browse_param(setting, browser_info, browser_info).
:- mode set_browse_param(in, in, out) is det.

set_browse_param(Setting, Info0, Info) :-
	%
	% XXX We can't yet give options to the `set' command.
	%
	No = bool__no,
	browser_info__set_param(No, No, No, No, No, No, Setting, Info0 ^ state,
			NewState),
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
"SICStus Prolog style commands are:\n",
"\tp              -- print\n",
"\t< n            -- set depth\n",
"\t^ [path]       -- cd [path] (default is root)\n",
"\t?              -- help\n",
"\th              -- help\n",
"\n",
"-- settings:\n",
"--    size; depth; path; format (flat pretty verbose); width; lines\n",
"--    Paths can be Unix-style or SICStus-style: /2/3/1 or ^2^3^1\n",
"\n"],
		HelpMessage) },
	write_string_debugger(Debugger, HelpMessage).

%---------------------------------------------------------------------------%
%
% Various pretty-print routines
%

:- pred portray(debugger, browse_caller_type, maybe(portray_format),
		browser_info, io__state, io__state).
:- mode portray(in, in, in, in, di, uo) is det.

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
			{ Format = pretty },
			portray_pretty(Debugger, SubUniv, Params)
		;
			{ Format = verbose },
			portray_verbose(Debugger, SubUniv, Params)
		)
	;
		write_string_debugger(Debugger, "error: no such subterm")
	),
	nl_debugger(Debugger).


:- pred portray_path(debugger, browse_caller_type, maybe(portray_format),
		browser_info, path, io__state, io__state).
:- mode portray_path(in, in, in, in, in, di, uo) is det.

portray_path(Debugger, Caller, MaybeFormat, Info0, Path) -->
	{ set_path(Path, Info0, Info) },
	portray(Debugger, Caller, MaybeFormat, Info).

:- pred portray_flat(debugger, univ, format_params, io__state, io__state).
:- mode portray_flat(in, in, in, di, uo) is det.

portray_flat(Debugger, Univ, Params) -->
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
	{ max_print_size(MaxSize) },
	{ term_size_left_from_max(Univ, MaxSize, RemainingSize) },
	( { RemainingSize >= 0 } ->
		io__write_univ(Univ)
	;
		{ term_to_string(Univ, Params ^ size, Params ^ depth, Str) },
		write_string_debugger(Debugger, Str)
	).

:- pred portray_verbose(debugger, univ, format_params, io__state, io__state).
:- mode portray_verbose(in, in, in, di, uo) is det.

portray_verbose(Debugger, Univ, Params) -->
	{ term_to_string_verbose(Univ, Params ^ size, Params ^ depth,
			Params ^ width, Params ^ lines, Str) },
	write_string_debugger(Debugger, Str).

:- pred portray_pretty(debugger, univ, format_params, io__state, io__state).
:- mode portray_pretty(in, in, in, di, uo) is det.

portray_pretty(Debugger, Univ, Params) -->
	{ term_to_string_pretty(Univ, Params ^ width, Params ^ depth, Str) },
	write_string_debugger(Debugger, Str).


	% The maximum estimated size for which we use `io__write'.
:- pred max_print_size(int::out) is det.
max_print_size(60).

term_size_left_from_max(Univ, MaxSize, RemainingSize) :-
	( MaxSize < 0 ->
		RemainingSize = MaxSize
	;
		deconstruct(univ_value(Univ), Functor, Arity, Args),
		string__length(Functor, FunctorSize),
		PrincipalSize = FunctorSize + Arity * 2,
		MaxArgsSize = MaxSize - PrincipalSize,
		list__foldl(term_size_left_from_max,
			Args, MaxArgsSize, RemainingSize)
	).

%---------------------------------------------------------------------------%
%
% Single-line representation of a term.
%

:- pred term_to_string(univ, int, int, string).
:- mode term_to_string(in, in, in, out) is det.
term_to_string(Univ, MaxSize, MaxDepth, Str) :-
	CurSize = 0,
	CurDepth = 0,
	term_to_string_2(Univ, MaxSize, CurSize, _NewSize,
		MaxDepth, CurDepth, Str).

	% Note: When the size limit is reached, we simply display
	% further subterms compressed. We don't just stopping printing.
	% XXX: Is this reasonable?
:- pred term_to_string_2(univ, int, int, int, int, int, string).
:- mode term_to_string_2(in, in, in, out, in, in, out) is det.
term_to_string_2(Univ, MaxSize, CurSize, NewSize, MaxDepth, CurDepth, Str) :-
	( ( (CurSize >= MaxSize) ; (CurDepth >= MaxDepth) ) ->
		% Str = "...",
		term_compress(Univ, Str),
		NewSize = CurSize
	;
		deconstruct(univ_value(Univ), Functor, _Arity, Args),
		CurSize1 is CurSize + 1,
		CurDepth1 is CurDepth + 1,
		term_to_string_list(Args, MaxSize, CurSize1, NewSize,
			MaxDepth, CurDepth1, ArgStrs),
		brack_args(ArgStrs, BrackArgsStr),
		string__append_list([Functor, BrackArgsStr], Str)
	).

:- pred term_to_string_list(list(univ), int, int, int, int, int, list(string)).
:- mode term_to_string_list(in, in, in, out, in, in, out) is det.
term_to_string_list([], _MaxSize, CurSize, NewSize,
		_MaxDepth, _CurDepth, Strs) :-
	Strs = [],
	NewSize = CurSize.
term_to_string_list([Univ | Univs], MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, Strs) :-
	term_to_string_2(Univ, MaxSize, CurSize, NewSize1,
		MaxDepth, CurDepth, Str),
	term_to_string_list(Univs, MaxSize, NewSize1, NewSize,
		MaxDepth, CurDepth, RestStrs),
	Strs = [Str | RestStrs].


:- pred brack_args(list(string), string).
:- mode brack_args(in, out) is det.
brack_args(Args, Str) :-
	( Args = [] ->
		Str = ""
	;
		comma_args(Args, CommaStr),
		string__append_list(["(", CommaStr, ")"], Str)
	).

:- pred comma_args(list(string), string).
:- mode comma_args(in, out) is det.
comma_args(Args, Str) :-
	(
		Args = [],
		Str = ""
	;
		Args = [S],
		Str = S
	;
		Args = [S1, S2 | Ss],
		comma_args([S2 | Ss], Rest),
		string__append_list([S1, ", ", Rest], Str)
	).

:- pred term_compress(univ, string).
:- mode term_compress(in, out) is det.
term_compress(Univ, Str) :-
	deconstruct(univ_value(Univ), Functor, Arity, _Args),
	( Arity = 0 ->
		Str = Functor
	;
		int_to_string(Arity, ArityS),
		append_list([Functor, "/", ArityS], Str)
	).
	

%---------------------------------------------------------------------------%
%
% Print using the pretty printer from the standard library.
% XXX the size of the term is not limited---the pretty printer
% provides no way of doing this.
%

:- pred term_to_string_pretty(univ, int, int, string).
:- mode term_to_string_pretty(in, in, in, out) is det.

term_to_string_pretty(Univ, Width, MaxDepth, Str) :-
	Value = univ_value(Univ),
	Doc = to_doc(MaxDepth, Value),
	Str = to_string(Width, Doc).

%---------------------------------------------------------------------------%
%
% Verbose printing. Tree layout with numbered branches.
% Numbering makes it easier to change to subterms.
%

:- pred term_to_string_verbose(univ, int, int, int, int, string).
:- mode term_to_string_verbose(in, in, in, in, in, out) is det.
term_to_string_verbose(Univ, MaxSize, MaxDepth, X, Y, Str) :-
	CurSize = 0,
	CurDepth = 0,
	term_to_string_verbose_2(Univ, MaxSize, CurSize, _NewSize,
		MaxDepth, CurDepth, Frame),
	frame__clip(X-Y, Frame, ClippedFrame),
	unlines(ClippedFrame, Str).

:- pred term_to_string_verbose_2(univ, int, int, int, int, int, frame).
:- mode term_to_string_verbose_2(in, in, in, out, in, in, out) is det.
term_to_string_verbose_2(Univ, MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, Frame) :-
	( ((CurSize >= MaxSize) ; (CurDepth >= MaxDepth)) ->
		term_compress(Univ, Line),
		Frame = [Line],
		NewSize = CurSize
	;
		deconstruct(univ_value(Univ), Functor, _Arity, Args),
		CurSize1 is CurSize + 1,
		CurDepth1 is CurDepth + 1,
		ArgNum = 1,
		term_to_string_verbose_list(Args, ArgNum,
			MaxSize, CurSize1, NewSize,
			MaxDepth, CurDepth1, ArgsFrame),
		frame__vglue([Functor], ArgsFrame, Frame)
	).

:- pred term_to_string_verbose_list(list(univ), int, int, int, int,
	int, int, frame).
:- mode term_to_string_verbose_list(in, in, in, in, out, in, in, out) is det.

term_to_string_verbose_list([], _ArgNum, _MaxSize, CurSize, NewSize,
		_MaxDepth, _CurDepth, []) :-
	NewSize = CurSize.

term_to_string_verbose_list([Univ], ArgNum, MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, Frame) :-
	term_to_string_verbose_2(Univ, MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, TreeFrame),
	% XXX: ArgNumS must have fixed length 2.
	string__int_to_string(ArgNum, ArgNumS),
	string__append_list([ArgNumS, "-"], LastBranchS),
	frame__hglue([LastBranchS], TreeFrame, Frame).

term_to_string_verbose_list([Univ1, Univ2 | Univs], ArgNum, MaxSize, CurSize,
		NewSize, MaxDepth, CurDepth, Frame) :-
	term_to_string_verbose_2(Univ1, MaxSize, CurSize, NewSize1,
		MaxDepth, CurDepth, TreeFrame),
	ArgNum1 is ArgNum + 1,
	term_to_string_verbose_list([Univ2 | Univs], ArgNum1, MaxSize,
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
		{ Dir = child(N) },
		write_string_debugger(Debugger, "/"), 
		write_int_debugger(Debugger, N)
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
		{ Dir = child(N) },
		write_string_debugger(Debugger, "/"), 
		write_int_debugger(Debugger, N)
	).
write_path_2(Debugger, [Dir, Dir2 | Dirs]) -->
	(
		{ Dir = parent },
		write_string_debugger(Debugger, "/.."),
		write_path_2(Debugger, [Dir2 | Dirs])
	;
		{ Dir = child(N) },
		write_string_debugger(Debugger, "/"), 
		write_int_debugger(Debugger, N),
		write_path_2(Debugger, [Dir2 | Dirs])
	).

	% We assume a root-relative path. We assume Term is the entire term
	% passed into browse/3, not a subterm.
:- pred deref_subterm(univ, list(dir), univ) is semidet.
:- mode deref_subterm(in, in, out) is semidet.
deref_subterm(Univ, Path, SubUniv) :-
	path_to_int_list(Path, PathN),
	deref_subterm_2(Univ, PathN, SubUniv).

:- pred path_to_int_list(list(dir), list(int)).
:- mode path_to_int_list(in, out) is semidet.
path_to_int_list(Path, Ints) :-
	simplify_dirs(Path, NewPath),
	dirs_to_ints(NewPath, Ints).

:- pred dirs_to_ints(list(dir), list(int)).
:- mode dirs_to_ints(in, out) is semidet.
dirs_to_ints([], []).
dirs_to_ints([child(N) | Dirs], [N | Ns]) :-
	dirs_to_ints(Dirs, Ns).
dirs_to_ints([parent | _], _) :-
 	error("dirs_to_ints: software error").

:- pred deref_subterm_2(univ, list(int), univ) is semidet.
:- mode deref_subterm_2(in, in, out) is semidet.
deref_subterm_2(Univ, Path, SubUniv) :-
	( Path = [] ->
		Univ = SubUniv
	; 
		Path = [N | Ns],
		deconstruct(univ_value(Univ), _Functor, _Arity, Args),
		list__index1(Args, N, ArgN),
		deref_subterm_2(ArgN, Ns, SubUniv)
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

:- pred set_term(T, browser_info, browser_info).
:- mode set_term(in, in, out) is det.
set_term(Term, Info0, Info) :-
	type_to_univ(Term, Univ),
	set_univ(Univ, Info0, Info1),
	% Display from the root term.
	% This avoid errors due to dereferencing non-existent subterms.
	set_path(root_rel([]), Info1, Info).

:- pred set_univ(univ, browser_info, browser_info).
:- mode set_univ(in, in, out) is det.
set_univ(NewUniv, Info, Info ^ term := NewUniv).

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
	;
		string__to_int(Name, Num),
		Dirs = [child(Num) | RestDirs],
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
simplify([parent | Dirs], Dirs).
simplify([child(Dir)], [child(Dir)]).
simplify([child(_Dir), parent | Dirs], Dirs).
simplify([child(Dir1), child(Dir2) | Dirs], [child(Dir1) | Rest]) :-
	simplify([child(Dir2) | Dirs], Rest).

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
		{ X = pretty },
		send_term_to_socket(browser_str("pretty"))
	;
		{ X = verbose },
		send_term_to_socket(browser_str("verbose"))
	).

:- pred send_term_to_socket(term_browser_response, io__state, io__state).
:- mode send_term_to_socket(in, di, uo) is det.
send_term_to_socket(Term) -->
	write(Term),
	print(".\n"),
	flush_output.

%---------------------------------------------------------------------------%
