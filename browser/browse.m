%---------------------------------------------------------------------------%
% Copyright (C) 1998-1999 The University of Melbourne.
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

:- module browse.

:- interface.

:- import_module io.

	% an abstract data type that holds persistent browser settings,
	% e.g. the maximum print depth
:- type browser_state.

	% initialize the browser state with default values
:- pred browse__init_state(browser_state, io__state, io__state).
:- mode browse__init_state(out, di, uo) is det.

	% The interactive term browser.
:- pred browse__browse(T, io__input_stream, io__output_stream,
			browser_state, browser_state, io__state, io__state).
:- mode browse__browse(in, in, in, in, out, di, uo) is det.

:- pred browse__browse_external(T, io__input_stream, io__output_stream,
			browser_state, browser_state, io__state, io__state).
:- mode browse__browse_external(in, in, in, in, out, di, uo) is det.

	% The non-interactive term browser.
:- pred browse__print(T, io__output_stream, browser_state,
			io__state, io__state).
:- mode browse__print(in, in, in, di, uo) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module parse, util, frame.
:- import_module string, list, parser, require, std_util, int, char.

%---------------------------------------------------------------------------%
%
% We export these predicates to C for use by the tracer:
% they are used in trace/mercury_trace_browser.c.
%

:- pragma export(browse__init_state(out, di, uo),
	"ML_BROWSE_init_state").
:- pragma export(browse__browse(in, in, in, in, out, di, uo),
	"ML_BROWSE_browse").
:- pragma export(browse__print(in, in, in, di, uo),
	"ML_BROWSE_print").
:- pragma export(browse__browser_state_type(out),
	"ML_BROWSE_browser_state_type").

:- pragma export(browse__browse_external(in, in, in, in, out, di, uo),
	"ML_BROWSE_browse_external").

%---------------------------------------------------------------------------%
% If the term browser is called from the internal debugger, Input/Output are 
% done via MR_mdb_in/MR_mdb_out. If it is called from the external debugger,
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

browse__init_state(State) -->
	{ default_state(State) }.

% return the type_info for a browser_state type
:- pred browse__browser_state_type(type_info).
:- mode browse__browser_state_type(out) is det.

browse__browser_state_type(Type) :-
	default_state(State),
	Type = type_of(State).

%---------------------------------------------------------------------------%
%
% Non-interactive display
%

browse__print(Term, OutputStream, State0) -->
	{ set_term(Term, State0, State) },
	io__set_output_stream(OutputStream, OldStream),
	browse__print(State),
	io__set_output_stream(OldStream, _).

:- pred browse__print(browser_state, io__state, io__state).
:- mode browse__print(in, di, uo) is det.

browse__print(State) -->
	%
	% io__write handles the special cases such as lists,
	% operators, etc. better, so we prefer to use it if we
	% can.  However, io__write doesn't have a depth or size limit,
	% so we need to check the size first; if the term is small
	% enough, we use io__write (actually io__write_univ), otherwise
	% we use portray_fmt(..., flat).
	%
	{ get_term(State, Univ) },
	{ max_print_size(MaxSize) },
	{ term_size_left_from_max(Univ, MaxSize, RemainingSize) },
	( { RemainingSize >= 0 } ->
		io__write_univ(Univ),
		io__nl
	;
		portray_fmt(internal, State, flat)
	).

:- pred browse__print_external(browser_state, io__state, io__state).
:- mode browse__print_external(in, di, uo) is det.

browse__print_external(State) -->
	portray_fmt(external, State, flat).

	% The maximum estimated size for which we use `io__write'.
:- pred max_print_size(int::out) is det.
max_print_size(60).

	% Estimate the total term size, in characters.
	% We count the number of characters in the functor,
	% plus two characters for each argument: "(" and ")"
	% for the first, and ", " for each of the rest,
	% plus the sizes of the arguments themselves.
	% This is only approximate since it doesn't take into
	% account all the special cases such as operators.
:- pred term_size(univ::in, int::out) is det.
term_size(Univ, TotalSize) :-
	deconstruct(Univ, Functor, Arity, Args),
	string__length(Functor, FunctorSize),
	list__map(term_size, Args, ArgSizes),
	AddSizes = (pred(X::in, Y::in, Z::out) is det :- Z = X + Y),
	list__foldl(AddSizes, ArgSizes, Arity * 2, TotalArgsSize),
	TotalSize = TotalArgsSize + FunctorSize.

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
term_size_left_from_max(Univ, MaxSize, RemainingSize) :-
	( MaxSize < 0 ->
		RemainingSize = MaxSize
	;
		deconstruct(Univ, Functor, Arity, Args),
		string__length(Functor, FunctorSize),
		PrincipalSize = FunctorSize + Arity * 2,
		MaxArgsSize = MaxSize - PrincipalSize,
		list__foldl(term_size_left_from_max,
			Args, MaxArgsSize, RemainingSize)
	).

%---------------------------------------------------------------------------%
%
% Interactive display
%

browse__browse(Object, InputStream, OutputStream, State0, State) -->
	browse_common(internal, Object, InputStream, OutputStream, 
		State0, State).

browse__browse_external(Object, InputStream, OutputStream, State0, State) -->
	browse_common(external, Object, InputStream, OutputStream, 
		State0, State).


:- pred browse_common(debugger, T, io__input_stream, io__output_stream,
			browser_state, browser_state, io__state, io__state).
:- mode browse_common(in, in, in, in, in, out, di, uo) is det.
browse_common(Debugger, Object, InputStream, OutputStream, State0, State) -->
	{ type_to_univ(Object, Univ) },
	{ set_term(Univ, State0, State1) },
	io__set_input_stream(InputStream, OldInputStream),
	io__set_output_stream(OutputStream, OldOutputStream),
	% startup_message,
	browse_main_loop(Debugger, State1, State),
	io__set_input_stream(OldInputStream, _),
	io__set_output_stream(OldOutputStream, _).

:- pred browse_main_loop(debugger, browser_state, browser_state, 
	io__state, io__state).
:- mode browse_main_loop(in, in, out, di, uo) is det.
browse_main_loop(Debugger, State0, State) -->
	{ prompt(Prompt) },
	( { Debugger = internal } ->
		parse__read_command(Prompt, Command)
	;
		parse__read_command_external(Command)
	),
	( { Command = quit } ->
		% write_string_debugger(Debugger, "quitting...\n")
		( { Debugger = external } ->
			send_term_to_socket(browser_quit)
		;
			{ true }
		),
		{ State = State0 }
	;
		run_command(Debugger, Command, State0, State1),
		browse_main_loop(Debugger, State1, State)
	).

:- pred startup_message(debugger::in, io__state::di, io__state::uo) is det.
startup_message(Debugger) -->
	write_string_debugger(Debugger, "-- Simple Mercury Term Browser.\n"),
	write_string_debugger(Debugger, "-- Type \"help\" for help.\n\n").

:- pred prompt(string::out) is det.
prompt("browser> ").


:- pred run_command(debugger, command, browser_state, browser_state,
	io__state, io__state).
:- mode run_command(in, in, in, out, di, uo) is det.
run_command(Debugger, Command, State, NewState) -->
	( { Command = unknown } ->
		write_string_debugger(Debugger, 
			"Error: unknown command or syntax error.\n"),
		write_string_debugger(Debugger, "Type \"help\" for help.\n"),
		{ NewState = State }
	; { Command = help } ->
		help(Debugger),
		{ NewState = State }
	; { Command = set } ->
		show_settings(Debugger, State),
		{ NewState = State }
	; { Command = set(Setting) } ->
		( { Setting = depth(MaxDepth) } ->
			{ set_depth(MaxDepth, State, NewState) }
		; { Setting = size(MaxSize) } ->
			{ set_size(MaxSize, State, NewState) }
		; { Setting = clipx(X) } ->
			{ set_clipx(X, State, NewState) }
		; { Setting = clipy(Y) } ->
			{ set_clipy(Y, State, NewState) }
		; { Setting = format(Fmt) } ->
			{ set_fmt(Fmt, State, NewState) }
		;
			write_string_debugger(Debugger, 
				"error: unknown setting.\n"),
			{ NewState = State }
		)
	; { Command = ls } ->
		portray(Debugger, State),
		{ NewState = State }
	; { Command = ls(Path) } ->
		portray_path(Debugger, State, Path),
		{ NewState = State }
	; { Command = cd } ->
		{ set_path(root_rel([]), State, NewState) }
	; { Command = cd(Path) } ->
		{ get_dirs(State, Pwd) },
		{ get_term(State, Univ) },
		{ change_dir(Pwd, Path, NewPwd) },
		( { deref_subterm(Univ, NewPwd, _SubUniv) } ->
			{ set_path(Path, State, NewState) }
		;
			write_string_debugger(Debugger, 
				"error: cannot change to subterm\n"),
			{ NewState = State }
		)
	; { Command = print } ->
		( { Debugger = internal} ->
			browse__print(State)
		;
			browse__print_external(State)
		),
		{ NewState = State }
	; { Command = pwd } ->
		{ get_dirs(State, Path) },
		write_path(Debugger, Path),
		nl_debugger(Debugger),
		{ NewState = State }
	;	
		write_string_debugger(Debugger, "command not yet implemented\n"),
		{ NewState = State }
	),
	( { Debugger = external } ->
		send_term_to_socket(browser_end_command)
	;
		{ true }
	).

	% XXX: default depth is hardwired to 10.
:- pred help(debugger::in, io__state::di, io__state::uo) is det.
help(Debugger) -->
	write_string_debugger(Debugger,
"Commands are:\n\
\tls [path]      -- list subterm (expanded)\n\
\tcd [path]      -- cd current subterm (default is root)\n\
\thelp           -- show this help message\n\
\tset var value  -- set a setting\n\
\tset            -- show settings\n\
\tprint          -- show single line representation of current term\n\
\tquit           -- quit browser\n\
SICStus Prolog style commands are:\n\
\tp              -- print\n\
\t< [n]          -- set depth (default is 10)\n\
\t^ [path]       -- cd [path]\n\
\t?              -- help\n\
\th              -- help\n\
\n\
-- settings:\n\
--    size; depth; path; format (flat pretty verbose); clipx; clipy\n\
--    Paths can be Unix-style or SICStus-style: /2/3/1 or ^2^3^1\n\
\n"
	).

%---------------------------------------------------------------------------%
%
% Various pretty-print routines
%

:- pred portray(debugger, browser_state, io__state, io__state).
:- mode portray(in, in, di, uo) is det.
portray(Debugger, State) -->
	{ get_fmt(State, Fmt) },
	(
		{ Fmt = flat },
		portray_flat(Debugger, State)
	;
		{ Fmt = pretty },
		portray_pretty(Debugger, State)
	;
		{ Fmt = verbose },
		portray_verbose(Debugger, State)
	).


:- pred portray_path(debugger, browser_state, path, io__state, io__state).
:- mode portray_path(in, in, in, di, uo) is det.
portray_path(Debugger, State, Path) -->
	{ set_path(Path, State, NewState) },
	portray(Debugger, NewState).

:- pred portray_fmt(debugger, browser_state, portray_format, 
	io__state, io__state).
:- mode portray_fmt(in, in, in, di, uo) is det.
portray_fmt(Debugger, State, Format) -->
	(
		{ Format = flat },
		portray_flat(Debugger, State)
	;
		{ Format = pretty },
		portray_pretty(Debugger, State)
	;
		{ Format = verbose },
		portray_verbose(Debugger, State)
	).

	% XXX: could abstract out the code common to the following preds.
:- pred portray_flat(debugger, browser_state, io__state, io__state).
:- mode portray_flat(in, in, di, uo) is det.
portray_flat(Debugger, State) -->
	{ get_term(State, Univ) },
	{ get_size(State, MaxSize) },
	{ get_depth(State, MaxDepth) },
	{ get_dirs(State, Dir) },
	( { deref_subterm(Univ, Dir, SubUniv) } ->
		{ term_to_string(SubUniv, MaxSize, MaxDepth, Str) },
		write_string_debugger(Debugger, Str)
	;
		write_string_debugger(Debugger, "error: no such subterm")
	),
	nl_debugger(Debugger).

:- pred portray_verbose(debugger, browser_state, io__state, io__state).
:- mode portray_verbose(in, in, di, uo) is det.
portray_verbose(Debugger, State) -->
	{ get_size(State, MaxSize) },
	{ get_depth(State, MaxDepth) },
	{ get_term(State, Univ) },
	{ get_dirs(State, Dir) },
	{ get_clipx(State, X) },
	{ get_clipy(State, Y) },
	( { deref_subterm(Univ, Dir, SubUniv) } ->
		{ term_to_string_verbose(SubUniv, MaxSize,
			MaxDepth, X, Y, Str) },
		write_string_debugger(Debugger, Str)
	;
		write_string_debugger(Debugger, "error: no such subterm")
	),
	nl_debugger(Debugger).


:- pred portray_pretty(debugger, browser_state, io__state, io__state).
:- mode portray_pretty(in, in, di, uo) is det.
portray_pretty(Debugger, State) -->
	{ get_size(State, MaxSize) },
	{ get_depth(State, MaxDepth) },
	{ get_term(State, Univ) },
	{ get_dirs(State, Dir) },
	( { deref_subterm(Univ, Dir, SubUniv) } ->
		{ term_to_string_pretty(SubUniv, MaxSize, MaxDepth, Str) },
		write_string_debugger(Debugger, Str)
	;
		write_string_debugger(Debugger, "error: no such subterm")
	),
	nl_debugger(Debugger).

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
		deconstruct(Univ, Functor, _Arity, Args),
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
	deconstruct(Univ, Functor, Arity, _Args),
	( Arity = 0 ->
		Str = Functor
	;
		int_to_string(Arity, ArityS),
		append_list([Functor, "/", ArityS], Str)
	).
	

%---------------------------------------------------------------------------%
%
% Simple indented view of a term. This isn't really
% pretty printing since parentheses and commas are omitted.
% XXX: Should do proper pretty printing?
%

:- pred term_to_string_pretty(univ, int, int, string).
:- mode term_to_string_pretty(in, in, in, out) is det.
term_to_string_pretty(Univ, MaxSize, MaxDepth, Str) :-
	CurSize = 0,
	CurDepth = 0,
	term_to_string_pretty_2(Univ, MaxSize, CurSize, _NewSize,
		MaxDepth, CurDepth, Lines),
	unlines(Lines, Str).

:- pred term_to_string_pretty_2(univ, int, int, int, int, int, list(string)).
:- mode term_to_string_pretty_2(in, in, in, out, in, in, out) is det.
term_to_string_pretty_2(Univ, MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, Lines) :-
	( ((CurSize >= MaxSize) ; (CurDepth >= MaxDepth)) ->
		term_compress(Univ, Line),
		Lines = [Line],
		% Lines = ["..."],
		NewSize = CurSize
	;
		deconstruct(Univ, Functor, Arity, Args),
		CurSize1 is CurSize + 1,
		CurDepth1 is CurDepth + 1,
		( Arity >= 1 ->
			string__append(Functor, "(", Functor1)
		;
			Functor1 = Functor
		),
		term_to_string_pretty_list(Args, MaxSize, CurSize1,
			NewSize, MaxDepth, CurDepth1, ArgsLines),
		list__condense(ArgsLines, ArgsLineses),
		map(indent, ArgsLineses, IndentedArgLines),
		list__append([Functor1], IndentedArgLines, Lines1),
		( Arity >= 1 ->
			list__append(Lines1, [")"], Lines)
		;
			Lines = Lines1
		)
	).


:- pred term_to_string_pretty_list(list(univ), int, int, int, int, int,
	list(list(string))).
:- mode term_to_string_pretty_list(in, in, in, out, in, in, out) is det.
term_to_string_pretty_list([], _MaxSize, CurSize, NewSize,
		_MaxDepth, _CurDepth, Lines) :-
	Lines = [],
	NewSize = CurSize.
term_to_string_pretty_list([Univ], MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, Lineses) :-
	term_to_string_pretty_2(Univ, MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, Lines),
	Lineses = [Lines].
term_to_string_pretty_list([Univ1, Univ2 | Univs], MaxSize, CurSize, NewSize,
		MaxDepth, CurDepth, Lineses) :-
	term_to_string_pretty_2(Univ1, MaxSize, CurSize, NewSize1,
		MaxDepth, CurDepth, Lines1),
	comma_last(Lines1, Lines),
	term_to_string_pretty_list([Univ2 | Univs], MaxSize, NewSize1, NewSize,
		MaxDepth, CurDepth, RestLineses),
	Lineses = [Lines | RestLineses].

:- pred comma_last(list(string), list(string)).
:- mode comma_last(in, out) is det.
comma_last([], []).
comma_last([S], [Sc]) :-
	string__append(S, ",", Sc).
comma_last([S1, S2 | Ss], [S1 | Rest]) :-
	comma_last([S2 | Ss], Rest).

	
:- pred indent(string::in, string::out) is det.
indent(Str, IndentedStr) :-
	string__append("  ", Str, IndentedStr).

:- pred unlines(list(string)::in, string::out) is det.
unlines([], "").
unlines([Line | Lines], Str) :-
	string__append(Line, "\n", NLine),
	unlines(Lines, Strs),
	string__append(NLine, Strs, Str).


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
		deconstruct(Univ, Functor, _Arity, Args),
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
		deconstruct(Univ, _Functor, _Arity, Args),
		list__index1(Args, N, ArgN),
		deref_subterm_2(ArgN, Ns, SubUniv)
	).

%---------------------------------------------------------------------------%
%
% The definition of the browser_state type and its access routines.
%

:- type browser_state
	--->	browser_state(
			univ,	% term to browse (as univ)
			int,	% depth of tree
			int,	% max nodes printed
			list(dir),	% root rel `present working directory'
			portray_format,	% format for ls.
			int,	% X clipping for verbose display
			int	% Y clipping for verbose display
		).

	% access predicates

:- pred default_state(browser_state).
:- mode default_state(out) is det.
default_state(State) :-
	% We need to supply an object to initialize the state,
	% but this object won't be used, since the first call
	% to browse__browse will overwrite it.  So we just supply
	% a dummy object -- it doesn't matter what its type or value is.
	DummyObject = "",
	type_to_univ(DummyObject, Univ),
	default_depth(DefaultDepth),
	State = browser_state(Univ, 3, DefaultDepth, [], verbose, 79, 25).

:- pred get_term(browser_state, univ).
:- mode get_term(in, out) is det.
get_term(browser_state(Univ, _Depth, _Size, _Path, _Fmt, _X, _Y), Univ).

:- pred get_depth(browser_state, int).
:- mode get_depth(in, out) is det.
get_depth(browser_state(_Univ, Depth, _Size, _Path, _Fmt, _X, _Y), Depth).

:- pred get_size(browser_state, int).
:- mode get_size(in, out) is det.
get_size(browser_state(_Univ, _Depth, Size, _Path, _Fmt, _X, _Y), Size).

:- pred get_clipx(browser_state, int).
:- mode get_clipx(in, out) is det.
get_clipx(browser_state(_Univ, _Depth, _Size, _Path, _Fmt, X, _Y), X).

:- pred get_clipy(browser_state, int).
:- mode get_clipy(in, out) is det.
get_clipy(browser_state(_Univ, _Depth, _Size, _Path, _Fmt, _X, Y), Y).

:- pred get_dirs(browser_state, list(dir)).
:- mode get_dirs(in, out) is det.
get_dirs(browser_state(_Univ, _Depth, _Size, Dirs, _Fmt, _X, _Y), Dirs).

:- pred get_path(browser_state, path).
:- mode get_path(in, out) is det.
get_path(browser_state(_Univ, _Depth, _Size, Dirs, _Fmt, _X, _Y),
	root_rel(Dirs)).

:- pred get_fmt(browser_state, portray_format).
:- mode get_fmt(in, out) is det.
get_fmt(browser_state(_Univ, _Depth, _Size, _Path, Fmt, _X, _Y), Fmt).

:- pred set_depth(int, browser_state, browser_state).
:- mode set_depth(in, in, out) is det.
set_depth(NewMaxDepth, State, NewState) :-
	State = browser_state(Univ, _MaxDepth, MaxSize, Dirs, Fmt, X, Y),
	NewState = browser_state(Univ, NewMaxDepth, MaxSize, Dirs, Fmt, X, Y).

:- pred set_size(int, browser_state, browser_state).
:- mode set_size(in, in, out) is det.
set_size(NewMaxSize, State, NewState) :-
	State = browser_state(Univ, MaxDepth, _MaxSize, Dirs, Fmt, X, Y),
	NewState = browser_state(Univ, MaxDepth, NewMaxSize, Dirs, Fmt, X, Y).

:- pred set_clipx(int, browser_state, browser_state).
:- mode set_clipx(in, in, out) is det.
set_clipx(NewX, State, NewState) :-
	State = browser_state(Univ, MaxDepth, MaxSize, Dirs, Fmt, _X, Y),
	NewState = browser_state(Univ, MaxDepth, MaxSize, Dirs, Fmt, NewX, Y).

:- pred set_clipy(int, browser_state, browser_state).
:- mode set_clipy(in, in, out) is det.
set_clipy(NewY, State, NewState) :-
	State = browser_state(Univ, MaxDepth, MaxSize, Dirs, Fmt, X, _Y),
	NewState = browser_state(Univ, MaxDepth, MaxSize, Dirs, Fmt, X, NewY).

:- pred set_path(path, browser_state, browser_state).
:- mode set_path(in, in, out) is det.
set_path(NewPath, State, NewState) :-
	State = browser_state(Univ, MaxDepth, MaxSize, Dirs, Fmt, X, Y),
	change_dir(Dirs, NewPath, NewDirs),
	NewState = browser_state(Univ, MaxDepth, MaxSize, NewDirs, Fmt, X, Y).

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

:- pred set_fmt(portray_format, browser_state, browser_state).
:- mode set_fmt(in, in, out) is det.
set_fmt(NewFmt, browser_state(Univ, Depth, Size, Path, _OldFmt, X, Y),
	browser_state(Univ, Depth, Size, Path, NewFmt, X, Y)).

:- pred set_term(T, browser_state, browser_state).
:- mode set_term(in, in, out) is det.
set_term(Term, State0, State) :-
	type_to_univ(Term, Univ),
	set_univ(Univ, State0, State1),
	% Display from the root term.
	% This avoid errors due to dereferencing non-existent subterms.
	set_path(root_rel([]), State1, State).

:- pred set_univ(univ, browser_state, browser_state).
:- mode set_univ(in, in, out) is det.
set_univ(NewUniv, browser_state(_OldUniv, Dep, Siz, Path, Fmt, X, Y),
	browser_state(NewUniv, Dep, Siz, Path, Fmt, X, Y)).

%---------------------------------------------------------------------------%
%
% Display predicates.
%

:- pred show_settings(debugger, browser_state, io__state, io__state).
:- mode show_settings(in, in, di, uo) is det.
show_settings(Debugger, State) -->
	{ State = browser_state(_Univ, MaxDepth, MaxSize,
		CurPath, Fmt, X, Y) },
	write_string_debugger(Debugger, "Max depth is: "), 
		write_int_debugger(Debugger, MaxDepth), 
		nl_debugger(Debugger),
	write_string_debugger(Debugger, "Max size is: "), 
		write_int_debugger(Debugger, MaxSize), 
		nl_debugger(Debugger),
	write_string_debugger(Debugger, "X clip is: "), 
		write_int_debugger(Debugger, X), 
		nl_debugger(Debugger),
	write_string_debugger(Debugger, "Y clip is: "), 
		write_int_debugger(Debugger, Y), nl_debugger(Debugger),
	write_string_debugger(Debugger, "Current path is: "),
		write_path(Debugger, CurPath), nl_debugger(Debugger),
	write_string_debugger(Debugger, "Print format is "),
		print_format_debugger(Debugger, Fmt),
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
write_string_debugger(Debugger, String) -->
	( { Debugger = internal } ->
		io__write_string(String)
	;
		send_term_to_socket(browser_str(String))
	).

:- pred nl_debugger(debugger, io__state, io__state).
:- mode nl_debugger(in, di, uo) is det.
nl_debugger(Debugger) -->
	( { Debugger = internal } ->
		io__nl
	;
		send_term_to_socket(browser_nl)
	).

:- pred write_int_debugger(debugger, int, io__state, io__state).
:- mode write_int_debugger(in, in, di, uo) is det.
write_int_debugger(Debugger, Int) -->
	( { Debugger = internal } ->
		io__write_int(Int)
	;
		send_term_to_socket(browser_int(Int))
	).


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
