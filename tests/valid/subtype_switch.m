% regression test for det switches on subtypes.

:- module subtype_switch.

:- interface.

:- import_module io.

:- type widget.

:- type tcl_interp == c_pointer.
:- type tcl_status ---> tcl_ok ; tcl_error.

:- type config	--->
		text(string)
	;	width(int)
	;	height(int)
	;	command(pred(tcl_interp, io__state, io__state))
	;	title(string)
	;	fill_color(string)
	.

:- inst widget = bound((
		text(ground)
	;	width(ground)
	;	height(ground)
	;	command(pred(in, di, uo) is det)
	;	title(ground)
	)).

%------------------------------------------------------------------------------%

:- implementation.

:- type widget
	--->	text(string)
	;	width(int)
	;	height(int)
	;	command(pred(string, io__state, io__state))
	;	title(string).

:- import_module string, int, require, list.

:- pred stringify_config(tcl_interp, config, string, io__state, io__state).
:- mode stringify_config(in, in(widget), out, di, uo) is det.

stringify_config(_Interp, text(Text), Str, IO, IO) :-
	string__format("-text ""%s""", [s(Text)], Str).
stringify_config(_Interp, width(Width), Str, IO, IO) :-
	string__format("-width %d", [i(Width)], Str).
stringify_config(_Interp, height(Height), Str, IO, IO) :-
	string__format("-height %d", [i(Height)], Str).
stringify_config(_Interp, command(_Closure), Str) -->
	get_thingy_counter(Id),
	set_thingy_counter(Id+1),
	{ string__format("cmd%d", [i(Id)], CmdName) },
	% create_command(Interp, CmdName, command_wrapper(Closure)),
	{ string__format("-command %s", [s(CmdName)], Str) }.
stringify_config(_Interp, title(Text), Str, IO, IO) :-
	string__format("-title ""%s""", [s(Text)], Str).

:- pragma c_header_code("
	extern MR_Integer	tk_direct_thingy_counter;
").

:- pragma c_code("
	MR_Integer	tk_direct_thingy_counter = 0;
").

:- pred get_thingy_counter(int::out, io__state::di, io__state::uo) is det.

:- pragma c_code(get_thingy_counter(Int::out, IO0::di, IO::uo), "
	Int = tk_direct_thingy_counter;
	IO = IO0;
").
get_thingy_counter(5) --> [].

:- pred set_thingy_counter(int::in, io__state::di, io__state::uo) is det.

:- pragma c_code(set_thingy_counter(Int::in, IO0::di, IO::uo), "
	tk_direct_thingy_counter = Int;
	IO = IO0;
").
set_thingy_counter(_) --> [].

:- pred command_wrapper(pred(tcl_interp, io__state, io__state), tcl_interp,
		list(string), tcl_status, string, io__state, io__state).
:- mode command_wrapper(pred(in, di, uo) is det, in, in, out, out,
		di, uo) is det.

command_wrapper(Closure, Interp, _Args, tcl_ok, "") -->
	call(Closure, Interp).

