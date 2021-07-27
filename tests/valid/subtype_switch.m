%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% regression test for det switches on subtypes.

:- module subtype_switch.

:- interface.

:- import_module io.

:- type widget.

:- type tcl_interp == c_pointer.
:- type tcl_status
    --->    tcl_ok
    ;       tcl_error.

:- type config
    --->    text(string)
    ;       width(int)
    ;       height(int)
    ;       command(pred(tcl_interp, io, io))
    ;       title(string)
    ;       fill_color(string) .

:- inst widget
    --->    text(ground)
    ;       width(ground)
    ;       height(ground)
    ;       command(pred(in, di, uo) is det)
    ;       title(ground).

%---------------------------------------------------------------------------%

:- implementation.

:- type widget
    --->    text(string)
    ;       width(int)
    ;       height(int)
    ;       command(pred(string, io, io))
    ;       title(string).

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- pred stringify_config(tcl_interp::in, config::in(widget), string::out,
    io::di, io::uo) is det.

stringify_config(_Interp, text(Text), Str, !IO) :-
    string.format("-text ""%s""", [s(Text)], Str).
stringify_config(_Interp, width(Width), Str, !IO) :-
    string.format("-width %d", [i(Width)], Str).
stringify_config(_Interp, height(Height), Str, !IO) :-
    string.format("-height %d", [i(Height)], Str).
stringify_config(_Interp, command(_Closure), Str, !IO) :-
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("cmd%d", [i(Id)], CmdName),
    % create_command(Interp, CmdName, command_wrapper(Closure)),
    string.format("-command %s", [s(CmdName)], Str).
stringify_config(_Interp, title(Text), Str, !IO) :-
    string.format("-title ""%s""", [s(Text)], Str).

:- pragma foreign_decl("C", "
    extern MR_Integer   tk_direct_thingy_counter;
").

:- pragma foreign_code("C", "
    MR_Integer  tk_direct_thingy_counter = 0;
").

:- pred get_thingy_counter(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_thingy_counter(Int::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Int = tk_direct_thingy_counter;
    IO = IO0;
").
get_thingy_counter(5, !IO).

:- pred set_thingy_counter(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_thingy_counter(Int::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    tk_direct_thingy_counter = Int;
    IO = IO0;
").
set_thingy_counter(_, !IO).

:- pred command_wrapper(pred(tcl_interp, io, io), tcl_interp,
    list(string), tcl_status, string, io, io).
:- mode command_wrapper(pred(in, di, uo) is det, in, in, out, out,
    di, uo) is det.

command_wrapper(Closure, Interp, _Args, tcl_ok, "", !IO) :-
    call(Closure, Interp, !IO).
