%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2007, 2009-2010 The University of Melbourne.
% Copyright (C) 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: browse.m.
% Author: aet.
% Stability: low.
%
% Implements a very simple term browser.
% There are a number of features that haven't been incorporated:
%
% - Scripting language that allows precise control over
%   how types are printed.
% - User preferences, which use the scripting language
%   to allow user control beyond the provided defaults.
% - Node expansion and contraction in the style of Windows Explorer.
%
%---------------------------------------------------------------------------%

:- module mdb.browse.
:- interface.

:- import_module mdb.browser_info.
:- import_module mdb.browser_term.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % The non-interactive term browser. The caller type should be either
    % `print' or `print_all'. The default portray format for that
    % caller type is used.
    %
:- pred print_browser_term(io.text_output_stream::in, browse_caller_type::in,
    browser_term::in, browser_persistent_state::in,
    io::di, io::uo) is cc_multi.

    % As above, except that the supplied format will override the default.
    %
:- pred print_browser_term_format(io.text_output_stream::in,
    browse_caller_type::in, portray_format::in, browser_term::in,
    browser_persistent_state::in, io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

    % The interactive term browser. The caller type will be `browse', and
    % the default format for the `browse' caller type will be used. Since
    % this predicate is exported to be used by C code, no browser term
    % mode function can be supplied.
    %
:- pred browse_browser_term_no_modes(io.text_input_stream::in,
    io.text_output_stream::in, browser_term::in,
    maybe_track_subterm(list(down_dir))::out,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

    % The interactive term browser. The caller type will be `browse' and
    % the default format for the `browse' caller type will be used.
    %
:- pred browse_browser_term(io.text_input_stream::in,
    io.text_output_stream::in, maybe(browser_mode_func)::in, browser_term::in,
    maybe_track_subterm(list(down_dir))::out,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

    % As above, except that the supplied format will override the default.
    % Again, this is exported to C code, so the browser term mode function
    % can't be supplied.
    %
:- pred browse_browser_term_format_no_modes(io.text_input_stream::in,
    io.text_output_stream::in, portray_format::in, browser_term::in,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

    % As above, except that the supplied format will override the default.
    %
:- pred browse_browser_term_format(io.text_input_stream::in,
    io.text_output_stream::in, portray_format::in,
    maybe(browser_mode_func)::in, browser_term::in,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

    % The browser interface for the external debugger. The caller type
    % will be `browse', and the default format will be used.
    % This version is exported for use in C code, so no browser term mode
    % function can be supplied.
    %
:- pred browse_external_no_modes(io.text_input_stream::in,
    io.text_output_stream::in, T::in,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

    % The browser interface for the external debugger. The caller type
    % will be `browse', and the default format will be used.
    %
:- pred browse_external(io.text_input_stream::in, io.text_output_stream::in,
    maybe(browser_mode_func)::in, T::in,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.parse.
:- import_module mdb.print_term.
:- import_module mdb.term_paths.

:- import_module bool.
:- import_module getopt.
:- import_module map.
:- import_module pair.
:- import_module stream.
:- import_module string.
:- import_module univ.

%---------------------------------------------------------------------------%
%
% We export these predicates to C for use by the tracer:
% they are used in trace/mercury_trace_browse.c.
%

:- pragma foreign_export("C",
    browse_browser_term_no_modes(in, in, in, out, in, out, di, uo),
    "ML_BROWSE_browse_browser_term_no_modes").
:- pragma foreign_export("C",
    browse_browser_term_format_no_modes(in, in, in, in, in, out, di, uo),
    "ML_BROWSE_browse_browser_term_format_no_modes").
:- pragma foreign_export("C",
    browse_external_no_modes(in, in, in, in, out, di, uo),
    "ML_BROWSE_browse_external_no_modes").
:- pragma foreign_export("C",
    print_browser_term(in, in, in, in, di, uo),
    "ML_BROWSE_print_browser_term").
:- pragma foreign_export("C",
    print_browser_term_format(in, in, in, in, in, di, uo),
    "ML_BROWSE_print_browser_term_format").

%---------------------------------------------------------------------------%
%
% Non-interactive display.
%

print_browser_term(OutputStream, Caller, Term, State, !IO) :-
    print_common(OutputStream, Caller, no, Term, State, !IO).

print_browser_term_format(OutputStream, Caller, Format, Term, State, !IO) :-
    print_common(OutputStream, Caller, yes(Format), Term, State, !IO).

:- pred print_common(io.text_output_stream::in, browse_caller_type::in,
    maybe(portray_format)::in, browser_term::in,
    browser_persistent_state::in, io::di, io::uo) is cc_multi.

print_common(OutputStream, Caller, MaybeFormat, BrowserTerm, State, !IO) :-
    MaybeModeFunc = no,
    Info = browser_info_init(BrowserTerm, Caller, MaybeFormat,
        MaybeModeFunc, State),
    browser_info.get_format(Info, Caller, MaybeFormat, Format),

    % For plain terms, we assume that the variable name has been printed
    % on the first part of the line. If the format is something other than
    % `flat', then we need to start on the next line.
    ( if
        BrowserTerm = plain_term(_),
        Format \= flat
    then
        io.nl(OutputStream, !IO)
    else
        true
    ),
    portray(debugger_internal(OutputStream), Caller, Format, Info, !IO).

%---------------------------------------------------------------------------%
%
% Interactive display.
%

browse_browser_term_no_modes(InputStream, OutputStream, Term, MaybeTrack,
        !State, !IO) :-
    MaybeFormat = no,
    MaybeModeFunc = no,
    browse_common(InputStream, debugger_internal(OutputStream),
        MaybeFormat, MaybeModeFunc, Term, MaybeTrack, !State, !IO).

browse_browser_term(InputStream, OutputStream, MaybeModeFunc, Term, MaybeTrack,
        !State, !IO) :-
    MaybeFormat = no,
    browse_common(InputStream, debugger_internal(OutputStream),
        MaybeFormat, MaybeModeFunc, Term, MaybeTrack, !State, !IO).

browse_browser_term_format_no_modes(InputStream, OutputStream, Format, Term,
        !State, !IO) :-
    MaybeModeFunc = no,
    browse_common(InputStream, debugger_internal(OutputStream),
        yes(Format), MaybeModeFunc, Term, _, !State, !IO).

browse_browser_term_format(InputStream, OutputStream, Format, MaybeModeFunc,
        Term, !State, !IO) :-
    browse_common(InputStream, debugger_internal(OutputStream),
        yes(Format), MaybeModeFunc, Term, _, !State, !IO).

browse_external_no_modes(InputStream, OutputStream, Term, !State, !IO) :-
    MaybeFormat = no,
    MaybeModeFunc = no,
    browse_common(InputStream, debugger_external(OutputStream),
        MaybeFormat, MaybeModeFunc, plain_term(univ(Term)), _, !State, !IO).

browse_external(InputStream, OutputStream, MaybeModeFunc, Term, !State, !IO) :-
    MaybeFormat = no,
    browse_common(InputStream, debugger_external(OutputStream),
        MaybeFormat, MaybeModeFunc, plain_term(univ(Term)), _, !State, !IO).

%---------------------%

:- pred browse_common(io.text_input_stream::in, debugger::in,
    maybe(portray_format)::in, maybe(browser_mode_func)::in, browser_term::in,
    maybe_track_subterm(list(down_dir))::out,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

browse_common(InputStream, Debugger, MaybeFormat, MaybeModeFunc, Object,
        MaybeTrack, !State, !IO) :-
    Info0 = browser_info_init(Object, browse, MaybeFormat, MaybeModeFunc,
        !.State),
    browse_main_loop(InputStream, Debugger, Info0, Info, !IO),
    MaybeTrack = Info ^ bri_maybe_track,
    !:State = Info ^ bri_state.

:- pred browse_main_loop(io.text_input_stream::in, debugger::in,
    browser_info::in, browser_info::out, io::di, io::uo) is cc_multi.

browse_main_loop(InputStream, Debugger, !Info, !IO) :-
    (
        Debugger = debugger_internal(OutputStream),
        parse.read_browser_command(InputStream, OutputStream, prompt,
            Command, !IO)
    ;
        Debugger = debugger_external(OutputStream),
        parse.read_browser_command_external(InputStream, Command, !IO)
    ),
    run_browse_cmd(Debugger, Command, Quit, !Info, !IO),
    (
        Quit = yes,
        % write_string_debugger(Debugger, "quitting...\n", !IO)
        (
            Debugger = debugger_external(_),
            send_term_to_socket(OutputStream, browser_quit, !IO)
        ;
            Debugger = debugger_internal(_)
        )
    ;
        Quit = no,
        browse_main_loop(InputStream, Debugger, !Info, !IO)
    ).

:- func prompt = string.

prompt = "browser> ".

%---------------------------------------------------------------------------%

:- pred run_browse_cmd(debugger::in, command::in, bool::out,
    browser_info::in, browser_info::out, io::di, io::uo) is cc_multi.

run_browse_cmd(Debugger, Command, !:Quit, !Info, !IO) :-
    % Please keep the code implementing commands in the same order
    % as the definition of the command type.

    % This is the default, which is overridden in some places below.
    !:Quit = no,

    % XXX The commands `set', `ls' and `print' should allow the format
    % to be specified by an option. In each case we pass a default format
    % the respective handler.
    (
        Command = cmd_print(PrintOption, MaybePath),
        run_browse_cmd_print(Debugger, browse, PrintOption, !.Info,
            MaybePath, !IO)
    ;
        Command = cmd_display,
        write_string_debugger(Debugger, "command not yet implemented\n", !IO)
    ;
        Command = cmd_write,
        write_string_debugger(Debugger, "command not yet implemented\n", !IO)
    ;
        Command = cmd_memory_addr(MaybePath),
        run_browse_cmd_memory_addr(Debugger, !.Info, MaybePath, !IO)
    ;
        Command = cmd_cd_no_path,
        set_path(root_rel([]), !Info)
    ;
        Command = cmd_cd_path(Path),
        change_dir(!.Info ^ bri_dirs, Path, NewPwd),
        deref_subterm(!.Info ^ bri_term, NewPwd, Result),
        (
            Result = deref_result(_),
            !Info ^ bri_dirs := NewPwd
        ;
            Result = deref_error(OKPath, ErrorDir),
            report_deref_error(Debugger, OKPath, ErrorDir, !IO)
        )
    ;
        Command = cmd_pwd,
        write_down_path(Debugger, !.Info ^ bri_dirs, !IO),
        nl_debugger(Debugger, !IO)
    ;
        Command = cmd_track(HowTrack, ShouldAssertInvalid, MaybePath),
        (
            MaybePath = yes(Path),
            change_dir(!.Info ^ bri_dirs, Path, NewPwd),
            deref_subterm(!.Info ^ bri_term, NewPwd, SubResult),
            (
                SubResult = deref_result(_),
                !Info ^ bri_maybe_track :=
                    track(HowTrack, ShouldAssertInvalid, NewPwd),
                !:Quit = yes
            ;
                SubResult = deref_error(_, _),
                write_string_debugger(Debugger,
                    "error: cannot track subterm\n", !IO)
            )
        ;
            MaybePath = no,
            !Info ^ bri_maybe_track :=
                track(HowTrack, ShouldAssertInvalid, !.Info ^ bri_dirs),
            !:Quit = yes
        )
    ;
        Command = cmd_mode_query(Path),
        change_dir(!.Info ^ bri_dirs, Path, NewPwd),
        MaybeModeFunc = !.Info ^ bri_maybe_mode_func,
        run_browse_cmd_mode_query(Debugger, MaybeModeFunc, NewPwd, !IO)
    ;
        Command = cmd_mode_query_no_path,
        MaybeModeFunc = !.Info ^ bri_maybe_mode_func,
        Dirs = !.Info ^ bri_dirs,
        run_browse_cmd_mode_query(Debugger, MaybeModeFunc, Dirs, !IO)
    ;
        Command = cmd_param(ParamCmd),
        run_param_command(Debugger, ParamCmd, yes, !Info, !IO)
    ;
        Command = cmd_help,
        run_browse_cmnd_help(Debugger, !IO)
    ;
        Command = cmd_quit,
        !:Quit = yes
    ;
        Command = cmd_empty
    ;
        Command = cmd_unknown,
        Msg =
            "Error: unknown command or syntax error.\n" ++
            "Type \"help\" for help.\n",
        write_string_debugger(Debugger, Msg, !IO)
    ),
    (
        Debugger = debugger_external(OutputStream),
        send_term_to_socket(OutputStream, browser_end_command, !IO)
    ;
        Debugger = debugger_internal(_)
    ).

%---------------------------------------------------------------------------%

:- pred run_browse_cmd_print(debugger::in, browse_caller_type::in,
    maybe(maybe_option_table(format_option))::in, browser_info::in,
    maybe(path)::in, io::di, io::uo) is cc_multi.

run_browse_cmd_print(Debugger, Caller, MaybeMaybeOptionTable, Info,
        MaybePath, !IO) :-
    (
        MaybeMaybeOptionTable = no,
        browser_info.get_format(Info, Caller, no, Format),
        portray_maybe_path(Debugger, Caller, Format, Info, MaybePath, !IO)
    ;
        MaybeMaybeOptionTable = yes(MaybeOptionTable),
        (
            MaybeOptionTable = ok(OptionTable),
            interpret_format_options(OptionTable, FormatResult),
            (
                FormatResult = ok(MaybeFormat),
                browser_info.get_format(Info, Caller, MaybeFormat, Format),
                portray_maybe_path(Debugger, Caller, Format, Info,
                    MaybePath, !IO)
            ;
                FormatResult = error(Msg),
                write_string_debugger(Debugger, Msg, !IO),
                write_string_debugger(Debugger, "\n", !IO)
            )
        ;
            MaybeOptionTable = error(Msg),
            write_string_debugger(Debugger, Msg, !IO),
            write_string_debugger(Debugger, "\n", !IO)
        )
    ).

%---------------------%

:- pred interpret_format_options(option_table(format_option)::in,
    maybe_error(maybe(portray_format))::out) is det.

interpret_format_options(OptionTable, MaybeMaybeFormat) :-
    map.to_assoc_list(OptionTable, OptionAssocList),
    list.filter_map(bool_format_option_is_true, OptionAssocList,
        TrueFormatOptions),
    (
        TrueFormatOptions = [],
        MaybeMaybeFormat = ok(no)
    ;
        TrueFormatOptions = [FormatOption],
        (
            FormatOption = flat,
            Format = flat
        ;
            FormatOption = raw_pretty,
            Format = raw_pretty
        ;
            FormatOption = pretty,
            Format = pretty
        ;
            FormatOption = verbose,
            Format = verbose
        ),
        MaybeMaybeFormat = ok(yes(Format))
    ;
        TrueFormatOptions = [_, _ | _],
        MaybeMaybeFormat = error("error: inconsistent format options")
    ).

:- pred bool_format_option_is_true(pair(format_option, option_data)::in,
    format_option::out) is semidet.

bool_format_option_is_true(Format - bool(yes), Format).

%---------------------------------------------------------------------------%

:- pred run_browse_cmd_memory_addr(debugger::in, browser_info::in,
    maybe(path)::in, io::di, io::uo) is cc_multi.

run_browse_cmd_memory_addr(Debugger, Info, MaybePath, !IO) :-
    Dirs0 = Info ^ bri_dirs,
    (
        MaybePath = no,
        Dirs = Dirs0
    ;
        MaybePath = yes(Path),
        change_dir(Dirs0, Path, Dirs)
    ),
    deref_subterm(Info ^ bri_term, Dirs, DerefResult),
    (
        DerefResult = deref_result(BrowserTerm),
        (
            BrowserTerm = plain_term(Univ),
            Value = univ_value(Univ),
            get_value_representation(Value, Addr),
            string.format("addr = %x\n", [i(Addr)], Str)
        ;
            BrowserTerm = synthetic_term(_, _, _),
            Str = "synthetic terms have no addresses\n"
        ),
        write_string_debugger(Debugger, Str, !IO)
    ;
        DerefResult = deref_error(OKPath, ErrorDir),
        report_deref_error(Debugger, OKPath, ErrorDir, !IO),
        nl_debugger(Debugger, !IO)
    ).

:- pred get_value_representation(T::in, int::out) is cc_multi.

:- pragma foreign_proc("C",
    get_value_representation(Value::in, Addr::out),
    [will_not_call_mercury, promise_pure],
"
    Addr = (MR_Integer) Value;
").

% Java doesn't support converting addresses to integers, so we just
% return zero. For backends other than C the debugger doesn't yet work,
% so it doesn't matter what we return.
get_value_representation(_Value, X) :-
    cc_multi_equal(0, X).

%---------------------------------------------------------------------------%

:- pred run_browse_cmd_mode_query(debugger::in, maybe(browser_mode_func)::in,
    list(down_dir)::in, io::di, io::uo) is det.

run_browse_cmd_mode_query(Debugger, MaybeModeFunc, Dirs, !IO) :-
    (
        MaybeModeFunc = yes(ModeFunc),
        Mode = ModeFunc(Dirs),
        ModeStr = browser_mode_to_string(Mode),
        write_string_debugger(Debugger, ModeStr ++ "\n", !IO)
    ;
        MaybeModeFunc = no,
        write_string_debugger(Debugger,
            "Mode information not available.\n", !IO)
    ).

:- func browser_mode_to_string(browser_term_mode) = string.

browser_mode_to_string(btm_input) = "Input".
browser_mode_to_string(btm_output) = "Output".
browser_mode_to_string(btm_not_applicable) = "Not Applicable".
browser_mode_to_string(btm_unbound) = "Unbound".

%---------------------------------------------------------------------------%

:- pred run_browse_cmnd_help(debugger::in, io::di, io::uo) is det.

run_browse_cmnd_help(Debugger, !IO) :-
    string.append_list([
"Commands are:\n",
"\t[print|p|ls] [format_options] [path]\n",
"\t               -- print the specified subterm using the `browse' params\n",
% "\t[d|display] [path]\n",
% The display command is not yet implemented
% "\t[w|write] [path]\n",
% The write command is not yet implemented
"\t[addr|memory_addr] [path]\n",
"\t               -- print the raw memory address of the specified subterm\n",
"\tcd [path]      -- cd to the specified subterm (default is root)\n",
"\tcdr n path     -- repeatedly apply the cd command n times\n",
"\tpwd            -- print the path to the current subterm\n",
% How should we document the "[a|accurate]" option on [t|track|m|mark]?
"\t[t|track] [path]\n",
"\t               -- mark the specified subterm (default is current)\n",
"\t                  for tracking, and quit\n",
"\t[m|mark] [path]\n",
"\t               -- mark the specified subterm (default is current)\n",
"\t                  for tracking, asserting for the declarative debugger\n",
"\t                  that it makes the current goal invalid\n",
"\tmode [path]    -- show the mode of the specified subterm\n",
"\t                  (default is current)\n",
"\tformat [format_options] <flat|raw-pretty|verbose|pretty>\n",
"\t               -- set the format\n",
"\tdepth [format_param_options] <n>\n",
"\tsize  [format_param_options] <n>\n",
"\twidth [format_param_options] <n>\n",
"\tlines [format_param_options] <n>\n",
"\tnum_io_actions <n>\n",
"\t               -- set the named parameter value\n",
"\tparams         -- show format and parameter values\n",
"\tquit           -- quit browser\n",
"\thelp           -- show this help message\n",
"SICStus Prolog style commands are:\n",
"\tp              -- print\n",
"\t< n            -- set depth\n",
"\t^ [path]       -- cd to the specified subterm (default is root)\n",
"\t?              -- help\n",
"\th              -- help\n",
"\n",
"-- Paths can be Unix-style or SICStus-style: /2/3/1 or ^2^3^1\n",
"\n"],
        HelpMessage),
    write_string_debugger(Debugger, HelpMessage, !IO).

%---------------------------------------------------------------------------%
%
% Display predicates.
%

:- pred show_settings(debugger::in, browser_info::in, io::di, io::uo) is det.
% XXX Why is this here? Neither this predicate, nor the rest of this section,
% is used anywhere.
% XXX There *is* a show_settings predicate in browser_info.m, which is
% similar, but not identical, to this code.
:- pragma consider_used(pred(show_settings/4)).

show_settings(Debugger, Info, !IO) :-
    show_settings_caller(Debugger, Info, browse, "Browser", !IO),
    show_settings_caller(Debugger, Info, print, "Print", !IO),
    show_settings_caller(Debugger, Info, print_all, "Printall", !IO),

    write_string_debugger(Debugger, "Current path is: ", !IO),
    write_down_path(Debugger, Info ^ bri_dirs, !IO),
    nl_debugger(Debugger, !IO),

    write_string_debugger(Debugger, "Number of I/O actions printed is: ", !IO),
    write_int_debugger(Debugger,
        get_num_printed_io_actions(Info ^ bri_state), !IO),
    nl_debugger(Debugger, !IO).

:- pred show_settings_caller(debugger::in, browser_info::in,
    browse_caller_type::in, string::in, io::di, io::uo) is det.

show_settings_caller(Debugger, Info, Caller, CallerName, !IO) :-
    browser_info.get_format(Info, Caller, no, Format),
    write_string_debugger(Debugger, CallerName ++ " default format: ", !IO),
    print_format_debugger(Debugger, Format, !IO),
    nl_debugger(Debugger, !IO),

    write_string_debugger(Debugger, pad_right("", ' ', row_name_len), !IO),
    write_string_debugger(Debugger, pad_right("depth", ' ', depth_len), !IO),
    write_string_debugger(Debugger, pad_right("size", ' ', size_len), !IO),
    write_string_debugger(Debugger, pad_right("x clip", ' ', width_len), !IO),
    write_string_debugger(Debugger, pad_right("y clip", ' ', lines_len), !IO),
    nl_debugger(Debugger, !IO),

    show_settings_caller_format(Debugger, Info, Caller, CallerName,
        flat, "flat", !IO),
    show_settings_caller_format(Debugger, Info, Caller, CallerName,
        verbose, "verbose", !IO),
    show_settings_caller_format(Debugger, Info, Caller, CallerName,
        pretty, "pretty", !IO),
    show_settings_caller_format(Debugger, Info, Caller, CallerName,
        raw_pretty, "raw_pretty", !IO),
    nl_debugger(Debugger, !IO).

:- pred show_settings_caller_format(debugger::in, browser_info::in,
    browse_caller_type::in, string::in, portray_format::in, string::in,
    io::di, io::uo) is det.

show_settings_caller_format(Debugger, Info, Caller, CallerName,
        Format, FormatName, !IO) :-
    browser_info.get_format_params(Info, Caller, Format, Params),
    write_string_debugger(Debugger,
        pad_right(CallerName ++ " " ++ FormatName ++ ":", ' ', row_name_len),
        !IO),
    write_string_debugger(Debugger,
        pad_right(" ", ' ', centering_len), !IO),
    write_string_debugger(Debugger,
        pad_right(int_to_string(Params ^ depth), ' ', depth_len), !IO),
    write_string_debugger(Debugger,
        pad_right(int_to_string(Params ^ size), ' ', size_len), !IO),
    write_string_debugger(Debugger,
        pad_right(int_to_string(Params ^ width), ' ', width_len), !IO),
    write_string_debugger(Debugger,
        pad_right(int_to_string(Params ^ lines), ' ', lines_len), !IO),
    nl_debugger(Debugger, !IO).

:- func row_name_len = int.
:- func centering_len = int.
:- func depth_len = int.
:- func size_len = int.
:- func width_len = int.
:- func lines_len = int.

row_name_len  = 30.
centering_len =  3.
depth_len     = 10.
size_len      = 10.
width_len     = 10.
lines_len     = 10.

%---------------------------------------------------------------------------%
:- end_module mdb.browse.
%---------------------------------------------------------------------------%
