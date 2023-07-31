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

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module univ.

%---------------------------------------------------------------------------%

    % The non-interactive term browser. The caller type should be either
    % `print' or `print_all'. The default portray format for that
    % caller type is used.
    %
:- pred print_browser_term(io.output_stream::in, browse_caller_type::in,
    browser_term::in, browser_persistent_state::in,
    io::di, io::uo) is cc_multi.

    % As above, except that the supplied format will override the default.
    %
:- pred print_browser_term_format(io.output_stream::in,
    browse_caller_type::in, portray_format::in, browser_term::in,
    browser_persistent_state::in, io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

    % The interactive term browser. The caller type will be `browse', and
    % the default format for the `browse' caller type will be used. Since
    % this predicate is exported to be used by C code, no browser term
    % mode function can be supplied.
    %
:- pred browse_browser_term_no_modes(io.input_stream::in, io.output_stream::in,
    browser_term::in, maybe_track_subterm(list(down_dir))::out,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

    % The interactive term browser. The caller type will be `browse' and
    % the default format for the `browse' caller type will be used.
    %
:- pred browse_browser_term(io.input_stream::in, io.output_stream::in,
    maybe(browser_mode_func)::in, browser_term::in,
    maybe_track_subterm(list(down_dir))::out,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

    % As above, except that the supplied format will override the default.
    % Again, this is exported to C code, so the browser term mode function
    % can't be supplied.
    %
:- pred browse_browser_term_format_no_modes(io.input_stream::in,
    io.output_stream::in, portray_format::in, browser_term::in,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

    % As above, except that the supplied format will override the default.
    %
:- pred browse_browser_term_format(io.input_stream::in, io.output_stream::in,
    portray_format::in, maybe(browser_mode_func)::in, browser_term::in,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

    % The browser interface for the external debugger. The caller type
    % will be `browse', and the default format will be used.
    % This version is exported for use in C code, so no browser term mode
    % function can be supplied.
    %
:- pred browse_external_no_modes(io.input_stream::in, io.output_stream::in,
    T::in, browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

    % The browser interface for the external debugger. The caller type
    % will be `browse', and the default format will be used.
    %
:- pred browse_external(io.input_stream::in, io.output_stream::in,
    maybe(browser_mode_func)::in, T::in,
    browser_persistent_state::in, browser_persistent_state::out,
    io::di, io::uo) is cc_multi.

    % Estimate the total term size, in characters, We count the number of
    % characters in the functor, plus two characters for each argument:
    % "(" and ")" for the first, and ", " for each of the rest, plus the
    % sizes of the arguments themselves. This is only approximate since it
    % doesn't take into account all the special cases such as operators.
    %
    % This predicate returns not the estimated total term size,
    % but the difference between the given maximum size the caller
    % is interested in and the estimated total term size.
    % This difference is positive if the term is smaller than the
    % maximum and negative if it is bigger. If the difference is
    % negative, term_size_left_from_max will return a negative difference
    % but the value will usually not be accurate, since in such cases
    % by definition the caller is not interested in the accurate value.
    %
:- pred term_size_left_from_max(univ::in, int::in, int::out) is cc_multi.
:- pred browser_term_size_left_from_max(browser_term::in,
    int::in, int::out) is cc_multi.

%---------------------------------------------------------------------------%

    % save_term_to_file(OutputStream, FileName, Format, BrowserTerm, !IO):
    %
    % Save BrowserTerm to the file FileName. If there is an error,
    % print an error message to OutputStream.
    %
    % The format of the saved term can be influenced by the Format
    % argument, but how this works is not specified.
    %
:- pred save_term_to_file(io.output_stream::in, string::in, string::in,
    browser_term::in, io::di, io::uo) is cc_multi.

    % save_term_to_file_xml(FileName, BrowserTerm, Out, !IO):
    %
    % Save BrowserTerm to FileName as an XML document. If there is an error,
    % print an error message to Out.
    %
:- pred save_term_to_file_xml(io.output_stream::in, string::in,
    browser_term::in, io::di, io::uo) is cc_multi.

    % Save BrowserTerm in an HTML file and launch the web browser specified
    % by the web_browser_cmd field in the browser_persistent_state.
    %
:- pred save_and_browse_browser_term_web(io.output_stream::in,
    io.output_stream::in, browser_term::in,
    browser_persistent_state::in, io::di, io::uo) is cc_multi.

    % Exported for term_to_html.
    %
:- pred browser_term_to_html_flat_string(browser_term::in, string::out,
    bool::out, io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

    % Remove "/dir/../" sequences from a list of directories to yield
    % a form that lacks ".." entries.
    % If there are more ".." entries than normal entries, we return
    % the empty list.
    %
:- pred simplify_dirs(list(up_down_dir)::in, list(down_dir)::out) is det.

    % True if the given string can be used to cd to the return value of a
    % function.
    %
:- pred string_is_return_value_alias(string::in) is semidet.

    % For use in representing unbound head variables in the "print goal"
    % commands in the debugger.
:- type unbound
    --->    '_'.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.parse.
:- import_module mdb.frame.
:- import_module mdb.sized_pretty.
:- import_module mdb.term_to_html.

:- import_module deconstruct.
:- import_module dir.
:- import_module getopt.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module pretty_printer.
:- import_module stream.
:- import_module stream.string_writer.
:- import_module string.
:- import_module string.builder.
:- import_module term_io.
:- import_module term_to_xml.
:- import_module type_desc.

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
:- pragma foreign_export("C", print_browser_term(in, in, in, in, di, uo),
    "ML_BROWSE_print_browser_term").
:- pragma foreign_export("C",
    print_browser_term_format(in, in, in, in, in, di, uo),
    "ML_BROWSE_print_browser_term_format").

:- pragma foreign_export("C", save_term_to_file(in, in, in, in, di, uo),
    "ML_BROWSE_save_term_to_file").

:- pragma foreign_export("C", save_term_to_file_xml(in, in, in, di, uo),
    "ML_BROWSE_save_term_to_file_xml").

:- pragma foreign_export("C",
    save_and_browse_browser_term_web(in, in, in, in, di, uo),
    "ML_BROWSE_save_and_browse_browser_term_web").

%---------------------------------------------------------------------------%
%
% Non-interactive display.
%

print_browser_term(OutputStream, CallerType, Term, State, !IO) :-
    print_common(OutputStream, CallerType, no, Term, State, !IO).

print_browser_term_format(OutputStream, CallerType, Format, Term, State, !IO) :-
    print_common(OutputStream, CallerType, yes(Format), Term, State, !IO).

:- pred print_common(io.output_stream::in, browse_caller_type::in,
    maybe(portray_format)::in, browser_term::in,
    browser_persistent_state::in, io::di, io::uo) is cc_multi.

print_common(OutputStream, CallerType, MaybeFormat, BrowserTerm, State, !IO) :-
    MaybeModeFunc = no,
    Info = browser_info_init(BrowserTerm, CallerType, MaybeFormat,
        MaybeModeFunc, State),
    browser_info.get_format(Info, CallerType, MaybeFormat, Format),

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
    % XXX why different from MaybeFormat?
    PortrayMaybeFormat = no,
    portray(debugger_internal(OutputStream), CallerType, PortrayMaybeFormat,
        Info, !IO).

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

:- pred browse_common(io.input_stream::in, debugger::in,
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
        parse.read_command(InputStream, OutputStream, prompt, Command, !IO)
    ;
        Debugger = debugger_external(OutputStream),
        parse.read_command_external(InputStream, Command, !IO)
    ),
    run_command(Debugger, Command, Quit, !Info, !IO),
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

:- pred run_command(debugger::in, command::in, bool::out,
    browser_info::in, browser_info::out, io::di, io::uo) is cc_multi.

run_command(Debugger, Command, Quit, !Info, !IO) :-
    % Please keep the code implementing commands in the same order
    % as the definition of the command type.

    % XXX The commands `set', `ls' and `print' should allow the format
    % to be specified by an option. In each case we instead pass `no' to
    % the respective handler.
    (
        Command = cmd_print(PrintOption, MaybePath),
        do_portray(Debugger, browse, PrintOption, !.Info, MaybePath, !IO),
        Quit = no
    ;
        Command = cmd_display,
        write_string_debugger(Debugger, "command not yet implemented\n", !IO),
        Quit = no
    ;
        Command = cmd_write,
        write_string_debugger(Debugger, "command not yet implemented\n", !IO),
        Quit = no
    ;
        Command = cmd_memory_addr(MaybePath),
        do_print_memory_addr(Debugger, !.Info, MaybePath, !IO),
        Quit = no
    ;
        Command = cmd_cd_no_path,
        set_path(root_rel([]), !Info),
        Quit = no
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
        ),
        Quit = no
    ;
        Command = cmd_pwd,
        write_down_path(Debugger, !.Info ^ bri_dirs, !IO),
        nl_debugger(Debugger, !IO),
        Quit = no
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
                Quit = yes
            ;
                SubResult = deref_error(_, _),
                write_string_debugger(Debugger,
                    "error: cannot track subterm\n", !IO),
                Quit = no
            )
        ;
            MaybePath = no,
            !Info ^ bri_maybe_track :=
                track(HowTrack, ShouldAssertInvalid, !.Info ^ bri_dirs),
            Quit = yes
        )
    ;
        Command = cmd_mode_query(Path),
        change_dir(!.Info ^ bri_dirs, Path, NewPwd),
        MaybeModeFunc = !.Info ^ bri_maybe_mode_func,
        write_term_mode_debugger(Debugger, MaybeModeFunc, NewPwd, !IO),
        Quit = no
    ;
        Command = cmd_mode_query_no_path,
        MaybeModeFunc = !.Info ^ bri_maybe_mode_func,
        write_term_mode_debugger(Debugger, MaybeModeFunc, !.Info ^ bri_dirs,
            !IO),
        Quit = no
    ;
        Command = cmd_param(ParamCmd),
        run_param_command(Debugger, ParamCmd, yes, !Info, !IO),
        Quit = no
    ;
        Command = cmd_help,
        help(Debugger, !IO),
        Quit = no
    ;
        Command = cmd_quit,
        Quit = yes
    ;
        Command = cmd_empty,
        Quit = no
    ;
        Command = cmd_unknown,
        write_string_debugger(Debugger,
            "Error: unknown command or syntax error.\n", !IO),
        write_string_debugger(Debugger, "Type \"help\" for help.\n", !IO),
        Quit = no
    ),
    (
        Debugger = debugger_external(OutputStream),
        send_term_to_socket(OutputStream, browser_end_command, !IO)
    ;
        Debugger = debugger_internal(_)
    ).

:- pred do_portray(debugger::in, browse_caller_type::in,
    maybe(maybe_option_table(format_option))::in, browser_info::in,
    maybe(path)::in, io::di, io::uo) is cc_multi.

do_portray(Debugger, CallerType, MaybeMaybeOptionTable, Info, MaybePath,
        !IO) :-
    (
        MaybeMaybeOptionTable = no,
        portray_maybe_path(Debugger, CallerType, no, Info, MaybePath, !IO)
    ;
        MaybeMaybeOptionTable = yes(MaybeOptionTable),
        (
            MaybeOptionTable = ok(OptionTable),
            interpret_format_options(OptionTable, FormatResult),
            (
                FormatResult = ok(MaybeFormat),
                portray_maybe_path(Debugger, CallerType, MaybeFormat, Info,
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

:- pred do_print_memory_addr(debugger::in, browser_info::in, maybe(path)::in,
    io::di, io::uo) is cc_multi.

do_print_memory_addr(Debugger, Info, MaybePath, !IO) :-
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
% return zero. For other backends the debugger doesn't yet work,
% so it doesn't matter what we return.
get_value_representation(_Value, X) :-
    cc_multi_equal(0, X).

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

:- pred help(debugger::in, io::di, io::uo) is det.

help(Debugger, !IO) :-
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
% Various pretty-print routines.
%

:- pred portray_maybe_path(debugger::in, browse_caller_type::in,
    maybe(portray_format)::in, browser_info::in,
    maybe(path)::in, io::di, io::uo) is cc_multi.

portray_maybe_path(Debugger, Caller, MaybeFormat, Info, MaybePath, !IO) :-
    (
        MaybePath = no,
        portray(Debugger, Caller, MaybeFormat, Info, !IO)
    ;
        MaybePath = yes(Path),
        portray_path(Debugger, Caller, MaybeFormat, Info, Path, !IO)
    ).

:- pred portray(debugger::in, browse_caller_type::in,
    maybe(portray_format)::in, browser_info::in,
    io::di, io::uo) is cc_multi.

portray(Debugger, Caller, MaybeFormat, Info, !IO) :-
    % XXX Move the next call up to caller.
    browser_info.get_format(Info, Caller, MaybeFormat, Format),
    browser_info.get_format_params(Info, Caller, Format, Params),
    deref_subterm(Info ^ bri_term, Info ^ bri_dirs, SubResult),
    (
        SubResult = deref_result(BrowserTerm),
        (
            Format = flat,
            portray_flat(Debugger, BrowserTerm, Params, !IO)
        ;
            Format = raw_pretty,
            portray_raw_pretty(Debugger, BrowserTerm, Params, !IO)
        ;
            Format = verbose,
            portray_verbose(Debugger, BrowserTerm, Params, !IO)
        ;
            Format = pretty,
            portray_pretty(Debugger, BrowserTerm, Params, !IO)
        )
    ;
        SubResult = deref_error(OKPath, ErrorDir),
        report_deref_error(Debugger, OKPath, ErrorDir, !IO)
        % write_string_debugger(Debugger, "error: no such subterm")
    ),
    nl_debugger(Debugger, !IO).

:- pred portray_path(debugger::in, browse_caller_type::in,
    maybe(portray_format)::in, browser_info::in, path::in,
    io::di, io::uo) is cc_multi.

portray_path(Debugger, Caller, MaybeFormat, Info0, Path, !IO) :-
    set_path(Path, Info0, Info),
    portray(Debugger, Caller, MaybeFormat, Info, !IO).

:- pred portray_flat(debugger::in, browser_term::in, format_params::in,
    io::di, io::uo) is cc_multi.

portray_flat(Debugger, BrowserTerm, Params, !IO) :-
    % io.write handles the special cases such as lists, operators, etc better,
    % so we prefer to use it if we can. However, io.write doesn't have
    % a depth or size limit, so we need to check the size first; if the term
    % is small enough, we use string_writer.write (actually
    % string_writer.write_univ), otherwise we use term_to_string/4.
    %
    % XXX This ignores the maximum number of lines.

    browser_term_size_left_from_max(BrowserTerm, max_print_size,
        RemainingSize),
    ( if RemainingSize >= 0 then
        portray_flat_write_browser_term(string.builder.handle, BrowserTerm,
            string.builder.init, State),
        BrowserTermStr = to_string(State)
    else
        io.get_stream_db(StreamDb, !IO),
        BrowserDb = browser_db(StreamDb),
        browser_term_to_string(BrowserDb, BrowserTerm, Params ^ size,
            Params ^ depth, BrowserTermStr)
    ),
    write_string_debugger(Debugger, BrowserTermStr, !IO).

:- pred portray_flat_write_browser_term(Stream::in, browser_term::in,
    State::di, State::uo) is cc_multi
    <= (stream.writer(Stream, string, State),
        stream.writer(Stream, character, State)).

portray_flat_write_browser_term(OutputStream, BrowserTerm, !IO) :-
    (
        BrowserTerm = plain_term(Univ),
        string_writer.write_univ(OutputStream, include_details_cc, Univ, !IO)
    ;
        BrowserTerm = synthetic_term(Functor, Args, MaybeReturn),
        put(OutputStream, Functor, !IO),
        (
            Args = []
        ;
            Args = [_ | _],
            put(OutputStream, "(", !IO),
            put_list(OutputStream, write_univ_or_unbound, put_comma_space,
                Args, !IO),
            put(OutputStream, ")", !IO)
        ),
        (
            MaybeReturn = yes(Return),
            put(OutputStream, " = ", !IO),
            string_writer.write_univ(OutputStream, include_details_cc,
                Return, !IO)
        ;
            MaybeReturn = no
        )
    ).

:- pred put_comma_space(Stream::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

put_comma_space(Stream, !State) :-
    put(Stream, ", ", !State).

:- pred portray_verbose(debugger::in, browser_term::in, format_params::in,
    io::di, io::uo) is cc_multi.

portray_verbose(Debugger, BrowserTerm, Params, !IO) :-
    io.get_stream_db(StreamDb, !IO),
    BrowserDb = browser_db(StreamDb),
    browser_term_to_string_verbose(BrowserDb, BrowserTerm, Params ^ size,
        Params ^ depth, Params ^ width, Params ^ lines, Str),
    write_string_debugger(Debugger, Str, !IO).

:- pred portray_pretty(debugger::in, browser_term::in, format_params::in,
    io::di, io::uo) is det.

portray_pretty(Debugger, BrowserTerm, Params, !IO) :-
    browser_term_to_string_pretty(Debugger, BrowserTerm, Params ^ width,
        Params ^ lines, Params ^ size, Params ^ depth, !IO).

:- pred portray_raw_pretty(debugger::in, browser_term::in, format_params::in,
    io::di, io::uo) is cc_multi.

portray_raw_pretty(Debugger, BrowserTerm, Params, !IO) :-
    io.get_stream_db(StreamDb, !IO),
    BrowserDb = browser_db(StreamDb),
    sized_pretty.browser_term_to_string_line(BrowserDb, BrowserTerm,
        Params ^ width, Params ^ lines, Str),
    write_string_debugger(Debugger, Str, !IO).

    % The maximum estimated size for which we use `io.write'.
    %
:- func max_print_size = int.

max_print_size = 60.

term_size_left_from_max(Univ, MaxSize, RemainingSize) :-
    ( if MaxSize < 0 then
        RemainingSize = MaxSize
    else
        deconstruct.limited_deconstruct_cc(univ_value(Univ), MaxSize,
            MaybeFunctorArityArgs),
        (
            MaybeFunctorArityArgs = yes({Functor, Arity, Args}),
            string.length(Functor, FunctorSize),
            % "()", plus Arity-1 times ", "
            PrincipalSize = FunctorSize + Arity * 2,
            MaxArgsSize = MaxSize - PrincipalSize,
            list.foldl(term_size_left_from_max, Args,
                MaxArgsSize, RemainingSize)
        ;
            MaybeFunctorArityArgs = no,
            RemainingSize = -1
        )
    ;
        RemainingSize = -1
    ).

browser_term_size_left_from_max(BrowserTerm, MaxSize, RemainingSize) :-
    (
        BrowserTerm = plain_term(Univ),
        term_size_left_from_max(Univ, MaxSize, RemainingSize)
    ;
        BrowserTerm = synthetic_term(Functor, Args, MaybeReturn),
        string.length(Functor, FunctorSize),
        list.length(Args, Arity),
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
        list.foldl(term_size_left_from_max, Args, MaxArgsSize, RemainingSize)
    ).

:- pred write_univ_or_unbound(Stream::in, univ::in, State::di, State::uo)
    is cc_multi
    <= (stream.writer(Stream, string, State),
        stream.writer(Stream, character, State)).

write_univ_or_unbound(Stream, Univ, !IO) :-
    ( if univ_to_type(Univ, _ `with_type` unbound) then
        put_char(Stream, '_', !IO)
    else
        string_writer.write_univ(Stream, include_details_cc, Univ, !IO)
    ).

:- pred report_deref_error(debugger::in, list(down_dir)::in, down_dir::in,
    io::di, io::uo) is det.

report_deref_error(Debugger, OKPath, ErrorDir, !IO) :-
    write_string_debugger(Debugger, "error: ", !IO),
    (
        OKPath = [_ | _],
        Context = "in subdir " ++ down_dirs_to_string(OKPath) ++ ": ",
        write_string_debugger(Debugger, Context, !IO)
    ;
        OKPath = []
    ),
    Msg = "there is no subterm " ++ down_dir_to_string(ErrorDir) ++ "\n",
    write_string_debugger(Debugger, Msg, !IO).

%---------------------------------------------------------------------------%
%
% Single-line representation of a term.
%

:- pred browser_term_to_string(browser_db::in, browser_term::in,
    int::in, int::in, string::out) is cc_multi.

browser_term_to_string(BrowserDb, BrowserTerm, MaxSize, MaxDepth, Str) :-
    CurSize = 0,
    CurDepth = 0,
    browser_term_to_string_2(BrowserDb, BrowserTerm,
        MaxSize, CurSize, _NewSize, MaxDepth, CurDepth, Str).

    % Note: When the size limit is reached, we simply display further subterms
    % compressed. This is consistent with the User's Guide, which describes
    % the size limit as a "suggested maximum".
    %
:- pred browser_term_to_string_2(browser_db::in, browser_term::in,
    int::in, int::in, int::out, int::in, int::in, string::out) is cc_multi.

browser_term_to_string_2(BrowserDb, BrowserTerm, MaxSize, CurSize, NewSize,
        MaxDepth, CurDepth, Str) :-
    limited_deconstruct_browser_term_cc(BrowserDb, BrowserTerm, MaxSize,
        MaybeFunctorArityArgs, MaybeReturn),
    ( if
        CurSize < MaxSize,
        CurDepth < MaxDepth,
        MaybeFunctorArityArgs = yes({Functor, _Arity, Args})
    then
        browser_term_to_string_3(BrowserDb, Functor, Args, MaybeReturn,
            MaxSize, CurSize, NewSize, MaxDepth, CurDepth, Str)
    else
        browser_term_compress(BrowserDb, BrowserTerm, Str),
        NewSize = CurSize
    ).

:- pred browser_term_to_string_3(browser_db::in, string::in,
    list(univ)::in, maybe(univ)::in, int::in, int::in, int::out,
    int::in, int::in, string::out) is cc_multi.

browser_term_to_string_3(BrowserDb, Functor, Args, MaybeReturn,
        MaxSize, Size0, Size, MaxDepth, Depth0, Str) :-
    ( if
        Functor = "[|]",
        Args = [ListHead, ListTail],
        MaybeReturn = no
    then
        % For the purposes of size and depth, we treat lists as if they consist
        % of one functor plus an argument for each element of the list.
        Size1 = Size0 + 1,
        Depth1 = Depth0 + 1,
        browser_term_to_string_2(BrowserDb, plain_term(ListHead),
            MaxSize, Size1, Size2, MaxDepth, Depth1, HeadStr),
        list_tail_to_string_list(BrowserDb, ListTail,
            MaxSize, Size2, Size, MaxDepth, Depth1, TailStrs),
        list.append(TailStrs, ["]"], Strs),
        string.append_list(["[", HeadStr | Strs], Str)
    else if
        Functor = "[]",
        Args = [],
        MaybeReturn = no
    then
        Size = Size0 + 1,
        Str = "[]"
    else
        Size1 = Size0 + 1,
        Depth1 = Depth0 + 1,
        args_to_string_list(BrowserDb, Args, MaxSize, Size1, Size2,
            MaxDepth, Depth1, ArgStrs),
        BracketedArgsStr = bracket_string_list(ArgStrs),
        (
            MaybeReturn = yes(Return),
            browser_term_to_string_2(BrowserDb, plain_term(Return),
                MaxSize, Size2, Size, MaxDepth, Depth1, ReturnStr),
            string.append_list([Functor, BracketedArgsStr, " = ", ReturnStr],
                Str)
        ;
            MaybeReturn = no,
            Size = Size2,
            string.append_list([Functor, BracketedArgsStr], Str)
        )
    ).

:- pred list_tail_to_string_list(browser_db::in, univ::in,
    int::in, int::in, int::out, int::in, int::in, list(string)::out)
    is cc_multi.

list_tail_to_string_list(BrowserDb, TailUniv, MaxSize, Size0, Size,
        MaxDepth, Depth0, TailStrs) :-
    % We want the limit to be at least two to ensure that the limited
    % deconstruct won't fail for any list term.
    Limit = max(MaxSize, 2),
    limited_deconstruct_browser_term_cc(BrowserDb, plain_term(TailUniv),
        Limit, MaybeFunctorArityArgs, MaybeReturn),
    (
        MaybeFunctorArityArgs = yes({Functor, _Arity, Args}),
        ( if
            Functor = "[]",
            Args = [],
            MaybeReturn = no
        then
            Size = Size0,
            TailStrs = []
        else if
            Functor = "[|]",
            Args = [ListHead, ListTail],
            MaybeReturn = no
        then
            ( if
                Size0 < MaxSize,
                Depth0 < MaxDepth
            then
                browser_term_to_string_2(BrowserDb, plain_term(ListHead),
                    MaxSize, Size0, Size1, MaxDepth, Depth0, HeadStr),
                list_tail_to_string_list(BrowserDb, ListTail, MaxSize,
                    Size1, Size, MaxDepth, Depth0, TailStrs0),
                TailStrs = [", ", HeadStr | TailStrs0]
            else
                Size = Size0,
                TailStrs = [", ..."]
            )
        else
            ( if
                Size0 < MaxSize,
                Depth0 < MaxDepth
            then
                browser_term_to_string_3(BrowserDb, Functor, Args, MaybeReturn,
                    MaxSize, Size0, Size, MaxDepth, Depth0, TailStr),
                TailStrs = [" | ", TailStr]
            else
                Size = Size0,
                browser_term_compress(BrowserDb, plain_term(TailUniv),
                    TailCompressedStr),
                TailStrs = [" | ", TailCompressedStr]
            )
        )
    ;
        MaybeFunctorArityArgs = no,
        Size = Size0,
        browser_term_compress(BrowserDb, plain_term(TailUniv),
            TailCompressedStr),
        TailStrs = [" | ", TailCompressedStr]
    ).

:- pred args_to_string_list(browser_db::in, list(univ)::in,
    int::in, int::in, int::out, int::in, int::in, list(string)::out)
    is cc_multi.

args_to_string_list(_BrowserDb, [], _MaxSize, CurSize, NewSize,
        _MaxDepth, _CurDepth, Strs) :-
    Strs = [],
    NewSize = CurSize.
args_to_string_list(BrowserDb, [Univ | Univs], MaxSize, CurSize, NewSize,
        MaxDepth, CurDepth, Strs) :-
    browser_term_to_string_2(BrowserDb, plain_term(Univ),
        MaxSize, CurSize, NewSize1, MaxDepth, CurDepth, Str),
    args_to_string_list(BrowserDb, Univs, MaxSize, NewSize1, NewSize,
        MaxDepth, CurDepth, RestStrs),
    Strs = [Str | RestStrs].

:- func bracket_string_list(list(string)) = string.

bracket_string_list(Args) = Str :-
    (
        Args = [],
        Str = ""
    ;
        Args = [HeadArg | TailArgs],
        Str = "(" ++ HeadArg ++ comma_string_list(TailArgs) ++ ")"
    ).

:- func comma_string_list(list(string)) = string.

comma_string_list([]) = "".
comma_string_list([HeadArg | TailArgs]) =
    ", " ++ HeadArg ++ comma_string_list(TailArgs).

:- pred browser_term_compress(browser_db::in, browser_term::in, string::out)
    is cc_multi.

browser_term_compress(BrowserDb, BrowserTerm, Str) :-
    functor_browser_term_cc(BrowserDb, BrowserTerm, Functor, Arity, IsFunc),
    ( if Arity = 0 then
        Str = Functor
    else
        (
            IsFunc = yes,
            string.format("%s/%d+1", [s(Functor), i(Arity)], Str)
        ;
            IsFunc = no,
            string.format("%s/%d", [s(Functor), i(Arity)], Str)
        )
    ).

%---------------------------------------------------------------------------%

    % Print using the pretty printer from the standard library.
    % XXX Because the pretty printer doesn't support a combination
    % of both size and depth, we use the depth, except when depth is 0,
    % in which case we use the size.
    %
:- pred browser_term_to_string_pretty(S::in,
    browser_term::in, int::in, int::in,
    int::in, int::in, io::di, io::uo) is det
    <= stream.writer(S, string, io).

browser_term_to_string_pretty(S, Term, Width, Lines, Size, Depth, !IO) :-
    (
        Term = plain_term(Univ),
        Doc = format_univ(Univ)
    ;
        Term = synthetic_term(Functor, Args, MaybeReturn),
        Doc = synthetic_term_to_doc(Functor, Args, MaybeReturn)
    ),
    get_default_formatter_map(Formatters, !IO),

    ( if Depth > 0 then
        Limit = triangular(Depth)
    else
        Limit = linear(Size)
    ),

    Params = pp_params(Width, Lines, Limit),
    promise_equivalent_solutions [!:IO] (
        put_doc(S, include_details_cc, Formatters, Params, Doc, !IO)
    ).

%---------------------------------------------------------------------------%

    % Verbose printing. Tree layout with numbered branches.
    % Numbering makes it easier to change to subterms.
    %
:- pred browser_term_to_string_verbose(browser_db::in, browser_term::in,
    int::in, int::in, int::in, int::in, string::out) is cc_multi.

browser_term_to_string_verbose(BrowserDb, BrowserTerm, MaxSize, MaxDepth,
        X, Y, Str) :-
    CurSize = 0,
    CurDepth = 0,
    browser_term_to_string_verbose_2(BrowserDb, BrowserTerm,
        MaxSize, CurSize, _NewSize, MaxDepth, CurDepth, Frame),
    ClippedFrame = frame.clip(X-Y, Frame),
    unlines(ClippedFrame, Str).

:- pred browser_term_to_string_verbose_2(browser_db::in, browser_term::in,
    int::in, int::in, int::out, int::in, int::in, frame::out) is cc_multi.

browser_term_to_string_verbose_2(BrowserDb, BrowserTerm,
        MaxSize, CurSize, NewSize, MaxDepth, CurDepth, Frame) :-
    limited_deconstruct_browser_term_cc(BrowserDb, BrowserTerm, MaxSize,
        MaybeFunctorArityArgs, MaybeReturn),
    ( if
        CurSize < MaxSize,
        CurDepth < MaxDepth,
        MaybeFunctorArityArgs = yes({Functor, _Arity, Args0})
    then
        % XXX We should consider formatting function terms differently.
        (
            MaybeReturn = yes(Return),
            list.append(Args0, [Return], Args)
        ;
            MaybeReturn = no,
            Args = Args0
        ),
        CurSize1 = CurSize + 1,
        CurDepth1 = CurDepth + 1,
        ArgNum = 1,
        args_to_string_verbose_list(BrowserDb, Args, ArgNum,
            MaxSize, CurSize1, NewSize, MaxDepth, CurDepth1, ArgsFrame),
        Frame = frame.vglue([Functor], ArgsFrame)
    else
        browser_term_compress(BrowserDb, BrowserTerm, Line),
        Frame = [Line],
        NewSize = CurSize
    ).

:- pred args_to_string_verbose_list(browser_db::in, list(univ)::in,
    int::in, int::in, int::in, int::out, int::in, int::in, frame::out)
    is cc_multi.

args_to_string_verbose_list(_BrowserDb, [], _ArgNum,
        _MaxSize, CurSize, NewSize, _MaxDepth, _CurDepth, []) :-
    NewSize = CurSize.
args_to_string_verbose_list(BrowserDb, [Univ], ArgNum,
        MaxSize, CurSize, NewSize, MaxDepth, CurDepth, Frame) :-
    browser_term_to_string_verbose_2(BrowserDb, plain_term(Univ), MaxSize,
        CurSize, NewSize, MaxDepth, CurDepth, TreeFrame),
    % XXX: ArgNumS must have fixed length 2.
    string.int_to_string(ArgNum, ArgNumS),
    string.append_list([ArgNumS, "-"], LastBranchS),
    Frame = frame.hglue([LastBranchS], TreeFrame).
args_to_string_verbose_list(BrowserDb, [Univ1, Univ2 | Univs], ArgNum, MaxSize,
        CurSize, NewSize, MaxDepth, CurDepth, Frame) :-
    browser_term_to_string_verbose_2(BrowserDb, plain_term(Univ1),
        MaxSize, CurSize, NewSize1, MaxDepth, CurDepth, TreeFrame),
    ArgNum1 = ArgNum + 1,
    args_to_string_verbose_list(BrowserDb, [Univ2 | Univs], ArgNum1,
        MaxSize, NewSize1, NewSize2, MaxDepth, CurDepth, RestTreesFrame),
    NewSize = NewSize2,
    % XXX: ArgNumS must have fixed length 2.
    string.int_to_string(ArgNum, ArgNumS),
    string.append_list([ArgNumS, "-"], BranchFrameS),
    Height = frame.vsize(TreeFrame) - 1,
    list.duplicate(Height, "|", VBranchFrame),
    LeftFrame = frame.vglue([BranchFrameS], VBranchFrame),
    TopFrame = frame.hglue(LeftFrame, TreeFrame),
    Frame = frame.vglue(TopFrame, RestTreesFrame).

:- pred unlines(list(string)::in, string::out) is det.

unlines([], "").
unlines([Line | Lines], Str) :-
    string.append(Line, "\n", NLine),
    unlines(Lines, Strs),
    string.append(NLine, Strs, Str).

%---------------------------------------------------------------------------%
%
% Miscellaneous path handling.
%

:- type deref_result(T)
    --->    deref_result(T)
    ;       deref_error(list(down_dir), down_dir).

    % We assume a root-relative path. We assume Term is the entire term
    % passed into browse/3, not a subterm.
    %
:- pred deref_subterm(browser_term::in, list(down_dir)::in,
    deref_result(browser_term)::out) is cc_multi.

deref_subterm(BrowserTerm, Path, Result) :-
    (
        BrowserTerm = plain_term(Univ),
        deref_subterm_2(Univ, Path, [], SubResult),
        deref_result_univ_to_browser_term(SubResult, Result)
    ;
        BrowserTerm = synthetic_term(_Functor, Args, MaybeReturn),
        (
            Path = [],
            SubBrowserTerm = BrowserTerm,
            Result = deref_result(SubBrowserTerm)
        ;
            Path = [Step | PathTail],
            ( if
                (
                    Step = down_child_num(N),
                    ( if
                        N = list.length(Args) + 1,
                        MaybeReturn = yes(ReturnValue)
                    then
                        ArgUniv = ReturnValue
                    else
                        % The first argument of a non-array
                        % is numbered argument 1.
                        list.index1(Args, N, ArgUniv)
                    )
                ;
                    Step = down_child_name(Name),
                    string_is_return_value_alias(Name),
                    MaybeReturn = yes(ArgUniv)
                )
            then
                deref_subterm_2(ArgUniv, PathTail, [Step], SubResult),
                deref_result_univ_to_browser_term(SubResult, Result)
            else
                Result = deref_error([], Step)
            )
        )
    ).

:- pred deref_result_univ_to_browser_term(deref_result(univ)::in,
    deref_result(browser_term)::out) is det.

deref_result_univ_to_browser_term(SubResult, Result) :-
    (
        SubResult = deref_result(SubUniv),
        SubBrowserTerm = plain_term(SubUniv),
        Result = deref_result(SubBrowserTerm)
    ;
        SubResult = deref_error(OKPath, ErrorDir),
        Result = deref_error(OKPath, ErrorDir)
    ).

:- pred deref_subterm_2(univ::in, list(down_dir)::in, list(down_dir)::in,
    deref_result(univ)::out) is cc_multi.

deref_subterm_2(Univ, Path, RevPath0, Result) :-
    (
        Path = [],
        Result = deref_result(Univ)
    ;
        Path = [Dir | Dirs],
        (
            Dir = down_child_num(N),
            ( if
                TypeCtor = type_ctor(univ_type(Univ)),
                type_ctor_name(TypeCtor) = "array",
                type_ctor_module_name(TypeCtor) = "array"
            then
                % The first element of an array is at index zero.
                arg_cc(univ_value(Univ), N, MaybeValue)
            else
                % The first argument of a non-array is numbered argument 1
                % by the user but argument 0 by deconstruct.argument.
                arg_cc(univ_value(Univ), N - 1, MaybeValue)
            )
        ;
            Dir = down_child_name(Name),
            named_arg_cc(univ_value(Univ), Name, MaybeValue)
        ),
        (
            MaybeValue = arg(Value),
            ArgN = univ(Value),
            deref_subterm_2(ArgN, Dirs, [Dir | RevPath0], Result)
        ;
            MaybeValue = no_arg,
            Result = deref_error(list.reverse(RevPath0), Dir)
        )
    ).

%---------------------------------------------------------------------------%

:- pred set_path(path::in, browser_info::in, browser_info::out) is det.

set_path(NewPath, !Info) :-
    Dirs0 = !.Info ^ bri_dirs,
    change_dir(Dirs0, NewPath, Dirs),
    !Info ^ bri_dirs := Dirs.

:- pred change_dir(list(down_dir)::in, path::in, list(down_dir)::out) is det.

change_dir(PwdDirs, Path, RootRelDirs) :-
    (
        Path = root_rel(Dirs),
        NewDirs = Dirs
    ;
        Path = dot_rel(Dirs),
        NewDirs = down_to_up_down_dirs(PwdDirs) ++ Dirs
    ),
    simplify_dirs(NewDirs, RootRelDirs).

%---------------------------------------------------------------------------%
%
% Saving terms to files.
%

save_term_to_file(OutputStream, FileName, _Format, BrowserTerm, !IO) :-
    trace [compile_time(flag("debug_save_term_to_file")), io(!TIO)] (
        io.format(OutputStream, "%s\n", [s(FileName)], !TIO),
        io.write_line(OutputStream, BrowserTerm, !TIO)
    ),
    io.open_output(FileName, FileStreamRes, !IO),
    (
        FileStreamRes = ok(FileStream),
        (
            BrowserTerm = plain_term(Term),
            save_univ(FileStream, 0, Term, !IO),
            io.nl(FileStream, !IO)
        ;
            BrowserTerm = synthetic_term(Functor, Args, MaybeRes),
            io.write_string(FileStream, Functor, !IO),
            io.write_string(FileStream, "(\n", !IO),
            save_args(FileStream, 1, Args, !IO),
            io.write_string(FileStream, "\n)\n", !IO),
            (
                MaybeRes = no
            ;
                MaybeRes = yes(Result),
                io.write_string(FileStream, "=\n", !IO),
                save_univ(FileStream, 1, Result, !IO),
                io.write_string(FileStream, "\n", !IO)
            )
        ),
        io.close_output(FileStream, !IO)
    ;
        FileStreamRes = error(Error),
        io.error_message(Error, Msg),
        io.write_string(OutputStream, Msg, !IO)
    ).

:- type xml_predicate_wrapper
    --->    predicate(
                predicate_name      :: string,
                predicate_arguments :: list(univ)
            ).

:- type xml_function_wrapper
    --->    function(
                function_name       :: string,
                function_arguments  :: list(univ),
                return_value        :: univ
            ).

save_term_to_file_xml(OutputStream, FileName, BrowserTerm, !IO) :-
    maybe_save_term_to_file_xml(FileName, BrowserTerm, Result, !IO),
    (
        Result = ok(_)
    ;
        Result = error(Error),
        io.error_message(Error, Msg),
        io.format(OutputStream, "%s\n", [s(Msg)], !IO)
    ).

:- pred maybe_save_term_to_file_xml(string::in, browser_term::in,
    io.res(io.output_stream)::out, io::di, io::uo) is cc_multi.

maybe_save_term_to_file_xml(FileName, BrowserTerm, FileStreamRes, !IO) :-
    io.open_output(FileName, FileStreamRes, !IO),
    (
        FileStreamRes = ok(OutputStream),
        (
            BrowserTerm = plain_term(Univ),
            Term = univ_value(Univ),
            term_to_xml.write_xml_doc_general_cc(OutputStream, Term, simple,
                no_stylesheet,  no_dtd, _, !IO)
        ;
            BrowserTerm = synthetic_term(Functor, Args, MaybeRes),
            (
                MaybeRes = no,
                PredicateTerm = predicate(Functor, Args),
                term_to_xml.write_xml_doc_general_cc(OutputStream,
                    PredicateTerm, simple, no_stylesheet, no_dtd, _, !IO)
            ;
                MaybeRes = yes(Result),
                FunctionTerm = function(Functor, Args, Result),
                term_to_xml.write_xml_doc_general_cc(OutputStream,
                    FunctionTerm, simple, no_stylesheet, no_dtd, _, !IO)
            )
        ),
        io.close_output(OutputStream, !IO)
    ;
        FileStreamRes = error(_)
    ).

%---------------------------------------------------------------------------%

save_and_browse_browser_term_web(OutputStream, ErrorStream, Term, State,
        !IO) :-
    get_mdb_dir(MaybeMdbDir, !IO),
    (
        MaybeMdbDir = yes(MdbDir),
        MaybeBrowserCmd = State ^ web_browser_cmd,
        (
            MaybeBrowserCmd = yes(BrowserCmd),
            io.get_temp_directory(TmpDir, !IO),
            io.make_temp_file(TmpDir, "mdb", ".html", TmpResult, !IO),
            (
                TmpResult = ok(TmpFileName0),
                ( if string.suffix(TmpFileName0, ".html") then
                    TmpFileName = TmpFileName0
                else
                    % Work around io.make_temp_file ignoring suffix.
                    io.remove_file(TmpFileName0, _, !IO),
                    TmpFileName = TmpFileName0 ++ ".html"
                ),
                save_term_to_file_web(TmpFileName, Term, MdbDir,
                    SaveResult, !IO),
                (
                    SaveResult = ok(_),
                    % We should actually quote the file name.
                    CommandStr = BrowserCmd ++ " " ++ TmpFileName,
                    launch_web_browser(OutputStream, ErrorStream,
                        CommandStr, !IO)
                ;
                    SaveResult = error(Error),
                    io.error_message(Error, Msg),
                    io.format(ErrorStream,
                        "Error opening file `%s': %s\n",
                        [s(TmpFileName), s(Msg)], !IO)
                )
            ;
                TmpResult = error(Error),
                io.error_message(Error, Msg),
                io.format(ErrorStream,
                    "Error opening temporary file: %s\n",
                    [s(Msg)], !IO)
            )
        ;
            MaybeBrowserCmd = no,
            io.write_string(ErrorStream,
                "mdb: You need to specify the shell command that launches " ++
                "your preferred web browser, by issuing an mdb command " ++
                "\"web_browser_cmd <command>\".\n", !IO)
        )
    ;
        MaybeMdbDir = no,
        io.write_string(ErrorStream,
            "Could not determine directory containing mdb files.\n", !IO)
    ).

:- pred get_mdb_dir(maybe(string)::out, io::di, io::uo) is det.

get_mdb_dir(Res, !IO) :-
    get_environment_var("MERCURY_DEBUGGER_INIT", MaybeValue, !IO),
    ( if
        MaybeValue = yes(Path),
        dir.path_name_is_absolute(Path),
        dir.split_name(Path, MdbDir, "mdbrc")
    then
        Res = yes(MdbDir)
    else
        Res = no
    ).

:- pred save_term_to_file_web(string::in, browser_term::in, string::in,
    io.res(io.output_stream)::out, io::di, io::uo) is cc_multi.

save_term_to_file_web(FileName, BrowserTerm, MdbDir, FileStreamRes,
        !IO) :-
    io.open_output(FileName, FileStreamRes, !IO),
    (
        FileStreamRes = ok(OutputStream),
        term_to_html.write_html_doc(OutputStream, BrowserTerm, MdbDir, _, !IO),
        io.close_output(OutputStream, !IO)
    ;
        FileStreamRes = error(_)
    ).

:- pred launch_web_browser(io.output_stream::in, io.output_stream::in,
    string::in, io::di, io::uo) is det.

launch_web_browser(OutputStream, ErrorStream, CommandStr, !IO) :-
    io.write_string(OutputStream, "Launching web browser...\n", !IO),
    io.flush_output(OutputStream, !IO),
    io.call_system_return_signal(CommandStr, Result, !IO),
    (
        Result = ok(ExitStatus),
        (
            ExitStatus = exited(ExitCode),
            ( if ExitCode = 0 then
                true
            else
                io.format(ErrorStream,
                    "mdb: The command `%s' terminated with " ++
                    "a non-zero exit code.\n",
                    [s(CommandStr)], !IO)
            )
        ;
            ExitStatus = signalled(_),
            io.write_string(ErrorStream, "mdb: The browser was killed.\n", !IO)
        )
    ;
        Result = error(Error),
        io.format(ErrorStream, "mdb: Error launching browser: %s.\n",
            [s(string.string(Error))], !IO)
    ).

browser_term_to_html_flat_string(BrowserTerm, Str, Elided, !IO) :-
    % Mimic portray_flat. We can afford larger sizes in a web browser due to
    % proportional fonts and horizontal scrolling.
    MaxTermSize = 120,
    browser_term_size_left_from_max(BrowserTerm, MaxTermSize, RemainingSize),
    ( if RemainingSize >= 0 then
        portray_flat_write_browser_term(string.builder.handle, BrowserTerm,
            string.builder.init, State),
        Str = to_string(State),
        Elided = no
    else
        io.get_stream_db(StreamDb, !IO),
        BrowserDb = browser_db(StreamDb),
        MaxSize = 10,
        MaxDepth = 5,
        browser_term_to_string(BrowserDb, BrowserTerm, MaxSize, MaxDepth, Str),
        Elided = yes
    ).

%---------------------------------------------------------------------------%

:- pred save_univ(io.text_output_stream::in, int::in, univ::in,
    io::di, io::uo) is cc_multi.

save_univ(OutputStream, Indent, Univ, !IO) :-
    save_term(OutputStream, Indent, univ_value(Univ), !IO).

:- pred save_term(io.text_output_stream::in, int::in, T::in,
    io::di, io::uo) is cc_multi.

save_term(OutputStream, Indent, Term, !IO) :-
    ( if dynamic_cast_to_list(Term, List) then
        (
            List = [],
            write_indent(OutputStream, Indent, !IO),
            io.write_string(OutputStream, "[]", !IO)
        ;
            List = [_ | _],
            MakeUniv =
                ( func(Element) = (ElementUniv) :-
                    ElementUniv = univ(Element)
                ),
            Univs = list.map(MakeUniv, List),
            write_indent(OutputStream, Indent, !IO),
            io.write_string(OutputStream, "[\n", !IO),
            save_args(OutputStream, Indent + 1, Univs, !IO),
            io.write_string(OutputStream, "\n", !IO),
            write_indent(OutputStream, Indent, !IO),
            io.write_string(OutputStream, "]", !IO)
        )
    else
        deconstruct(Term, include_details_cc, Functor, _Arity, Args),
        write_indent(OutputStream, Indent, !IO),
        io.write_string(OutputStream, Functor, !IO),
        (
            Args = []
        ;
            Args = [_ | _],
            io.write_string(OutputStream, "(\n", !IO),
            save_args(OutputStream, Indent + 1, Args, !IO),
            io.write_string(OutputStream, "\n", !IO),
            write_indent(OutputStream, Indent, !IO),
            io.write_string(OutputStream, ")", !IO)
        )
    ).

:- pred save_args(io.text_output_stream::in, int::in, list(univ)::in,
    io::di, io::uo) is cc_multi.

save_args(_OutputStream, _Indent, [], !IO).
save_args(OutputStream, Indent, [Univ | Univs], !IO) :-
    save_univ(OutputStream, Indent, Univ, !IO),
    (
        Univs = []
    ;
        Univs = [_ | _],
        io.write_string(OutputStream, ",\n", !IO),
        save_args(OutputStream, Indent, Univs, !IO)
    ).

:- pred write_indent(io.text_output_stream::in, int::in, io::di, io::uo)
    is det.

write_indent(OutputStream, Indent, !IO) :-
    ( if Indent =< 0 then
        true
    else
        io.write_char(OutputStream, ' ', !IO),
        write_indent(OutputStream, Indent - 1, !IO)
    ).

:- some [T2] pred dynamic_cast_to_list(T1::in, list(T2)::out) is semidet.

dynamic_cast_to_list(X, L) :-
    % The code of this predicate is copied from pprint.m.
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ `with_type` ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L `with_type` list(ArgType)).

%---------------------------------------------------------------------------%
%
% Display predicates.
%

:- pred show_settings(debugger::in, browser_info::in, io::di, io::uo) is det.

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

simplify_dirs(Dirs, SimpleDirs) :-
    list.reverse(Dirs, RevDirs),
    simplify_rev_dirs(RevDirs, 0, [], SimpleDirs).

    % simplify_rev_dirs(RevUpDownDirs, ToDelete, !DownDirs):
    %
    % Assumes a reverse list of directories and removes redundant `..'
    % entries by scanning from the bottom most directory to the top,
    % counting how many `..' occurred (!.ToDelete) and removing entries
    % accordingly. !DownDirs accumulates the simplified dirs processed so far
    % so we can be tail recursive.
    %
:- pred simplify_rev_dirs(list(up_down_dir)::in, int::in,
    list(down_dir)::in, list(down_dir)::out) is det.

simplify_rev_dirs([], _, !DownDirs).
simplify_rev_dirs([RevUpDownDir | RevUpDownDirs], !.ToDelete, !DownDirs) :-
    (
        RevUpDownDir = updown_parent,
        !:ToDelete = !.ToDelete + 1
    ;
        (
            RevUpDownDir = updown_child_num(ChildNum),
            DownDir = down_child_num(ChildNum)
        ;
            RevUpDownDir = updown_child_name(ChildName),
            DownDir = down_child_name(ChildName)
        ),
        ( if !.ToDelete > 0 then
            !:ToDelete = !.ToDelete - 1
        else
            !:DownDirs = [DownDir | !.DownDirs]
        )
    ),
    simplify_rev_dirs(RevUpDownDirs, !.ToDelete, !DownDirs).

:- func down_dir_to_string(down_dir) = string.

down_dir_to_string(down_child_num(Num)) = int_to_string(Num).
down_dir_to_string(down_child_name(Name)) = Name.

:- func down_dirs_to_string(list(down_dir)) = string.

down_dirs_to_string([]) = "".
down_dirs_to_string([Dir | Dirs]) = DirStr :-
    (
        Dirs = [],
        DirStr = down_dir_to_string(Dir)
    ;
        Dirs = [_ | _],
        DirStr = down_dir_to_string(Dir) ++ "/" ++ down_dirs_to_string(Dirs)
    ).

string_is_return_value_alias("r").
string_is_return_value_alias("res").
string_is_return_value_alias("rv").
string_is_return_value_alias("result").
string_is_return_value_alias("return").
string_is_return_value_alias("ret").

%---------------------------------------------------------------------------%

:- pred write_term_mode_debugger(debugger::in, maybe(browser_mode_func)::in,
    list(down_dir)::in, io::di, io::uo) is det.

write_term_mode_debugger(Debugger, MaybeModeFunc, Dirs, !IO) :-
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

    % These two functions are just like pprint.to_doc, except their input
    % is not a natural term, but a synthetic term defined by a functor, a list
    % of arguments, and if the synthetic term is a function application, then
    % the result of that function application.
    %
    % The functor name has to be treated specially because '.'s therein
    % usually denote separators in a module qualified name; the
    % default pretty_printer formatter does not know this and will quote
    % such names.
    %
:- func synthetic_term_to_doc(string, list(univ), maybe(univ)) = doc.

synthetic_term_to_doc(Functor0, Args, MaybeReturn) = Doc :-
    ( if
        ( Functor0 = "!."
        ; Functor0 = "."
        ; Functor0 = ".."
        ; Functor0 = "=.."
        ; not string.contains_char(Functor0, ('.'))
        )
    then
        Doc0 = format_term(Functor0, Args)
    else
        FunctorDoc =
            qualified_functor_to_doc(string.split_at_char(('.'), Functor0)),
        (
            Args = [],
            Doc0 = FunctorDoc
        ;
            Args = [_ | _],
            Doc0 = indent([
                FunctorDoc, str("("),
                    format_list(Args, group([str(", "), nl])),
                str(")")
            ])
        )
    ),
    (
        MaybeReturn = no,
        Doc = Doc0
    ;
        MaybeReturn = yes(Return),
        Doc = docs([Doc0, str(" = "), format_arg(format_univ(Return))])
    ).

%---------------------------------------------------------------------------%

:- func qualified_functor_to_doc(list(string)) = doc.

qualified_functor_to_doc([]) = str("").
qualified_functor_to_doc([Part]) = str(term_io.quoted_atom(Part)).
qualified_functor_to_doc([PartA, PartB | Parts]) =
    docs([str(term_io.quoted_atom(PartA)), str("."),
        qualified_functor_to_doc([PartB | Parts])]).

%---------------------------------------------------------------------------%
:- end_module mdb.browse.
%---------------------------------------------------------------------------%
