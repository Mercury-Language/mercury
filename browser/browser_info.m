%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2007, 2009-2010 The University of Melbourne.
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: browser_info.m
% Main author: Mark Brown
%
% Basic data structures used by the browser.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.browser_info.
:- interface.

:- import_module mdb.browser_term.
:- import_module mdb.parse.
:- import_module mdb.term_rep.
:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.

:- import_module bool.
:- import_module getopt.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module stream.
:- import_module univ.

%---------------------------------------------------------------------------%

    % The non-persistent browser information. A new one of these is created
    % every time the browser is called, based on the contents of the persistent
    % state, and lasts for the duration of the call.
    %
:- type browser_info
    --->    browser_info(
                % The term to browse.
                bri_term        :: browser_term,

                % The list of directories to take, starting from the root,
                % to reach the current subterm.
                bri_dirs        :: list(down_dir),

                % What command called the browser?
                bri_caller_type :: browse_caller_type,

                % Format specified as an option to the mdb command.
                bri_format      :: maybe(portray_format),

                % Persistent settings.
                bri_state       :: browser_persistent_state,

                % Location of subterm for which the `track' or `mark' command
                % was given, or `no_track' if no `track' command was given.
                bri_maybe_track :: maybe_track_subterm(list(down_dir)),

                % An optional function to determine the mode of a particular
                % subterm should the user issue a `mode' query.
                bri_maybe_mode_func :: maybe(browser_mode_func)
            ).

:- type maybe_track_subterm(P)
    --->    no_track
    ;       track(how_track_subterm, should_assert_invalid, P).

:- type how_track_subterm
    --->    track_accurate
    ;       track_fast.

:- type should_assert_invalid
    --->    assert_invalid
    ;       no_assert_invalid.

    % A signature for functions that can be used by the browser to work
    % out the mode of a sub-term.
    %
:- type browser_mode_func == (func(list(down_dir)) = browser_term_mode).

    % The possible modes of a sub-term in the browser. Note these do not
    % correspond directly with the declared Mercury modes.
    %
:- type browser_term_mode
    --->    btm_input
            % The sub-term is bound at the call. For example the Mercury
            % builtin modes `in', `di' and `ui'.

    ;       btm_output
            % The sub-term is unbound at the call. The call succeeded
            % and bound the sub-term. For example the Mercury builtin modes
            % `out' and `uo'.

    ;       btm_unbound
            % The sub-term is unbound at the call and at the final EXIT, FAIL
            % or EXCP event.

    ;       btm_not_applicable.
            % If the user asks about the mode of an atom, this value should be
            % returned by the browser term mode function.

:- type up_down_dir
    --->    updown_parent
    ;       updown_child_num(int)
    ;       updown_child_name(string).

:- type down_dir
    --->    down_child_num(int)
    ;       down_child_name(string).

:- func down_to_up_down_dir(down_dir) = up_down_dir.
:- func down_to_up_down_dirs(list(down_dir)) = list(up_down_dir).

:- pred convert_dirs_to_term_path(term_rep::in, list(down_dir)::in,
    term_path::out) is det.

    % The browser is required to behave differently for different caller
    % circumstances. The following type enumerates the various possibilities.
    %
:- type browse_caller_type
    --->    print
            % Non-interactively called via mdb's `print' command,
            % to print a single value.

    ;       browse
            % Interactively called via mdb's `browse' command.

    ;       print_all.
            % Non-interactively called via mdb's `print *' command,
            % to print one of a sequence of values.

    % The various ways of representing terms by the browser.
    %
:- type portray_format
    --->    flat
    ;       raw_pretty  % Calls pprint module directly, without first
                        % attempting to manipulate the term in any way.
    ;       verbose
    ;       pretty.     % It allows the user to specify the maximum number
                        % of lines which the term has to be printed within.

:- type format_params
    --->    format_params(
                depth   :: int,
                size    :: int,
                width   :: int,
                lines   :: int
            ).

:- type setting
    --->    setting_depth(int)
    ;       setting_size(int)
    ;       setting_width(int)
    ;       setting_lines(int)
    ;       setting_format(portray_format).

    % Initialise a new browser_info. The optional portray_format
    % overrides the default format.
    %
:- func init(browser_term, browse_caller_type,
    maybe(portray_format), maybe(browser_mode_func),
    browser_persistent_state) = browser_info.

    % Get the format to use for the given caller type. The optional
    % portray_format overrides the current default.
    %
:- pred get_format(browser_info::in, browse_caller_type::in,
    maybe(portray_format)::in, portray_format::out) is det.

    % Get the format parameters for the given caller type and format.
    %
:- pred get_format_params(browser_info::in, browse_caller_type::in,
    portray_format::in, format_params::out) is det.

:- pred info_set_browse_param(option_table(setting_option)::in,
    setting::in, browser_info::in, browser_info::out) is det.

:- pred info_set_num_io_actions(int::in,
    browser_info::in, browser_info::out) is det.

:- pred info_set_xml_browser_cmd(string::in,
    browser_info::in, browser_info::out) is det.

:- pred info_set_xml_tmp_filename(string::in,
    browser_info::in, browser_info::out) is det.

:- pred info_set_web_browser_cmd(string::in,
    browser_info::in, browser_info::out) is det.

%---------------------------------------------------------------------------%

    % A data type that holds persistent browser settings.
    % This state must be saved by the caller of the browse module
    % between calls.
    %
:- type browser_persistent_state.

:- func browser_persistent_state ^ xml_browser_cmd = maybe(string).
:- func browser_persistent_state ^ xml_browser_cmd := maybe(string) =
    browser_persistent_state.

:- func browser_persistent_state ^ xml_tmp_filename = maybe(string).
:- func browser_persistent_state ^ xml_tmp_filename := maybe(string) =
    browser_persistent_state.

:- func browser_persistent_state ^ web_browser_cmd = maybe(string).
:- func browser_persistent_state ^ web_browser_cmd := maybe(string) =
    browser_persistent_state.

    % Initialize the persistent browser state with default values.
    %
:- pred init_persistent_state(browser_persistent_state).
:- mode init_persistent_state(out) is det.

:- func get_num_printed_io_actions(browser_persistent_state) = int.
:- pred get_num_io_actions(browser_persistent_state::in, int::out) is det.
:- pred set_num_io_actions(int::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.

    % Update a setting in the browser state. The first argument should be
    % true iff the set command is invoked from within the browser. The next
    % seven arguments indicate the presence of the `set' options
    % -P, -B, -A, -f, -r, -v and -p, in that order.
    %
:- pred set_browser_param(bool::in, bool::in, bool::in, bool::in,
    bool::in, bool::in, bool::in, bool::in, setting::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.

    % As above but the first argument specifies where the browser was
    % invoked from.
    %
:- pred set_browser_param_with_caller_type(browse_caller_type::in,
    bool::in, bool::in, bool::in, bool::in, bool::in, bool::in, bool::in,
    setting::in, browser_persistent_state::in, browser_persistent_state::out)
    is det.

    % Update a setting in the browser state. The first argument should be
    % true iff the set command is invoked from within the browser. The next
    % argument indicates the presence of at most one of the options
    % -P, -B, -A, while the next four indicate the presence of -f, -r, -v
    % and -p, in that order.
    %
:- pred set_browser_param_maybe_caller_type(bool::in,
    maybe(browse_caller_type)::in,
    bool::in, bool::in, bool::in, bool::in, setting::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.

    % set_param_from_option_table(CallerType, OptionTable, Setting, !State).
    %
    % Same as set_param/11, but looks up the options in the supplied
    % option table.
    %
:- pred set_browser_param_from_option_table(browse_caller_type::in,
    option_table(setting_option)::in, setting::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.

:- type param_cmd
    --->    format(maybe_option_table(setting_option), setting)
    ;       format_param(maybe_option_table(setting_option), setting)
    ;       num_io_actions(int)
    ;       print_params.
% We can't set the browser command from within the browser because we parse
% user commands from the browser by breaking them up into words at whitespace,
% which doesn't respect quotation marks. Since the browser command will usually
% include spaces, this parsing method would need to be changed before we could
% include xml_browser_cmd here. And until we handle xml_browser_cmd, there is
% no point in handling xml_tmp_filename.
%
%   ;       xml_browser_cmd(string)
%   ;       xml_tmp_filename(string)

:- type debugger
    --->    debugger_internal
    ;       debugger_external.

% If the term browser is called from the internal debugger, input is
% done via a call to the readline library (if available), using streams
% MR_mdb_in and MR_mdb_out. If it is called from the external debugger,
% Input/Output are done via MR_debugger_socket_in/MR_debugger_socket_out.
% In the latter case we need to output terms; their type is
% term_browser_response.

:- type term_browser_response
    --->    browser_str(string)
    ;       browser_int(int)
    ;       browser_nl
    ;       browser_end_command
    ;       browser_quit.

:- instance stream.stream(debugger, io).
:- instance stream.output(debugger, io).
:- instance stream.writer(debugger, string, io).
:- instance stream.writer(debugger, int, io).

:- pred run_param_command(debugger::in, param_cmd::in, bool::in,
    browser_info::in, browser_info::out, io::di, io::uo) is det.

:- pred show_settings(debugger::in, bool::in, browser_info::in,
    io::di, io::uo) is det.

:- pred nl_debugger(debugger::in, io::di, io::uo) is det.

:- pred write_string_debugger(debugger::in, string::in, io::di, io::uo) is det.

:- pred write_int_debugger(debugger::in, int::in, io::di, io::uo) is det.

:- pred print_format_debugger(debugger::in, portray_format::in,
    io::di, io::uo) is det.

:- pred write_down_path(debugger::in, list(down_dir)::in,
    io::di, io::uo) is det.

:- pred send_term_to_socket(term_browser_response::in, io::di, io::uo) is det.

:- pred browser_params_to_string(browser_persistent_state::in, bool::in,
    string::out) is det.

%---------------------------------------------------------------------------%

% These three predicates are like the deconstruct, limited_deconstruct
% and functor procedures in deconstruct, except
%
% - they implicitly specify include_details_cc, and
% - they work on browser_terms instead of plain terms.
%
% The latter difference requires them to have an extra argument (the last).
% For deconstruct and limited_deconstruct, this returns the return value
% if the browser term represents a function call. For functor, it says
% whether the browser term represents a function call.

:- type browser_db
    --->    browser_db(
                browser_stream_db   :: io.stream_db
            ).

:- pred deconstruct_browser_term_cc(browser_db::in, browser_term::in,
    string::out, int::out, list(univ)::out, maybe(univ)::out) is cc_multi.

:- pred limited_deconstruct_browser_term_cc(browser_db::in, browser_term::in,
    int::in, maybe({string, int, list(univ)})::out, maybe(univ)::out)
    is cc_multi.

:- pred functor_browser_term_cc(browser_db::in, browser_term::in, string::out,
    int::out, bool::out) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module deconstruct.
:- import_module require.
:- import_module string.
:- import_module type_desc.

%---------------------------------------------------------------------------%

down_to_up_down_dir(down_child_num(Num)) = updown_child_num(Num).
down_to_up_down_dir(down_child_name(Name)) = updown_child_name(Name).

down_to_up_down_dirs([]) = [].
down_to_up_down_dirs([DownDir | DownDirs]) =
    [down_to_up_down_dir(DownDir) | down_to_up_down_dirs(DownDirs)].

%---------------------------------------------------------------------------%

convert_dirs_to_term_path(Term, Dirs, TermPath) :-
    (
        Dirs = [],
        TermPath = []
    ;
        Dirs = [down_child_num(N) | DirsTail],
        ( if
            term_rep.argument(Term, N, Subterm)
        then
            convert_dirs_to_term_path(Subterm, DirsTail, TermPathTail)
        else
            unexpected($module, $pred, "invalid argument")
        ),
        TermPath = [N | TermPathTail]
    ;
        Dirs =  [down_child_name(Name) | DirsTail],
        ( if
            term_rep.field_pos(Name, Term, Pos),
            term_rep.argument(Term, Pos, Subterm)
        then
            convert_dirs_to_term_path(Subterm, DirsTail, TermPathTail),
            N = Pos
        else
            unexpected($module, $pred, "invalid field name")
        ),
        TermPath = [N | TermPathTail]
    ).

%---------------------------------------------------------------------------%

init(BrowserTerm, CallerType, MaybeFormat, MaybeModeFunc, State) =
    browser_info(BrowserTerm, [], CallerType, MaybeFormat, State, no_track,
        MaybeModeFunc).

get_format(Info, Caller, MaybeFormat, Format) :-
    (
        MaybeFormat = yes(Format)
    ;
        MaybeFormat = no,
        MdbFormatOption = Info ^ bri_format,
        (
            MdbFormatOption = yes(Format)
        ;
            MdbFormatOption = no,
            get_caller_params(Info ^ bri_state, Caller, Params),
            Format = Params ^ default_format
        )
    ).

get_format_params(Info, Caller, Format, Params) :-
    get_caller_params(Info ^ bri_state, Caller, CallerParams),
    get_caller_format_params(CallerParams, Format, Params).

info_set_browse_param(OptionTable, Setting, !Info) :-
    PersistentState0 = !.Info ^ bri_state,
    CallerType = !.Info ^ bri_caller_type,
    set_browser_param_from_option_table(CallerType, OptionTable, Setting,
        PersistentState0, PersistentState),
    !Info ^ bri_state := PersistentState.

info_set_num_io_actions(N, !Info) :-
    PersistentState0 = !.Info ^ bri_state,
    set_num_io_actions(N, PersistentState0, PersistentState),
    !Info ^ bri_state := PersistentState.

info_set_xml_browser_cmd(Cmd, !Info) :-
    PersistentState0 = !.Info ^ bri_state,
    set_xml_browser_cmd_from_mdb(Cmd, PersistentState0, PersistentState),
    !Info ^ bri_state := PersistentState.

info_set_xml_tmp_filename(FileName, !Info) :-
    PersistentState0 = !.Info ^ bri_state,
    set_xml_tmp_filename_from_mdb(FileName, PersistentState0, PersistentState),
    !Info ^ bri_state := PersistentState.

info_set_web_browser_cmd(Cmd, !Info) :-
    PersistentState0 = !.Info ^ bri_state,
    set_web_browser_cmd_from_mdb(Cmd, PersistentState0, PersistentState),
    !Info ^ bri_state := PersistentState.

%---------------------------------------------------------------------------%

%
% The following exported predicates are a convenient way to
% call set_param from C code.
%

:- pred set_depth_from_mdb(bool::in, bool::in, bool::in,
    bool::in, bool::in, bool::in, bool::in, int::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.
:- pragma foreign_export("C",
    set_depth_from_mdb(in, in, in, in, in, in, in, in, in, out),
    "ML_BROWSE_set_depth_from_mdb").

set_depth_from_mdb(P, B, A, F, Pr, V, NPr, Depth, !Browser) :-
    set_browser_param(no, P, B, A, F, Pr, V, NPr, setting_depth(Depth),
        !Browser).

:- pred set_size_from_mdb(bool::in, bool::in, bool::in, bool::in,
    bool::in, bool::in, bool::in, int::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.
:- pragma foreign_export("C",
    set_size_from_mdb(in, in, in, in, in, in, in, in, in, out),
    "ML_BROWSE_set_size_from_mdb").

set_size_from_mdb(P, B, A, F, Pr, NPr, V, Size, !Browser) :-
    set_browser_param(no, P, B, A, F, Pr, V, NPr, setting_size(Size),
        !Browser).

:- pred set_width_from_mdb(bool::in, bool::in, bool::in,
    bool::in, bool::in, bool::in, bool::in, int::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.
:- pragma foreign_export("C",
    set_width_from_mdb(in, in, in, in, in, in, in, in, in, out),
    "ML_BROWSE_set_width_from_mdb").

set_width_from_mdb(P, B, A, F, Pr, V, NPr, Width, !Browser) :-
    set_browser_param(no, P, B, A, F, Pr, V, NPr, setting_width(Width),
        !Browser).

:- pred set_lines_from_mdb(bool::in, bool::in, bool::in,
    bool::in, bool::in, bool::in, bool::in, int::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.
:- pragma foreign_export("C",
    set_lines_from_mdb(in, in, in, in, in, in, in, in, in, out),
    "ML_BROWSE_set_lines_from_mdb").

set_lines_from_mdb(P, B, A, F, Pr, V, NPr, Lines, !Browser) :-
    set_browser_param(no, P, B, A, F, Pr, V, NPr, setting_lines(Lines),
        !Browser).

:- pred set_format_from_mdb(bool::in, bool::in, bool::in, portray_format::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.
:- pragma foreign_export("C",
    set_format_from_mdb(in, in, in, in, in, out),
    "ML_BROWSE_set_format_from_mdb").

set_format_from_mdb(P, B, A, Format, !Browser) :-
    % Any format flags are ignored for this parameter.
    set_browser_param(no, P, B, A, no, no, no, no, setting_format(Format),
        !Browser).

:- pred get_xml_browser_cmd_from_mdb(browser_persistent_state::in,
    string::out) is det.
:- pragma foreign_export("C", get_xml_browser_cmd_from_mdb(in, out),
    "ML_BROWSE_get_xml_browser_cmd_from_mdb").

get_xml_browser_cmd_from_mdb(Browser, Command) :-
    MaybeCommand = Browser ^ xml_browser_cmd,
    (
        MaybeCommand = no,
        Command = ""
    ;
        MaybeCommand = yes(Command)
    ).

:- pred set_xml_browser_cmd_from_mdb(string::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.
:- pragma foreign_export("C", set_xml_browser_cmd_from_mdb(in, in, out),
    "ML_BROWSE_set_xml_browser_cmd_from_mdb").

set_xml_browser_cmd_from_mdb(Command, !Browser) :-
    ( if Command = "" then
        !Browser ^ xml_browser_cmd := no
    else
        !Browser ^ xml_browser_cmd := yes(Command)
    ).

:- pred get_xml_tmp_filename_from_mdb(browser_persistent_state::in,
    string::out) is det.
:- pragma foreign_export("C", get_xml_tmp_filename_from_mdb(in, out),
    "ML_BROWSE_get_xml_tmp_filename_from_mdb").

get_xml_tmp_filename_from_mdb(Browser, FileName) :-
    MaybeFileName = Browser ^ xml_tmp_filename,
    (
        MaybeFileName = no,
        FileName = ""
    ;
        MaybeFileName = yes(FileName)
    ).

:- pred set_xml_tmp_filename_from_mdb(string::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.
:- pragma foreign_export("C", set_xml_tmp_filename_from_mdb(in, in, out),
    "ML_BROWSE_set_xml_tmp_filename_from_mdb").

set_xml_tmp_filename_from_mdb(FileName, !Browser) :-
    ( if FileName = "" then
        !Browser ^ xml_tmp_filename := no
    else
        !Browser ^ xml_tmp_filename := yes(FileName)
    ).

:- pred get_web_browser_cmd_from_mdb(browser_persistent_state::in,
    string::out) is det.
:- pragma foreign_export("C", get_web_browser_cmd_from_mdb(in, out),
    "ML_BROWSE_get_web_browser_cmd_from_mdb").

get_web_browser_cmd_from_mdb(Browser, Command) :-
    MaybeCommand = Browser ^ web_browser_cmd,
    (
        MaybeCommand = no,
        Command = ""
    ;
        MaybeCommand = yes(Command)
    ).

:- pred set_web_browser_cmd_from_mdb(string::in,
    browser_persistent_state::in, browser_persistent_state::out) is det.
:- pragma foreign_export("C", set_web_browser_cmd_from_mdb(in, in, out),
    "ML_BROWSE_set_web_browser_cmd_from_mdb").

set_web_browser_cmd_from_mdb(Command, !Browser) :-
    ( if Command = "" then
        !Browser ^ web_browser_cmd := no
    else
        !Browser ^ web_browser_cmd := yes(Command)
    ).

%---------------------------------------------------------------------------%
%
% The following functions allow C code to create Mercury values of type bool.
%

:- func mercury_bool_yes = bool.
:- pragma foreign_export("C", mercury_bool_yes = out,
    "ML_BROWSE_mercury_bool_yes").

mercury_bool_yes = yes.

:- func mercury_bool_no = bool.
:- pragma foreign_export("C", mercury_bool_no = out,
    "ML_BROWSE_mercury_bool_no").

mercury_bool_no = no.

%---------------------------------------------------------------------------%

:- type browser_persistent_state
    --->    browser_persistent_state(
                print_params            :: caller_params,
                browse_params           :: caller_params,
                print_all_params        :: caller_params,
                num_printed_io_actions  :: int,

                % The command to launch the user's preferred XML browser.
                xml_browser_cmd         :: maybe(string),

                % The file to save XML to before launching the browser.
                xml_tmp_filename        :: maybe(string),

                % The command to launch the user's preferred web browser.
                web_browser_cmd         :: maybe(string)
            ).

:- type caller_params
    --->    caller_params(
                default_format          :: portray_format,
                flat_params             :: format_params,
                raw_pretty_params       :: format_params,
                verbose_params          :: format_params,
                pretty_params           :: format_params
            ).

:- pragma foreign_export("C", init_persistent_state(out),
    "ML_BROWSE_init_persistent_state").

    % Initialise the persistent settings with default values. The rationale
    % for the default values is:
    %
    %   Depth and Size:
    %       For non-interactive display, these are 3 and 10 respectively,
    %       so that terms will generally fit on one line. For interactive
    %       browsing, these values are increased.
    %
    %   Width:
    %       Defaults to 80 characters in any situation.
    %
    %   Lines:
    %       If one term is printed then it is limited to 25 lines.
    %       If there can be more than one term (i.e., with `print *') then
    %       a much lower limit is imposed. For verbose format, there is
    %       not much point setting this to less than about 5, since otherwise
    %       very little of the term will be shown.
    %
init_persistent_state(State) :-
    Print = caller_type_print_defaults,
    Browse = caller_type_browse_defaults,
    PrintAll = caller_type_print_all_defaults,
    State = browser_persistent_state(Print, Browse, PrintAll,
        num_printed_io_actions_default, no, no, no).

:- func caller_type_print_defaults = caller_params.

caller_type_print_defaults = Params :-
    DefaultFormat = flat,
    Flat      = format_params(3, 10, 80, 25),
    RawPretty = format_params(3, 10, 80, 25),
    Verbose   = format_params(3, 10, 80, 25),
    Pretty    = format_params(3, 10, 80, 25),
    Params = caller_params(DefaultFormat, Flat, RawPretty, Verbose, Pretty).

:- func caller_type_browse_defaults = caller_params.

caller_type_browse_defaults = Params :-
    DefaultFormat = flat,
    Flat      = format_params(10, 30, 80, 25),
    RawPretty = format_params(10, 30, 80, 25),
    Verbose   = format_params(10, 30, 80, 25),
    Pretty    = format_params(10, 30, 80, 25),
    Params = caller_params(DefaultFormat, Flat, RawPretty, Verbose, Pretty).

:- func caller_type_print_all_defaults = caller_params.

caller_type_print_all_defaults = Params :-
    DefaultFormat = flat,
    Flat      = format_params(3, 10, 80, 2),
    RawPretty = format_params(3, 10, 80, 2),
    Verbose   = format_params(3, 10, 80, 5),
    Pretty    = format_params(3, 10, 80, 2),
    Params = caller_params(DefaultFormat, Flat, RawPretty, Verbose, Pretty).

:- func num_printed_io_actions_default = int.

% Since each I/O action typically takes one line, this usually leaves room
% on the typical 24-line screen for the atom, the query, and some previous
% context.
num_printed_io_actions_default = 20.

get_num_printed_io_actions(State) =
    State ^ num_printed_io_actions.

:- pragma foreign_export("C", get_num_io_actions(in, out),
    "ML_BROWSE_get_num_io_actions").

get_num_io_actions(Browser, NumIOActions) :-
    NumIOActions = Browser ^ num_printed_io_actions.

:- pragma foreign_export("C", set_num_io_actions(in, in, out),
    "ML_BROWSE_set_num_io_actions").

set_num_io_actions(NumIOActions, !Browser) :-
    !Browser ^ num_printed_io_actions := NumIOActions.

set_browser_param(FromBrowser, P0, B0, A0, F0, Pr0, V0, NPr0, Setting,
        !State) :-
    (
        FromBrowser = no,
        default_all_yes(P0, B0, A0, P, B, A)
    ;
        FromBrowser = yes,
        ( if
            P0 = no,
            B0 = no,
            A0 = no
        then
            affected_caller_types(FromBrowser, no, P, B, A)
        else
            P = P0,
            B = B0,
            A = A0
        )
    ),
    default_all_yes(F0, Pr0, V0, NPr0, F, Pr, V, NPr),
    PParams0 = !.State ^ print_params,
    BParams0 = !.State ^ browse_params,
    AParams0 = !.State ^ print_all_params,
    maybe_set_param(P, F, Pr, V, NPr, Setting, PParams0, PParams),
    maybe_set_param(B, F, Pr, V, NPr, Setting, BParams0, BParams),
    maybe_set_param(A, F, Pr, V, NPr, Setting, AParams0, AParams),
    !:State = browser_persistent_state(PParams, BParams, AParams,
        !.State ^ num_printed_io_actions,
        !.State ^ xml_browser_cmd, !.State ^ xml_tmp_filename,
        !.State ^ web_browser_cmd).

set_browser_param_with_caller_type(CallerType, P0, B0, A0, F0, Pr0, V0, NPr0,
        Setting, !State) :-
    ( if
        P0 = no,
        B0 = no,
        A0 = no
    then
        % The value of DummyInBrowser doesn't matter because the second
        % argument of the call to affected_caller_types/5 is yes/1.
        DummyInBrowser = yes,
        affected_caller_types(DummyInBrowser, yes(CallerType), P, B, A)
    else
        P = P0,
        B = B0,
        A = A0
    ),
    default_all_yes(F0, Pr0, V0, NPr0, F, Pr, V, NPr),
    PParams0 = !.State ^ print_params,
    BParams0 = !.State ^ browse_params,
    AParams0 = !.State ^ print_all_params,
    maybe_set_param(P, F, Pr, V, NPr, Setting, PParams0, PParams),
    maybe_set_param(B, F, Pr, V, NPr, Setting, BParams0, BParams),
    maybe_set_param(A, F, Pr, V, NPr, Setting, AParams0, AParams),
    !:State = browser_persistent_state(PParams, BParams, AParams,
        !.State ^ num_printed_io_actions,
        !.State ^ xml_browser_cmd, !.State ^ xml_tmp_filename,
        !.State ^ web_browser_cmd).

set_browser_param_maybe_caller_type(FromBrowser, MaybeCallerType,
        F0, Pr0, V0, NPr0, Setting, !State) :-
    affected_caller_types(FromBrowser, MaybeCallerType, P, B, A),
    set_browser_param(FromBrowser, P, B, A, F0, Pr0, V0, NPr0, Setting,
        !State).

set_browser_param_from_option_table(CallerType, OptionTable, Setting,
        !State) :-
    set_browser_param_with_caller_type(CallerType,
        lookup_bool_option(OptionTable, set_print)      : bool,
        lookup_bool_option(OptionTable, set_browse)     : bool,
        lookup_bool_option(OptionTable, set_print_all)  : bool,
        lookup_bool_option(OptionTable, set_flat)       : bool,
        lookup_bool_option(OptionTable, set_raw_pretty) : bool,
        lookup_bool_option(OptionTable, set_verbose)    : bool,
        lookup_bool_option(OptionTable, set_pretty)     : bool,
        Setting, !State).

:- pred affected_caller_types(bool::in, maybe(browse_caller_type)::in,
    bool::out, bool::out, bool::out) is det.

    % If no caller type is specified, the set command by default applies
    % to *all* caller types if invoked from the mdb prompt, and to the browser
    % only if invoked from the browser prompt.
affected_caller_types(no, no,            yes, yes, yes).
affected_caller_types(yes, no,           no, yes, no).
affected_caller_types(_, yes(print),     yes, no, no).
affected_caller_types(_, yes(browse),    no, yes, no).
affected_caller_types(_, yes(print_all), no, no, yes).

:- pred default_all_yes(bool::in, bool::in, bool::in,
    bool::out, bool::out, bool::out) is det.

default_all_yes(A0, B0, C0, A, B, C) :-
    % If none of the flags are set, the command by default
    % applies to _all_ caller types/formats.
    ( if
        A0 = no,
        B0 = no,
        C0 = no
    then
        A = yes,
        B = yes,
        C = yes
    else
        A = A0,
        B = B0,
        C = C0
    ).

:- pred default_all_yes(bool::in, bool::in, bool::in, bool::in,
    bool::out, bool::out, bool::out, bool::out) is det.

default_all_yes(A0, B0, C0, D0, A, B, C, D) :-
    % If none of the format flags are set, the command by default
    % applies to _all_ formats.
    ( if
        A0 = no,
        B0 = no,
        C0 = no,
        D0 = no
    then
        A = yes,
        B = yes,
        C = yes,
        D = yes
    else
        A = A0,
        B = B0,
        C = C0,
        D = D0
    ).

:- pred maybe_set_param(bool::in, bool::in, bool::in, bool::in, bool::in,
    setting::in, caller_params::in, caller_params::out) is det.

maybe_set_param(no, _, _, _, _, _, !Params).
maybe_set_param(yes, F, Pr, V, NPr, Setting, !Params) :-
    (
        Setting = setting_format(NewFormat),
        !Params ^ default_format := NewFormat
    ;
        ( Setting = setting_depth(_)
        ; Setting = setting_width(_)
        ; Setting = setting_lines(_)
        ; Setting = setting_size(_)
        ),
        Format0 = !.Params ^ default_format,
        FParams0 = !.Params ^ flat_params,
        PrParams0 = !.Params ^ raw_pretty_params,
        VParams0 = !.Params ^ verbose_params,
        NPrParams0 = !.Params ^ pretty_params,
        maybe_set_param_2(F, Setting, FParams0, FParams),
        maybe_set_param_2(Pr, Setting, PrParams0, PrParams),
        maybe_set_param_2(V, Setting, VParams0, VParams),
        maybe_set_param_2(NPr, Setting, NPrParams0, NPrParams),
        !:Params = caller_params(Format0,
            FParams, PrParams, VParams, NPrParams)
    ).

:- pred maybe_set_param_2(bool::in, setting::in,
    format_params::in, format_params::out) is det.

maybe_set_param_2(no, _, Params, Params).
maybe_set_param_2(yes, setting_depth(D), Params, Params ^ depth := D).
maybe_set_param_2(yes, setting_size(S), Params, Params ^ size := S).
maybe_set_param_2(yes, setting_format(_), _, _) :-
    unexpected($module, $pred, "cannot set format here").
maybe_set_param_2(yes, setting_width(W), Params, Params ^ width := W).
maybe_set_param_2(yes, setting_lines(L), Params, Params ^ lines := L).

:- pred get_caller_params(browser_persistent_state::in, browse_caller_type::in,
    caller_params::out) is det.

get_caller_params(State, print, State ^ print_params).
get_caller_params(State, browse, State ^ browse_params).
get_caller_params(State, print_all, State ^ print_all_params).

:- pred get_caller_format_params(caller_params::in, portray_format::in,
    format_params::out) is det.

get_caller_format_params(Params, flat, Params ^ flat_params).
get_caller_format_params(Params, raw_pretty, Params ^ raw_pretty_params).
get_caller_format_params(Params, verbose, Params ^ verbose_params).
get_caller_format_params(Params, pretty, Params ^ pretty_params).

%---------------------------------------------------------------------------%

run_param_command(Debugger, ParamCmd, ShowPath, !PersistentState, !IO) :-
    (
        ParamCmd = format(MaybeOptionTable, Setting),
        (
            MaybeOptionTable = ok(OptionTable),
            info_set_browse_param(OptionTable, Setting, !PersistentState)
        ;
            MaybeOptionTable = error(Msg),
            write_string_debugger(Debugger, Msg, !IO)
        )
    ;
        ParamCmd = format_param(MaybeOptionTable, Setting),
        (
            MaybeOptionTable = ok(OptionTable),
            info_set_browse_param(OptionTable, Setting, !PersistentState)
        ;
            MaybeOptionTable = error(Msg),
            write_string_debugger(Debugger, Msg, !IO)
        )
    ;
        ParamCmd = num_io_actions(N),
        info_set_num_io_actions(N, !PersistentState)
    ;
        ParamCmd = print_params,
        show_settings(Debugger, ShowPath, !.PersistentState, !IO)
%   ;
%       ParamCmd = xml_browser_cmd(Cmd),
%       set_xml_browser_cmd(Cmd, !PersistentState)
%   ;
%       ParamCmd = xml_tmp_filename(FileName),
%       set_xml_tmp_filename(FileName, !PersistentState)
    ).

%---------------------------------------------------------------------------%
%
% Display predicates.
%

show_settings(Debugger, ShowPath, Info, !IO) :-
    show_settings_caller(Debugger, Info, browse, "Browser", !IO),
    show_settings_caller(Debugger, Info, print, "Print", !IO),
    show_settings_caller(Debugger, Info, print_all, "Printall", !IO),

    write_string_debugger(Debugger,
        "Number of I/O actions printed is: ", !IO),
    write_int_debugger(Debugger,
        get_num_printed_io_actions(Info ^ bri_state), !IO),
    nl_debugger(Debugger, !IO),

    (
        ShowPath = yes,
        write_string_debugger(Debugger, "Current path is: ", !IO),
        write_down_path(Debugger, Info ^ bri_dirs, !IO),
        nl_debugger(Debugger, !IO)
    ;
        ShowPath = no
    ).

:- pred show_settings_caller(debugger::in, browser_info::in,
    browse_caller_type::in, string::in, io::di, io::uo) is det.

show_settings_caller(Debugger, Info, Caller, CallerName, !IO) :-
    browser_info.get_format(Info, Caller, no, Format),
    write_string_debugger(Debugger, CallerName ++ " default format: ", !IO),
    print_format_debugger(Debugger, Format, !IO),
    nl_debugger(Debugger, !IO),

    write_string_debugger(Debugger, pad_right("", ' ', row_name_len), !IO),
    write_string_debugger(Debugger, pad_right(" ", ' ', centering_len), !IO),
    write_string_debugger(Debugger, pad_right("depth", ' ', depth_len), !IO),
    write_string_debugger(Debugger, pad_right("size", ' ', size_len), !IO),
    write_string_debugger(Debugger, pad_right("width", ' ', width_len), !IO),
    write_string_debugger(Debugger, pad_right("lines", ' ', lines_len), !IO),
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

nl_debugger(debugger_internal, !IO) :-
    io.nl(!IO).
nl_debugger(debugger_external, !IO) :-
    send_term_to_socket(browser_nl, !IO).

write_string_debugger(debugger_internal, String, !IO) :-
    io.write_string(String, !IO).
write_string_debugger(debugger_external, String, !IO) :-
    send_term_to_socket(browser_str(String), !IO).

write_int_debugger(debugger_internal, Int, !IO) :-
    io.write_int(Int, !IO).
write_int_debugger(debugger_external, Int, !IO) :-
    send_term_to_socket(browser_int(Int), !IO).

print_format_debugger(debugger_internal, X, !IO) :-
    io.print(X, !IO).
print_format_debugger(debugger_external, X, !IO) :-
    (
        X = flat,
        send_term_to_socket(browser_str("flat"), !IO)
    ;
        X = raw_pretty,
        send_term_to_socket(browser_str("raw_pretty"), !IO)
    ;
        X = verbose,
        send_term_to_socket(browser_str("verbose"), !IO)
    ;
        X = pretty,
        send_term_to_socket(browser_str("pretty"), !IO)
    ).

%---------------------------------------------------------------------------%

write_down_path(Debugger, Dirs, !IO) :-
    (
        Dirs = [],
        % If the whole path is empty, we print a top level "/".
        % This is the difference between write_down_path and
        % write_down_path_loop.
        write_string_debugger(Debugger, "/", !IO)
    ;
        Dirs = [HeadDir | TailDirs],
        write_down_step(Debugger, HeadDir, !IO),
        write_down_path_loop(Debugger, TailDirs, !IO)
    ).

:- pred write_down_path_loop(debugger::in, list(down_dir)::in,
    io::di, io::uo) is det.

write_down_path_loop(Debugger, Dirs, !IO) :-
    (
        Dirs = []
    ;
        Dirs = [HeadDir | TailDirs],
        write_down_step(Debugger, HeadDir, !IO),
        write_down_path_loop(Debugger, TailDirs, !IO)
    ).

:- pred write_down_step(debugger::in, down_dir::in, io::di, io::uo) is det.

write_down_step(Debugger, Dir, !IO) :-
    (
        Dir = down_child_num(N),
        write_string_debugger(Debugger, "/", !IO),
        write_int_debugger(Debugger, N, !IO)
    ;
        Dir = down_child_name(Name),
        write_string_debugger(Debugger, "/", !IO),
        write_string_debugger(Debugger, Name, !IO)
    ).

%---------------------------------------------------------------------------%

send_term_to_socket(Term, !IO) :-
    write(Term, !IO),
    print(".\n", !IO),
    flush_output(!IO).

%---------------------------------------------------------------------------%

:- instance stream.stream(debugger, io) where [
    stream.name(_, "debugger", !IO)
].

:- instance stream.output(debugger, io) where [
    (flush(debugger_internal, !IO) :-
        io.flush_output(!IO)
    ),
    (flush(debugger_external, !IO) :-
        % XXX
        true
    )
].

:- instance stream.writer(debugger, string, io) where [
    (put(D, S, !IO) :-
        write_string_debugger(D, S, !IO)
    )
].

:- instance stream.writer(debugger, int, io) where [
    (put(D, I, !IO) :-
        write_int_debugger(D, I, !IO)
    )
].

%---------------------------------------------------------------------------%

:- pragma foreign_export("C", browser_params_to_string(in, in, out),
    "ML_BROWSE_browser_params_to_string").

browser_params_to_string(Browser, MDBCommandFormat, Desc) :-
    Browser = browser_persistent_state(PrintParams, BrowseParams,
        PrintAllParams, NumIOActions, MaybeXMLBrowserCmd, MaybeXMLTmpFileName,
        MaybeWebBrowserCmd),
    (
        MDBCommandFormat = yes,
        ParamCmds =
            caller_params_to_mdb_command("-P ", PrintParams) ++
            caller_params_to_mdb_command("-B ", BrowseParams) ++
            caller_params_to_mdb_command("-A ", PrintAllParams),
        NumIOActionCmd =
            "max_io_actions " ++ int_to_string(NumIOActions) ++ "\n",
        ( if
            MaybeXMLBrowserCmd = yes(XMLBrowserCmd),
            % XMLBrowserCmd shouldn't be "" if MaybeXMLBrowserCmd is yes,
            % but better safe than sorry.
            XMLBrowserCmd \= ""
        then
            XMLBrowserCmdCmd =
                "xml_browser_cmd " ++ XMLBrowserCmd ++ "\n"
        else
            XMLBrowserCmdCmd = ""
        ),
        ( if
            MaybeXMLTmpFileName = yes(XMLTmpFileName),
            % XMLTmpFileName shouldn't be "" if MaybeXMLTmpFileName is yes,
            % but better safe than sorry.
            XMLTmpFileName \= ""
        then
            XMLTmpFileNameCmd =
                "xml_tmp_filename " ++ XMLTmpFileName ++ "\n"
        else
            XMLTmpFileNameCmd = ""
        ),
        ( if
            MaybeWebBrowserCmd = yes(WebBrowserCmd),
            WebBrowserCmd \= ""
        then
            WebBrowserCmdCmd =
                "web_browser_cmd " ++ WebBrowserCmd ++ "\n"
        else
            WebBrowserCmdCmd = ""
        ),
        Desc = ParamCmds ++ NumIOActionCmd ++
            XMLBrowserCmdCmd ++ XMLTmpFileNameCmd ++
            WebBrowserCmdCmd
    ;
        MDBCommandFormat = no,
        ParamDesc =
            "Print parameters:\n" ++
            caller_params_to_desc(PrintParams) ++
            "Browse parameters:\n" ++
            caller_params_to_desc(BrowseParams) ++
            "Print all parameters:\n" ++
            caller_params_to_desc(PrintAllParams),
        NumIOActionDesc =
            "Maximum number of I/O actions printed: " ++
                int_to_string(NumIOActions) ++ "\n",
        (
            MaybeXMLBrowserCmd = yes(XMLBrowserCmd),
            XMLBrowserCmdDesc =
                "XML browser command:    " ++ XMLBrowserCmd ++ "\n"
        ;
            MaybeXMLBrowserCmd = no,
            XMLBrowserCmdDesc = ""
        ),
        (
            MaybeXMLTmpFileName = yes(XMLTmpFileName),
            XMLTmpFileNameDesc =
                "XML temporary filename: " ++ XMLTmpFileName ++ "\n"
        ;
            MaybeXMLTmpFileName = no,
            XMLTmpFileNameDesc = ""
        ),
        Desc = ParamDesc ++ NumIOActionDesc ++
            XMLBrowserCmdDesc ++ XMLTmpFileNameDesc
    ).

:- func caller_params_to_mdb_command(string, caller_params) = string.

caller_params_to_mdb_command(CallerOpt, CallerParams) = Cmds :-
    CallerParams = caller_params(Format, FlatParams, RawPrettyParams,
        VerboseParams, PrettyParams),
    FormatCmd = "format " ++ CallerOpt ++ format_to_string(Format) ++ "\n",
    CmdPrefix = "format_param " ++ CallerOpt,
    FormatParamCmds =
        format_params_to_mdb_command(CmdPrefix ++ "-f ", FlatParams) ++
        format_params_to_mdb_command(CmdPrefix ++ "-r ", RawPrettyParams) ++
        format_params_to_mdb_command(CmdPrefix ++ "-v ", VerboseParams) ++
        format_params_to_mdb_command(CmdPrefix ++ "-p ", PrettyParams),
    Cmds = FormatCmd ++ FormatParamCmds.

:- func caller_params_to_desc(caller_params) = string.

caller_params_to_desc(caller_params(Format, FlatParams, RawPrettyParams,
        VerboseParams, PrettyParams)) =
    "default format " ++ format_to_string(Format) ++ "\n" ++
    "flat parameters:       " ++ format_params_to_desc(FlatParams) ++
    "raw_pretty parameters: " ++ format_params_to_desc(RawPrettyParams) ++
    "verbose parameters:    " ++ format_params_to_desc(VerboseParams) ++
    "pretty parameters:     " ++ format_params_to_desc(PrettyParams).

:- func format_params_to_mdb_command(string, format_params) = string.

format_params_to_mdb_command(CmdCallerOpt, FormatParams) = Cmds :-
    FormatParams = format_params(Depth, Size, Width, Lines),
    DepthCmd = CmdCallerOpt ++ "depth " ++ int_to_string(Depth) ++ "\n",
    SizeCmd  = CmdCallerOpt ++ "size "  ++ int_to_string(Size) ++ "\n",
    WidthCmd = CmdCallerOpt ++ "width " ++ int_to_string(Width) ++ "\n",
    LinesCmd = CmdCallerOpt ++ "lines " ++ int_to_string(Lines) ++ "\n",
    Cmds = DepthCmd ++ SizeCmd ++ WidthCmd ++ LinesCmd.

:- func format_params_to_desc(format_params) = string.

format_params_to_desc(format_params(Depth, Size, Width, Lines)) =
    "depth " ++ int_to_string(Depth) ++ ", " ++
    "size "  ++ int_to_string(Size) ++ ", " ++
    "width " ++ int_to_string(Width) ++ ", " ++
    "lines " ++ int_to_string(Lines) ++ "\n".

:- func format_to_string(portray_format) = string.

format_to_string(flat) = "flat".
format_to_string(raw_pretty) = "raw_pretty".
format_to_string(verbose) = "verbose".
format_to_string(pretty) = "pretty".

%---------------------------------------------------------------------------%

:- pred browser_persistent_state_type(type_desc::out) is det.
:- pragma foreign_export("C", browser_persistent_state_type(out),
    "ML_BROWSE_browser_persistent_state_type").

browser_persistent_state_type(type_of(State)) :-
    init_persistent_state(State).

%---------------------------------------------------------------------------%

deconstruct_browser_term_cc(BrowserDb, BrowserTerm, Functor, Arity,
        Args, MaybeReturn) :-
    (
        BrowserTerm = plain_term(Univ),
        deconstruct.deconstruct(pretty_value(BrowserDb, Univ),
            include_details_cc, Functor, Arity, Args),
        MaybeReturn = no
    ;
        BrowserTerm = synthetic_term(Functor, Args, MaybeReturn),
        list.length(Args, Arity)
    ).

limited_deconstruct_browser_term_cc(BrowserDb, BrowserTerm, Limit,
        MaybeFunctorArityArgs, MaybeReturn) :-
    (
        BrowserTerm = plain_term(Univ),
        deconstruct.limited_deconstruct_cc(pretty_value(BrowserDb, Univ),
            Limit, MaybeFunctorArityArgs),
        MaybeReturn = no
    ;
        BrowserTerm = synthetic_term(Functor, Args, MaybeReturn),
        list.length(Args, Arity),
        MaybeFunctorArityArgs = yes({Functor, Arity, Args})
    ).

functor_browser_term_cc(BrowserDb, BrowserTerm, Functor, Arity, IsFunc) :-
    (
        BrowserTerm = plain_term(Univ),
        deconstruct.functor(pretty_value(BrowserDb, Univ), include_details_cc,
            Functor, Arity),
        IsFunc = no
    ;
        BrowserTerm = synthetic_term(Functor, Args, MaybeReturn),
        list.length(Args, Arity),
        (
            MaybeReturn = yes(_),
            IsFunc = yes
        ;
            MaybeReturn = no,
            IsFunc = no
        )
    ).

:- some [T] func pretty_value(browser_db, univ) = T.

pretty_value(BrowserDb, Univ0) = Value :-
    ( if univ_to_type(Univ0, InputStream) then
        io.input_stream_info(BrowserDb ^ browser_stream_db,
            InputStream) = InputStreamInfo,
        type_to_univ(InputStreamInfo, Univ)
    else if univ_to_type(Univ0, OutputStream) then
        io.output_stream_info(BrowserDb ^ browser_stream_db,
            OutputStream) = OutputStreamInfo,
        type_to_univ(OutputStreamInfo, Univ)
    else if univ_to_type(Univ0, BinaryInputStream) then
        io.binary_input_stream_info(BrowserDb ^ browser_stream_db,
            BinaryInputStream) = BinaryInputStreamInfo,
        type_to_univ(BinaryInputStreamInfo, Univ)
    else if univ_to_type(Univ0, BinaryOutputStream) then
        io.binary_output_stream_info(BrowserDb ^ browser_stream_db,
            BinaryOutputStream) = BinaryOutputStreamInfo,
        type_to_univ(BinaryOutputStreamInfo, Univ)
    else
        Univ = Univ0
    ),
    Value = univ_value(Univ).

%---------------------------------------------------------------------------%
