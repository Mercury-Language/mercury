%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: interface.m.
% Author: zs.
%
% This module defines interface between CGI programs acting as clients
% and CGI programs acting as servers.
%
% The interface consists of queries (sent from the CGI program to the server)
% and responses (sent back from the server to the CGI program), and shared
% knowledge of how to derive the names of some files from the name of the
% profiling data file being explored.
%
% Queries are sent and received as printed representations of Mercury terms,
% using the predicates send_term and recv_term. Responses are sent as strings
% using the predicates send_string and recv_string. Each response is actually
% the name of a file contained a web page, rather than the text of the web page
% itself. This makes things easy to debug (since we can leave the file around
% for inspection) and avoids any potential problems with the web page being too
% big to transmit atomically across the named pipe. (Printable representations
% of queries and filenames are both guaranteed to be smaller than eight
% kilobytes, which is the typical named pipe buffer size.)
%
% A query consists of three components, a command, a set of preferences, and
% the name of the profiling data file. The command tells the server what
% information the user wants displayed. The preferences tell the server how
% the user wants data displayed; they persist across queries unless the user
% changes them.
%
% This module defines the types of commands and preferences. It provides
% mechanisms for converting queries to URLs and URLs to queries, but it
% does not expose the encoding. The encoding is compositional; each component
% of the query (say x) has a x_to_string function to convert it to the URL form
% and a string_to_x predicate to try to convert an URL fragment back to it.
% The function/predicate pairs are adjacent to make it easy to update both at
% the same time. This is essential, because we have no other mechanism to
% ensure that the URLs we embed in the HTML pages we generate will be
% recognized and correctly parsed by the CGI program.
%
%-----------------------------------------------------------------------------%

:- module interface.

:- interface.

:- import_module bool.
:- import_module char.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % These functions derive the names of auxiliary files (or parts
    % thereof) from the name of the profiling data file being explored.
    % The auxiliary files are:
    %
    % - the name of the named pipe for transmitting queries to the server;
    % - the name of the named pipe for transmitting responses back to the
    %   CGI program;
    % - the name of the file containing the output of the server program,
    %   which prints statistics about its own performance at startup
    %   (and if invoked with debugging option, debugging information
    %   during its execution);
    % - the name of the mutual exclusion file (which is always empty);
    % - the naming scheme of the `want' files (which are always empty);
    % - the names of the files containing the web page responses;
    % - the name of the file containing contour exclusion information
    %   (see exclude.m).
    %
:- func to_server_pipe_name(string) = string.
:- func from_server_pipe_name(string) = string.
:- func server_startup_name(string) = string.
:- func mutex_file_name(string) = string.
:- func want_dir = string.
:- func want_prefix = string.
:- func want_file_name = string.
:- func response_file_name(string, int) = string.
:- func contour_file_name(string) = string.

    % send_term(ToFileName, Debug, Term):
    %   Write the term Term to ToFileName, making it is new contents.
    %   If Debug is `yes', write it to the file `/tmp/.send_term'
    %   as well.
    %
:- pred send_term(string::in, bool::in, T::in, io::di, io::uo) is det.

    % send_string(ToFileName, Debug, Str):
    %   Write the string Str to ToFileName, making it is new contents.
    %   If Debug is `yes', write it to the file `/tmp/.send_string'
    %   as well.
    %
:- pred send_string(string::in, bool::in, string::in, io::di, io::uo) is det.

    % recv_term(FromFileName, Debug, Term):
    %   Read the contents of FromFileName, which should be a single
    %   Mercury term. If Debug is `yes', write the result of the read
    %   to the file `/tmp/.recv_term' as well.
    %
:- pred recv_term(string::in, bool::in, T::out, io::di, io::uo) is det.

    % recv_string(FromFileName, Debug, Str):
    %   Read the contents of FromFileName, and return it as Str.
    %   If Debug is `yes', write the result of the read to the file
    %   `/tmp/.recv_string' as well.
    %
:- pred recv_string(string::in, bool::in, string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type resp
    --->    html(string).

:- type cmd_pref
    --->    cmd_pref(cmd, preferences).

:- type cmd
    --->    quit
    ;       restart
    ;       timeout(int)
    ;       menu
    ;       root(maybe(int))
    ;       clique(int)
    ;       proc(int)
    ;       proc_callers(int, caller_groups, int)
    ;       modules
    ;       module(string)
    ;       top_procs(display_limit, cost_kind, include_descendants,
                measurement_scope)
    %
    % The following commands are for debugging.
    %
    ;       proc_static(int)
    ;       proc_dynamic(int)
    ;       call_site_static(int)
    ;       call_site_dynamic(int)
    ;       raw_clique(int).

:- type caller_groups
    --->    group_by_call_site
    ;       group_by_proc
    ;       group_by_module
    ;       group_by_clique.

:- type cost_kind
    --->    calls
    ;       redos
    ;       time
    ;       allocs
    ;       words.

:- type include_descendants
    --->    self
    ;       self_and_desc.

:- type display_limit
    --->    rank_range(int, int)
                    % rank_range(M, N): display procedures
                    % with rank M to N, both inclusive.

    ;       threshold(float).
                    % threshold(Percent): display
                    % procedures whose cost is at least
                    % Fraction of the whole program's
                    % cost.

:- type preferences
    ---> preferences(
            pref_fields :: fields,
                    % set of fields to display
            pref_box    :: box,
                    % whether displays should be boxed
            pref_colour :: colour_scheme,
                    % what principle governs colours
            pref_anc    :: maybe(int),
                    % max number of ancestors to display
            pref_summarize  :: summarize,
                    % whether pages should summarize
                    % at higher order call sites
            pref_criteria   :: order_criteria,
                    % the criteria for ordering lines in
                    % pages, if the command doesn't specify
                    % otherwise
            pref_contour :: contour,
                    % whether contour exclusion should be
                    % applied
            pref_time :: time_format,

            pref_inactive :: inactive_items
                    % Whether we should show modules/procs
                    % that haven't been called.
        ).

:- type port_fields
    --->    no_port
    ;       port.

:- type time_fields
    --->    no_time
    ;       ticks
    ;       time
    ;       ticks_and_time
    ;       time_and_percall
    ;       ticks_and_time_and_percall.

:- type alloc_fields
    --->    no_alloc
    ;       alloc
    ;       alloc_and_percall.

:- type memory_fields
    --->    no_memory
    ;       memory(memory_units)
    ;       memory_and_percall(memory_units).

:- type memory_units
    --->    words
    ;       bytes.

:- type fields
    --->    fields(
                port_fields   :: port_fields,
                time_fields   :: time_fields,
                alloc_fields  :: alloc_fields,
                memory_fields :: memory_fields
            ).

:- type box
    --->    box
    ;       nobox.

:- type colour_scheme
    --->    column_groups
    ;       none.

:- type summarize
    --->    summarize
    ;       dont_summarize.

:- type order_criteria
    --->    by_context
    ;       by_name
    ;       by_cost(
                cost_kind,
                include_descendants,
                measurement_scope
            ).

:- type measurement_scope
    --->    per_call
    ;       overall.

:- type contour
    --->    apply_contour
    ;       no_contour.

:- type time_format
    --->    no_scale
    ;       scale_by_millions
    ;       scale_by_thousands.

:- type inactive_status ---> hide ; show.

:- type inactive_items
    --->    inactive_items(
                inactive_procs   :: inactive_status,
                inactive_modules :: inactive_status
            ).

%-----------------------------------------------------------------------------%

:- func default_preferences = preferences.

:- func default_fields = fields.
:- func all_fields = fields.
:- func default_box = box.
:- func default_colour_scheme = colour_scheme.
:- func default_ancestor_limit = maybe(int).
:- func default_summarize = summarize.
:- func default_order_criteria = order_criteria.
:- func default_cost_kind = cost_kind.
:- func default_incl_desc = include_descendants.
:- func default_scope = measurement_scope.
:- func default_contour = contour.
:- func default_time_format = time_format.
:- func default_inactive_items = inactive_items.

:- func query_separator_char = char.
:- func machine_datafile_cmd_pref_to_url(string, string, cmd, preferences)
    = string.
:- func url_component_to_cmd(string, cmd) = cmd.
:- func url_component_to_maybe_cmd(string) = maybe(cmd).
:- func url_component_to_maybe_pref(string) = maybe(preferences).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module conf.
:- import_module util.

:- import_module list.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

default_preferences =
    preferences(
        default_fields,
        default_box,
        default_colour_scheme,
        default_ancestor_limit,
        default_summarize,
        default_order_criteria,
        default_contour,
        default_time_format,
        default_inactive_items
    ).

default_fields = fields(port, ticks, no_alloc, memory(words)).
all_fields = fields(port, ticks_and_time_and_percall, alloc, memory(words)).
default_box = box.
default_colour_scheme = column_groups.
default_ancestor_limit = yes(5).
default_summarize = dont_summarize.
default_order_criteria = by_context.
default_cost_kind = time.
default_incl_desc = self_and_desc.
default_scope = overall.
default_contour = no_contour.
default_time_format = scale_by_thousands.
default_inactive_items = inactive_items(hide, hide).

%-----------------------------------------------------------------------------%

to_server_pipe_name(DataFileName) =
    server_dir ++ "/" ++
    "mdprof_server_to" ++ filename_mangle(DataFileName).

from_server_pipe_name(DataFileName) =
    server_dir ++ "/" ++
    "mdprof_server_from" ++ filename_mangle(DataFileName).

server_startup_name(DataFileName) =
    server_dir ++ "/" ++
    "mdprof_startup_err" ++ filename_mangle(DataFileName).

mutex_file_name(DataFileName) =
    server_dir ++ "/" ++
    "mdprof_mutex" ++ filename_mangle(DataFileName).

want_dir = server_dir.

want_prefix = "mdprof_want".

want_file_name =
    want_dir ++ "/" ++ want_prefix ++ string.int_to_string(getpid).

response_file_name(DataFileName, QueryNum) =
    server_dir ++ "/" ++
    "mdprof_response" ++ filename_mangle(DataFileName) ++
    string.int_to_string(QueryNum).

contour_file_name(DataFileName) =
    DataFileName ++ ".contour".

:- func server_dir = string.

server_dir = "/var/tmp".

:- func filename_mangle(string) = string.

filename_mangle(FileName) = MangledFileName :-
    FileNameChars = string.to_char_list(FileName),
    MangledFileNameChars = filename_mangle_2(FileNameChars),
    MangledFileName = string.from_char_list(MangledFileNameChars).

    % This mangling scheme ensures that (a) the mangled filename doesn't
    % contain any slashes, and (b) two different original filenames will
    % always yield different mangled filenames.
    %
:- func filename_mangle_2(list(char)) = list(char).

filename_mangle_2([]) = [].
filename_mangle_2([First | Rest]) = MangledChars :-
    MangledRest = filename_mangle_2(Rest),
    ( First = ('/') ->
        MangledChars = [':', '.' | MangledRest]
    ; First = (':') ->
        MangledChars = [':', ':' | MangledRest]
    ;
        MangledChars = [First | MangledRest]
    ).

send_term(ToPipeName, Debug, Data, !IO) :-
    io.open_output(ToPipeName, Res, !IO),
    ( Res = ok(ToStream) ->
        io.write(ToStream, Data, !IO),
        io.write_string(ToStream, ".\n", !IO),
        io.close_output(ToStream, !IO)
    ;
        error("send_term: couldn't open pipe")
    ),
    (
        Debug = yes,
        io.open_output("/tmp/.send_term", Res2, !IO),
        ( Res2 = ok(DebugStream) ->
            io.write(DebugStream, Data, !IO),
            io.write_string(DebugStream, ".\n", !IO),
            io.close_output(DebugStream, !IO)
        ;
            error("send_term: couldn't debug")
        )
    ;
        Debug = no
    ).

send_string(ToPipeName, Debug, Data, !IO) :-
    io.open_output(ToPipeName, Res, !IO),
    ( Res = ok(ToStream) ->
        io.write_string(ToStream, Data, !IO),
        io.close_output(ToStream, !IO)
    ;
        error("send_string: couldn't open pipe")
    ),
    (
        Debug = yes,
        io.open_output("/tmp/.send_string", Res2, !IO),
        ( Res2 = ok(DebugStream) ->
            io.write_string(DebugStream, Data, !IO),
            io.close_output(DebugStream, !IO)
        ;
            error("send_string: couldn't debug")
        )
    ;
        Debug = no
    ).

recv_term(FromPipeName, Debug, Resp, !IO) :-
    io.open_input(FromPipeName, Res0, !IO),
    ( Res0 = ok(FromStream) ->
        io.read(FromStream, Res1, !IO),
        ( Res1 = ok(Resp0) ->
            Resp = Resp0
        ;
            error("recv_term: read failed")
        ),
        io.close_input(FromStream, !IO),
        (
            Debug = yes,
            io.open_output("/tmp/.recv_term", Res2, !IO),
            ( Res2 = ok(DebugStream) ->
                io.write(DebugStream, Res1, !IO),
                io.write_string(DebugStream, ".\n", !IO),
                io.close_output(DebugStream, !IO)
            ;
                error("recv_term: couldn't debug")
            )
        ;
            Debug = no
        )
    ;
        error("recv_term: couldn't open pipe")
    ).

recv_string(FromPipeName, Debug, Resp, !IO) :-
    io.open_input(FromPipeName, Res0, !IO),
    ( Res0 = ok(FromStream) ->
        io.read_file_as_string(FromStream, Res1, !IO),
        ( Res1 = ok(Resp0) ->
            Resp = Resp0
        ;
            error("recv_string: read failed")
        ),
        io.close_input(FromStream, !IO),
        (
            Debug = yes,
            io.open_output("/tmp/.recv_string", Res2, !IO),
            ( Res2 = ok(DebugStream) ->
                io.write(DebugStream, Res1, !IO),
                io.write_string(DebugStream, ".\n", !IO),
                io.close_output(DebugStream, !IO)
            ;
                error("recv_string: couldn't debug")
            )
        ;
            Debug = no
        )
    ;
        error("recv_term: couldn't open pipe")
    ).

%-----------------------------------------------------------------------------%

:- func cmd_separator_char = char.
:- func pref_separator_char = char.
:- func criteria_separator_char = char.
:- func field_separator_char = char.
:- func limit_separator_char = char.

query_separator_char = ('&').
cmd_separator_char = ('/').
pref_separator_char = ('/').
criteria_separator_char = ('-').
field_separator_char = ('-').
limit_separator_char = ('-').

machine_datafile_cmd_pref_to_url(Machine, DataFileName, Cmd, Preferences) =
    "http://" ++
    Machine ++
    "/cgi-bin/mdprof_cgi?" ++
    cmd_to_string(Cmd) ++
    string.char_to_string(query_separator_char) ++
    preferences_to_string(Preferences) ++
    string.char_to_string(query_separator_char) ++
    DataFileName.

:- func cmd_to_string(cmd) = string.

cmd_to_string(Cmd) = CmdStr :-
    (
        Cmd = quit,
        CmdStr = "quit"
    ;
        Cmd = restart,
        CmdStr = "restart"
    ;
        Cmd = timeout(Minutes),
        CmdStr = string.format("timeout%c%d",
            [c(cmd_separator_char), i(Minutes)])
    ;
        Cmd = menu,
        CmdStr = "menu"
    ;
        Cmd = root(MaybePercent),
        (
            MaybePercent = yes(Percent),
            CmdStr = string.format("root%c%d",
                [c(cmd_separator_char), i(Percent)])
        ;
            MaybePercent = no,
            CmdStr = string.format("root%c%s",
                [c(cmd_separator_char), s("no")])
        )
    ;
        Cmd = clique(CliqueNum),
        CmdStr = string.format("clique%c%d",
            [c(cmd_separator_char), i(CliqueNum)])
    ;
        Cmd = proc(ProcNum),
        CmdStr = string.format("proc%c%d",
            [c(cmd_separator_char), i(ProcNum)])
    ;
        Cmd = proc_callers(ProcNum, GroupCallers, BunchNum),
        GroupCallersStr = caller_groups_to_string(GroupCallers),
        CmdStr = string.format("proc_callers%c%d%c%s%c%d",
            [c(cmd_separator_char), i(ProcNum),
            c(cmd_separator_char), s(GroupCallersStr),
            c(cmd_separator_char), i(BunchNum)])
    ;
        Cmd = modules,
        CmdStr = "modules"
    ;
        Cmd = module(ModuleName),
        CmdStr = string.format("module%c%s",
            [c(cmd_separator_char), s(ModuleName)])
    ;
        Cmd = top_procs(Limit, CostKind, InclDesc, Scope),
        LimitStr = limit_to_string(Limit),
        CostKindStr = cost_kind_to_string(CostKind),
        InclDescStr = incl_desc_to_string(InclDesc),
        ScopeStr = scope_to_string(Scope),
        CmdStr = string.format("top_procs%c%s%c%s%c%s%c%s",
            [c(cmd_separator_char), s(LimitStr),
            c(cmd_separator_char), s(CostKindStr),
            c(cmd_separator_char), s(InclDescStr),
            c(cmd_separator_char), s(ScopeStr)])
    ;
        Cmd = proc_static(PSI),
        CmdStr = string.format("proc_static%c%d",
            [c(cmd_separator_char), i(PSI)])
    ;
        Cmd = proc_dynamic(PDI),
        CmdStr = string.format("proc_dynamic%c%d",
            [c(cmd_separator_char), i(PDI)])
    ;
        Cmd = call_site_static(CSSI),
        CmdStr = string.format("call_site_static%c%d",
            [c(cmd_separator_char), i(CSSI)])
    ;
        Cmd = call_site_dynamic(CSDI),
        CmdStr = string.format("call_site_dynamic%c%d",
            [c(cmd_separator_char), i(CSDI)])
    ;
        Cmd = raw_clique(CI),
        CmdStr = string.format("raw_clique%c%d",
            [c(cmd_separator_char), i(CI)])
    ).

:- func preferences_to_string(preferences) = string.

preferences_to_string(Pref) = PrefStr :-
    Pref = preferences(Fields, Box, Colour, MaybeAncestorLimit,
        Summarize, Order, Contour, Time, InactiveItems),
    (
        MaybeAncestorLimit = yes(AncestorLimit),
        MaybeAncestorLimitStr =
            string.format("%d", [i(AncestorLimit)])
    ;
        MaybeAncestorLimit = no,
        MaybeAncestorLimitStr = "no"
    ),
    PrefStr = string.format("%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s",
        [s(fields_to_string(Fields)),
        c(pref_separator_char), s(box_to_string(Box)),
        c(pref_separator_char), s(colour_scheme_to_string(Colour)),
        c(pref_separator_char), s(MaybeAncestorLimitStr),
        c(pref_separator_char), s(summarize_to_string(Summarize)),
        c(pref_separator_char), s(order_criteria_to_string(Order)),
        c(pref_separator_char), s(contour_to_string(Contour)),
        c(pref_separator_char), s(time_format_to_string(Time)),
        c(pref_separator_char),
        s(inactive_items_to_string(InactiveItems))
    ]).

url_component_to_cmd(QueryString, DefaultCmd) = Cmd :-
    MaybeCmd = url_component_to_maybe_cmd(QueryString),
    (
        MaybeCmd = yes(Cmd)
    ;
        MaybeCmd = no,
        Cmd = DefaultCmd
    ).

url_component_to_maybe_cmd(QueryString) = MaybeCmd :-
    split(QueryString, pref_separator_char, Pieces),
    (
        Pieces = ["root", MaybePercentStr],
        ( MaybePercentStr = "no" ->
            MaybePercent = no
        ; string.to_int(MaybePercentStr, Percent) ->
            MaybePercent = yes(Percent)
        ;
            fail
        )
    ->
        MaybeCmd = yes(root(MaybePercent))
    ;
        Pieces = ["clique", CliqueNumStr],
        string.to_int(CliqueNumStr, CliqueNum)
    ->
        MaybeCmd = yes(clique(CliqueNum))
    ;
        Pieces = ["proc", PSIStr],
        string.to_int(PSIStr, PSI)
    ->
        MaybeCmd = yes(proc(PSI))
    ;
        Pieces = ["proc_callers", PSIStr, GroupCallersStr, BunchNumStr],
        string.to_int(PSIStr, PSI),
        string.to_int(BunchNumStr, BunchNum),
        string_to_caller_groups(GroupCallersStr, GroupCallers)
    ->
        MaybeCmd = yes(proc_callers(PSI, GroupCallers, BunchNum))
    ;
        Pieces = ["modules"]
    ->
        MaybeCmd = yes(modules)
    ;
        Pieces = ["module", ModuleName]
    ->
        MaybeCmd = yes(module(ModuleName))
    ;
        Pieces = ["top_procs", LimitStr,
            CostKindStr, InclDescStr, ScopeStr],
        string_to_limit(LimitStr, Limit),
        string_to_cost_kind(CostKindStr, CostKind),
        string_to_incl_desc(InclDescStr, InclDesc),
        string_to_scope(ScopeStr, Scope)
    ->
        MaybeCmd = yes(top_procs(Limit, CostKind, InclDesc, Scope))
    ;
        Pieces = ["menu"]
    ->
        MaybeCmd = yes(menu)
    ;
        Pieces = ["proc_static", PSIStr],
        string.to_int(PSIStr, PSI)
    ->
        MaybeCmd = yes(proc_static(PSI))
    ;
        Pieces = ["proc_dynamic", PDIStr],
        string.to_int(PDIStr, PDI)
    ->
        MaybeCmd = yes(proc_dynamic(PDI))
    ;
        Pieces = ["call_site_static", CSSIStr],
        string.to_int(CSSIStr, CSSI)
    ->
        MaybeCmd = yes(call_site_static(CSSI))
    ;
        Pieces = ["call_site_dynamic", CSDIStr],
        string.to_int(CSDIStr, CSDI)
    ->
        MaybeCmd = yes(call_site_dynamic(CSDI))
    ;
        Pieces = ["raw_clique", CliqueNumStr],
        string.to_int(CliqueNumStr, CliqueNum)
    ->
        MaybeCmd = yes(raw_clique(CliqueNum))
    ;
        Pieces = ["timeout", TimeOutStr],
        string.to_int(TimeOutStr, TimeOut)
    ->
        MaybeCmd = yes(timeout(TimeOut))
    ;
        Pieces = ["restart"]
    ->
        MaybeCmd = yes(restart)
    ;
        Pieces = ["quit"]
    ->
        MaybeCmd = yes(quit)
    ;
        MaybeCmd = no
    ).

url_component_to_maybe_pref(QueryString) = MaybePreferences :-
    split(QueryString, pref_separator_char, Pieces),
    (
        Pieces = [FieldsStr, BoxStr, ColourStr, MaybeAncestorLimitStr,
            SummarizeStr, OrderStr, ContourStr, TimeStr,
            InactiveItemsStr],
        string_to_fields(FieldsStr, Fields),
        string_to_box(BoxStr, Box),
        string_to_colour_scheme(ColourStr, Colour),
        ( string.to_int(MaybeAncestorLimitStr, AncestorLimit) ->
            MaybeAncestorLimit = yes(AncestorLimit)
        ; MaybeAncestorLimitStr = "no" ->
            MaybeAncestorLimit = no
        ;
            fail
        ),
        string_to_summarize(SummarizeStr, Summarize),
        string_to_order_criteria(OrderStr, Order),
        string_to_contour(ContourStr, Contour),
        string_to_time_format(TimeStr, Time),
        string_to_inactive_items(InactiveItemsStr, InactiveItems)
    ->
        Preferences = preferences(Fields, Box, Colour,
            MaybeAncestorLimit, Summarize, Order, Contour, Time,
            InactiveItems),
        MaybePreferences = yes(Preferences)
    ;
        MaybePreferences = no
    ).

%-----------------------------------------------------------------------------%

:- func port_fields_to_string(port_fields) = string.

port_fields_to_string(PortFields) = String :-
    string_to_port_fields(String, PortFields).

:- pred string_to_port_fields(string, port_fields).
:- mode string_to_port_fields(in, out) is semidet.
:- mode string_to_port_fields(out, in) is det.

string_to_port_fields("_", no_port).
string_to_port_fields("p", port).

:- func time_fields_to_string(time_fields) = string.

time_fields_to_string(TimeFields) = String :-
    string_to_time_fields(String, TimeFields).

:- pred string_to_time_fields(string, time_fields).
:- mode string_to_time_fields(in, out) is semidet.
:- mode string_to_time_fields(out, in) is det.

string_to_time_fields("_",   no_time).
string_to_time_fields("q",   ticks).
string_to_time_fields("t",   time).
string_to_time_fields("qt",  ticks_and_time).
string_to_time_fields("tp",  time_and_percall).
string_to_time_fields("qtp", ticks_and_time_and_percall).

:- func alloc_fields_to_string(alloc_fields) = string.

alloc_fields_to_string(AllocFields) = String :-
    string_to_alloc_fields(String, AllocFields).

:- pred string_to_alloc_fields(string, alloc_fields).
:- mode string_to_alloc_fields(in, out) is semidet.
:- mode string_to_alloc_fields(out, in) is det.

string_to_alloc_fields("_",  no_alloc).
string_to_alloc_fields("a",  alloc).
string_to_alloc_fields("ap", alloc_and_percall).

:- func memory_fields_to_string(memory_fields) = string.

memory_fields_to_string(MemoryFields) = String :-
    string_to_memory_fields(String, MemoryFields).

:- pred string_to_memory_fields(string, memory_fields).
:- mode string_to_memory_fields(in, out) is semidet.
:- mode string_to_memory_fields(out, in) is det.

string_to_memory_fields("_",  no_memory).
string_to_memory_fields("b",  memory(bytes)).
string_to_memory_fields("w",  memory(words)).
string_to_memory_fields("bp", memory_and_percall(bytes)).
string_to_memory_fields("wp", memory_and_percall(words)).

:- func fields_to_string(fields) = string.

fields_to_string(fields(Port, Time, Allocs, Memory)) =
    port_fields_to_string(Port) ++
    string.char_to_string(field_separator_char) ++
    time_fields_to_string(Time) ++
    string.char_to_string(field_separator_char) ++
    alloc_fields_to_string(Allocs) ++
    string.char_to_string(field_separator_char) ++
    memory_fields_to_string(Memory).

:- pred string_to_fields(string::in, fields::out) is semidet.

string_to_fields(FieldsStr, Fields) :-
    (
        split(FieldsStr, field_separator_char, Pieces),
        Pieces = [PortStr, TimeStr, AllocStr, MemoryStr],
        string_to_port_fields(PortStr, Port),
        string_to_time_fields(TimeStr, Time),
        string_to_alloc_fields(AllocStr, Alloc),
        string_to_memory_fields(MemoryStr, Memory)
    ->
        Fields = fields(Port, Time, Alloc, Memory)
    ;
        fail
    ).

:- func caller_groups_to_string(caller_groups) = string.

caller_groups_to_string(CallerGroups) = String :-
    string_to_caller_groups(String, CallerGroups).

:- pred string_to_caller_groups(string, caller_groups).
:- mode string_to_caller_groups(in, out) is semidet.
:- mode string_to_caller_groups(out, in) is det.

string_to_caller_groups("cs", group_by_call_site).
string_to_caller_groups("pr", group_by_proc).
string_to_caller_groups("mo", group_by_module).
string_to_caller_groups("cl", group_by_clique).

:- func cost_kind_to_string(cost_kind) = string.

cost_kind_to_string(CostKind) = String :-
    string_to_cost_kind(String, CostKind).

:- pred string_to_cost_kind(string, cost_kind).
:- mode string_to_cost_kind(in, out) is semidet.
:- mode string_to_cost_kind(out, in) is det.

string_to_cost_kind("calls",  calls).
string_to_cost_kind("redos",  redos).
string_to_cost_kind("time",   time).
string_to_cost_kind("allocs", allocs).
string_to_cost_kind("words",  words).

:- func incl_desc_to_string(include_descendants) = string.

incl_desc_to_string(InclDesc) = String :-
    string_to_incl_desc(String, InclDesc).

:- pred string_to_incl_desc(string, include_descendants).
:- mode string_to_incl_desc(in, out) is semidet.
:- mode string_to_incl_desc(out, in) is det.

string_to_incl_desc("self", self).
string_to_incl_desc("both", self_and_desc).

:- func limit_to_string(display_limit) = string.

limit_to_string(rank_range(Lo, Hi)) =
    string.format("%d%c%d", [i(Lo), c(limit_separator_char), i(Hi)]).
limit_to_string(threshold(Threshold)) =
    string.format("%f", [f(Threshold)]).

:- pred string_to_limit(string::in, display_limit::out) is semidet.

string_to_limit(LimitStr, Limit) :-
    (
        split(LimitStr, limit_separator_char, Pieces),
        Pieces = [FirstStr, LastStr],
        string.to_int(FirstStr, First),
        string.to_int(LastStr, Last)
    ->
        Limit = rank_range(First, Last)
    ;
        string.to_float(LimitStr, Threshold)
    ->
        Limit = threshold(Threshold)
    ;
        fail
    ).

:- func summarize_to_string(summarize) = string.

summarize_to_string(summarize)      = "sum".
summarize_to_string(dont_summarize) = "nosum".

:- pred string_to_summarize(string::in, summarize::out) is semidet.

string_to_summarize("sum",   summarize).
string_to_summarize("nosum", dont_summarize).

:- func order_criteria_to_string(order_criteria) = string.

order_criteria_to_string(by_context) = "context".
order_criteria_to_string(by_name) = "name".
order_criteria_to_string(by_cost(CostKind, InclDesc, Scope)) =
    "cost" ++
    string.char_to_string(criteria_separator_char) ++
    cost_kind_to_string(CostKind) ++
    string.char_to_string(criteria_separator_char) ++
    incl_desc_to_string(InclDesc) ++
    string.char_to_string(criteria_separator_char) ++
    scope_to_string(Scope).

:- pred string_to_order_criteria(string::in, order_criteria::out) is semidet.

string_to_order_criteria(CriteriaStr, Criteria) :-
    (
        CriteriaStr = "context"
    ->
        Criteria = by_context
    ;
        CriteriaStr = "name"
    ->
        Criteria = by_name
    ;
        split(CriteriaStr, criteria_separator_char, Pieces),
        Pieces = ["cost", CostKindStr, InclDescStr, ScopeStr],
        string_to_cost_kind(CostKindStr, CostKind),
        string_to_incl_desc(InclDescStr, InclDesc),
        string_to_scope(ScopeStr, Scope)
    ->
        Criteria = by_cost(CostKind, InclDesc, Scope)
    ;
        fail
    ).

:- func scope_to_string(measurement_scope) = string.

scope_to_string(Scope) = String :-
    string_to_scope(String, Scope).

:- pred string_to_scope(string, measurement_scope).
:- mode string_to_scope(in, out) is semidet.
:- mode string_to_scope(out, in) is det.

string_to_scope("pc", per_call).
string_to_scope("oa",  overall).

:- func contour_to_string(contour) = string.

contour_to_string(Contour) = String :-
    string_to_contour(String, Contour).

:- pred string_to_contour(string, contour).
:- mode string_to_contour(in, out) is semidet.
:- mode string_to_contour(out, in) is det.

string_to_contour("ac", apply_contour).
string_to_contour("nc", no_contour).

:- func time_format_to_string(time_format) = string.

time_format_to_string(no_scale)           = "no".
time_format_to_string(scale_by_millions)  = "mi".
time_format_to_string(scale_by_thousands) = "th".

:- pred string_to_time_format(string, time_format).
:- mode string_to_time_format(in, out) is semidet.
:- mode string_to_time_format(out, in) is det.

string_to_time_format("no", no_scale).
string_to_time_format("mi", scale_by_millions).
string_to_time_format("th", scale_by_thousands).

:- func inactive_items_to_string(inactive_items) = string.

inactive_items_to_string(Items) = String :-
    string_to_inactive_items(String, Items).

:- pred string_to_inactive_items(string, inactive_items).
:- mode string_to_inactive_items(in, out) is semidet.
:- mode string_to_inactive_items(out, in) is det.

string_to_inactive_items("hh", inactive_items(hide,  hide)).
string_to_inactive_items("sh", inactive_items(show,  hide)).
string_to_inactive_items("hs", inactive_items(hide,  show)).
string_to_inactive_items("ss", inactive_items(show,  show)).

:- func colour_scheme_to_string(colour_scheme) = string.

colour_scheme_to_string(Scheme) = String :-
    string_to_colour_scheme(String, Scheme).

:- pred string_to_colour_scheme(string, colour_scheme).
:- mode string_to_colour_scheme(in, out) is semidet.
:- mode string_to_colour_scheme(out, in) is det.

string_to_colour_scheme("cols", column_groups).
string_to_colour_scheme("none", none).

:- func box_to_string(box) = string.

box_to_string(Box) = String :-
    string_to_box(String, Box).

:- pred string_to_box(string, box).
:- mode string_to_box(in, out) is semidet.
:- mode string_to_box(out, in) is det.

string_to_box("box",   box).
string_to_box("nobox", nobox).

%-----------------------------------------------------------------------------%
:- end_module interface.
%-----------------------------------------------------------------------------%
