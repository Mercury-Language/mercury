%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: zs.
%
% This module defines the type of the commands that the CGI program
% (mdprof_cgi.m) passes to the deep profiling server (mdprof_server.m),
% as well as utility predicates for manipulating commands and responses.

:- module interface.

:- interface.

:- import_module std_util, io.

:- func to_server_pipe_name(string) = string.
:- func from_server_pipe_name(string) = string.
:- func server_startup_name(string) = string.
:- func contour_file_name(string) = string.

:- pred to(string::in, cmd_pref::in, io__state::di, io__state::uo) is det.
:- pred from(string::in, resp::out, io__state::di, io__state::uo) is det.

:- type resp
	--->	html(string).

:- type cmd_pref
	--->	cmd_pref(cmd, preferences).

:- type cmd
	--->	quit
	;	restart
	;	timeout(int)
	;	menu
	;	root(maybe(int))
	;	clique(int)
	;	proc(int)
	;	proc_callers(int, caller_groups, int)
	;	modules
	;	module(string)
	;	top_procs(display_limit,
			cost_kind, include_descendants, measurement_scope)

		% The commands below are for debugging.
	;	proc_static(int)
	;	proc_dynamic(int)
	;	call_site_static(int)
	;	call_site_dynamic(int)
	;	raw_clique(int).

:- type caller_groups
	--->	group_by_call_site
	;	group_by_proc
	;	group_by_module
	;	group_by_clique.

:- type cost_kind
	--->	calls
	;	time
	;	allocs
	;	words.

:- type include_descendants
	--->	self
	;	self_and_desc.

:- type display_limit
	--->	rank_range(int, int)
	;	threshold(float).

:- type preferences
	--->	preferences(
			pref_fields	:: fields,
					% set of fields to display
			pref_box	:: box,
					% whether displays should be boxed
			pref_colour	:: colour_scheme,
					% what principle governs colours
			pref_anc	:: maybe(int),
					% max number of ancestors to display
			pref_summarize	:: summarize,
					% whether pages should summarize
					% at higher order call sites
			pref_criteria	:: order_criteria,
					% the criteria for ordering lines in
					% pages, if the command doesn't specify
					% otherwise
			pref_contour	:: contour,
					% whether contour exclusion should be
					% applied
			pref_time	:: time_format
		).

:- type port_fields
	--->	no_port
	;	port.

:- type time_fields
	--->	no_time
	;	ticks
	;	time
	;	ticks_and_time
	;	time_and_percall
	;	ticks_and_time_and_percall.

:- type alloc_fields
	--->	no_alloc
	;	alloc
	;	alloc_and_percall.

:- type memory_fields
	--->	no_memory
	;	memory(memory_units)
	;	memory_and_percall(memory_units).

:- type memory_units
	--->	words
	;	bytes.

:- type fields
	--->	fields(
			port_fields	:: port_fields,
			time_fields	:: time_fields,
			alloc_fields	:: alloc_fields,
			memory_fields	:: memory_fields
		).

:- type box
	--->	box
	;	nobox.

:- type colour_scheme
	--->	column_groups
	;	none.

:- type summarize
	--->	summarize
	;	dont_summarize.

:- type order_criteria
	--->	by_context
	;	by_name
	;	by_cost(
			cost_kind,
			include_descendants,
			measurement_scope
		).

:- type measurement_scope
	--->	per_call
	;	overall.

:- type contour
	--->	apply_contour
	;	no_contour.

:- type time_format
	--->	no_scale
	;	scale_by_millions
	;	scale_by_thousands.

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

:- func cmd_pref_to_url(string, string, cmd, preferences) = string.
:- func url_component_to_cmd(string) = maybe(cmd).
:- func url_component_to_preferences(string) = maybe(preferences).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module util.
:- import_module char, string, list, set, require.

default_preferences =
	preferences(
		default_fields,
		default_box,
		default_colour_scheme,
		default_ancestor_limit,
		default_summarize,
		default_order_criteria,
		default_contour,
		default_time_format
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

%-----------------------------------------------------------------------------%

to_server_pipe_name(DataFileName) =
	server_dir ++ "/" ++
	"mdprof_server_to" ++
	filename_mangle(DataFileName).

from_server_pipe_name(DataFileName) =
	server_dir ++ "/" ++
	"mdprof_server_from" ++
	filename_mangle(DataFileName).

server_startup_name(DataFileName) =
	server_dir ++ "/" ++
	"mdprof_startup_err" ++
	filename_mangle(DataFileName).

contour_file_name(DataFileName) =
	DataFileName ++ ".contour".

:- func server_dir = string.

server_dir = "/var/tmp".

:- func filename_mangle(string) = string.

filename_mangle(FileName) = MangledFileName :-
	FileNameChars = string__to_char_list(FileName),
	MangledFileNameChars = filename_mangle_2(FileNameChars),
	MangledFileName = string__from_char_list(MangledFileNameChars).

	% This mangling scheme ensures that (a) the mangled filename doesn't
	% contain any slashes, and (b) two different original filenames will
	% always yield different mangled filenames.

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

%-----------------------------------------------------------------------------%

to(Where, CmdPref) -->
	io__tell(Where, Res),
	( { Res = ok } ->
		io__write(CmdPref),
		io__write_string(".\n"),
		io__told
	;
		{ error("mdprof to: couldn't open pipe") }
	).

from(Where, Resp) -->
	io__see(Where, Res0),
	( { Res0 = ok } ->
		io__read(Res1),
		( { Res1 = ok(Resp0) } ->
			{ Resp = Resp0 }
		;
			{ error("mdprof from: read failed") }
		),
		io__seen
	;
		{ error("mdprof from: couldn't open pipe") }
	).

%-----------------------------------------------------------------------------%

cmd_pref_to_url(Machine, DataFileName, Cmd, Preferences) =
	"http://" ++
	Machine ++
	"/cgi-bin/mdprof?" ++
	cmd_to_string(Cmd) ++
	"$" ++
	preferences_to_string(Preferences) ++
	"$" ++
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
		CmdStr = string__format("timeout+%d", [i(Minutes)])
	;
		Cmd = menu,
		CmdStr = "menu"
	;
		Cmd = root(MaybePercent),
		(
			MaybePercent = yes(Percent),
			CmdStr = string__format("root+%d", [i(Percent)])
		;
			MaybePercent = no,
			CmdStr = "root+no"
		)
	;
		Cmd = clique(CliqueNum),
		CmdStr = string__format("clique+%d", [i(CliqueNum)])
	;
		Cmd = proc(ProcNum),
		CmdStr = string__format("proc+%d", [i(ProcNum)])
	;
		Cmd = proc_callers(ProcNum, GroupCallers, BunchNum),
		GroupCallersStr = caller_groups_to_string(GroupCallers),
		CmdStr = string__format("proc_callers+%d+%s+%d",
			[i(ProcNum), s(GroupCallersStr), i(BunchNum)])
	;
		Cmd = modules,
		CmdStr = "modules"
	;
		Cmd = module(ModuleName),
		CmdStr = "module+" ++ ModuleName
	;
		Cmd = top_procs(Limit, CostKind, InclDesc, Scope),
		LimitStr = limit_to_string(Limit),
		CostKindStr = cost_kind_to_string(CostKind),
		InclDescStr = incl_desc_to_string(InclDesc),
		ScopeStr = scope_to_string(Scope),
		CmdStr = string__format("top_procs+%s+%s+%s+%s",
			[s(LimitStr), s(CostKindStr),
			s(InclDescStr), s(ScopeStr)])
	;
		Cmd = proc_static(PSI),
		CmdStr = string__format("proc_static+%d", [i(PSI)])
	;
		Cmd = proc_dynamic(PDI),
		CmdStr = string__format("proc_dynamic+%d", [i(PDI)])
	;
		Cmd = call_site_static(CSSI),
		CmdStr = string__format("call_site_static+%d", [i(CSSI)])
	;
		Cmd = call_site_dynamic(CSDI),
		CmdStr = string__format("call_site_dynamic+%d", [i(CSDI)])
	;
		Cmd = raw_clique(CI),
		CmdStr = string__format("raw_clique+%d", [i(CI)])
	).

:- func preferences_to_string(preferences) = string.

preferences_to_string(Pref) = PrefStr :-
	Pref = preferences(Fields, Box, Colour, MaybeAncestorLimit,
		Summarize, Order, Contour, Time),
	(
		MaybeAncestorLimit = yes(AncestorLimit),
		MaybeAncestorLimitStr =
			string__format("%d", [i(AncestorLimit)])
	;
		MaybeAncestorLimit = no,
		MaybeAncestorLimitStr = "no"
	),
	PrefStr = string__format("%s+%s+%s+%s+%s+%s+%s+%s",
		[s(fields_to_string(Fields)), s(box_to_string(Box)),
		s(colour_scheme_to_string(Colour)), s(MaybeAncestorLimitStr),
		s(summarize_to_string(Summarize)),
		s(order_criteria_to_string(Order)),
		s(contour_to_string(Contour)),
		s(time_format_to_string(Time))]).

url_component_to_cmd(QueryString) = MaybeCmd :-
	split(QueryString, ('+'), Pieces),
	(
		Pieces = ["root", MaybePercentStr],
		( MaybePercentStr = "no" ->
			MaybePercent = no
		; string__to_int(MaybePercentStr, Percent) ->	
			MaybePercent = yes(Percent)
		;
			fail
		)
	->
		MaybeCmd = yes(root(MaybePercent))
	;
		Pieces = ["clique", CliqueNumStr],
		string__to_int(CliqueNumStr, CliqueNum)
	->
		MaybeCmd = yes(clique(CliqueNum))
	;
		Pieces = ["proc", PSIStr],
		string__to_int(PSIStr, PSI)
	->
		MaybeCmd = yes(proc(PSI))
	;
		Pieces = ["proc_callers", PSIStr, GroupCallersStr, BunchNumStr],
		string__to_int(PSIStr, PSI),
		string__to_int(BunchNumStr, BunchNum),
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
		string__to_int(PSIStr, PSI)
	->
		MaybeCmd = yes(proc_static(PSI))
	;
		Pieces = ["proc_dynamic", PDIStr],
		string__to_int(PDIStr, PDI)
	->
		MaybeCmd = yes(proc_dynamic(PDI))
	;
		Pieces = ["call_site_static", CSSIStr],
		string__to_int(CSSIStr, CSSI)
	->
		MaybeCmd = yes(call_site_static(CSSI))
	;
		Pieces = ["call_site_dynamic", CSDIStr],
		string__to_int(CSDIStr, CSDI)
	->
		MaybeCmd = yes(call_site_dynamic(CSDI))
	;
		Pieces = ["raw_clique", CliqueNumStr],
		string__to_int(CliqueNumStr, CliqueNum)
	->
		MaybeCmd = yes(raw_clique(CliqueNum))
	;
		Pieces = ["timeout", TimeOutStr],
		string__to_int(TimeOutStr, TimeOut)
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

url_component_to_preferences(QueryString) = MaybePreferences :-
	split(QueryString, ('+'), Pieces),
	(
		Pieces = [FieldsStr, BoxStr, ColourStr, MaybeAncestorLimitStr,
			SummarizeStr, OrderStr, ContourStr, TimeStr],
		string_to_fields(FieldsStr, Fields),
		string_to_box(BoxStr, Box),
		string_to_colour_scheme(ColourStr, Colour),
		( string__to_int(MaybeAncestorLimitStr, AncestorLimit) ->
			MaybeAncestorLimit = yes(AncestorLimit)
		; MaybeAncestorLimitStr = "no" ->
			MaybeAncestorLimit = no
		;
			fail
		),
		string_to_summarize(SummarizeStr, Summarize),
		string_to_order_criteria(OrderStr, Order),
		string_to_contour(ContourStr, Contour),
		string_to_time_format(TimeStr, Time)
	->
		Preferences = preferences(Fields, Box, Colour,
			MaybeAncestorLimit, Summarize, Order, Contour, Time),
		MaybePreferences = yes(Preferences)
	;
		MaybePreferences = no
	).

%-----------------------------------------------------------------------------%

:- func port_fields_to_string(port_fields) = string.

port_fields_to_string(no_port) = "_".
port_fields_to_string(port)    = "p".

:- pred string_to_port_fields(string::in, port_fields::out) is semidet.

string_to_port_fields("_", no_port).
string_to_port_fields("p", port).

:- func time_fields_to_string(time_fields) = string.

time_fields_to_string(no_time)                    = "_".
time_fields_to_string(ticks)                      = "q".
time_fields_to_string(time)                       = "t".
time_fields_to_string(ticks_and_time)             = "qt".
time_fields_to_string(time_and_percall)           = "tp".
time_fields_to_string(ticks_and_time_and_percall) = "qtp".

:- pred string_to_time_fields(string::in, time_fields::out) is semidet.

string_to_time_fields("_",   no_time).
string_to_time_fields("q",   ticks).
string_to_time_fields("t",   time).
string_to_time_fields("qt",  ticks_and_time).
string_to_time_fields("tp",  time_and_percall).
string_to_time_fields("qtp", ticks_and_time_and_percall).

:- func alloc_fields_to_string(alloc_fields) = string.

alloc_fields_to_string(no_alloc)          = "_".
alloc_fields_to_string(alloc)             = "a".
alloc_fields_to_string(alloc_and_percall) = "ap".

:- pred string_to_alloc_fields(string::in, alloc_fields::out) is semidet.

string_to_alloc_fields("_",  no_alloc).
string_to_alloc_fields("a",  alloc).
string_to_alloc_fields("ap", alloc_and_percall).

:- func memory_fields_to_string(memory_fields) = string.

memory_fields_to_string(no_memory)                 = "_".
memory_fields_to_string(memory(bytes))             = "b".
memory_fields_to_string(memory(words))             = "w".
memory_fields_to_string(memory_and_percall(bytes)) = "bp".
memory_fields_to_string(memory_and_percall(words)) = "wp".

:- pred string_to_memory_fields(string::in, memory_fields::out) is semidet.

string_to_memory_fields("_",  no_memory).
string_to_memory_fields("b",  memory(bytes)).
string_to_memory_fields("w",  memory(words)).
string_to_memory_fields("bp", memory_and_percall(bytes)).
string_to_memory_fields("wp", memory_and_percall(words)).

:- func fields_to_string(fields) = string.

fields_to_string(fields(Port, Time, Allocs, Memory)) =
	port_fields_to_string(Port) ++ "-" ++
	time_fields_to_string(Time) ++ "-" ++
	alloc_fields_to_string(Allocs) ++ "-" ++
	memory_fields_to_string(Memory).

:- pred string_to_fields(string::in, fields::out) is semidet.

string_to_fields(FieldsStr, Fields) :-
	(
		split(FieldsStr, '-', Pieces),
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

caller_groups_to_string(group_by_call_site) = "cs".
caller_groups_to_string(group_by_proc)      = "pr".
caller_groups_to_string(group_by_module)    = "mo".
caller_groups_to_string(group_by_clique)    = "cl".

:- pred string_to_caller_groups(string::in, caller_groups::out) is semidet.

string_to_caller_groups("cs", group_by_call_site).
string_to_caller_groups("pr", group_by_proc).
string_to_caller_groups("mo", group_by_module).
string_to_caller_groups("cl", group_by_clique).

:- func cost_kind_to_string(cost_kind) = string.

cost_kind_to_string(calls) =  "calls".
cost_kind_to_string(time) =   "time".
cost_kind_to_string(allocs) = "allocs".
cost_kind_to_string(words) =  "words".

:- pred string_to_cost_kind(string::in, cost_kind::out) is semidet.

string_to_cost_kind("calls",  calls).
string_to_cost_kind("time",   time).
string_to_cost_kind("allocs", allocs).
string_to_cost_kind("words",  words).

:- func incl_desc_to_string(include_descendants) = string.

incl_desc_to_string(self) =          "self".
incl_desc_to_string(self_and_desc) = "both".

:- pred string_to_incl_desc(string::in, include_descendants::out) is semidet.

string_to_incl_desc("self", self).
string_to_incl_desc("both", self_and_desc).

:- func limit_to_string(display_limit) = string.

limit_to_string(rank_range(Lo, Hi)) =   string__format("%d-%d", [i(Lo), i(Hi)]).
limit_to_string(threshold(Threshold)) = string__format("%f", [f(Threshold)]).

:- pred string_to_limit(string::in, display_limit::out) is semidet.

string_to_limit(LimitStr, Limit) :-
	(
		split(LimitStr, '-', Pieces),
		Pieces = [FirstStr, LastStr],
		string__to_int(FirstStr, First),
		string__to_int(LastStr, Last)
	->
		Limit = rank_range(First, Last)
	;
		string__to_float(LimitStr, Threshold)
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
	"cost" ++ "-" ++
	cost_kind_to_string(CostKind) ++ "-" ++
	incl_desc_to_string(InclDesc) ++ "-" ++
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
		split(CriteriaStr, '-', Pieces),
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

scope_to_string(per_call) = "pc".
scope_to_string(overall)  = "oa".

:- pred string_to_scope(string::in, measurement_scope::out) is semidet.

string_to_scope("pc", per_call).
string_to_scope("oa",  overall).

:- func contour_to_string(contour) = string.

contour_to_string(apply_contour) = "ac".
contour_to_string(no_contour)    = "nc".

:- pred string_to_contour(string::in, contour::out) is semidet.

string_to_contour("ac", apply_contour).
string_to_contour("nc", no_contour).

:- func time_format_to_string(time_format) = string.

time_format_to_string(no_scale)           = "no".
time_format_to_string(scale_by_millions)  = "mi".
time_format_to_string(scale_by_thousands) = "th".

:- pred string_to_time_format(string::in, time_format::out) is semidet.

string_to_time_format("no", no_scale).
string_to_time_format("mi", scale_by_millions).
string_to_time_format("th", scale_by_thousands).

:- pred string_to_colour_scheme(string::in, colour_scheme::out) is semidet.

string_to_colour_scheme("cols", column_groups).
string_to_colour_scheme("none", none).

:- func colour_scheme_to_string(colour_scheme) = string.

colour_scheme_to_string(column_groups) = "cols".
colour_scheme_to_string(none)          = "none".

:- pred string_to_box(string::in, box::out) is semidet.

string_to_box("box",   box).
string_to_box("nobox", nobox).

:- func box_to_string(box) = string.

box_to_string(box)   = "box".
box_to_string(nobox) = "nobox".

%-----------------------------------------------------------------------------%
