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

:- type cmd
	--->	quit
	;	timeout(int)
	;	menu
	;	root(fields)
	;	clique(int, fields)
	;	proc(int, fields)
	;	top_procs(sort_measurement, include_descendants,
			display_limit, fields)
	;	proc_static(int)
	;	proc_dynamic(int)
	;	call_site_static(int)
	;	call_site_dynamic(int)
	;	raw_clique(int)
	;	num_proc_statics
	;	num_call_site_statics
	;	num_proc_dynamics
	;	num_call_site_dynamics.

:- type sort_measurement
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

:- type resp
	--->	html(string).

:- type fields	==	string.		% some subset of "apqtw", meaning
					% a: memory allocations
					% p: port counts
					% q: quanta
					% t: times
					% w: memory words
					% The characters must be sorted.

:- func default_fields = string.
:- func all_fields = string.

:- func to_server_pipe_name(string) = string.
:- func from_server_pipe_name(string) = string.
:- func server_startup_name(string) = string.

:- pred to(string::in, cmd::in, io__state::di, io__state::uo) is det.
:- pred from(string::in, resp::out, io__state::di, io__state::uo) is det.

:- pred cmd_to_url(string::in, string::in, cmd::in, string::out) is det.
:- pred cmd_to_query(cmd::in, string::out) is det.
:- pred query_to_cmd(string::in, maybe(cmd)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module util.
:- import_module char, string, list, set, require.

default_fields = "pqw".

all_fields = "apqtw".

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

cmd_to_url(Machine, DataFileName, Cmd, URL) :-
	cmd_to_query(Cmd, Query),
	URL =
		"http://" ++
		Machine ++
		"/cgi-bin/mdprof?" ++
		Query ++
		"$" ++
		DataFileName.

cmd_to_query(Cmd, Query) :-
	(
		Cmd = quit,
		Query = "quit"
	;
		Cmd = timeout(Minutes),
		Query = format("timeout+%d", [i(Minutes)])
	;
		Cmd = menu,
		Query = "menu"
	;
		Cmd = root(Fields),
		Query = format("root+%s", [s(Fields)])
	;
		Cmd = clique(CliqueNum, Fields),
		Query = format("clique+%s+%d", [s(Fields), i(CliqueNum)])
	;
		Cmd = proc(ProcNum, Fields),
		Query = format("proc+%s+%d", [s(Fields), i(ProcNum)])
	;
		Cmd = top_procs(Sort, InclDesc, Limit, Fields),
		sort_to_str(Sort, SortStr),
		incl_desc_to_str(InclDesc, InclDescStr),
		limit_to_str(Limit, LimitStr),
		Query = format("procs+%s+%s+%s+%s",
			[s(SortStr), s(InclDescStr), s(LimitStr), s(Fields)])
	;
		Cmd = proc_static(PSI),
		Query = format("proc_static+%d", [i(PSI)])
	;
		Cmd = proc_dynamic(PDI),
		Query = format("proc_dynamic+%d", [i(PDI)])
	;
		Cmd = call_site_static(CSSI),
		Query = format("call_site_static+%d", [i(CSSI)])
	;
		Cmd = call_site_dynamic(CSDI),
		Query = format("call_site_dynamic+%d", [i(CSDI)])
	;
		Cmd = raw_clique(CI),
		Query = format("raw_clique+%d", [i(CI)])
	;
		Cmd = num_proc_statics,
		Query = "num_proc_statics"
	;
		Cmd = num_proc_dynamics,
		Query = "num_proc_dynamics"
	;
		Cmd = num_call_site_statics,
		Query = "num_call_site_statics"
	;
		Cmd = num_call_site_dynamics,
		Query = "num_call_site_dynamics"
	).

query_to_cmd(QueryString, MaybeCmd) :-
	split(QueryString, ('+'), Pieces),
	(
		(
			Pieces = ["clique", NStr],
			string__to_int(NStr, N),
			Fields = default_fields
		;
			Pieces = ["clique", Fields, NStr],
			string__to_int(NStr, N),
			validate_fields(Fields)
		)
	->
		MaybeCmd = yes(clique(N, Fields))
	;
		(
			Pieces = ["proc", NStr],
			string__to_int(NStr, N),
			Fields = default_fields
		;
			Pieces = ["proc", Fields, NStr],
			string__to_int(NStr, N),
			validate_fields(Fields)
		)
	->
		MaybeCmd = yes(proc(N, Fields))
	;
		(
			Pieces = ["procs", SortStr, InclDescStr,
				LimitStr],
			Fields = default_fields
		;
			Pieces = ["procs", SortStr, InclDescStr,
				LimitStr, Fields],
			validate_fields(Fields)
		),
		translate_criteria(SortStr, Sort,
			InclDescStr, InclDesc, LimitStr, Limit)
	->
		MaybeCmd = yes(top_procs(Sort, InclDesc, Limit, Fields))
	;
		(
			Pieces = ["root"],
			Fields = default_fields
		;
			Pieces = ["root", Fields],
			validate_fields(Fields)
		)
	->
		MaybeCmd = yes(root(Fields))
	;
		Pieces = ["menu"]
	->
		MaybeCmd = yes(menu)
	;
		Pieces = ["proc_static", NStr],
		string__to_int(NStr, N)
	->
		MaybeCmd = yes(proc_static(N))
	;
		Pieces = ["proc_dynamic", NStr],
		string__to_int(NStr, N)
	->
		MaybeCmd = yes(proc_dynamic(N))
	;
		Pieces = ["call_site_static", NStr],
		string__to_int(NStr, N)
	->
		MaybeCmd = yes(call_site_static(N))
	;
		Pieces = ["call_site_dynamic", NStr],
		string__to_int(NStr, N)
	->
		MaybeCmd = yes(call_site_dynamic(N))
	;
		Pieces = ["raw_clique", NStr],
		string__to_int(NStr, N)
	->
		MaybeCmd = yes(raw_clique(N))
	;
		Pieces = ["num_proc_statics"]
	->
		MaybeCmd = yes(num_proc_statics)
	;
		Pieces = ["num_call_site_statics"]
	->
		MaybeCmd = yes(num_call_site_statics)
	;
		Pieces = ["num_proc_dynamics"]
	->
		MaybeCmd = yes(num_proc_dynamics)
	;
		Pieces = ["num_call_site_dynamics"]
	->
		MaybeCmd = yes(num_call_site_dynamics)
	;
		Pieces = ["timeout", TStr],
		string__to_int(TStr, TimeOut)
	->
		MaybeCmd = yes(timeout(TimeOut))
	;
		Pieces = ["quit"]
	->
		MaybeCmd = yes(quit)
	;
		MaybeCmd = no
	).

%-----------------------------------------------------------------------------%

:- pred sort_to_str(sort_measurement::in, string::out) is det.

sort_to_str(calls,  "calls").
sort_to_str(time,   "time").
sort_to_str(allocs, "allocs").
sort_to_str(words,  "words").

:- pred incl_desc_to_str(include_descendants::in, string::out) is det.

incl_desc_to_str(self,          "self").
incl_desc_to_str(self_and_desc, "both").

:- pred limit_to_str(display_limit::in, string::out) is det.

limit_to_str(rank_range(Lo, Hi),   format("%d-%d", [i(Lo), i(Hi)])).
limit_to_str(threshold(Threshold), format("%f", [f(Threshold)])).

:- pred translate_criteria(string::in, sort_measurement::out,
	string::in, include_descendants::out, string::in, display_limit::out)
	is semidet.

translate_criteria(SortStr, Sort, InclDescStr, InclDesc, LimitStr, Limit) :-
	(
		SortStr = "calls",
		Sort = calls
	;
		SortStr = "time",
		Sort = time
	;
		SortStr = "allocs",
		Sort = allocs
	;
		SortStr = "words",
		Sort = words
	),
	(
		InclDescStr = "self",
		InclDesc = self
	;
		InclDescStr = "both",
		InclDesc = self_and_desc
	),
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

:- pred validate_fields(string::in) is semidet.

validate_fields(String) :-
	Chars = string__to_char_list(String),
	list__sort_and_remove_dups(Chars, Chars),
	validate_field_chars(Chars,
		set__list_to_set(string__to_char_list(all_fields))).

:- pred validate_field_chars(list(char)::in, set(char)::in) is semidet.

validate_field_chars([], _).
validate_field_chars([Char | Chars], AvailFields0) :-
	set__delete(AvailFields0, Char, AvailFields1),
	validate_field_chars(Chars, AvailFields1).

%-----------------------------------------------------------------------------%

to(Where, Cmd) -->
	io__tell(Where, Res),
	( { Res = ok } ->
		io__write(Cmd),
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
