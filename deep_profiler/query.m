%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module contains the top level predicates for servicing individual
% queries.

:- module query.

:- interface.

:- import_module profile, interface.
:- import_module io.

:- pred try_exec(cmd::in, preferences::in, deep::in, string::out,
	io__state::di, io__state::uo) is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module measurements, top_procs, html_format, exclude.
:- import_module std_util, bool, int, float, char, string.
:- import_module array, list, assoc_list, set, map, exception, require.

%-----------------------------------------------------------------------------%

try_exec(Cmd, Pref, Deep, HTML, IO0, IO) :-
	try_io(exec(Cmd, Pref, Deep), Result, IO0, IO),
	(
		Result = succeeded(HTML)
	;
		Result = exception(Exception),
		( univ_to_type(Exception, MsgPrime) ->
			Msg = MsgPrime
		; univ_to_type(Exception, software_error(MsgPrime)) ->
			Msg = MsgPrime
		;
			Msg = "unknown exception"
		),
		HTML =
			string__format(
				"<H3>AN EXCEPTION HAS OCCURRED: %s</H3>\n",
				[s(Msg)])
	).

:- pred exec(cmd::in, preferences::in, deep::in, string::out,
	io__state::di, io__state::uo) is det.

exec(restart, _Pref, _Deep, _HTML, IO, IO) :-
	% Our caller is supposed to filter out restart commands.
	error("exec: found restart command").
exec(quit, _Pref, Deep, HTML, IO, IO) :-
	HTML = string__format(
		"<H3>Shutting down deep profile server for %s.</H3>\n",
		[s(Deep ^ data_file_name)]).
exec(timeout(TimeOut), _Pref, _Deep, HTML, IO, IO) :-
	HTML = string__format("<H3>Timeout set to %d minutes</H3>\n",
		[i(TimeOut)]).
exec(Cmd, Pref, Deep, HTML, IO, IO) :-
	Cmd = menu,
	HTML = generate_menu_page(Cmd, Pref, Deep).
exec(Cmd, Pref, Deep, HTML, IO, IO) :-
	Cmd = root(MaybePercent),
	deep_lookup_clique_index(Deep, Deep ^ root, RootCliquePtr),
	RootCliquePtr = clique_ptr(RootCliqueNum),
	(
		MaybePercent = yes(Percent),
		HTML = chase_the_action(Cmd, RootCliqueNum,
			Pref, Deep, Percent)
	;
		MaybePercent = no,
		generate_clique_page(Cmd, RootCliqueNum, Pref, Deep, HTML,
			100, _)
	).
exec(Cmd, Pref, Deep, HTML, IO, IO) :-
	Cmd = clique(CliqueNum),
	CliquePtr = clique_ptr(CliqueNum),
	( valid_clique_ptr(Deep, CliquePtr) ->
		generate_clique_page(Cmd, CliqueNum, Pref, Deep, HTML, 100, _)
	;
		HTML =
			page_banner(Cmd, Pref) ++
			"There is no clique with that number.\n" ++
			page_footer(Cmd, Pref, Deep)
	).
exec(Cmd, Pref, Deep, HTML, IO, IO) :-
	Cmd = proc(PSI),
	PSPtr = proc_static_ptr(PSI),
	( valid_proc_static_ptr(Deep, PSPtr) ->
		HTML = generate_proc_page(Cmd, PSPtr, Pref, Deep)
	;
		HTML =
			page_banner(Cmd, Pref) ++
			"There is no procedure with that number.\n" ++
			page_footer(Cmd, Pref, Deep)
	).
exec(Cmd, Pref, Deep, HTML, IO0, IO) :-
	Cmd = proc_callers(PSI, CallerGroups, BunchNum),
	PSPtr = proc_static_ptr(PSI),
	( valid_proc_static_ptr(Deep, PSPtr) ->
		generate_proc_callers_page(Cmd, PSPtr, CallerGroups, BunchNum,
			Pref, Deep, HTML, IO0, IO)
	;
		HTML =
			page_banner(Cmd, Pref) ++
			"There is no procedure with that number.\n" ++
			page_footer(Cmd, Pref, Deep),
		IO = IO0
	).
exec(Cmd, Pref, Deep, HTML, IO, IO) :-
	Cmd = modules,
	HTML = generate_modules_page(Cmd, Pref, Deep).
exec(Cmd, Pref, Deep, HTML, IO, IO) :-
	Cmd = module(ModuleName),
	( map__search(Deep ^ module_data, ModuleName, ModuleData) ->
		HTML = generate_module_page(Cmd, ModuleName, ModuleData,
			Pref, Deep)
	;
		HTML =
			page_banner(Cmd, Pref) ++
			"There is no procedure with that number.\n" ++
			page_footer(Cmd, Pref, Deep)
	).
exec(Cmd, Pref, Deep, HTML, IO, IO) :-
	Cmd = top_procs(Limit, CostKind, InclDesc, Scope),
	HTML = generate_top_procs_page(Cmd, Limit, CostKind, InclDesc, Scope,
		Pref, Deep).
exec(proc_static(PSI), _Pref, Deep, HTML, IO, IO) :-
	HTML = generate_proc_static_debug_page(PSI, Deep).
exec(proc_dynamic(PDI), _Pref, Deep, HTML, IO, IO) :-
	HTML = generate_proc_dynamic_debug_page(PDI, Deep).
exec(call_site_static(CSSI), _Pref, Deep, HTML, IO, IO) :-
	HTML = generate_call_site_static_debug_page(CSSI, Deep).
exec(call_site_dynamic(CSDI), _Pref, Deep, HTML, IO, IO) :-
	HTML = generate_call_site_dynamic_debug_page(CSDI, Deep).
exec(raw_clique(CI), _Pref, Deep, HTML, IO, IO) :-
	HTML = generate_clique_debug_page(CI, Deep).

%-----------------------------------------------------------------------------%

:- func generate_proc_static_debug_page(int, deep) = string.

generate_proc_static_debug_page(PSI, Deep) = HTML :-
	PSPtr = proc_static_ptr(PSI),
	( valid_proc_static_ptr(Deep, PSPtr) ->
		deep_lookup_proc_statics(Deep, PSPtr, PS),
		Refined = PS ^ ps_refined_id,
		Raw = PS ^ ps_raw_id,
		FileName = PS ^ ps_file_name,
		HTML =
			"<HTML>\n" ++
			Refined ++ " " ++ Raw ++ " " ++ FileName ++ " " ++
			string__int_to_string(array__max(PS ^ ps_sites)) ++
			"</HTML>\n"
	;
		HTML =
			"<HTML>\n" ++
			"Invalid proc_static_ptr" ++
			"</HTML>\n"
	).

:- func generate_proc_dynamic_debug_page(int, deep) = string.

generate_proc_dynamic_debug_page(PDI, Deep) = HTML :-
	PDPtr = proc_dynamic_ptr(PDI),
	( valid_proc_dynamic_ptr(Deep, PDPtr) ->
		deep_lookup_proc_dynamics(Deep, PDPtr, PD),
		PSPtr = PD ^ pd_proc_static,
		PSPtr = proc_static_ptr(PSI),
		HTML =
			"<HTML>\n" ++
			string__format("proc_static %d, ", [i(PSI)]) ++
			array_slots_to_html(PD ^ pd_sites) ++
			"</HTML>\n"
	;
		HTML =
			"<HTML>\n" ++
			"Invalid proc_dynamic_ptr" ++
			"</HTML>\n"
	).

:- func generate_call_site_static_debug_page(int, deep) = string.

generate_call_site_static_debug_page(CSSI, Deep) = HTML :-
	CSSPtr = call_site_static_ptr(CSSI),
	( valid_call_site_static_ptr(Deep, CSSPtr) ->
		deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
		ContainerPtr = CSS ^ css_container,
		ContainerPtr = proc_static_ptr(Container),
		HTML =
			"<HTML>\n" ++
			string__int_to_string(Container) ++ " " ++
			string__int_to_string(CSS ^ css_slot_num) ++ " " ++
			string__int_to_string(CSS ^ css_line_num) ++ " " ++
			kind_and_callee_to_string(CSS ^ css_kind) ++ " " ++
			CSS ^ css_goal_path ++
			"</HTML>\n"
	;
		HTML =
			"<HTML>\n" ++
			"Invalid call_site_static_ptr" ++
			"</HTML>\n"
	).

:- func generate_call_site_dynamic_debug_page(int, deep) = string.

generate_call_site_dynamic_debug_page(CSDI, Deep) = HTML :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	( valid_call_site_dynamic_ptr(Deep, CSDPtr) ->
		deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
		CSD ^ csd_caller = proc_dynamic_ptr(CallerPDI),
		CSD ^ csd_callee = proc_dynamic_ptr(CalleePDI),
		HTML =
			"<HTML>\n" ++
			string__int_to_string(CallerPDI) ++ " -> " ++
			string__int_to_string(CalleePDI) ++ ": " ++
			own_to_string(CSD ^ csd_own_prof) ++
			"</HTML>\n"
	;
		HTML =
			"<HTML>\n" ++
			"Invalid call_site_dynamic_ptr" ++
			"</HTML>\n"
	).

:- func generate_clique_debug_page(int, deep) = string.

generate_clique_debug_page(CI, Deep) = HTML :-
	CliquePtr = clique_ptr(CI),
	( valid_clique_ptr(Deep, CliquePtr) ->
		deep_lookup_clique_parents(Deep, CliquePtr, Parent),
		Parent = call_site_dynamic_ptr(ParentPDI),
		ParentStr = string__format("%d ->", [i(ParentPDI)]),
		deep_lookup_clique_members(Deep, CliquePtr, Members),
		HTML =
			"<HTML>\n" ++
			ParentStr ++
			list__foldl(append_pdi_to_string, Members, "") ++
			"</HTML>\n"
	;
		HTML =
			"<HTML>\n" ++
			"Invalid call_site_dynamic_ptr" ++
			"</HTML>\n"
	).

%-----------------------------------------------------------------------------%

:- func array_slots_to_html(array(call_site_array_slot)) = string.

array_slots_to_html(SlotArray) = HTML :-
	array__to_list(SlotArray, SlotList),
	list__foldl(append_slot_to_string, SlotList, "multi", HTML).

:- pred append_slot_to_string(call_site_array_slot::in,
	string::in, string::out) is det.

append_slot_to_string(Slot, Str0, Str) :-
	Str = Str0 ++ " " ++ array_slot_to_html(Slot).

:- func array_slot_to_html(call_site_array_slot) = string.

array_slot_to_html(normal(CSDPtr)) = HTML :-
	CSDPtr = call_site_dynamic_ptr(CSDI),
	HTML = "normal " ++ string__int_to_string(CSDI).
array_slot_to_html(multi(_, CSDPtrArray)) = HTML :-
	array__to_list(CSDPtrArray, CSDPtrs),
	list__foldl(append_csdi_to_string, CSDPtrs, "", CSDI_HTML),
	list__length(CSDPtrs, CSDPtrCount),
	HTML = string__format("multi(%d): [", [i(CSDPtrCount)])
		++ CSDI_HTML ++ "]".

:- pred append_csdi_to_string(call_site_dynamic_ptr::in,
	string::in, string::out) is det.

append_csdi_to_string(call_site_dynamic_ptr(CSDI), Str0, Str) :-
	Str = Str0 ++ " " ++ string__int_to_string(CSDI).

:- func append_pdi_to_string(proc_dynamic_ptr, string) = string.

append_pdi_to_string(proc_dynamic_ptr(PDI), Str0) =
	Str0 ++ " " ++ string__int_to_string(PDI).

:- func kind_and_callee_to_string(call_site_kind_and_callee) = string.

kind_and_callee_to_string(normal_call(proc_static_ptr(PSI), TypeSpec)) =
	"normal " ++ string__int_to_string(PSI) ++ " " ++ TypeSpec.
kind_and_callee_to_string(special_call) = "special_call".
kind_and_callee_to_string(higher_order_call) = "higher_order_call".
kind_and_callee_to_string(method_call) = "method_call".
kind_and_callee_to_string(callback) = "callback".

%-----------------------------------------------------------------------------%

:- func call_site_kind_and_callee_to_html(call_site_kind_and_callee) = string.

call_site_kind_and_callee_to_html(normal_call(_, _)) = "normal_call".
call_site_kind_and_callee_to_html(special_call) =      "special_call".
call_site_kind_and_callee_to_html(higher_order_call) = "higher_order_call".
call_site_kind_and_callee_to_html(method_call) =       "method_call".
call_site_kind_and_callee_to_html(callback) =          "callback".

%-----------------------------------------------------------------------------%

:- func generate_menu_page(cmd, preferences, deep) = string.

generate_menu_page(Cmd, Pref, Deep) = HTML :-
	HTML =
		page_banner(Cmd, Pref) ++
		"<p>\n" ++
		menu_text ++
		"<ul>\n" ++
		"<li>\n" ++
		menu_item(Deep, Pref, root(no),
			"Exploring the call graph, starting at the root.") ++
		"<li>\n" ++
		menu_item(Deep, Pref, root(yes(90)),
			"Exploring the call graph, starting at the action.") ++
		"<li>\n" ++
		menu_item(Deep, Pref, modules,
			"Exploring the program module by module.") ++
		"<li>\n" ++
		menu_item(Deep, Pref,
			top_procs(rank_range(1, 100), time,
				self, overall),
			"Top 100 most expensive procedures: time, self.") ++
		"<li>\n" ++
		menu_item(Deep, Pref,
			top_procs(rank_range(1, 100), time,
				self_and_desc, overall),
			"Top 100 most expensive procedures: time, self+desc.")
			++
		"<li>\n" ++
		menu_item(Deep, Pref,
			top_procs(rank_range(1, 100), words,
				self, overall),
			"Top 100 most expensive procedures: words, self.") ++
		"<li>\n" ++
		menu_item(Deep, Pref,
			top_procs(rank_range(1, 100), words,
				self_and_desc, overall),
			"Top 100 most expensive procedures: words, self+desc.")
			++
		"<li>\n" ++
		menu_item(Deep, Pref,
			top_procs(threshold(0.1), time,
				self, overall),
			"Procedures above 0.1% threshold: time, self.") ++
		"<li>\n" ++
		menu_item(Deep, Pref,
			top_procs(threshold(0.1), time,
				self_and_desc, overall),
			"Procedures above 1% threshold: time, self+desc.")
			++
		"<li>\n" ++
		menu_item(Deep, Pref,
			top_procs(threshold(0.1), words,
				self, overall),
			"Procedures above 0.1% threshold: words, self.") ++
		"<li>\n" ++
		menu_item(Deep, Pref,
			top_procs(threshold(0.1), words,
				self_and_desc, overall),
			"Procedures above 1% threshold: words, self+desc.")
			++
		"</ul>\n" ++
		"<p>\n" ++
		present_stats(Deep) ++
		page_footer(Cmd, Pref, Deep).

:- func menu_text = string.

menu_text =
	"You can start exploring the deep profile at the following points.\n".

:- func menu_item(deep, preferences, cmd, string) = string.

menu_item(Deep, Pref, Cmd, Text) =
	string__format("<A HREF=""%s"">%s</A>\n",
		[s(deep_cmd_pref_to_url(Pref, Deep, Cmd)), s(Text)]).

:- func present_stats(deep) = string.

present_stats(Deep) = HTML :-
	Stats = Deep ^ profile_stats,
	lookup_ticks_per_sec(Stats, TicksPerSec, Assumed),
	(
		Assumed = yes,
		AssumedStr = " (assumed)"
	;
		Assumed = no,
		AssumedStr = ""
	),
	HTML =
		"<TABLE>\n" ++
		"<TR><TD ALIGN=left>Quanta per second:</TD>\n" ++
		string__format("<TD ALIGN=right>%d%s</TD></TR>\n",
			[i(TicksPerSec), s(AssumedStr)]) ++
		"<TR><TD ALIGN=left>Quanta in user code:</TD>\n" ++
		string__format("<TD ALIGN=right>%d</TD></TR>\n",
			[i(Stats ^ user_quanta)]) ++
		"<TR><TD ALIGN=left>Quanta in instrumentation:</TD>\n" ++
		string__format("<TD ALIGN=right>%d</TD></TR>\n",
			[i(Stats ^ instrument_quanta)]) ++
		"<TR><TD ALIGN=left>CallSiteDynamic structures:</TD>\n" ++
		string__format("<TD ALIGN=right>%d</TD></TR>\n",
			[i(Stats ^ max_csd)]) ++
		"<TR><TD ALIGN=left>ProcDynamic structures:</TD>\n" ++
		string__format("<TD ALIGN=right>%d</TD></TR>\n",
			[i(Stats ^ max_pd)]) ++
		"<TR><TD ALIGN=left>CallSiteStatic structures:</TD>\n" ++
		string__format("<TD ALIGN=right>%d</TD></TR>\n",
			[i(Stats ^ max_css)]) ++
		"<TR><TD ALIGN=left>ProcStatic structures:</TD>\n" ++
		string__format("<TD ALIGN=right>%d</TD></TR>\n",
			[i(Stats ^ max_ps)]) ++
		"<TR><TD ALIGN=left>Cliques:</TD>\n" ++
		string__format("<TD ALIGN=right>%d</TD></TR>\n",
			[i(array__max(Deep ^ clique_members))]) ++
		"</TABLE>\n".

%-----------------------------------------------------------------------------%

:- func chase_the_action(cmd, int, preferences, deep, int) = string.

chase_the_action(Cmd, CliqueNum, Pref, Deep, Percent) = HTML :-
	generate_clique_page(Cmd, CliqueNum, Pref, Deep, HTML0,
		Percent, ActionPtrs),
	( ActionPtrs = [clique_ptr(ActionCliqueNum)] ->
		HTML = chase_the_action(Cmd, ActionCliqueNum,
			Pref, Deep, Percent)
	;
		HTML = HTML0
	).

%-----------------------------------------------------------------------------%

:- pred generate_clique_page(cmd::in, int::in, preferences::in, deep::in,
	string::out, int::in, list(clique_ptr)::out) is det.

generate_clique_page(Cmd, CliqueNum, Pref, Deep, HTML,
		Percent, ActionPtrs) :-
	clique_to_html(Pref, Deep, clique_ptr(CliqueNum),
		CliqueHTML, Percent, ActionPtrs),
	HTML =
		page_banner(Cmd, Pref) ++
		string__format("<H3>Clique %d:</H3>\n",
			[i(CliqueNum)]) ++
		table_start(Pref) ++
		fields_header(Pref, source_proc, totals_meaningful,
			wrap_clique_links(clique_ptr(CliqueNum),
				Pref, Deep)) ++
		CliqueHTML ++
		table_end(Pref) ++
		page_footer(Cmd, Pref, Deep).

:- func generate_proc_page(cmd, proc_static_ptr, preferences, deep)
	= string.

generate_proc_page(Cmd, PSPtr, Pref, Deep) =
	page_banner(Cmd, Pref) ++
	string__format("<H3>Summary of procedure %s:</H3>\n",
		[s(proc_static_name(Deep, PSPtr))]) ++
	table_start(Pref) ++
	fields_header(Pref, source_proc, totals_meaningful,
		wrap_proc_links(PSPtr, Pref, Deep)) ++
	proc_summary_to_html(Pref, Deep, PSPtr) ++
	table_end(Pref) ++
	"<p>\n" ++
	proc_summary_toggles_to_html(Pref, Deep, PSPtr) ++
	page_footer(Cmd, Pref, Deep).

:- pred generate_proc_callers_page(cmd::in, proc_static_ptr::in,
	caller_groups::in, int::in, preferences::in, deep::in, string::out,
	io__state::di, io__state::uo) is det.

generate_proc_callers_page(Cmd, PSPtr, CallerGroups, BunchNum, Pref, Deep,
		HTML, IO0, IO) :-
	proc_callers_to_html(Pref, Deep, PSPtr, CallerGroups, BunchNum,
		MaybePage, IO0, IO),
	(
		MaybePage = ok({IdFields, Heading, CallersHTML, Toggles}),
		HTML =
			page_banner(Cmd, Pref) ++
			Heading ++
			( CallersHTML = "" ->
				""
			;
				table_start(Pref) ++
				fields_header(Pref, IdFields,
					totals_meaningful,
					wrap_proc_callers_links(PSPtr,
						CallerGroups, 1,
						Pref, Deep)) ++
				CallersHTML ++
				table_end(Pref) ++
				"<p>\n"
			) ++
			Toggles ++
			page_footer(Cmd, Pref, Deep)
	;
		MaybePage = error(Msg),
		HTML =
			string__format("<H3>%s</H3>\n", [s(Msg)])
	).

:- func generate_modules_page(cmd, preferences, deep) = string.

generate_modules_page(Cmd, Pref, Deep) =
	page_banner(Cmd, Pref) ++
	"<H3>The modules of the program:</H3>\n" ++
	table_start(Pref) ++
	fields_header(Pref, rank_module, totals_not_meaningful,
		wrap_modules_links(Pref, Deep)) ++
	modules_to_html(Pref, Deep) ++
	table_end(Pref) ++
	page_footer(Cmd, Pref, Deep).

:- func generate_module_page(cmd, string, module_data, preferences, deep)
	= string.

generate_module_page(Cmd, ModuleName, ModuleData, Pref, Deep) = HTML :-
	module_to_html(Pref, Deep, ModuleName, ModuleData,
		IdFields, ModulesHTML),
	HTML =
		page_banner(Cmd, Pref) ++
		string__format("<H3>The procedures of module %s:</H3>\n",
			[s(ModuleName)]) ++
		table_start(Pref) ++
		fields_header(Pref, IdFields, totals_meaningful,
			wrap_module_links(ModuleName, Pref, Deep)) ++
		ModulesHTML ++
		table_end(Pref) ++
		page_footer(Cmd, Pref, Deep).

:- func generate_top_procs_page(cmd, display_limit,
	cost_kind, include_descendants, measurement_scope,
	preferences, deep) = string.

generate_top_procs_page(Cmd, Limit, CostKind, InclDesc0, Scope0, Pref, Deep)
		= HTML :-
	( CostKind = calls ->
		% counting calls is incompatible both with self_and_desc
		% and per_call.
		InclDesc = self,
		Scope = overall
	;
		InclDesc = InclDesc0,
		Scope = Scope0
	),
	MaybeTopPSIs = find_top_procs(CostKind, InclDesc, Scope, Limit, Deep),
	(
		MaybeTopPSIs = error(ErrorMessage),
		HTML =
			page_banner(Cmd, Pref) ++
			ErrorMessage ++ "\n" ++
			page_footer(Cmd, Pref, Deep)
	;
		MaybeTopPSIs = ok(TopPSIs),
		ToggleLimitHTML = "",
		ToggleCostHTML = toggle_cost_criteria_in_top_procs_cmd(
			Pref, Deep, Limit, CostKind, InclDesc, Scope),
		Desc = cost_criteria_to_description(CostKind, InclDesc, Scope),
		Heading = string__format("<H3>Top procedures %s</H3>\n",
			[s(Desc)]),
		( TopPSIs = [] ->
			HTML =
				page_banner(Cmd, Pref) ++
				Heading ++ "<p>\n" ++
				"No procedures match the specification.\n" ++
				"<p>\n" ++
				ToggleLimitHTML ++
				ToggleCostHTML ++
				page_footer(Cmd, Pref, Deep)
		;
			TopProcs = list__map(
				lookup_proc_total_to_html(Pref, Deep, no, ""),
				list__map(wrap_proc_static_ptr, TopPSIs)),
			RankedTopProcs = add_ranks(TopProcs),
			SummaryHTMLs = list__map(
				two_id_line_to_html(Pref, Deep,
					totals_meaningful),
				RankedTopProcs),
			HTML =
				page_banner(Cmd, Pref) ++
				Heading ++ "<p>\n" ++
				table_start(Pref) ++
				fields_header(Pref, rank_proc,
					totals_meaningful,
					wrap_top_procs_links(Limit, Pref,
						Deep)) ++
				string__append_list(SummaryHTMLs) ++
				table_end(Pref) ++
				"<p>\n" ++
				ToggleLimitHTML ++
				ToggleCostHTML ++
				page_footer(Cmd, Pref, Deep)
		)
	).

%-----------------------------------------------------------------------------%

:- func modules_to_html(preferences, deep) = string.

modules_to_html(Pref, Deep) = HTML :-
	map__to_assoc_list(Deep ^ module_data, ModulePairs0),
	list__filter(not_mercury_runtime, ModulePairs0, ModulePairs),
	ModuleLines = list__map(module_summary_to_html(Pref, Deep),
		ModulePairs),
	SortedModuleLines = sort_line_groups(Pref ^ pref_criteria,
		ModuleLines),
	RankedModuleLines = add_ranks(SortedModuleLines),
	ModuleHTMLs = list__map(
		two_id_line_to_html(Pref, Deep, totals_not_meaningful),
		RankedModuleLines),
	HTML =
		separator_row(Pref, rank_module, totals_not_meaningful) ++
		string__append_list(ModuleHTMLs).

:- pred not_mercury_runtime(pair(string, module_data)::in) is semidet.

not_mercury_runtime(ModuleName - _) :-
	ModuleName \= "Mercury runtime".

:- func module_summary_to_html(preferences, deep, pair(string, module_data))
	= one_id_line.

module_summary_to_html(Pref, Deep, ModuleName - ModuleData) = LineGroup :-
	Own = ModuleData ^ module_own,
	Desc = ModuleData ^ module_desc,
	HTML =
		string__format("<TD><A HREF=""%s"">%s</A></TD>\n",
			[s(deep_cmd_pref_to_url(Pref, Deep,
				module(ModuleName))),
			s(ModuleName)]),
	LineGroup = line_group(ModuleName, 0, ModuleName, Own, Desc, HTML,
		unit).

%-----------------------------------------------------------------------------%

:- pred module_to_html(preferences::in, deep::in, string::in, module_data::in,
	id_fields::out, string::out) is det.

module_to_html(Pref, Deep, _ModuleName, ModuleData, IdHeaders, HTML) :-
	ModuleData = module_data(_Own, _Desc, PSPtrs),
	ProcLines = list__map(lookup_proc_total_to_html(Pref, Deep, yes, ""),
		PSPtrs),
	Criteria = Pref ^ pref_criteria,
	SortedProcLines = sort_line_groups(Criteria, ProcLines),
	( Criteria = by_cost(_, _, _) ->
		IdHeaders = rank_proc,
		RankedProcLines = add_ranks(SortedProcLines)
	;
		IdHeaders = source_proc,
		RankedProcLines = list__map(add_self_context, SortedProcLines)
	),
	ProcHTMLs = list__map(
		two_id_line_to_html(Pref, Deep, totals_meaningful),
		RankedProcLines),
	HTML =
		separator_row(Pref, IdHeaders, totals_meaningful) ++
		string__append_list(ProcHTMLs).

%-----------------------------------------------------------------------------%

:- pred clique_to_html(preferences::in, deep::in, clique_ptr::in,
	string::out, int::in, list(clique_ptr)::out) is det.

clique_to_html(Pref, Deep, CliquePtr, HTML, PerCent, ActionPtrs) :-
	(
		Pref ^ pref_anc = yes(AncestorLimit),
		RespectLimit = yes
	;
		Pref ^ pref_anc = no,
		AncestorLimit = 0, % the value doesn't matter
		RespectLimit = no
	),
	clique_ancestors_to_html(Pref, Deep,
		AncestorLimit, RespectLimit, CliquePtr, Ancestors, Cutoff),
	deep_lookup_clique_members(Deep, CliquePtr, PDPtrs),
	list__foldl(group_proc_dynamics_by_proc_static(Deep), PDPtrs,
		map__init, PStoPDsMap),
	map__to_assoc_list(PStoPDsMap, PStoPDsList0),

	deep_lookup_clique_parents(Deep, CliquePtr, EntryCSDPtr),
	( valid_call_site_dynamic_ptr(Deep, EntryCSDPtr) ->
		deep_lookup_call_site_dynamics(Deep, EntryCSDPtr, EntryCSD),
		EntryPDPtr = EntryCSD ^ csd_callee,
		list__filter(proc_group_contains(EntryPDPtr), PStoPDsList0,
			EntryGroup, RestGroup),
		list__append(EntryGroup, RestGroup, PStoPDsList)
	;
		PStoPDsList = PStoPDsList0
	),

	list__map2(procs_in_clique_to_html(Pref, Deep, CliquePtr, PerCent),
		PStoPDsList, PDsStrs, ActionPtrLists),
	list__condense(ActionPtrLists, ActionPtrs),
	string__append_list(PDsStrs, ProcGroups),
	(
		Cutoff = yes,
		Heading = string__format("The %d closest ancestors:",
			[i(AncestorLimit)])
	;
		Cutoff = no,
		Heading = "Ancestors:"
	),
	HTML =
		header_row(Heading, Pref, source_proc, totals_meaningful) ++
		separator_row(Pref, source_proc, totals_meaningful) ++
		Ancestors ++
		separator_row(Pref, source_proc, totals_meaningful) ++
		header_row("Procedures of the clique:",
			Pref, source_proc, totals_meaningful) ++
		separator_row(Pref, source_proc, totals_meaningful) ++
		ProcGroups.

:- pred proc_group_contains(proc_dynamic_ptr::in,
	pair(proc_static_ptr, list(proc_dynamic_ptr))::in) is semidet.

proc_group_contains(EntryPDPtr, _ - PDPtrs) :-
	list__member(EntryPDPtr, PDPtrs).

:- pred clique_ancestors_to_html(preferences::in, deep::in, int::in, bool::in,
	clique_ptr::in, string::out, bool::out) is det.

clique_ancestors_to_html(Pref, Deep, AncestorLimit, RespectLimit, CliquePtr,
		HTML, Cutoff) :-
	deep_lookup_clique_parents(Deep, CliquePtr, EntryCSDPtr),
	( valid_call_site_dynamic_ptr(Deep, EntryCSDPtr) ->
		deep_lookup_call_site_dynamics(Deep, EntryCSDPtr, EntryCSD),
		EntryPDPtr = EntryCSD ^ csd_caller,
		( EntryPDPtr = Deep ^ root ->
			% we have reached the root
			HTML = "",
			Cutoff = no
		; RespectLimit = yes, AncestorLimit =< 0 ->
			HTML = "",
			Cutoff = yes
		;
			deep_lookup_clique_index(Deep, EntryPDPtr,
				EntryCliquePtr),
			ThisLine = call_site_dynamic_to_html(Pref, Deep,
				ancestor_display, yes(EntryCliquePtr),
				EntryCSDPtr),
			ThisHTML = two_id_line_to_html(Pref, Deep,
				totals_meaningful, ThisLine),
			clique_ancestors_to_html(Pref, Deep, AncestorLimit - 1,
				RespectLimit, EntryCliquePtr,
				AncestorHTML, Cutoff),
			HTML =
				AncestorHTML ++
				ThisHTML
		)
	;
		% we have reached the parent of root
		HTML = "",
		Cutoff = no
	).

:- pred group_proc_dynamics_by_proc_static(deep::in, proc_dynamic_ptr::in,
	map(proc_static_ptr, list(proc_dynamic_ptr))::in,
	map(proc_static_ptr, list(proc_dynamic_ptr))::out) is det.

group_proc_dynamics_by_proc_static(Deep, PDPtr, PStoPDsMap0, PStoPDsMap) :-
	require(valid_proc_dynamic_ptr(Deep, PDPtr),
		"group_proc_dynamics_by_proc_static: invalid PDPtr"),
	deep_lookup_proc_dynamics(Deep, PDPtr, PD),
	PSPtr = PD ^ pd_proc_static,
	( map__search(PStoPDsMap0, PSPtr, PSPDs0) ->
		PSPDs = [PDPtr | PSPDs0],
		map__det_update(PStoPDsMap0, PSPtr, PSPDs, PStoPDsMap)
	;
		map__det_insert(PStoPDsMap0, PSPtr, [PDPtr], PStoPDsMap)
	).

:- pred procs_in_clique_to_html(preferences::in, deep::in, clique_ptr::in,
	int::in, pair(proc_static_ptr, list(proc_dynamic_ptr))::in,
	string::out, list(clique_ptr)::out) is det.

procs_in_clique_to_html(Pref, Deep, CliquePtr, Percent, PSPtr - PDPtrs,
		HTML, ActionPtrs) :-
	( PDPtrs = [] ->
		HTML = "",
		ActionPtrs = []
	; PDPtrs = [PDPtr] ->
		proc_in_clique_to_html(Pref, Deep, CliquePtr, Percent, PDPtr,
			HTML, ActionPtrs)
	;
		list__map(deep_lookup_pd_own(Deep), PDPtrs, ProcOwns),
		list__map(deep_lookup_pd_desc(Deep), PDPtrs, ProcDescs),
		ProcOwn = sum_own_infos(ProcOwns),
		ProcDesc = sum_inherit_infos(ProcDescs),
		ProcTotal = proc_total_to_two_id_line(Pref, Deep,
			yes, "summary ", PSPtr, ProcOwn, ProcDesc),
		list__map2(
			proc_in_clique_to_html(Pref, Deep, CliquePtr, Percent),
			PDPtrs, ComponentHTMLs, ActionPtrLists),
		list__condense(ActionPtrLists, ActionPtrs),
		string__append_list(ComponentHTMLs, ComponentHTML),
		HTML =
			separator_row(Pref, source_proc, totals_meaningful) ++
			two_id_line_to_html(Pref, Deep, totals_meaningful,
				ProcTotal) ++
			separator_row(Pref, source_proc, totals_meaningful) ++
			ComponentHTML
	).

:- pred proc_in_clique_to_html(preferences::in, deep::in, clique_ptr::in,
	int::in, proc_dynamic_ptr::in, string::out, list(clique_ptr)::out)
	is det.

proc_in_clique_to_html(Pref, Deep, CliquePtr, Percent, PDPtr,
		HTML, ActionPtrs) :-
	( valid_proc_dynamic_ptr(Deep, PDPtr) ->
		deep_lookup_pd_own(Deep, PDPtr, ProcOwn),
		deep_lookup_pd_desc(Deep, PDPtr, ProcDesc),
		deep_lookup_proc_dynamics(Deep, PDPtr, PD),
		PSPtr = PD ^ pd_proc_static,
		ProcTotal = proc_total_to_two_id_line(Pref, Deep,
			yes, "", PSPtr, ProcOwn, ProcDesc),
		child_call_sites(Deep ^ proc_dynamics, Deep ^ proc_statics,
			PDPtr, GroupPairs),
		( GroupPairs = [] ->
			HTML =
				separator_row(Pref, source_proc,
					totals_meaningful) ++
				two_id_line_to_html(Pref, Deep,
					totals_meaningful,ProcTotal),
			ActionPtrs = []
		;
			list__map2(call_site_clique_to_html(Pref, Deep,
				CliquePtr, Percent),
				GroupPairs, CallSiteLists, ActionPtrLists),
			list__condense(CallSiteLists, CallSites),
			list__condense(ActionPtrLists, ActionPtrs),
			SortedCallSites = sort_line_groups(
				Pref ^ pref_criteria, CallSites),
			BodyHTMLs = list__map(
				two_id_line_group_to_html(Pref, Deep,
					totals_meaningful),
				SortedCallSites),
			HTML =
				separator_row(Pref, source_proc,
					totals_meaningful) ++
				two_id_line_to_html(Pref, Deep,
					totals_meaningful,ProcTotal) ++
				separator_row(Pref, source_proc,
					totals_meaningful) ++
				string__append_list(BodyHTMLs)
		)
	;
		HTML = "",
		ActionPtrs = []
	).

:- pred child_call_sites(proc_dynamics::in, proc_statics::in,
	proc_dynamic_ptr::in,
	assoc_list(call_site_static_ptr, call_site_array_slot)::out) is det.

child_call_sites(ProcDynamics, ProcStatics, PDPtr, PairedSlots) :-
	lookup_proc_dynamics(ProcDynamics, PDPtr, PD),
	PSPtr = PD ^ pd_proc_static,
	CSDArray = PD ^ pd_sites,
	lookup_proc_statics(ProcStatics, PSPtr, PS),
	CSSArray = PS ^ ps_sites,
	array__to_list(CSDArray, CSDSlots),
	array__to_list(CSSArray, CSSSlots),
	assoc_list__from_corresponding_lists(CSSSlots, CSDSlots, PairedSlots).

%-----------------------------------------------------------------------------%

:- func lookup_proc_total_to_html(preferences, deep, bool, string,
	proc_static_ptr) = one_id_line.

lookup_proc_total_to_html(Pref, Deep, Bold, Prefix, PSPtr) = LineGroup :-
	deep_lookup_ps_own(Deep, PSPtr, Own),
	deep_lookup_ps_desc(Deep, PSPtr, Desc),
	LineGroup = proc_total_to_html(Pref, Deep, Bold, Prefix,
		PSPtr, Own, Desc).

:- func lookup_proc_total_to_two_id_line(preferences, deep, bool, string,
	proc_static_ptr) = two_id_line.

lookup_proc_total_to_two_id_line(Pref, Deep, Bold, Prefix, PSPtr) = LineGroup :-
	deep_lookup_ps_own(Deep, PSPtr, Own),
	deep_lookup_ps_desc(Deep, PSPtr, Desc),
	LineGroup = proc_total_to_two_id_line(Pref, Deep, Bold, Prefix,
		PSPtr, Own, Desc).

:- func proc_total_to_html(preferences, deep, bool, string,
	proc_static_ptr, own_prof_info, inherit_prof_info) = one_id_line.

proc_total_to_html(Pref, Deep, Bold, Prefix, PSPtr, Own, Desc)
		= LineGroup :-
	proc_total_to_html_base(Pref, Deep, 1, Bold, Prefix, PSPtr,
		FileName, LineNumber, ProcName, HTML),
	LineGroup = line_group(FileName, LineNumber, ProcName, Own, Desc, HTML,
		unit).

:- func proc_total_to_two_id_line(preferences, deep, bool, string,
	proc_static_ptr, own_prof_info, inherit_prof_info) = two_id_line.

proc_total_to_two_id_line(Pref, Deep, Bold, Prefix, PSPtr, Own, Desc)
		= LineGroup :-
	proc_total_to_html_base(Pref, Deep, 2, Bold, Prefix, PSPtr,
		FileName, LineNumber, ProcName, HTML),
	LineGroup = line_group(FileName, LineNumber, ProcName, Own, Desc, HTML,
		unit).

:- pred proc_total_to_html_base(preferences::in, deep::in,
	int::in, bool::in, string::in, proc_static_ptr::in,
	string::out, int::out, string::out, string::out) is det.

proc_total_to_html_base(Pref, Deep, Span, Bold, Prefix, PSPtr,
		FileName, LineNumber, ProcName, HTML) :-
	proc_static_to_line_group_info(Pref, Deep, PSPtr, FileName, LineNumber,
		ProcName, WrappedProcName),
	(
		Bold = no,
		BoldStart = "",
		BoldEnd = ""
	;
		Bold = yes,
		BoldStart = "<B>",
		BoldEnd = "</B>"
	),
	HTML = string__format("<TD CLASS=id COLSPAN=%d>%s%s%s%s</TD>\n",
		[i(Span), s(BoldStart), s(Prefix),
		s(WrappedProcName), s(BoldEnd)]).

%-----------------------------------------------------------------------------%

:- pred call_site_clique_to_html(preferences::in, deep::in,
	clique_ptr::in, int::in,
	pair(call_site_static_ptr, call_site_array_slot)::in,
	list(two_id_line_group)::out, list(clique_ptr)::out) is det.

call_site_clique_to_html(Pref, Deep, CallerCliquePtr, Percent, Pair,
		LineGroups, ActionPtrs) :-
	Pair = CSSPtr - CallSiteArraySlot,
	deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
	Kind = CSS ^ css_kind,
	( Kind = normal_call(_CalleePSPtr, _) ->
		( CallSiteArraySlot = normal(CSDPtr0) ->
			CSDPtr = CSDPtr0
		;
			error("call_site_clique_to_html: normal_call error")
		),
		normal_call_site_clique_to_html(Pref, Deep, CallerCliquePtr,
			CSDPtr, LineGroups, Percent, ActionPtrs)
	;
		( CallSiteArraySlot = multi(_, CSDPtrs0) ->
			array__to_list(CSDPtrs0, CSDPtrs)
		;
			error("call_site_clique_to_html: non-normal_call error")
		),
		call_site_context(Deep, CSSPtr, FileName, LineNumber),
		multi_call_site_clique_to_html(Pref, Deep,
			FileName, LineNumber, Kind, CallerCliquePtr, CSDPtrs,
			LineGroups, Percent, ActionPtrs)
	).

:- func maybe_extract_action_clique(deep, clique_ptr, int,
	call_site_dynamic_ptr) = list(clique_ptr).

maybe_extract_action_clique(Deep, CallerCliquePtr, Percent, CSDPtr)
		= ActionPtrs :-
	( Percent > 100 ->
		ActionPtrs = []
	;
		deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
		deep_lookup_clique_index(Deep, CSD ^ csd_callee,
			CalleeCliquePtr),
		( CalleeCliquePtr = CallerCliquePtr ->
			ActionPtrs = []
		;
			deep_lookup_csd_desc(Deep, CSDPtr, CSDDesc),
			CSDOwn = CSD ^ csd_own_prof,
			CSDTotal = add_own_to_inherit(CSDOwn, CSDDesc),
			RootTotal = root_total_info(Deep),
			CSDQuanta = inherit_quanta(CSDTotal),
			RootQuanta = inherit_quanta(RootTotal),
			( CSDQuanta * 100 > RootQuanta * Percent ->
				ActionPtrs = [CalleeCliquePtr]
			;
				ActionPtrs = []
			)
		)
	).

:- pred normal_call_site_clique_to_html(preferences::in, deep::in,
	clique_ptr::in, call_site_dynamic_ptr::in,
	list(two_id_line_group)::out, int::in, list(clique_ptr)::out) is det.

normal_call_site_clique_to_html(Pref, Deep, CallerCliquePtr, CSDPtr,
		LineGroups, Percent, ActionPtrs) :-
	( valid_call_site_dynamic_ptr(Deep, CSDPtr) ->
		LineGroup = call_site_dynamic_to_html(Pref, Deep,
			downward_display, yes(CallerCliquePtr), CSDPtr),
		LineGroups = [line_to_two_id_subline_group(LineGroup)],
		ActionPtrs = maybe_extract_action_clique(Deep, CallerCliquePtr,
			Percent, CSDPtr)
	;
		LineGroups = [],
		ActionPtrs = []
	).

:- pred multi_call_site_clique_to_html(preferences::in, deep::in,
	string::in, int::in, call_site_kind_and_callee::in, clique_ptr::in,
	list(call_site_dynamic_ptr)::in, list(two_id_line_group)::out,
	int::in, list(clique_ptr)::out) is det.

multi_call_site_clique_to_html(Pref, Deep, FileName, LineNumber, Kind,
		CallerCliquePtr, CSDPtrs, LineGroups, Percent, ActionPtrs) :-
	ValidCSDPtrs = CSDPtrs,
	RawCallSiteName = call_site_kind_and_callee_to_html(Kind),
	CallSiteName = multi_call_site_add_suffix(Pref, RawCallSiteName,
		ValidCSDPtrs),
	SubLines = list__map(call_site_dynamic_to_html(Pref, Deep,
		downward_summary_display, yes(CallerCliquePtr)),
		ValidCSDPtrs),
	ActionPtrLists = list__map(
		maybe_extract_action_clique(Deep, CallerCliquePtr, Percent),
		ValidCSDPtrs),
	list__condense(ActionPtrLists, ActionPtrs),
	sum_line_group_measurements(SubLines, Own, Desc),
	SummaryHTML =
		string__format("<TD CLASS=id>%s:%d</TD>\n",
			[s(FileName), i(LineNumber)]) ++
		string__format("<TD CLASS=id>%s</TD>\n",
			[s(CallSiteName)]),
	(
		Pref ^ pref_summarize = summarize,
		LineGroup = line_group(FileName, LineNumber,
			RawCallSiteName, Own, Desc, SummaryHTML,
				sub_lines(two_id, []))
	;
		Pref ^ pref_summarize = dont_summarize,
		LineGroup = line_group(FileName, LineNumber,
			RawCallSiteName, Own, Desc, SummaryHTML,
				sub_lines(two_id, SubLines))
	),
	LineGroups = [LineGroup].

%-----------------------------------------------------------------------------%

:- func call_site_summary_to_html(preferences, deep,
	call_site_static_ptr) = two_id_line_group.

call_site_summary_to_html(Pref, Deep, CSSPtr) = LineGroup :-
	deep_lookup_call_site_calls(Deep, CSSPtr, CallSiteCallMap),
	map__to_assoc_list(CallSiteCallMap, CallSiteCallList),
	deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
	Kind = CSS ^ css_kind,
	CallerPSPtr = CSS ^ css_container,
	call_site_context(Deep, CSSPtr, FileName, LineNumber),
	( Kind = normal_call(CalleePSPtr, _) ->
		LineGroup0 = normal_call_site_summary_to_html(Pref, Deep,
			FileName, LineNumber, CallerPSPtr, CalleePSPtr,
			CallSiteCallList)
	;
		LineGroup0 = multi_call_site_summary_to_html(Pref, Deep,
			FileName, LineNumber, Kind,
			CallerPSPtr, CallSiteCallList)
	),
	CSSContext = string__format("%s:%d", [s(FileName), i(LineNumber)]),
	LineGroup = add_context(CSSContext, LineGroup0).

:- func normal_call_site_summary_to_html(preferences, deep, string, int,
	proc_static_ptr, proc_static_ptr,
	assoc_list(proc_static_ptr, list(call_site_dynamic_ptr))) =
	one_two_id_line_group.

normal_call_site_summary_to_html(Pref, Deep, FileName, LineNumber,
		CallerPSPtr, CalleePSPtr, CallSiteCallList) = LineGroup :-
	deep_lookup_proc_statics(Deep, CalleePSPtr, CalleePS),
	ProcName = CalleePS ^ ps_refined_id,
	( CallSiteCallList = [] ->
		Own = zero_own_prof_info,
		Desc = zero_inherit_prof_info,
		SummaryHTML =
			string__format("<TD CLASS=id>%s</TD>\n",
				[s(proc_static_to_html_ref(Pref,
					Deep, CalleePSPtr))]),
		LineGroup = line_group(FileName, LineNumber,
			ProcName, Own, Desc, SummaryHTML,
			sub_lines(two_id, []))
	; CallSiteCallList = [CallSiteCall] ->
		CallSiteCall = CalleePSPtrFromCall - _,
		require(unify(CalleePSPtr, CalleePSPtrFromCall),
			"call_site_summary_to_html: callee mismatch"),
		LineGroup0 = call_site_summary_group_to_html(Pref, Deep,
			FileName, LineNumber, ProcName,
			CallerPSPtr, CallSiteCall),
		LineGroup = line_to_two_id_subline_group(LineGroup0)
	;
		error("normal_call_site_summary_to_html: too many procedures")
	).

:- func multi_call_site_summary_to_html(preferences, deep, string, int,
	call_site_kind_and_callee, proc_static_ptr,
	assoc_list(proc_static_ptr, list(call_site_dynamic_ptr))) =
	one_two_id_line_group.

multi_call_site_summary_to_html(Pref, Deep, FileName, LineNumber, Kind,
		CallerPSPtr, CallSiteCallList) = LineGroup :-
	RawCallSiteName = call_site_kind_and_callee_to_html(Kind),
	CallSiteName = multi_call_site_add_suffix(Pref, RawCallSiteName,
		CallSiteCallList),
	SubLines = list__map(call_site_summary_group_to_html(Pref, Deep,
		FileName, LineNumber, RawCallSiteName, CallerPSPtr),
		CallSiteCallList),
	sum_line_group_measurements(SubLines, Own, Desc),
	SummaryHTML =
		string__format("<TD CLASS=id>%s</TD>\n",
			[s(CallSiteName)]),
	(
		Pref ^ pref_summarize = summarize,
		LineGroup = line_group(FileName, LineNumber,
			RawCallSiteName, Own, Desc, SummaryHTML,
			sub_lines(two_id, []))
	;
		Pref ^ pref_summarize = dont_summarize,
		ContextSubLines = list__map(add_context(""), SubLines),
		LineGroup = line_group(FileName, LineNumber,
			RawCallSiteName, Own, Desc, SummaryHTML,
			sub_lines(two_id, ContextSubLines))
	).

:- func call_site_summary_group_to_html(preferences, deep,
	string, int, string, proc_static_ptr,
	pair(proc_static_ptr, list(call_site_dynamic_ptr))) = one_id_line.

call_site_summary_group_to_html(Pref, Deep, FileName, LineNumber, ProcName,
		CallerPSPtr, PSPtr - CSDPtrs) = LineGroup :-
	list__foldl2(accumulate_csd_prof_info(Deep, CallerPSPtr), CSDPtrs,
		zero_own_prof_info, Own, zero_inherit_prof_info, Desc),
	HTML =
		string__format("<TD CLASS=id>%s</TD>\n",
			[s(proc_static_to_html_ref(Pref, Deep, PSPtr))]),
	LineGroup = line_group(FileName, LineNumber, ProcName,
		Own, Desc, HTML, unit).

%-----------------------------------------------------------------------------%

:- func multi_call_site_add_suffix(preferences, string, list(T)) = string.

multi_call_site_add_suffix(Pref, RawCallSiteName, CallList) = CallSiteName :-
	( CallList = [] ->
		CallSiteName = RawCallSiteName ++ " (no&nbps;calls&nbps;made)"
	; Pref ^ pref_summarize = summarize ->
		CallSiteName = RawCallSiteName ++ " (summary)"
	;
		CallSiteName = RawCallSiteName
	).

%-----------------------------------------------------------------------------%

:- pred process_call_site_dynamics_group(list(call_site_dynamic_ptr)::in,
	deep::in, proc_static_ptr::in,
	maybe(clique_ptr)::in, maybe(clique_ptr)::out,
	own_prof_info::in, own_prof_info::out,
	inherit_prof_info::in, inherit_prof_info::out) is det.

process_call_site_dynamics_group([], _, _,
		MaybeCalleeCliquePtr, MaybeCalleeCliquePtr,
		Own, Own, Desc, Desc).
process_call_site_dynamics_group([CSDPtr | CSDPtrs], Deep, CalleePSPtr,
		MaybeCalleeCliquePtr0, MaybeCalleeCliquePtr,
		Own0, Own, Desc0, Desc) :-
	deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
	PDPtr = CSD ^ csd_callee,
	deep_lookup_proc_dynamics(Deep, PDPtr, PD),
	PSPtr = PD ^ pd_proc_static,
	require(unify(CalleePSPtr, PSPtr),
		"process_call_site_dynamics_group: callee mismatch"),
	deep_lookup_clique_index(Deep, PDPtr, CalleeCliquePtr),
	(
		MaybeCalleeCliquePtr0 = no,
		MaybeCalleeCliquePtr1 = yes(CalleeCliquePtr)
	;
		MaybeCalleeCliquePtr0 = yes(PrevCalleeCliquePtr),
		MaybeCalleeCliquePtr1 = MaybeCalleeCliquePtr0,
		require(unify(PrevCalleeCliquePtr, CalleeCliquePtr),
			"process_call_site_dynamics_group: clique mismatch")
	),
	deep_lookup_csd_own(Deep, CSDPtr, CSDOwn),
	deep_lookup_csd_desc(Deep, CSDPtr, CSDDesc),
	Own1 = add_own_to_own(Own0, CSDOwn),
	Desc1 = add_inherit_to_inherit(Desc0, CSDDesc),
	process_call_site_dynamics_group(CSDPtrs, Deep, CalleePSPtr,
		MaybeCalleeCliquePtr1, MaybeCalleeCliquePtr,
		Own1, Own, Desc1, Desc).

:- pred accumulate_csd_prof_info(deep::in, proc_static_ptr::in,
	call_site_dynamic_ptr::in,
	own_prof_info::in, own_prof_info::out,
	inherit_prof_info::in, inherit_prof_info::out) is det.

accumulate_csd_prof_info(Deep, CallerPSPtr, CSDPtr, Own0, Own, Desc0, Desc) :-
	deep_lookup_csd_own(Deep, CSDPtr, CSDOwn),
	deep_lookup_csd_desc(Deep, CSDPtr, CSDDesc),
	add_own_to_own(Own0, CSDOwn) = Own,
	add_inherit_to_inherit(Desc0, CSDDesc) = Desc1,
	deep_lookup_csd_comp_table(Deep, CSDPtr, CompTableArray),
	( map__search(CompTableArray, CallerPSPtr, InnerTotal) ->
		Desc = subtract_inherit_from_inherit(InnerTotal, Desc1)
	;
		Desc = Desc1
	).

:- func call_site_dynamic_to_html_with_caller(preferences, deep,
	call_site_display, call_site_dynamic_ptr) = two_id_line.

call_site_dynamic_to_html_with_caller(Pref, Deep, Display, CSDPtr)
		= LineGroup :-
	deep_extract_csdptr_caller(Deep, CSDPtr, CallerPDPtr),
	deep_lookup_clique_index(Deep, CallerPDPtr, CallerClique),
	LineGroup = call_site_dynamic_to_html(Pref, Deep, Display,
		yes(CallerClique), CSDPtr).

:- func call_site_dynamic_to_html(preferences, deep, call_site_display,
	maybe(clique_ptr), call_site_dynamic_ptr) = two_id_line.

call_site_dynamic_to_html(Pref, Deep, CallSiteDisplay, MaybeCallerCliquePtr,
		CSDPtr) = LineGroup :-
	require(valid_call_site_dynamic_ptr(Deep, CSDPtr),
		"call_site_dynamic_to_html: invalid call_site_dynamic_ptr"),
	deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
	CallerPDPtr = CSD ^ csd_caller,
	CalleePDPtr = CSD ^ csd_callee,
	CallSiteOwn = CSD ^ csd_own_prof,
	deep_lookup_csd_desc(Deep, CSDPtr, CallSiteDesc),
	deep_lookup_clique_index(Deep, CalleePDPtr, CalleeCliquePtr),
	call_site_dynamic_context(Deep, CSDPtr, FileName, LineNumber),
	Context = string__format("%s:%d", [s(FileName), i(LineNumber)]),
	HTML = call_to_html(Pref, Deep, CallSiteDisplay, Context,
		CallerPDPtr, CalleePDPtr,
		MaybeCallerCliquePtr, CalleeCliquePtr),
	ProcName = proc_dynamic_name(Deep, CalleePDPtr),
	LineGroup = line_group(FileName, LineNumber, ProcName,
		CallSiteOwn, CallSiteDesc, HTML, unit).

%-----------------------------------------------------------------------------%

:- type call_site_display
	--->	call_site_display(
			display_context		:: call_site_context,
			display_proc_name	:: call_site_proc_name,
			display_url		:: url_with_proc_name,
			display_wrap		:: wrap_with_url
		).

:- type call_site_context
	--->	call_context
	;	empty_context.

:- type call_site_proc_name
	--->	caller_proc_name
	;	callee_proc_name.

:- type url_with_proc_name
	--->	caller_clique
	;	callee_clique.

:- type wrap_with_url
	--->	always
	;	if_cross_clique(assume_cross_clique)
	;	never.

:- type assume_cross_clique
	--->	assume_cross_clique
	;	assume_within_clique.

:- func ancestor_display = call_site_display.
:- func upward_display = call_site_display.
:- func downward_display = call_site_display.
:- func downward_summary_display = call_site_display.

ancestor_display =
	call_site_display(call_context, caller_proc_name,
		caller_clique, always).

upward_display =
	call_site_display(call_context, caller_proc_name,
		callee_clique, always).

downward_display =
	call_site_display(call_context, callee_proc_name,
		callee_clique, if_cross_clique(assume_within_clique)).

downward_summary_display =
	call_site_display(empty_context, callee_proc_name,
		callee_clique, if_cross_clique(assume_within_clique)).

%-----------------------------------------------------------------------------%

:- func call_to_html(preferences, deep, call_site_display, string,
	proc_dynamic_ptr, proc_dynamic_ptr, maybe(clique_ptr), clique_ptr)
	= string.

call_to_html(Pref, Deep, CallSiteDisplay, CallContext,
		CallerPDPtr, CalleePDPtr,
		MaybeCallerCliquePtr, CalleeCliquePtr) = HTML :-
	( MaybeCallerCliquePtr = yes(CallerCliquePtr0) ->
		CallerCliquePtr = CallerCliquePtr0
	;
		CallerCliquePtr = dummy_clique_ptr
	),
	(
		CallSiteDisplay ^ display_context = call_context,
		Context = CallContext
	;
		CallSiteDisplay ^ display_context = empty_context,
		Context = ""
	),
	(
		CallSiteDisplay ^ display_proc_name = caller_proc_name,
		ProcName = proc_dynamic_name(Deep, CallerPDPtr)
	;
		CallSiteDisplay ^ display_proc_name = callee_proc_name,
		ProcName = proc_dynamic_name(Deep, CalleePDPtr)
	),
	(
		CallSiteDisplay ^ display_url = caller_clique,
		ChosenCliquePtr = CallerCliquePtr
	;
		CallSiteDisplay ^ display_url = callee_clique,
		ChosenCliquePtr = CalleeCliquePtr
	),
	ChosenCliquePtr = clique_ptr(ChosenCliqueNum),
	WrappedProcName = string__format("<A HREF=""%s"">%s</A>",
		[s(deep_cmd_pref_to_url(Pref, Deep, clique(ChosenCliqueNum))),
		s(ProcName)]),
	(
		CallSiteDisplay ^ display_wrap = always,
		UsedProcName0 = WrappedProcName
	;
		CallSiteDisplay ^ display_wrap = if_cross_clique(Assume),
		(
			MaybeCallerCliquePtr = yes(_),
			( CallerCliquePtr \= CalleeCliquePtr ->
				UsedProcName0 = WrappedProcName
			;
				UsedProcName0 = ProcName
			)
		;
			MaybeCallerCliquePtr = no,
			(
				Assume = assume_cross_clique,
				UsedProcName0 = WrappedProcName
			;
				Assume = assume_within_clique,
				UsedProcName0 = ProcName
			)
		)
	;
		CallSiteDisplay ^ display_wrap = never,
		UsedProcName0 = ProcName
	),
	(
		UsedProcName0 = WrappedProcName,
		valid_clique_ptr(Deep, ChosenCliquePtr)
	->
		UsedProcName = UsedProcName0
	;
		UsedProcName = ProcName
	),
	HTML =
		string__format("<TD CLASS=id>%s</TD>\n", [s(Context)]) ++
		string__format("<TD CLASS=id>%s</TD>\n", [s(UsedProcName)]).

%-----------------------------------------------------------------------------%

:- pred call_site_dynamic_context(deep::in, call_site_dynamic_ptr::in,
	string::out, int::out) is det.

call_site_dynamic_context(Deep, CSDPtr, FileName, LineNumber) :-
	deep_lookup_call_site_static_map(Deep, CSDPtr, CSSPtr),
	deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
	PSPtr = CSS ^ css_container,
	LineNumber = CSS ^ css_line_num,
	deep_lookup_proc_statics(Deep, PSPtr, PS),
	FileName = PS ^ ps_file_name.

%-----------------------------------------------------------------------------%

:- pred proc_callers_to_html(preferences::in, deep::in, proc_static_ptr::in,
	caller_groups::in, int::in,
	maybe_error({id_fields, string, string, string})::out,
	io__state::di, io__state::uo) is det.

proc_callers_to_html(Pref, Deep, PSPtr, CallerGroups, BunchNum0, MaybePage,
		IO0, IO) :-
	deep_lookup_proc_callers(Deep, PSPtr, CallerCSDPtrs),
	(
		Pref ^ pref_contour = no_contour,
		CallerCSDPtrPairs = list__map(pair_self, CallerCSDPtrs),
		IO = IO0,
		MaybeErrorMsg = no
	;
		Pref ^ pref_contour = apply_contour,
		read_exclude_file(contour_file_name(Deep ^ data_file_name),
			Deep, Result, IO0, IO),
		(
			Result = ok(ExcludeSpec),
			CallerCSDPtrPairs = list__map(
				pair_contour(Deep, ExcludeSpec),
				CallerCSDPtrs),
			MaybeErrorMsg = no
		;
			Result = error(ErrorMsg),
			MaybeErrorMsg = yes(ErrorMsg ++ "\n<br>"),
			CallerCSDPtrPairs = list__map(pair_self, CallerCSDPtrs)
		)
	),
	ProcName = proc_static_name(Deep, PSPtr),
	PSPtr = proc_static_ptr(PSI),
	CmdSite    = proc_callers(PSI, group_by_call_site, 1),
	CmdProc    = proc_callers(PSI, group_by_proc, 1),
	CmdModule  = proc_callers(PSI, group_by_module, 1),
	CmdClique  = proc_callers(PSI, group_by_clique, 1),
	LinkSite   = "Group callers by call site",
	LinkProc   = "Group callers by procedure",
	LinkModule = "Group callers by module",
	LinkClique = "Group callers by clique",
	BunchSize = 100,	% don't display more lines than this,
				% to avoid quadratic behaviour in Netscape.
	(
		CallerGroups = group_by_call_site,
		GroupMap = list__foldl(accumulate_csds_by_call_site(Deep),
			CallerCSDPtrPairs, map__init),
		map__to_assoc_list(GroupMap, GroupList),
		Lines = list__map(
			proc_callers_call_site_to_html(Pref, Deep, PSPtr),
			GroupList),
		SortedLines = sort_line_groups(Pref ^ pref_criteria, Lines),
		IdFields = source_proc,
		Entity = "call site",
		GroupToggles =
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdProc)),
				s(LinkProc)]) ++
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdModule)),
				s(LinkModule)]) ++
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdClique)),
				s(LinkClique)])
	;
		CallerGroups = group_by_proc,
		GroupMap = list__foldl(accumulate_csds_by_procedure(Deep),
			CallerCSDPtrPairs, map__init),
		map__to_assoc_list(GroupMap, GroupList),
		Lines = list__map(
			proc_callers_proc_to_html(Pref, Deep, PSPtr),
			GroupList),
		SortedLines = sort_line_groups(Pref ^ pref_criteria, Lines),
		IdFields = source_proc,
		Entity = "procedure",
		GroupToggles =
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdSite)),
				s(LinkSite)]) ++
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdModule)),
				s(LinkModule)]) ++
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdClique)),
				s(LinkClique)])
	;
		CallerGroups = group_by_module,
		GroupMap = list__foldl(accumulate_csds_by_module(Deep),
			CallerCSDPtrPairs, map__init),
		map__to_assoc_list(GroupMap, GroupList),
		RawLines = list__map(
			proc_callers_module_to_html(Pref, Deep, PSPtr),
			GroupList),
		SortedRawLines = sort_line_groups(Pref ^ pref_criteria,
			RawLines),
		SortedLines = add_ranks(SortedRawLines),
		IdFields = rank_module,
		Entity = "module",
		GroupToggles =
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdSite)),
				s(LinkSite)]) ++
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdProc)),
				s(LinkProc)]) ++
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdClique)),
				s(LinkClique)])
	;
		CallerGroups = group_by_clique,
		GroupMap = list__foldl(accumulate_csds_by_clique(Deep),
			CallerCSDPtrPairs, map__init),
		map__to_assoc_list(GroupMap, GroupList),
		RawLines = list__map(
			proc_callers_clique_to_html(Pref, Deep, PSPtr),
			GroupList),
		SortedRawLines = sort_line_groups(Pref ^ pref_criteria,
			RawLines),
		SortedLines = add_ranks(SortedRawLines),
		IdFields = source_proc,
		Entity = "clique",
		GroupToggles =
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdSite)),
				s(LinkSite)]) ++
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdProc)),
				s(LinkProc)]) ++
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, CmdModule)),
				s(LinkModule)])
	),
	% SortedLines may contain many thousand elements, and Netscape
	% chokes on the output unless we filter them or break them into chunks.
	% This simple limit device is temporary until we decide what filtering
	% and/or chunking mechanism we want to use.
	list__length(SortedLines, NumLines),
	select_line_bunch(NumLines, BunchNum0, BunchNum, BunchSize,
		SortedLines, DisplayedLines),
	Banner = proc_callers_banner(PSI, ProcName, Pref, Deep,
		NumLines, BunchSize, BunchNum, Entity),
	DisplayedHTMLs = list__map(
		two_id_line_to_html(Pref, Deep, totals_meaningful),
		DisplayedLines),
	HTML = string__append_list(DisplayedHTMLs),
	( BunchNum > 1 ->
		FirstCmd = proc_callers(PSI, CallerGroups, 1),
		FirstLink = "First group",
		FirstToggle =
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, FirstCmd)),
				s(FirstLink)])
	;
		FirstToggle = ""
	),
	( BunchNum > 2 ->
		PrevCmd = proc_callers(PSI, CallerGroups, BunchNum - 1),
		PrevLink = "Previous group",
		PrevToggle =
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, PrevCmd)),
				s(PrevLink)])
	;
		PrevToggle = ""
	),
	( NumLines > BunchNum * BunchSize ->
		NextCmd = proc_callers(PSI, CallerGroups, BunchNum + 1),
		NextLink = "Next group",
		NextToggle =
			string__format("<A HREF=""%s"">%s</A>\n",
				[s(deep_cmd_pref_to_url(Pref, Deep, NextCmd)),
				s(NextLink)])
	;
		NextToggle = ""
	),
	Toggles = GroupToggles ++ FirstToggle ++ PrevToggle ++ NextToggle,
	(
		MaybeErrorMsg = no,
		MaybePage = ok({IdFields, Banner, HTML, Toggles})
	;
		MaybeErrorMsg = yes(Msg),
		MaybePage = error(Msg)
	).

:- pred select_line_bunch(int::in, int::in, int::out, int::in, list(T)::in,
	list(T)::out) is det.

select_line_bunch(NumLines, BunchNum0, BunchNum, BunchSize,
		Lines, DisplayedLines) :-
	ToDelete = (BunchNum0 - 1) * BunchSize,
	(
		list__drop(ToDelete, Lines, RemainingLines0),
		RemainingLines0 = [_ | _]
	->
		BunchNum = BunchNum0,
		RemainingLines = RemainingLines0,
		RemainingNumLines = NumLines - ToDelete
	;
		BunchNum = 1,
		RemainingLines = Lines,
		RemainingNumLines = NumLines
	),
	( RemainingNumLines > BunchSize ->
		list__take_upto(BunchSize, RemainingLines, DisplayedLines)
	;
		DisplayedLines = RemainingLines
	).

:- func proc_callers_banner(int, string, preferences, deep, int, int, int,
	string) = string.

proc_callers_banner(PSI, ProcName, Pref, Deep, NumLines, BunchSize, BunchNum,
		Parent) = HTML :-
	Cmd = proc(PSI),
	WrappedProcName = string__format("<A HREF=""%s"">%s</A>",
		[s(deep_cmd_pref_to_url(Pref, Deep, Cmd)), s(ProcName)]),
	( NumLines = 0 ->
		HTML = string__format("<H3>There are no %ss calling %s</H3>",
			[s(Parent), s(WrappedProcName)])
	; NumLines = 1 ->
		HTML = string__format("<H3>There is one %s calling %s:</H3>\n",
			[s(Parent), s(WrappedProcName)])
	; NumLines =< BunchSize ->
		HTML = string__format("<H3>The %d %ss calling %s:</H3>",
			[i(NumLines), s(Parent), s(WrappedProcName)])
	; BunchNum = 1 ->
		HTML = string__format("<H3>There are %d %ss calling %s, showing first %d:</H3>",
			[i(NumLines), s(Parent), s(WrappedProcName),
			i(BunchSize)])
	;
		First = (BunchNum - 1) * BunchSize + 1,
		Last0 = (BunchNum) * BunchSize,
		( Last0 > NumLines ->
			Last = NumLines
		;
			Last = Last0
		),
		HTML = string__format("<H3>There are %d %ss calling %s, showing %d to %d:</H3>",
			[i(NumLines), s(Parent), s(WrappedProcName),
			i(First), i(Last)])
	).

:- func proc_callers_call_site_to_html(preferences, deep, proc_static_ptr,
	pair(call_site_static_ptr, list(call_site_dynamic_ptr))) = two_id_line.

proc_callers_call_site_to_html(Pref, Deep, CalleePSPtr, CSSPtr - CSDPtrs)
		= LineGroup :-
	call_site_context(Deep, CSSPtr, FileName, LineNumber),
	deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
	CallerPSPtr = CSS ^ css_container,
	deep_lookup_proc_statics(Deep, CallerPSPtr, CallerPS),
	CallerProcName = CallerPS ^ ps_refined_id,
	list__foldl2(accumulate_parent_csd_prof_info(Deep, CalleePSPtr),
		CSDPtrs,
		zero_own_prof_info, Own, zero_inherit_prof_info, Desc),
	HTML =
		string__format("<TD CLASS=id>%s:%d</TD>\n",
			[s(FileName), i(LineNumber)]) ++
		string__format("<TD CLASS=id>%s</TD>\n",
			[s(proc_static_to_html_ref(Pref, Deep,
				CallerPSPtr))]),
	LineGroup = line_group(FileName, LineNumber, CallerProcName,
		Own, Desc, HTML, unit).

:- func proc_callers_proc_to_html(preferences, deep, proc_static_ptr,
	pair(proc_static_ptr, list(call_site_dynamic_ptr))) = two_id_line.

proc_callers_proc_to_html(Pref, Deep, CalleePSPtr, CallerPSPtr - CSDPtrs)
		= LineGroup :-
	proc_static_context(Deep, CallerPSPtr, FileName, LineNumber),
	deep_lookup_proc_statics(Deep, CallerPSPtr, CallerPS),
	CallerProcName = CallerPS ^ ps_refined_id,
	list__foldl2(accumulate_parent_csd_prof_info(Deep, CalleePSPtr),
		CSDPtrs,
		zero_own_prof_info, Own, zero_inherit_prof_info, Desc),
	HTML =
		string__format("<TD CLASS=id>%s:%d</TD>\n",
			[s(FileName), i(LineNumber)]) ++
		string__format("<TD CLASS=id>%s</TD>\n",
			[s(proc_static_to_html_ref(Pref, Deep,
				CallerPSPtr))]),
	LineGroup = line_group(FileName, LineNumber, CallerProcName,
		Own, Desc, HTML, unit).

:- func proc_callers_module_to_html(preferences, deep, proc_static_ptr,
	pair(string, list(call_site_dynamic_ptr))) = one_id_line.

proc_callers_module_to_html(Pref, Deep, CalleePSPtr, ModuleName - CSDPtrs)
		= LineGroup :-
	list__foldl2(accumulate_parent_csd_prof_info(Deep, CalleePSPtr),
		CSDPtrs,
		zero_own_prof_info, Own, zero_inherit_prof_info, Desc),
	HTML = string__format("<TD CLASS=id>%s</TD>\n",
		[s(module_name_to_html_ref(Pref, Deep, ModuleName))]),
	% We don't have filename information for modules, and line numbers
	% are not meaningful for modules.
	LineGroup = line_group(ModuleName, 0, ModuleName,
		Own, Desc, HTML, unit).

:- func proc_callers_clique_to_html(preferences, deep, proc_static_ptr,
	pair(clique_ptr, list(call_site_dynamic_ptr))) = one_id_line.

proc_callers_clique_to_html(Pref, Deep, CalleePSPtr, CliquePtr - CSDPtrs)
		= LineGroup :-
	list__foldl2(accumulate_parent_csd_prof_info(Deep, CalleePSPtr),
		CSDPtrs,
		zero_own_prof_info, Own, zero_inherit_prof_info, Desc),
	deep_lookup_clique_parents(Deep, CliquePtr, EntryCSDPtr),
	deep_lookup_call_site_dynamics(Deep, EntryCSDPtr, EntryCSD),
	EntryPDPtr = EntryCSD ^ csd_callee,
	proc_dynamic_context(Deep, EntryPDPtr, FileName, LineNumber),
	ProcName = proc_dynamic_name(Deep, EntryPDPtr),
	HTML = string__format("<TD CLASS=id>%s</TD>\n",
		[s(clique_ptr_to_html_ref(Pref, Deep, ProcName, CliquePtr))]),
	LineGroup = line_group(FileName, LineNumber, ProcName,
		Own, Desc, HTML, unit).

:- func accumulate_csds_by_call_site(deep, pair(call_site_dynamic_ptr),
	map(call_site_static_ptr, list(call_site_dynamic_ptr))) =
	map(call_site_static_ptr, list(call_site_dynamic_ptr)).

accumulate_csds_by_call_site(Deep, GroupCSDPtr - CostCSDPtr, Map0) = Map :-
	deep_lookup_call_site_static_map(Deep, GroupCSDPtr, GroupCSSPtr),
	( map__search(Map0, GroupCSSPtr, CostCSDPtrs0) ->
		map__det_update(Map0, GroupCSSPtr, [CostCSDPtr | CostCSDPtrs0],
			Map)
	;
		map__det_insert(Map0, GroupCSSPtr, [CostCSDPtr], Map)
	).

:- func accumulate_csds_by_procedure(deep, pair(call_site_dynamic_ptr),
	map(proc_static_ptr, list(call_site_dynamic_ptr))) =
	map(proc_static_ptr, list(call_site_dynamic_ptr)).

accumulate_csds_by_procedure(Deep, GroupCSDPtr - CostCSDPtr, Map0) = Map :-
	deep_lookup_call_site_static_map(Deep, GroupCSDPtr, GroupCSSPtr),
	deep_lookup_call_site_statics(Deep, GroupCSSPtr, GroupCSS),
	GroupPSPtr = GroupCSS ^ css_container,
	( map__search(Map0, GroupPSPtr, CostCSDPtrs0) ->
		map__det_update(Map0, GroupPSPtr, [CostCSDPtr | CostCSDPtrs0],
			Map)
	;
		map__det_insert(Map0, GroupPSPtr, [CostCSDPtr], Map)
	).

:- func accumulate_csds_by_module(deep, pair(call_site_dynamic_ptr),
	map(string, list(call_site_dynamic_ptr))) =
	map(string, list(call_site_dynamic_ptr)).

accumulate_csds_by_module(Deep, GroupCSDPtr - CostCSDPtr, Map0) = Map :-
	deep_lookup_call_site_static_map(Deep, GroupCSDPtr, GroupCSSPtr),
	deep_lookup_call_site_statics(Deep, GroupCSSPtr, GroupCSS),
	GroupPSPtr = GroupCSS ^ css_container,
	deep_lookup_proc_statics(Deep, GroupPSPtr, GroupPS),
	GroupModuleName = GroupPS ^ ps_decl_module,
	( map__search(Map0, GroupModuleName, CostCSDPtrs0) ->
		map__det_update(Map0, GroupModuleName,
			[CostCSDPtr | CostCSDPtrs0], Map)
	;
		map__det_insert(Map0, GroupModuleName, [CostCSDPtr], Map)
	).

:- func accumulate_csds_by_clique(deep, pair(call_site_dynamic_ptr),
	map(clique_ptr, list(call_site_dynamic_ptr))) =
	map(clique_ptr, list(call_site_dynamic_ptr)).

accumulate_csds_by_clique(Deep, GroupCSDPtr - CostCSDPtr, Map0) = Map :-
	deep_lookup_call_site_dynamics(Deep, GroupCSDPtr, GroupCSD),
	CallerPDPtr = GroupCSD ^ csd_caller,
	deep_lookup_clique_index(Deep, CallerPDPtr, CliquePtr),
	( map__search(Map0, CliquePtr, CostCSDPtrs0) ->
		map__det_update(Map0, CliquePtr, [CostCSDPtr | CostCSDPtrs0],
			Map)
	;
		map__det_insert(Map0, CliquePtr, [CostCSDPtr], Map)
	).

:- pred accumulate_parent_csd_prof_info(deep::in, proc_static_ptr::in,
	call_site_dynamic_ptr::in,
	own_prof_info::in, own_prof_info::out,
	inherit_prof_info::in, inherit_prof_info::out) is det.

accumulate_parent_csd_prof_info(Deep, CallerPSPtr, CSDPtr,
		Own0, Own, Desc0, Desc) :-
	deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
	( CSD ^ csd_callee = CSD ^ csd_caller ->
			% We want to sum only cross-clique callers.
		Own = Own0,
		Desc = Desc0
	;
		deep_lookup_csd_own(Deep, CSDPtr, CSDOwn),
		deep_lookup_csd_desc(Deep, CSDPtr, CSDDesc),
		add_own_to_own(Own0, CSDOwn) = Own,
		add_inherit_to_inherit(Desc0, CSDDesc) = Desc1,

		deep_lookup_clique_index(Deep, CSD ^ csd_callee,
			CalleeCliquePtr),
		deep_lookup_clique_members(Deep, CalleeCliquePtr,
			CalleeCliquePDPtrs),
		list__foldl(compensate_using_comp_table(Deep, CallerPSPtr),
			CalleeCliquePDPtrs, Desc1, Desc)
	).

:- pred compensate_using_comp_table(deep::in, proc_static_ptr::in,
	proc_dynamic_ptr::in, inherit_prof_info::in, inherit_prof_info::out)
	is det.

compensate_using_comp_table(Deep, CallerPSPtr, PDPtr, Desc0, Desc) :-
	deep_lookup_pd_comp_table(Deep, PDPtr, CompTableArray),
	( map__search(CompTableArray, CallerPSPtr, InnerTotal) ->
		Desc = subtract_inherit_from_inherit(InnerTotal, Desc0)
	;
		Desc = Desc0
	).

:- func pair_self(call_site_dynamic_ptr) = pair(call_site_dynamic_ptr).

pair_self(CSDPtr) = CSDPtr - CSDPtr.

:- func pair_contour(deep, exclude_file, call_site_dynamic_ptr)
	= pair(call_site_dynamic_ptr).

pair_contour(Deep, ExcludeSpec, CSDPtr) =
	apply_contour_exclusion(Deep, ExcludeSpec, CSDPtr) - CSDPtr.

%-----------------------------------------------------------------------------%

:- func proc_summary_to_html(preferences, deep, proc_static_ptr) = string.

proc_summary_to_html(Pref, Deep, PSPtr) = HTML :-
	SumHTML = two_id_line_to_html(Pref, Deep, totals_meaningful,
		lookup_proc_total_to_two_id_line(Pref, Deep, yes, "", PSPtr)),
	deep_lookup_proc_statics(Deep, PSPtr, PS),
	CSSPtrsArray = PS ^ ps_sites,
	array__to_list(CSSPtrsArray, CSSPtrs),
	CallSiteGroups = list__map(call_site_summary_to_html(Pref, Deep),
		CSSPtrs),
	SortedCallSiteGroups = sort_line_groups(Pref ^ pref_criteria,
		CallSiteGroups),
	BodyHTMLs = list__map(
		two_id_line_group_to_html(Pref, Deep, totals_meaningful),
		SortedCallSiteGroups),
	string__append_list(BodyHTMLs, BodyHTML0),
	( SortedCallSiteGroups = [] ->
		BodyHTML = BodyHTML0
	;
		BodyHTML =
			BodyHTML0 ++
			separator_row(Pref, source_proc, totals_meaningful)
	),
	HTML =
		SumHTML ++
		separator_row(Pref, source_proc, totals_meaningful) ++
		BodyHTML.

:- func proc_summary_toggles_to_html(preferences, deep, proc_static_ptr)
	= string.

proc_summary_toggles_to_html(Pref, Deep, PSPtr) = HTML :-
	PSPtr = proc_static_ptr(PSI),
	Msg1 = "Parent call sites",
	Cmd1 = proc_callers(PSI, group_by_call_site, 1),
	Msg2 = "Parent procedures",
	Cmd2 = proc_callers(PSI, group_by_proc, 1),
	Msg3 = "Parent modules",
	Cmd3 = proc_callers(PSI, group_by_module, 1),
	Msg4 = "Parent cliques",
	Cmd4 = proc_callers(PSI, group_by_clique, 1),
	Link1 = string__format("<A HREF=""%s"">%s</A>\n",
		[s(deep_cmd_pref_to_url(Pref, Deep, Cmd1)), s(Msg1)]),
	Link2 = string__format("<A HREF=""%s"">%s</A>\n",
		[s(deep_cmd_pref_to_url(Pref, Deep, Cmd2)), s(Msg2)]),
	Link3 = string__format("<A HREF=""%s"">%s</A>\n",
		[s(deep_cmd_pref_to_url(Pref, Deep, Cmd3)), s(Msg3)]),
	Link4 = string__format("<A HREF=""%s"">%s</A>\n",
		[s(deep_cmd_pref_to_url(Pref, Deep, Cmd4)), s(Msg4)]),
	HTML =
		Link1 ++
		Link2 ++
		Link3 ++
		Link4.

%-----------------------------------------------------------------------------%

:- func wrap_clique_links(clique_ptr, preferences, deep, string,
	order_criteria) = string.

wrap_clique_links(CliquePtr, Pref0, Deep, Str0, Criteria) = Str :-
	CliquePtr = clique_ptr(CI),
	Cmd = clique(CI),
	Pref = Pref0 ^ pref_criteria := Criteria,
	URL = deep_cmd_pref_to_url(Pref, Deep, Cmd),
	Str = string__format("<A HREF=%s>%s</A>",
		[s(URL), s(Str0)]).

:- func wrap_proc_links(proc_static_ptr, preferences, deep, string,
	order_criteria) = string.

wrap_proc_links(PSPtr, Pref0, Deep, Str0, Criteria) = Str :-
	PSPtr = proc_static_ptr(PSI),
	Cmd = proc(PSI),
	Pref = Pref0 ^ pref_criteria := Criteria,
	URL = deep_cmd_pref_to_url(Pref, Deep, Cmd),
	Str = string__format("<A HREF=%s>%s</A>",
		[s(URL), s(Str0)]).

:- func wrap_proc_callers_links(proc_static_ptr, caller_groups, int,
	preferences, deep, string, order_criteria) = string.

wrap_proc_callers_links(PSPtr, CallerGroups, BunchNum, Pref0, Deep,
		Str0, Criteria) = Str :-
	PSPtr = proc_static_ptr(PSI),
	Cmd = proc_callers(PSI, CallerGroups, BunchNum),
	Pref = Pref0 ^ pref_criteria := Criteria,
	URL = deep_cmd_pref_to_url(Pref, Deep, Cmd),
	Str = string__format("<A HREF=%s>%s</A>",
		[s(URL), s(Str0)]).

:- func wrap_module_links(string, preferences, deep, string,
	order_criteria) = string.

wrap_module_links(ModuleName, Pref0, Deep, Str0, Criteria) = Str :-
	Cmd = module(ModuleName),
	Pref = Pref0 ^ pref_criteria := Criteria,
	URL = deep_cmd_pref_to_url(Pref, Deep, Cmd),
	Str = string__format("<A HREF=%s>%s</A>",
		[s(URL), s(Str0)]).

:- func wrap_modules_links(preferences, deep, string, order_criteria) = string.

wrap_modules_links(Pref0, Deep, Str0, Criteria) = Str :-
	Cmd = modules,
	Pref = Pref0 ^ pref_criteria := Criteria,
	URL = deep_cmd_pref_to_url(Pref, Deep, Cmd),
	Str = string__format("<A HREF=%s>%s</A>",
		[s(URL), s(Str0)]).

:- func wrap_top_procs_links(display_limit, preferences, deep, string,
	order_criteria) = string.

wrap_top_procs_links(Limit, Pref, Deep, Str0, Criteria) = Str :-
	(
		Criteria = by_context,
		Str = Str0
	;
		Criteria = by_name,
		Str = Str0
	;
		Criteria = by_cost(CostKind, InclDesc, Scope),
		Cmd = top_procs(Limit, CostKind, InclDesc, Scope),
		URL = deep_cmd_pref_to_url(Pref, Deep, Cmd),
		Str = string__format("<A HREF=%s>%s</A>",
			[s(URL), s(Str0)])
	).
