%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: output.m
% Main author: petdr.
%
% Prints out the output of the profiler.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module output.
:- interface.

:- import_module output_prof_info.

:- import_module io.
:- import_module map.

%---------------------------------------------------------------------------%

:- pred output_profile(io.text_output_stream::in, profiler_output::in,
    map(string, int)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module globals.
:- import_module options.
:- import_module generate_output.

:- import_module assoc_list.
:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

output_profile(OutputStream, ProfilerOutput, IndexMap, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.get_what_to_profile(Globals, WhatToProfile),
    what_to_profile(WhatToProfileString, WhatToProfile),
    io.format(OutputStream, "*** profiling %s ***\n\n",
        [s(WhatToProfileString)], !IO),

    ProfilerOutput = profiler_output(InfoMap, CallList, FlatList),
    globals.io_lookup_bool_option(call_graph, CallGraphOpt, !IO),
    (
        CallGraphOpt = yes,
        output_call_graph_headers(OutputStream, !IO),
        output_call_graph(OutputStream, CallList, InfoMap, IndexMap, !IO)
    ;
        CallGraphOpt = no
    ),

    output_flat_headers(OutputStream, !IO),
    output_flat_profile(OutputStream, FlatList, 0.0, InfoMap, IndexMap, !IO),

    output_alphabet_headers(OutputStream, !IO),
    output_alphabet_listing(OutputStream, IndexMap, !IO).

:- type header_category
    --->    time_headers
    ;       memory_words_headers
    ;       memory_cells_headers.

:- func classify_profile(what_to_profile) = header_category.

classify_profile(user_time) = time_headers.
classify_profile(user_plus_system_time) = time_headers.
classify_profile(real_time) = time_headers.
classify_profile(memory_words) = memory_words_headers.
classify_profile(memory_cells) = memory_cells_headers.
classify_profile(memory_snapshots) = memory_cells_headers.  % dummy

:- pred units(header_category::in, string::out, string::out, string::out,
    string::out, string::out, string::out, string::out, string::out) is det.

units(time_headers, "time", "time", "running time", "seconds", "seconds",
         "milliseconds", "ms/call", "spent executing").
units(memory_words_headers, "mem", "memory", "allocated memory",
        "k-words", "kilowords", "words", "wds/call", "allocated by").
units(memory_cells_headers, "cells", "allocations",
        "number of memory allocations", "k-cells", "kilocells", "cells",
        "cls/call", "occurring in").

:- pred output_call_graph_headers(io.text_output_stream::in,
    io::di, io::uo) is det.

output_call_graph_headers(OutputStream, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.get_what_to_profile(Globals, WhatToProfile),
    Category = classify_profile(WhatToProfile),
    units(Category, ShortWhat, What, LongWhat,
        _ShortUnits, Units, _MilliUnits, _MilliUnitsPerCall, SpentIn),

    io.write_string(OutputStream,
        "call graph profile:\n", !IO),
    io.format(OutputStream,
        "\tSorted on the %%%s field.\n\n", [s(ShortWhat)], !IO),

    io.write_string(OutputStream,
        "\tprocedure entries:\n\n", !IO),

    io.write_string(OutputStream,
        "index\t\tthe index number of the procedure in the call graph\n", !IO),
    io.write_string(OutputStream,
        "\t\tlisting.\n\n", !IO),

    io.format(OutputStream,
        "%%%s\t\tthe percentage of the total %s of\n",
        [s(ShortWhat), s(LongWhat)], !IO),
    io.format(OutputStream,
        "\t\tthe program %s this procedure and its\n",
        [s(SpentIn)], !IO),
    io.write_string(OutputStream,
        "\t\tdescendents.\n\n", !IO),

    io.format(OutputStream,
        "self\t\tthe number of %s actually %s\n",
        [s(Units), s(SpentIn)], !IO),
    io.write_string(OutputStream,
        "\t\tthe procedure's own code.\n\n", !IO),

    io.format(OutputStream,
        "descendents\tthe number of %s %s the\n",
        [s(Units), s(SpentIn)], !IO),
    io.write_string(OutputStream,
        "\t\tdescendents of the current procedure.\n\n", !IO),

    io.write_string(OutputStream,
        "called\t\tthe number of times the current procedure is\n", !IO),
    io.write_string(OutputStream,
        "\t\tcalled (not counting self recursive calls).\n\n", !IO),

    io.write_string(OutputStream,
        "self\t\tthe number of self recursive calls.\n\n", !IO),

    io.write_string(OutputStream,
        "name\t\tthe name of the current procedure.\n\n", !IO),

    io.write_string(OutputStream,
        "index\t\tan index number to locate the function easily.\n\n\n\n",
        !IO),

    io.write_string(OutputStream,
        "\tparent listings:\n\n", !IO),

    io.format(OutputStream,
        "self*\t\tthe number of %s of the current procedure's self\n",
        [s(Units)], !IO),
    io.format(OutputStream,
        "\t\t%s due to calls from this parent.\n\n", [s(What)], !IO),

    io.format(OutputStream,
        "descendents*\tthe number of %s of the current " ++
        "procedure's descendent\n", [s(Units)], !IO),
    io.format(OutputStream,
        "\t\t%s which is due to calls from this parent.\n\n", [s(What)], !IO),

    io.write_string(OutputStream,
        "called*\t\tthe number of times the current procedure is called\n",
        !IO),
    io.write_string(OutputStream,
        "\t\tby this parent.\n\n", !IO),

    io.write_string(OutputStream,
        "total\t\tthe number of times this procedure "
        ++ "is called by its parents.\n\n", !IO),

    io.write_string(OutputStream,
        "parents\t\tthe name of this parent.\n\n", !IO),

    io.write_string(OutputStream,
        "index\t\tthe index number of the parent procedure\n\n\n\n", !IO),

    io.write_string(OutputStream,
        "children listings:\n\n", !IO),

    io.format(OutputStream,
        "self*\t\tthe number of %s of this child's self %s which is\n",
        [s(Units), s(What)], !IO),
    io.write_string(OutputStream,
        "\t\tdue to being called by the current procedure.\n\n", !IO),

    io.format(OutputStream,
        "descendent*\tthe number of %s of this child's descendent %s which\n",
        [s(Units), s(What)], !IO),
    io.write_string(OutputStream,
        "\t\tis due to the current procedure.\n\n", !IO),

    io.write_string(OutputStream,
        "called*\t\tthe number of times this child is called by the current\n",
        !IO),
    io.write_string(OutputStream,
        "\t\tprocedure.\n\n", !IO),

    io.write_string(OutputStream,
        "total*\t\tthe number of times this child "
        ++ "is called by all procedures.\n\n", !IO),

    io.write_string(OutputStream,
        "children\tthe name of this child.\n\n", !IO),

    io.write_string(OutputStream,
        "index\t\tthe index number of the child.\n\n\n\n", !IO),

    io.write_string(OutputStream,
        "                                  called/total", !IO),
    io.write_string(OutputStream,
        "       parents\n", !IO),
    io.format(OutputStream,
        "index %6s    self descendents  called+self",
        [s("%" ++ ShortWhat)], !IO),
    io.write_string(OutputStream,
        "    name           index\n", !IO),
    io.write_string(OutputStream,
        "                                  called/total", !IO),
    io.write_string(OutputStream,
        "       children\n\n", !IO).

:- pred output_call_graph(io.text_output_stream::in, list(string)::in,
    map(string, output_prof)::in, map(string, int)::in, io::di, io::uo) is det.

output_call_graph(_, [], _, _, !IO).
output_call_graph(OutputStream, [LabelName | LabelNames],
        InfoMap, IndexMap, !IO) :-
    map.lookup(InfoMap, LabelName, PN),
    map.lookup(IndexMap, LabelName, Index),
    output_formatted_prof_node(OutputStream, PN, Index, IndexMap, !IO),
    io.write_string(OutputStream,
        "\n-----------------------------------------------\n\n", !IO),
    output_call_graph(OutputStream, LabelNames, InfoMap, IndexMap, !IO).

:- pred output_formatted_prof_node(io.text_output_stream::in, output_prof::in,
    int::in, map(string, int)::in, io::di, io::uo) is det.

output_formatted_prof_node(OutputStream, ProfNode, Index, IndexMap, !IO) :-
    (
        ProfNode = output_prof(Name, CycleNum, Percentage, _, Self,
            Descendant, TotalCalls, SelfCalls, ParentList,
            ChildList, CycleParentList, CycleChildList)
    ;
        ProfNode = output_cycle_prof(_, _, _, _, _, _, _, _, _),
        error("output_formatted_prof_node: Cannot have output_cycle_prof\n")
    ),

    % Set up all the output strings.
    FullName = construct_output_name(Name, CycleNum),
    string.int_to_string(Index, IndexStr0),
    string.append_list(["[", IndexStr0, "] "], IndexStr),
    string.format("%40d             %s [%d]\n",
        [i(SelfCalls), s(FullName), i(Index)], SelfCallsString),
    string.format("%-6s %5.1f %7.2f %11.2f %7d", [s(IndexStr),
        f(Percentage), f(Self), f(Descendant), i(TotalCalls)], InitMiddleStr),

    ( if SelfCalls = 0 then
        true
    else
        io.write_string(OutputStream, SelfCallsString, !IO)
    ),

    ( if
        CycleParentList = [],
        ParentList = []
    then
        io.format(OutputStream, "%67s", [s("<spontaneous>\n")], !IO)
    else
        list.sort(CycleParentList, SortedCycleParentList),
        output_formatted_cycle_parent_list(OutputStream, SortedCycleParentList,
            IndexMap, !IO),
        list.sort(ParentList, SortedParentList),
        output_formatted_parent_list(OutputStream, SortedParentList, IndexMap,
            TotalCalls, !IO)
    ),

    % Output the info about the current procedure.
    io.write_string(OutputStream, InitMiddleStr, !IO),
    ( if SelfCalls = 0 then
        io.write_string(OutputStream, "         ", !IO)
    else
        io.format(OutputStream, "+%-7d", [i(SelfCalls)], !IO)
    ),
    io.write_string(OutputStream, FullName ++ " " ++ IndexStr ++ "\n", !IO),

    list.sort(ChildList, SortedChildList),
    output_formatted_child_list(OutputStream, SortedChildList, IndexMap, !IO),
    list.sort(CycleChildList, SortedCycleChildList),
    output_formatted_cycle_child_list(OutputStream, SortedCycleChildList,
        IndexMap, !IO),

    ( if SelfCalls = 0 then
        true
    else
        io.write_string(OutputStream, SelfCallsString, !IO)
    ).

    % output_formatted_cycle_parent_list
    % outputs the parents of a procedure that are in the same cycle.
    %
:- pred output_formatted_cycle_parent_list(io.text_output_stream::in,
    list(parent)::in, map(string, int)::in, io::di, io::uo) is det.

output_formatted_cycle_parent_list(OutputStream, Parents, IndexMap, !IO) :-
    list.foldl(
        ( pred(Parent::in, !.IO::di, !:IO::uo) is det :-
            Parent = parent(LabelName, CycleNum, _, _, Calls),
            Name = construct_output_name(LabelName, CycleNum),
            map.lookup(IndexMap, LabelName, Index),
            io.format(OutputStream, "%40d             %s [%d]\n",
                [i(Calls), s(Name), i(Index)], !IO)
        ), Parents, !IO).

    % output_formatted_parent_list:
    % Outputs the parent list of the current procedure.
    %
:- pred output_formatted_parent_list(io.text_output_stream::in,
    list(parent)::in, map(string, int)::in, int::in, io::di, io::uo) is det.

output_formatted_parent_list(_, [], _, _, !IO).
output_formatted_parent_list(OutputStream, [Parent | Parents], IndexMap,
        TotalCalls, !IO) :-
    Parent = parent(LabelName, CycleNum, Self, Descendant, Calls),
    Name = construct_output_name(LabelName, CycleNum),
    map.lookup(IndexMap, LabelName, Index),
    io.format(OutputStream, "%20.2f %11.2f %7d/%-11d %s [%d]\n",
        [f(Self), f(Descendant), i(Calls), i(TotalCalls), s(Name), i(Index)],
        !IO),
    output_formatted_parent_list(OutputStream, Parents, IndexMap,
        TotalCalls, !IO).

    % output_formatted_cycle_child_list:
    %
    % Outputs the children of a procedure that are in the same cycle.
    %
:- pred output_formatted_cycle_child_list(io.text_output_stream::in,
    list(child)::in, map(string, int)::in, io::di, io::uo) is det.

output_formatted_cycle_child_list(_, [], _, !IO).
output_formatted_cycle_child_list(OutputStream, [Child | Childs],
        IndexMap, !IO) :-
    Child = child(LabelName, CycleNum, _Self, _Descendant, Calls, _),
    Name = construct_output_name(LabelName, CycleNum),
    map.lookup(IndexMap, LabelName, Index),
    io.format(OutputStream, "%40d             %s [%d]\n",
        [i(Calls), s(Name), i(Index)], !IO),
    output_formatted_cycle_child_list(OutputStream, Childs, IndexMap, !IO).

    % output_formatted_child_list:
    %
    % outputs the child list of the current procedure.
    %
:- pred output_formatted_child_list(io.text_output_stream::in,
    list(child)::in, map(string, int)::in, io::di, io::uo) is det.

output_formatted_child_list(OutputStream, Children, IndexMap, !IO) :-
    list.foldl(
        ( pred(Child::in, !.IO::di, !:IO::uo) is det :-
            Child = child(LabelName, CycleNum, Self, Descendant,
                Calls, TotalCalls),
            Name = construct_output_name(LabelName, CycleNum),
            map.lookup(IndexMap, LabelName, Index),
            io.format(OutputStream, "%20.2f %11.2f %7d/%-11d %s [%d]\n",
                [f(Self), f(Descendant), i(Calls), i(TotalCalls),
                s(Name), i(Index)], !IO)
        ), Children, !IO).

:- pred output_flat_headers(io.text_output_stream::in, io::di, io::uo) is det.

output_flat_headers(OutputStream, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.get_what_to_profile(Globals, WhatToProfile),
    Category = classify_profile(WhatToProfile),
    units(Category, ShortWhat, _What, LongWhat,
        ShortUnits, Units, MilliUnits, MilliUnitsPerCall, SpentIn),

    io.write_string(OutputStream, "\nflat profile:\n\n", !IO),

    io.format(OutputStream,
        "%%\t\tthe percentage of total %s of the program\n",
        [s(LongWhat)], !IO),
    io.format(OutputStream,
        "%s\t\tused by this procedure.\n\n",
        [s(ShortWhat)], !IO),

    io.format(OutputStream,
        "cumulative\tthe total number of %s for the current procedure and\n",
        [s(Units)], !IO),
    io.format(OutputStream,
        "%s\t\tthe ones listed above it.\n\n",
        [s(ShortUnits)], !IO),

    io.format(OutputStream,
        "self\t\tthe number of %s accounted for by this procedure alone.\n",
        [s(Units)], !IO),
    io.format(OutputStream,
        "%s\t\tThe listing is sorted on this row.\n\n",
        [s(ShortUnits)], !IO),

    io.write_string(OutputStream,
        "calls\t\tthe number of times this procedure was called.\n\n", !IO),

    io.format(OutputStream,
        "self\t\tthe average number of %s %s\n",
        [s(MilliUnits), s(SpentIn)], !IO),
    io.format(OutputStream,
        "%s  \tthis procedure per call.\n\n",
        [s(MilliUnitsPerCall)], !IO),

    io.format(OutputStream,
       "total\t\tthe average number of %s %s this procedure and its\n",
        [s(MilliUnits), s(SpentIn)], !IO),
    io.format(OutputStream,
        "%s  \tdescendents per call.\n\n",
        [s(MilliUnitsPerCall)], !IO),

    io.write_string(OutputStream,
        "name\t\tthe name of the procedure followed by its index number.\n\n",
        !IO),

    io.write_string(OutputStream,
        "   %  cumulative    self              self", !IO),
    io.write_string(OutputStream,
        "    total\n", !IO),
    io.format(OutputStream,
        " %4s    %7s  %7s    calls %8s %8s name\n",
        [s(ShortWhat), s(ShortUnits), s(ShortUnits),
        s(MilliUnitsPerCall), s(MilliUnitsPerCall)], !IO).

:- pred output_flat_profile(io.text_output_stream::in, list(string)::in,
    float::in, map(string, output_prof)::in, map(string, int)::in,
    io::di, io::uo) is det.

output_flat_profile(_, [], _, _, _, !IO).
output_flat_profile(OutputStream, [LabelName | LNs], CumTime0, InfoMap,
        IndexMap, !IO) :-
    map.lookup(InfoMap, LabelName, ProfNode),
    map.lookup(IndexMap, LabelName, Index),
    (
        ProfNode = output_prof(Name, CycleNum, _Percentage, Percentage, Self,
            Descendant, TotalCalls, SelfCalls,  _ParentList, _ChildList, _, _)
    ;
        ProfNode = output_cycle_prof(_, _, _, _, _, _, _, _, _),
        error("output_flat_profile: Cannot have output_cycle_prof\n")
    ),
    FloatTotalCalls = float(TotalCalls) + float(SelfCalls),
    Calls = SelfCalls + TotalCalls,
    CumTime = CumTime0 + Self,
    SelfSeconds = checked_float_divide(Self, FloatTotalCalls),
    DescSeconds = checked_float_divide(Descendant, FloatTotalCalls),
    SelfMs = 1000.0 * SelfSeconds,
    DescMs = 1000.0 * DescSeconds,

    FullName = construct_output_name(Name, CycleNum),
    string.int_to_string(Index, IndexStr0),
    string.append_list(["[", IndexStr0, "] "], IndexStr),
    io.format(OutputStream, "%5.1f %10.2f %8.2f %8d %8.2f %8.2f %s %s\n",
        [f(Percentage), f(CumTime), f(Self), i(Calls),
        f(SelfMs), f(DescMs), s(FullName), s(IndexStr)], !IO),
    output_flat_profile(OutputStream, LNs, CumTime, InfoMap, IndexMap, !IO).

:- pred output_alphabet_headers(io.text_output_stream::in,
    io::di, io::uo) is det.

output_alphabet_headers(OutputStream, !IO) :-
    io.write_string(OutputStream, "\n\n\nalphabetic listing:\n\n", !IO).

:- pred output_alphabet_listing(io.text_output_stream::in,
    map(string, int)::in, io::di, io::uo) is det.

output_alphabet_listing(OutputStream, IndexMap, !IO) :-
    IndexList = map.to_assoc_list(IndexMap),
    output_alphabet_listing_2(OutputStream, IndexList, !IO).

:- pred output_alphabet_listing_2(io.text_output_stream::in,
    assoc_list(string, int)::in, io::di, io::uo) is det.

output_alphabet_listing_2(OutputStream, [], !IO) :-
    io.nl(OutputStream, !IO).
output_alphabet_listing_2(OutputStream, [Name - Index | T], !IO) :-
    io.format(OutputStream, "[%d]\t%-30s\n", [i(Index), s(Name)], !IO),
    output_alphabet_listing_2(OutputStream, T, !IO).

    % construct_output_name:
    %
    % Constructs an output name with an optional cycle number if required.
    %
:- func construct_output_name(string, int) = string.

construct_output_name(Name, CycleNum) = FullName :-
    ( if CycleNum = 0 then
        FullName = Name
    else
        string.int_to_string(CycleNum, CycleStr),
        string.append_list([Name, "  <cycle ", CycleStr, ">"], FullName)
    ).

%---------------------------------------------------------------------------%
:- end_module output.
%---------------------------------------------------------------------------%
