%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 2000-2001, 2004-2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: options.m.
% Main author: fjh.
%
% This defines the stuff necessary so that getopt.m can parse the command line
% options.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module options.
:- interface.

:- import_module bool.
:- import_module getopt.
:- import_module io.

%---------------------------------------------------------------------------%

:- type option
    % Verbosity options
    --->    verbose
    ;       very_verbose
    % Profiler options
    ;       dynamic_cg
    ;       call_graph
    ;       profile
    ;       profile_time
    ;       profile_memory_words
    ;       profile_memory_cells
    ;       demangle
    % Filename options
    ;       countfile
    ;       pairfile
    ;       declfile
    ;       libraryfile
    % Snapshot (memory attribution profiling) options
    ;       snapshots
    ;       snapshots_file
    ;       snapshots_by_type
    ;       snapshots_brief
    ;       snapshots_include_runtime
    ;       snapshots_recalc_size       % developers only
    % Miscellaneous Options
    ;       help.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_default(option::out, option_data::out) is multi.
:- pred special_handler(option::in, special_data::in, option_table::in,
    maybe_option_table(option)::out) is semidet.

:- pred options_help(io.text_output_stream::in, io::di, io::uo) is det.

% A couple of misc utilities

:- pred maybe_write_string(io.text_output_stream::in, bool::in, string::in,
    io::di, io::uo) is det.
:- pred maybe_flush_output(io.text_output_stream::in, bool::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

    % please keep this in alphabetic order
short_option('b', snapshots_brief).
short_option('C', countfile).
short_option('c', call_graph).
short_option('d', dynamic_cg).
short_option('D', declfile).
short_option('h', help).
short_option('L', libraryfile).
short_option('m', profile_memory_words).
short_option('M', profile_memory_cells).
short_option('p', profile).
short_option('P', pairfile).
short_option('r', snapshots_include_runtime).
short_option('s', snapshots).
short_option('t', profile_time).
short_option('T', snapshots_by_type).
short_option('v', verbose).
short_option('V', very_verbose).

long_option("call-pair-file",       pairfile).
long_option("call-graph",           call_graph).
long_option("count-file",           countfile).
long_option("declaration-file",     declfile).
long_option("demangle",             demangle).
long_option("help",                 help).
% XXX: what is this doing here?
long_option("library-callgraph",    help).
long_option("profile",              profile).
long_option("profile-memory-words", profile_memory_words).
long_option("profile-memory-cells", profile_memory_cells).
long_option("profile-time",         profile_time).
long_option("snapshots",            snapshots).
long_option("snapshots-brief",      snapshots_brief).
long_option("snapshots-by-type",    snapshots_by_type).
long_option("snapshots-file",       snapshots_file).
long_option("snapshots-include-runtime", snapshots_include_runtime).
long_option("snapshots-recalc-size", snapshots_recalc_size).
long_option("use-dynamic",          dynamic_cg).
long_option("verbose",              verbose).
long_option("very-verbose",         very_verbose).

    % Verbosity Options
option_default(verbose,                 bool(no)).
option_default(very_verbose,            bool(no)).

    % General profiler options
option_default(dynamic_cg,              bool(no)).
option_default(call_graph,              bool(no)).
option_default(profile,                 string_special).
option_default(profile_time,            special).
option_default(profile_memory_words,    special).
option_default(profile_memory_cells,    special).
option_default(countfile,               string("Prof.Counts")).
option_default(pairfile,                string("Prof.CallPair")).
option_default(declfile,                string("Prof.Decl")).
option_default(libraryfile,             string("")).
option_default(demangle,                bool(yes)).
option_default(snapshots,               bool(no)).
option_default(snapshots_file,          string("Prof.Snapshots")).
option_default(snapshots_by_type,       bool(no)).
option_default(snapshots_brief,         bool(no)).
option_default(snapshots_include_runtime, bool(no)).
option_default(snapshots_recalc_size,   bool(yes)).

    % Miscellaneous Options
option_default(help,                    bool(no)).

special_handler(profile, string(WhatToProfile), !.OptionTable, Result) :-
    ( if valid_profile_option(WhatToProfile, CountFile) then
        map.set(countfile, string(CountFile), !OptionTable),
        Result = ok(!.OptionTable)
    else
        Result = error("Invalid argument to `--profile' or `-p' option")
    ).
special_handler(profile_memory_words, _, !.OptionTable, ok(!:OptionTable)) :-
    map.set(countfile, string("Prof.MemoryWords"), !OptionTable).
special_handler(profile_memory_cells, _, !.OptionTable, ok(!:OptionTable)) :-
    map.set(countfile, string("Prof.MemoryCells"), !OptionTable).
special_handler(profile_time, _, !.OptionTable, ok(!:OptionTable)) :-
    map.set(countfile, string("Prof.Counts"), !OptionTable).

:- pred valid_profile_option(string::in, string::out) is semidet.

valid_profile_option("memory-words", "Prof.MemoryWords").
valid_profile_option("memory-cells", "Prof.MemoryCells").
valid_profile_option("time", "Prof.Counts").

options_help(OutputStream, !IO) :-
    io.write_strings(OutputStream, [
        "-h, --help\n",
        "\t\tPrint this usage message.\n",
        "\n",
        "Profiler Options:\n",
        "\t-c, --call-graph\n",
        "\t\tInclude the call graph profile.\n",
        "\t-d, --use-dynamic\n",
        "\t\tBuild the call graph dynamically.\n",
        "\t-p, --profile {time, memory-words, memory-cells}\n",
        "\t\tSelect what to profile: time, amount of memory allocated, or\n",
        "\t\tnumber of memory allocations (regardless of size).\n",
        "\t-m\n",
        "\t\tSame as `--profile memory-words'\n",
        "\t-M\n",
        "\t\tSame as `--profile memory-cells'.\n",
        "\t-t\n",
        "\t\tSame as `--profile time'.\n",
        "\t--no-demangle\n",
        "\t\tOutput the mangled predicate and function names.\n",
        "\n",
        "Filename Options:\n",
        "\t-C <file>, --count-file <file>\n",
        "\t\tName of the count file. Usually `Prof.Counts',\n",
        "\t\t`Prof.MemoryWords', or `Prof.MemoryCells'.\n",
        "\t-D <file>, --declaration-file <file>\n",
        "\t\tName of the declaration file. Usually `Prof.Decl'.\n",
        "\t-P <file>, --call-pair-file <file>\n",
        "\t\tName of the call-pair file. Usually `Prof.CallPair'.\n",
        "\t-L <file>, --library-callgraph <file>\n",
        "\t\tName of the file which contains the call graph for\n",
        "\t\tthe library modules.\n",
        "\n",
        "Snapshot options:\n",
        "\t-s, --snapshots\n",
        "\t\tShow summary of heap objects at the times\n",
        "\t\t`benchmarking.report_memory_attribution' was called.\n",
        "\t\tThis overrides other profiler modes.\n",
        "\t--snapshots-file <file>\n",
        "\t\tName of the snapshots file. Usually `Prof.Snapshots'.\n",
        "\t-T, --snapshots-by-type\n",
        "\t\tGroup results by type.\n",
        "\t-b, --snapshots-brief\n",
        "\t\tGenerate a brief profile.\n",
        "\t-r, --snapshots-include-runtime\n",
        "\t\tInclude internal Mercury runtime structures in the\n",
        "\t\tprofile. These are excluded by default.\n",
        "\n",
        "Verbosity Options:\n",
        "\t-v, --verbose\n",
        "\t\tOutput progress messages at each stage.\n",
        "\t-V, --very-verbose\n",
        "\t\tOutput very verbose progress messages.\n"], !IO).

%---------------------------------------------------------------------------%

maybe_write_string(OutputStream, yes, String, !IO) :-
    io.write_string(OutputStream, String, !IO).
maybe_write_string(_OutputStream, no, _, !IO).

maybe_flush_output(OutputStream, yes, !IO) :-
    io.flush_output(OutputStream, !IO).
maybe_flush_output(_OutputStream, no, !IO).

%---------------------------------------------------------------------------%
:- end_module options.
%---------------------------------------------------------------------------%
