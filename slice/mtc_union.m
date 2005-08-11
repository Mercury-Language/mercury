%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Tool to combine several trace counts into one.
% Main Author: Ian MacLarty.
%
%-----------------------------------------------------------------------------%

:- module mtc_union.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.trace_counts.

:- import_module assoc_list.
:- import_module bool.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module string.
:- import_module require.

main(!IO) :-
	io.command_line_arguments(Args0, !IO),
	OptionOps = option_ops_multi(short_option, long_option,
		option_default),
	getopt.process_options(OptionOps, Args0, Args, GetoptResult),
	(
		GetoptResult = ok(OptionTable),
		lookup_string_option(OptionTable, output_filename, 
			OutputFile),
		(
			Args \= [],
			OutputFile \= ""
		->
			lookup_bool_option(OptionTable, verbose, Verbose),
			union_trace_counts(Verbose, Args, 0, map.init,
				OutputFile, !IO)
		;
			usage(!IO)
		)
	;
		GetoptResult = error(GetoptErrorMsg),
		io.write_string(GetoptErrorMsg, !IO),
		io.nl(!IO)
	).

:- pred union_trace_counts(bool::in, list(string)::in, int::in, 
	trace_counts::in, string::in, io::di, io::uo) is det.

union_trace_counts(_, [], NumTests, TraceCounts, OutputFile, !IO) :-
	write_trace_counts_to_file(union(NumTests), TraceCounts,
		OutputFile, WriteResult, !IO),
	(
		WriteResult = ok
	;
		WriteResult = error(Error),
		stderr_stream(StdErr, !IO),
		io.write_string(StdErr, "Error writing to " ++
			"file `" ++ OutputFile ++
			"'" ++ ": " ++ string(Error), !IO),
		io.nl(StdErr, !IO)
	).
union_trace_counts(ShowProgress, [File | Files], NumTests0, TraceCounts0, 
		OutputFile, !IO) :-
	read_trace_counts_source(ShowProgress, try_single_first, File, 
		TCResult, !IO),
	(
		TCResult = list_ok(FileType, TraceCounts1),
		summarize_trace_counts_list([TraceCounts0, TraceCounts1], 
			TraceCounts),
		NumTests = NumTests0 + num_tests_for_file_type(FileType),
		union_trace_counts(ShowProgress, Files, NumTests, TraceCounts, 
			OutputFile, !IO)
	;
		TCResult = list_error_message(Message),
		stderr_stream(StdErr, !IO),
		io.write_string(StdErr, Message, !IO),
		io.nl(StdErr, !IO)
	).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
	io__write_strings([
		"Usage: mtc_union [-p] -o output_file file1 file2 ...\n",
		"The -v or --verbose option causes each trace count ",
		"file name\n",
		"to be printed as it is added to the union.\n",
		"file1, file2, etc can be trace count files or they\n",
		"can be files which contains lists of other trace count ",
		"files.\n"], !IO).

%-----------------------------------------------------------------------------%

:- type option	
	--->	verbose
	;	output_filename.

:- type option_table == option_table(option).
		
:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_default(option::out, option_data::out) is multi.

option_default(verbose,		bool(no)).
option_default(output_filename,	string("")).

short_option('v',		verbose).
short_option('o',		output_filename).

long_option("verbose",		verbose).
long_option("out",		output_filename).

%-----------------------------------------------------------------------------%
