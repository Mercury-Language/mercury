%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: getopt.m
% Author: fjh
% Stability: medium

% This module exports the predicate getopt__process_options/4,
% which can be used to parse command-line options.
%
% This version allows both short (single-character) options
% and GNU-style long options. It also has the GNU extension
% of recognizing options anywhere in the command-line, not
% just at the start.
%
% To use this module, you must provide an `option' type which
% is an enumeration of all your different options.
% You must provide predicates `short_option(Char, Option)'
% and `long_option(String, Option)' which convert the short
% and/or long names for the option to this enumeration type.
% (An option can have as many names as you like, long or short.)
% You must provide a predicate `option_default(Option, OptionData)'
% which specifies both the type and the default value for every option.
% We support four different option types: bool, int, string, and
% "accumulating" (which accumulates a list of strings).
% For the first three option types, if there are multiple occurrences
% of the same option on the command-line, then the last (right-most)
% occurrence will take precedence.  For "accumulating" options,
% multiple occurrences will be appended together into a list.
% Single-character boolean options can be negated by following them
% with another `-', e.g. `-x-' will negate the `-x' option.
% Long boolean options can be negated by preceding them with `--no-',
% e.g. `--no-foo' will negate the `--foo' option.

:- module getopt.
:- interface.
:- import_module int, string, list, map, std_util.

	% getopt__process_options(OptionOps, Args, NonOptionArgs, Result)
	%
	%	Scans through 'Args' looking for options, places all the
	%	non-option arguments in 'NonOptionArgs', and record the
	%	options in the OptionTable.  OptionTable is a map from 
	%	a user-defined option type to option_data.
	%	If an invalid option is encountered, we return error(Message)
	%	otherwise we return ok(OptionTable) in 'Result'.
	% 
	%	The argument `OptionOps' holds three predicates used
	%	to categorize a set of options. `OptionOps' should be
	%	bound to `option_ops(ShortOption, LongOption, OptionDefault)',
	%	where the three variables are higher-order predicate variables
	%	bound to predicates which implement the following interfaces.
/*

:- pred short_option(character::in, option::out) is semidet.
	% true if the character names a valid single-character option

:- pred long_option(string::in, option::out) is semidet.
	% true if the character names a valid long option

:- pred option_default(option::out, option_data::out) is nondet.
	% nondeterministically returns all the options with their
	% corresponding types and default values
*/

:- pred getopt__process_options(
		option_ops(OptionType)::in(option_ops),
		list(string)::in,
		list(string)::out,
		maybe_option_table(OptionType)::out
	) is det.

:- type option_ops(OptionType)
	--->	option_ops(
			pred(character, OptionType),	% short_option
			pred(string, OptionType),	% long_option
			pred(OptionType, option_data)	% option_default
		).

:- inst option_ops
	=	bound(option_ops(
			pred(in, out) is semidet,	% short_option
			pred(in, out) is semidet,	% long_option
			pred(out, out) is nondet	% option_default
		)).

:- type option_data
	--->	bool(bool)
	;	int(int)
	;	string(string)
	;	accumulating(list(string)).

:- type option_table(OptionType)
	==	map(OptionType, option_data).

:- type maybe_option_table(OptionType)
	--->	ok(option_table(OptionType))
	;	error(string).

	% The following three predicates search the option table for
	% an option of the specified type; if it is not found, they
	% report an error by calling error/1.

:- pred getopt__lookup_bool_option(option_table(Option), Option, bool).
:- mode getopt__lookup_bool_option(in, in, out) is det.

:- pred getopt__lookup_int_option(option_table(Option), Option, int).
:- mode getopt__lookup_int_option(in, in, out) is det.

:- pred getopt__lookup_string_option(option_table(Option), Option, string).
:- mode getopt__lookup_string_option(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require.

getopt__process_options(OptionOps, Args0, Args, Result) :-
	OptionOps = option_ops(_, _, OptionDefaults),
	solutions(lambda([OptionDataPair::out] is nondet, (
			OptionDataPair = Option - OptionData,
			call(OptionDefaults, Option, OptionData)
		)), OptionDefaultsList),
	map__from_assoc_list(OptionDefaultsList, OptionTable0),
	process_options_2(Args0, OptionTable0, OptionOps, Args, Result).

:- pred process_options_2(list(string), option_table(OptionType),
			option_ops(OptionType),
			list(string), maybe_option_table(OptionType)).
:- mode process_options_2(in, in, in(option_ops), out, out) is det.

process_options_2([], OptionTable, _, [], ok(OptionTable)).
process_options_2([Option | Args0], OptionTable0, OptionOps, Args, Result) :-
	OptionOps = option_ops(ShortOptionPred, LongOptionPred, _),
	( Option = "--" ->	% "--" terminates option processing
		Args = Args0,
		Result = ok(OptionTable0)
	; string__append("--no-", LongOption, Option) ->
		( call(LongOptionPred, LongOption, Flag) ->
			string__append("--", LongOption, OptName),
			process_negated_bool_option(OptName, Flag, Args0,
					OptionTable0, OptionOps, Args, Result)
		;
			string__append("unrecognized option `--no-",
					LongOption, Tmp),
			string__append(Tmp, "'", ErrorMsg),
			Result = error(ErrorMsg),
			Args = Args0
		)
	; string__append("--", LongOption, Option) ->
	  	( call(LongOptionPred, LongOption, Flag) ->
			process_option(Option, Flag, Args0, OptionTable0,
					OptionOps, Args, Result)
		;
			string__append("unrecognized option `", Option, Tmp),
			string__append(Tmp, "'", ErrorMsg),
			Result = error(ErrorMsg),
			Args = Args0
		)
	; string__first_char(Option, '-', ShortOptions) ->
		string__to_char_list(ShortOptions, ShortOptionsList),
		    % check for a single `-x' option
		( ShortOptionsList = [SingleShortOpt] ->
			( call(ShortOptionPred, SingleShortOpt, Flag) ->
				process_option(Option, Flag,
					Args0, OptionTable0, OptionOps,
					Args, Result)
			;
				string__append("unrecognized option `",
					Option, Tmp),
				string__append(Tmp, "'", ErrorMsg),
				Result = error(ErrorMsg),
				Args = Args0
			)
		    % check for a single negated option `-x-'
		; ShortOptionsList = [SingleShortOpt, '-'] ->
			( call(ShortOptionPred, SingleShortOpt, Flag) ->
				string__from_char_list(['-', SingleShortOpt],
						OptName),
				process_negated_bool_option(OptName, Flag,
					Args0, OptionTable0, OptionOps,
					Args, Result)
			;
				string__append("unrecognized option `-",
					ShortOptions, Tmp),
				string__append(Tmp, "'", ErrorMsg),
				Result = error(ErrorMsg),
				Args = Args0
			)
		;
		    % process a list of boolean options `-xyz'
			process_short_option_list(ShortOptionsList, Args0,
					OptionTable0, OptionOps, Args, Result)
		)
	;
	    % It's a normal non-option argument.
	    % As a GNU extension, keep searching for options
	    % in the remaining arguments.
		Args = [Option | Args1],
		process_options_2(Args0, OptionTable0, OptionOps, Args1, Result)
	).

:- pred process_short_option_list(list(character),
		list(string), option_table(OptionType), option_ops(OptionType),
		list(string), maybe_option_table(OptionType)).
:- mode process_short_option_list(in, in, in, in(option_ops), out, out) is det.

process_short_option_list([], Args0, OptionsTable0, OptionOps, Args, Result) :-
	process_options_2(Args0, OptionsTable0, OptionOps, Args, Result).

process_short_option_list([Opt | Opts], Args0, OptionsTable0, OptionOps,
			Args, Result) :-
	OptionOps = option_ops(ShortOptionPred, _, _),
	( call(ShortOptionPred, Opt, Flag) ->
		process_short_option(Opt, Flag, Opts, Args0, OptionsTable0,
				OptionOps, Args, Result)
	;
		string__char_to_string(Opt, OptString),
		string__append("unrecognized option `-", OptString, Tmp),
		string__append(Tmp, "'", ErrorMessage),
		Result = error(ErrorMessage),
		Args = Args0
	).

:- pred process_short_option(character, OptionType, list(character),
		list(string), option_table(OptionType), option_ops(OptionType),
		list(string), maybe_option_table(OptionType)).
:- mode process_short_option(in, in, in, in, in, in(option_ops),
		out, out) is det.

process_short_option(Opt, Flag, Opts, Args0, OptionTable0, OptionOps,
		Args, Result) :-
	map__lookup(OptionTable0, Flag, Data),
	process_short_option_2(Data, Opt, Flag, Opts, Args0, OptionTable0,
		OptionOps, Args, Result).

:- pred process_short_option_2(option_data, character, OptionType,
		list(character), list(string), option_table(OptionType),
		option_ops(OptionType),
		list(string), maybe_option_table(OptionType)).
:- mode process_short_option_2(in, in, in, in, in, in, in(option_ops),
		out, out) is det.

process_short_option_2(bool(_), _Opt, Flag, Opts, Args0, OptionTable0,
		OptionOps, Args, Result) :-
	map__set(OptionTable0, Flag, bool(yes), OptionTable1),
	process_short_option_list(Opts, Args0, OptionTable1,
		OptionOps, Args, Result).
process_short_option_2(string(_), _Opt, Flag, Opts, Args0, OptionTable0,
		OptionOps, Args, Result) :-
	string__from_char_list(Opts, Arg),
	map__set(OptionTable0, Flag, string(Arg), OptionTable1),
	process_options_2(Args0, OptionTable1, OptionOps, Args, Result).
process_short_option_2(int(_), Opt, Flag, Opts, Args0, OptionTable0,
		OptionOps, Args, Result) :-
	string__from_char_list(Opts, Arg),
	( string__to_int(Arg, IntArg) ->
		map__set(OptionTable0, Flag, int(IntArg), OptionTable1),
		process_options_2(Args0, OptionTable1, OptionOps, Args, Result)
	;
		Args = Args0,
		string__char_to_string(Opt, Option),
		string__append_list(["option `-", Option,
			"' requires a numeric argument"], ErrorMsg),
		Result = error(ErrorMsg)
	).
process_short_option_2(accumulating(List0), _Opt, Flag, Opts, Args0,
		OptionTable0, OptionOps, Args, Result) :-
	string__from_char_list(Opts, Arg),
	list__append(List0, [Arg], List),
	map__set(OptionTable0, Flag, accumulating(List), OptionTable1),
	process_options_2(Args0, OptionTable1, OptionOps, Args, Result).

:- pred process_option(string, OptionType,
			list(string), option_table(OptionType),
			option_ops(OptionType),
			list(string), maybe_option_table(OptionType)).
:- mode process_option(in, in, in, in, in(option_ops), out, out) is det.

process_option(OptName, Flag, Args0, OptionTable0, OptionOps, Args, Result) :-
	map__lookup(OptionTable0, Flag, Data),
	process_option_2(Data, OptName, Flag, Args0, OptionTable0, OptionOps,
		Args, Result).

:- pred process_option_2(option_data, string, OptionType, list(string),
			option_table(OptionType), option_ops(OptionType),
			list(string), maybe_option_table(OptionType)).
:- mode process_option_2(in, in, in, in, in, in(option_ops), out, out) is det.

process_option_2(bool(_), _OptName, Flag, Args0, OptionTable0, OptionOps,
		Args, Result) :-
	map__set(OptionTable0, Flag, bool(yes), OptionTable1),
	process_options_2(Args0, OptionTable1, OptionOps, Args, Result).

process_option_2(string(_), OptName, Flag, Args0, OptionTable0, OptionOps,
			Args, Result) :-
	( Args0 = [Arg | Args1] ->
		map__set(OptionTable0, Flag, string(Arg), OptionTable1),
		process_options_2(Args1, OptionTable1, OptionOps, Args, Result)
	;
		Args = Args0,
		string__append_list(["option `", OptName,
			"' requires an argument"], ErrorMsg),
		Result = error(ErrorMsg)
	).

process_option_2(int(_), OptName, Flag, Args0, OptionTable0, OptionOps,
			Args, Result) :-
	( Args0 = [Arg | Args1] ->
		( string__to_int(Arg, IntArg) ->
			map__set(OptionTable0, Flag, int(IntArg), OptionTable1),
			process_options_2(Args1, OptionTable1, OptionOps,
				Args, Result)
		;
			Args = Args0,
			string__append_list(["option `", OptName,
				"' requires a numeric argument"], ErrorMsg),
			Result = error(ErrorMsg)
		)
	;
		Args = Args0,
		string__append_list(["option `", OptName,
			"' requires an argument"], ErrorMsg),
		Result = error(ErrorMsg)
	).

process_option_2(accumulating(List0), OptName, Flag, Args0, OptionTable0,
		OptionOps, Args, Result) :-
	( Args0 = [Arg | Args1] ->
		list__append(List0, [Arg], List),
		map__set(OptionTable0, Flag, accumulating(List), OptionTable1),
		process_options_2(Args1, OptionTable1, OptionOps, Args, Result)
	;
		Args = Args0,
		string__append_list(["option `", OptName,
			"' requires an argument"], ErrorMsg),
		Result = error(ErrorMsg)
	).

:- pred process_negated_bool_option(string, OptionType, list(string),
			option_table(OptionType), option_ops(OptionType),
			list(string), maybe_option_table(OptionType)).
:- mode process_negated_bool_option(in, in, in, in, in(option_ops), out, out)
	is det.

process_negated_bool_option(OptName, Flag, Args0, OptionTable0, OptionOps,
		Args, Result) :-
	map__lookup(OptionTable0, Flag, Data),
	( Data = bool(_) ->
		map__set(OptionTable0, Flag, bool(no), OptionTable1),
		process_options_2(Args0, OptionTable1, OptionOps, Args, Result)
	;
		Args = Args0,
		string__append_list(["cannot negate option `", OptName,
			"' -- only boolean options can be negated"], ErrorMsg),
		Result = error(ErrorMsg)
	).

%-----------------------------------------------------------------------------%

getopt__lookup_bool_option(OptionTable, Opt, Val) :-
	(
		map__lookup(OptionTable, Opt, bool(Val0))
	->
		Val = Val0
	;
		error("Expected bool option and didn't get one.")
	).

getopt__lookup_int_option(OptionTable, Opt, Val) :-
	(
		map__lookup(OptionTable, Opt, int(Val0))
	->
		Val = Val0
	;
		error("Expected int option and didn't get one.")
	).

getopt__lookup_string_option(OptionTable, Opt, Val) :-
	(
		map__lookup(OptionTable, Opt, string(Val0))
	->
		Val = Val0
	;
		error("Expected string option and didn't get one.")
	).

:- end_module getopt.

%-----------------------------------------------------------------------------%
