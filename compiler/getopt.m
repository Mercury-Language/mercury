%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module getopt.
:- interface.
:- import_module options, map, list, string.

	% getopt__process_options(Args, NonOptionArgs, Result)
	%
	%	Scans through 'Args' looking for options, places all the
	%	non-option arguments in 'NonOptionArgs', and record the
	%	options in the OptionTable.
	%	If an invalid option is encountered, we return error(Message)
	%	otherwise we return ok(OptionTable) in Result.
	%	OptionTable is a map from option to option_data.
	% 
	% 	This version allows both short (single-character) options
	% 	and GNU-style long options. It also has the GNU extension
	%	of recognizing options anywhere in the command-line, not
	%	just at the start.
	% 	Options are recognized using the short_option/3 and
	%	long_option/3 predicate provided in options.m.
	%
	%	TODO:
	%		check POSIX conformance.
	%		improve error messages.

:- type maybe_option_table	--->	ok(option_table)
				;	error(string).

:- pred getopt__process_options(list(string)::in, list(string)::out,
			maybe_option_table::out) is det.

:- implementation.
:- import_module std_util.

getopt__process_options(Args0, Args, Result) :-
	option_defaults(OptionDefaultsList),
	map__from_assoc_list(OptionDefaultsList, OptionTable0),
	process_options_2(Args0, OptionTable0, Args, Result).

% :- pred process_options_2(list(string)::in, option_table::in,
% 			list(string)::out, maybe_option_table::out) is det.
:- pred process_options_2(list(string), option_table,
			list(string), maybe_option_table).
:- mode process_options_2(in, in, out, out) is det.

process_options_2([], OptionTable, [], ok(OptionTable)).
process_options_2([Option | Args0], OptionTable0, Args, Result) :-
	( Option = "--" ->	% "--" terminates option processing
		Args = Args0,
		Result = ok(OptionTable0)
	; string__append("--no-", LongOption, Option) ->
		( long_option(LongOption, Flag) ->
			string__append("--", LongOption, OptName),
			process_negated_bool_option(OptName, Flag, Args0,
					OptionTable0, Args, Result)
		;
			string__append("unrecognized option `--no-",
					LongOption, Tmp),
			string__append(Tmp, "'", ErrorMsg),
			Result = error(ErrorMsg),
			Args = Args0
		)
	; string__append("--", LongOption, Option) ->
	  	( long_option(LongOption, Flag) ->
			process_option(Option, Flag, Args0, OptionTable0,
					Args, Result)
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
			( short_option(SingleShortOpt, Flag) ->
				process_option(Option, Flag, Args0,
					OptionTable0, Args, Result)
			;
				string__append("unrecognized option `",
					Option, Tmp),
				string__append(Tmp, "'", ErrorMsg),
				Result = error(ErrorMsg),
				Args = Args0
			)
		    % check for a single negated option `-x-'
		; ShortOptionsList = [SingleShortOpt, '-'] ->
			( short_option(SingleShortOpt, Flag) ->
				string__from_char_list(['-', SingleShortOpt],
						OptName),
				process_negated_bool_option(OptName, Flag,
					Args0, OptionTable0, Args, Result)
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
					OptionTable0, Args, Result)
		)
	;
	    % It's a normal non-option argument.
	    % As a GNU extension, keep searching for options
	    % in the remaining arguments.
		Args = [Option | Args1],
		process_options_2(Args0, OptionTable0, Args1, Result)
	).

:- pred process_short_option_list(list(character), list(string), option_table,
		list(string), maybe_option_table).
:- mode process_short_option_list(in, in, in, out, out) is det.

process_short_option_list([], Args0, OptionsTable0, Args, Result) :-
	process_options_2(Args0, OptionsTable0, Args, Result).

process_short_option_list([Opt | Opts], Args0, OptionsTable0, Args, Result) :-
	( short_option(Opt, Flag) ->
		process_short_option(Opt, Flag, Opts, Args0, OptionsTable0,
				Args, Result)
	;
		string__char_to_string(Opt, OptString),
		string__append("unrecognized option `-", OptString, Tmp),
		string__append(Tmp, "'", ErrorMessage),
		Result = error(ErrorMessage),
		Args = Args0
	).

:- pred process_short_option(character, option, list(character), list(string),
		option_table, list(string), maybe_option_table).
:- mode process_short_option(in, in, in, in, in, out, out) is det.

process_short_option(Opt, Flag, Opts, Args0, OptionTable0, Args, Result) :-
	map__lookup(OptionTable0, Flag, Data),
	process_short_option_2(Data, Opt, Flag, Opts, Args0, OptionTable0,
		Args, Result).

:- pred process_short_option_2(option_data, character, option, list(character),
		list(string), option_table, list(string), maybe_option_table).
:- mode process_short_option_2(in, in, in, in, in, in, out, out) is det.

process_short_option_2(bool(_), _Opt, Flag, Opts, Args0, OptionTable0, Args,
		Result) :-
	map__set(OptionTable0, Flag, bool(yes), OptionTable1),
	process_short_option_list(Opts, Args0, OptionTable1, Args, Result).
process_short_option_2(string(_), _Opt, Flag, Opts, Args0, OptionTable0, Args,
		Result) :-
	string__from_char_list(Opts, Arg),
	map__set(OptionTable0, Flag, string(Arg), OptionTable1),
	process_options_2(Args0, OptionTable1, Args, Result).
process_short_option_2(int(_), Opt, Flag, Opts, Args0, OptionTable0, Args,
		Result) :-
	string__from_char_list(Opts, Arg),
	( string__to_int(Arg, IntArg) ->
		map__set(OptionTable0, Flag, int(IntArg), OptionTable1),
		process_options_2(Args0, OptionTable1, Args, Result)
	;
		Args = Args0,
		string__char_to_string(Opt, Option),
		string__append_list(["option `-", Option,
			"' requires a numeric argument"], ErrorMsg),
		Result = error(ErrorMsg)
	).
process_short_option_2(accumulating(List0), _Opt, Flag, Opts, Args0,
		OptionTable0, Args, Result) :-
	string__from_char_list(Opts, Arg),
	list__append(List0, [Arg], List),
	map__set(OptionTable0, Flag, accumulating(List), OptionTable1),
	process_options_2(Args0, OptionTable1, Args, Result).

:- pred process_option(string, option, list(string), option_table,
			list(string), maybe_option_table).
:- mode process_option(in, in, in, in, out, out) is det.

process_option(OptName, Flag, Args0, OptionTable0, Args, Result) :-
	map__lookup(OptionTable0, Flag, Data),
	process_option_2(Data, OptName, Flag, Args0, OptionTable0,
		Args, Result).

:- pred process_option_2(option_data, string, option, list(string),
		 	option_table, list(string), maybe_option_table).
:- mode process_option_2(in, in, in, in, in, out, out) is det.

process_option_2(bool(_), _OptName, Flag, Args0, OptionTable0, Args, Result) :-
	map__set(OptionTable0, Flag, bool(yes), OptionTable1),
	process_options_2(Args0, OptionTable1, Args, Result).

process_option_2(string(_), OptName, Flag, Args0, OptionTable0, Args, Result) :-
	( Args0 = [Arg | Args1] ->
		map__set(OptionTable0, Flag, string(Arg), OptionTable1),
		process_options_2(Args1, OptionTable1, Args, Result)
	;
		Args = Args0,
		string__append_list(["option `", OptName,
			"' requires an argument"], ErrorMsg),
		Result = error(ErrorMsg)
	).

process_option_2(int(_), OptName, Flag, Args0, OptionTable0, Args, Result) :-
	( Args0 = [Arg | Args1] ->
		( string__to_int(Arg, IntArg) ->
			map__set(OptionTable0, Flag, int(IntArg), OptionTable1),
			process_options_2(Args1, OptionTable1, Args, Result)
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
		Args, Result) :-
	( Args0 = [Arg | Args1] ->
		list__append(List0, [Arg], List),
		map__set(OptionTable0, Flag, accumulating(List), OptionTable1),
		process_options_2(Args1, OptionTable1, Args, Result)
	;
		Args = Args0,
		string__append_list(["option `", OptName,
			"' requires an argument"], ErrorMsg),
		Result = error(ErrorMsg)
	).

:- pred process_negated_bool_option(string, option, list(string), option_table,
			list(string), maybe_option_table).
:- mode process_negated_bool_option(in, in, in, in, out, out) is det.

process_negated_bool_option(OptName, Flag, Args0, OptionTable0, Args, Result) :-
	map__lookup(OptionTable0, Flag, Data),
	( Data = bool(_) ->
		map__set(OptionTable0, Flag, bool(no), OptionTable1),
		process_options_2(Args0, OptionTable1, Args, Result)
	;
		Args = Args0,
		string__append_list(["cannot negate option `", OptName,
			"' -- only boolean options can be negated"], ErrorMsg),
		Result = error(ErrorMsg)
	).

:- end_module getopt.

%-----------------------------------------------------------------------------%
