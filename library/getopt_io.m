%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2011 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: getopt_io.m
% Authors: fjh, zs
% Stability: medium
%
% This module exports predicates for parsing command-line options.
% It is identical to the getopt.m module, except that it supports
% one more kind of option: file_special options (see below).
%
% These predicates allow both short (single-character) options,
% which are preceded on command lines with a single dash, and GNU-style
% long options, which are preceded on command lines with a double dash.
% An argument starting with a single dash can specify more than one
% short option, so that e.g. `-cde' is equivalent to `-c -d -e', while 
% each long option name must be in an argument of its own.
%
% The predicates in this module support the GNU extension of recognizing
% options anywhere in the command-line, not just at its start.
%
% To use this module:
%
% - You must provide an `option' type which is an enumeration of
%   all your different options.
% - You must provide predicates `short_option(Char, Option)' and
%   `long_option(String, Option)' which convert the short and long names
%   respectively for the option to this enumeration type.
%   (An option can have as many names as you like, long or short.)
% - You must provide a predicate `option_default(Option, OptionData)'
%   which specifies both the type and the default value for every option.
%
% You may optionally provide a predicate `special_handler(Option, SpecialData,
% OptionTable, MaybeOptionTable)' for handling special option types.
% (See below.)
%
% We support the following "simple" option types:
%
%   - bool
%   - int
%   - string
%   - maybe_int (which have a value of `no' or `yes(int)')
%   - maybe_string (which have a value of `no' or `yes(string)')
%
% We also support one "accumulating" option type:
%
%   - accumulating (which accumulates a list of strings)
%
% And the following "special" option types:
%
%   - special
%   - bool_special
%   - int_special
%   - string_special
%   - maybe_string_special
%   - file_special
%
% For the "simple" option types, if there are multiple occurrences of the same
% option on the command-line, then the last (right-most) occurrence will take
% precedence. For "accumulating" options, multiple occurrences will be
% appended together into a list.
%
% With the exception of file_special, the "special" option types are handled
% by a special option handler (see `special_handler' below), which may perform
% arbitrary modifications to the option_table. For example, an option which
% is not yet implemented could be handled by a special handler which produces
% an error report, or an option which is a synonym for a set of more
% "primitive" options could be handled by a special handler which sets those
% "primitive" options.
%
% The file_special option type requires no handler, and is implemented
% entirely by this module. It always takes a single argument, a file name.
% Its handling always consists of
% - reading the named file,
% - converting its contents into a sequence of words separated by white space,
%   and
% - interpreting those words as options in the usual manner.
%
% It is an error to use a "special" option other than file_special
% for which there is no handler, or for which the handler fails.
%
% Boolean (i.e. bool or bool_special), maybe_int, maybe_string
% and accumulating options can be negated. Negating an accumulating
% option empties the accumulated list of strings.
% Single-character options can be negated by following them
% with another `-', e.g. `-x-' will negate the `-x' option.
% Long options can be negated by preceding them with `--no-',
% e.g. `--no-foo' will negate the `--foo' option.
%
% Note that arguments following an option may be separated from the option by
% either whitespace or the equals character `=', so that e.g. `--foo 3' and
% `--foo=3' both specify the option `--foo' with the integer argument `3'.
%
% If the argument `--' is encountered on the command-line, then option
% processing will immediately terminate, without processing any remaining
% arguments. This is sometimes needed to tell a program to treat strings
% that start with a dash as non-option arguments.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module getopt_io.
:- interface.

:- import_module bool.
:- import_module char.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

    % The predicates below that process options, namely
    %
    %   - process_options
    %   - process_options_se
    %   - process_options_track
    %   - process_options_track_se
    %
    % all take an argument of the option_ops type to tell them
    %
    % - what the default value of each option is, and
    % - what the short and long names of the options are.
    %   (See the comment at the top for a description of the distinction.)
    %
    % The job of the option_ops type is to hold the three or four predicates
    % used to categorize a set of options. Their interfaces should be
    % like these:
    %
    %   % True if the character names a valid single-character short option.
    %   %
    % :- pred short_option(char::in, option::out) is semidet.
    %
    %   % True if the string names a valid long option.
    %   %
    % :- pred long_option(string::in, option::out) is semidet.
    %
    %   % Nondeterministically returns all the options with their
    %   % corresponding types and default values.
    %   %
    % :- pred option_default(option::out, option_data::out) is multi.
    %
    %   % This predicate is invoked whenever getopt finds an option
    %   % (long or short) designated as special, with special_data holding
    %   % the argument of the option (if any). The predicate can change the
    %   % option table in arbitrary ways in the course of handling the option,
    %   % or it can return an error message.
    %   % The canonical examples of special options are -O options setting
    %   % optimization levels in compilers, which set many other options
    %   % at once.
    %   %
    % :- pred special_handler(option::in, special_data::in,
    %   option_table::in, maybe_option_table(_)::out) is semidet.
    %
    % The four function symbols in the option_ops type differ in
    %
    % - whether they contain a special_handler or not, and
    % - whether the determinism of option_default is nondet or multi.
    %
:- type option_ops(OptionType)
    --->    option_ops(
                pred(char, OptionType),         % short_option
                pred(string, OptionType),       % long_option
                pred(OptionType, option_data)   % option_default
            )
    ;       option_ops(
                pred(char, OptionType),         % short_option
                pred(string, OptionType),       % long_option
                pred(OptionType, option_data),  % option_default
                pred(OptionType, special_data,  % special option handler
                    option_table(OptionType),
                    maybe_option_table(OptionType))
            )
    ;       option_ops_multi(
                pred(char, OptionType),         % short_option
                pred(string, OptionType),       % long_option
                pred(OptionType, option_data)   % option_default
            )
    ;       option_ops_multi(
                pred(char, OptionType),         % short_option
                pred(string, OptionType),       % long_option
                pred(OptionType, option_data),  % option_default
                pred(OptionType, special_data,  % special option handler
                    option_table(OptionType),
                    maybe_option_table(OptionType))
            ).

:- inst option_ops for option_ops/1
    --->    option_ops(
                pred(in, out) is semidet,               % short_option
                pred(in, out) is semidet,               % long_option
                pred(out, out) is nondet                % option_default
            )
    ;       option_ops_multi(
                pred(in, out) is semidet,               % short_option
                pred(in, out) is semidet,               % long_option
                pred(out, out) is multi                 % option_default
            )
    ;       option_ops(
                pred(in, out) is semidet,               % short_option
                pred(in, out) is semidet,               % long_option
                pred(out, out) is nondet,               % option_default
                pred(in, in, in, out) is semidet        % special handler
            )
    ;       option_ops_multi(
                pred(in, out) is semidet,               % short_option
                pred(in, out) is semidet,               % long_option
                pred(out, out) is multi,                % option_default
                pred(in, in, in, out) is semidet        % special handler
            ).

%---------------------%

    % A version of the option_ops type for the process_options_track
    % predicate and its process_options_track_se variant.
    % Unlike the option_ops type, it does not contain a predicate
    % for setting the initial default values of options, since
    % process_options_track expects that to be done separately.
    %
:- type option_ops_track(OptionType)
    --->    option_ops_track(
                pred(char, OptionType),         % short_option
                pred(string, OptionType),       % long_option
                pred(OptionType, special_data,  % special option handler
                    option_table(OptionType),
                    maybe_option_table(OptionType),
                    set(OptionType))
            ).

:- inst option_ops_track for option_ops_track/1
    --->    option_ops_track(
                pred(in, out) is semidet,               % short_option
                pred(in, out) is semidet,               % long_option
                pred(in, in, in, out, out) is semidet   % special handler
            ).

%---------------------%

:- type option_data
    --->    bool(bool)
    ;       int(int)
    ;       string(string)
    ;       maybe_int(maybe(int))
    ;       maybe_string(maybe(string))
    ;       accumulating(list(string))
    ;       special
    ;       bool_special
    ;       int_special
    ;       string_special
    ;       maybe_string_special
    ;       file_special.

:- type special_data
    --->    none
    ;       bool(bool)
    ;       int(int)
    ;       string(string)
    ;       maybe_string(maybe(string)).

:- type option_table(OptionType) == map(OptionType, option_data).

%---------------------%

:- type maybe_option_table(OptionType)
    --->    ok(option_table(OptionType))
    ;       error(string).

:- type maybe_option_table_se(OptionType)
    --->    ok(option_table(OptionType))
    ;       error(option_error(OptionType)).

%---------------------%

:- type option_error(OptionType)
    --->    unrecognized_option(string)
            % An option that is not recognized appeared on the command line.
            % The argument gives the option as it appeared on the command line.

    ;       option_error(OptionType, string, option_error_reason).
            % An error occurred with a specific option. The first  argument
            % identifies the option enumeration value; the second identifies
            % the string that appeared on the command line for that option;
            % the third argument describes the nature of the error with that
            % option.

:- type option_error_reason
    --->    unknown_type
            % No type for this option has been specified in the
            % `option_default'/2 predicate.

    ;       requires_argument
            % The option requires an argument but it occurred on the command
            % line without one.

    ;       does_not_allow_argument(string)
            % The option does not allow an argument but it was provided with
            % one on the command line.
            % The argument gives the contents of the argument position on the
            % command line.

    ;       cannot_negate
            % The option cannot be negated but its negated form appeared on the
            % command line.

    ;       special_handler_failed
            % The special option handler predicate for the option failed.

    ;       special_handler_missing
            % A special option handler predicate was not provided
            % for the option.

    ;       special_handler_error(string)
            % The special option handler predicate for the option returned an
            % error.
            % The argument is a string describing the error.

    ;       requires_numeric_argument(string)
            % The option requires a numeric argument but it occurred on the
            % command line with a non-numeric argument.
            % The argument gives the contents of the argument position on the
            % command line.

    ;       file_special_cannot_open(string, io.error)
            % The option is a file_special option whose argument is the file
            % named by the first argument.
            % Attempting to open this file resulted in the I/O error given
            % by the second argument.

    ;       file_special_cannot_read(string, io.error)
            % The option is a file_special option whose argument is the file
            % named by the first argument.
            % Attempting to read from this file resulted in the I/O error given
            % by the second argument.

    ;       file_special_contains_non_option_args(string).
            % The option is a file_special option whose argument is the file
            % named by the argument. This file contained some non-option
            % arguments.

%---------------------------------------------------------------------------%

    % process_options(OptionOps, Args, NonOptionArgs, Result, !IO):
    % process_options(OptionOps, Args, OptionArgs, NonOptionArgs, Result, !IO):
    %
    % Scans through 'Args' looking for options. Places all the option arguments
    % in `OptionArgs', places all the non-option arguments in 'NonOptionArgs',
    % and records the options in the `OptionTable'. `OptionTable' is a map
    % from a user-defined option type to option_data.
    %
    % If an invalid option is encountered, we return `error(Message)',
    % otherwise we return `ok(OptionTable)' in 'Result'.
    %
    % The structure of the `OptionOps' argument is documented above.
    %
    % The two different arity versions differ only in that the lower arity
    % version does not return the arguments that contained the options.
    % While some callers will want those arguments, other callers will not,
    % considering that the only information they want from them is that
    % contained in the option table.
    %
:- pred process_options(option_ops(OptionType)::in(option_ops),
    list(string)::in, list(string)::out,
    maybe_option_table(OptionType)::out, io::di, io::uo) is det.
:- pred process_options(option_ops(OptionType)::in(option_ops),
    list(string)::in, list(string)::out, list(string)::out,
    maybe_option_table(OptionType)::out, io::di, io::uo) is det.

    % process_options_track(OptionOps, Args, OptionArgs, NonOptionArgs,
    %   OptionTable0, Result, OptionsSet, !IO):
    %
    % This predicate differs from the two variants of process_options
    % above in only two respects.
    %
    % First, it expects the caller to supply an argument containing
    % the initial contents of the option table, instead of calling
    % the initialization predicate itself. The point of this is that
    % it allows the option table to be initialized once (using either
    % init_option_table or init_option_table_multi below), and then 
    % process_options_track to be called several times with different
    % sets of arguments, perhaps obtained from different sources
    % (command line, configuration file, etc).
    %
    % Second, each call to process_options_track returns the set of options
    % that were set by that call. This helps with the same objective;
    % for example, it can tell the caller whether an option was set
    % from a configuration file, the command line, both, or neither.
    %
:- pred process_options_track(
    option_ops_track(OptionType)::in(option_ops_track),
    list(string)::in, list(string)::out, list(string)::out,
    option_table(OptionType)::in,
    maybe_option_table(OptionType)::out, set(OptionType)::out,
    io::di, io::uo) is det.

    % Variants of the above that return structured errors. These behave
    % as the above versions, except that they return any error values
    % as members of the option_error/1 type, rather than as strings.
    %
:- pred process_options_se(option_ops(OptionType)::in(option_ops),
    list(string)::in, list(string)::out,
    maybe_option_table_se(OptionType)::out, io::di, io::uo) is det.
:- pred process_options_se(option_ops(OptionType)::in(option_ops),
    list(string)::in, list(string)::out, list(string)::out,
    maybe_option_table_se(OptionType)::out, io::di, io::uo) is det.
:- pred process_options_track_se(
    option_ops_track(OptionType)::in(option_ops_track),
    list(string)::in, list(string)::out, list(string)::out,
    option_table(OptionType)::in, maybe_option_table_se(OptionType)::out,
    set(OptionType)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % init_option_table(InitPred, OptionTable):
    % init_option_table_multi(InitPred, OptionTable):
    %
    % Create an initial option table that maps each option to the default
    % value specified for it by InitPred.
    %
:- pred init_option_table(
    pred(OptionType, option_data)::in(pred(out, out) is nondet),
    option_table(OptionType)::out) is det.
:- pred init_option_table_multi(
    pred(OptionType, option_data)::in(pred(out, out) is multi),
    option_table(OptionType)::out) is det.

%---------------------------------------------------------------------------%

    % The following predicates search the option table for an option
    % of the specified kind. If the option is not in the table,
    % they throw an exception.

:- func lookup_bool_option(option_table(Option), Option) = bool.
:- pred lookup_bool_option(option_table(Option)::in, Option::in,
    bool::out) is det.

:- func lookup_int_option(option_table(Option), Option) = int.
:- pred lookup_int_option(option_table(Option)::in, Option::in,
    int::out) is det.

:- func lookup_string_option(option_table(Option), Option) = string.
:- pred lookup_string_option(option_table(Option)::in, Option::in,
    string::out) is det.

:- func lookup_maybe_int_option(option_table(Option), Option) =
    maybe(int).
:- pred lookup_maybe_int_option(option_table(Option)::in, Option::in,
    maybe(int)::out) is det.

:- func lookup_maybe_string_option(option_table(Option), Option) =
    maybe(string).
:- pred lookup_maybe_string_option(option_table(Option)::in,
    Option::in, maybe(string)::out) is det.

:- func lookup_accumulating_option(option_table(Option), Option) =
    list(string).
:- pred lookup_accumulating_option(option_table(Option)::in,
    Option::in, list(string)::out) is det.

%---------------------------------------------------------------------------%

:- func option_error_to_string(option_error(OptionType)) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module require.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

% Please keep the differences between this module and getopt.m to the
% minimum. Most changes should done in both modules.

:- type option_ops_special(OptionType)
    --->    none
    ;       notrack(
                pred(OptionType, special_data,
                    option_table(OptionType),
                    maybe_option_table(OptionType))
            )
    ;       track(
                pred(OptionType, special_data,
                    option_table(OptionType),
                    maybe_option_table(OptionType),
                    set(OptionType))
            ).

:- inst option_ops_special for option_ops_special/1
    --->    none
    ;       notrack(pred(in, in, in, out) is semidet)
    ;       track(pred(in, in, in, out, out) is semidet).

:- type option_ops_internal(OptionType)
    --->    option_ops_internal(
                short_option    :: pred(char, OptionType),
                long_option     :: pred(string, OptionType),
                special_handler :: option_ops_special(OptionType)
            ).

:- inst option_ops_internal for option_ops_internal/1
    --->    option_ops_internal(
                pred(in, out) is semidet,           % short_option
                pred(in, out) is semidet,           % long_option
                option_ops_special                  % special handler, if any
            ).

%---------------------------------------------------------------------------%

process_options(OptionOps, Args0, NonOptionArgs, Result, !IO) :-
    process_options_se(OptionOps, Args0, NonOptionArgs, Result0, !IO),
    (
        Result0 = ok(OptionTable),
        Result = ok(OptionTable)
    ;
        Result0 = error(Error),
        Msg = option_error_to_string(Error),
        Result = error(Msg)
    ).

process_options(OptionOps, Args0, OptionArgs, NonOptionArgs, Result, !IO) :-
    process_options_se(OptionOps, Args0, OptionArgs, NonOptionArgs, Result0,
        !IO),
    (
        Result0 = ok(OptionTable),
        Result = ok(OptionTable)
    ;
        Result0 = error(Error),
        Msg = option_error_to_string(Error),
        Result = error(Msg)
    ).

process_options_track(OptionOps, Args0, OptionArgs, NonOptionArgs,
        OptionTable0, Result, OptionsSet, !IO) :-
    process_options_track_se(OptionOps, Args0, OptionArgs, NonOptionArgs,
        OptionTable0, Result0, OptionsSet, !IO),
    (
        Result0 = ok(OptionTable),
        Result = ok(OptionTable)
    ;
        Result0 = error(Error),
        Msg = option_error_to_string(Error),
        Result = error(Msg)
    ).

%---------------------------------------------------------------------------%

process_options_se(OptionOps, Args0, NonOptionArgs, Result, !IO) :-
    process_options_se(OptionOps, Args0, _OptionArgs, NonOptionArgs,
        Result, !IO).

process_options_se(OptionOps, Args0, OptionArgs, NonOptionArgs,
        Result, !IO) :-
    (
        OptionOps = option_ops(Short, Long, Defaults),
        MaybeSpecial = none,
        init_option_table(Defaults, OptionTable0)
    ;
        OptionOps = option_ops(Short, Long, Defaults, Special),
        MaybeSpecial = notrack(Special),
        init_option_table(Defaults, OptionTable0)
    ;
        OptionOps = option_ops_multi(Short, Long, Defaults),
        MaybeSpecial = none,
        init_option_table_multi(Defaults, OptionTable0)
    ;
        OptionOps = option_ops_multi(Short, Long, Defaults, Special),
        MaybeSpecial = notrack(Special),
        init_option_table_multi(Defaults, OptionTable0)
    ),
    Internal = option_ops_internal(Short, Long, MaybeSpecial),
    process_arguments(Args0, NonOptionArgs, Internal,
        [], RevOptionArgs, OptionTable0, Result, set.init, _OptionsSet, !IO),
    OptionArgs = list.reverse(RevOptionArgs).

process_options_track_se(OptionOps, Args0, OptionArgs, NonOptionArgs,
        OptionTable0, Result, OptionsSet, !IO) :-
    OptionOps = option_ops_track(Short, Long, Special),
    Internal = option_ops_internal(Short, Long, track(Special)),
    process_arguments(Args0, NonOptionArgs, Internal,
        [], RevOptionArgs, OptionTable0, Result, set.init, OptionsSet, !IO),
    OptionArgs = list.reverse(RevOptionArgs).

%---------------------------------------------------------------------------%

init_option_table(OptionDefaultsPred, OptionTable) :-
    solutions(
        ( pred(OptionDataPair::out) is nondet :-
            OptionDefaultsPred(Option, OptionData),
            OptionDataPair = Option - OptionData
        ), OptionDefaultsList),
    map.from_sorted_assoc_list(OptionDefaultsList, OptionTable).

init_option_table_multi(OptionDefaultsPred, OptionTable) :-
    solutions(
        ( pred(OptionDataPair::out) is multi :-
            OptionDefaultsPred(Option, OptionData),
            OptionDataPair = Option - OptionData
        ), OptionDefaultsList),
    map.from_sorted_assoc_list(OptionDefaultsList, OptionTable).

%---------------------------------------------------------------------------%

:- pred process_arguments(list(string)::in, list(string)::out,
    option_ops_internal(OptionType)::in(option_ops_internal), list(string)::in,
    list(string)::out, option_table(OptionType)::in,
    maybe_option_table_se(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out, io::di, io::uo) is det.

process_arguments([], [], _, OptionArgs, OptionArgs,
        OptionTable, ok(OptionTable), !OptionsSet, !IO).
process_arguments([Option | Args0], Args, OptionOps, OptionArgs0, OptionArgs,
        OptionTable0, Result, !OptionsSet, !IO) :-
    ( if
        Option = "--"
    then
        % "--" terminates option processing
        OptionArgs = OptionArgs0,
        Args = Args0,
        Result = ok(OptionTable0)
    else if
        string.append("--no-", LongOption, Option)
    then
        LongOptionPred = OptionOps ^ long_option,
        ( if LongOptionPred(LongOption, Flag) then
            string.append("--", LongOption, OptName),
            process_negated_option(OptName, Flag, OptionOps,
                OptionTable0, Result1, !OptionsSet),
            (
                Result1 = ok(OptionTable1),
                process_arguments(Args0, Args, OptionOps,
                    [Option | OptionArgs0], OptionArgs, OptionTable1, Result,
                    !OptionsSet, !IO)
            ;
                Result1 = error(_),
                Result = Result1,
                OptionArgs = OptionArgs0,
                Args = Args0
            )
        else
            Error = unrecognized_option(Option),
            Result = error(Error),
            OptionArgs = OptionArgs0,
            Args = Args0
        )
    else if
        string.append("--", LongOptionStr, Option)
    then
        LongOptionPred = OptionOps ^ long_option,
        ( if string.sub_string_search(LongOptionStr, "=", OptionLen) then
            string.split(LongOptionStr, OptionLen, LongOption,
                EqualOptionArg),
            ( if string.first_char(EqualOptionArg, '=', OptionArg) then
                MaybeArg = yes(OptionArg)
            else
                error("bad split of --longoption=arg")
            )
        else
            LongOption = LongOptionStr,
            MaybeArg = no
        ),
        OptionName = "--" ++ LongOption,
        ( if LongOptionPred(LongOption, Flag) then
            ( if map.search(OptionTable0, Flag, OptionData) then
                handle_long_option(OptionName, Flag, OptionData,
                    MaybeArg, Args0, Args, OptionOps,
                    [Option | OptionArgs0], OptionArgs,
                    OptionTable0, Result, !OptionsSet, !IO)
            else
                Error = option_error(Flag, Option, unknown_type),
                Result = error(Error),
                OptionArgs = OptionArgs0,
                Args = Args0
            )
        else
            Error = unrecognized_option(OptionName),
            Result = error(Error),
            OptionArgs = OptionArgs0,
            Args = Args0
        )
    else if
        string.first_char(Option, '-', ShortOptions),
        ShortOptions \= ""
    then
        string.to_char_list(ShortOptions, ShortOptionsList),
        % Process a single negated option `-x-'.
        ( if ShortOptionsList = [SingleShortOpt, '-'] then
            ShortOptionPred = OptionOps ^ short_option,
            ( if ShortOptionPred(SingleShortOpt, Flag) then
                string.from_char_list(['-', SingleShortOpt], OptName),
                process_negated_option(OptName, Flag, OptionOps,
                    OptionTable0, Result1, !OptionsSet),
                (
                    Result1 = ok(OptionTable1),
                    process_arguments(Args0, Args, OptionOps,
                        [Option | OptionArgs0], OptionArgs,
                        OptionTable1, Result, !OptionsSet, !IO)
                ;
                    Result1 = error(_),
                    Result = Result1,
                    OptionArgs = OptionArgs0,
                    Args = Args0
                )
            else
                Error = unrecognized_option("-" ++ ShortOptions),
                Result = error(Error),
                OptionArgs = OptionArgs0,
                Args = Args0
            )
        else
            % Process a list of options `-xyz'.
            % -xyz may be several boolean options
            % or part of it may be the argument of an option.
            % The first element of Args0 may also be an argument
            % of an option.
            handle_short_options(ShortOptionsList, OptionOps,
                Args0, Args1, [Option | OptionArgs0], OptionArgs1,
                OptionTable0, Result1, !OptionsSet, !IO),
            (
                Result1 = ok(OptionTable1),
                process_arguments(Args1, Args, OptionOps,
                    OptionArgs1, OptionArgs, OptionTable1, Result, !OptionsSet,
                    !IO)
            ;
                Result1 = error(_),
                Result = Result1,
                OptionArgs = OptionArgs1,
                Args = Args0
            )
        )
    else
        % It is a normal non-option argument.
        % As a GNU extension, keep searching for options
        % in the remaining arguments.
        process_arguments(Args0, Args1, OptionOps,
            OptionArgs0, OptionArgs, OptionTable0, Result, !OptionsSet, !IO),
        Args = [Option | Args1]
    ).

:- pred handle_long_option(string::in,
    OptionType::in, option_data::in,
    maybe(string)::in, list(string)::in, list(string)::out,
    option_ops_internal(OptionType)::in(option_ops_internal), list(string)::in,
    list(string)::out, option_table(OptionType)::in,
    maybe_option_table_se(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out, io::di, io::uo) is det.

handle_long_option(Option, Flag, OptionData, MaybeOptionArg0,
        Args0, Args, OptionOps, OptionArgs0, OptionArgs, OptionTable0, Result,
        !OptionsSet, !IO) :-
    ( if
        need_arg(OptionData, yes),
        MaybeOptionArg0 = no
    then
        (
            Args0 = [Arg | ArgsTail],
            MaybeOptionArg = yes(Arg),
            Args1 = ArgsTail,
            MissingArg = no,
            OptionArgs1 = [Arg | OptionArgs0]
        ;
            Args0 = [],
            MaybeOptionArg = no,
            Args1 = Args0,
            OptionArgs1 = OptionArgs0,
            MissingArg = yes
        )
    else
        MaybeOptionArg = MaybeOptionArg0,
        Args1 = Args0,
        OptionArgs1 = OptionArgs0,
        MissingArg = no
    ),
    (
        MissingArg = yes,
        Args = Args0,
        OptionArgs = OptionArgs1,
        Error = option_error(Flag, Option, requires_argument),
        Result = error(Error)
    ;
        MissingArg = no,
        ( if
            need_arg(OptionData, no),
            MaybeOptionArg = yes(ArgVal)
        then
            Args = Args0,
            OptionArgs = OptionArgs1,
            Error = option_error(Flag, Option,
                does_not_allow_argument(ArgVal)),
            Result = error(Error)
        else
            process_option(OptionData, Option, Flag, MaybeOptionArg,
                OptionOps, OptionTable0, Result1, !OptionsSet, !IO),
            (
                Result1 = ok(OptionTable1),
                process_arguments(Args1, Args, OptionOps,
                    OptionArgs1, OptionArgs, OptionTable1, Result, !OptionsSet,
                    !IO)
            ;
                Result1 = error(_),
                Result = Result1,
                OptionArgs = OptionArgs1,
                Args = Args1
            )
        )
    ).

:- pred handle_short_options(list(char)::in,
    option_ops_internal(OptionType)::in(option_ops_internal), list(string)::in,
    list(string)::out, list(string)::in, list(string)::out,
    option_table(OptionType)::in,
    maybe_option_table_se(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out, io::di, io::uo) is det.

handle_short_options([], _, Args, Args, OptionArgs, OptionArgs,
        OptionTable, ok(OptionTable), !OptionsSet, !IO).
handle_short_options([Opt | Opts0], OptionOps, Args0, Args,
        OptionArgs0, OptionArgs, OptionTable0, Result, !OptionsSet, !IO) :-
    ShortOptionPred = OptionOps ^ short_option,
    ( if ShortOptionPred(Opt, Flag) then
        ( if map.search(OptionTable0, Flag, OptionData) then
            ( if need_arg(OptionData, yes) then
                get_short_option_arg(Opts0, Arg, Args0, Args1,
                    OptionArgs0, OptionArgs1),
                MaybeOptionArg = yes(Arg),
                Opts1 = []
            else
                MaybeOptionArg = no,
                Opts1 = Opts0,
                OptionArgs1 = OptionArgs0,
                Args1 = Args0
            ),
            string.from_char_list(['-', Opt], Option),
            process_option(OptionData, Option, Flag, MaybeOptionArg,
                OptionOps, OptionTable0, Result1, !OptionsSet, !IO),
            (
                Result1 = ok(OptionTable1),
                handle_short_options(Opts1, OptionOps, Args1, Args,
                    OptionArgs1, OptionArgs, OptionTable1, Result, !OptionsSet,
                    !IO)
            ;
                Result1 = error(_),
                Result = Result1,
                OptionArgs = OptionArgs1,
                Args = Args1
            )
        else
            string.char_to_string(Opt, OptString),
            Error = option_error(Flag, "-" ++ OptString, unknown_type),
            Result = error(Error),
            OptionArgs = OptionArgs0,
            Args = Args0
        )
    else
        string.char_to_string(Opt, OptString),
        Error = unrecognized_option("-" ++ OptString),
        Result = error(Error),
        OptionArgs = OptionArgs0,
        Args = Args0
    ).

:- pred get_short_option_arg(list(char)::in, string::out,
    list(string)::in, list(string)::out, list(string)::in, list(string)::out)
    is det.

get_short_option_arg(Opts, Arg, Args0, Args,
        OptionArgs0, OptionArgs) :-
    ( if
        Opts = [],
        Args0 = [ArgPrime | ArgsPrime]
    then
        OptionArgs = [ArgPrime | OptionArgs0],
        Arg = ArgPrime,
        Args = ArgsPrime
    else
        string.from_char_list(Opts, Arg),
        OptionArgs = OptionArgs0,
        Args = Args0
    ).

:- pred process_option(option_data::in, string::in, OptionType::in,
    maybe(string)::in,
    option_ops_internal(OptionType)::in(option_ops_internal),
    option_table(OptionType)::in, maybe_option_table_se(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out, io::di, io::uo) is det.

process_option(bool(_), _Option, Flag, MaybeArg, _OptionOps,
        !.OptionTable, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(_Arg),
        map.set(Flag, bool(no), !OptionTable),
        Result = ok(!.OptionTable)
    ;
        MaybeArg = no,
        map.set(Flag, bool(yes), !OptionTable),
        Result = ok(!.OptionTable)
    ).
process_option(int(_), Option, Flag, MaybeArg, _OptionOps,
        !.OptionTable, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(Arg),
        ( if string.to_int(Arg, IntArg) then
            map.set(Flag, int(IntArg), !OptionTable),
            Result = ok(!.OptionTable)
        else
            numeric_argument(Flag, Option, Arg, Result)
        )
    ;
        MaybeArg = no,
        error("integer argument expected in getopt_io.process_option")
    ).
process_option(string(_), _Option, Flag, MaybeArg, _OptionOps,
        !.OptionTable, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(Arg),
        map.set(Flag, string(Arg), !OptionTable),
        Result = ok(!.OptionTable)
    ;
        MaybeArg = no,
        error("string argument expected in getopt_io.process_option")
    ).
process_option(maybe_int(_), Option, Flag, MaybeArg, _OptionOps,
        !.OptionTable, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(Arg),
        ( if string.to_int(Arg, IntArg) then
            map.set(Flag, maybe_int(yes(IntArg)), !OptionTable),
            Result = ok(!.OptionTable)
        else
            numeric_argument(Flag, Option, Arg, Result)
        )
    ;
        MaybeArg = no,
        error("integer argument expected in getopt_io.process_option")
    ).
process_option(maybe_string(_), _Option, Flag, MaybeArg, _OptionOps,
        !.OptionTable, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(Arg),
        map.set(Flag, maybe_string(yes(Arg)), !OptionTable),
        Result = ok(!.OptionTable)
    ;
        MaybeArg = no,
        error("string argument expected in getopt_io.process_option")
    ).
process_option(accumulating(List0), _Option, Flag, MaybeArg, _OptionOps,
        !.OptionTable, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(Arg),
        list.append(List0, [Arg], List),
        map.set(Flag, accumulating(List), !OptionTable),
        Result = ok(!.OptionTable)
    ;
        MaybeArg = no,
        error("acumulating argument expected in getopt_io.process_option")
    ).
process_option(special, Option, Flag, MaybeArg, OptionOps,
        OptionTable0, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(_Arg),
        error("no special argument expected in getopt_io.process_option")
    ;
        MaybeArg = no,
        process_special_option(Option, Flag, none,
            OptionOps, OptionTable0, Result, !OptionsSet)
    ).
process_option(bool_special, Option, Flag, MaybeArg, OptionOps,
        OptionTable0, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(_Arg),
        process_special_option(Option, Flag, bool(no),
            OptionOps, OptionTable0, Result, !OptionsSet)
    ;
        MaybeArg = no,
        process_special_option(Option, Flag, bool(yes),
            OptionOps, OptionTable0, Result, !OptionsSet)
    ).
process_option(int_special, Option, Flag, MaybeArg, OptionOps,
        OptionTable0, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(Arg),
        ( if string.to_int(Arg, IntArg) then
            process_special_option(Option, Flag, int(IntArg),
                OptionOps, OptionTable0, Result, !OptionsSet)
        else
            numeric_argument(Flag, Option, Arg, Result)
        )
    ;
        MaybeArg = no,
        error("int_special argument expected in getopt_io.process_option")
    ).
process_option(string_special, Option, Flag, MaybeArg, OptionOps,
        OptionTable0, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(Arg),
        process_special_option(Option, Flag, string(Arg),
            OptionOps, OptionTable0, Result, !OptionsSet)
    ;
        MaybeArg = no,
        error("string_special argument expected in getopt_io.process_option")
    ).
process_option(maybe_string_special, Option, Flag, MaybeArg,
        OptionOps, OptionTable0, Result, !OptionsSet, !IO) :-
    (
        MaybeArg = yes(_Arg),
        process_special_option(Option, Flag, maybe_string(MaybeArg),
            OptionOps, OptionTable0, Result, !OptionsSet)
    ;
        MaybeArg = no,
        error("maybe_string_special argument expected " ++
            "in getopt_io.process_option")
    ).
process_option(file_special, Option, Flag, MaybeArg, OptionOps,
        OptionTable0, Result, !OptionsSet, !IO) :-
    (
        MaybeArg = yes(FileName),
        io.open_input(FileName, OpenRes, !IO),
        (
            OpenRes = ok(FileStream),
            io.read_file_as_string(FileStream, ReadRes, !IO),
            (
                ReadRes = ok(Contents),
                Words = string.words(Contents),
                process_arguments(Words, Args, OptionOps,
                    [], _OptionArgs, OptionTable0, Result0, !OptionsSet, !IO),
                (
                    Args = [],
                    Result = Result0
                ;
                    Args = [_ | _],
                    Reason = file_special_contains_non_option_args(FileName),
                    Error = option_error(Flag, Option, Reason),
                    Result = error(Error)
                )
            ;
                ReadRes = error(_, IO_Error),
                Reason = file_special_cannot_read(FileName, IO_Error),
                Error = option_error(Flag, Option, Reason),
                Result = error(Error)
            ),
            io.close_input(FileStream, !IO)
        ;
            OpenRes = error(IO_Error),
            Reason = file_special_cannot_open(FileName, IO_Error),
            Error = option_error(Flag, Option, Reason),
            Result = error(Error)
        )
    ;
        MaybeArg = no,
        error("file_special argument expected in getopt_io.process_option")
    ).

:- pred process_negated_option(string::in, OptionType::in,
    option_ops_internal(OptionType)::in(option_ops_internal),
    option_table(OptionType)::in, maybe_option_table_se(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out) is det.

process_negated_option(Option, Flag, OptionOps, OptionTable0, Result,
        !OptionsSet) :-
    ( if map.search(OptionTable0, Flag, OptionData) then
        (
            OptionData = bool(_),
            set.insert(Flag, !OptionsSet),
            map.set(Flag, bool(no), OptionTable0, OptionTable),
            Result = ok(OptionTable)
        ;
            OptionData = maybe_int(_),
            set.insert(Flag, !OptionsSet),
            map.set(Flag, maybe_int(no), OptionTable0, OptionTable),
            Result = ok(OptionTable)
        ;
            OptionData = maybe_string(_),
            set.insert(Flag, !OptionsSet),
            map.set(Flag, maybe_string(no), OptionTable0, OptionTable),
            Result = ok(OptionTable)
        ;
            OptionData = accumulating(_),
            set.insert(Flag, !OptionsSet),
            map.set(Flag, accumulating([]), OptionTable0, OptionTable),
            Result = ok(OptionTable)
        ;
            OptionData = bool_special,
            set.insert(Flag, !OptionsSet),
            process_special_option(Option, Flag, bool(no),
                OptionOps, OptionTable0, Result, !OptionsSet)
        ;
            OptionData = maybe_string_special,
            set.insert(Flag, !OptionsSet),
            process_special_option(Option, Flag, maybe_string(no),
                OptionOps, OptionTable0, Result, !OptionsSet)
        ;
            ( OptionData = int_special
            ; OptionData = string_special
            ; OptionData = int(_)
            ; OptionData = string(_)
            ; OptionData = special
            ; OptionData = file_special
            ),
            Error = option_error(Flag, Option, cannot_negate),
            Result = error(Error)
        )
    else
        Error = option_error(Flag, Option, unknown_type),
        Result = error(Error)
    ).

:- pred process_special_option(string::in, OptionType::in, special_data::in,
    option_ops_internal(OptionType)::in(option_ops_internal),
    option_table(OptionType)::in, maybe_option_table_se(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out) is det.

process_special_option(Option, Flag, OptionData, OptionOps,
        OptionTable0, Result, !OptionsSet) :-
    MaybeHandler = OptionOps ^ special_handler,
    (
        MaybeHandler = notrack(Handler),
        ( if
            Handler(Flag, OptionData, OptionTable0, Result0)
        then
            (
                Result0 = ok(OptionTable),
                Result = ok(OptionTable)
            ;
                Result0 = error(HandlerMsg),
                Reason = special_handler_error(HandlerMsg),
                Error = option_error(Flag, Option, Reason),
                Result = error(Error)
            )
        else
            Error = option_error(Flag, Option, special_handler_failed),
            Result = error(Error)
        )
    ;
        MaybeHandler = track(TrackHandler),
        ( if
            TrackHandler(Flag, OptionData, OptionTable0, Result0,
                NewOptionsSet)
        then
            set.union(NewOptionsSet, !OptionsSet),
            (
                Result0 = ok(OptionTable),
                Result = ok(OptionTable)
            ;
                Result0 = error(TrackHandlerMsg),
                Reason = special_handler_error(TrackHandlerMsg),
                Error = option_error(Flag, Option, Reason),
                Result = error(Error)
            )
        else
            Error = option_error(Flag, Option, special_handler_failed),
            Result = error(Error)
        )
    ;
        MaybeHandler = none,
        Error = option_error(Flag, Option, special_handler_missing),
        Result = error(Error)
    ).

%---------------------------------------------------------------------------%

:- pred need_arg(option_data::in, bool::out) is det.

need_arg(bool(_), no).
need_arg(int(_), yes).
need_arg(string(_), yes).
need_arg(maybe_int(_), yes).
need_arg(maybe_string(_), yes).
need_arg(accumulating(_), yes).
need_arg(special, no).
need_arg(bool_special, no).
need_arg(int_special, yes).
need_arg(string_special, yes).
need_arg(maybe_string_special, yes).
need_arg(file_special, yes).

:- pred numeric_argument(OptionType::in, string::in, string::in,
    maybe_option_table_se(OptionType)::out) is det.

numeric_argument(Flag, Option, Arg, Result) :-
    Reason = requires_numeric_argument(Arg),
    Error = option_error(Flag, Option, Reason),
    Result = error(Error).

%---------------------------------------------------------------------------%

lookup_bool_option(OptionTable, Opt) = Bool :-
    lookup_bool_option(OptionTable, Opt, Bool).

lookup_bool_option(OptionTable, Opt, Bool) :-
    ( if map.lookup(OptionTable, Opt, bool(BoolPrime)) then
        Bool = BoolPrime
    else
        error("Expected bool option and didn't get one.")
    ).

lookup_int_option(OptionTable, Opt) = Int :-
    lookup_int_option(OptionTable, Opt, Int).

lookup_int_option(OptionTable, Opt, Int) :-
    ( if map.lookup(OptionTable, Opt, int(IntPrime)) then
        Int = IntPrime
    else
        error("Expected int option and didn't get one.")
    ).

lookup_string_option(OptionTable, Opt) = Str :-
    lookup_string_option(OptionTable, Opt, Str).

lookup_string_option(OptionTable, Opt, Str) :-
    ( if map.lookup(OptionTable, Opt, string(StrPrime)) then
        Str = StrPrime
    else
        error("Expected string option and didn't get one.")
    ).

lookup_maybe_int_option(OptionTable, Opt) = MaybeInt :-
    lookup_maybe_int_option(OptionTable, Opt, MaybeInt).

lookup_maybe_int_option(OptionTable, Opt, MaybeInt) :-
    ( if map.lookup(OptionTable, Opt, maybe_int(MaybeIntPrime)) then
        MaybeInt = MaybeIntPrime
    else
        error("Expected maybe_int option and didn't get one.")
    ).

lookup_maybe_string_option(OptionTable, Opt) = MaybeString :-
    lookup_maybe_string_option(OptionTable, Opt, MaybeString).

lookup_maybe_string_option(OptionTable, Opt, MaybeString) :-
    ( if map.lookup(OptionTable, Opt, maybe_string(MaybeStringPrime)) then
        MaybeString = MaybeStringPrime
    else
        error("Expected maybe_string option and didn't get one.")
    ).

lookup_accumulating_option(OptionTable, Opt) = Acc :-
    lookup_accumulating_option(OptionTable, Opt, Acc).

lookup_accumulating_option(OptionTable, Opt, Acc) :-
    ( if map.lookup(OptionTable, Opt, accumulating(AccPrime)) then
        Acc = AccPrime
    else
        error("Expected accumulating option and didn't get one.")
    ).

%---------------------------------------------------------------------------%

option_error_to_string(Error) = String :-
    (
        Error = unrecognized_option(OptionName),
        string.format("unrecognized option `%s'", [s(OptionName)], String)
    ;
        Error = option_error(_, OptionName, Reason),
        (
            Reason = unknown_type,
            string.format("unknown type for option `%s'",
                [s(OptionName)], String)
        ;
            Reason = requires_argument,
            string.format("option `%s' needs an argument",
                [s(OptionName)], String)
        ;
            Reason = does_not_allow_argument(_),
            string.format("option `%s' does not allow an argument",
                [s(OptionName)], String)
        ;
            Reason = cannot_negate,
            string.format("cannot negate option `%s' -- " ++
                "only boolean, maybe and accumulating options can be negated",
                [s(OptionName)], String)
        ;
            Reason = special_handler_failed,
            string.format("the handler of option `%s' failed",
                [s(OptionName)], String)
        ;
            Reason = special_handler_missing,
            string.format("option `%s' has no handler",
                [s(OptionName)], String)
        ;
            Reason = special_handler_error(String)
        ;
            Reason = requires_numeric_argument(Arg),
            string.format(
                "option `%s' requires a numeric argument; `%s' is not numeric",
                [s(OptionName), s(Arg)], String)
        ;
            Reason = file_special_cannot_open(FileName, IO_Error),
            io.error_message(IO_Error, Msg),
            string.format("cannot open %s: %s", [s(FileName), s(Msg)], String)
        ;
            Reason = file_special_cannot_read(FileName, IO_Error),
            io.error_message(IO_Error, Msg),
            string.format("cannot read %s: %s", [s(FileName), s(Msg)], String)
        ;
            Reason = file_special_contains_non_option_args(FileName),
            string.format("%s contains non-option arguments",
                [s(FileName)], String)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module getopt_io.
%---------------------------------------------------------------------------%
