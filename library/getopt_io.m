%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: getopt_io.m
% Authors: fjh, zs
% Stability: medium
%
% This module exports the predicate getopt_io.process_options/6, which can be
% used to parse command-line options.
%
% This version allows both short (single-character) options and GNU-style long
% options. It also has the GNU extension of recognizing options anywhere in
% the command-line, not just at the start.
%
% To use this module, you must provide an `option' type which
% is an enumeration of all your different options.
% You must provide predicates `short_option(Char, Option)'
% and `long_option(String, Option)' which convert the short
% and/or long names for the option to this enumeration type.
% (An option can have as many names as you like, long or short.)
% You must provide a predicate `option_default(Option, OptionData)'
% which specifies both the type and the default value for every option.
% You may optionally provide a predicate `special_handler(Option,
% SpecialData, OptionTable, MaybeOptionTable)' for handling special
% option types.  (See below.)
%
% We support the following "simple" option types:
%
%   - bool
%   - int
%   - maybe_int (which have a value of `no' or `yes(int)')
%   - string
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
% precedence.  For "accumulating" options, multiple occurrences will be
% appended together into a list.
%
% With the exception of file_special, the "special" option types are handled
% by a special option handler (see `special_handler' below), which may perform
% arbitrary modifications to the option_table.  For example, an option which
% is not yet implemented could be handled by a special handler which produces
% an error report, or an option which is a synonym for a set of more
% "primitive" options could be handled by a special handler which sets those
% "primitive" options.
%
% It is an error to use a "special" option for which there is no handler, or
% for which the handler fails.
%
% Boolean (i.e. bool or bool_special), maybe_int, maybe_string
% and accumulating options can be negated. Negating an accumulating
% option empties the accumulated list of strings.
% Single-character options can be negated by following them
% with another `-', e.g. `-x-' will negate the `-x' option.
% Long options can be negated by preceding them with `--no-',
% e.g. `--no-foo' will negate the `--foo' option.
%
% The file_special option type requires no handler, and is implemented
% entirely by this module. It always takes a single argument, a file name.
% Its handling always consists of reading the named file, converting its
% contents into a sequence of words separated by white space, and interpreting
% those words as options in the usual manner.
%
% Note that arguments following an option may be separated from the option by
% either whitespace or an equals, `=', character, e.g. `--foo 3' and `--foo=3'
% both specify the option `--foo' with the integer argument `3'.
%
% If the argument `--' is encountered on the command-line then option
% processing will immediately terminate, without processing any remaining
% options.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module getopt_io.
:- interface.

:- import_module bool.
:- import_module char.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

% getopt_io.process_options(OptionOps, Args, NonOptionArgs, Result)
% getopt_io.process_options(OptionOps, Args, OptionArgs, NonOptionArgs, Result)
%
%   Scans through 'Args' looking for options, places all the option
%   arguments in `OptionArgs', places all the non-option arguments in
%   'NonOptionArgs', and records the options in the `OptionTable'.
%   `OptionTable' is a map from a user-defined option type to option_data.
%   If an invalid option is encountered, we return `error(Message)'
%   otherwise we return `ok(OptionTable)' in 'Result'.
%
%   The argument `OptionOps' is a structure holding three or four
%   predicates used to categorize a set of options. Their
%   interfaces should be like these:
%
% :- pred short_option(char::in, option::out) is semidet.
%   True if the character names a valid single-character option.
%
% :- pred long_option(string::in, option::out) is semidet.
%   True if the string names a valid long option.
%
% :- pred option_default(option::out, option_data::out) is multi.
%   Nondeterministically returns all the options with their
%   corresponding types and default values.
%
% :- pred special_handler(option::in, special_data::in,
%   option_table::in, maybe_option_table(_)::out) is semidet.
%   This predicate is invoked whenever getopt finds an option
%   (long or short) designated as special, with special_data holding
%   the argument of the option (if any). The predicate can change the
%   option table in arbitrary ways in the course of handling the option,
%   or it can return an error message.
%   The canonical examples of special options are -O options in compilers,
%   which set many other options at once.

:- pred getopt_io.process_options(option_ops(OptionType)::in(option_ops),
    list(string)::in, list(string)::out, maybe_option_table(OptionType)::out,
    io::di, io::uo) is det.

:- pred getopt_io.process_options(option_ops(OptionType)::in(option_ops),
    list(string)::in, list(string)::out, list(string)::out,
    maybe_option_table(OptionType)::out, io::di, io::uo) is det.

% getopt_io.process_options_track(OptionOps, Args, OptionArgs,
%       NonOptionArgs, OptionTable0, Result, OptionsSet)

:- pred getopt_io.process_options_track(
    option_ops_track(OptionType)::in(option_ops_track),
    list(string)::in, list(string)::out, list(string)::out,
    option_table(OptionType)::in, maybe_option_table(OptionType)::out,
    set(OptionType)::out, io::di, io::uo) is det.

:- pred init_option_table(
    pred(OptionType, option_data)::in(pred(out, out) is nondet),
    option_table(OptionType)::out) is det.

:- pred init_option_table_multi(
    pred(OptionType, option_data)::in(pred(out, out) is multi),
    option_table(OptionType)::out) is det.

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

:- type option_ops_track(OptionType)
    --->    option_ops_track(
                pred(char, OptionType),         % short_option
                pred(string, OptionType),       % long_option
                pred(OptionType, special_data,  % special option handler
                    option_table(OptionType),
                    maybe_option_table(OptionType),
                    set(OptionType))
            ).

:- inst option_ops ==
    bound((
        option_ops(
            pred(in, out) is semidet,               % short_option
            pred(in, out) is semidet,               % long_option
            pred(out, out) is nondet                % option_default
        )
    ;   option_ops_multi(
            pred(in, out) is semidet,               % short_option
            pred(in, out) is semidet,               % long_option
            pred(out, out) is multi                 % option_default
        )
    ;   option_ops(
            pred(in, out) is semidet,               % short_option
            pred(in, out) is semidet,               % long_option
            pred(out, out) is nondet,               % option_default
            pred(in, in, in, out) is semidet        % special handler
        )
    ;   option_ops_multi(
            pred(in, out) is semidet,               % short_option
            pred(in, out) is semidet,               % long_option
            pred(out, out) is multi,                % option_default
            pred(in, in, in, out) is semidet        % special handler
        )
    )).

:- inst option_ops_track ==
    bound((
        option_ops_track(
            pred(in, out) is semidet,               % short_option
            pred(in, out) is semidet,               % long_option
            pred(in, in, in, out, out) is semidet   % special handler
        )
    )).

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

:- type option_table(OptionType) ==  map(OptionType, option_data).

:- type maybe_option_table(OptionType)
    --->    ok(option_table(OptionType))
    ;       error(string).

    % The following three predicates search the option table for
    % an option of the specified type; if it is not found, they
    % report an error by calling error/1.

:- pred getopt_io.lookup_bool_option(option_table(Option)::in, Option::in,
    bool::out) is det.
:- func getopt_io.lookup_bool_option(option_table(Option), Option) = bool.

:- pred getopt_io.lookup_int_option(option_table(Option)::in, Option::in,
    int::out) is det.
:- func getopt_io.lookup_int_option(option_table(Option), Option) = int.

:- pred getopt_io.lookup_string_option(option_table(Option)::in, Option::in,
    string::out) is det.
:- func getopt_io.lookup_string_option(option_table(Option), Option) = string.

:- pred getopt_io.lookup_maybe_int_option(option_table(Option)::in, Option::in,
    maybe(int)::out) is det.
:- func getopt_io.lookup_maybe_int_option(option_table(Option), Option) =
    maybe(int).

:- pred getopt_io.lookup_maybe_string_option(option_table(Option)::in,
    Option::in, maybe(string)::out) is det.
:- func getopt_io.lookup_maybe_string_option(option_table(Option), Option) =
    maybe(string).

:- pred getopt_io.lookup_accumulating_option(option_table(Option)::in,
    Option::in, list(string)::out) is det.
:- func getopt_io.lookup_accumulating_option(option_table(Option), Option) =
    list(string).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module require.
:- import_module solutions.
:- import_module string.

%-----------------------------------------------------------------------------%

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

:- type option_ops_internal(OptionType)
    --->    option_ops_internal(
                short_option    :: pred(char, OptionType),
                long_option     :: pred(string, OptionType),
                special_handler :: option_ops_special(OptionType)
            ).

:- inst option_ops_internal ==
    bound((
        option_ops_internal(
            pred(in, out) is semidet,               % short_option
            pred(in, out) is semidet,               % long_option
            bound((                                 % special handler, if any
                none
            ;
                notrack(pred(in, in, in, out) is semidet)
            ;
                track(pred(in, in, in, out, out) is semidet)
            ))
        )
    )).

init_option_table(OptionDefaultsPred, OptionTable) :-
    solutions((pred(OptionDataPair::out) is nondet :-
            OptionDataPair = Option - OptionData,
            OptionDefaultsPred(Option, OptionData)
        ), OptionDefaultsList),
    map.from_assoc_list(OptionDefaultsList, OptionTable).

init_option_table_multi(OptionDefaultsPred, OptionTable) :-
    solutions((pred(OptionDataPair::out) is multi :-
            OptionDataPair = Option - OptionData,
            OptionDefaultsPred(Option, OptionData)
        ), OptionDefaultsList),
    map.from_assoc_list(OptionDefaultsList, OptionTable).

getopt_io.process_options(OptionOps, Args0, NonOptionArgs, Result, !IO) :-
    getopt_io.process_options(OptionOps, Args0, _OptionArgs, NonOptionArgs,
        Result, !IO).

getopt_io.process_options(OptionOps, Args0, OptionArgs, NonOptionArgs, Result,
        !IO) :-
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
    getopt_io.process_arguments(Args0, NonOptionArgs, Internal,
        [], RevOptionArgs, OptionTable0, Result, set.init, _OptionsSet, !IO),
    OptionArgs = list.reverse(RevOptionArgs).

getopt_io.process_options_track(OptionOps, Args0, OptionArgs, NonOptionArgs,
        OptionTable0, Result, OptionsSet, !IO) :-
    OptionOps = option_ops_track(Short, Long, Special),
    Internal = option_ops_internal(Short, Long, track(Special)),
    getopt_io.process_arguments(Args0, NonOptionArgs, Internal,
        [], RevOptionArgs, OptionTable0, Result, set.init, OptionsSet, !IO),
    OptionArgs = list.reverse(RevOptionArgs).

:- pred getopt_io.process_arguments(list(string)::in, list(string)::out,
    option_ops_internal(OptionType)::in(option_ops_internal), list(string)::in,
    list(string)::out, option_table(OptionType)::in,
    maybe_option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out, io::di, io::uo) is det.

getopt_io.process_arguments([], [], _, OptionArgs, OptionArgs,
        OptionTable, ok(OptionTable), !OptionsSet, !IO).
getopt_io.process_arguments([Option | Args0], Args, OptionOps,
        OptionArgs0, OptionArgs, OptionTable0, Result, !OptionsSet, !IO) :-
    ( Option = "--" ->  % "--" terminates option processing
        OptionArgs = OptionArgs0,
        Args = Args0,
        Result = ok(OptionTable0)
    ; string.append("--no-", LongOption, Option) ->
        LongOptionPred = OptionOps ^ long_option,
        ( LongOptionPred(LongOption, Flag) ->
            string.append("--", LongOption, OptName),
            process_negated_option(OptName, Flag, OptionOps,
                OptionTable0, Result1, !OptionsSet),
            (
                Result1 = ok(OptionTable1),
                getopt_io.process_arguments(Args0, Args, OptionOps,
                    [Option | OptionArgs0], OptionArgs, OptionTable1, Result,
                    !OptionsSet, !IO)
            ;
                Result1 = error(_),
                Result = Result1,
                OptionArgs = OptionArgs0,
                Args = Args0
            )
        ;
            ErrorMsg = "unrecognized option `-" ++ Option ++ "'",
            Result = error(ErrorMsg),
            OptionArgs = OptionArgs0,
            Args = Args0
        )
    ; string.append("--", LongOptionStr, Option) ->
        LongOptionPred = OptionOps ^ long_option,
        ( string.sub_string_search(LongOptionStr, "=", OptionLen) ->
            string.split(LongOptionStr, OptionLen, LongOption,
                EqualOptionArg),
            ( string.first_char(EqualOptionArg, '=', OptionArg) ->
                MaybeArg = yes(OptionArg)
            ;
                error("bad split of --longoption=arg")
            )
        ;
            LongOption = LongOptionStr,
            MaybeArg = no
        ),
        OptionName = "--" ++ LongOption,
        ( LongOptionPred(LongOption, Flag) ->
            ( map.search(OptionTable0, Flag, OptionData) ->
                getopt_io.handle_long_option(OptionName, Flag, OptionData,
                    MaybeArg, Args0, Args, OptionOps,
                    [Option | OptionArgs0], OptionArgs,
                    OptionTable0, Result, !OptionsSet, !IO)
            ;
                ErrorMsg = "unknown type for option `" ++ Option ++ "'",
                Result = error(ErrorMsg),
                OptionArgs = OptionArgs0,
                Args = Args0
            )
        ;
            ErrorMsg = "unrecognized option `" ++ OptionName ++ "'",
            Result = error(ErrorMsg),
            OptionArgs = OptionArgs0,
            Args = Args0
        )
    ; string.first_char(Option, '-', ShortOptions), ShortOptions \= "" ->
        string.to_char_list(ShortOptions, ShortOptionsList),
        % Process a single negated option `-x-'.
        ( ShortOptionsList = [SingleShortOpt, '-'] ->
            ShortOptionPred = OptionOps ^ short_option,
            ( ShortOptionPred(SingleShortOpt, Flag) ->
                string.from_char_list(['-', SingleShortOpt], OptName),
                process_negated_option(OptName, Flag, OptionOps,
                    OptionTable0, Result1, !OptionsSet),
                (
                    Result1 = ok(OptionTable1),
                    getopt_io.process_arguments(Args0, Args, OptionOps,
                        [Option | OptionArgs0], OptionArgs,
                        OptionTable1, Result, !OptionsSet, !IO)
                ;
                    Result1 = error(_),
                    Result = Result1,
                    OptionArgs = OptionArgs0,
                    Args = Args0
                )
            ;
                ErrorMsg = "unrecognized option `-" ++ ShortOptions ++ "'",
                Result = error(ErrorMsg),
                OptionArgs = OptionArgs0,
                Args = Args0
            )
        ;
            % Process a list of options `-xyz'.
            % -xyz may be several boolean options
            % or part of it may be the argument of an option.
            % The first element of Args0 may also be an argument
            % of an option.
            getopt_io.handle_short_options(ShortOptionsList, OptionOps,
                Args0, Args1, [Option | OptionArgs0], OptionArgs1,
                OptionTable0, Result1, !OptionsSet, !IO),
            (
                Result1 = ok(OptionTable1),
                getopt_io.process_arguments(Args1, Args, OptionOps,
                    OptionArgs1, OptionArgs, OptionTable1, Result, !OptionsSet,
                    !IO)
            ;
                Result1 = error(_),
                Result = Result1,
                OptionArgs = OptionArgs1,
                Args = Args0
            )
        )
    ;
        % It's a normal non-option argument.
        % As a GNU extension, keep searching for options
        % in the remaining arguments.
        getopt_io.process_arguments(Args0, Args1, OptionOps,
            OptionArgs0, OptionArgs, OptionTable0, Result, !OptionsSet, !IO),
        Args = [Option | Args1]
    ).

:- pred getopt_io.handle_long_option(string::in,
    OptionType::in, option_data::in,
    maybe(string)::in, list(string)::in, list(string)::out,
    option_ops_internal(OptionType)::in(option_ops_internal), list(string)::in,
    list(string)::out, option_table(OptionType)::in,
    maybe_option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out, io::di, io::uo) is det.

getopt_io.handle_long_option(Option, Flag, OptionData, MaybeOptionArg0,
        Args0, Args, OptionOps, OptionArgs0, OptionArgs, OptionTable0, Result,
        !OptionsSet, !IO) :-
    (
        getopt_io.need_arg(OptionData, yes),
        MaybeOptionArg0 = no
    ->
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
    ;
        MaybeOptionArg = MaybeOptionArg0,
        Args1 = Args0,
        OptionArgs1 = OptionArgs0,
        MissingArg = no
    ),
    ( MissingArg = yes ->
        Args = Args0,
        OptionArgs = OptionArgs1,
        ErrorMsg = "option `" ++ Option ++ "' needs an argument",
        Result = error(ErrorMsg)
    ;
        getopt_io.need_arg(OptionData, no),
        MaybeOptionArg = yes(_)
    ->
        Args = Args0,
        OptionArgs = OptionArgs1,
        ErrorMsg = "option `" ++ Option ++ "' does not allow an argument",
        Result = error(ErrorMsg)
    ;
        getopt_io.process_option(OptionData, Option, Flag, MaybeOptionArg,
            OptionOps, OptionTable0, Result1, !OptionsSet, !IO),
        (
            Result1 = ok(OptionTable1),
            getopt_io.process_arguments(Args1, Args, OptionOps,
                OptionArgs1, OptionArgs, OptionTable1, Result, !OptionsSet,
                !IO)
        ;
            Result1 = error(_),
            Result = Result1,
            OptionArgs = OptionArgs1,
            Args = Args1
        )
    ).

:- pred getopt_io.handle_short_options(list(char)::in,
    option_ops_internal(OptionType)::in(option_ops_internal), list(string)::in,
    list(string)::out, list(string)::in, list(string)::out,
    option_table(OptionType)::in,
    maybe_option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out, io::di, io::uo) is det.

getopt_io.handle_short_options([], _, Args, Args, OptionArgs, OptionArgs,
        OptionTable, ok(OptionTable), !OptionsSet, !IO).
getopt_io.handle_short_options([Opt | Opts0], OptionOps, Args0, Args,
        OptionArgs0, OptionArgs, OptionTable0, Result, !OptionsSet, !IO) :-
    ShortOptionPred = OptionOps ^ short_option,
    ( ShortOptionPred(Opt, Flag) ->
        ( map.search(OptionTable0, Flag, OptionData) ->
            ( getopt_io.need_arg(OptionData, yes) ->
                getopt_io.get_short_option_arg(Opts0, Arg, Args0, Args1,
                    OptionArgs0, OptionArgs1),
                MaybeOptionArg = yes(Arg),
                Opts1 = []
            ;
                MaybeOptionArg = no,
                Opts1 = Opts0,
                OptionArgs1 = OptionArgs0,
                Args1 = Args0
            ),
            string.from_char_list(['-', Opt], Option),
            getopt_io.process_option(OptionData, Option, Flag, MaybeOptionArg,
                OptionOps, OptionTable0, Result1, !OptionsSet, !IO),
            (
                Result1 = ok(OptionTable1),
                getopt_io.handle_short_options(Opts1, OptionOps, Args1, Args,
                    OptionArgs1, OptionArgs, OptionTable1, Result, !OptionsSet,
                    !IO)
            ;
                Result1 = error(_),
                Result = Result1,
                OptionArgs = OptionArgs1,
                Args = Args1
            )
        ;
            string.char_to_string(Opt, OptString),
            ErrorMsg = "unknown type for option `-" ++ OptString ++ "'",
            Result = error(ErrorMsg),
            OptionArgs = OptionArgs0,
            Args = Args0
        )
    ;
        string.char_to_string(Opt, OptString),
        ErrorMsg = "unrecognized option `-" ++ OptString ++ "'",
        Result = error(ErrorMsg),
        OptionArgs = OptionArgs0,
        Args = Args0
    ).

:- pred getopt_io.get_short_option_arg(list(char)::in, string::out,
    list(string)::in, list(string)::out, list(string)::in, list(string)::out)
    is det.

getopt_io.get_short_option_arg(Opts, Arg, Args0, Args,
        OptionArgs0, OptionArgs) :-
    (
        Opts = [],
        Args0 = [ArgPrime | ArgsPrime]
    ->
        OptionArgs = [ArgPrime | OptionArgs0],
        Arg = ArgPrime,
        Args = ArgsPrime
    ;
        string.from_char_list(Opts, Arg),
        OptionArgs = OptionArgs0,
        Args = Args0
    ).

:- pred getopt_io.process_option(option_data::in, string::in, OptionType::in,
    maybe(string)::in, option_ops_internal(OptionType)::in(option_ops_internal),
    option_table(OptionType)::in,
    maybe_option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out, io::di, io::uo) is det.

getopt_io.process_option(bool(_), _Option, Flag, MaybeArg, _OptionOps,
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
getopt_io.process_option(int(_), Option, Flag, MaybeArg, _OptionOps,
        !.OptionTable, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(Arg),
        ( string.to_int(Arg, IntArg) ->
            map.set(Flag, int(IntArg), !OptionTable),
            Result = ok(!.OptionTable)
        ;
            getopt_io.numeric_argument(Option, Arg, Result)
        )
    ;
        MaybeArg = no,
        error("integer argument expected in getopt_io.process_option")
    ).
getopt_io.process_option(string(_), _Option, Flag, MaybeArg, _OptionOps,
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
getopt_io.process_option(maybe_int(_), Option, Flag, MaybeArg, _OptionOps,
        !.OptionTable, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(Arg),
        ( string.to_int(Arg, IntArg) ->
            map.set(Flag, maybe_int(yes(IntArg)), !OptionTable),
            Result = ok(!.OptionTable)
        ;
            getopt_io.numeric_argument(Option, Arg, Result)
        )
    ;
        MaybeArg = no,
        error("integer argument expected in getopt_io.process_option")
    ).
getopt_io.process_option(maybe_string(_), _Option, Flag, MaybeArg, _OptionOps,
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
getopt_io.process_option(accumulating(List0), _Option, Flag, MaybeArg, _OptionOps,
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
getopt_io.process_option(special, Option, Flag, MaybeArg, OptionOps,
        OptionTable0, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(_Arg),
        error("no special argument expected in getopt_io.process_option")
    ;
        MaybeArg = no,
        getopt_io.process_special(Option, Flag, none,
            OptionOps, OptionTable0, Result, !OptionsSet)
    ).
getopt_io.process_option(bool_special, Option, Flag, MaybeArg, OptionOps,
        OptionTable0, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(_Arg),
        getopt_io.process_special(Option, Flag, bool(no),
            OptionOps, OptionTable0, Result, !OptionsSet)
    ;
        MaybeArg = no,
        getopt_io.process_special(Option, Flag, bool(yes),
            OptionOps, OptionTable0, Result, !OptionsSet)
    ).
getopt_io.process_option(int_special, Option, Flag, MaybeArg, OptionOps,
        OptionTable0, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(Arg),
        ( string.to_int(Arg, IntArg) ->
            getopt_io.process_special(Option, Flag, int(IntArg),
                OptionOps, OptionTable0, Result, !OptionsSet)
        ;
            getopt_io.numeric_argument(Option, Arg, Result)
        )
    ;
        MaybeArg = no,
        error("int_special argument expected in getopt_io.process_option")
    ).
getopt_io.process_option(string_special, Option, Flag, MaybeArg, OptionOps,
        OptionTable0, Result, !OptionsSet, !IO) :-
    set.insert(Flag, !OptionsSet),
    (
        MaybeArg = yes(Arg),
        getopt_io.process_special(Option, Flag, string(Arg),
            OptionOps, OptionTable0, Result, !OptionsSet)
    ;
        MaybeArg = no,
        error("string_special argument expected in getopt_io.process_option")
    ).
getopt_io.process_option(maybe_string_special, Option, Flag, MaybeArg,
        OptionOps, OptionTable0, Result, !OptionsSet, !IO) :-
    (
        MaybeArg = yes(_Arg),
        getopt_io.process_special(Option, Flag, maybe_string(MaybeArg),
            OptionOps, OptionTable0, Result, !OptionsSet)
    ;
        MaybeArg = no,
        error("maybe_string_special argument expected " ++
            "in getopt_io.process_option")
    ).
getopt_io.process_option(file_special, _Option, _Flag, MaybeArg, OptionOps,
        OptionTable0, Result, !OptionsSet, !IO) :-
    (
        MaybeArg = yes(FileName),
        io.see(FileName, SeeRes, !IO),
        (
            SeeRes = ok,
            io.read_file_as_string(ReadRes, !IO),
            (
                ReadRes = ok(Contents),
                Words = string.words(Contents),
                getopt_io.process_arguments(Words, Args, OptionOps,
                    [], _OptionArgs, OptionTable0, Result0, !OptionsSet, !IO),
                (
                    Args = [],
                    Result = Result0
                ;
                    Args = [_ | _],
                    Result = error(FileName ++
                        " contains non-option arguments")
                )
            ;
                ReadRes = error(_, Error),
                io.error_message(Error, Msg),
                Result = error("cannot read " ++ FileName ++ ": " ++ Msg)
            ),
            io.seen(!IO)
        ;
            SeeRes = error(Error),
            io.error_message(Error, Msg),
            Result = error("cannot open " ++ FileName ++ ": " ++ Msg)
        )
    ;
        MaybeArg = no,
        error("file_special argument expected in getopt_io.process_option")
    ).

:- pred process_negated_option(string::in, OptionType::in,
    option_ops_internal(OptionType)::in(option_ops_internal),
    option_table(OptionType)::in, maybe_option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out) is det.

process_negated_option(Option, Flag, OptionOps, OptionTable0, Result,
        !OptionsSet) :-
    ( map.search(OptionTable0, Flag, OptionData) ->
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
            getopt_io.process_special(Option, Flag, bool(no),
                OptionOps, OptionTable0, Result, !OptionsSet)
        ;
            OptionData = maybe_string_special,
            set.insert(Flag, !OptionsSet),
            getopt_io.process_special(Option, Flag, maybe_string(no),
                OptionOps, OptionTable0, Result, !OptionsSet)
        ;
            OptionData = int_special,
            ErrorMsg = "cannot negate option `" ++ Option ++ "' --" ++
                "only boolean, maybe and accumulating options can be negated",
            Result = error(ErrorMsg)
        ;
            OptionData = string_special,
            ErrorMsg = "cannot negate option `" ++ Option ++ "' --" ++
                "only boolean, maybe and accumulating options can be negated",
            Result = error(ErrorMsg)
        ;
            OptionData = int(_),
            ErrorMsg = "cannot negate option `" ++ Option ++ "' --" ++
                "only boolean, maybe and accumulating options can be negated",
            Result = error(ErrorMsg)
        ;
            OptionData = string(_),
            ErrorMsg = "cannot negate option `" ++ Option ++ "' --" ++
                "only boolean, maybe and accumulating options can be negated",
            Result = error(ErrorMsg)
        ;
            OptionData = special,
            ErrorMsg = "cannot negate option `" ++ Option ++ "' --" ++
                "only boolean, maybe and accumulating options can be negated",
            Result = error(ErrorMsg)
        ;
            OptionData = file_special,
            ErrorMsg = "cannot negate option `" ++ Option ++ "' --" ++
                "only boolean, maybe and accumulating options can be negated",
            Result = error(ErrorMsg)
        )
    ;
        string.append_list(["unknown type for option `", Option, "'"],
            ErrorMsg),
        Result = error(ErrorMsg)
    ).

:- pred getopt_io.process_special(string::in, OptionType::in, special_data::in,
    option_ops_internal(OptionType)::in(option_ops_internal),
    option_table(OptionType)::in, maybe_option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out) is det.

getopt_io.process_special(Option, Flag, OptionData, OptionOps,
        OptionTable0, Result, !OptionsSet) :-
    MaybeHandler = OptionOps ^ special_handler,
    (
        MaybeHandler = notrack(Handler),
        (
            Handler(Flag, OptionData, OptionTable0, Result0)
        ->
            Result = Result0
        ;
            string.append_list(["the handler of option `",
                Option, "' failed"], ErrorMsg),
            Result = error(ErrorMsg)
        )
    ;
        MaybeHandler = track(TrackHandler),
        (
            TrackHandler(Flag, OptionData, OptionTable0, Result0,
                NewOptionsSet)
        ->
            set.union(NewOptionsSet, !OptionsSet),
            Result = Result0
        ;
            string.append_list(["the handler of option `",
                Option, "' failed"], ErrorMsg),
            Result = error(ErrorMsg)
        )
    ;
        MaybeHandler = none,
        ErrorMsg = "option `" ++ Option ++ "' has no handler",
        Result = error(ErrorMsg)
    ).

%-----------------------------------------------------------------------------%

:- pred getopt_io.need_arg(option_data::in, bool::out) is det.

getopt_io.need_arg(bool(_), no).
getopt_io.need_arg(int(_), yes).
getopt_io.need_arg(string(_), yes).
getopt_io.need_arg(maybe_int(_), yes).
getopt_io.need_arg(maybe_string(_), yes).
getopt_io.need_arg(accumulating(_), yes).
getopt_io.need_arg(special, no).
getopt_io.need_arg(bool_special, no).
getopt_io.need_arg(int_special, yes).
getopt_io.need_arg(string_special, yes).
getopt_io.need_arg(maybe_string_special, yes).
getopt_io.need_arg(file_special, yes).

:- pred getopt_io.numeric_argument(string::in, string::in,
    maybe_option_table(OptionType)::out) is det.

getopt_io.numeric_argument(Option, Arg, Result) :-
    ErrorMsg = "option `" ++ Option ++
        "' requires a numeric argument; `" ++ Arg ++ "' is not numeric",
    Result = error(ErrorMsg).

%-----------------------------------------------------------------------------%

getopt_io.lookup_bool_option(OptionTable, Opt, Val) :-
    ( map.lookup(OptionTable, Opt, bool(Val0)) ->
        Val = Val0
    ;
        error("Expected bool option and didn't get one.")
    ).

getopt_io.lookup_int_option(OptionTable, Opt, Val) :-
    ( map.lookup(OptionTable, Opt, int(Val0)) ->
        Val = Val0
    ;
        error("Expected int option and didn't get one.")
    ).

getopt_io.lookup_string_option(OptionTable, Opt, Val) :-
    ( map.lookup(OptionTable, Opt, string(Val0)) ->
        Val = Val0
    ;
        error("Expected string option and didn't get one.")
    ).

getopt_io.lookup_maybe_int_option(OptionTable, Opt, Val) :-
    ( map.lookup(OptionTable, Opt, maybe_int(Val0)) ->
        Val = Val0
    ;
        error("Expected maybe_int option and didn't get one.")
    ).

getopt_io.lookup_maybe_string_option(OptionTable, Opt, Val) :-
    ( map.lookup(OptionTable, Opt, maybe_string(Val0)) ->
        Val = Val0
    ;
        error("Expected maybe_string option and didn't get one.")
    ).

getopt_io.lookup_accumulating_option(OptionTable, Opt, Val) :-
    ( map.lookup(OptionTable, Opt, accumulating(Val0)) ->
        Val = Val0
    ;
        error("Expected accumulating option and didn't get one.")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%   Functional forms added.

getopt_io.lookup_bool_option(OT, Opt) = B :-
    getopt_io.lookup_bool_option(OT, Opt, B).

getopt_io.lookup_int_option(OT, Opt) = N :-
    getopt_io.lookup_int_option(OT, Opt, N).

getopt_io.lookup_string_option(OT, Opt) = S :-
    getopt_io.lookup_string_option(OT, Opt, S).

getopt_io.lookup_maybe_int_option(OT, Opt) = MN :-
    getopt_io.lookup_maybe_int_option(OT, Opt, MN).

getopt_io.lookup_maybe_string_option(OT, Opt) =MS :-
    getopt_io.lookup_maybe_string_option(OT, Opt, MS).

getopt_io.lookup_accumulating_option(OT, Opt) =Ss :-
    getopt_io.lookup_accumulating_option(OT, Opt, Ss).
