%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1999,2001-2007, 2011 The University of Melbourne.
% Copyright (C) 2014-2018,2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: getopt_io.m.
% Authors: fjh, zs.
% Stability: medium.
%
% This module defines predicates that parse command line options.
%
% These predicates allow both short (single-character) options,
% which are preceded on command lines with a single dash, and GNU-style
% long options, which are preceded on command lines with a double dash.
% An argument starting with a single dash can specify more than one
% short option, so that e.g. `-cde' is equivalent to `-c -d -e', while
% each long option name must be in an argument of its own.
%
% The predicates in this module support the GNU extension of recognizing
% options anywhere in the command line, not just at its start.
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
%   - file_special (in the predicate variants that do I/O; see below)
%
% For the "simple" option types, if there are multiple occurrences of the same
% option on the command line, then the last (right-most) occurrence will take
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
% It is an error to use a "special" option (other than file_special)
% for which there is no handler, or for which the handler fails.
%
% The file_special option type requires no handler, and is implemented
% entirely by this module. It always takes a single argument, a file name.
% Its handling always consists of
%
% - reading the named file,
% - converting its contents into a sequence of words separated by white space,
%   and
% - interpreting those words as options in the usual manner.
%
% The reading of the file obviously requires doing I/O, which means that
% only the predicate variants that take an I/O state pair of arguments
% support file_special options. If a call to a predicate variant that
% does not take a pair of I/O states does nevertheless specify a file_special
% option, that predicate will report an error when processing a command line
% that contains that option.
%
% Boolean (i.e. bool or bool_special), maybe_int, maybe_string and
% accumulating options can be negated. Negating an accumulating option
% empties the accumulated list of strings. Single-character options
% can be negated by following them with another `-', e.g. `-x-' will negate
% the `-x' option. Long options can be negated by preceding them with `--no-',
% e.g. `--no-foo' will negate the `--foo' option.
%
% Note that arguments following an option may be separated from the option by
% either whitespace or the equals character `=', so that e.g. `--foo 3' and
% `--foo=3' both specify the option `--foo' with the integer argument `3'.
%
% If the argument `--' is encountered on the command line, then option
% processing will immediately terminate, without processing any remaining
% arguments. This is sometimes needed to tell a program to treat strings
% that start with a dash as non-option arguments.
%
% NOTE_TO_IMPLEMENTORS: Until we delete getopt_io.m, all maintenance
% NOTE_TO_IMPLEMENTORS: should be done on getopt_io.m, NOT getopt.m.
% NOTE_TO_IMPLEMENTORS: getopt.m should then be derived from getopt_io.m
% NOTE_TO_IMPLEMENTORS: by invoking "mmake getopt.m"
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

:- type short_option(OptionType) == (pred(char, OptionType)).
:- inst short_option ==             (pred(in, out) is semidet).

:- type long_option(OptionType) ==  (pred(string, OptionType)).
:- inst long_option ==              (pred(in, out) is semidet).

:- type option_default_value(OptionType) == (pred(OptionType, option_data)).
:- inst option_default_value_nondet ==      (pred(out, out) is nondet).
:- inst option_default_value_multi ==       (pred(out, out) is multi).

:- type special_handler(OptionType) ==
    (pred(OptionType, special_data,
        option_table(OptionType), maybe_option_table(OptionType))).
:- inst special_handler ==
    (pred(in, in, in, out) is semidet).

    % The predicates below that process options, namely
    %
    %   - process_options
    %   - process_options_io
    %   - process_options_track
    %   - process_options_track_io
    %
    % all take an argument of the option_ops type to tell them
    %
    % - what the default value of each option is;
    % - what the short and long names of the options are;
    %   (see the comment at the top for a description of the distinction), and
    % - if there are any special options, how they should be handled.
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
    %   % designated as special (by either a short or long name),
    %   % with special_data holding the argument of the option (if any).
    %   % The predicate can change the option table in arbitrary ways
    %   % in the course of handling the option, or it can return
    %   % an error message.
    %   %
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
                short_option(OptionType),
                long_option(OptionType),
                option_default_value(OptionType)
            )
    ;       option_ops(
                short_option(OptionType),
                long_option(OptionType),
                option_default_value(OptionType),
                special_handler(OptionType)
            )
    ;       option_ops_multi(
                short_option(OptionType),
                long_option(OptionType),
                option_default_value(OptionType)
            )
    ;       option_ops_multi(
                short_option(OptionType),
                long_option(OptionType),
                option_default_value(OptionType),
                special_handler(OptionType)
            ).

:- inst option_ops for option_ops/1
    --->    option_ops(
                short_option,
                long_option,
                option_default_value_nondet
            )
    ;       option_ops(
                short_option,
                long_option,
                option_default_value_nondet,
                special_handler
            )
    ;       option_ops_multi(
                short_option,
                long_option,
                option_default_value_multi
            )
    ;       option_ops_multi(
                short_option,
                long_option,
                option_default_value_multi,
                special_handler
            ).

%---------------------%

:- type special_handler_track(OptionType) ==
    (pred(OptionType, special_data,
        option_table(OptionType), maybe_option_table(OptionType),
        set(OptionType))).
:- inst special_handler_track ==
    (pred(in, in, in, out, out) is semidet).

    % A version of the option_ops type for the process_options_track
    % predicate and its process_options_track_io variant.
    % Unlike the option_ops type, it does not contain a predicate
    % for setting the initial default values of options, since
    % process_options_track expects that to be done separately.
    %
:- type option_ops_track(OptionType)
    --->    option_ops_track(
                short_option(OptionType),
                long_option(OptionType),
                special_handler_track(OptionType)
            ).

:- inst option_ops_track for option_ops_track/1
    --->    option_ops_track(
                short_option,
                long_option,
                special_handler_track
            ).

%---------------------%

:- type user_data_handler(OptionType, UserDataType) ==
    (pred(OptionType, special_data,
        option_table(OptionType), maybe_option_table(OptionType),
        UserDataType, UserDataType)).
:- inst user_data_handler ==
    (pred(in, in, in, out, in, out) is semidet).

    % A version of the option_ops type for the process_options_userdata
    % predicate and its process_options_userdata_io variant.
    %
:- type option_ops_userdata(OptionType, UserDataType)
    --->    option_ops_userdata(
                short_option(OptionType),
                long_option(OptionType),
                user_data_handler(OptionType, UserDataType)
            ).

:- inst option_ops_userdata for option_ops_userdata/2
    --->    option_ops_userdata(
                short_option,
                long_option,
                user_data_handler
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

    ;       file_special_but_no_io(string)
            % The option is a file_special option whose argument is the file
            % named by the first argument, but the user has not given the
            % predicate access to the I/O state.

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

    ;       file_special_contains_non_option_args(string)
            % The option is a file_special option whose argument is the file
            % named by the argument. This file contained some non-option
            % arguments.

    ;       file_special_circular_inclusion(string).
            % The option is a file_special option whose argument is the file
            % named by the argument. This file contained either a direct
            % or an indirect reference to an option that called for its
            % inclusion, which, if followed, would have resulted in
            % an infinite loop of inclusions.

%---------------------------------------------------------------------------%

    % process_options(OptionOps, Args, NonOptionArgs, Result):
    % process_options(OptionOps, Args, OptionArgs, NonOptionArgs, Result):
    % process_options_io(OptionOps, Args, NonOptionArgs,
    %   Result, !IO):
    % process_options_io(OptionOps, Args, OptionArgs, NonOptionArgs,
    %   Result, !IO):
    %
    % These four predicates do effectively the same job, differing
    % from each other in two minor ways.
    %
    % The job they all do is scanning through 'Args' looking for options.
    % The various fields of the OptionOps structure specify the names
    % (both short and long) of the options to look for, as well as their
    % default values, and possibly the handler for the special options.
    % The structure of the `OptionOps' argument is documented above,
    % at the definition of the option_ops type.
    %
    % All these predicates place all the non-option arguments in
    % 'NonOptionArgs', and the predicates that have an `OptionArgs' argument
    % place the option arguments there. (While some callers will want
    % the arguments contain the options, other callers will not, considering
    % that the only information they want from them is that contained in
    % the option table.)
    %
    % If they find a problem, such as an unknown option name, an option
    % being given an argument of the wrong type, or the failure of the handler
    % for a special option, all the predicates will put into `Result'
    % an `error' wrapped around an error code. That error code can be turned
    % into an error message using the option_error_to_string function below.
    %
    % If they do not find a problem, all these predicates will place into
    % `Result' an `ok' wrapped around an option table, which maps each option
    % to its final value. Unless updated by an option in `Args', this will be
    % its default value.
    %
    % The predicate versions whose names end in `io' take a pair of I/O state
    % arguments. This is so that they can handle file_special options, which
    % require reading a named file, and treating the file's contents as
    % specifying additional options. The predicate versions whose names
    % do not end in `io' cannot do I/O, and will report an error if they
    % encounter a file_special option.
    %
:- pred process_options(option_ops(OptionType)::in(option_ops),
    list(string)::in, list(string)::out,
    maybe_option_table_se(OptionType)::out) is det.
:- pred process_options(option_ops(OptionType)::in(option_ops),
    list(string)::in, list(string)::out, list(string)::out,
    maybe_option_table_se(OptionType)::out) is det.
:- pred process_options_io(option_ops(OptionType)::in(option_ops),
    list(string)::in, list(string)::out,
    maybe_option_table_se(OptionType)::out, io::di, io::uo) is det.
:- pred process_options_io(option_ops(OptionType)::in(option_ops),
    list(string)::in, list(string)::out, list(string)::out,
    maybe_option_table_se(OptionType)::out, io::di, io::uo) is det.

    % process_options_track(OptionOps, Args, OptionArgs, NonOptionArgs,
    %   OptionTable0, Result, OptionsSet):
    % process_options_track_io(OptionOps, Args, OptionArgs, NonOptionArgs,
    %   OptionTable0, Result, OptionsSet, !IO):
    %
    % These predicates differ from the non-track variants above
    % in only two respects.
    %
    % First, they expect the caller to supply an argument containing
    % the initial contents of the option table, instead of calling
    % the initialization predicate themselves. This allows a program
    % to initialize the option table just once (using either the
    % init_option_table or the init_option_table_multi predicate below),
    % but then call process_options_track or process_options_track_io
    % several times, with different sets of arguments, perhaps obtained
    % from different sources (such as command line, configuration file,
    % and so on).
    %
    % Second, each call to one of these predicates returns the set of options
    % that were set by that call. This helps with the same objective.
    % For example, the caller can tell whether an option was set from
    % a configuration file, the command line, both, or neither.
    %
:- pred process_options_track(
    option_ops_track(OptionType)::in(option_ops_track),
    list(string)::in, list(string)::out, list(string)::out,
    option_table(OptionType)::in, maybe_option_table_se(OptionType)::out,
    set(OptionType)::out) is det.
:- pred process_options_track_io(
    option_ops_track(OptionType)::in(option_ops_track),
    list(string)::in, list(string)::out, list(string)::out,
    option_table(OptionType)::in, maybe_option_table_se(OptionType)::out,
    set(OptionType)::out, io::di, io::uo) is det.

    % process_options_userdata(OptionOps, Args, OptionArgs, NonOptionArgs,
    %   MaybeError, OptionsSet, !OptionTable, !UserData):
    % process_options_userdata_io(OptionOps, Args, OptionArgs, NonOptionArgs,
    %   MaybeError, OptionsSet, !OptionTable, !UserData, !IO):
    %
    % These predicates are similar to the track predicates above, but differ
    % in two ways.
    %
    % - They also thread a piece of state of a user-specified "userdata" type
    %   through all the handlers of special options, so that each
    %   special handler can read from and/or write to this state.
    %   Amongst other things, this can be used by the caller to recover
    %   the *sequence* in which special options are specified,
    %   information that is not present in the (orderless) set
    %   of specified options.
    %
    % - Even if they find an error, they return the option table as it was
    %   just before it found the error. This option table will reflect
    %   all the previous options that could be correctly processed.
    %
:- pred process_options_userdata(
    option_ops_userdata(OptionType, UserDataType)::in(option_ops_userdata),
    list(string)::in, list(string)::out, list(string)::out,
    maybe(option_error(OptionType))::out, set(OptionType)::out,
    option_table(OptionType)::in, option_table(OptionType)::out,
    UserDataType::in, UserDataType::out) is det.
:- pred process_options_userdata_io(
    option_ops_userdata(OptionType, UserDataType)::in(option_ops_userdata),
    list(string)::in, list(string)::out, list(string)::out,
    maybe(option_error(OptionType))::out, set(OptionType)::out,
    option_table(OptionType)::in, option_table(OptionType)::out,
    UserDataType::in, UserDataType::out, io::di, io::uo) is det.

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

    % Each value of this type specifies
    %
    % - the identity of an option (in its first argument),
    % - the string on the command line setting that option
    %   (in its second argument), and, if the option takes a value,
    % - the value of the option (in its third argument).
    %
    % "special" options have no third argument, because they have
    % no associated value.
    %
    % Each occurrence of an accumulating option adds only one string
    % to the option's value, which is why ov_accumulating has one string.
    % Options that reset an accumulating option to the empty list
    % obviously have no associated value.
:- type option_value(OptionType)
    --->    ov_bool(OptionType, string, bool)
    ;       ov_int(OptionType, string, int)
    ;       ov_string(OptionType, string, string)
    ;       ov_maybe_int(OptionType, string, maybe(int))
    ;       ov_maybe_string(OptionType, string, maybe(string))
    ;       ov_accumulating(OptionType, string, string)
    ;       ov_accumulating_reset(OptionType, string)
    ;       ov_special(OptionType, string)
    ;       ov_bool_special(OptionType, string, bool)
    ;       ov_int_special(OptionType, string, int)
    ;       ov_string_special(OptionType, string, string)
    ;       ov_maybe_string_special(OptionType, string, maybe(string))
    ;       ov_file_special(OptionType, string, string).

:- type maybe_option_error(OptionType)
    --->    no_option_error
    ;       found_option_error(option_error(OptionType)).

    % record_arguments(ShortOptionPred, LongOptionPred, OptionTable,
    %   Args, NonOptionArgs, OptionArgs, MaybeError, OptionValues):
    %
    % Given Args, which is a list of command line arguments,
    %
    % - classify them into arguments that are and are not option args,
    %   returning them as OptionArgs and NonOptionArgs respectively,
    %
    % - use the ShortOptionPred and LongOptionPred predicates
    %   to figure out which user-defined options the OptionArgs refer to,
    %
    % - use OptionTable to figure out what kind of value, if any,
    %   each of those user-defined options has as its argument,
    %
    % - find those arguments and convert them to the expected type, and
    %
    % - provided no errors occurred in any of the above steps,
    %   return a list of those options and their values in OptionValues,
    %   and set MaybeError to no_option_error.
    %
    % - If some errors *did* occur, then set MaybeError to found_option_error
    %   wrapped around a description of one of them. This will probably be
    %   the first, but we do not guarantee that. Also, in this error case,
    %   OptionValues will probably contain the values of the options processed
    %   before the error, but we do not guarantee that either.
    %
    % Note that unlike the process_options_... predicates above,
    % this predicate does *not* update the option table in any way.
    % It also simply returns file_special options in OptionValues;
    % it does not process them. That processing can be done by
    % expand_file_specials below.
    %
:- pred record_arguments(short_option(OptionType)::in(short_option),
    long_option(OptionType)::in(long_option), option_table(OptionType)::in,
    list(string)::in, list(string)::out, list(string)::out,
    maybe_option_error(OptionType)::out,
    list(option_value(OptionType))::out) is det.

%---------------------------------------------------------------------------%

    % An inst that lists all the function symbols of the option_value type
    % *except* ov_file_special.
:- inst non_file_special for option_value/1
    --->    ov_bool(ground, ground, ground)
    ;       ov_int(ground, ground, ground)
    ;       ov_string(ground, ground, ground)
    ;       ov_maybe_int(ground, ground, ground)
    ;       ov_maybe_string(ground, ground, ground)
    ;       ov_accumulating(ground, ground, ground)
    ;       ov_accumulating_reset(ground, ground)
    ;       ov_special(ground, ground)
    ;       ov_bool_special(ground, ground, ground)
    ;       ov_int_special(ground, ground, ground)
    ;       ov_string_special(ground, ground, ground)
    ;       ov_maybe_string_special(ground, ground, ground).

    % expand_file_specials(ShortOptionPred, LongOptionPred, OptionTable,
    %   OptionValues, MaybeError, NonFileSpecialOptionValues, !MaybeIO):
    %
    % Given a list of OptionValues as generated for example by
    % record_arguments, replace each ov_file_special option value in that list
    % with the option values in the file named by that option.
    % If there are any errors, return a description of one of them
    % in MaybeError; otherwise, return the fully expanded list of options
    % in NonFileSpecialOptionValues, and set MaybeError to no_option_error.
    %
    % The ShortOptionPred, LongOptionPred and OptionTable arguments
    % play the same role as in record_arguments, since expand_file_specials
    % must of course record all the options in files named by ov_file_special
    % option values.
    %
:- pred expand_file_specials(short_option(OptionType)::in(short_option),
    long_option(OptionType)::in(long_option), option_table(OptionType)::in,
    list(option_value(OptionType))::in, maybe_option_error(OptionType)::out,
    list(option_value(OptionType))::out(list_skel(non_file_special)),
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % The following functions and predicates search the option table
    % for an option of the specified kind. If the option is not in the table,
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

:- func lookup_maybe_int_option(option_table(Option), Option) = maybe(int).
:- pred lookup_maybe_int_option(option_table(Option)::in, Option::in,
    maybe(int)::out) is det.

:- func lookup_maybe_string_option(option_table(Option), Option) =
    maybe(string).
:- pred lookup_maybe_string_option(option_table(Option)::in, Option::in,
    maybe(string)::out) is det.

:- func lookup_accumulating_option(option_table(Option), Option) =
    list(string).
:- pred lookup_accumulating_option(option_table(Option)::in, Option::in,
    list(string)::out) is det.

%---------------------------------------------------------------------------%

    % Convert the structured representation of an error
    % to an error message.
    %
:- func option_error_to_string(option_error(OptionType)) = string.

%---------------------%

    % If the argument represents an error, then convert that error from
    % the structured representation to an error message.
    %
:- func convert_to_maybe_option_table(maybe_option_table_se(OptionType))
    = maybe_option_table(OptionType).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module pair.
:- import_module require.
:- import_module solutions.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

:- type option_ops_special(OptionType, UserDataType)
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
            )
    ;       userdata(
                pred(OptionType, special_data,
                    option_table(OptionType),
                    maybe_option_table(OptionType),
                    UserDataType, UserDataType)
            ).

:- inst option_ops_special for option_ops_special/2
    --->    none
    ;       notrack(pred(in, in, in, out) is semidet)
    ;       track(pred(in, in, in, out, out) is semidet)
    ;       userdata(pred(in, in, in, out, in, out) is semidet).

:- type option_ops_internal(OptionType, UserDataType)
    --->    option_ops_internal(
                short_option    :: pred(char, OptionType),
                long_option     :: pred(string, OptionType),
                special_handler :: option_ops_special(OptionType, UserDataType)
            ).

:- inst option_ops_internal for option_ops_internal/2
    --->    option_ops_internal(
                pred(in, out) is semidet,           % short_option
                pred(in, out) is semidet,           % long_option
                option_ops_special                  % special handler, if any
            ).

%---------------------------------------------------------------------------%

    % This type records the result of attempting the read the contents
    % of a named file.
:- type read_file_contents_result
    --->    read_success(string)
            % The read succeeded, and the string is the entire contents
            % of the named file.
    ;       read_failure_open(io.error)
            % The read failed, as the file open failed with this error.
    ;       read_failure_read(io.error)
            % The read failed, as the file read failed with this error.
    ;       read_failure_no_io.
            % The read failed, as we have no access to I/O.

:- typeclass read_file_contents(MaybeIO) where [
    pred read_file_contents(string::in, read_file_contents_result::out,
        MaybeIO::di, MaybeIO::uo) is det
].

:- instance read_file_contents(io) where [
    pred(read_file_contents/4) is do_read_file_contents
].

:- instance read_file_contents(unit) where [
    pred(read_file_contents/4) is read_file_contents_no_io
].

:- pred do_read_file_contents(string::in, read_file_contents_result::out,
    io::di, io::uo) is det.

do_read_file_contents(FileName, Result, !IO) :-
    io.open_input(FileName, OpenResult, !IO),
    (
        OpenResult = ok(FileStream),
        io.read_file_as_string(FileStream, ReadResult, !IO),
        (
            ReadResult = ok(Contents),
            Result = read_success(Contents)
        ;
            ReadResult = error(_, IO_Error),
            Result = read_failure_read(IO_Error)
        ),
        io.close_input(FileStream, !IO)
    ;
        OpenResult = error(IO_Error),
        Result = read_failure_open(IO_Error)
    ).

:- pred read_file_contents_no_io(string::in, read_file_contents_result::out,
    unit::di, unit::uo) is det.

read_file_contents_no_io(_FileName, Result, !Unit) :-
    Result = read_failure_no_io.

%---------------------------------------------------------------------------%

process_options(OptionOps, Args, NonOptionArgs, Result) :-
    process_options(OptionOps, Args, _OptionArgs, NonOptionArgs, Result).

process_options(OptionOps, Args, OptionArgs, NonOptionArgs, Result) :-
    option_ops_to_internal_and_option_table(OptionOps, Short, Long, Special,
        OptionTable0),
    process_arguments(Short, Long, Special, Args, NonOptionArgs, OptionArgs,
        MaybeOptionError, OptionTable0, OptionTable,
        set.init, _OptionsSet, unit, _, unit, _),
    return_option_table_if_ok(MaybeOptionError, OptionTable, Result).

process_options_io(OptionOps, Args, NonOptionArgs, Result, !IO) :-
    process_options_io(OptionOps, Args, _OptionArgs, NonOptionArgs,
        Result, !IO).

process_options_io(OptionOps, Args, OptionArgs, NonOptionArgs, Result, !IO) :-
    option_ops_to_internal_and_option_table(OptionOps, Short, Long, Special,
        OptionTable0),
    process_arguments(Short, Long, Special, Args, NonOptionArgs, OptionArgs,
        MaybeOptionError, OptionTable0, OptionTable,
        set.init, _OptionsSet, unit, _, !IO),
    return_option_table_if_ok(MaybeOptionError, OptionTable, Result).

process_options_track(OptionOps, Args, OptionArgs, NonOptionArgs,
        OptionTable0, Result, OptionsSet) :-
    OptionOps = option_ops_track(Short, Long, Special),
    process_arguments(Short, Long, track(Special),
        Args, NonOptionArgs, OptionArgs, MaybeOptionError,
        OptionTable0, OptionTable, set.init, OptionsSet, unit, _, unit, _),
    return_option_table_if_ok(MaybeOptionError, OptionTable, Result).

process_options_track_io(OptionOps, Args, OptionArgs, NonOptionArgs,
        OptionTable0, Result, OptionsSet, !IO) :-
    OptionOps = option_ops_track(Short, Long, Special),
    process_arguments(Short, Long, track(Special),
        Args, NonOptionArgs, OptionArgs, MaybeOptionError,
        OptionTable0, OptionTable, set.init, OptionsSet, unit, _, !IO),
    return_option_table_if_ok(MaybeOptionError, OptionTable, Result).

process_options_userdata(OptionOps, Args, OptionArgs, NonOptionArgs,
        MaybeError, OptionsSet, !OptionTable, !UserData) :-
    OptionOps = option_ops_userdata(Short, Long, Special),
    process_arguments(Short, Long, userdata(Special),
        Args, NonOptionArgs, OptionArgs, MaybeOptionError,
        !OptionTable, set.init, OptionsSet, !UserData, unit, _),
    return_maybe_option_error(MaybeOptionError, MaybeError).

process_options_userdata_io(OptionOps, Args, OptionArgs, NonOptionArgs,
        MaybeError, OptionsSet, !OptionTable, !UserData, !IO) :-
    OptionOps = option_ops_userdata(Short, Long, Special),
    process_arguments(Short, Long, userdata(Special),
        Args, NonOptionArgs, OptionArgs, MaybeOptionError,
        !OptionTable, set.init, OptionsSet, !UserData, !IO),
    return_maybe_option_error(MaybeOptionError, MaybeError).

%---------------------%

:- pred option_ops_to_internal_and_option_table(
    option_ops(OptionType)::in(option_ops),
    short_option(OptionType)::out(short_option),
    long_option(OptionType)::out(long_option),
    option_ops_special(OptionType, UserDataType)::out(option_ops_special),
    option_table(OptionType)::out) is det.

option_ops_to_internal_and_option_table(OptionOps, Short, Long, MaybeSpecial,
        OptionTable0) :-
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
    ).

:- pred return_option_table_if_ok(maybe_option_error(OptionType)::in,
    option_table(OptionType)::in,
    maybe_option_table_se(OptionType)::out) is det.

return_option_table_if_ok(MaybeOptionError, OptionTable, Result) :-
    (
        MaybeOptionError = no_option_error,
        Result = ok(OptionTable)
    ;
        MaybeOptionError = found_option_error(Error),
        Result = error(Error)
    ).

:- pred return_maybe_option_error(maybe_option_error(OptionType)::in,
    maybe(option_error(OptionType))::out) is det.

return_maybe_option_error(no_option_error, no).
return_maybe_option_error(found_option_error(Error), yes(Error)).

%---------------------------------------------------------------------------%

init_option_table(OptionDefaultsPred, OptionTable) :-
    solutions(
        ( pred(OptionDataPair::out) is nondet :-
            OptionDefaultsPred(Flag, OptionData),
            OptionDataPair = Flag - OptionData
        ), OptionDefaultsList),
    map.from_sorted_assoc_list(OptionDefaultsList, OptionTable).

init_option_table_multi(OptionDefaultsPred, OptionTable) :-
    solutions(
        ( pred(OptionDataPair::out) is multi :-
            OptionDefaultsPred(Flag, OptionData),
            OptionDataPair = Flag - OptionData
        ), OptionDefaultsList),
    map.from_sorted_assoc_list(OptionDefaultsList, OptionTable).

%---------------------------------------------------------------------------%

:- type maybe_option_arg
    --->    no_option_arg
    ;       option_arg(string).

%---------------------------------------------------------------------------%

:- pred process_arguments(
    short_option(OptionType)::in(short_option),
    long_option(OptionType)::in(long_option),
    option_ops_special(OptionType, UserDataType)::in(option_ops_special),
    list(string)::in, list(string)::out, list(string)::out,
    maybe_option_error(OptionType)::out,
    option_table(OptionType)::in, option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out,
    UserDataType::in, UserDataType::out, MaybeIO::di, MaybeIO::uo) is det
    <= read_file_contents(MaybeIO).

process_arguments(ShortOptionPred, LongOptionPred, SpecialHandler,
        Args, NonOptionArgs, OptionArgs, MaybeError,
        !OptionTable, !OptionsSet, !UserData, !MaybeIO) :-
    % We process the argument list in three phases.
    %
    % - In phase 1, record_arguments process all the arguments, sorts them
    %   into option arguments and non-option arguments, and then recognizes
    %   and records the options (and if relevant, their values) in option
    %   arguments.
    %
    % - In phase 2, expand_file_specials loops over all the recorded option
    %   values, looking for file_special options. When it finds one, it
    %   replaces that file_special option with the option sequence in the
    %   file named in the option's argument, *after* invoking itself on that
    %   option sequence. This expand out all file_special options, however
    %   deeply nested inside other file_special options.
    %
    % - In phase 3, we process the list of recorded option values as expanded
    %   out by expand_file_specials, updating the option table and (if relevant
    %   !UserData as we go, and filling in !OptionsSet.
    %
    % All three phases may find and report errors. At the moment,
    % we go on to second and third phase only if the earlier phases
    % did not report any errors, but this is mostly because the signatures
    % of the exported predicates of this module have long had room to report
    % only *one* error. If we could report more than one error, we could
    % run every phase on the outputs of the earlier phases, even if those
    % outputs are only partial due to earlier failures.
    %
    record_arguments(ShortOptionPred, LongOptionPred, !.OptionTable,
        Args, NonOptionArgs, OptionArgs, MaybeRecordError, OptionValues),
    (
        MaybeRecordError = no_option_error,
        expand_file_specials_check(ShortOptionPred, LongOptionPred,
            !.OptionTable, OptionValues, set.init, MaybeExpandError,
            NFSOptionValues, !MaybeIO),
        (
            MaybeExpandError = no_option_error,
            process_option_values(SpecialHandler, NFSOptionValues, MaybeError,
                !OptionTable, !OptionsSet, !UserData)
        ;
            MaybeExpandError = found_option_error(_Error),
            MaybeError = MaybeExpandError
        )
    ;
        MaybeRecordError = found_option_error(_Error),
        MaybeError = MaybeRecordError
    ).

%---------------------------------------------------------------------------%

record_arguments(ShortOptionPred, LongOptionPred, OptionTable, Args,
        NonOptionArgs, OptionArgs, MaybeError, OptionValues) :-
    record_arguments_loop(ShortOptionPred, LongOptionPred, OptionTable, Args,
        NonOptionArgs, [], RevOptionArgs, MaybeError,
        cord.init, OptionValuesCord),
    list.reverse(RevOptionArgs, OptionArgs),
    OptionValues = cord.to_list(OptionValuesCord).

:- pred record_arguments_loop(short_option(OptionType)::in(short_option),
    long_option(OptionType)::in(long_option), option_table(OptionType)::in,
    list(string)::in, list(string)::out,
    list(string)::in, list(string)::out,
    maybe_option_error(OptionType)::out,
    cord(option_value(OptionType))::in, cord(option_value(OptionType))::out)
    is det.

record_arguments_loop(_, _, _, [], [], !RevOptionArgs,
        no_option_error, !OptionValues).
record_arguments_loop(ShortOptionPred, LongOptionPred, OptionTable,
        [Arg0 | Args0], NonOptionArgs, !RevOptionArgs,
        MaybeError, !OptionValues) :-
    ( if
        Arg0 = "--"
    then
        % "--" terminates option processing
        NonOptionArgs = Args0,
        MaybeError = no_option_error
    else if
        string.append("--no-", LongOption, Arg0)
    then
        ( if LongOptionPred(LongOption, Flag) then
            OptName = "--" ++ LongOption,
            record_negated_option(OptionTable, Flag, OptName, NegMaybeError,
                !OptionValues),
            (
                NegMaybeError = no_option_error,
                !:RevOptionArgs = [Arg0 | !.RevOptionArgs],
                record_arguments_loop(ShortOptionPred, LongOptionPred,
                    OptionTable, Args0, NonOptionArgs, !RevOptionArgs,
                    MaybeError, !OptionValues)
            ;
                NegMaybeError = found_option_error(_),
                MaybeError = NegMaybeError,
                NonOptionArgs = Args0
            )
        else
            Error = unrecognized_option(Arg0),
            MaybeError = found_option_error(Error),
            NonOptionArgs = Args0
        )
    else if
        string.append("--", LongOptionStr, Arg0)
    then
        ( if string.sub_string_search(LongOptionStr, "=", OptionLen) then
            string.split(LongOptionStr, OptionLen, LongOption, EqualOptionArg),
            ( if string.first_char(EqualOptionArg, '=', OptionArg) then
                MaybeArg = option_arg(OptionArg)
            else
                error("bad split of --longoption=arg")
            )
        else
            LongOption = LongOptionStr,
            MaybeArg = no_option_arg
        ),
        OptName = "--" ++ LongOption,
        ( if LongOptionPred(LongOption, Flag) then
            ( if map.search(OptionTable, Flag, OptionData) then
                !:RevOptionArgs = [Arg0 | !.RevOptionArgs],
                record_unnegated_long_option(Flag, OptName,
                    OptionData, MaybeArg, Args0, Args1, !RevOptionArgs,
                    LongMaybeError, !OptionValues),
                (
                    LongMaybeError = no_option_error,
                    record_arguments_loop(ShortOptionPred, LongOptionPred,
                        OptionTable, Args1, NonOptionArgs, !RevOptionArgs,
                        MaybeError, !OptionValues)
                ;
                    LongMaybeError = found_option_error(_),
                    MaybeError = LongMaybeError,
                    NonOptionArgs = Args0
                )
            else
                Error = option_error(Flag, Arg0, unknown_type),
                MaybeError = found_option_error(Error),
                NonOptionArgs = Args0
            )
        else
            Error = unrecognized_option(OptName),
            MaybeError = found_option_error(Error),
            NonOptionArgs = Args0
        )
    else if
        string.first_char(Arg0, '-', ShortOptions),
        ShortOptions \= ""
    then
        string.to_char_list(ShortOptions, ShortOptionsList),
        % Process a single negated option `-x-'.
        ( if ShortOptionsList = [SingleShortOpt, '-'] then
            ( if ShortOptionPred(SingleShortOpt, Flag) then
                string.from_char_list(['-', SingleShortOpt], OptName),
                record_negated_option(OptionTable, Flag, OptName,
                    NegMaybeError, !OptionValues),
                (
                    NegMaybeError = no_option_error,
                    !:RevOptionArgs = [Arg0 | !.RevOptionArgs],
                    record_arguments_loop(ShortOptionPred, LongOptionPred,
                        OptionTable, Args0, NonOptionArgs, !RevOptionArgs,
                        MaybeError, !OptionValues)
                ;
                    NegMaybeError = found_option_error(_),
                    MaybeError = NegMaybeError,
                    NonOptionArgs = Args0
                )
            else
                Error = unrecognized_option("-" ++ ShortOptions),
                MaybeError = found_option_error(Error),
                NonOptionArgs = Args0
            )
        else
            % Process a list of options `-xyz'.
            % -xyz may be several boolean options,
            % or part of it may be the argument of an option.
            % The first element of Args0 may also be an argument of an option.
            !:RevOptionArgs = [Arg0 | !.RevOptionArgs],
            record_unnegated_short_options(ShortOptionPred, OptionTable,
                ShortOptionsList, Args0, Args1, !RevOptionArgs,
                ShortMaybeError, !OptionValues),
            (
                ShortMaybeError = no_option_error,
                record_arguments_loop(ShortOptionPred, LongOptionPred,
                    OptionTable, Args1, NonOptionArgs, !RevOptionArgs,
                    MaybeError, !OptionValues)
            ;
                ShortMaybeError = found_option_error(_),
                MaybeError = ShortMaybeError,
                NonOptionArgs = Args0
            )
        )
    else
        % It is a normal non-option argument.
        % As a GNU extension, keep searching for options
        % in the remaining arguments.
        record_arguments_loop(ShortOptionPred, LongOptionPred, OptionTable,
            Args0, NonOptionArgsTail, !RevOptionArgs,
            MaybeError, !OptionValues),
        NonOptionArgs = [Arg0 | NonOptionArgsTail]
    ).

%---------------------%

:- pred record_negated_option(option_table(OptionType)::in,
    OptionType::in, string::in, maybe_option_error(OptionType)::out,
    cord(option_value(OptionType))::in, cord(option_value(OptionType))::out)
    is det.

record_negated_option(OptionTable, Flag, OptName, MaybeError, !OptionValues) :-
    ( if map.search(OptionTable, Flag, OptionData) then
        (
            (
                OptionData = bool(_),
                OV = ov_bool(Flag, OptName, bool.no)
            ;
                OptionData = maybe_int(_),
                OV = ov_maybe_int(Flag, OptName, maybe.no)
            ;
                OptionData = maybe_string(_),
                OV = ov_maybe_string(Flag, OptName, maybe.no)
            ;
                OptionData = accumulating(_),
                OV = ov_accumulating_reset(Flag, OptName)
            ;
                OptionData = bool_special,
                OV = ov_bool_special(Flag, OptName, bool.no)
            ;
                OptionData = maybe_string_special,
                OV = ov_maybe_string_special(Flag, OptName, maybe.no)
            ),
            MaybeError = no_option_error,
            cord.snoc(OV, !OptionValues)
        ;
            ( OptionData = int_special
            ; OptionData = string_special
            ; OptionData = int(_)
            ; OptionData = string(_)
            ; OptionData = special
            ; OptionData = file_special
            ),
            Error = option_error(Flag, OptName, cannot_negate),
            MaybeError = found_option_error(Error)
        )
    else
        Error = option_error(Flag, OptName, unknown_type),
        MaybeError = found_option_error(Error)
    ).

%---------------------%

:- pred record_unnegated_long_option(
    OptionType::in, string::in, option_data::in,
    maybe_option_arg::in, list(string)::in, list(string)::out,
    list(string)::in, list(string)::out,
    maybe_option_error(OptionType)::out,
    cord(option_value(OptionType))::in, cord(option_value(OptionType))::out)
    is det.

record_unnegated_long_option(Flag, OptName, OptionData,
        MaybeOptionArg0, Args0, Args1, !RevOptionArgs,
        MaybeError, !OptionValues) :-
    (
        OptionData = special,
        Args1 = Args0,
        record_option_none(OptionData, Flag, OptName, !OptionValues),
        report_any_unexpected_arg(MaybeOptionArg0, Flag, OptName, MaybeError)
    ;
        ( OptionData = bool(_)
        ; OptionData = bool_special
        ),
        Args1 = Args0,
        record_option_bool(OptionData, Flag, OptName, yes, !OptionValues),
        report_any_unexpected_arg(MaybeOptionArg0, Flag, OptName, MaybeError)
    ;
        ( OptionData = int(_)
        ; OptionData = maybe_int(_)
        ; OptionData = int_special
        ),
        get_long_option_arg(Flag, OptName, MaybeOptionArg0, OptionArgOrError,
            Args0, Args1, !RevOptionArgs),
        (
            OptionArgOrError = option_arg_no_error(Arg),
            record_option_int(OptionData, Flag, OptName, Arg,
                MaybeError, !OptionValues)
        ;
            OptionArgOrError = option_arg_error(Error),
            MaybeError = found_option_error(Error)
        )
    ;
        ( OptionData = string(_)
        ; OptionData = maybe_string(_)
        ; OptionData = accumulating(_)
        ; OptionData = string_special
        ; OptionData = maybe_string_special
        ; OptionData = file_special
        ),
        get_long_option_arg(Flag, OptName, MaybeOptionArg0, OptionArgOrError,
            Args0, Args1, !RevOptionArgs),
        (
            OptionArgOrError = option_arg_no_error(Arg),
            record_option_string(OptionData, Flag, OptName, Arg,
                !OptionValues),
            MaybeError = no_option_error
        ;
            OptionArgOrError = option_arg_error(Error),
            MaybeError = found_option_error(Error)
        )
    ).

:- pred report_any_unexpected_arg(maybe_option_arg::in, OptionType::in,
    string::in, maybe_option_error(OptionType)::out) is det.

report_any_unexpected_arg(MaybeOptionArg, Flag, OptName, MaybeError) :-
    (
        MaybeOptionArg = no_option_arg,
        MaybeError = no_option_error
    ;
        MaybeOptionArg = option_arg(Arg),
        ErrorReason = does_not_allow_argument(Arg),
        Error = option_error(Flag, OptName, ErrorReason),
        MaybeError = found_option_error(Error)
    ).

:- type option_arg_or_error(OptionType)
    --->    option_arg_no_error(string)
    ;       option_arg_error(option_error(OptionType)).

:- pred get_long_option_arg(OptionType::in, string::in,
    maybe_option_arg::in, option_arg_or_error(OptionType)::out,
    list(string)::in, list(string)::out,
    list(string)::in, list(string)::out) is det.

get_long_option_arg(Flag, OptName, MaybeOptionArg0, OptionArgOrError,
        Args0, Args1, !RevOptionArgs) :-
    (
        MaybeOptionArg0 = no_option_arg,
        (
            Args0 = [Arg | Args1],
            OptionArgOrError = option_arg_no_error(Arg),
            !:RevOptionArgs = [Arg | !.RevOptionArgs]
        ;
            Args0 = [],
            Error = option_error(Flag, OptName, requires_argument),
            OptionArgOrError = option_arg_error(Error),
            Args1 = Args0
        )
    ;
        MaybeOptionArg0 = option_arg(Arg),
        OptionArgOrError = option_arg_no_error(Arg),
        Args1 = Args0
    ).

%---------------------%

:- pred record_unnegated_short_options(
    short_option(OptionType)::in(short_option), option_table(OptionType)::in,
    list(char)::in, list(string)::in, list(string)::out,
    list(string)::in, list(string)::out, maybe_option_error(OptionType)::out,
    cord(option_value(OptionType))::in, cord(option_value(OptionType))::out)
    is det.

record_unnegated_short_options(_, _OptionTable, [],
        Args, Args, !RevOptionArgs, no_option_error, !OptionValues).
record_unnegated_short_options(ShortOptionPred, OptionTable, [Opt | Opts0],
        Args0, Args, !RevOptionArgs, MaybeError, !OptionValues) :-
    ( if ShortOptionPred(Opt, Flag) then
        ( if map.search(OptionTable, Flag, OptionData) then
            string.from_char_list(['-', Opt], OptName),
            (
                OptionData = special,
                record_option_none(OptionData, Flag, OptName, !OptionValues),
                record_unnegated_short_options(ShortOptionPred, OptionTable,
                    Opts0, Args0, Args, !RevOptionArgs,
                    MaybeError, !OptionValues)
            ;
                ( OptionData = bool(_)
                ; OptionData = bool_special
                ),
                record_option_bool(OptionData, Flag, OptName, yes,
                    !OptionValues),
                record_unnegated_short_options(ShortOptionPred, OptionTable,
                    Opts0, Args0, Args, !RevOptionArgs,
                    MaybeError, !OptionValues)
            ;
                ( OptionData = int(_)
                ; OptionData = maybe_int(_)
                ; OptionData = int_special
                ),
                get_short_option_arg(Opts0, Arg, Args0, Args, !RevOptionArgs),
                record_option_int(OptionData, Flag, OptName, Arg,
                    MaybeError, !OptionValues)
            ;
                ( OptionData = string(_)
                ; OptionData = maybe_string(_)
                ; OptionData = accumulating(_)
                ; OptionData = string_special
                ; OptionData = maybe_string_special
                ; OptionData = file_special
                ),
                get_short_option_arg(Opts0, Arg, Args0, Args, !RevOptionArgs),
                record_option_string(OptionData, Flag, OptName, Arg,
                    !OptionValues),
                MaybeError = no_option_error
            )
        else
            string.char_to_string(Opt, OptString),
            Args = Args0,
            Error = option_error(Flag, "-" ++ OptString, unknown_type),
            MaybeError = found_option_error(Error)
        )
    else
        Args = Args0,
        string.char_to_string(Opt, OptString),
        Error = unrecognized_option("-" ++ OptString),
        MaybeError = found_option_error(Error)
    ).

:- pred get_short_option_arg(list(char)::in, string::out,
    list(string)::in, list(string)::out, list(string)::in, list(string)::out)
    is det.

get_short_option_arg(Opts, Arg, Args0, Args, !RevOptionArgs) :-
    ( if
        Opts = [],
        Args0 = [ArgPrime | ArgsPrime]
    then
        !:RevOptionArgs = [ArgPrime | !.RevOptionArgs],
        Arg = ArgPrime,
        Args = ArgsPrime
    else
        string.from_char_list(Opts, Arg),
        Args = Args0
    ).

%---------------------------------------------------------------------------%

:- inst option_data_none for option_data/0
    --->    special.

:- inst option_data_bool for option_data/0
    --->    bool(ground)
    ;       bool_special.

:- inst option_data_int for option_data/0
    --->    int(ground)
    ;       maybe_int(ground)
    ;       int_special.

:- inst option_data_string for option_data/0
    --->    string(ground)
    ;       maybe_string(ground)
    ;       accumulating(ground)
    ;       string_special
    ;       maybe_string_special
    ;       file_special.

%---------------------%

:- pred record_option_none(option_data::in(option_data_none), OptionType::in,
    string::in,
    cord(option_value(OptionType))::in, cord(option_value(OptionType))::out)
    is det.

record_option_none(OptionData, Flag, OptName, !OptionValues) :-
    (
        OptionData = special,
        OV = ov_special(Flag, OptName)
    ),
    cord.snoc(OV, !OptionValues).

:- pred record_option_bool(option_data::in(option_data_bool),
    OptionType::in, string::in, bool::in,
    cord(option_value(OptionType))::in, cord(option_value(OptionType))::out)
    is det.

record_option_bool(OptionData, Flag, OptName, BoolValue, !OptionValues) :-
    (
        OptionData = bool(_),
        OV = ov_bool(Flag, OptName, BoolValue)
    ;
        OptionData = bool_special,
        OV = ov_bool_special(Flag, OptName, BoolValue)
    ),
    cord.snoc(OV, !OptionValues).

:- pred record_option_int(option_data::in(option_data_int),
    OptionType::in, string::in, string::in, maybe_option_error(OptionType)::out,
    cord(option_value(OptionType))::in, cord(option_value(OptionType))::out)
    is det.

record_option_int(OptionData, Flag, OptName, Arg, MaybeError, !OptionValues) :-
    (
        OptionData = int(_),
        ( if string.to_int(Arg, IntValue) then
            MaybeError = no_option_error,
            OV = ov_int(Flag, OptName, IntValue),
            cord.snoc(OV, !OptionValues)
        else
            numeric_argument_error(Flag, OptName, Arg, MaybeError)
        )
    ;
        OptionData = maybe_int(_),
        ( if string.to_int(Arg, IntValue) then
            MaybeError = no_option_error,
            OV = ov_maybe_int(Flag, OptName, yes(IntValue)),
            cord.snoc(OV, !OptionValues)
        else
            numeric_argument_error(Flag, OptName, Arg, MaybeError)
        )
    ;
        OptionData = int_special,
        ( if string.to_int(Arg, IntValue) then
            MaybeError = no_option_error,
            OV = ov_int_special(Flag, OptName, IntValue),
            cord.snoc(OV, !OptionValues)
        else
            numeric_argument_error(Flag, OptName, Arg, MaybeError)
        )
    ).

:- pred record_option_string(option_data::in(option_data_string),
    OptionType::in, string::in, string::in,
    cord(option_value(OptionType))::in, cord(option_value(OptionType))::out)
    is det.

record_option_string(OptionData, Flag, OptName, Arg, !OptionValues) :-
    (
        OptionData = string(_),
        OV = ov_string(Flag, OptName, Arg)
    ;
        OptionData = maybe_string(_),
        OV = ov_maybe_string(Flag, OptName, yes(Arg))
    ;
        OptionData = accumulating(_),
        OV = ov_accumulating(Flag, OptName, Arg)
    ;
        OptionData = string_special,
        OV = ov_string_special(Flag, OptName, Arg)
    ;
        OptionData = maybe_string_special,
        OV = ov_maybe_string_special(Flag, OptName, yes(Arg))
    ;
        OptionData = file_special,
        OV = ov_file_special(Flag, OptName, Arg)
    ),
    cord.snoc(OV, !OptionValues).

%---------------------------------------------------------------------------%

expand_file_specials(ShortOptionPred, LongOptionPred, OptionTable,
        OVs, MaybeError, NFSOVs, !MaybeIO) :-
    expand_file_specials_check(ShortOptionPred, LongOptionPred, OptionTable,
        OVs, set.init, MaybeError, NFSOVs, !MaybeIO).

:- pred expand_file_specials_check(short_option(OptionType)::in(short_option),
    long_option(OptionType)::in(long_option),
    option_table(OptionType)::in, list(option_value(OptionType))::in,
    set(string)::in, maybe_option_error(OptionType)::out,
    list(option_value(OptionType))::out(list_skel(non_file_special)),
    MaybeIO::di, MaybeIO::uo) is det <= read_file_contents(MaybeIO).

expand_file_specials_check(ShortOptionPred, LongOptionPred, OptionTable,
        OVs, SeenFileNames, MaybeError, NFSOVs, !MaybeIO) :-
    expand_file_specials_loop(ShortOptionPred, LongOptionPred, OptionTable,
        OVs, SeenFileNames, MaybeError, go_empty_cord, NFSOVsCord, !MaybeIO),
    NFSOVs = go_cord_to_list(NFSOVsCord).

:- pred expand_file_specials_loop(short_option(OptionType)::in(short_option),
    long_option(OptionType)::in(long_option),
    option_table(OptionType)::in, list(option_value(OptionType))::in,
    set(string)::in, maybe_option_error(OptionType)::out,
    go_cord(option_value(OptionType))::in(go_cord_skel(non_file_special)),
    go_cord(option_value(OptionType))::out(go_cord_skel(non_file_special)),
    MaybeIO::di, MaybeIO::uo) is det <= read_file_contents(MaybeIO).

expand_file_specials_loop(_, _, _, [], _SeenFileNames,
        no_option_error, !NFSOVsCord, !MaybeIO).
expand_file_specials_loop(ShortOptionPred, LongOptionPred, OptionTable,
        [OV0 | OVs0], SeenFileNames0, MaybeError, !NFSOVsCord, !MaybeIO) :-
    (
        ( OV0 = ov_bool(_, _, _)
        ; OV0 = ov_int(_, _, _)
        ; OV0 = ov_string(_, _, _)
        ; OV0 = ov_maybe_int(_, _, _)
        ; OV0 = ov_maybe_string(_, _, _)
        ; OV0 = ov_accumulating(_, _, _)
        ; OV0 = ov_accumulating_reset(_, _)
        ; OV0 = ov_special(_, _)
        ; OV0 = ov_bool_special(_, _, _)
        ; OV0 = ov_int_special(_, _, _)
        ; OV0 = ov_string_special(_, _, _)
        ; OV0 = ov_maybe_string_special(_, _, _)
        ),
        cord_snoc(OV0, !NFSOVsCord),
        expand_file_specials_loop(ShortOptionPred, LongOptionPred, OptionTable,
            OVs0, SeenFileNames0, MaybeError, !NFSOVsCord, !MaybeIO)
    ;
        OV0 = ov_file_special(Flag, OptName, FileName),
        ( if set.insert_new(FileName, SeenFileNames0, SeenFileNames) then
            expand_file_special_option(ShortOptionPred, LongOptionPred,
                OptionTable, SeenFileNames, Flag, OptName, FileName,
                FileOptionsOrError, !MaybeIO),
            (
                FileOptionsOrError = file_options_no_error(NFSFileOVsCord),
                go_cord_append(!.NFSOVsCord, NFSFileOVsCord, !:NFSOVsCord),
                expand_file_specials_loop(ShortOptionPred, LongOptionPred,
                    OptionTable, OVs0, SeenFileNames0, MaybeError,
                    !NFSOVsCord, !MaybeIO)
            ;
                FileOptionsOrError = file_options_error(Error),
                MaybeError = found_option_error(Error)
            )
        else
            Reason = file_special_circular_inclusion(FileName),
            Error = option_error(Flag, OptName, Reason),
            MaybeError = found_option_error(Error)
        )
    ).

:- type file_options_or_error(OptionType)
    --->    file_options_no_error(go_cord(option_value(OptionType)))
    ;       file_options_error(option_error(OptionType)).

:- inst file_options_or_error for file_options_or_error/1
    --->    file_options_no_error(go_cord_skel(non_file_special))
    ;       file_options_error(ground).

:- pred expand_file_special_option(short_option(OptionType)::in(short_option),
    long_option(OptionType)::in(long_option), option_table(OptionType)::in,
    set(string)::in, OptionType::in, string::in, string::in,
    file_options_or_error(OptionType)::out(file_options_or_error),
    MaybeIO::di, MaybeIO::uo) is det <= read_file_contents(MaybeIO).

expand_file_special_option(ShortOptionPred, LongOptionPred, OptionTable,
        SeenFileNames, Flag, OptName, FileName, FileOptionsOrError,
        !MaybeIO) :-
    read_file_contents(FileName, ReadFromFileResult, !MaybeIO),
    (
        ReadFromFileResult = read_success(Contents),
        Words = string.words(Contents),
        record_arguments(ShortOptionPred, LongOptionPred, OptionTable,
            Words, NonOptionArgs, _OptionArgs, FileMaybeError, FileOVs),
        (
            FileMaybeError = no_option_error,
            (
                NonOptionArgs = [],
                expand_file_specials_loop(ShortOptionPred, LongOptionPred,
                    OptionTable, FileOVs, SeenFileNames, MaybeError,
                    go_empty_cord, NFSOVsCord, !MaybeIO),
                (
                    MaybeError = no_option_error,
                    FileOptionsOrError = file_options_no_error(NFSOVsCord)
                ;
                    MaybeError = found_option_error(Error),
                    FileOptionsOrError = file_options_error(Error)
                )
            ;
                NonOptionArgs = [_ | _],
                Reason = file_special_contains_non_option_args(FileName),
                Error = option_error(Flag, OptName, Reason),
                FileOptionsOrError = file_options_error(Error)
            )
        ;
            FileMaybeError = found_option_error(FileError),
            FileOptionsOrError = file_options_error(FileError)
        )
    ;
        (
            ReadFromFileResult = read_failure_no_io,
            Reason = file_special_but_no_io(FileName)
        ;
            ReadFromFileResult = read_failure_open(IO_Error),
            Reason = file_special_cannot_open(FileName, IO_Error)
        ;
            ReadFromFileResult = read_failure_read(IO_Error),
            Reason = file_special_cannot_read(FileName, IO_Error)
        ),
        Error = option_error(Flag, OptName, Reason),
        FileOptionsOrError = file_options_error(Error)
    ).

%---------------------------------------------------------------------------%

:- pred process_option_values(
    option_ops_special(OptionType, UserDataType)::in(option_ops_special),
    list(option_value(OptionType))::in(list_skel(non_file_special)),
    maybe_option_error(OptionType)::out,
    option_table(OptionType)::in, option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out,
    UserDataType::in, UserDataType::out) is det.

process_option_values(_SpecialHandler, [], no_option_error,
        !OptionTable, !OptionsSet, !UserData).
process_option_values(SpecialHandler, [OV | OVs], MaybeError,
        !OptionTable, !OptionsSet, !UserData) :-
    (
        (
            OV = ov_bool(Flag, _OptName, BoolValue),
            map.set(Flag, bool(BoolValue), !OptionTable)
        ;
            OV = ov_int(Flag, _OptName, IntValue),
            map.set(Flag, int(IntValue), !OptionTable)
        ;
            OV = ov_maybe_int(Flag, _OptName, MaybeIntValue),
            map.set(Flag, maybe_int(MaybeIntValue), !OptionTable)
        ;
            OV = ov_string(Flag, _OptName, StringValue),
            map.set(Flag, string(StringValue), !OptionTable)
        ;
            OV = ov_maybe_string(Flag, _OptName, MaybeStringValue),
            map.set(Flag, maybe_string(MaybeStringValue), !OptionTable)
        ;
            OV = ov_accumulating(Flag, _OptName, StringValue),
            map.lookup(!.OptionTable, Flag, Value0),
            ( if Value0 = accumulating(AccStrings0) then
                AccStrings = AccStrings0 ++ [StringValue],
                map.set(Flag, accumulating(AccStrings), !OptionTable)
            else
                % OV can be ov_accumulating only if the entry for Flag
                % in the option_table is accumulating.
                error($pred, "ov_accumulating but not accumulating")
            )
        ;
            OV = ov_accumulating_reset(Flag, _OptName),
            map.set(Flag, accumulating([]), !OptionTable)
        ),
        set.insert(Flag, !OptionsSet),
        process_option_values(SpecialHandler, OVs, MaybeError,
            !OptionTable, !OptionsSet, !UserData)
    ;
        (
            OV = ov_special(Flag, OptName),
            OptionData = none
        ;
            OV = ov_bool_special(Flag, OptName, BoolValue),
            OptionData = bool(BoolValue)
        ;
            OV = ov_int_special(Flag, OptName, IntValue),
            OptionData = int(IntValue)
        ;
            OV = ov_string_special(Flag, OptName, StringValue),
            OptionData = string(StringValue)
        ;
            OV = ov_maybe_string_special(Flag, OptName, MaybeStringValue),
            OptionData = maybe_string(MaybeStringValue)
        ),
        set.insert(Flag, !OptionsSet),
        process_special_option(SpecialHandler, Flag, OptName, OptionData,
            MaybeSpecialError, !OptionTable, !OptionsSet, !UserData),
        (
            MaybeSpecialError = no_option_error,
            process_option_values(SpecialHandler, OVs, MaybeError,
                !OptionTable, !OptionsSet, !UserData)
        ;
            MaybeSpecialError = found_option_error(_),
            MaybeError = MaybeSpecialError
        )
    ).

:- pred process_special_option(
    option_ops_special(OptionType, UserDataType)::in(option_ops_special),
    OptionType::in, string::in, special_data::in,
    maybe_option_error(OptionType)::out,
    option_table(OptionType)::in, option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out,
    UserDataType::in, UserDataType::out) is det.

process_special_option(SpecialHandler, Flag, OptName, OptionData,
        MaybeError, !OptionTable, !OptionsSet, !UserData) :-
    (
        SpecialHandler = none,
        Error = option_error(Flag, OptName, special_handler_missing),
        MaybeError = found_option_error(Error)
    ;
        SpecialHandler = notrack(Handler),
        ( if
            Handler(Flag, OptionData, !.OptionTable, Result0)
        then
            (
                Result0 = ok(NewOptionTable),
                !:OptionTable = NewOptionTable,
                MaybeError = no_option_error
            ;
                Result0 = error(HandlerMsg),
                Reason = special_handler_error(HandlerMsg),
                Error = option_error(Flag, OptName, Reason),
                MaybeError = found_option_error(Error)
                % Leave !OptionTable as it was before the error.
            )
        else
            Error = option_error(Flag, OptName, special_handler_failed),
            MaybeError = found_option_error(Error)
        )
    ;
        SpecialHandler = track(TrackHandler),
        ( if
            TrackHandler(Flag, OptionData, !.OptionTable, Result0,
                NewOptionsSet)
        then
            set.union(NewOptionsSet, !OptionsSet),
            (
                Result0 = ok(NewOptionTable),
                !:OptionTable = NewOptionTable,
                MaybeError = no_option_error
            ;
                Result0 = error(TrackHandlerMsg),
                Reason = special_handler_error(TrackHandlerMsg),
                Error = option_error(Flag, OptName, Reason),
                MaybeError = found_option_error(Error)
                % Leave !OptionTable as it was before the error.
            )
        else
            Error = option_error(Flag, OptName, special_handler_failed),
            MaybeError = found_option_error(Error)
        )
    ;
        SpecialHandler = userdata(UserDataHandler),
        ( if
            UserDataHandler(Flag, OptionData, !.OptionTable, Result0,
                !.UserData, NewUserData)
        then
            (
                Result0 = ok(NewOptionTable),
                MaybeError = no_option_error,
                !:OptionTable = NewOptionTable,
                !:UserData = NewUserData
            ;
                Result0 = error(UserDataHandlerMsg),
                Reason = special_handler_error(UserDataHandlerMsg),
                Error = option_error(Flag, OptName, Reason),
                MaybeError = found_option_error(Error)
                % Leave !OptionTable/!UserData as they were before the error.
            )
        else
            Error = option_error(Flag, OptName, special_handler_failed),
            MaybeError = found_option_error(Error)
        )
    ).

%---------------------------------------------------------------------------%

:- pred numeric_argument_error(OptionType::in, string::in, string::in,
    maybe_option_error(OptionType)::out) is det.

numeric_argument_error(Flag, OptName, Arg, MaybeError) :-
    Reason = requires_numeric_argument(Arg),
    Error = option_error(Flag, OptName, Reason),
    MaybeError = found_option_error(Error).

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
            Reason = file_special_but_no_io(FileName),
            Msg = "the option processing predicate has no access to I/O",
            string.format("cannot open %s: %s", [s(FileName), s(Msg)], String)
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
        ;
            Reason = file_special_circular_inclusion(FileName),
            string.format("%s includes itself either directly or indirectly",
                [s(FileName)], String)
        )
    ).

convert_to_maybe_option_table(MaybeStructuredError) = MaybeError :-
    (
        MaybeStructuredError = ok(OptionTable),
        MaybeError = ok(OptionTable)
    ;
        MaybeStructuredError = error(Error),
        MaybeError = error(option_error_to_string(Error))
    ).

%---------------------------------------------------------------------------%

:- type go_cord(T)
    --->    go_empty_cord
    ;       go_nonempty_cord(go_cord_node(T)).

:- inst go_cord_skel(I) for go_cord/1
    --->    go_empty_cord
    ;       go_nonempty_cord(go_cord_node_skel(I)).

:- type go_cord_node(T)
    --->    go_unit_node(T)
    ;       go_list_node(T, list(T))
    ;       go_branch_node(go_cord_node(T), go_cord_node(T)).

:- inst go_cord_node_skel(I) for go_cord_node/1
    --->    go_unit_node(I)
    ;       go_list_node(I, list_skel(I))
    ;       go_branch_node(go_cord_node_skel(I), go_cord_node_skel(I)).

:- func go_cord_to_list(go_cord(T)::in(go_cord_skel(I =< ground)))
    = (list(T)::out(list_skel(I =< ground))) is det.

go_cord_to_list(go_empty_cord) = [].
go_cord_to_list(go_nonempty_cord(N)) = go_cord_to_list_2([N], []).

:- func go_cord_to_list_2(
    list(go_cord_node(T))::in(list_skel(go_cord_node_skel(I =< ground))),
    list(T)::in(list_skel(I =< ground)))
    = (list(T)::out(list_skel(I =< ground))) is det.

go_cord_to_list_2([], L) = L.
go_cord_to_list_2([N | Ns], L0) = L :-
    (
        N = go_unit_node(X),
        L = go_cord_to_list_2(Ns, [X | L0])
    ;
        N = go_list_node(H, T),
        TL0 = list.inst_preserving_append(T, L0),
        L = go_cord_to_list_2(Ns, [H | TL0])
    ;
        N = go_branch_node(A, B),
        L = go_cord_to_list_2([B, A | Ns], L0)
    ).

:- pred go_cord_append(
    go_cord(T)::in(go_cord_skel(I =< ground)),
    go_cord(T)::in(go_cord_skel(I =< ground)),
    go_cord(T)::out(go_cord_skel(I =< ground))) is det.

go_cord_append(A, B, C) :-
    (
        A = go_empty_cord,
        C = B
    ;
        A = go_nonempty_cord(_),
        B = go_empty_cord,
        C = A
    ;
        A = go_nonempty_cord(AN),
        B = go_nonempty_cord(BN),
        C = go_nonempty_cord(go_branch_node(AN, BN))
    ).

:- pred cord_snoc(T::in(I =< ground),
    go_cord(T)::in(go_cord_skel(I =< ground)),
     go_cord(T)::out(go_cord_skel(I =< ground))) is det.

cord_snoc(X, C, CX) :-
    (
        C = go_empty_cord,
        CX = go_nonempty_cord(go_unit_node(X))
    ;
        C = go_nonempty_cord(N),
        CX = go_nonempty_cord(go_branch_node(N, go_unit_node(X)))
    ).

%---------------------------------------------------------------------------%
:- end_module getopt_io.
%---------------------------------------------------------------------------%
