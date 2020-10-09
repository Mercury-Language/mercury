%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1999,2001-2007, 2011 The University of Melbourne.
% Copyright (C) 2014-2018,2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: getopt.m.
% Authors: fjh, zs.
% Stability: medium.
%
% This module defines predicates that parse command-line options.
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
%   - file_special (in the predicate variants that do I/O; see below)
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
% option, that predicate will report an error for that option.
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
% NOTE_TO_IMPLEMENTORS: getopt.m should be the same as getopt.m
% NOTE_TO_IMPLEMENTORS: with s/getopt/getopt/ applied.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module getopt.
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
                pred(char, OptionType),                     % short_option
                pred(string, OptionType),                   % long_option
                pred(OptionType, option_data)               % option_default
            )
    ;       option_ops(
                pred(char, OptionType),                     % short_option
                pred(string, OptionType),                   % long_option
                pred(OptionType, option_data),              % option_default
                pred(OptionType, special_data,              % special handler
                    option_table(OptionType),
                    maybe_option_table(OptionType))
            )
    ;       option_ops_multi(
                pred(char, OptionType),                     % short_option
                pred(string, OptionType),                   % long_option
                pred(OptionType, option_data)               % option_default
            )
    ;       option_ops_multi(
                pred(char, OptionType),                     % short_option
                pred(string, OptionType),                   % long_option
                pred(OptionType, option_data),              % option_default
                pred(OptionType, special_data,              % special handler
                    option_table(OptionType),
                    maybe_option_table(OptionType))
            ).

:- inst option_ops for option_ops/1
    --->    option_ops(
                pred(in, out) is semidet,                   % short_option
                pred(in, out) is semidet,                   % long_option
                pred(out, out) is nondet                    % option_default
            )
    ;       option_ops_multi(
                pred(in, out) is semidet,                   % short_option
                pred(in, out) is semidet,                   % long_option
                pred(out, out) is multi                     % option_default
            )
    ;       option_ops(
                pred(in, out) is semidet,                   % short_option
                pred(in, out) is semidet,                   % long_option
                pred(out, out) is nondet,                   % option_default
                pred(in, in, in, out) is semidet            % special handler
            )
    ;       option_ops_multi(
                pred(in, out) is semidet,                   % short_option
                pred(in, out) is semidet,                   % long_option
                pred(out, out) is multi,                    % option_default
                pred(in, in, in, out) is semidet            % special handler
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
                pred(char, OptionType),                     % short_option
                pred(string, OptionType),                   % long_option
                pred(OptionType, special_data,              % special handler
                    option_table(OptionType),
                    maybe_option_table(OptionType),
                    set(OptionType))
            ).

:- inst option_ops_track for option_ops_track/1
    --->    option_ops_track(
                pred(in, out) is semidet,                   % short_option
                pred(in, out) is semidet,                   % long_option
                pred(in, in, in, out, out) is semidet       % special handler
            ).

%---------------------%

    % A version of the option_ops type for the process_options_userdata
    % predicate and its process_options_userdata_se variant.
    %
:- type option_ops_userdata(OptionType, UserDataType)
    --->    option_ops_userdata(
                pred(char, OptionType),                     % short_option
                pred(string, OptionType),                   % long_option
                pred(OptionType, special_data,              % special handler
                    option_table(OptionType),
                    maybe_option_table(OptionType),
                    UserDataType, UserDataType)
            ).

:- inst option_ops_userdata for option_ops_userdata/2
    --->    option_ops_userdata(
                pred(in, out) is semidet,                   % short_option
                pred(in, out) is semidet,                   % long_option
                pred(in, in, in, out, in, out) is semidet   % special handler
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

    ;       file_special_contains_non_option_args(string).
            % The option is a file_special option whose argument is the file
            % named by the argument. This file contained some non-option
            % arguments.

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

:- import_module pair.
:- import_module require.
:- import_module solutions.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

% Please keep the differences between this module and getopt.m to the
% minimum. Most changes should done in both modules.

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

process_options(OptionOps, Args0, NonOptionArgs, Result) :-
    process_options(OptionOps, Args0, _OptionArgs, NonOptionArgs, Result).

process_options(OptionOps, Args0, OptionArgs, NonOptionArgs, Result) :-
    option_ops_to_internal_and_option_table(OptionOps, Internal, OptionTable0),
    process_arguments(Internal, Args0, NonOptionArgs,
        [], RevOptionArgs, MaybeError, OptionTable0, OptionTable,
        set.init, _OptionsSet, unit, _, unit, _),
    OptionArgs = list.reverse(RevOptionArgs),
    return_option_table_if_ok(MaybeError, OptionTable, Result).

process_options_io(OptionOps, Args0, NonOptionArgs, Result, !IO) :-
    process_options_io(OptionOps, Args0, _OptionArgs, NonOptionArgs,
        Result, !IO).

process_options_io(OptionOps, Args0, OptionArgs, NonOptionArgs, Result, !IO) :-
    option_ops_to_internal_and_option_table(OptionOps, Internal, OptionTable0),
    process_arguments(Internal, Args0, NonOptionArgs,
        [], RevOptionArgs, MaybeError, OptionTable0, OptionTable,
        set.init, _OptionsSet, unit, _, !IO),
    OptionArgs = list.reverse(RevOptionArgs),
    return_option_table_if_ok(MaybeError, OptionTable, Result).

process_options_track(OptionOps, Args0, OptionArgs, NonOptionArgs,
        OptionTable0, Result, OptionsSet) :-
    OptionOps = option_ops_track(Short, Long, Special),
    Internal = option_ops_internal(Short, Long, track(Special)),
    process_arguments(Internal, Args0, NonOptionArgs,
        [], RevOptionArgs, MaybeError, OptionTable0, OptionTable,
        set.init, OptionsSet, unit, _, unit, _),
    OptionArgs = list.reverse(RevOptionArgs),
    return_option_table_if_ok(MaybeError, OptionTable, Result).

process_options_track_io(OptionOps, Args0, OptionArgs, NonOptionArgs,
        OptionTable0, Result, OptionsSet, !IO) :-
    OptionOps = option_ops_track(Short, Long, Special),
    Internal = option_ops_internal(Short, Long, track(Special)),
    process_arguments(Internal, Args0, NonOptionArgs,
        [], RevOptionArgs, MaybeError, OptionTable0, OptionTable,
        set.init, OptionsSet, unit, _, !IO),
    OptionArgs = list.reverse(RevOptionArgs),
    return_option_table_if_ok(MaybeError, OptionTable, Result).

process_options_userdata(OptionOps, Args0, OptionArgs, NonOptionArgs,
        MaybeError, OptionsSet, !OptionTable, !UserData) :-
    OptionOps = option_ops_userdata(Short, Long, Special),
    Internal = option_ops_internal(Short, Long, userdata(Special)),
    process_arguments(Internal, Args0, NonOptionArgs,
        [], RevOptionArgs, MaybeError, !OptionTable,
        set.init, OptionsSet, !UserData, unit, _),
    OptionArgs = list.reverse(RevOptionArgs).

process_options_userdata_io(OptionOps, Args0, OptionArgs, NonOptionArgs,
        MaybeError, OptionsSet, !OptionTable, !UserData, !IO) :-
    OptionOps = option_ops_userdata(Short, Long, Special),
    Internal = option_ops_internal(Short, Long, userdata(Special)),
    process_arguments(Internal, Args0, NonOptionArgs,
        [], RevOptionArgs, MaybeError, !OptionTable,
        set.init, OptionsSet, !UserData, !IO),
    OptionArgs = list.reverse(RevOptionArgs).

%---------------------%

:- pred option_ops_to_internal_and_option_table(
    option_ops(OptionType)::in(option_ops),
    option_ops_internal(OptionType, UserDataType)::out(option_ops_internal),
    option_table(OptionType)::out) is det.

option_ops_to_internal_and_option_table(OptionOps, Internal, OptionTable0) :-
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
    Internal = option_ops_internal(Short, Long, MaybeSpecial).

:- pred return_option_table_if_ok(maybe(option_error(OptionType))::in,
    option_table(OptionType)::in, maybe_option_table_se(OptionType)::out)
    is det.

return_option_table_if_ok(MaybeError, OptionTable, Result) :-
    (
        MaybeError = no,
        Result = ok(OptionTable)
    ;
        MaybeError = yes(Error),
        Result = error(Error)
    ).

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

:- pred process_arguments(
    option_ops_internal(OptionType, UserDataType)::in(option_ops_internal),
    list(string)::in, list(string)::out,
    list(string)::in, list(string)::out,
    maybe(option_error(OptionType))::out,
    option_table(OptionType)::in, option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out,
    UserDataType::in, UserDataType::out, MaybeIO::di, MaybeIO::uo) is det
    <= read_file_contents(MaybeIO).

process_arguments(_, [], [], !OptionArgs,
        no, !OptionTable, !OptionsSet, !UserData, !MaybeIO).
process_arguments(OptionOps, [Arg0 | Args0], NonOptionArgs, !OptionArgs,
        MaybeError, !OptionTable, !OptionsSet, !UserData, !MaybeIO) :-
    ( if
        Arg0 = "--"
    then
        % "--" terminates option processing
        NonOptionArgs = Args0,
        MaybeError = no
    else if
        string.append("--no-", LongOption, Arg0)
    then
        LongOptionPred = OptionOps ^ long_option,
        ( if LongOptionPred(LongOption, Flag) then
            OptName = "--" ++ LongOption,
            process_negated_option(OptionOps, OptName, Flag,
                NegMaybeError, !.OptionTable, NewOptionTable,
                !OptionsSet, !UserData),
            (
                NegMaybeError = no,
                !:OptionTable = NewOptionTable,
                !:OptionArgs = [Arg0 | !.OptionArgs],
                process_arguments(OptionOps, Args0, NonOptionArgs, !OptionArgs,
                    MaybeError, !OptionTable, !OptionsSet, !UserData, !MaybeIO)
            ;
                NegMaybeError = yes(_),
                MaybeError = NegMaybeError,
                NonOptionArgs = Args0
            )
        else
            Error = unrecognized_option(Arg0),
            MaybeError = yes(Error),
            NonOptionArgs = Args0
        )
    else if
        string.append("--", LongOptionStr, Arg0)
    then
        LongOptionPred = OptionOps ^ long_option,
        ( if string.sub_string_search(LongOptionStr, "=", OptionLen) then
            string.split(LongOptionStr, OptionLen, LongOption, EqualOptionArg),
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
            ( if map.search(!.OptionTable, Flag, OptionData) then
                !:OptionArgs = [Arg0 | !.OptionArgs],
                handle_long_option(OptionOps, OptionName, Flag, OptionData,
                    MaybeArg, Args0, Args1, !OptionArgs,
                    LongMaybeError, !.OptionTable, NewOptionTable,
                    !OptionsSet, !UserData, !MaybeIO),
                (
                    LongMaybeError = no,
                    !:OptionTable = NewOptionTable,
                    process_arguments(OptionOps, Args1, NonOptionArgs,
                        !OptionArgs, MaybeError, !OptionTable,
                        !OptionsSet, !UserData, !MaybeIO)
                ;
                    LongMaybeError = yes(_),
                    MaybeError = LongMaybeError,
                    NonOptionArgs = Args0
                )
            else
                Error = option_error(Flag, Arg0, unknown_type),
                MaybeError = yes(Error),
                NonOptionArgs = Args0
            )
        else
            Error = unrecognized_option(OptionName),
            MaybeError = yes(Error),
            NonOptionArgs = Args0
        )
    else if
        string.first_char(Arg0, '-', ShortOptions),
        ShortOptions \= ""
    then
        string.to_char_list(ShortOptions, ShortOptionsList),
        % Process a single negated option `-x-'.
        ( if ShortOptionsList = [SingleShortOpt, '-'] then
            ShortOptionPred = OptionOps ^ short_option,
            ( if ShortOptionPred(SingleShortOpt, Flag) then
                string.from_char_list(['-', SingleShortOpt], OptName),
                process_negated_option(OptionOps, OptName, Flag,
                    NegMaybeError, !.OptionTable, NewOptionTable,
                    !OptionsSet, !UserData),
                (
                    NegMaybeError = no,
                    !:OptionTable = NewOptionTable,
                    !:OptionArgs = [Arg0 | !.OptionArgs],
                    process_arguments(OptionOps, Args0, NonOptionArgs,
                        !OptionArgs, MaybeError, !OptionTable,
                        !OptionsSet, !UserData, !MaybeIO)
                ;
                    NegMaybeError = yes(_),
                    MaybeError = NegMaybeError,
                    NonOptionArgs = Args0
                )
            else
                Error = unrecognized_option("-" ++ ShortOptions),
                MaybeError = yes(Error),
                NonOptionArgs = Args0
            )
        else
            % Process a list of options `-xyz'.
            % -xyz may be several boolean options,
            % or part of it may be the argument of an option.
            % The first element of Args0 may also be an argument of an option.
            !:OptionArgs = [Arg0 | !.OptionArgs],
            handle_short_options(OptionOps, ShortOptionsList, Args0, Args1,
                !OptionArgs, ShortMaybeError, !.OptionTable, NewOptionTable,
                !OptionsSet, !UserData, !MaybeIO),
            (
                ShortMaybeError = no,
                !:OptionTable = NewOptionTable,
                process_arguments(OptionOps, Args1, NonOptionArgs, !OptionArgs,
                    MaybeError, !OptionTable, !OptionsSet, !UserData, !MaybeIO)
            ;
                ShortMaybeError = yes(_),
                MaybeError = ShortMaybeError,
                NonOptionArgs = Args0
            )
        )
    else
        % It is a normal non-option argument.
        % As a GNU extension, keep searching for options
        % in the remaining arguments.
        process_arguments(OptionOps, Args0, NonOptionArgsTail, !OptionArgs,
            MaybeError, !OptionTable, !OptionsSet, !UserData, !MaybeIO),
        NonOptionArgs = [Arg0 | NonOptionArgsTail]
    ).

:- pred handle_long_option(
    option_ops_internal(OptionType, UserDataType)::in(option_ops_internal),
    string::in, OptionType::in, option_data::in,
    maybe(string)::in, list(string)::in, list(string)::out,
    list(string)::in, list(string)::out,
    maybe(option_error(OptionType))::out,
    option_table(OptionType)::in, option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out,
    UserDataType::in, UserDataType::out, MaybeIO::di, MaybeIO::uo) is det
    <= read_file_contents(MaybeIO).

handle_long_option(OptionOps, Option, Flag, OptionData, MaybeOptionArg0,
        Args0, Args1, !OptionArgs, MaybeError, !OptionTable,
        !OptionsSet, !UserData, !MaybeIO) :-
    need_arg(OptionData, NeedArg),
    ( if
        NeedArg = yes,
        MaybeOptionArg0 = no
    then
        (
            Args0 = [Arg | ArgsTail],
            MaybeOptionArg = yes(Arg),
            Args1 = ArgsTail,
            MissingArg = no,
            !:OptionArgs = [Arg | !.OptionArgs]
        ;
            Args0 = [],
            MaybeOptionArg = no,
            Args1 = Args0,
            MissingArg = yes
        )
    else
        MaybeOptionArg = MaybeOptionArg0,
        Args1 = Args0,
        MissingArg = no
    ),
    (
        MissingArg = yes,
        Error = option_error(Flag, Option, requires_argument),
        MaybeError = yes(Error)
    ;
        MissingArg = no,
        ( if
            NeedArg = no,
            MaybeOptionArg = yes(ArgVal)
        then
            ErrorReason = does_not_allow_argument(ArgVal),
            Error = option_error(Flag, Option, ErrorReason),
            MaybeError = yes(Error)
        else
            process_option(OptionOps, OptionData, Option, Flag, MaybeOptionArg,
                MaybeError, !OptionTable, !OptionsSet, !UserData, !MaybeIO)
        )
    ).

:- pred handle_short_options(
    option_ops_internal(OptionType, UserDataType)::in(option_ops_internal),
    list(char)::in, list(string)::in, list(string)::out,
    list(string)::in, list(string)::out,
    maybe(option_error(OptionType))::out,
    option_table(OptionType)::in, option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out,
    UserDataType::in, UserDataType::out, MaybeIO::di, MaybeIO::uo) is det
    <= read_file_contents(MaybeIO).

handle_short_options(_, [], Args, Args, !OptionArgs,
        no, !OptionTable, !OptionsSet, !UserData, !MaybeIO).
handle_short_options(OptionOps, [Opt | Opts0], Args0, Args,
        !OptionArgs, MaybeError, !OptionTable, !OptionsSet,
        !UserData, !MaybeIO) :-
    ShortOptionPred = OptionOps ^ short_option,
    ( if ShortOptionPred(Opt, Flag) then
        ( if map.search(!.OptionTable, Flag, OptionData) then
            ( if need_arg(OptionData, yes) then
                get_short_option_arg(Opts0, Arg, Args0, Args1, !OptionArgs),
                MaybeOptionArg = yes(Arg),
                Opts1 = []
            else
                MaybeOptionArg = no,
                Opts1 = Opts0,
                Args1 = Args0
            ),
            string.from_char_list(['-', Opt], Option),
            process_option(OptionOps, OptionData, Option, Flag, MaybeOptionArg,
                OptionMaybeError, !.OptionTable, NewOptionTable,
                !OptionsSet, !UserData, !MaybeIO),
            (
                OptionMaybeError = no,
                !:OptionTable = NewOptionTable,
                handle_short_options(OptionOps, Opts1, Args1, Args,
                    !OptionArgs, MaybeError, !OptionTable,
                    !OptionsSet, !UserData, !MaybeIO)
            ;
                OptionMaybeError = yes(_),
                Args = Args1,
                MaybeError = OptionMaybeError
            )
        else
            string.char_to_string(Opt, OptString),
            Args = Args0,
            Error = option_error(Flag, "-" ++ OptString, unknown_type),
            MaybeError = yes(Error)
        )
    else
        Args = Args0,
        string.char_to_string(Opt, OptString),
        Error = unrecognized_option("-" ++ OptString),
        MaybeError = yes(Error)
    ).

:- pred get_short_option_arg(list(char)::in, string::out,
    list(string)::in, list(string)::out, list(string)::in, list(string)::out)
    is det.

get_short_option_arg(Opts, Arg, Args0, Args, !OptionArgs) :-
    ( if
        Opts = [],
        Args0 = [ArgPrime | ArgsPrime]
    then
        !:OptionArgs = [ArgPrime | !.OptionArgs],
        Arg = ArgPrime,
        Args = ArgsPrime
    else
        string.from_char_list(Opts, Arg),
        Args = Args0
    ).

:- pred process_option(
    option_ops_internal(OptionType, UserDataType)::in(option_ops_internal),
    option_data::in, string::in, OptionType::in, maybe(string)::in,
    maybe(option_error(OptionType))::out,
    option_table(OptionType)::in, option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out,
    UserDataType::in, UserDataType::out, MaybeIO::di, MaybeIO::uo) is det
    <= read_file_contents(MaybeIO).

process_option(OptionOps, OptionData, Option, Flag, MaybeArg,
        MaybeError, !OptionTable, !OptionsSet, !UserData, !MaybeIO) :-
    (
        OptionData = bool(_),
        set.insert(Flag, !OptionsSet),
        (
            MaybeArg = yes(_Arg),
            map.set(Flag, bool(no), !OptionTable),
            MaybeError = no
        ;
            MaybeArg = no,
            map.set(Flag, bool(yes), !OptionTable),
            MaybeError = no
        )
    ;
        OptionData = int(_),
        set.insert(Flag, !OptionsSet),
        (
            MaybeArg = yes(Arg),
            ( if string.to_int(Arg, IntArg) then
                map.set(Flag, int(IntArg), !OptionTable),
                MaybeError = no
            else
                numeric_argument_error(Flag, Option, Arg, MaybeError)
            )
        ;
            MaybeArg = no,
            error("integer argument expected in getopt.process_option")
        )
    ;
        OptionData = string(_),
        set.insert(Flag, !OptionsSet),
        (
            MaybeArg = yes(Arg),
            map.set(Flag, string(Arg), !OptionTable),
            MaybeError = no
        ;
            MaybeArg = no,
            error("string argument expected in getopt.process_option")
        )
    ;
        OptionData = maybe_int(_),
        set.insert(Flag, !OptionsSet),
        (
            MaybeArg = yes(Arg),
            ( if string.to_int(Arg, IntArg) then
                map.set(Flag, maybe_int(yes(IntArg)), !OptionTable),
                MaybeError = no
            else
                numeric_argument_error(Flag, Option, Arg, MaybeError)
            )
        ;
            MaybeArg = no,
            error("integer argument expected in getopt.process_option")
        )
    ;
        OptionData = maybe_string(_),
        set.insert(Flag, !OptionsSet),
        (
            MaybeArg = yes(Arg),
            map.set(Flag, maybe_string(yes(Arg)), !OptionTable),
            MaybeError = no
        ;
            MaybeArg = no,
            error("string argument expected in getopt.process_option")
        )
    ;
        OptionData = accumulating(List0),
        set.insert(Flag, !OptionsSet),
        (
            MaybeArg = yes(Arg),
            List = List0 ++ [Arg],
            map.set(Flag, accumulating(List), !OptionTable),
            MaybeError = no
        ;
            MaybeArg = no,
            error("acumulating argument expected in getopt.process_option")
        )
    ;
        OptionData = special,
        set.insert(Flag, !OptionsSet),
        (
            MaybeArg = yes(_Arg),
            error("no special argument expected in getopt.process_option")
        ;
            MaybeArg = no,
            process_special_option(OptionOps, Option, Flag, none,
                MaybeError, !OptionTable, !OptionsSet, !UserData)
        )
    ;
        OptionData = bool_special,
        set.insert(Flag, !OptionsSet),
        (
            MaybeArg = yes(_Arg),
            process_special_option(OptionOps, Option, Flag, bool(no),
                MaybeError, !OptionTable, !OptionsSet, !UserData)
        ;
            MaybeArg = no,
            process_special_option(OptionOps, Option, Flag, bool(yes),
                MaybeError, !OptionTable, !OptionsSet, !UserData)
        )
    ;
        OptionData = int_special,
        set.insert(Flag, !OptionsSet),
        (
            MaybeArg = yes(Arg),
            ( if string.to_int(Arg, IntArg) then
                process_special_option(OptionOps, Option, Flag, int(IntArg),
                    MaybeError, !OptionTable, !OptionsSet, !UserData)
            else
                numeric_argument_error(Flag, Option, Arg, MaybeError)
            )
        ;
            MaybeArg = no,
            error("int_special argument expected in getopt.process_option")
        )
    ;
        OptionData = string_special,
        set.insert(Flag, !OptionsSet),
        (
            MaybeArg = yes(Arg),
            process_special_option(OptionOps, Option, Flag, string(Arg),
                MaybeError, !OptionTable, !OptionsSet, !UserData)
        ;
            MaybeArg = no,
            error("string_special argument expected " ++
                "in getopt.process_option")
        )
    ;
        OptionData = maybe_string_special,
        (
            MaybeArg = yes(_Arg),
            process_special_option(OptionOps, Option, Flag,
                maybe_string(MaybeArg),
                MaybeError, !OptionTable, !OptionsSet, !UserData)
        ;
            MaybeArg = no,
            error("maybe_string_special argument expected " ++
                "in getopt.process_option")
        )
    ;
        OptionData = file_special,
        (
            MaybeArg = yes(FileName),
            read_file_contents(FileName, ReadFromFileResult, !MaybeIO),
            (
                ReadFromFileResult = read_success(Contents),
                Words = string.words(Contents),
                process_arguments(OptionOps, Words, Args, [], _OptionArgs,
                    FileMaybeError, !.OptionTable, NewOptionTable,
                    !OptionsSet, !UserData, !MaybeIO),
                (
                    FileMaybeError = no,
                    (
                        Args = [],
                        !:OptionTable = NewOptionTable,
                        MaybeError = no
                    ;
                        Args = [_ | _],
                        Reason =
                            file_special_contains_non_option_args(FileName),
                        Error = option_error(Flag, Option, Reason),
                        MaybeError = yes(Error)
                    )
                ;
                    FileMaybeError = yes(_),
                    MaybeError = FileMaybeError
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
                Error = option_error(Flag, Option, Reason),
                MaybeError = yes(Error)
            )
        ;
            MaybeArg = no,
            error("file_special argument expected in getopt.process_option")
        )
    ).

:- pred process_negated_option(
    option_ops_internal(OptionType, UserDataType)::in(option_ops_internal),
    string::in, OptionType::in,
    maybe(option_error(OptionType))::out,
    option_table(OptionType)::in, option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out,
    UserDataType::in, UserDataType::out) is det.

process_negated_option(OptionOps, Option, Flag, MaybeError,
        !OptionTable, !OptionsSet, !UserData) :-
    ( if map.search(!.OptionTable, Flag, OptionData) then
        (
            OptionData = bool(_),
            set.insert(Flag, !OptionsSet),
            map.set(Flag, bool(no), !OptionTable),
            MaybeError = no
        ;
            OptionData = maybe_int(_),
            set.insert(Flag, !OptionsSet),
            map.set(Flag, maybe_int(no), !OptionTable),
            MaybeError = no
        ;
            OptionData = maybe_string(_),
            set.insert(Flag, !OptionsSet),
            map.set(Flag, maybe_string(no), !OptionTable),
            MaybeError = no
        ;
            OptionData = accumulating(_),
            set.insert(Flag, !OptionsSet),
            map.set(Flag, accumulating([]), !OptionTable),
            MaybeError = no
        ;
            OptionData = bool_special,
            set.insert(Flag, !OptionsSet),
            process_special_option(OptionOps, Option, Flag, bool(no),
                MaybeError, !OptionTable, !OptionsSet, !UserData)
        ;
            OptionData = maybe_string_special,
            set.insert(Flag, !OptionsSet),
            process_special_option(OptionOps, Option, Flag, maybe_string(no),
                MaybeError, !OptionTable, !OptionsSet, !UserData)
        ;
            ( OptionData = int_special
            ; OptionData = string_special
            ; OptionData = int(_)
            ; OptionData = string(_)
            ; OptionData = special
            ; OptionData = file_special
            ),
            Error = option_error(Flag, Option, cannot_negate),
            MaybeError = yes(Error)
        )
    else
        Error = option_error(Flag, Option, unknown_type),
        MaybeError = yes(Error)
    ).

:- pred process_special_option(
    option_ops_internal(OptionType, UserDataType)::in(option_ops_internal),
    string::in, OptionType::in, special_data::in,
    maybe(option_error(OptionType))::out,
    option_table(OptionType)::in, option_table(OptionType)::out,
    set(OptionType)::in, set(OptionType)::out,
    UserDataType::in, UserDataType::out) is det.

process_special_option(OptionOps, Option, Flag, OptionData,
        MaybeError, !OptionTable, !OptionsSet, !UserData) :-
    MaybeHandler = OptionOps ^ special_handler,
    (
        MaybeHandler = none,
        Error = option_error(Flag, Option, special_handler_missing),
        MaybeError = yes(Error)
    ;
        MaybeHandler = notrack(Handler),
        ( if
            Handler(Flag, OptionData, !.OptionTable, Result0)
        then
            (
                Result0 = ok(NewOptionTable),
                !:OptionTable = NewOptionTable,
                MaybeError = no
            ;
                Result0 = error(HandlerMsg),
                Reason = special_handler_error(HandlerMsg),
                Error = option_error(Flag, Option, Reason),
                MaybeError = yes(Error)
                % Leave !OptionTable as it was before the error.
            )
        else
            Error = option_error(Flag, Option, special_handler_failed),
            MaybeError = yes(Error)
        )
    ;
        MaybeHandler = track(TrackHandler),
        ( if
            TrackHandler(Flag, OptionData, !.OptionTable, Result0,
                NewOptionsSet)
        then
            set.union(NewOptionsSet, !OptionsSet),
            (
                Result0 = ok(NewOptionTable),
                !:OptionTable = NewOptionTable,
                MaybeError = no
            ;
                Result0 = error(TrackHandlerMsg),
                Reason = special_handler_error(TrackHandlerMsg),
                Error = option_error(Flag, Option, Reason),
                MaybeError = yes(Error)
                % Leave !OptionTable as it was before the error.
            )
        else
            Error = option_error(Flag, Option, special_handler_failed),
            MaybeError = yes(Error)
        )
    ;
        MaybeHandler = userdata(UserDataHandler),
        ( if
            UserDataHandler(Flag, OptionData, !.OptionTable, Result0,
                !.UserData, NewUserData)
        then
            (
                Result0 = ok(NewOptionTable),
                MaybeError = no,
                !:OptionTable = NewOptionTable,
                !:UserData = NewUserData
            ;
                Result0 = error(UserDataHandlerMsg),
                Reason = special_handler_error(UserDataHandlerMsg),
                Error = option_error(Flag, Option, Reason),
                MaybeError = yes(Error)
                % Leave !OptionTable/!UserData as they were before the error.
            )
        else
            Error = option_error(Flag, Option, special_handler_failed),
            MaybeError = yes(Error)
        )
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

:- pred numeric_argument_error(OptionType::in, string::in, string::in,
    maybe(option_error(OptionType))::out) is det.

numeric_argument_error(Flag, Option, Arg, MaybeError) :-
    Reason = requires_numeric_argument(Arg),
    Error = option_error(Flag, Option, Reason),
    MaybeError = yes(Error).

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
:- end_module getopt.
%---------------------------------------------------------------------------%
