%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
% This code was taken from library/getopt.m, and simplified.
%
% Name: agc_ho_pred.m
%
% Description of bug:
%   The liveness of a typeinfo variable was being computed
%   incorrectly.
%   This was caused because the variable TypeInfo_for_OptionType was
%   incorrectly added to the initial_liveness. It was assumed that
%   all type variables for the types of the input arguments would
%   have corresponding typeinfos as input. This is incorrect, higher
%   order preds do not have typeinfo variables as input.
%
% Symptom(s) of bug:
%   % Generating code for predicate
%       `agc_ho_pred:agc_ho_pred__LambdaGoal__1/2'
%   Software error: variable TypeInfo_for_OptionType (16) not found

:- module agc_ho_pred.
:- interface.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module solutions.

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
            ).

:- inst option_ops ==
    bound((
        option_ops(
            pred(in, out) is semidet,   % short_option
            pred(in, out) is semidet,   % long_option
            pred(out, out) is nondet    % option_default
        )
    ;   option_ops(
            pred(in, out) is semidet,   % short_option
            pred(in, out) is semidet,   % long_option
            pred(out, out) is nondet,   % option_default
            pred(in, in, in, out) is semidet% special handler
        )
    )).

:- type option_data
    --->    bool(bool)
    ;       int(int)
    ;       string(string)
    ;       maybe_string(maybe(string))
    ;       accumulating(list(string))
    ;       special
    ;       bool_special
    ;       int_special
    ;       string_special.

:- type special_data
    --->    none
    ;       bool(bool)
    ;       int(int)
    ;       string(string).

:- type option_table(OptionType) ==  map(OptionType, option_data).

:- type maybe_option_table(OptionType)
    --->    ok(option_table(OptionType))
    ;       error(string).

:- pred agc_ho_pred__process_options(option_ops(OptionType)::in(option_ops),
    list(pair(OptionType, option_data))::out) is det.

:- implementation.

agc_ho_pred__process_options(OptionOps, Args) :-
    (
        OptionOps = option_ops(_, _, OptionDefaultsPred)
    ;
        OptionOps = option_ops(_, _, OptionDefaultsPred, _)
    ),
    solutions(
        ( pred(OptionDataPair::out) is nondet :-
            OptionDataPair = Option - OptionData,
            call(OptionDefaultsPred, Option, OptionData)
        ), OptionDefaultsList),
    Args = OptionDefaultsList.
