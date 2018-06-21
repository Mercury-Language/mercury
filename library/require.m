%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-1999, 2003, 2005-2006, 2010-2011 The University of Melbourne.
% Copyright (C) 2014-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: require.m.
% Main author: fjh.
% Stability: medium to high.
%
% This module provides features similar to <assert.h> in C.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module require.
:- interface.

    % error(Message):
    %
    % Throw a `software_error(Message)' exception.
    % This will normally cause execution to abort with an error message.
    %
:- pred error(string::in) is erroneous.

    % func_error(Message):
    %
    % An expression that results in a `software_error(Message)'
    % exception being thrown.
    %
:- func func_error(string) = _ is erroneous.

    % error(Pred, Message):
    % func_error(Pred, Message):
    %
    % Equivalent to invoking error or func_error on the string
    % Pred ++ ": " ++ Message.
    %
:- pred error(string::in, string::in) is erroneous.
:- func func_error(string, string) = _ is erroneous.

%---------------------------------------------------------------------------%

    % sorry(Module, What):
    %
    % Call error/1 with the string
    % "Module: Sorry, not implemented: What".
    %
    % Use this for features that should be implemented (or at least could be
    % implemented).
    %
:- func sorry(string, string) = _ is erroneous.
:- pred sorry(string::in, string::in) is erroneous.

    % sorry(Module, Proc, What):
    %
    % Call error/1 with the string
    % "Module: Proc: Sorry, not implemented: What".
    %
    % Use this for features that should be implemented,
    % or at least could be implemented.
    %
:- func sorry(string, string, string) = _ is erroneous.
:- pred sorry(string::in, string::in, string::in) is erroneous.

    % unexpected(Module, Message):
    %
    % Call error/1 with the string
    % "Module: Unexpected: What".
    %
    % Use this to handle cases which are not expected to arise (i.e. bugs).
    %
:- func unexpected(string, string) = _ is erroneous.
:- pred unexpected(string::in, string::in) is erroneous.

    % unexpected(Module, Proc, Message):
    %
    % Call error/1 with the string
    % "Module: Proc: Unexpected: What".
    %
    % Use this to handle cases which are not expected to arise (i.e. bugs).
    %
:- func unexpected(string, string, string) = _ is erroneous.
:- pred unexpected(string::in, string::in, string::in) is erroneous.

%---------------------------------------------------------------------------%

    % require(Goal, Message):
    %
    % Call goal, and call error(Message) if Goal fails.
    % This is not as useful as you might imagine, since it requires
    % that the goal not produce any output variables. In most circumstances,
    % you should use an explicit if-then-else with a call to error/1,
    % or one of its wrappers, in the "else".
    %
:- pred require((pred)::((pred) is semidet), string::in) is det.

    % expect(Goal, Module, Message):
    %
    % Call Goal, and call unexpected(Module, Message) if Goal fails.
    %
:- pred expect((pred)::((pred) is semidet), string::in, string::in) is det.

    % expect(Goal, Module, Proc, Message):
    %
    % Call Goal, and call unexpected(Module, Proc, Message) if Goal fails.
    %
:- pred expect((pred)::((pred) is semidet), string::in, string::in,
    string::in) is det.

    % expect_not(Goal, Module, Message):
    %
    % Call Goal, and call unexpected(Module, Message) if Goal succeeds.
    %
:- pred expect_not((pred)::((pred) is semidet), string::in, string::in) is det.

    % expect_not(Goal, Module, Proc, Message):
    %
    % Call Goal, and call unexpected(Module, Proc, Message) if Goal succeeds.
    %
:- pred expect_not((pred)::((pred) is semidet), string::in, string::in,
    string::in) is det.

%---------------------------------------------------------------------------%

    % report_lookup_error(Message, Key):
    %
    % Call error/1 with an error message that is appropriate for
    % the failure of a lookup operation involving the specified Key.
    % The error message will include Message and information about Key.
    %
:- pred report_lookup_error(string::in, K::in) is erroneous.

    % report_lookup_error(Message, Key, Value):
    %
    % Call error/1 with an error message that is appropriate for
    % the failure of a lookup operation involving the specified Key and Value.
    % The error message will include Message and information about Key
    % and Value.
    %
:- pred report_lookup_error(string::in, K::in, V::unused) is erroneous.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% :- import_module bool.
:- import_module exception.
:- import_module list.
:- import_module string.
:- import_module type_desc.

%---------------------------------------------------------------------------%

% Hopefully error won't be called often (!), so no point inlining it.
:- pragma no_inline(error/1).
:- pragma no_inline(error/2).
:- pragma no_inline(func_error/1).
:- pragma no_inline(func_error/2).

% We declare error to be terminating so that all of the standard library
% will treat it as terminating.
:- pragma terminates(error/1).
:- pragma terminates(error/2).
:- pragma terminates(func_error/1).
:- pragma terminates(func_error/2).

error(Message) :-
    throw(software_error(Message)).

func_error(Message) = _ :-
    error(Message).

error(Pred, Message) :-
    error(Pred ++ ": " ++ Message).

func_error(Pred, Message) = _ :-
    error(Pred, Message).

%---------------------------------------------------------------------------%

sorry(Module, What) = _ :-
    sorry(Module, What).
sorry(Module, What) :-
    string.format("%s: Sorry, not implemented: %s",
        [s(Module), s(What)], ErrorMessage),
    error(ErrorMessage).

sorry(Module, Proc, What) = _ :-
    sorry(Module, Proc, What).
sorry(Module, Proc, What) :-
    string.format("%s: %s: Sorry, not implemented: %s",
        [s(Module), s(Proc), s(What)], ErrorMessage),
    error(ErrorMessage).

unexpected(Module, What) = _ :-
    unexpected(Module, What).
unexpected(Module, What) :-
    string.format("%s: Unexpected: %s", [s(Module), s(What)], ErrorMessage),
    error(ErrorMessage).

unexpected(Module, Proc, What) = _ :-
    unexpected(Module, Proc, What).
unexpected(Module, Proc, What) :-
    string.format("%s: %s: Unexpected: %s", [s(Module), s(Proc), s(What)],
        ErrorMessage),
    error(ErrorMessage).

%---------------------------------------------------------------------------%

require(Goal, Message) :-
    ( if call(Goal) then
        true
    else
        error(Message)
    ).

expect(Goal, Module, Message) :-
    ( if Goal then
        true
    else
        unexpected(Module, Message)
    ).

expect(Goal, Module, Proc, Message) :-
    ( if Goal then
        true
    else
        unexpected(Module, Proc, Message)
    ).

expect_not(Goal, Module, Message) :-
    ( if Goal then
        unexpected(Module, Message)
    else
        true
    ).

expect_not(Goal, Module, Proc, Message) :-
    ( if Goal then
        unexpected(Module, Proc, Message)
    else
        true
    ).

%---------------------------------------------------------------------------%

report_lookup_error(Msg, K) :-
    KeyType = type_name(type_of(K)),
    string.append_list(
        [Msg,
        "\n\tKey Type: ",
        KeyType,
        "\n\tKey Value: ",
        string(K)
        ],
        ErrorString),
    error(ErrorString).

report_lookup_error(Msg, K, V) :-
    KeyType = type_name(type_of(K)),
    ValueType = type_name(type_of(V)),
    string.append_list(
        [Msg,
        "\n\tKey Type: ",
        KeyType,
        "\n\tKey Value: ",
        string(K),
        "\n\tValue Type: ",
        ValueType
        ],
        ErrorString),
    error(ErrorString).

%---------------------------------------------------------------------------%
:- end_module require.
%---------------------------------------------------------------------------%
