%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: string.from_int.m.
% Main author: juliensf.
%
% This module includes the non-interface parts of operations
% that convert from integers to strings.
%
%---------------------------------------------------------------------------%

:- module string.from_int.
:- interface.

:- import_module char.
:- import_module list.

    % int_to_base_string_loop(NegN, Base, !RevChars):
    %
    % Convert the integer NegN, which must be strictly negative,
    % to its base Base string representation. Add the characters of
    % that string representation in reverse order in front of !.RevChars.
    %
:- pred int_to_base_string_loop(int::in, int::in,
    list(char)::in, list(char)::out) is det.

    % int_to_base_string_group_loop(NegN, Base, Curr, GroupLength, Sep, Str):

    % int_to_base_string_group_loop/6 has almost the same job as
    % int_to_base_string_loop/3 above, so any changes here might also
    % need to be applied to int_to_base_string_loop/3.
    %
    % The difference between the two predicates is that this one
    % also adds the separator Sep every GroupLength digits.
    % Curr is how many digits have been processed since the last separator
    % was inserted. This is an internal-use counter; our caller in string.m
    % should always pass zero.
    %
:- pred int_to_base_string_group_loop(int::in, int::in, int::in, int::in,
    string::in, string::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

int_to_base_string_loop(NegN, Base, !RevChars) :-
    % int_to_base_string_loop/3 is almost identical to
    % int_to_base_string_group_loop/6 below so any changes here might
    % also need to be applied to int_to_base_string_group_loop/3.
    ( if NegN > -Base then
        N = -NegN,
        DigitChar = char.det_base_int_to_digit(Base, N),
        !:RevChars = [DigitChar | !.RevChars]
    else
        NegN1 = NegN // Base,
        N10 = (NegN1 * Base) - NegN,
        DigitChar = char.det_base_int_to_digit(Base, N10),
        int_to_base_string_loop(NegN1, Base, !RevChars),
        !:RevChars = [DigitChar | !.RevChars]
    ).

%---------------------------------------------------------------------------%

int_to_base_string_group_loop(NegN, Base, Curr, GroupLength, Sep, Str) :-
    ( if
        Curr = GroupLength,
        GroupLength > 0
    then
        int_to_base_string_group_loop(NegN, Base, 0, GroupLength, Sep, Str1),
        string.append(Str1, Sep, Str)
    else
        ( if NegN > -Base then
            N = -NegN,
            DigitChar = char.det_base_int_to_digit(Base, N),
            string.char_to_string(DigitChar, Str)
        else
            NegN1 = NegN // Base,
            N10 = (NegN1 * Base) - NegN,
            DigitChar = char.det_base_int_to_digit(Base, N10),
            string.char_to_string(DigitChar, DigitString),
            int_to_base_string_group_loop(NegN1, Base, Curr + 1,
                GroupLength, Sep, Str1),
            string.append(Str1, DigitString, Str)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module string.from_int.
%---------------------------------------------------------------------------%
