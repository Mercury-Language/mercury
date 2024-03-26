%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2015, 2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%

:- module backend_libs.string_encoding.
:- interface.

:- import_module libs.
:- import_module libs.globals.

:- import_module list.
:- import_module string.

    % target_char_range(Target, Min, Max):
    %
    % Return the smallest and largest integers that represent
    % valid code points in the encoding we use on the given target platform.
    %
:- pred target_char_range(compilation_target::in, int::out, int::out) is det.

    % Return the string_encoding we use on the given target platform.
    %
:- func target_string_encoding(compilation_target) = string_encoding.

    % Convert a string to the list of its code units in the given encoding.
    %
:- pred to_code_unit_list_in_encoding(string_encoding::in, string::in,
    list(int)::out) is det.

    % Convert a list of code units in the given encoding to a string.
    % Fails if the list does not follow the rules of the encoding.
    %
:- pred from_code_unit_list_in_encoding(string_encoding::in, list(int)::in,
    string::out) is semidet.
:- pred det_from_code_unit_list_in_encoding(string_encoding::in, list(int)::in,
    string::out) is det.

    % Convert a list of code units in the given encoding to a string.
    % Allows ill-formed sequences, and will succeed *unless* the given list
    % includes a zero, signifying a null character.
    %
    % At the moment, it works only when the encoding specified by the first
    % argument is utf8, *and* the compiler's own encoding is utf8.
    % If either encoding is utf16, it will throw an exception.
    %
:- pred from_code_unit_list_in_encoding_allow_ill_formed(string_encoding::in,
    list(int)::in, string::out) is semidet.
:- pred det_from_code_unit_list_in_encoding_allow_ill_formed(
    string_encoding::in, list(int)::in, string::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

target_char_range(_Target, Min, Max) :-
    % The range of `char' is the same for all existing targets.
    Min = 0,
    Max = 0x10ffff.

target_string_encoding(Target) = Encoding :-
    (
        Target = target_c,
        Encoding = utf8
    ;
        ( Target = target_java
        ; Target = target_csharp
        ),
        Encoding = utf16
    ).

to_code_unit_list_in_encoding(Encoding, String, CodeUnits) :-
    require_complete_switch [Encoding]
    (
        Encoding = utf8,
        string.to_utf8_code_unit_list(String, CodeUnits)
    ;
        Encoding = utf16,
        string.to_utf16_code_unit_list(String, CodeUnits)
    ).

from_code_unit_list_in_encoding(Encoding, CodeUnits, String) :-
    require_complete_switch [Encoding]
    (
        Encoding = utf8,
        string.from_utf8_code_unit_list(CodeUnits, String)
    ;
        Encoding = utf16,
        string.from_utf16_code_unit_list(CodeUnits, String)
    ).

det_from_code_unit_list_in_encoding(Encoding, CodeUnits, String) :-
    ( if from_code_unit_list_in_encoding(Encoding, CodeUnits, StringPrime) then
        String = StringPrime
    else
        unexpected($pred, "from_code_unit_list_in_encoding failed")
    ).

from_code_unit_list_in_encoding_allow_ill_formed(Encoding, CodeUnits, String) :-
    require_complete_switch [Encoding]
    (
        Encoding = utf8,
        InternalEncoding = internal_string_encoding,
        (
            InternalEncoding = utf8,
            string.from_code_unit_list_allow_ill_formed(CodeUnits, String)
        ;
            InternalEncoding = utf16,
            unexpected($pred, "implementing on utf16 is nyi")
        )
    ;
        Encoding = utf16,
        unexpected($pred, "utf16 is nyi")
    ).

det_from_code_unit_list_in_encoding_allow_ill_formed(Encoding,
        CodeUnits, String) :-
    ( if
        from_code_unit_list_in_encoding_allow_ill_formed(Encoding,
            CodeUnits, StringPrime)
    then
        String = StringPrime
    else
        unexpected($pred,
            "from_code_unit_list_in_encoding_allow_ill_formed failed")
    ).

%----------------------------------------------------------------------------%
:- end_module backend_libs.string_encoding.
%----------------------------------------------------------------------------%
