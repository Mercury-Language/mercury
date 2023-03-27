%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%

:- module backend_libs.string_encoding.
:- interface.

:- import_module libs.
:- import_module libs.globals.

:- import_module list.

:- type string_encoding
    --->    utf8
    ;       utf16.

:- pred target_char_range(compilation_target::in, int::out, int::out) is det.

:- func target_string_encoding(compilation_target) = string_encoding.

:- pred to_code_unit_list_in_encoding(string_encoding::in, string::in,
    list(int)::out) is det.

:- pred from_code_unit_list_in_encoding(string_encoding::in, list(int)::in,
    string::out) is semidet.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

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

%----------------------------------------------------------------------------%
:- end_module backend_libs.string_encoding.
%----------------------------------------------------------------------------%
