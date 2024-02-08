%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: indent.m.
%
% This module provides utility functions and predicates dealing with
% indentation.
%
%---------------------------------------------------------------------------%

:- module libs.indent.
:- interface.

:- import_module io.

    % A value of type `indent' records the number of levels of indentation
    % to indent a piece of text (usually code). The number of spaces
    % per indent level depends on the particular output destination.
    % When the MLDS backend generates C, Java or C# code, each indent level
    % is two spaces, so the appropriate way to translate an indent level
    % to an indent string is the indent2 family of operations below.
:- type indent == uint.

%---------------------------------------------------------------------------%
%
% Write out indentation.
%

    % Return the number of spaces per indent level.
    % This will be the number in the name of the function.
    % For now, we support only two-spaces-per-indent-level, which is
    % used by most of the compiler, including error_util.m and mlds_to_*.m.
    % Later, we will also support four-spaces-per-indent-level, which is
    % used by parse_tree_out_*.m.
    %
:- func indent2_increment = indent.

    % Write out the given indent level (indent2_increment spaces per level).
    %
:- pred write_indent2(io.text_output_stream::in, indent::in,
    io::di, io::uo) is det.

    % Return the indent for the given level as a string.
    %
:- func indent2_string(indent) = string.

:- func add_indent2_prefix(indent, string) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

indent2_increment = 2u.

write_indent2(Stream, Indent, !IO) :-
    Str = indent2_string(Indent),
    io.write_string(Stream, Str, !IO).

indent2_string(Indent) = Str :-
    % The code here is modelled after output_std_indent_levels in
    % library/pretty_printer.m, except we can, and do, assume that
    % Indent is never negative, and in our use case, deep indentation
    % is much rarer.
    ( if indent2_str_lo(Indent, StrPrime) then
        Str = StrPrime
    else if Indent > 0u then
        indent2_str_16(SixteenIndentStr),
        Str = SixteenIndentStr ++ indent2_string(Indent - 16u)
    else
        % Indent must be zero.
        Str = ""
    ).

:- pred indent2_str_lo(indent::in, string::out) is semidet.
:- pred indent2_str_16(string::out) is det.

indent2_str_lo( 0u,  "").
indent2_str_lo( 1u,  "  ").
indent2_str_lo( 2u,  "    ").
indent2_str_lo( 3u,  "      ").
indent2_str_lo( 4u,  "        ").
indent2_str_lo( 5u,  "          ").
indent2_str_lo( 6u,  "            ").
indent2_str_lo( 7u,  "              ").
indent2_str_lo( 8u,  "                ").
indent2_str_lo( 9u,  "                  ").
indent2_str_lo(10u,  "                    ").
indent2_str_lo(11u,  "                      ").
indent2_str_lo(12u,  "                        ").
indent2_str_lo(13u,  "                          ").
indent2_str_lo(14u,  "                            ").
indent2_str_lo(15u,  "                              ").
indent2_str_16(      "                                ").

add_indent2_prefix(Indent, Str0) = Str :-
    Str = indent2_string(Indent) ++ Str0.

%---------------------------------------------------------------------------%
:- end_module libs.indent.
%---------------------------------------------------------------------------%
