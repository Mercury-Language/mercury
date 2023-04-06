%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_tree_out_sym_name.m.
% Main author: fjh.
%
% This module converts sym_names back into Mercury source text.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_sym_name.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.

:- import_module io.

%---------------------------------------------------------------------------%
%
% Output sym_names, maybe with their arities.
%
% Use mercury_output_bracketed_sym_name when the sym_name has no arguments,
% otherwise use mercury_output_sym_name.
%

:- type needs_brackets
    --->    needs_brackets
            % Needs brackets, if it is an op.
    ;       does_not_need_brackets.
            % Doesn't need brackets.

:- pred mercury_output_sym_name(sym_name::in, io.text_output_stream::in,
    io::di, io::uo) is det.
:- func mercury_sym_name_to_string(sym_name) = string.
:- pred mercury_format_sym_name(sym_name::in, S::in,
    U::di, U::uo) is det <= output(S, U).

:- pred mercury_format_sym_name_ngt(needs_quotes::in, sym_name::in, S::in,
    U::di, U::uo) is det <= output(S, U).

:- func mercury_sym_name_arity_to_string(sym_name_arity) = string.
:- pred mercury_format_sym_name_arity(sym_name_arity::in, S::in,
    U::di, U::uo) is det <= output(S, U).

%---------------------%

:- pred mercury_output_bracketed_sym_name(sym_name::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_bracketed_sym_name_to_string(sym_name) = string.
:- pred mercury_format_bracketed_sym_name(sym_name::in, S::in,
    U::di, U::uo) is det <= output(S, U).

:- func mercury_bracketed_sym_name_arity_to_string(sym_name_arity) = string.
:- pred mercury_format_bracketed_sym_name_arity(sym_name_arity::in, S::in,
    U::di, U::uo) is det <= output(S, U).

%---------------------%

:- pred mercury_output_bracketed_sym_name_ngt(needs_quotes::in, sym_name::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_bracketed_sym_name_to_string_ngt(needs_quotes, sym_name)
    = string.
:- pred mercury_format_bracketed_sym_name_ngt(needs_quotes::in, sym_name::in,
    S::in, U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module unit.

%---------------------------------------------------------------------------%

mercury_output_sym_name(SymName, Stream, !IO) :-
    mercury_format_sym_name_ngt(not_next_to_graphic_token, SymName,
        Stream, !IO).

mercury_sym_name_to_string(SymName) = Str :-
    mercury_format_sym_name(SymName, unit, "", Str).

mercury_format_sym_name(SymName, S, !U) :-
    mercury_format_sym_name_ngt(not_next_to_graphic_token, SymName, S, !U).

mercury_format_sym_name_ngt(NextToGraphicToken, SymName, S, !U) :-
    (
        SymName = qualified(ModuleName, PredName),
        mercury_format_bracketed_sym_name_ngt(next_to_graphic_token,
            ModuleName, S, !U),
        add_string(".", S, !U),
        mercury_format_quoted_atom(next_to_graphic_token, PredName, S, !U)
    ;
        SymName = unqualified(PredName),
        mercury_format_quoted_atom(NextToGraphicToken, PredName, S, !U)
    ).

%---------------------%

mercury_sym_name_arity_to_string(SNA) = Str :-
    mercury_format_sym_name_arity(SNA, unit, "", Str).

mercury_format_sym_name_arity(sym_name_arity(SymName, Arity), S, !U) :-
    mercury_format_sym_name(SymName, S, !U),
    add_char('/', S, !U),
    add_int(Arity, S, !U).

%---------------------%

mercury_output_bracketed_sym_name(SymName, Stream, !IO) :-
    mercury_output_bracketed_sym_name_ngt(not_next_to_graphic_token, SymName,
        Stream, !IO).

mercury_bracketed_sym_name_to_string(SymName) =
    mercury_bracketed_sym_name_to_string_ngt(not_next_to_graphic_token,
        SymName).

mercury_format_bracketed_sym_name(SymName, S, !U) :-
    mercury_format_bracketed_sym_name_ngt(not_next_to_graphic_token, SymName,
        S, !U).

%---------------------%

mercury_bracketed_sym_name_arity_to_string(SNA) = Str :-
    mercury_format_bracketed_sym_name_arity(SNA, unit, "", Str).

mercury_format_bracketed_sym_name_arity(sym_name_arity(SymName, Arity),
        S, !U) :-
    mercury_format_bracketed_sym_name(SymName, S, !U),
    add_char('/', S, !U),
    add_int(Arity, S, !U).

%---------------------%

mercury_output_bracketed_sym_name_ngt(NextToGraphicToken, SymName,
        Stream, !IO) :-
    mercury_format_bracketed_sym_name_ngt(NextToGraphicToken, SymName,
        Stream, !IO).

mercury_bracketed_sym_name_to_string_ngt(NextToGraphicToken, SymName) = Str :-
    mercury_format_bracketed_sym_name_ngt(NextToGraphicToken, SymName,
        unit, "", Str).

mercury_format_bracketed_sym_name_ngt(NextToGraphicToken, SymName, S, !U) :-
    (
        SymName = qualified(ModuleName, Name),
        add_string("(", S, !U),
        mercury_format_bracketed_sym_name_ngt(next_to_graphic_token,
            ModuleName, S, !U),
        add_string(".", S, !U),
        mercury_format_bracketed_atom(next_to_graphic_token, Name, S, !U),
        add_string(")", S, !U)
    ;
        SymName = unqualified(Name),
        mercury_format_bracketed_atom(NextToGraphicToken, Name, S, !U)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_sym_name.
%---------------------------------------------------------------------------%
