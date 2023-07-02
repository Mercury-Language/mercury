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
% NOTE All the predicates and functions below whose names have
% the "mercury_" prefix were originally in a module, mercury_to_mercury.m,
% whose documentation said that it "converts sym_names back into
% Mercury source text". The other predicates and functions originally
% came from prog_out.m, which made no such claim.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_sym_name.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
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

    % Return or write out a symbol name, with special characters escaped,
    % but without any quotes. This is suitable for use in error messages,
    % where the caller should print out an enclosing forward/backward-quote
    % pair (`...').
    %
:- func sym_name_to_escaped_string(sym_name) = string.
:- pred write_sym_name(io.text_output_stream::in, sym_name::in,
    io::di, io::uo) is det.

    % Write out a symbol name, enclosed in single forward quotes ('...'),
    % and with any special characters escaped.
    % The output should be a syntactically valid Mercury term.
    %
:- pred write_quoted_sym_name(io.text_output_stream::in, sym_name::in,
    io::di, io::uo) is det.

    % sym_name_arity_to_string(SymName, String):
    %
    % Convert a symbol name and arity to a "<Name>/<Arity>" string,
    % with module qualifiers separated by the standard Mercury module
    % qualifier operator.
    %
:- func sym_name_arity_to_string(sym_name_arity) = string.
:- pred write_sym_name_arity(io.text_output_stream::in, sym_name_arity::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Write out a module name.
    %
:- func module_name_to_escaped_string(module_name) = string.

%-----------------------------------------------------------------------------%

:- func pf_sym_name_pred_form_arity_to_string(pf_sym_name_arity) = string.
:- func pf_sym_name_pred_form_arity_to_string(pred_or_func, sym_name_arity)
    = string.
:- func pf_sym_name_pred_form_arity_to_string(pred_or_func, sym_name,
    pred_form_arity) = string.

:- func pf_sym_name_user_arity_to_string(pred_pf_name_arity) = string.
:- func pf_sym_name_user_arity_to_string(pred_or_func, sym_name_arity)
    = string.
:- func pf_sym_name_user_arity_to_string(pred_or_func, sym_name, arity)
    = string.

:- func pf_sym_name_user_arity_to_unquoted_string(pred_pf_name_arity) = string.
:- func pf_sym_name_user_arity_to_unquoted_string(pred_or_func, sym_name_arity)
    = string.
:- func pf_sym_name_user_arity_to_unquoted_string(pred_or_func, sym_name,
    arity) = string.

%-----------------------------------------------------------------------------%

:- func type_ctor_to_string(type_ctor) = string.

:- func type_name_to_string(type_ctor) = string.
:- pred write_type_name(io.text_output_stream::in, type_ctor::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- func class_id_to_string(class_id) = string.
:- pred write_class_id(io.text_output_stream::in, class_id::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_util.

:- import_module list.
:- import_module string.
:- import_module term_io.
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

sym_name_to_escaped_string(qualified(Module, Name)) =
    sym_name_to_escaped_string(Module)
    ++ "."
    ++ term_io.escaped_string(Name).
sym_name_to_escaped_string(unqualified(Name)) =
    term_io.escaped_string(Name).

write_sym_name(Stream, qualified(Module, Name), !IO) :-
    write_sym_name(Stream, Module, !IO),
    io.write_string(Stream, ".", !IO),
    term_io.write_escaped_string(Stream, Name, !IO).
write_sym_name(Stream, unqualified(Name), !IO) :-
    term_io.write_escaped_string(Stream, Name, !IO).

write_quoted_sym_name(Stream, SymName, !IO) :-
    io.write_string(Stream, "'", !IO),
    write_sym_name(Stream, SymName, !IO),
    io.write_string(Stream, "'", !IO).

sym_name_arity_to_string(sym_name_arity(SymName, Arity)) = Str :-
    SymNameStr = sym_name_to_string(SymName),
    string.format("%s/%d", [s(SymNameStr), i(Arity)], Str).

write_sym_name_arity(Stream, sym_name_arity(Name, Arity), !IO) :-
    write_sym_name(Stream, Name, !IO),
    io.write_string(Stream, "/", !IO),
    io.write_int(Stream, Arity, !IO).

%-----------------------------------------------------------------------------%

module_name_to_escaped_string(ModuleName) =
    sym_name_to_escaped_string(ModuleName).

%-----------------------------------------------------------------------------%

pf_sym_name_pred_form_arity_to_string(PFSymNameArity) = Str :-
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, SymName, PredFormArity),
    Str = pf_sym_name_pred_form_arity_to_string(PredOrFunc, SymName,
        PredFormArity).

pf_sym_name_pred_form_arity_to_string(PredOrFunc, SNA) = Str :-
    SNA = sym_name_arity(SymName, Arity),
    PredFormArity = pred_form_arity(Arity),
    Str = pf_sym_name_pred_form_arity_to_string(PredOrFunc, SymName,
        PredFormArity).

pf_sym_name_pred_form_arity_to_string(PredOrFunc, SymName, PredFormArity)
        = Str :-
    user_arity_pred_form_arity(PredOrFunc,
        user_arity(UserArityInt), PredFormArity),
    PredOrFuncStr = pred_or_func_to_string(PredOrFunc),
    SymNameStr = sym_name_to_string(SymName),
    string.format("%s `%s'/%d",
        [s(PredOrFuncStr), s(SymNameStr), i(UserArityInt)], Str).

%-----------------------------------------------------------------------------%

pf_sym_name_user_arity_to_string(PFSymNameArity) = Str :-
    PFSymNameArity =
        pred_pf_name_arity(PredOrFunc, SymName, user_arity(Arity)),
    Str = pf_sym_name_user_arity_to_string(PredOrFunc, SymName, Arity).

pf_sym_name_user_arity_to_string(PredOrFunc, SNA) = Str :-
    SNA = sym_name_arity(SymName, Arity),
    Str = pf_sym_name_user_arity_to_string(PredOrFunc, SymName, Arity).

pf_sym_name_user_arity_to_string(PredOrFunc, SymName, Arity) = Str :-
    PredOrFuncStr = pred_or_func_to_string(PredOrFunc),
    SymNameStr = sym_name_to_string(SymName),
    string.format("%s `%s'/%d",
        [s(PredOrFuncStr), s(SymNameStr), i(Arity)], Str).

%-----------------------------------------------------------------------------%

pf_sym_name_user_arity_to_unquoted_string(PFSymNameArity) = Str :-
    PFSymNameArity =
        pred_pf_name_arity(PredOrFunc, SymName, user_arity(Arity)),
    Str =
        pf_sym_name_user_arity_to_unquoted_string(PredOrFunc, SymName, Arity).

pf_sym_name_user_arity_to_unquoted_string(PredOrFunc, SNA) = Str :-
    SNA = sym_name_arity(SymName, Arity),
    Str =
        pf_sym_name_user_arity_to_unquoted_string(PredOrFunc, SymName, Arity).

pf_sym_name_user_arity_to_unquoted_string(PredOrFunc, SymName, Arity) = Str :-
    PredOrFuncStr = pred_or_func_to_string(PredOrFunc),
    SymNameStr = sym_name_to_string(SymName),
    string.format("%s %s/%d",
        [s(PredOrFuncStr), s(SymNameStr), i(Arity)], Str).

%-----------------------------------------------------------------------------%

type_ctor_to_string(type_ctor(Name, Arity)) =
    sym_name_arity_to_string(sym_name_arity(Name, Arity)).

type_name_to_string(type_ctor(Name, _Arity)) =
    sym_name_to_escaped_string(Name).

write_type_name(Stream, type_ctor(Name, _Arity), !IO) :-
    write_sym_name(Stream, Name, !IO).

%-----------------------------------------------------------------------------%

class_id_to_string(class_id(Name, Arity)) =
    sym_name_arity_to_string(sym_name_arity(Name, Arity)).
write_class_id(Stream, class_id(Name, Arity), !IO) :-
    write_sym_name_arity(Stream, sym_name_arity(Name, Arity), !IO).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_sym_name.
%---------------------------------------------------------------------------%
