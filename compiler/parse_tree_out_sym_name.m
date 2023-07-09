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
% All the predicates and functions in this module separate sym name components
% using the standard Mercury module qualifier operator ".".
%
% The escaped versions always generate a valid term; the unescaped versions
% may generate invalid Mercury terms.
%
% Use mercury_output_bracketed_sym_name when the sym_name has no arguments,
% otherwise use mercury_output_sym_name.
%

:- type needs_brackets
    --->    needs_brackets
            % Needs brackets, if it is an op.
    ;       does_not_need_brackets.
            % Doesn't need brackets.

:- func mercury_sym_name_to_string(sym_name) = string.
:- pred mercury_output_sym_name(sym_name::in, io.text_output_stream::in,
    io::di, io::uo) is det.
:- pred mercury_format_sym_name(sym_name::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_sym_name_ngt(needs_quotes::in, sym_name::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_sym_name_arity_to_string(sym_name_arity) = string.
:- pred mercury_format_sym_name_arity(sym_name_arity::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------%

:- func mercury_bracketed_sym_name_to_string(sym_name) = string.
:- pred mercury_output_bracketed_sym_name(sym_name::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_bracketed_sym_name(sym_name::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_bracketed_sym_name_arity_to_string(sym_name_arity) = string.
:- pred mercury_format_bracketed_sym_name_arity(sym_name_arity::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------%

:- func mercury_bracketed_sym_name_to_string_ngt(needs_quotes, sym_name)
    = string.
:- pred mercury_output_bracketed_sym_name_ngt(needs_quotes::in, sym_name::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_bracketed_sym_name_ngt(needs_quotes::in, sym_name::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

    % Convert a symbol name to a string.
    %
:- func unescaped_sym_name_to_string(sym_name) = string.
:- pred write_unescaped_sym_name(io.text_output_stream::in, sym_name::in,
    io::di, io::uo) is det.
:- pred format_unescaped_sym_name(S::in, sym_name::in, U::di, U::uo) is det
    <= pt_output(S, U).
:- func escaped_sym_name_to_string(sym_name) = string.
:- pred write_escaped_sym_name(io.text_output_stream::in, sym_name::in,
    io::di, io::uo) is det.
:- pred format_escaped_sym_name(S::in, sym_name::in, U::di, U::uo) is det
    <= pt_output(S, U).

    % Convert a symbol name and arity to a "<SymName>/<Arity>" string.
    %
:- func unescaped_sym_name_arity_to_string(sym_name_arity) = string.
:- pred write_unescaped_sym_name_arity(io.text_output_stream::in,
    sym_name_arity::in, io::di, io::uo) is det.
:- pred format_unescaped_sym_name_arity(S::in,
    sym_name_arity::in, U::di, U::uo) is det <= pt_output(S, U).
:- func escaped_sym_name_arity_to_string(sym_name_arity) = string.
:- pred write_escaped_sym_name_arity(io.text_output_stream::in,
    sym_name_arity::in, io::di, io::uo) is det.
:- pred format_escaped_sym_name_arity(S::in,
    sym_name_arity::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

    % Convert a type constructor name, including its arity to a string.
    % The string is escaped.
    %
:- func type_ctor_to_string(type_ctor) = string.
:- pred write_type_ctor(io.text_output_stream::in, type_ctor::in,
    io::di, io::uo) is det.
:- pred format_type_ctor(S::in, type_ctor::in, U::di, U::uo) is det
    <= pt_output(S, U).

    % Convert a type name name, without its arity, to a string.
    % The string is escaped.
    %
:- func type_name_to_string(type_ctor) = string.
:- pred write_type_name(io.text_output_stream::in, type_ctor::in,
    io::di, io::uo) is det.
:- pred format_type_name(S::in, type_ctor::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- func class_id_to_string(class_id) = string.
:- pred write_class_id(io.text_output_stream::in, class_id::in,
    io::di, io::uo) is det.
:- pred format_class_id(S::in, class_id::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_util.

:- import_module list.
:- import_module string.
:- import_module string.builder.
:- import_module term_io.

%---------------------------------------------------------------------------%

mercury_sym_name_to_string(SymName) = Str :-
    State0 = string.builder.init,
    mercury_format_sym_name_ngt(not_next_to_graphic_token, SymName,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_sym_name(SymName, Stream, !IO) :-
    mercury_format_sym_name_ngt(not_next_to_graphic_token, SymName,
        Stream, !IO).

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
    State0 = string.builder.init,
    mercury_format_sym_name_arity(SNA, string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_format_sym_name_arity(sym_name_arity(SymName, Arity), S, !U) :-
    mercury_format_sym_name(SymName, S, !U),
    add_char('/', S, !U),
    add_int(Arity, S, !U).

%---------------------%

mercury_bracketed_sym_name_to_string(SymName) =
    mercury_bracketed_sym_name_to_string_ngt(not_next_to_graphic_token,
        SymName).

mercury_output_bracketed_sym_name(SymName, Stream, !IO) :-
    mercury_output_bracketed_sym_name_ngt(not_next_to_graphic_token, SymName,
        Stream, !IO).

mercury_format_bracketed_sym_name(SymName, S, !U) :-
    mercury_format_bracketed_sym_name_ngt(not_next_to_graphic_token, SymName,
        S, !U).

%---------------------%

mercury_bracketed_sym_name_arity_to_string(SNA) = Str :-
    State0 = string.builder.init,
    mercury_format_bracketed_sym_name_arity(SNA, string.builder.handle,
        State0, State),
    Str = string.builder.to_string(State).

mercury_format_bracketed_sym_name_arity(sym_name_arity(SymName, Arity),
        S, !U) :-
    mercury_format_bracketed_sym_name(SymName, S, !U),
    add_char('/', S, !U),
    add_int(Arity, S, !U).

%---------------------%

mercury_bracketed_sym_name_to_string_ngt(NextToGraphicToken, SymName) = Str :-
    State0 = string.builder.init,
    mercury_format_bracketed_sym_name_ngt(NextToGraphicToken, SymName,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_bracketed_sym_name_ngt(NextToGraphicToken, SymName,
        Stream, !IO) :-
    mercury_format_bracketed_sym_name_ngt(NextToGraphicToken, SymName,
        Stream, !IO).

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

unescaped_sym_name_to_string(SymName) = Str :-
    State0 = string.builder.init,
    format_unescaped_sym_name(string.builder.handle, SymName, State0, State),
    Str = string.builder.to_string(State).

write_unescaped_sym_name(Stream, SymName, !IO) :-
    format_unescaped_sym_name(Stream, SymName, !IO).

format_unescaped_sym_name(S, SymName, !U) :-
    (
        SymName = qualified(ModuleName, Name),
        format_unescaped_sym_name(S, ModuleName, !U),
        add_string(".", S, !U),
        add_string(Name, S, !U)
    ;
        SymName = unqualified(Name),
        add_string(Name, S, !U)
    ).

%---------------------%

escaped_sym_name_to_string(SymName) = Str :-
    State0 = string.builder.init,
    format_escaped_sym_name(string.builder.handle, SymName, State0, State),
    Str = string.builder.to_string(State).

write_escaped_sym_name(Stream, SymName, !IO) :-
    format_escaped_sym_name(Stream, SymName, !IO).

format_escaped_sym_name(S, SymName, !U) :-
    (
        SymName = qualified(ModuleName, Name),
        format_escaped_sym_name(S, ModuleName, !U),
        add_string(".", S, !U),
        term_io.format_escaped_string(S, Name, !U)
    ;
        SymName = unqualified(Name),
        term_io.format_escaped_string(S, Name, !U)
    ).

%---------------------%

unescaped_sym_name_arity_to_string(SymNameArity) = Str :-
    State0 = string.builder.init,
    format_unescaped_sym_name_arity(string.builder.handle, SymNameArity,
        State0, State),
    Str = string.builder.to_string(State).

write_unescaped_sym_name_arity(Stream, SymNameArity, !IO) :-
    format_escaped_sym_name_arity(Stream, SymNameArity, !IO).

format_unescaped_sym_name_arity(S, sym_name_arity(SymName, Arity), !U) :-
    format_unescaped_sym_name(S, SymName, !U),
    add_string("/", S, !U),
    add_int(Arity, S, !U).

%---------------------%

escaped_sym_name_arity_to_string(SymNameArity) = Str :-
    State0 = string.builder.init,
    format_escaped_sym_name_arity(string.builder.handle, SymNameArity,
        State0, State),
    Str = string.builder.to_string(State).

write_escaped_sym_name_arity(Stream, SymNameArity, !IO) :-
    format_escaped_sym_name_arity(Stream, SymNameArity, !IO).

format_escaped_sym_name_arity(S, sym_name_arity(SymName, Arity), !U) :-
    format_escaped_sym_name(S, SymName, !U),
    add_string("/", S, !U),
    add_int(Arity, S, !U).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

type_ctor_to_string(TypeCtor) = Str :-
    State0 = string.builder.init,
    format_type_ctor(string.builder.handle, TypeCtor, State0, State),
    Str = string.builder.to_string(State).

write_type_ctor(Stream, TypeCtor, !IO) :-
    format_type_ctor(Stream, TypeCtor, !IO).

format_type_ctor(S, type_ctor(SymName, Arity), !U) :-
    format_escaped_sym_name_arity(S, sym_name_arity(SymName, Arity), !U).

%---------------------%

type_name_to_string(TypeCtor) = Str :-
    State0 = string.builder.init,
    format_type_name(string.builder.handle, TypeCtor, State0, State),
    Str = string.builder.to_string(State).

write_type_name(Stream, TypeCtor, !IO) :-
    format_type_name(Stream, TypeCtor, !IO).

format_type_name(S, type_ctor(SymName, _Arity), !U) :-
    format_escaped_sym_name(S, SymName, !U).

%---------------------------------------------------------------------------%

class_id_to_string(ClassId) = Str :-
    State0 = string.builder.init,
    format_class_id(string.builder.handle, ClassId, State0, State),
    Str = string.builder.to_string(State).

write_class_id(Stream, ClassId, !IO) :-
    format_class_id(Stream, ClassId, !IO).

format_class_id(S, class_id(SymName, Arity), !U) :-
    format_escaped_sym_name_arity(S, sym_name_arity(SymName, Arity), !U).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_sym_name.
%---------------------------------------------------------------------------%
