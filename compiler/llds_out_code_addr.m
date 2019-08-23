%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: llds_out_code_addr.m.
% Main authors: conway, fjh, zs.
%
% This module defines the routines for printing out LLDS labels and code
% addresses.
%
%----------------------------------------------------------------------------%

:- module ll_backend.llds_out.llds_out_code_addr.
:- interface.

:- import_module ll_backend.llds.
:- import_module ll_backend.llds_out.llds_out_util.

:- import_module bool.
:- import_module io.

%----------------------------------------------------------------------------%

    % output_record_code_addr_decls(Info, CodeAddr, ...) outputs the
    % declarations of any extern symbols, etc. that need to be declared
    % before output_code_addr(CodeAddr) is called.
    %
:- pred output_record_code_addr_decls(llds_out_info::in, code_addr::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

:- pred output_record_code_addr_decls_format(llds_out_info::in, code_addr::in,
    string::in, string::in, int::in, int::out, decl_set::in,
    decl_set::out, io::di, io::uo) is det.

    % Return the suffix after do_call_closure_ or do_call_class_method_
    % represented by the given variant.
    %
:- func ho_call_variant_to_string(ho_call_variant) = string.

    % Output a label (used by garbage collection).
    %
:- pred output_label(label::in, io::di, io::uo) is det.

:- pred output_label_no_prefix(label::in, io::di, io::uo) is det.

    % Output a label with or without the standard mercury__ prefix.
    %
:- pred output_label_maybe_prefix(label::in, bool::in, io::di, io::uo) is det.

    % Convert a label to a C string. The boolean controls whether
    % a prefix ("mercury__") is added to the string.
    %
:- func label_to_c_string(label, bool) = string.

:- pred output_code_addr(code_addr::in, io::di, io::uo) is det.

:- type wrapper
    --->    wrapper_entry
    ;       wrapper_label
    ;       wrapper_none.

:- pred output_code_addr_from_pieces(string::in, bool::in, wrapper::in,
    io::di, io::uo) is det.

:- pred code_addr_to_string_base(code_addr::in, string::out,
    bool::out, wrapper::out) is det.

:- pred output_label_as_code_addr(label::in, io::di, io::uo) is det.

:- func label_is_external_to_c_module(label) = bool.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.name_mangle.

:- import_module int.
:- import_module string.

%----------------------------------------------------------------------------%
%
% Declare code addresses.
%

output_record_code_addr_decls(Info, CodeAddress, !DeclSet, !IO) :-
    output_record_code_addr_decls_format(Info, CodeAddress, "", "", 0, _,
        !DeclSet, !IO).

output_record_code_addr_decls_format(Info, CodeAddress,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    ( if decl_set_is_member(decl_code_addr(CodeAddress), !.DeclSet) then
        true
    else
        decl_set_insert(decl_code_addr(CodeAddress), !DeclSet),
        need_code_addr_decls(Info, CodeAddress, NeedDecl),
        (
            NeedDecl = yes,
            output_indent(FirstIndent, LaterIndent, !.N, !IO),
            !:N = !.N + 1,
            output_code_addr_decls(Info, CodeAddress, !IO)
        ;
            NeedDecl = no
        )
    ).

:- pred need_code_addr_decls(llds_out_info::in, code_addr::in, bool::out)
    is det.

need_code_addr_decls(Info, CodeAddr, Need) :-
    (
        CodeAddr = code_label(Label),
        (
            Label = entry_label(entry_label_exported, _),
            Need = yes
        ;
            Label = entry_label(entry_label_local, _),
            Need = yes
        ;
            Label = entry_label(entry_label_c_local, _),
            Need = no
        ;
            Label = internal_label(_, _),
            Need = no
        )
    ;
        ( CodeAddr = code_imported_proc(_)
        ; CodeAddr = do_trace_redo_fail_shallow
        ; CodeAddr = do_trace_redo_fail_deep
        ; CodeAddr = do_call_closure(_)
        ; CodeAddr = do_call_class_method(_)
        ; CodeAddr = do_not_reached
        ),
        Need = yes
    ;
        ( CodeAddr = code_succip
        ; CodeAddr = do_succeed(_)
        ),
        Need = no
    ;
        ( CodeAddr = do_redo
        ; CodeAddr = do_fail
        ),
        UseMacro = Info ^ lout_use_macro_for_redo_fail,
        (
            UseMacro = yes,
            Need = no
        ;
            UseMacro = no,
            Need = yes
        )
    ).

:- pred output_code_addr_decls(llds_out_info::in, code_addr::in,
    io::di, io::uo) is det.

output_code_addr_decls(Info, CodeAddr, !IO) :-
    (
        CodeAddr = code_label(Label),
        output_label_as_code_addr_decls(Label, !IO)
    ;
        CodeAddr = code_imported_proc(ProcLabel),
        io.write_string("MR_decl_entry(", !IO),
        output_proc_label_no_prefix(ProcLabel, !IO),
        io.write_string(");\n", !IO)
    ;
        ( CodeAddr = code_succip
        ; CodeAddr = do_succeed(_)
        )
    ;
        CodeAddr = do_redo,
        UseMacro = Info ^ lout_use_macro_for_redo_fail,
        (
            UseMacro = yes
        ;
            UseMacro = no,
            io.write_string("MR_declare_entry(MR_do_redo);\n", !IO)
        )
    ;
        CodeAddr = do_fail,
        UseMacro = Info ^ lout_use_macro_for_redo_fail,
        (
            UseMacro = yes
        ;
            UseMacro = no,
            io.write_string("MR_declare_entry(MR_do_fail);\n", !IO)
        )
    ;
        CodeAddr = do_trace_redo_fail_shallow,
        io.write_string("MR_declare_entry(MR_do_trace_redo_fail_shallow);\n",
            !IO)
    ;
        CodeAddr = do_trace_redo_fail_deep,
        io.write_string("MR_declare_entry(MR_do_trace_redo_fail_deep);\n",
            !IO)
    ;
        CodeAddr = do_call_closure(Variant),
        io.write_string("MR_declare_entry(mercury__do_call_closure_", !IO),
        io.write_string(ho_call_variant_to_string(Variant), !IO),
        io.write_string(");\n", !IO)
    ;
        CodeAddr = do_call_class_method(Variant),
        io.write_string("MR_declare_entry(mercury__do_call_class_method_",
            !IO),
        io.write_string(ho_call_variant_to_string(Variant), !IO),
        io.write_string(");\n", !IO)
    ;
        CodeAddr = do_not_reached,
        io.write_string("MR_declare_entry(MR_do_not_reached);\n", !IO)
    ).

:- pred output_label_as_code_addr_decls(label::in, io::di, io::uo) is det.

output_label_as_code_addr_decls(Label, !IO) :-
    (
        Label = entry_label(entry_label_exported, ProcLabel),
        io.write_string("MR_decl_entry(", !IO),
        output_label_no_prefix(entry_label(entry_label_exported, ProcLabel),
            !IO),
        io.write_string(");\n", !IO)
    ;
        Label = entry_label(entry_label_local, _ProcLabel)
    ;
        Label = entry_label(entry_label_c_local, _ProcLabel)
    ;
        Label = internal_label(_, _)
    ).

%----------------------------------------------------------------------------%

ho_call_variant_to_string(Variant) = Str :-
    (
        Variant = generic,
        Str = "compact"
    ;
        Variant = specialized_known(Num),
        Str = int_to_string(Num)
    ).

%----------------------------------------------------------------------------%
%
% Output labels.
%

output_label(Label, !IO) :-
    LabelStr = label_to_c_string(Label, yes),
    io.write_string(LabelStr, !IO).

output_label_no_prefix(Label, !IO) :-
    LabelStr = label_to_c_string(Label, no),
    io.write_string(LabelStr, !IO).

output_label_maybe_prefix(Label, AddPrefix, !IO) :-
    LabelStr = label_to_c_string(Label, AddPrefix),
    io.write_string(LabelStr, !IO).

label_to_c_string(Label, AddPrefix) = LabelStr :-
    (
        Label = entry_label(_, ProcLabel),
        % Entry labels should have the same string form regardless of the
        % entry label type, because we may refer to an entry label via
        % different entry label types in different circumstances.
        % For example, the entry label of a recursive unification predicate
        % is referred to as local in type_info structures and as c_local
        % in the recursive call, since the c_local is special cased in some
        % circumstances, leading to better code.
        LabelStr = proc_label_to_c_string(ProcLabel, AddPrefix)
    ;
        Label = internal_label(Num, ProcLabel),
        ProcLabelStr = proc_label_to_c_string(ProcLabel, AddPrefix),
        string.int_to_string(Num, NumStr),
        LabelStr = ProcLabelStr ++ "_i" ++ NumStr
    ).

%----------------------------------------------------------------------------%
%
% Output code addresses.
%

output_code_addr(CodeAddr, !IO) :-
    code_addr_to_string_base(CodeAddr, BaseStr, NeedsPrefix, Wrapper),
    output_code_addr_from_pieces(BaseStr, NeedsPrefix, Wrapper, !IO).

output_code_addr_from_pieces(BaseStr, NeedsPrefix, Wrapper, !IO) :-
    (
        Wrapper = wrapper_none,
        (
            NeedsPrefix = yes,
            io.write_string(mercury_label_prefix, !IO)
        ;
            NeedsPrefix = no
        ),
        io.write_string(BaseStr, !IO)
    ;
        Wrapper = wrapper_entry,
        (
            NeedsPrefix = yes,
            % The _AP version of the macro adds the prefix.
            io.write_string("MR_ENTRY_AP(", !IO),
            io.write_string(BaseStr, !IO),
            io.write_string(")", !IO)
        ;
            NeedsPrefix = no,
            io.write_string("MR_ENTRY(", !IO),
            io.write_string(BaseStr, !IO),
            io.write_string(")", !IO)
        )
    ;
        Wrapper = wrapper_label,
        (
            NeedsPrefix = yes,
            % The _AP version of the macro adds the prefix.
            io.write_string("MR_LABEL_AP(", !IO),
            io.write_string(BaseStr, !IO),
            io.write_string(")", !IO)
        ;
            NeedsPrefix = no,
            io.write_string("MR_LABEL(", !IO),
            io.write_string(BaseStr, !IO),
            io.write_string(")", !IO)
        )
    ).

code_addr_to_string_base(CodeAddr, BaseStr, NeedsPrefix, Wrapper) :-
    (
        CodeAddr = code_label(Label),
        BaseStr = label_to_c_string(Label, no),
        NeedsPrefix = yes,
        IsExternal = label_is_external_to_c_module(Label),
        (
            IsExternal = yes,
            Wrapper = wrapper_entry
        ;
            IsExternal = no,
            Wrapper = wrapper_label
        )
    ;
        CodeAddr = code_imported_proc(ProcLabel),
        BaseStr = proc_label_to_c_string(ProcLabel, no),
        NeedsPrefix = yes,
        Wrapper = wrapper_entry
    ;
        CodeAddr = code_succip,
        BaseStr = "MR_succip",
        NeedsPrefix = no,
        Wrapper = wrapper_none
    ;
        CodeAddr = do_succeed(Last),
        (
            Last = no,
            BaseStr = "MR_do_succeed"
        ;
            Last = yes,
            BaseStr = "MR_do_last_succeed"
        ),
        NeedsPrefix = no,
        Wrapper = wrapper_entry
    ;
        (
            CodeAddr = do_redo,
            BaseStr = "MR_do_redo"
        ;
            CodeAddr = do_fail,
            BaseStr = "MR_do_fail"
        ;
            CodeAddr = do_trace_redo_fail_shallow,
            BaseStr = "MR_do_trace_redo_fail_shallow"
        ;
            CodeAddr = do_trace_redo_fail_deep,
            BaseStr = "MR_do_trace_redo_fail_deep"
        ;
            CodeAddr = do_not_reached,
            BaseStr = "MR_do_not_reached"
        ),
        NeedsPrefix = no,
        Wrapper = wrapper_entry
    ;
        CodeAddr = do_call_closure(Variant),
        BaseStr = "mercury__do_call_closure_" ++
            ho_call_variant_to_string(Variant),
        NeedsPrefix = no,
        Wrapper = wrapper_entry
    ;
        CodeAddr = do_call_class_method(Variant),
        BaseStr = "mercury__do_call_class_method_" ++
            ho_call_variant_to_string(Variant),
        NeedsPrefix = no,
        Wrapper = wrapper_entry
    ).

output_label_as_code_addr(Label, !IO) :-
    label_as_code_addr_to_string(Label, Str),
    io.write_string(Str, !IO).

:- pred label_as_code_addr_to_string(label::in, string::out) is det.

label_as_code_addr_to_string(Label, Str) :-
    LabelStr = label_to_c_string(Label, no),
    IsEntry = label_is_external_to_c_module(Label),
    (
        IsEntry = yes,
        Str = "MR_ENTRY_AP(" ++ LabelStr ++ ")"
    ;
        IsEntry = no,
        Str = "MR_LABEL_AP(" ++ LabelStr ++ ")"
    ).

label_is_external_to_c_module(entry_label(entry_label_exported, _)) = yes.
label_is_external_to_c_module(entry_label(entry_label_local, _)) = yes.
label_is_external_to_c_module(entry_label(entry_label_c_local, _)) = no.
label_is_external_to_c_module(internal_label(_, _)) = no.

%---------------------------------------------------------------------------%
:- end_module llds_out_code_addr.
%---------------------------------------------------------------------------%
