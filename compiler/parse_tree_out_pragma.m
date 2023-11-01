%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module converts the parse tree structure representations of pragmas
% back into Mercury source text.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_pragma.
:- interface.

:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred mercury_format_item_decl_pragma(merc_out_info::in, S::in,
    item_decl_pragma_info::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_item_decl_marker(S::in, item_decl_marker_info::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_item_impl_pragma(merc_out_info::in, S::in,
    item_impl_pragma_info::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_item_impl_marker(S::in, item_impl_marker_info::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_item_generated_pragma(merc_out_info::in, S::in,
    item_generated_pragma_info::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_output_pragma_decl_pred_pf_name_arity(
    io.text_output_stream::in, string::in, pred_pf_name_arity::in, string::in,
    io::di, io::uo) is det.
:- func mercury_pragma_decl_pred_pf_name_arity_to_string(string,
    pred_pf_name_arity, string) = string.
:- pred mercury_format_pragma_decl_pred_pf_name_arity(string::in,
    pred_pf_name_arity::in, string::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

:- pred mercury_output_pragma_foreign_decl(io.text_output_stream::in,
    impl_pragma_foreign_decl_info::in, io::di, io::uo) is det.
:- func mercury_pragma_foreign_decl_to_string(impl_pragma_foreign_decl_info)
    = string.
:- pred mercury_format_pragma_foreign_decl(impl_pragma_foreign_decl_info::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_output_item_foreign_proc(io.text_output_stream::in,
    output_lang::in, item_foreign_proc_info::in, io::di, io::uo) is det.
:- func mercury_item_foreign_proc_to_string(output_lang,
    item_foreign_proc_info) = string.
:- pred mercury_format_item_foreign_proc(S::in, output_lang::in,
    item_foreign_proc_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_pragma_type_spec(S::in, output_lang::in,
    decl_pragma_type_spec_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_pragma_unused_args(S::in,
    gen_pragma_unused_args_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_pragma_exceptions(S::in,
    gen_pragma_exceptions_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_pragma_trailing(S::in,
    gen_pragma_trailing_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_pragma_mm_tabling(S::in,
    gen_pragma_mm_tabling_info::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

    % This predicate outputs termination_info pragmas;
    % such annotations can be part of .opt and .trans_opt files.
    %
:- pred mercury_format_pragma_termination(S::in, output_lang::in,
    decl_pragma_termination_info::in, U::di, U::uo) is det
    <= pt_output(S, U).

    % Return a string representation of the given arg size info.
    % Include the representation of any error infos if the first arg is yes.
    %
:- func maybe_arg_size_info_to_string(bool, maybe(generic_arg_size_info(T)))
    = string.

    % Return a string representation of the given termination info.
    % Include the representation of any error_infos if the first arg is yes.
    %
:- func maybe_termination_info_to_string(bool,
    maybe(generic_termination_info(S, T))) = string.

%---------------------------------------------------------------------------%

:- pred mercury_format_pragma_termination2(S::in, output_lang::in,
    decl_pragma_termination2_info::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_format_pragma_struct_sharing(S::in, output_lang::in,
    decl_pragma_struct_sharing_info::in, U::di, U::uo) is det
    <= pt_output(S, U).

:- pred mercury_format_pragma_struct_reuse(S::in, output_lang::in,
    decl_pragma_struct_reuse_info::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.rat.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.var_db.

:- import_module char.
:- import_module list.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module string.builder.
:- import_module term.
:- import_module term_io.
:- import_module unit.
:- import_module varset.

%---------------------------------------------------------------------------%

mercury_format_item_decl_pragma(Info, Stream, DeclPragma, !IO) :-
    Context = get_decl_pragma_context(DeclPragma),
    maybe_format_line_number(Info, Context, Stream, !IO),
    Lang = get_output_lang(Info),
    (
        DeclPragma = decl_pragma_obsolete_pred(ObsPred),
        mercury_format_pragma_obsolete_pred(ObsPred, Stream, !IO)
    ;
        DeclPragma = decl_pragma_obsolete_proc(ObsProc),
        mercury_format_pragma_obsolete_proc(Lang, ObsProc, Stream, !IO)
    ;
        DeclPragma = decl_pragma_format_call(FormatCall),
        mercury_format_pragma_format_call(FormatCall, Stream, !IO)
    ;
        DeclPragma = decl_pragma_type_spec(TypeSpec),
        mercury_format_pragma_type_spec(Stream, Lang, TypeSpec, !IO)
    ;
        DeclPragma = decl_pragma_oisu(OISU),
        mercury_format_pragma_oisu(OISU, Stream, !IO)
    ;
        DeclPragma = decl_pragma_termination(Term),
        mercury_format_pragma_termination(Stream, Lang, Term, !IO)
    ;
        DeclPragma = decl_pragma_termination2(Term2),
        mercury_format_pragma_termination2(Stream, Lang, Term2, !IO)
    ;
        DeclPragma = decl_pragma_struct_sharing(Sharing),
        mercury_format_pragma_struct_sharing(Stream, Lang, Sharing, !IO)
    ;
        DeclPragma = decl_pragma_struct_reuse(Reuse),
        mercury_format_pragma_struct_reuse(Stream, Lang, Reuse, !IO)
    ).

mercury_format_item_decl_marker(Stream, DeclMarker, !IO) :-
    DeclMarker = item_decl_marker_info(MarkerKind, PredSpec, _, _),
    (
        MarkerKind = dpmk_terminates,
        MarkerKindStr = "terminates"
    ;
        MarkerKind = dpmk_does_not_terminate, 
        MarkerKindStr = "does_not_terminate"
    ;
        MarkerKind = dpmk_check_termination, 
        MarkerKindStr = "check_termination"
    ),
    mercury_format_pragma_decl_pred_pfu_name_arity(MarkerKindStr,
        PredSpec, "", Stream, !IO).

mercury_format_item_impl_pragma(Info, S, ImplPragma, !U) :-
    Context = get_impl_pragma_context(ImplPragma),
    maybe_format_line_number(Info, Context, S, !U),
    Lang = get_output_lang(Info),
    (
        ImplPragma = impl_pragma_foreign_decl(FDInfo),
        mercury_format_pragma_foreign_decl(FDInfo, S, !U)
    ;
        ImplPragma = impl_pragma_foreign_code(FCInfo),
        mercury_format_pragma_foreign_code(FCInfo, S, !U)
    ;
        ImplPragma = impl_pragma_fproc_export(FPEInfo),
        mercury_format_pragma_foreign_proc_export(S, Lang, FPEInfo, !U)
    ;
        ImplPragma = impl_pragma_external_proc(ExternalInfo),
        mercury_format_pragma_external_proc(ExternalInfo, S, !U)
    ;
        ImplPragma = impl_pragma_fact_table(FactTableInfo),
        mercury_format_pragma_fact_table(FactTableInfo, S, !U)
    ;
        ImplPragma = impl_pragma_tabled(TabledInfo),
        mercury_format_pragma_tabled(TabledInfo, S, !U)
    ;
        ImplPragma = impl_pragma_req_tail_rec(RequireTailrecPragma),
        mercury_format_pragma_require_tail_rec(Lang, RequireTailrecPragma,
            S, !U)
    ;
        ImplPragma = impl_pragma_req_feature_set(RFSInfo),
        mercury_format_pragma_require_feature_set(RFSInfo, S, !U)
    ).

mercury_format_item_impl_marker(Stream, ImplMarker, !IO) :-
    ImplMarker = item_impl_marker_info(MarkerKind, PredSpec, _, _),
    (
        MarkerKind = ipmk_inline,
        MarkerKindStr = "inline"
    ;
        MarkerKind = ipmk_no_inline,
        MarkerKindStr = "no_inline"
    ;
        MarkerKind = ipmk_consider_used,
        MarkerKindStr = "consider_used"
    ;
        MarkerKind = ipmk_mode_check_clauses,
        MarkerKindStr = "mode_check_clauses"
    ;
        MarkerKind = ipmk_no_detism_warning,
        MarkerKindStr = "no_determinism_warning"
    ;
        MarkerKind = ipmk_promise_pure,
        MarkerKindStr = "promise_pure"
    ;
        MarkerKind = ipmk_promise_semipure,
        MarkerKindStr = "promise_semipure"
    ;
        MarkerKind = ipmk_promise_eqv_clauses,
        MarkerKindStr = "promise_equivalent_clauses"
    ),
    mercury_format_pragma_decl_pred_pfu_name_arity(MarkerKindStr,
        PredSpec, "", Stream, !IO).

mercury_format_item_generated_pragma(Info, S, GenPragma, !U) :-
    Context = get_gen_pragma_context(GenPragma),
    maybe_format_line_number(Info, Context, S, !U),
    (
        GenPragma = gen_pragma_unused_args(UnusedArgs),
        mercury_format_pragma_unused_args(S, UnusedArgs, !U)
    ;
        GenPragma = gen_pragma_exceptions(Exceptions),
        mercury_format_pragma_exceptions(S, Exceptions, !U)
    ;
        GenPragma = gen_pragma_trailing(Trailing),
        mercury_format_pragma_trailing(S, Trailing, !U)
    ;
        GenPragma = gen_pragma_mm_tabling(Tabling),
        mercury_format_pragma_mm_tabling(S, Tabling, !U)
    ).

%---------------------------------------------------------------------------%
%
% Output a generic pragma declaration. Used to implement
% several kinds of pragmas.
%

:- pred mercury_format_pragma_decl_pred_or_proc_pfumm_name(string::in,
    pred_or_proc_pfumm_name::in, string::in, S::in, U::di, U::uo)
    is det <= pt_output(S, U).

mercury_format_pragma_decl_pred_or_proc_pfumm_name(PragmaName,
        PredOrProcSpec, AfterStr, S, !U) :-
    PredOrProcSpecStr =
        pred_or_proc_pfumm_name_to_string(output_mercury, PredOrProcSpec),
    add_string(":- pragma ", S, !U),
    add_string(PragmaName, S, !U),
    add_string("(", S, !U),
    add_string(PredOrProcSpecStr, S, !U),
    add_string(AfterStr, S, !U),
    add_string(").\n", S, !U).

%---------------------%

mercury_output_pragma_decl_pred_pf_name_arity(Stream, PragmaName, PredSpec,
        MaybeAfter, !IO) :-
    mercury_format_pragma_decl_pred_pf_name_arity(PragmaName, PredSpec,
        MaybeAfter, Stream, !IO).

mercury_pragma_decl_pred_pf_name_arity_to_string(PragmaName, PredSpec,
        MaybeAfter) = Str :-
    State0 = string.builder.init,
    mercury_format_pragma_decl_pred_pf_name_arity(PragmaName, PredSpec,
        MaybeAfter, string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_format_pragma_decl_pred_pf_name_arity(PragmaName, PredSpec0,
        AfterStr, S, !U) :-
    add_string(":- pragma ", S, !U),
    add_string(PragmaName, S, !U),
    add_string("(", S, !U),
    PredSpec0 = pred_pf_name_arity(PredOrFunc, PredName, Arity),
    mercury_format_pred_pf_name_arity(PredOrFunc, PredName, Arity, S, !U),
    add_string(AfterStr, S, !U),
    add_string(").\n", S, !U).

%---------------------%

:- pred mercury_format_pragma_decl_pred_pfu_name_arity(string::in,
    pred_pfu_name_arity::in, string::in, S::in, U::di, U::uo)
    is det <= pt_output(S, U).

mercury_format_pragma_decl_pred_pfu_name_arity(PragmaName, PredSpec0,
        AfterStr, S, !U) :-
    add_string(":- pragma ", S, !U),
    add_string(PragmaName, S, !U),
    add_string("(", S, !U),
    PredSpec0 = pred_pfu_name_arity(PFU, PredName, Arity),
    mercury_format_pred_pfu_name_arity(PFU, PredName, Arity, S, !U),
    add_string(AfterStr, S, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%
%
% Output a foreign_decl pragma.
%

mercury_output_pragma_foreign_decl(Stream, FDInfo, !IO) :-
    mercury_format_pragma_foreign_decl(FDInfo, Stream, !IO).

mercury_pragma_foreign_decl_to_string(FDInfo) = Str :-
    State0 = string.builder.init,
    mercury_format_pragma_foreign_decl(FDInfo,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_format_pragma_foreign_decl(FDInfo, S, !U) :-
    FDInfo = impl_pragma_foreign_decl_info(Lang, IsLocal, LiteralOrInclude,
        _, _),
    add_string(":- pragma foreign_decl(", S, !U),
    mercury_format_foreign_language_string(Lang, S, !U),
    add_string(", ", S, !U),
    (
        IsLocal = foreign_decl_is_local,
        add_string("local", S, !U)
    ;
        IsLocal = foreign_decl_is_exported,
        add_string("exported", S, !U)
    ),
    add_string(", ", S, !U),
    mercury_format_foreign_literal_or_include(LiteralOrInclude, S, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%
%
% Output a foreign_code pragma.
%

:- pred mercury_format_pragma_foreign_code(impl_pragma_foreign_code_info::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pragma_foreign_code(FCInfo, S, !U) :-
    FCInfo = impl_pragma_foreign_code_info(Lang, LiteralOrInclude, _, _),
    LangStr = mercury_foreign_language_to_string(Lang),
    LorIStr = foreign_literal_or_include_to_string(LiteralOrInclude),
    string.format(":- pragma foreign_code(%s, %s).\n",
        [s(LangStr), s(LorIStr)], DeclStr),
    add_string(DeclStr, S, !U).

:- func foreign_literal_or_include_to_string(foreign_literal_or_include)
    =  string.

foreign_literal_or_include_to_string(LiteralOrInclude) = Str :-
    State0 = string.builder.init,
    mercury_format_foreign_literal_or_include(LiteralOrInclude,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

:- pred mercury_format_foreign_literal_or_include(
    foreign_literal_or_include::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_foreign_literal_or_include(LiteralOrInclude, S, !U) :-
    (
        LiteralOrInclude = floi_literal(Code),
        mercury_format_foreign_code_string(Code, S, !U)
    ;
        LiteralOrInclude = floi_include_file(FileName),
        add_string("include_file(", S, !U),
        add_quoted_string(FileName, S, !U),
        add_string(")", S, !U)
    ).

%---------------------%

% The code here is similar to the code for term_io.quote_string,
% but \n and \t are output directly, rather than escaped.
% Any changes here may require corresponding changes to term_io and vice versa.

:- pred mercury_format_foreign_code_string(string::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_foreign_code_string(Str, S, !U) :-
    add_string("""", S, !U),
    mercury_format_escaped_string(Str, S, !U),
    add_string("""", S, !U).

:- pred mercury_format_escaped_string(string::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_escaped_string(String, S, !U) :-
    string.foldl(mercury_format_escaped_char(S), String, !U).

:- pred mercury_format_escaped_char(S::in, char::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_escaped_char(S, Char, !U) :-
    ( if escape_special_char(Char, QuoteChar) then
        add_char('\\', S, !U),
        add_char(QuoteChar, S, !U)
    else if mercury_is_source_char(Char) then
        add_char(Char, S, !U)
    else
        add_string(mercury_escape_char(Char), S, !U)
    ).

    % escape_special_char(Char, EscapeChar):
    %
    % True iff Char is character for which there is a special backslash-escape
    % character EscapeChar that can be used after a backslash in Mercury
    % foreign_code string literals to represent Char.
    %
:- pred escape_special_char(char::in, char::out) is semidet.

escape_special_char('\\', '\\').
escape_special_char('''', '''').
escape_special_char('"', '"').
escape_special_char('\b', 'b').

    % Succeed if Char is a character which is allowed in
    % Mercury string and character literals.
    %
:- pred mercury_is_source_char(char::in) is semidet.

mercury_is_source_char(Char) :-
    ( char.is_alnum(Char)
    ; is_mercury_punctuation_char(Char)
    ; Char = '\n'
    ; Char = '\t'
    ).

%---------------------------------------------------------------------------%
%
% Output a foreign_proc pragma.
%

mercury_output_item_foreign_proc(Stream, Lang, FPInfo, !IO) :-
    mercury_format_item_foreign_proc(Stream, Lang, FPInfo, !IO).

mercury_item_foreign_proc_to_string(Lang, FPInfo) = Str :-
    State0 = string.builder.init,
    mercury_format_item_foreign_proc(string.builder.handle, Lang, FPInfo,
        State0, State),
    Str = string.builder.to_string(State).

mercury_format_item_foreign_proc(S, Lang, FPInfo, !U) :-
    FPInfo = item_foreign_proc_info(Attributes, PredName, PredOrFunc, Vars0,
        ProgVarSet, InstVarSet, PragmaCode, _Context, _SeqNum),
    add_string(":- pragma foreign_proc(", S, !U),
    ForeignLang = get_foreign_language(Attributes),
    mercury_format_foreign_language_string(ForeignLang, S, !U),
    add_string(",\n    ", S, !U),
    mercury_format_sym_name(PredName, S, !U),
    (
        PredOrFunc = pf_predicate,
        Vars = Vars0,
        ResultVars = []
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Vars0, Vars, ResultVar),
        ResultVars = [ResultVar]
    ),
    (
        Vars = []
    ;
        Vars = [_ | _],
        add_string("(", S, !U),
        mercury_format_pragma_foreign_proc_vars(Lang, ProgVarSet, InstVarSet,
            Vars, S, !U),
        add_string(")", S, !U)
    ),
    (
        PredOrFunc = pf_predicate
    ;
        PredOrFunc = pf_function,
        add_string(" = (", S, !U),
        mercury_format_pragma_foreign_proc_vars(Lang, ProgVarSet, InstVarSet,
            ResultVars, S, !U),
        add_string(")", S, !U)
    ),
    add_string(",\n    ", S, !U),
    mercury_format_pragma_foreign_attributes(ProgVarSet, Attributes, S, !U),
    add_string(",\n", S, !U),
    PragmaCode = fp_impl_ordinary(C_Code, _),
    mercury_format_foreign_code_string(C_Code, S, !U),
    add_string(").\n", S, !U).

%---------------------%

:- pred mercury_format_pragma_foreign_proc_vars(output_lang::in,
    prog_varset::in, inst_varset::in, list(pragma_var)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pragma_foreign_proc_vars(_, _, _, [], _S, !U).
mercury_format_pragma_foreign_proc_vars(Lang, ProgVarSet, InstVarSet,
        [Var | Vars], S, !U) :-
    Var = pragma_var(_Var, VarName, Mode, _BoxPolicy),
    add_string(VarName, S, !U),
    add_string(" :: ", S, !U),
    mercury_format_mode(Lang, InstVarSet, Mode, S, !U),
    (
        Vars = []
    ;
        Vars = [_ | _],
        add_string(", ", S, !U)
    ),
    mercury_format_pragma_foreign_proc_vars(Lang, ProgVarSet, InstVarSet,
        Vars, S, !U).

%---------------------%

:- pred mercury_format_pragma_foreign_attributes(prog_varset::in,
    foreign_proc_attributes::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pragma_foreign_attributes(VarSet, Attributes, S, !U) :-
    add_string("[", S, !U),
    add_list(add_string, ", ",
        foreign_proc_attributes_to_strings(Attributes, VarSet), S, !U),
    add_string("]", S, !U).

    % Convert the foreign code attributes to their source code representations
    % suitable for placing in the attributes list of the pragma (not all
    % attributes have one). In particular, the foreign language attribute needs
    % to be handled separately as it belongs at the start of the pragma.
    %
:- func foreign_proc_attributes_to_strings(foreign_proc_attributes,
    prog_varset) = list(string).

foreign_proc_attributes_to_strings(Attrs, VarSet) = StringList :-
    MayCallMercury = get_may_call_mercury(Attrs),
    ThreadSafe = get_thread_safe(Attrs),
    TabledForIO = get_tabled_for_io(Attrs),
    Purity = get_purity(Attrs),
    Terminates = get_terminates(Attrs),
    UserSharing = get_user_annotated_sharing(Attrs),
    OrdinaryDespiteDetism = get_ordinary_despite_detism(Attrs),
    Exceptions = get_may_throw_exception(Attrs),
    MayModifyTrail = get_may_modify_trail(Attrs),
    MayCallMM_Tabled = get_may_call_mm_tabled(Attrs),
    BoxPolicy = get_box_policy(Attrs),
    AffectsLiveness = get_affects_liveness(Attrs),
    AllocatesMemory = get_allocates_memory(Attrs),
    RegistersRoots = get_registers_roots(Attrs),
    RefersToLldsStack = get_refers_to_llds_stack(Attrs),
    CallStdOutRegs = get_call_std_out_regs(Attrs),
    MaybeMayDuplicate = get_may_duplicate(Attrs),
    MaybeMayExportBody = get_may_export_body(Attrs),
    ForSpecificBackend = get_for_specific_backend(Attrs),
    (
        MayCallMercury = proc_may_call_mercury,
        MayCallMercuryStr = "may_call_mercury"
    ;
        MayCallMercury = proc_will_not_call_mercury,
        MayCallMercuryStr = "will_not_call_mercury"
    ),
    (
        ThreadSafe = proc_not_thread_safe,
        ThreadSafeStr = "not_thread_safe"
    ;
        ThreadSafe = proc_thread_safe,
        ThreadSafeStr = "thread_safe"
    ;
        ThreadSafe = proc_maybe_thread_safe,
        ThreadSafeStr = "maybe_thread_safe"
    ),
    (
        TabledForIO = proc_tabled_for_io,
        TabledForIOStr = "tabled_for_io"
    ;
        TabledForIO = proc_tabled_for_io_unitize,
        TabledForIOStr = "tabled_for_io_unitize"
    ;
        TabledForIO = proc_tabled_for_descendant_io,
        TabledForIOStr = "tabled_for_descendant_io"
    ;
        TabledForIO = proc_not_tabled_for_io,
        TabledForIOStr = "not_tabled_for_io"
    ),
    (
        Purity = purity_pure,
        PurityStrList = ["promise_pure"]
    ;
        Purity = purity_semipure,
        PurityStrList = ["promise_semipure"]
    ;
        Purity = purity_impure,
        PurityStrList = []
    ),
    (
        Terminates = proc_terminates,
        TerminatesStrList = ["terminates"]
    ;
        Terminates = proc_does_not_terminate,
        TerminatesStrList = ["does_not_terminate"]
    ;
        Terminates = depends_on_mercury_calls,
        TerminatesStrList = []
    ),
    (
        UserSharing = user_sharing(Sharing, MaybeTypes),
        String = user_annotated_sharing_to_string(VarSet, Sharing, MaybeTypes),
        UserSharingStrList = [String]
    ;
        UserSharing = no_user_annotated_sharing,
        UserSharingStrList = []
    ),
    (
        OrdinaryDespiteDetism = not_ordinary_despite_detism,
        OrdinaryDespiteDetismStrList = []
    ;
        OrdinaryDespiteDetism = ordinary_despite_detism,
        OrdinaryDespiteDetismStrList = ["ordinary_despite_detism"]
    ),
    (
        Exceptions = proc_will_not_throw_exception,
        ExceptionsStrList = ["will_not_throw_exception"]
    ;
        Exceptions = default_exception_behaviour,
        ExceptionsStrList = []
    ),
    (
        MayModifyTrail = proc_may_modify_trail,
        MayModifyTrailStrList = ["may_modify_trail"]
    ;
        MayModifyTrail = proc_will_not_modify_trail,
        MayModifyTrailStrList = ["will_not_modify_trail"]
    ),
    (
        MayCallMM_Tabled = proc_may_call_mm_tabled,
        MayCallMM_TabledStrList = ["may_call_mm_tabled"]
    ;
        MayCallMM_Tabled = proc_will_not_call_mm_tabled,
        MayCallMM_TabledStrList =["will_not_call_mm_tabled"]
    ;
        MayCallMM_Tabled = proc_default_calls_mm_tabled,
        MayCallMM_TabledStrList = []
    ),
    (
        BoxPolicy = bp_native_if_possible,
        BoxPolicyStrList = []
    ;
        BoxPolicy = bp_always_boxed,
        BoxPolicyStrList = ["always_boxed"]
    ),
    (
        AffectsLiveness = proc_affects_liveness,
        AffectsLivenessStrList = ["affects_liveness"]
    ;
        AffectsLiveness = proc_does_not_affect_liveness,
        AffectsLivenessStrList = ["doesnt_affect_liveness"]
    ;
        AffectsLiveness = proc_default_affects_liveness,
        AffectsLivenessStrList = []
    ),
    (
        AllocatesMemory = proc_does_not_allocate_memory,
        AllocatesMemoryStrList =["doesnt_allocate_memory"]
    ;
        AllocatesMemory = proc_allocates_bounded_memory,
        AllocatesMemoryStrList = ["allocates_bounded_memory"]
    ;
        AllocatesMemory = proc_allocates_unbounded_memory,
        AllocatesMemoryStrList = ["allocates_unbounded_memory"]
    ;
        AllocatesMemory = proc_default_allocates_memory,
        AllocatesMemoryStrList = []
    ),
    (
        RegistersRoots = proc_registers_roots,
        RegistersRootsStrList = ["registers_roots"]
    ;
        RegistersRoots = proc_does_not_register_roots,
        RegistersRootsStrList =["doesnt_register_roots"]
    ;
        RegistersRoots = proc_does_not_have_roots,
        RegistersRootsStrList = ["doesnt_have_roots"]
    ;
        RegistersRoots = proc_default_registers_roots,
        RegistersRootsStrList = []
    ),
    (
        RefersToLldsStack = does_not_refer_to_llds_stack,
        RefersToLldsStackList = []
    ;
        RefersToLldsStack = refers_to_llds_stack,
        RefersToLldsStackList = ["refers_to_llds_stack"]
    ),
    (
        CallStdOutRegs = no_request_for_call_std_out_regs,
        CallStdOutRegsList = []
    ;
        CallStdOutRegs = needs_call_std_out_regs,
        CallStdOutRegsList = ["needs_call_standard_output_registers"]
    ),
    (
        MaybeMayDuplicate = yes(MayDuplicate),
        (
            MayDuplicate = proc_may_duplicate,
            MayDuplicateStrList = ["may_duplicate"]
        ;
            MayDuplicate = proc_may_not_duplicate,
            MayDuplicateStrList = ["may_not_duplicate"]
        )
    ;
        MaybeMayDuplicate = no,
        MayDuplicateStrList = []
    ),
    (
        MaybeMayExportBody = yes(MayExportBody),
        (
            MayExportBody = proc_may_export_body,
            MayExportBodyStrList = ["may_export_body"]
        ;
            MayExportBody = proc_may_not_export_body,
            MayExportBodyStrList = ["may_not_export_body"]
        )
    ;
        MaybeMayExportBody = no,
        MayExportBodyStrList = []
    ),
    (
        ForSpecificBackend = no,
        ForSpecificBackendList = []
    ;
        ForSpecificBackend = yes(low_level_backend),
        ForSpecificBackendList = ["low_level_backend"]
    ;
        ForSpecificBackend = yes(high_level_backend),
        ForSpecificBackendList = ["high_level_backend"]
    ),
    StringList = [MayCallMercuryStr, ThreadSafeStr, TabledForIOStr |
        PurityStrList] ++ TerminatesStrList ++ UserSharingStrList ++
        OrdinaryDespiteDetismStrList ++
        ExceptionsStrList ++ MayModifyTrailStrList ++
        MayCallMM_TabledStrList ++ BoxPolicyStrList ++
        AffectsLivenessStrList ++ AllocatesMemoryStrList ++
        RegistersRootsStrList ++ RefersToLldsStackList ++ CallStdOutRegsList ++
        MayDuplicateStrList ++ MayExportBodyStrList ++
        ForSpecificBackendList.

:- func user_annotated_sharing_to_string(prog_varset, structure_sharing_domain,
    maybe(user_sharing_type_information)) = string.

user_annotated_sharing_to_string(VarSet, Sharing, MaybeTypes) = String :-
    (
        Sharing = structure_sharing_bottom,
        String = "no_sharing"
    ;
        Sharing = structure_sharing_top(_),
        String = "unknown_sharing"
    ;
        Sharing = structure_sharing_real(SharingPairs),
        (
            MaybeTypes = yes(user_type_info(Types, TypeVarSet)),
            TypeStrs = list.map(
                mercury_type_to_string(TypeVarSet, print_name_only),
                Types),
            TypeListStr = string.join_list(", ", TypeStrs),
            MaybeTypesStr = "yes(" ++ TypeListStr ++ ")"
        ;
            MaybeTypes = no,
            MaybeTypesStr = "no",
            TypeVarSet = varset.init
        ),
        SharingPairStrs = list.map(sharing_pair_to_string(VarSet, TypeVarSet),
            SharingPairs),
        SharingPairListStr = string.join_list(", ", SharingPairStrs),
        String = string.append_list(
            ["sharing(", MaybeTypesStr, ", [", SharingPairListStr, "])"])
    ).

:- func sharing_pair_to_string(prog_varset, tvarset, structure_sharing_pair)
    = string.

sharing_pair_to_string(VarSet, TypeVarSet, DataA - DataB) = Str :-
    DataA = selected_cel(VarA, SelectorA),
    DataB = selected_cel(VarB, SelectorB),
    VarStrA = mercury_var_to_string_vs(VarSet, print_name_only, VarA),
    VarStrB = mercury_var_to_string_vs(VarSet, print_name_only, VarB),
    SelectorStrA = selector_to_string(TypeVarSet, SelectorA),
    SelectorStrB = selector_to_string(TypeVarSet, SelectorB),
    StrA = "cel(" ++ VarStrA ++ ", [" ++ SelectorStrA ++ "])",
    StrB = "cel(" ++ VarStrB ++ ", [" ++ SelectorStrB ++ "])",
    Str = StrA ++ " - " ++ StrB.

:- func selector_to_string(tvarset, selector) = string.

selector_to_string(TypeVarSet, Selector) = String :-
    UnitStrs = list.map(unit_selector_to_string(TypeVarSet), Selector),
    String = string.join_list(", ", UnitStrs).

:- func unit_selector_to_string(tvarset, unit_selector) = string.

unit_selector_to_string(TypeVarSet, UnitSelector) = String :-
    (
        UnitSelector = typesel(Type),
        String = mercury_type_to_string(TypeVarSet, print_name_only, Type)
    ;
        UnitSelector = termsel(_, _),
        unexpected($pred, "termsel in user-annotated sharing")
    ).

%---------------------------------------------------------------------------%
%
% Output a foreign_proc_export pragma.
%

:- pred mercury_format_pragma_foreign_proc_export(S::in, output_lang::in,
    impl_pragma_fproc_export_info::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pragma_foreign_proc_export(S, Lang, FPE, !U) :-
    FPE = impl_pragma_fproc_export_info(_Origin, ForeignLang,
        PredNameModesPF, ExportName, VarSet, _, _),
    PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
    add_string(":- pragma foreign_export(", S, !U),
    mercury_format_foreign_language_string(ForeignLang, S, !U),
    add_string(", ", S, !U),
    mercury_format_sym_name(SymName, S, !U),
    varset.coerce(VarSet, InstVarSet),
    (
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        add_string("(", S, !U),
        mercury_format_mode_list(Lang, InstVarSet, ArgModes, S, !U),
        add_string(") = ", S, !U),
        mercury_format_mode(Lang, InstVarSet, RetMode, S, !U)
    ;
        PredOrFunc = pf_predicate,
        add_string("(", S, !U),
        mercury_format_mode_list(Lang, InstVarSet, Modes, S, !U),
        add_string(")", S, !U)
    ),
    add_string(", ", S, !U),
    add_string(ExportName, S, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%
%
% Output an external_proc pragma.
%

:- pred mercury_format_pragma_external_proc(impl_pragma_external_proc_info::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pragma_external_proc(External, S, !U) :-
    External = impl_pragma_external_proc_info(PFNameArity, MaybeBackend, _, _),
    PFNameArity = pred_pf_name_arity(PorF, PredName, user_arity(Arity)),
    PorFStr = pred_or_func_to_str(PorF),
    add_string(":- pragma external_", S, !U),
    add_string(PorFStr, S, !U),
    add_string("(", S, !U),
    mercury_format_sym_name(PredName, S, !U),
    add_string("/", S, !U),
    add_int(Arity, S, !U),
    (
        MaybeBackend = no
    ;
        MaybeBackend = yes(Backend),
        add_string(", [", S, !U),
        add_string(backend_to_string(Backend), S, !U),
        add_string("]", S, !U)
    ),
    add_string(").\n", S, !U).

:- func backend_to_string(backend) = string.

backend_to_string(Backend) = Str :-
    (
        Backend = low_level_backend,
        Str = "low_level_backend"
    ;
        Backend = high_level_backend,
        Str = "high_level_backend"
    ).

%---------------------------------------------------------------------------%
%
% Output a type_spec pragma.
%

mercury_format_pragma_type_spec(S, Lang, TypeSpec, !U) :-
    TypeSpec = decl_pragma_type_spec_info(PFUMM, PredName, _SpecModuleName,
        TypeSubst, VarSet, _, _, _),
    add_string(":- pragma type_spec(", S, !U),
    (
        (
            PFUMM = pfumm_predicate(ModesOrArity),
            PredOrFunc = pf_predicate
        ;
            PFUMM = pfumm_function(ModesOrArity),
            PredOrFunc = pf_function
        ),
        (
            ModesOrArity = moa_modes(Modes),
            PredNameModesPF = proc_pf_name_modes(PredOrFunc, PredName, Modes),
            PredStr = proc_pf_name_modes_to_string(Lang, PredNameModesPF),
            add_string(PredStr, S, !U)
        ;
            ModesOrArity = moa_arity(Arity),
            mercury_format_pred_pf_name_arity(PredOrFunc, PredName, Arity,
                S, !U)
        )
    ;
        PFUMM = pfumm_unknown(PredArity),
        mercury_format_pred_name_arity(PredName, PredArity, S, !U)
    ),
    add_string(", ", S, !U),
    % The code that parses type_spec pragmas ensures that all types variables
    % in the substitution are named. Therefore there is no reason to print
    % variable numbers. In fact, printing variable numbers would be a bug,
    % since any code reading the pragma we are now writing out would mistake
    % the variable number as part of the variable *name*. See the long comment
    % on the tspec_tvarset field of the pragma in prog_item.m.
    TypeSubst = one_or_more(HeadTypeSubst, TailTypeSubsts),
    (
        TailTypeSubsts = [],
        % In the common case of there being only type substitution,
        % do not put unnecessary parentheses around it.
        mercury_format_type_subst(VarSet, print_name_only, HeadTypeSubst,
            S, !U)
    ;
        TailTypeSubsts = [_ | _],
        add_string("(", S, !U),
        add_list(mercury_format_type_subst(VarSet, print_name_only), ", ",
            [HeadTypeSubst | TailTypeSubsts], S, !U),
        add_string(")", S, !U)
    ),
    add_string(").\n", S, !U).

:- pred mercury_format_type_subst(tvarset::in, var_name_print::in,
    pair(tvar, mer_type)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_type_subst(VarSet, VarNamePrint, Var - Type, S, !U) :-
    mercury_format_var_vs(VarSet, VarNamePrint, Var, S, !U),
    add_string(" = ", S, !U),
    mercury_format_type(VarSet, VarNamePrint, Type, S, !U).

%---------------------------------------------------------------------------%
%
% Output an unused_args pragma.
%

mercury_format_pragma_unused_args(S, UnusedArgsInfo, !U) :-
    UnusedArgsInfo =
        gen_pragma_unused_args_info(PredNameArityPFMn, UnusedArgs, _, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName,
        user_arity(Arity), ModeNum),
    PorFStr = pred_or_func_to_full_str(PredOrFunc),
    SymNameStr = mercury_bracketed_sym_name_to_string(SymName),
    UnusedArgsStr = int_list_to_string(UnusedArgs),
    string.format(":- pragma unused_args(%s, %s, %d, %d, [%s]",
        [s(PorFStr), s(SymNameStr), i(Arity), i(ModeNum), s(UnusedArgsStr)],
        DeclStr),
    add_string(DeclStr, S, !U).

%---------------------------------------------------------------------------%
%
% Output an exceptions pragma.
%

mercury_format_pragma_exceptions(S, Exceptions, !U) :-
    Exceptions = gen_pragma_exceptions_info(PredNameArityPFMn, ThrowStatus,
        _, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName,
        user_arity(Arity), ModeNum),
    PorFStr = pred_or_func_to_full_str(PredOrFunc),
    SymNameStr = mercury_bracketed_sym_name_to_string(SymName),
    (
        ThrowStatus = will_not_throw,
        ThrowStr = "will_not_throw"
    ;
        ThrowStatus = may_throw(ExceptionType),
        (
            ExceptionType = user_exception,
            ThrowStr = "may_throw(user_exception)"
        ;
            ExceptionType = type_exception,
            ThrowStr = "may_throw(type_exception)"
        )
    ;
        ThrowStatus = throw_conditional,
        ThrowStr = "conditional"
    ),
    string.format(":- pragma exceptions(%s, %s, %d, %d, %s).\n",
        [s(PorFStr), s(SymNameStr), i(Arity), i(ModeNum), s(ThrowStr)],
        DeclStr),
    add_string(DeclStr, S, !U).

%---------------------------------------------------------------------------%
%
% Output a trailing pragma.
%

mercury_format_pragma_trailing(S, Trailing, !U) :-
    Trailing = gen_pragma_trailing_info(PredNameArityPFMn, TrailingStatus,
        _, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName,
        user_arity(Arity), ModeNum),
    PorFStr = pred_or_func_to_full_str(PredOrFunc),
    SymNameStr = mercury_bracketed_sym_name_to_string(SymName),
    (
        TrailingStatus = trail_may_modify,
        TrailStr = "may_modify_trail"
    ;
        TrailingStatus = trail_will_not_modify,
        TrailStr = "will_not_modify_trail"
    ;
        TrailingStatus = trail_conditional,
        TrailStr = "conditional"
    ),
    string.format(":- pragma trailing_info(%s, %s, %d, %d, %s).\n",
        [s(PorFStr), s(SymNameStr), i(Arity), i(ModeNum), s(TrailStr)],
        DeclStr),
    add_string(DeclStr, S, !U).

%---------------------------------------------------------------------------%
%
% Output a mm_tabling pragma.
%

mercury_format_pragma_mm_tabling(S, MMTabling, !U) :-
    MMTabling = gen_pragma_mm_tabling_info(PredNameArityPFMn,
        MM_TablingStatus, _, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName,
        user_arity(Arity), ModeNum),
    PorFStr = pred_or_func_to_full_str(PredOrFunc),
    SymNameStr = mercury_bracketed_sym_name_to_string(SymName),
    (
        MM_TablingStatus = mm_tabled_may_call,
        MMStr = "mm_tabled_may_call"
    ;
        MM_TablingStatus = mm_tabled_will_not_call,
        MMStr = "mm_tabled_will_not_call"
    ;
        MM_TablingStatus = mm_tabled_conditional,
        MMStr = "mm_tabled_conditional"
    ),
    string.format(":- pragma mm_tabling_info(%s, %s, %d, %d, %s).\n",
        [s(PorFStr), s(SymNameStr), i(Arity), i(ModeNum), s(MMStr)],
        DeclStr),
    add_string(DeclStr, S, !U).

%---------------------------------------------------------------------------%
%
% Output an obsolete_pred or obsolete_proc pragma.
%

:- pred mercury_format_pragma_obsolete_pred(decl_pragma_obsolete_pred_info::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pragma_obsolete_pred(ObsoletePred, S, !U) :-
    ObsoletePred =
        decl_pragma_obsolete_pred_info(PredSpec, ObsoleteInFavourOf, _, _),
    PredSpec = pred_pfu_name_arity(PFU, PredName, Arity),
    PredStr = mercury_pred_pfu_name_arity_to_string(PFU, PredName, Arity),
    ObsoleteStrs = list.map(wrapped_sym_name_arity_to_string,
        ObsoleteInFavourOf),
    ObsoleteStr = string.join_list(", ", ObsoleteStrs),
    string.format(":- pragma obsolete(%s, [%s]).\n",
        [s(PredStr), s(ObsoleteStr)], DeclStr),
    add_string(DeclStr, S, !U).

:- pred mercury_format_pragma_obsolete_proc(output_lang::in,
    decl_pragma_obsolete_proc_info::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_pragma_obsolete_proc(Lang, ObsoleteProc, S, !U) :-
    ObsoleteProc = decl_pragma_obsolete_proc_info(PredNameModesPF,
        ObsoleteInFavourOf, _, _),
    PredStr = proc_pf_name_modes_to_string(Lang, PredNameModesPF),
    ObsoleteStrs = list.map(wrapped_sym_name_arity_to_string,
        ObsoleteInFavourOf),
    ObsoleteStr = string.join_list(", ", ObsoleteStrs),
    string.format(":- pragma obsolete_proc(%s, [%s]).\n",
        [s(PredStr), s(ObsoleteStr)], DeclStr),
    add_string(DeclStr, S, !U).

:- func wrapped_sym_name_arity_to_string(sym_name_arity) = string.

wrapped_sym_name_arity_to_string(SNA) = Str :-
    SNA = sym_name_arity(SymName, Arity),
    Str = mercury_bracketed_sym_name_to_string(SymName) ++
        "/" ++ string.int_to_string(Arity).

%---------------------------------------------------------------------------%
%
% Output a format_call pragma.
%

:- pred mercury_format_pragma_format_call(decl_pragma_format_call_info::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pragma_format_call(FormatCall, S, !U) :-
    FormatCall = decl_pragma_format_call_info(PredSpec, OoMFormatArgsSpecs,
        _, _),
    PredSpec = pred_pf_name_arity(PF, PredName, Arity),
    PredStr = mercury_pred_pf_name_arity_to_string(PF, PredName, Arity),
    OoMFormatArgsSpecs = one_or_more(HeadFormatArgsSpec, TailFormatArgsSpecs),
    (
        TailFormatArgsSpecs = [],
        SecondArgStr = format_string_values_to_string(HeadFormatArgsSpec)
    ;
        TailFormatArgsSpecs = [_ | _],
        ArgsSpecsStrs = list.map(format_string_values_to_string,
            [HeadFormatArgsSpec | TailFormatArgsSpecs]),
        ArgsSpecsStr = string.join_list(", ", ArgsSpecsStrs),
        string.format("[%s]", [s(ArgsSpecsStr)], SecondArgStr)
    ),
    string.format(":- pragma format_call(%s, [%s]).\n",
        [s(PredStr), s(SecondArgStr)], DeclStr),
    add_string(DeclStr, S, !U).

:- func format_string_values_to_string(format_string_values) = string.

format_string_values_to_string(FormatStringValues) = Str :-
    FormatStringValues = format_string_values(OrigFS, OrigVL, _CurFS, _CurVL),
    string.format("format_string_values(%d, %d)", [i(OrigFS), i(OrigVL)], Str).

%---------------------------------------------------------------------------%
%
% Output a require tail recursion pragma.
%

:- pred mercury_format_pragma_require_tail_rec(output_lang::in,
    impl_pragma_req_tail_rec_info::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_pragma_require_tail_rec(Lang, RequireTR, S, !U) :-
    RequireTR = impl_pragma_req_tail_rec_info(PredOrProcSpec, Warn, _, _),
    ProcSpecStr = pred_or_proc_pfumm_name_to_string(Lang, PredOrProcSpec),
    (
        Warn = suppress_tailrec_warnings(_),
        string.format(":- pragma warn_tail_recursion(%s, [none]).\n",
            [s(ProcSpecStr)], DeclStr)
    ;
        Warn = enable_tailrec_warnings(WarnOrError, Type, _),
        warning_or_error_string(WarnOrError, WarnOrErrorStr),
        require_tailrec_type_string(Type, TypeStr),
        string.format(":- pragma warn_tail_recursion(%s, [%s, %s]).\n",
            [s(ProcSpecStr), s(WarnOrErrorStr), s(TypeStr)], DeclStr)
    ),
    add_string(DeclStr, S, !U).

:- func pred_or_proc_pfumm_name_to_string(output_lang,
    pred_or_proc_pfumm_name) = string.

pred_or_proc_pfumm_name_to_string(Lang, PredOrProcSpec)
        = PredOrProcSpecStr :-
    PredOrProcSpec = pred_or_proc_pfumm_name(PFUMM, PredName),
    (
        PFUMM = pfumm_unknown(PredArity),
        PredOrProcSpecStr =
            mercury_pred_name_arity_to_string(PredName, PredArity)
    ;
        (
            PFUMM = pfumm_predicate(ModesOrArity),
            PredOrFunc = pf_predicate
        ;
            PFUMM = pfumm_function(ModesOrArity),
            PredOrFunc = pf_function
        ),
        (
            ModesOrArity = moa_arity(Arity),
            PredOrProcSpecStr = mercury_pred_pf_name_arity_to_string(
                PredOrFunc, PredName, Arity)
        ;
            ModesOrArity = moa_modes(Modes),
            MaybeDet = maybe.no,
            % XXX ARITY Compare to the corresponding code in
            % mercury_format_pragma_foreign_proc_export.
            varset.init(InstVarSet), % The varset isn't really used.
            (
                PredOrFunc = pf_predicate,
                PredOrProcSpecStr = mercury_pred_mode_subdecl_to_string(Lang,
                    InstVarSet, PredName, Modes, MaybeDet)
            ;
                PredOrFunc = pf_function,
                pred_args_to_func_args(Modes, ArgModes, RetMode),
                PredOrProcSpecStr = mercury_func_mode_subdecl_to_string(Lang,
                    InstVarSet, PredName, ArgModes, RetMode, MaybeDet)
            )
        )
    ).

%---------------------------------------------------------------------------%
%
% Output a tabled pragma.
%

:- pred mercury_format_pragma_tabled(impl_pragma_tabled_info::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pragma_tabled(Tabled, S, !U) :-
    Tabled = impl_pragma_tabled_info(TabledMethod, PredOrProcSpec,
        MaybeAttributes, _, _),
    PragmaName = tabled_eval_method_to_pragma_name(TabledMethod),
    (
        MaybeAttributes = yes(Attributes),
        Attributes = table_attributes(Strictness, MaybeSizeLimit, Stats,
            AllowReset, WarnForIgnore),
        some [!Strs] (
            !:Strs = [],
            (
                Strictness = cts_all_strict
            ;
                Strictness = cts_all_fast_loose,
                !:Strs = ["fast_loose" | !.Strs]
            ;
                Strictness = cts_specified(Args, HiddenArgMethod),
                ArgStrs = list.map(maybe_arg_tabling_method_to_string, Args),
                ArgsStr = string.join_list(", ", ArgStrs),
                (
                    HiddenArgMethod = table_hidden_arg_value,
                    HiddenArgStr = "hidden_arg_value"
                ;
                    HiddenArgMethod = table_hidden_arg_addr,
                    HiddenArgStr = "hidden_arg_addr"
                ),
                SpecifiedStr = "specified([" ++ ArgsStr ++ "], " ++
                    HiddenArgStr ++ ")",
                !:Strs = [SpecifiedStr | !.Strs]
            ),
            (
                MaybeSizeLimit = yes(SizeLimit),
                LimitStr = "limit(" ++ int_to_string(SizeLimit) ++ ")",
                !:Strs = [LimitStr | !.Strs]
            ;
                MaybeSizeLimit = no
            ),
            (
                Stats = table_gather_statistics,
                !:Strs = ["statistics" | !.Strs]
            ;
                Stats = table_dont_gather_statistics
            ),
            (
                AllowReset = table_allow_reset,
                !:Strs = ["allow_reset" | !.Strs]
            ;
                AllowReset = table_dont_allow_reset
            ),
            (
                WarnForIgnore = table_attr_ignore_with_warning
            ;
                WarnForIgnore = table_attr_ignore_without_warning,
                !:Strs = ["disable_warning_if_ignored" | !.Strs]
            ),
            (
                !.Strs = [],
                AfterStr = ""
            ;
                !.Strs = [_ | _],
                AfterStr = ", [" ++ string.join_list(", ", !.Strs) ++ "]"
            )
        )
    ;
        MaybeAttributes = no,
        AfterStr = ""
    ),
    mercury_format_pragma_decl_pred_or_proc_pfumm_name(PragmaName,
        PredOrProcSpec, AfterStr, S, !U).

%---------------------------------------------------------------------------%
%
% Output a fact_table pragma.
%

:- pred mercury_format_pragma_fact_table(impl_pragma_fact_table_info::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pragma_fact_table(FactTable, S, !U) :-
    FactTable = impl_pragma_fact_table_info(PredSpec, FileName, _, _),
    add_string(":- pragma fact_table(", S, !U),
    PredSpec = pred_pfu_name_arity(PFU, PredName, UserArity),
    mercury_format_pred_pfu_name_arity(PFU, PredName, UserArity, S, !U),
    add_string(", ", S, !U),
    add_quoted_string(FileName, S, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%
%
% Output an oisu (order independent state update) pragma.
%

:- pred mercury_format_pragma_oisu(decl_pragma_oisu_info::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pragma_oisu(OISU, S, !U) :-
    OISU = decl_pragma_oisu_info(TypeCtor, CreatorPreds, MutatorPreds,
        DestructorPreds, _, _),
    add_string(":- pragma oisu(", S, !U),
    TypeCtor = type_ctor(TypeName, TypeArity),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, TypeName,
        S, !U),
    add_string("/", S, !U),
    add_int(TypeArity, S, !U),
    add_string(",\n", S, !U),
    add_string("\tcreators([\n", S, !U),
    mercury_format_pred_pf_name_arity_list(CreatorPreds, S, !U),
    add_string("\t]),\n", S, !U),
    add_string("\tmutators([\n", S, !U),
    mercury_format_pred_pf_name_arity_list(MutatorPreds, S, !U),
    add_string("\t]),\n", S, !U),
    add_string("\tdestructors([\n", S, !U),
    mercury_format_pred_pf_name_arity_list(DestructorPreds, S, !U),
    add_string("\t])\n", S, !U),
    add_string(").\n", S, !U).

:- pred mercury_format_pred_pf_name_arity_list(list(pred_pf_name_arity)::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pred_pf_name_arity_list([], _S, !U).
mercury_format_pred_pf_name_arity_list([PredSpec | PredSpecs], S, !U) :-
    mercury_format_pred_pf_name_arity_list_lag(PredSpec, PredSpecs, S, !U).

:- pred mercury_format_pred_pf_name_arity_list_lag(pred_pf_name_arity::in,
    list(pred_pf_name_arity)::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_pred_pf_name_arity_list_lag(PredSpec, PredSpecs, S, !U) :-
    add_string("\t\t", S, !U),
    PredSpec = pred_pf_name_arity(PredOrFunc, PredName, UserArity),
    mercury_format_pred_pf_name_arity(PredOrFunc, PredName, UserArity, S, !U),
    (
        PredSpecs = [],
        add_string("\n", S, !U)
    ;
        PredSpecs = [HeadPredSpec | TailPredSpecs],
        add_string(",\n", S, !U),
        mercury_format_pred_pf_name_arity_list_lag(HeadPredSpec, TailPredSpecs,
            S, !U)
    ).

%---------------------------------------------------------------------------%
%
% Output a termination_info pragma.
%

mercury_format_pragma_termination(S, Lang, Term, !U) :-
    Term = decl_pragma_termination_info(PredNameModesPF,
        MaybeArgSize, MaybeTermination, _, _),
    PredStr = proc_pf_name_modes_to_string(Lang, PredNameModesPF),
    ArgSizeStr = maybe_arg_size_info_to_string(no, MaybeArgSize),
    TermStr = maybe_termination_info_to_string(no, MaybeTermination),
    string.format(":- pragma termination_info(%s, %s, %s).\n",
        [s(PredStr), s(ArgSizeStr), s(TermStr)], DeclStr),
    add_string(DeclStr, S, !U).

maybe_arg_size_info_to_string(Verbose, MaybeArgSizeInfo) = Str :-
    (
        MaybeArgSizeInfo = no,
        Str = "not_set"
    ;
        MaybeArgSizeInfo = yes(finite(Const, UsedArgs)),
        string.format("finite(%d, %s)",
            [i(Const), s(used_args_to_string(UsedArgs))], Str)
    ;
        MaybeArgSizeInfo = yes(infinite(ErrorInfo)),
        (
            Verbose = no,
            Str = "infinite"
        ;
            Verbose = yes,
            string.format("infinite(%s)", [s(string.string(ErrorInfo))], Str)
        )
    ).

:- func used_args_to_string(list(bool)) = string.

used_args_to_string(UsedArgs) = Str :-
    BoolStrs = list.map(bool_to_string, UsedArgs),
    string.format("[%s]", [s(string.join_list(", ", BoolStrs))], Str).

:- func bool_to_string(bool) = string.

bool_to_string(no) = "no".
bool_to_string(yes) = "yes".

maybe_termination_info_to_string(Verbose, MaybeTerminationInfo) = Str :-
    (
        MaybeTerminationInfo = no,
        Str = "not_set"
    ;
        MaybeTerminationInfo = yes(cannot_loop(_)),
        Str = "cannot_loop"
    ;
        MaybeTerminationInfo = yes(can_loop(ErrorInfo)),
        (
            Verbose = no,
            Str = "can_loop"
        ;
            Verbose = yes,
            string.format("can_loop(%s)", [s(string.string(ErrorInfo))], Str)
        )
    ).

%---------------------------------------------------------------------------%
%
% Output a termination2_info pragma.
%

mercury_format_pragma_termination2(S, Lang, Term2, !U) :-
    Term2 = decl_pragma_termination2_info(PredNameModesPF,
        MaybeSuccess, MaybeFailure, MaybeTermination, _, _),
    PredStr = proc_pf_name_modes_to_string(Lang, PredNameModesPF),
    SuccessStr = maybe_pragma_constr_arg_size_info_to_string(MaybeSuccess),
    FailureStr = maybe_pragma_constr_arg_size_info_to_string(MaybeFailure),
    TermStr = maybe_pragma_termination_info_to_string(MaybeTermination),
    string.format(":- pragma termination2_info(%s, %s, %s, %s).\n",
        [s(PredStr), s(SuccessStr), s(FailureStr), s(TermStr)], DeclStr),
    add_string(DeclStr, S, !U).

:- func maybe_pragma_constr_arg_size_info_to_string(
    maybe(pragma_constr_arg_size_info)) = string.

maybe_pragma_constr_arg_size_info_to_string(no) = "not_set".
maybe_pragma_constr_arg_size_info_to_string(yes(ArgSizeConstraints)) = Str :-
    ConstraintStrs = list.map(arg_size_constr_to_string, ArgSizeConstraints),
    ConstraintsStr = string.join_list(", ", ConstraintStrs),
    Str = string.format("constraints([%s])", [s(ConstraintsStr)]).

:- func arg_size_constr_to_string(arg_size_constr) = string.

arg_size_constr_to_string(Constraint) = Str :-
    (
        Constraint = le(Terms, Constant),
        OpStr = "le"
    ;
        Constraint = eq(Terms, Constant),
        OpStr = "eq"
    ),
    TermStrs = list.map(arg_size_term_to_string, Terms),
    TermsStr = string.join_list(", ", TermStrs),
    Str = string.format("%s([%s], %s)",
        [s(OpStr), s(TermsStr), s(rat.to_rat_string(Constant))]).

:- func arg_size_term_to_string(arg_size_term) = string.

arg_size_term_to_string(arg_size_term(VarId, Coeff)) =
    string.format("term(%d, %s)", [i(VarId), s(rat.to_rat_string(Coeff))]).

:- func maybe_pragma_termination_info_to_string(maybe(pragma_termination_info))
    = string.

maybe_pragma_termination_info_to_string(MaybeTermination) = Str :-
    (
        MaybeTermination = no,
        Str = "not_set"
    ;
        MaybeTermination = yes(Termination),
        (
            Termination = can_loop(_),
            Str = "can_loop"
        ;
            Termination = cannot_loop(_),
            Str = "cannot_loop"
        )
    ).

%---------------------------------------------------------------------------%
%
% Output a structure_sharing pragma.
%

mercury_format_pragma_struct_sharing(S, Lang, Sharing, !U) :-
    Sharing = decl_pragma_struct_sharing_info(PredNameModesPF,
        HeadVars, HeadVarTypes, VarSet, TVarSet, MaybeSharingAs, _, _),
    PredStr = proc_pf_name_modes_to_string(Lang, PredNameModesPF),
    string.format(":- pragma structure_sharing(%s, ", [s(PredStr)], InitStr),
    add_string(InitStr, S, !U),
    format_vars_and_types(S, VarSet, TVarSet, HeadVars, HeadVarTypes, !U),
    add_string(", ", S, !U),
    prog_ctgc.format_interface_structure_sharing_domain(S,
        vns_varset(VarSet), TVarSet, MaybeSharingAs, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%
%
% Output a structure_reuse pragma.
%

mercury_format_pragma_struct_reuse(S, Lang, Reuse, !U) :-
    Reuse = decl_pragma_struct_reuse_info(PredNameModesPF, HeadVars,
        HeadVarTypes, VarSet, TVarSet, MaybeStructureReuseDomain, _, _),
    PredStr = proc_pf_name_modes_to_string(Lang, PredNameModesPF),
    string.format(":- pragma structure_reuse(%s, ", [s(PredStr)], InitStr),
    add_string(InitStr, S, !U),
    format_vars_and_types(S, VarSet, TVarSet, HeadVars, HeadVarTypes, !U),
    add_string(", ", S, !U),
    prog_ctgc.format_interface_maybe_structure_reuse_domain(S,
        vns_varset(VarSet), TVarSet, MaybeStructureReuseDomain, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%
%
% Predicates used to help print both structure_sharing and structure_reuse
% pragmas.
%

:- pred format_vars_and_types(S::in, prog_varset::in, tvarset::in,
    list(prog_var)::in, list(mer_type)::in, U::di, U::uo) is det
    <= pt_output(S, U).

format_vars_and_types(S, VarSet, TypeVarSet, HeadVars, HeadVarTypes, !U) :-
    (
        HeadVars = [],
        add_string("vars, types", S, !U)
    ;
        HeadVars = [_ | _],
        add_string("vars(", S, !U),
        mercury_format_vars_vs(VarSet, print_name_only, HeadVars, S, !U),
        add_string("), ", S, !U),

        add_string("types(", S, !U),
        add_list(mercury_format_type(TypeVarSet, print_name_only),
            ", ", HeadVarTypes, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%
%
% Output a require_feature_set pragma.
%

:- pred mercury_format_pragma_require_feature_set(
    impl_pragma_req_feature_set_info::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pragma_require_feature_set(RFSInfo, S, !U) :-
    RFSInfo = impl_pragma_req_feature_set_info(Features0, _, _),
    Features = set.to_sorted_list(Features0),
    add_string(":- pragma require_feature_set(", S, !U),
    add_list(mercury_format_required_feature, ", ", Features, S, !U),
    add_string(").\n", S, !U).

:- pred mercury_format_required_feature(required_feature::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_required_feature(Feature, S, !U) :-
    ( Feature = reqf_concurrency, Str = "concurrency"
    ; Feature = reqf_single_prec_float, Str = "single_prec_float"
    ; Feature = reqf_double_prec_float, Str = "double_prec_float"
    ; Feature = reqf_memo, Str = "memo"
    ; Feature = reqf_parallel_conj, Str = "parallel_conj"
    ; Feature = reqf_trailing, Str = "trailing"
    ; Feature = reqf_strict_sequential, Str = "strict_sequential"
    ; Feature = reqf_conservative_gc, Str = "conservative_gc"
    ),
    add_string(Str, S, !U).

%---------------------------------------------------------------------------%
%
% Utility predicates.
%

:- func mercury_pred_name_arity_to_string(sym_name, user_arity) = string.

mercury_pred_name_arity_to_string(PredName, UserArity) = Str :-
    State0 = string.builder.init,
    mercury_format_pred_name_arity(PredName, UserArity,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

:- pred mercury_format_pred_name_arity(sym_name::in, user_arity::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pred_name_arity(PredName, user_arity(Arity), S, !U) :-
    NGT = next_to_graphic_token,
    mercury_format_bracketed_sym_name_ngt(NGT, PredName, S, !U),
    add_string("/", S, !U),
    add_int(Arity, S, !U).

:- func mercury_pred_pf_name_arity_to_string(pred_or_func, sym_name,
    user_arity) = string.

mercury_pred_pf_name_arity_to_string(PredOrFunc, PredName, UserArity) = Str :-
    State0 = string.builder.init,
    mercury_format_pred_pf_name_arity(PredOrFunc, PredName, UserArity,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

:- pred mercury_format_pred_pf_name_arity(pred_or_func::in,
    sym_name::in, user_arity::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_pred_pf_name_arity(PredOrFunc, PredName, UserArity, S, !U) :-
    add_string(pred_or_func_to_str(PredOrFunc), S, !U),
    add_string("(", S, !U),
    mercury_format_pred_name_arity(PredName, UserArity, S, !U),
    add_string(")", S, !U).

:- func mercury_pred_pfu_name_arity_to_string(pred_func_or_unknown,
    sym_name, user_arity) = string.

mercury_pred_pfu_name_arity_to_string(PFU, PredName, UserArity) = Str :-
    State0 = string.builder.init,
    mercury_format_pred_pfu_name_arity(PFU, PredName, UserArity,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

:- pred mercury_format_pred_pfu_name_arity(pred_func_or_unknown::in,
    sym_name::in, user_arity::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_pred_pfu_name_arity(PFU, PredName, UserArity, S, !U) :-
    (
        ( PFU = pfu_predicate, PredOrFunc = pf_predicate
        ; PFU = pfu_function,  PredOrFunc = pf_function
        ),
        mercury_format_pred_pf_name_arity(PredOrFunc, PredName, UserArity,
            S, !U)
    ;
        PFU = pfu_unknown,
        mercury_format_pred_name_arity(PredName, UserArity, S, !U)
    ).

%---------------------%

:- func proc_pf_name_modes_to_string(output_lang, proc_pf_name_modes) = string.

proc_pf_name_modes_to_string(Lang, PredNameModesPF) = Str :-
    PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
    varset.init(InstVarSet),
    MaybeDet = no,
    (
        PredOrFunc = pf_predicate,
        Str = mercury_pred_mode_subdecl_to_string(Lang, InstVarSet, SymName,
            Modes, MaybeDet)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, FuncArgModes, ReturnArgMode),
        Str = mercury_func_mode_subdecl_to_string(Lang, InstVarSet, SymName,
            FuncArgModes, ReturnArgMode, MaybeDet)
    ).

%---------------------------------------------------------------------------%

:- func int_list_to_string(list(int)) = string.

int_list_to_string(Ints) = Str :-
    State0 = string.builder.init,
    mercury_format_int_list(Ints, string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

:- pred mercury_format_int_list(list(int)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_int_list([], _S, !U).
mercury_format_int_list([Head | Tail], S, !U) :-
    add_int(Head, S, !U),
    mercury_format_int_list_2(Tail, S, !U).

:- pred mercury_format_int_list_2(list(int)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_int_list_2([], _S, !U).
mercury_format_int_list_2([Head | Tail], S, !U) :-
    add_string(", ", S, !U),
    add_int(Head, S, !U),
    mercury_format_int_list_2(Tail, S, !U).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_pragma.
%---------------------------------------------------------------------------%
