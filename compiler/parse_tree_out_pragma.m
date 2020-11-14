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

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred mercury_output_item_decl_pragma(merc_out_info::in,
    io.text_output_stream::in, item_decl_pragma_info::in,
    io::di, io::uo) is det.
:- pred mercury_output_item_impl_pragma(merc_out_info::in,
    io.text_output_stream::in, item_impl_pragma_info::in,
    io::di, io::uo) is det.
:- pred mercury_output_item_generated_pragma(merc_out_info::in,
    io.text_output_stream::in, item_generated_pragma_info::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mercury_output_item_pred_marker(io.text_output_stream::in,
    pragma_info_pred_marker::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mercury_output_pragma_decl(io.text_output_stream::in,
    sym_name::in, int::in, pred_or_func::in, string::in, maybe(string)::in,
    io::di, io::uo) is det.
:- func mercury_pragma_decl_to_string(sym_name, int, pred_or_func, string,
    maybe(string)) = string.

:- pred mercury_output_pragma_foreign_decl(io.text_output_stream::in,
    pragma_info_foreign_decl::in, io::di, io::uo) is det.
:- func mercury_pragma_foreign_decl_to_string(pragma_info_foreign_decl)
    = string.

:- pred mercury_output_pragma_foreign_proc(io.text_output_stream::in,
    output_lang::in, pragma_info_foreign_proc::in, io::di, io::uo) is det.
:- func mercury_pragma_foreign_proc_to_string(output_lang,
    pragma_info_foreign_proc) = string.

:- pred mercury_output_pragma_type_spec(io.text_output_stream::in,
    var_name_print::in, output_lang::in, pragma_info_type_spec::in,
    io::di, io::uo) is det.

:- pred mercury_output_pragma_unused_args(io.text_output_stream::in,
    pragma_info_unused_args::in, io::di, io::uo) is det.

:- pred mercury_output_pragma_exceptions(io.text_output_stream::in,
    pragma_info_exceptions::in, io::di, io::uo) is det.

:- pred mercury_output_pragma_trailing_info(io.text_output_stream::in,
    pragma_info_trailing_info::in, io::di, io::uo) is det.

:- pred mercury_output_pragma_mm_tabling_info(io.text_output_stream::in,
    pragma_info_mm_tabling_info::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % This predicate outputs termination_info pragmas;
    % such annotations can be part of .opt and .trans_opt files.
    %
:- pred write_pragma_termination_info(io.text_output_stream::in,
    output_lang::in, pragma_info_termination_info::in, io::di, io::uo) is det.

    % Write the given arg size info. Verbose if the first arg is yes.
    %
:- pred write_maybe_arg_size_info(io.text_output_stream::in, bool::in,
    maybe(generic_arg_size_info(T))::in, io::di, io::uo) is det.

    % Write the given termination info. Verbose if the first arg is yes.
    %
:- pred write_maybe_termination_info(io.text_output_stream::in, bool::in,
    maybe(generic_termination_info(S, T))::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred write_pragma_termination2_info(io.text_output_stream::in,
    output_lang::in, pragma_info_termination2_info::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred write_pragma_structure_sharing_info(io.text_output_stream::in,
    output_lang::in, pragma_info_structure_sharing::in, io::di, io::uo) is det.

:- pred write_pragma_structure_reuse_info(io.text_output_stream::in,
    output_lang::in, pragma_info_structure_reuse::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.rat.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module unit.
:- import_module varset.

%---------------------------------------------------------------------------%

mercury_output_item_decl_pragma(Info, Stream, ItemDeclPragma, !IO) :-
    ItemDeclPragma = item_pragma_info(Pragma, Context, _SeqNum),
    maybe_output_line_number(Info, Context, Stream, !IO),
    Lang = get_output_lang(Info),
    (
        Pragma = decl_pragma_obsolete_pred(ObsoletePredInfo),
        mercury_output_pragma_obsolete_pred(Stream, ObsoletePredInfo, !IO)
    ;
        Pragma = decl_pragma_obsolete_proc(ObsoleteProcInfo),
        mercury_output_pragma_obsolete_proc(Stream, Lang, ObsoleteProcInfo, !IO)
    ;
        Pragma = decl_pragma_type_spec(TypeSpecInfo),
        VarNamePrint = print_name_only,
        mercury_output_pragma_type_spec(Stream, VarNamePrint, Lang,
            TypeSpecInfo, !IO)
    ;
        Pragma = decl_pragma_oisu(OISUInfo),
        mercury_output_pragma_oisu(Stream, OISUInfo, !IO)
    ;
        Pragma = decl_pragma_terminates(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "terminates", no, !IO)
    ;
        Pragma = decl_pragma_does_not_terminate(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "does_not_terminate", no, !IO)
    ;
        Pragma = decl_pragma_check_termination(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "check_termination", no, !IO)
    ;
        Pragma = decl_pragma_termination_info(TermInfo),
        write_pragma_termination_info(Stream, Lang, TermInfo, !IO)
    ;
        Pragma = decl_pragma_termination2_info(Term2Info),
        write_pragma_termination2_info(Stream, Lang, Term2Info, !IO)
    ;
        Pragma = decl_pragma_structure_sharing(SharingInfo),
        write_pragma_structure_sharing_info(Stream, Lang, SharingInfo, !IO)
    ;
        Pragma = decl_pragma_structure_reuse(ReuseInfo),
        write_pragma_structure_reuse_info(Stream, Lang, ReuseInfo, !IO)
    ).

mercury_output_item_impl_pragma(Info, Stream, ItemImplPragma, !IO) :-
    ItemImplPragma = item_pragma_info(Pragma, Context, _SeqNum),
    maybe_output_line_number(Info, Context, Stream, !IO),
    Lang = get_output_lang(Info),
    (
        Pragma = impl_pragma_foreign_decl(FDInfo),
        mercury_output_pragma_foreign_decl(Stream, FDInfo, !IO)
    ;
        Pragma = impl_pragma_foreign_code(FCInfo),
        mercury_output_pragma_foreign_code(Stream, FCInfo, !IO)
    ;
        Pragma = impl_pragma_foreign_proc(FPInfo),
        mercury_output_pragma_foreign_proc(Stream, Lang, FPInfo, !IO)
    ;
        Pragma = impl_pragma_foreign_proc_export(FPEInfo),
        mercury_format_pragma_foreign_proc_export(Lang, FPEInfo, Stream, !IO)
    ;
        Pragma = impl_pragma_external_proc(ExternalInfo),
        mercury_format_pragma_external_proc(ExternalInfo, Stream, !IO)
    ;
        Pragma = impl_pragma_fact_table(FactTableInfo),
        mercury_format_pragma_fact_table(FactTableInfo, Stream, !IO)
    ;
        Pragma = impl_pragma_inline(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "inline", no, !IO)
    ;
        Pragma = impl_pragma_no_inline(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "no_inline", no, !IO)
    ;
        Pragma = impl_pragma_tabled(TabledInfo),
        mercury_output_pragma_tabled(Stream, TabledInfo, !IO)
    ;
        Pragma = impl_pragma_consider_used(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "consider_used", no, !IO)
    ;
        Pragma = impl_pragma_no_detism_warning(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "no_determinism_warning", no, !IO)
    ;
        Pragma = impl_pragma_mode_check_clauses(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "mode_check_clauses", no, !IO)
    ;
        Pragma = impl_pragma_require_tail_rec(RequireTailrecPragma),
        mercury_output_pragma_require_tail_rec(Stream, Lang,
            RequireTailrecPragma, !IO)
    ;
        Pragma = impl_pragma_promise_pure(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "promise_pure", no, !IO)
    ;
        Pragma = impl_pragma_promise_semipure(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "promise_semipure", no, !IO)
    ;
        Pragma = impl_pragma_promise_eqv_clauses(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "promise_equivalent_clauses", no, !IO)
    ;
        Pragma = impl_pragma_require_feature_set(RFSInfo),
        mercury_format_pragma_require_feature_set(RFSInfo, Stream, !IO)
    ).

mercury_output_item_generated_pragma(Info, Stream, ItemGenPragma, !IO) :-
    ItemGenPragma = item_pragma_info(Pragma, Context, _SeqNum),
    maybe_output_line_number(Info, Context, Stream, !IO),
    (
        Pragma = gen_pragma_unused_args(UnusedArgsInfo),
        mercury_output_pragma_unused_args(Stream, UnusedArgsInfo, !IO)
    ;
        Pragma = gen_pragma_exceptions(ExceptionsInfo),
        mercury_output_pragma_exceptions(Stream, ExceptionsInfo, !IO)
    ;
        Pragma = gen_pragma_trailing_info(TrailingInfo),
        mercury_output_pragma_trailing_info(Stream, TrailingInfo, !IO)
    ;
        Pragma = gen_pragma_mm_tabling_info(TablingInfo),
        mercury_output_pragma_mm_tabling_info(Stream, TablingInfo, !IO)
    ).

%---------------------------------------------------------------------------%

mercury_output_item_pred_marker(Stream, PredMarker, !IO) :-
    PredMarker = pragma_info_pred_marker(PredNameArity, PredMarkerKind),
    PredNameArity = pred_name_arity(Pred, Arity),
    (
        PredMarkerKind = pmpk_inline,
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "inline", no, !IO)
    ;
        PredMarkerKind = pmpk_noinline,
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "no_inline", no, !IO)
    ;
        PredMarkerKind = pmpk_promise_eqv_clauses,
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "promise_equivalent_clauses", no, !IO)
    ;
        PredMarkerKind = pmpk_promise_pure,
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "promise_pure", no, !IO)
    ;
        PredMarkerKind = pmpk_promise_semipure,
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "promise_semipure", no, !IO)
    ;
        PredMarkerKind = pmpk_terminates,
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "terminates", no, !IO)
    ;
        PredMarkerKind = pmpk_does_not_terminate,
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "does_not_terminate", no, !IO)
    ;
        PredMarkerKind = pmpk_mode_check_clauses,
        mercury_output_pragma_decl(Stream, Pred, Arity, pf_predicate,
            "mode_check_clauses", no, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output a generic pragma declaration. Used to implement
% several kinds of pragmas.
%

mercury_output_pragma_decl(Stream, PredName, Arity, PredOrFunc, PragmaName,
        MaybeAfter, !IO) :-
    mercury_format_pragma_decl(PredName, Arity, PredOrFunc, PragmaName,
        MaybeAfter, Stream, !IO).

mercury_pragma_decl_to_string(PredName, Arity, PredOrFunc, PragmaName,
        MaybeAfter) = String :-
    mercury_format_pragma_decl(PredName, Arity, PredOrFunc, PragmaName,
        MaybeAfter, unit, "", String).

:- pred mercury_format_pragma_decl(sym_name::in, int::in, pred_or_func::in,
    string::in, maybe(string)::in, S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_pragma_decl(PredName, Arity, PredOrFunc, PragmaName, MaybeAfter,
        S, !U) :-
    (
        PredOrFunc = pf_predicate,
        DeclaredArity = Arity
    ;
        PredOrFunc = pf_function,
        DeclaredArity = Arity - 1
    ),
    add_string(":- pragma ", S, !U),
    add_string(PragmaName, S, !U),
    add_string("(", S, !U),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, PredName,
        S, !U),
    add_string("/", S, !U),
    add_int(DeclaredArity, S, !U),
    (
        MaybeAfter = yes(After),
        add_string(", ", S, !U),
        add_string(After, S, !U)
    ;
        MaybeAfter = no
    ),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%
%
% Output a foreign_decl pragma.
%

mercury_output_pragma_foreign_decl(Stream, FDInfo, !IO) :-
    mercury_format_pragma_foreign_decl(FDInfo, Stream, !IO).

mercury_pragma_foreign_decl_to_string(FDInfo) = String :-
    mercury_format_pragma_foreign_decl(FDInfo, unit, "", String).

:- pred mercury_format_pragma_foreign_decl(pragma_info_foreign_decl::in, S::in,
    U::di, U::uo) is det <= output(S, U).

mercury_format_pragma_foreign_decl(FDInfo, S, !U) :-
    FDInfo = pragma_info_foreign_decl(Lang, IsLocal, LiteralOrInclude),
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

:- pred mercury_output_pragma_foreign_code(io.text_output_stream::in,
    pragma_info_foreign_code::in, io::di, io::uo) is det.

mercury_output_pragma_foreign_code(Stream, FCInfo, !IO) :-
    FCInfo = pragma_info_foreign_code(Lang, LiteralOrInclude),
    io.write_string(Stream, ":- pragma foreign_code(", !IO),
    mercury_format_foreign_language_string(Lang, Stream, !IO),
    io.write_string(Stream, ", ", !IO),
    mercury_format_foreign_literal_or_include(LiteralOrInclude, Stream, !IO),
    io.write_string(Stream, ").\n", !IO).

:- pred mercury_format_foreign_literal_or_include(
    foreign_literal_or_include::in, S::in,
    U::di, U::uo) is det <= output(S, U).

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
    U::di, U::uo) is det <= output(S, U).

mercury_format_foreign_code_string(Str, S, !U) :-
    add_string("""", S, !U),
    mercury_format_escaped_string(Str, S, !U),
    add_string("""", S, !U).

:- pred mercury_format_escaped_string(string::in, S::in,
    U::di, U::uo) is det <= output(S, U).

mercury_format_escaped_string(String, S, !U) :-
    string.foldl(mercury_format_escaped_char(S), String, !U).

:- pred mercury_format_escaped_char(S::in, char::in,
    U::di, U::uo) is det <= output(S, U).

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

escape_special_char('''', '''').
escape_special_char('"', '"').
escape_special_char('\\', '\\').
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

mercury_output_pragma_foreign_proc(Stream, Lang, FPInfo, !IO) :-
    mercury_format_pragma_foreign_proc(Lang, FPInfo, Stream, !IO).

mercury_pragma_foreign_proc_to_string(Lang, FPInfo) = String :-
    mercury_format_pragma_foreign_proc(Lang, FPInfo, unit, "", String).

:- pred mercury_format_pragma_foreign_proc(output_lang::in,
    pragma_info_foreign_proc::in, S::in,
    U::di, U::uo) is det <= output(S, U).

mercury_format_pragma_foreign_proc(Lang, FPInfo, S, !U) :-
    FPInfo = pragma_info_foreign_proc(Attributes, PredName, PredOrFunc, Vars0,
        ProgVarSet, InstVarSet, PragmaCode),
    add_string(":- pragma foreign_proc(", S, !U),
    ForeignLang = get_foreign_language(Attributes),
    mercury_format_foreign_language_string(ForeignLang, S, !U),
    add_string(", ", S, !U),
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
    add_string(", ", S, !U),
    mercury_format_pragma_foreign_attributes(ProgVarSet, Attributes, S, !U),
    add_string(", ", S, !U),
    PragmaCode = fp_impl_ordinary(C_Code, _),
    mercury_format_foreign_code_string(C_Code, S, !U),
    add_string(").\n", S, !U).

%---------------------%

:- pred mercury_format_pragma_foreign_proc_vars(output_lang::in,
    prog_varset::in, inst_varset::in, list(pragma_var)::in, S::in,
    U::di, U::uo) is det <= output(S, U).

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
    pragma_foreign_proc_attributes::in, S::in,
    U::di, U::uo) is det <= output(S, U).

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
:- func foreign_proc_attributes_to_strings(pragma_foreign_proc_attributes,
    prog_varset) = list(string).

foreign_proc_attributes_to_strings(Attrs, VarSet) = StringList :-
    MayCallMercury = get_may_call_mercury(Attrs),
    ThreadSafe = get_thread_safe(Attrs),
    TabledForIO = get_tabled_for_io(Attrs),
    Purity = get_purity(Attrs),
    Terminates = get_terminates(Attrs),
    UserSharing = get_user_annotated_sharing(Attrs),
    Exceptions = get_may_throw_exception(Attrs),
    OrdinaryDespiteDetism = get_ordinary_despite_detism(Attrs),
    MayModifyTrail = get_may_modify_trail(Attrs),
    MayCallMM_Tabled = get_may_call_mm_tabled(Attrs),
    BoxPolicy = get_box_policy(Attrs),
    AffectsLiveness = get_affects_liveness(Attrs),
    AllocatesMemory = get_allocates_memory(Attrs),
    RegistersRoots = get_registers_roots(Attrs),
    MaybeMayDuplicate = get_may_duplicate(Attrs),
    ExtraAttributes = get_extra_attributes(Attrs),
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
        Exceptions = proc_will_not_throw_exception,
        ExceptionsStrList = ["will_not_throw_exception"]
    ;
        Exceptions = default_exception_behaviour,
        ExceptionsStrList = []
    ),
    (
        OrdinaryDespiteDetism = yes,
        OrdinaryDespiteDetismStrList = ["ordinary_despite_detism"]
    ;
        OrdinaryDespiteDetism = no,
        OrdinaryDespiteDetismStrList = []
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
    StringList = [MayCallMercuryStr, ThreadSafeStr, TabledForIOStr |
        PurityStrList] ++ TerminatesStrList ++ UserSharingStrList ++
        ExceptionsStrList ++
        OrdinaryDespiteDetismStrList ++ MayModifyTrailStrList ++
        MayCallMM_TabledStrList ++ BoxPolicyStrList ++
        AffectsLivenessStrList ++ AllocatesMemoryStrList ++
        RegistersRootsStrList ++ MayDuplicateStrList ++
        list.map(extra_attribute_to_string, ExtraAttributes).

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
    VarStrA = mercury_var_to_string(VarSet, print_name_only, VarA),
    VarStrB = mercury_var_to_string(VarSet, print_name_only, VarB),
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

:- func extra_attribute_to_string(pragma_foreign_proc_extra_attribute)
    = string.

extra_attribute_to_string(refers_to_llds_stack) = "refers_to_llds_stack".
extra_attribute_to_string(backend(low_level_backend)) = "low_level_backend".
extra_attribute_to_string(backend(high_level_backend)) = "high_level_backend".
extra_attribute_to_string(needs_call_standard_output_registers) =
    "needs_call_standard_output_registers".

%---------------------------------------------------------------------------%
%
% Output a foreign_proc_export pragma.
%

:- pred mercury_format_pragma_foreign_proc_export(output_lang::in,
    pragma_info_foreign_proc_export::in, S::in,
    U::di, U::uo) is det <= output(S, U).

mercury_format_pragma_foreign_proc_export(Lang, FPEInfo, S, !U) :-
    FPEInfo = pragma_info_foreign_proc_export(_Origin, ForeignLang,
        PredNameModesPF, ExportName),
    PredNameModesPF = pred_name_modes_pf(Name, ModeList, PredOrFunc),
    varset.init(InstVarSet), % The varset isn't really used.
    add_string(":- pragma foreign_export(", S, !U),
    mercury_format_foreign_language_string(ForeignLang, S, !U),
    add_string(", ", S, !U),
    mercury_format_sym_name(Name, S, !U),
    (
        PredOrFunc = pf_function,
        pred_args_to_func_args(ModeList, ArgModes, RetMode),
        add_string("(", S, !U),
        mercury_format_mode_list(Lang, InstVarSet, ArgModes, S, !U),
        add_string(") = ", S, !U),
        mercury_format_mode(Lang, InstVarSet, RetMode, S, !U)
    ;
        PredOrFunc = pf_predicate,
        add_string("(", S, !U),
        mercury_format_mode_list(Lang, InstVarSet, ModeList, S, !U),
        add_string(")", S, !U)
    ),
    add_string(", ", S, !U),
    add_string(ExportName, S, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%
%
% Output an external_proc pragma.
%

:- pred mercury_format_pragma_external_proc(pragma_info_external_proc::in,
    S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_pragma_external_proc(ExternalInfo, S, !U) :-
    ExternalInfo = pragma_info_external_proc(PredName, Arity, PorF,
        MaybeBackend),
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

mercury_output_pragma_type_spec(Stream, VarNamePrint, Lang, TypeSpecInfo,
        !IO) :-
    TypeSpecInfo = pragma_info_type_spec(PredName, SpecName, Arity,
        MaybePredOrFunc, MaybeModes, Subst, VarSet, _),
    io.write_string(Stream, ":- pragma type_spec(", !IO),
    (
        MaybeModes = yes(Modes),
        (
            MaybePredOrFunc = yes(PredOrFunc0),
            PredOrFunc = PredOrFunc0
        ;
            MaybePredOrFunc = no,
            unexpected($pred, "no pred_or_func")
        ),
        PredNameModesPF = pred_name_modes_pf(PredName, Modes, PredOrFunc),
        write_pred_name_modes_pf(Stream, Lang, PredNameModesPF, !IO)
    ;
        MaybeModes = no,
        mercury_output_bracketed_sym_name_ngt(next_to_graphic_token, PredName,
            Stream, !IO),
        io.write_string(Stream, "/", !IO),
        io.write_int(Stream, Arity, !IO)
    ),
    io.write_string(Stream, ", (", !IO),
    write_out_list(mercury_output_type_subst(VarSet, VarNamePrint),
        ", ", Subst, Stream, !IO),
    io.write_string(Stream, "), ", !IO),
    mercury_output_bracketed_sym_name_ngt(not_next_to_graphic_token, SpecName,
        Stream, !IO),
    io.write_string(Stream, ").\n", !IO).

:- pred mercury_output_type_subst(tvarset::in, var_name_print::in,
    pair(tvar, mer_type)::in, io.text_output_stream::in,
    io::di, io::uo) is det.

mercury_output_type_subst(VarSet, VarNamePrint, Var - Type, Stream, !IO) :-
    mercury_output_var(VarSet, VarNamePrint, Var, Stream, !IO),
    io.write_string(Stream, " = ", !IO),
    mercury_output_type(VarSet, VarNamePrint, Type, Stream, !IO).

%---------------------------------------------------------------------------%
%
% Output an unused_args pragma.
%

mercury_output_pragma_unused_args(Stream, UnusedArgsInfo, !IO) :-
    UnusedArgsInfo = pragma_info_unused_args(PredNameArityPFMn, UnusedArgs),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    io.write_string(Stream, ":- pragma unused_args(", !IO),
    io.write_string(Stream, pred_or_func_to_full_str(PredOrFunc), !IO),
    io.write_string(Stream, ", ", !IO),
    mercury_output_bracketed_sym_name(SymName, Stream, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_int(Stream, Arity, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_int(Stream, ModeNum, !IO),
    io.write_string(Stream, ", [", !IO),
    mercury_format_int_list(UnusedArgs, Stream, !IO),
    io.write_string(Stream, "]).\n", !IO).

:- pred mercury_format_int_list(list(int)::in, S::in,
    U::di, U::uo) is det <= output(S, U).

mercury_format_int_list([], _S, !U).
mercury_format_int_list([Head | Tail], S, !U) :-
    add_int(Head, S, !U),
    mercury_format_int_list_2(Tail, S, !U).

:- pred mercury_format_int_list_2(list(int)::in, S::in,
    U::di, U::uo) is det <= output(S, U).

mercury_format_int_list_2([], _S, !U).
mercury_format_int_list_2([Head | Tail], S, !U) :-
    add_string(", ", S, !U),
    add_int(Head, S, !U),
    mercury_format_int_list_2(Tail, S, !U).

%---------------------------------------------------------------------------%
%
% Output an exceptions pragma.
%

mercury_output_pragma_exceptions(Stream, ExceptionsInfo, !IO) :-
    ExceptionsInfo = pragma_info_exceptions(PredNameArityPFMn, ThrowStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    io.write_string(Stream, ":- pragma exceptions(", !IO),
    io.write_string(Stream, pred_or_func_to_full_str(PredOrFunc), !IO),
    io.write_string(Stream, ", ", !IO),
    mercury_output_bracketed_sym_name(SymName, Stream, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_int(Stream, Arity, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_int(Stream, ModeNum, !IO),
    io.write_string(Stream, ", ", !IO),
    (
        ThrowStatus = will_not_throw,
        io.write_string(Stream, "will_not_throw", !IO)
    ;
        ThrowStatus = may_throw(ExceptionType),
        io.write_string(Stream, "may_throw(", !IO),
        (
            ExceptionType = user_exception,
            io.write_string(Stream, "user_exception)", !IO)
        ;
            ExceptionType = type_exception,
            io.write_string(Stream, "type_exception)", !IO)
        )
    ;
        ThrowStatus = throw_conditional,
        io.write_string(Stream, "conditional", !IO)
    ),
    io.write_string(Stream, ").\n", !IO).

%---------------------------------------------------------------------------%
%
% Output a trailing_info pragma.
%

mercury_output_pragma_trailing_info(Stream, TrailingInfo, !IO) :-
    TrailingInfo =
        pragma_info_trailing_info(PredNameArityPFMn, TrailingStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    io.write_string(Stream, ":- pragma trailing_info(", !IO),
    io.write_string(Stream, pred_or_func_to_full_str(PredOrFunc), !IO),
    io.write_string(Stream, ", ", !IO),
    mercury_output_bracketed_sym_name(SymName, Stream, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_int(Stream, Arity, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_int(Stream, ModeNum, !IO),
    io.write_string(Stream, ", ", !IO),
    (
        TrailingStatus = trail_may_modify,
        io.write_string(Stream, "may_modify_trail", !IO)
    ;
        TrailingStatus = trail_will_not_modify,
        io.write_string(Stream, "will_not_modify_trail", !IO)
    ;
        TrailingStatus = trail_conditional,
        io.write_string(Stream, "conditional", !IO)
    ),
    io.write_string(Stream, ").\n", !IO).

%---------------------------------------------------------------------------%
%
% Output a mm_tabling_info pragma.
%

mercury_output_pragma_mm_tabling_info(Stream, TablingInfo, !IO) :-
    TablingInfo = pragma_info_mm_tabling_info(PredNameArityPFMn,
        MM_TablingStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    io.write_string(Stream, ":- pragma mm_tabling_info(", !IO),
    io.write_string(Stream, pred_or_func_to_full_str(PredOrFunc), !IO),
    io.write_string(Stream, ", ", !IO),
    mercury_output_bracketed_sym_name(SymName, Stream, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_int(Stream, Arity, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_int(Stream, ModeNum, !IO),
    io.write_string(Stream, ", ", !IO),
    (
        MM_TablingStatus = mm_tabled_may_call,
        io.write_string(Stream, "mm_tabled_may_call", !IO)
    ;
        MM_TablingStatus = mm_tabled_will_not_call,
        io.write_string(Stream, "mm_tabled_will_not_call", !IO)
    ;
        MM_TablingStatus = mm_tabled_conditional,
        io.write_string(Stream, "mm_tabled_conditional", !IO)
    ),
    io.write_string(Stream, ").\n", !IO).

%---------------------------------------------------------------------------%
%
% Output an obsolete_pred or obsolete_proc pragma.
%

:- pred mercury_output_pragma_obsolete_pred(io.text_output_stream::in,
    pragma_info_obsolete_pred::in, io::di, io::uo) is det.

mercury_output_pragma_obsolete_pred(Stream, ObsoletePredInfo, !IO) :-
    ObsoletePredInfo =
        pragma_info_obsolete_pred(PredNameArity, ObsoleteInFavourOf),
    PredNameArity = pred_name_arity(SymName, Arity),
    PredSymNameArity = sym_name_arity(SymName, Arity),
    ObsoleteStrs = list.map(wrapped_sym_name_arity_to_string,
        ObsoleteInFavourOf),
    ObsoleteStr = string.join_list(", ", ObsoleteStrs),
    io.format(Stream, ":- pragma obsolete(%s, [%s]).\n",
        [s(wrapped_sym_name_arity_to_string(PredSymNameArity)),
        s(ObsoleteStr)], !IO).

:- pred mercury_output_pragma_obsolete_proc(io.text_output_stream::in,
    output_lang::in, pragma_info_obsolete_proc::in, io::di, io::uo) is det.

mercury_output_pragma_obsolete_proc(Stream, Lang, ObsoleteProcInfo, !IO) :-
    ObsoleteProcInfo =
        pragma_info_obsolete_proc(PredNameModesPF, ObsoleteInFavourOf),
    io.write_string(Stream, ":- pragma obsolete_proc(", !IO),
    write_pred_name_modes_pf(Stream, Lang, PredNameModesPF, !IO),
    ObsoleteStrs = list.map(wrapped_sym_name_arity_to_string,
        ObsoleteInFavourOf),
    ObsoleteStr = string.join_list(", ", ObsoleteStrs),
    io.format(Stream, ", [%s]).\n", [s(ObsoleteStr)], !IO).

:- func wrapped_sym_name_arity_to_string(sym_name_arity) = string.

wrapped_sym_name_arity_to_string(SNA) = Str :-
    SNA = sym_name_arity(SymName, Arity),
    Str = mercury_bracketed_sym_name_to_string(SymName) ++
        "/" ++ string.int_to_string(Arity).

%---------------------------------------------------------------------------%
%
% Output a require tail recursion pragma
%

:- pred mercury_output_pragma_require_tail_rec(io.text_output_stream::in,
    output_lang::in, pragma_info_require_tail_rec::in, io::di, io::uo) is det.

mercury_output_pragma_require_tail_rec(Stream, Lang, RequireTR, !IO) :-
    RequireTR = pragma_info_require_tail_rec(Proc, Info),
    ProcSpecStr = format_pred_name_arity_mpf_mmode(Lang, Proc),
    (
        Info = suppress_tailrec_warnings(_),
        io.format(Stream, ":- pragma warn_tail_recursion(%s, [none]).\n",
            [s(ProcSpecStr)], !IO)
    ;
        Info = enable_tailrec_warnings(WarnOrError, Type, _),
        warning_or_error_string(WarnOrError, WarnOrErrorStr),
        require_tailrec_type_string(Type, TypeStr),
        io.format(Stream, ":- pragma warn_tail_recursion(%s, [%s, %s]).\n",
            [s(ProcSpecStr), s(WarnOrErrorStr), s(TypeStr)], !IO)
    ).

:- func format_pred_name_arity_mpf_mmode(output_lang,
    pred_name_arity_mpf_mmode) = string.

format_pred_name_arity_mpf_mmode(Lang, Proc) = ProcSpecStr :-
    Proc = pred_name_arity_mpf_mmode(Pred, Arity, MaybePredOrFunc, MaybeMode),
    (
        MaybePredOrFunc = yes(PredOrFunc)
    ;
        MaybePredOrFunc = no,
        PredOrFunc = pf_predicate
    ),
    (
        MaybeMode = no,
        (
            PredOrFunc = pf_predicate,
            DeclaredArity = Arity
        ;
            PredOrFunc = pf_function,
            DeclaredArity = Arity - 1
        ),
        ProcSpecStr = format("%s/%d",
            [s(mercury_bracketed_sym_name_to_string(Pred)), i(DeclaredArity)])
    ;
        MaybeMode = yes(ModeList),
        varset.init(InitVarSet),
        (
            PredOrFunc = pf_predicate,
            ProcSpecStr = mercury_pred_mode_subdecl_to_string(Lang, InitVarSet,
                Pred, ModeList, no)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(ModeList, FuncModeList, RetMode),
            ProcSpecStr = mercury_func_mode_subdecl_to_string(Lang,
                InitVarSet, Pred, FuncModeList, RetMode, no)
        )
    ).

%---------------------------------------------------------------------------%
%
% Output a tabled pragma.
%

:- pred mercury_output_pragma_tabled(io.text_output_stream::in,
    pragma_info_tabled::in, io::di, io::uo) is det.

mercury_output_pragma_tabled(Stream, TabledInfo, !IO) :-
    TabledInfo = pragma_info_tabled(EvalMethod, PredNameArityMPF, _Mode,
        MaybeAttributes),
    PredNameArityMPF = pred_name_arity_mpf(PredName, Arity, _MaybePorF),
    PragmaName = eval_method_to_pragma_name(EvalMethod),
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
                MaybeAfter = no
            ;
                !.Strs = [_ | _],
                MaybeAfter =
                    yes("[" ++ string.join_list(", ", !.Strs) ++ "]")
            )
        )
    ;
        MaybeAttributes = no,
        MaybeAfter = no
    ),
    mercury_output_pragma_decl(Stream, PredName, Arity, pf_predicate,
        PragmaName, MaybeAfter, !IO).

%---------------------------------------------------------------------------%
%
% Output a fact_table pragma.
%

:- pred mercury_format_pragma_fact_table(pragma_info_fact_table::in, S::in,
    U::di, U::uo) is det <= output(S, U).

mercury_format_pragma_fact_table(FactTableInfo, S, !U) :-
    FactTableInfo = pragma_info_fact_table(PredNameArity, FileName),
    PredNameArity = pred_name_arity(PredName, Arity),
    add_string(":- pragma fact_table(", S, !U),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, PredName,
        S, !U),
    add_string("/", S, !U),
    add_int(Arity, S, !U),
    add_string(", ", S, !U),
    add_quoted_string(FileName, S, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%
%
% Output an oisu (order independent state update) pragma.
%

:- pred mercury_output_pragma_oisu(io.text_output_stream::in,
    pragma_info_oisu::in, io::di, io::uo) is det.

mercury_output_pragma_oisu(Stream, OISUInfo, !IO) :-
    mercury_format_pragma_oisu(OISUInfo, Stream, !IO).

:- pred mercury_format_pragma_oisu(pragma_info_oisu::in, S::in,
    U::di, U::uo) is det <= output(S, U).

mercury_format_pragma_oisu(OISUInfo, S, !U) :-
    OISUInfo = pragma_info_oisu(TypeCtor, CreatorPreds, MutatorPreds,
        DestructorPreds),
    add_string(":- pragma oisu(", S, !U),
    TypeCtor = type_ctor(TypeName, TypeArity),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, TypeName,
        S, !U),
    add_string("/", S, !U),
    add_int(TypeArity, S, !U),
    add_string(",\n", S, !U),
    add_string("\tcreators([\n", S, !U),
    mercury_format_pred_name_arity_list(CreatorPreds, S, !U),
    add_string("\t]),\n", S, !U),
    add_string("\tmutators([\n", S, !U),
    mercury_format_pred_name_arity_list(MutatorPreds, S, !U),
    add_string("\t]),\n", S, !U),
    add_string("\tdestructors([\n", S, !U),
    mercury_format_pred_name_arity_list(DestructorPreds, S, !U),
    add_string("\t])\n", S, !U),
    add_string(").\n", S, !U).

:- pred mercury_format_pred_name_arity_list(list(pred_name_arity)::in, S::in,
    U::di, U::uo) is det <= output(S, U).

mercury_format_pred_name_arity_list([], _S, !U).
mercury_format_pred_name_arity_list([PredNameArity | PredNameArities],
        S, !U) :-
    mercury_format_pred_name_arity_list_lag(PredNameArity, PredNameArities,
        S, !U).

:- pred mercury_format_pred_name_arity_list_lag(pred_name_arity::in,
    list(pred_name_arity)::in, S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_pred_name_arity_list_lag(PredNameArity, PredNameArities,
        S, !U) :-
    add_string("\t\t", S, !U),
    mercury_format_pred_name_arity(PredNameArity, S, !U),
    (
        PredNameArities = [],
        add_string("\n", S, !U)
    ;
        PredNameArities = [HeadPredNameArity | TailPredNameArities],
        add_string(",\n", S, !U),
        mercury_format_pred_name_arity_list_lag(HeadPredNameArity,
            TailPredNameArities, S, !U)
    ).

:- pred mercury_format_pred_name_arity(pred_name_arity::in, S::in,
    U::di, U::uo) is det <= output(S, U).

mercury_format_pred_name_arity(PredNameArity, S, !U) :-
    PredNameArity = pred_name_arity(PredName, Arity),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, PredName,
        S, !U),
    add_string("/", S, !U),
    add_int(Arity, S, !U).

%---------------------------------------------------------------------------%
%
% Output a termination_info pragma.
%

write_pragma_termination_info(Stream, Lang, TermInfo, !IO) :-
    TermInfo = pragma_info_termination_info(PredNameModesPF,
        MaybeArgSize, MaybeTermination),
    io.write_string(Stream, ":- pragma termination_info(", !IO),
    write_pred_name_modes_pf(Stream, Lang, PredNameModesPF, !IO),
    io.write_string(Stream, ", ", !IO),
    write_maybe_arg_size_info(Stream, no, MaybeArgSize, !IO),
    io.write_string(Stream, ", ", !IO),
    write_maybe_termination_info(Stream, no, MaybeTermination, !IO),
    io.write_string(Stream, ").\n", !IO).

write_maybe_arg_size_info(Stream, Verbose, MaybeArgSizeInfo, !IO) :-
    (
        MaybeArgSizeInfo = no,
        io.write_string(Stream, "not_set", !IO)
    ;
        MaybeArgSizeInfo = yes(infinite(Error)),
        io.write_string(Stream, "infinite", !IO),
        (
            Verbose = yes,
            io.write_string(Stream, "(", !IO),
            io.write(Stream, Error, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            Verbose = no
        )
    ;
        MaybeArgSizeInfo = yes(finite(Const, UsedArgs)),
        io.write_string(Stream, "finite(", !IO),
        io.write_int(Stream, Const, !IO),
        io.write_string(Stream, ", ", !IO),
        write_used_args(Stream, UsedArgs, !IO),
        io.write_string(Stream, ")", !IO)
    ).

:- pred write_used_args(io.text_output_stream::in, list(bool)::in,
    io::di, io::uo) is det.

write_used_args(Stream, [], !IO) :-
    io.write_string(Stream, "[]", !IO).
write_used_args(Stream, [UsedArg | UsedArgs], !IO) :-
    io.write_string(Stream, "[", !IO),
    write_bool(Stream, UsedArg, !IO),
    write_used_comma_args(Stream, UsedArgs, !IO),
    io.write_string(Stream, "]", !IO).

:- pred write_used_comma_args(io.text_output_stream::in, list(bool)::in,
    io::di, io::uo) is det.

write_used_comma_args(_Stream, [], !IO).
write_used_comma_args(Stream, [UsedArg | UsedArgs], !IO) :-
    io.write_string(Stream, ", ", !IO),
    write_bool(Stream, UsedArg, !IO),
    write_used_comma_args(Stream, UsedArgs, !IO).

:- pred write_bool(io.text_output_stream::in, bool::in, io::di, io::uo) is det.

write_bool(Stream, Bool, !IO) :-
    (
        Bool = no,
        io.write_string(Stream, "no", !IO)
    ;
        Bool = yes,
        io.write_string(Stream, "yes", !IO)
    ).

write_maybe_termination_info(Stream, Verbose, MaybeTerminationInfo, !IO) :-
    (
        MaybeTerminationInfo = no,
        io.write_string(Stream, "not_set", !IO)
    ;
        MaybeTerminationInfo = yes(cannot_loop(_)),
        io.write_string(Stream, "cannot_loop", !IO)
    ;
        MaybeTerminationInfo = yes(can_loop(Error)),
        io.write_string(Stream, "can_loop", !IO),
        (
            Verbose = yes,
            io.write_string(Stream, "(", !IO),
            io.write(Stream, Error, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            Verbose = no
        )
    ).

%---------------------------------------------------------------------------%
%
% Output a termination2_info pragma.
%

write_pragma_termination2_info(Stream, Lang, Term2Info, !IO) :-
    Term2Info = pragma_info_termination2_info(PredNameModesPF,
        MaybeSuccess, MaybeFailure, MaybeTermination),
    io.write_string(Stream, ":- pragma termination2_info(", !IO),
    write_pred_name_modes_pf(Stream, Lang, PredNameModesPF, !IO),
    io.write_string(Stream, ", ", !IO),
    write_maybe_pragma_constr_arg_size_info(Stream, MaybeSuccess, !IO),
    io.write_string(Stream, ", ", !IO),
    write_maybe_pragma_constr_arg_size_info(Stream, MaybeFailure, !IO),
    io.write_string(Stream, ", ", !IO),
    write_maybe_pragma_termination_info(Stream, MaybeTermination, !IO),
    io.write_string(Stream, ").\n", !IO).

:- pred write_maybe_pragma_constr_arg_size_info(io.text_output_stream::in,
    maybe(pragma_constr_arg_size_info)::in, io::di, io::uo) is det.

write_maybe_pragma_constr_arg_size_info(Stream, no, !IO) :-
    io.write_string(Stream, "not_set", !IO).
write_maybe_pragma_constr_arg_size_info(Stream, yes(ArgSizeInfo), !IO) :-
    io.write_string(Stream, "constraints(", !IO),
    io.write_char(Stream, '[', !IO),
    write_out_list(write_arg_size_constr, ", ", ArgSizeInfo, Stream, !IO),
    io.write_string(Stream, "])", !IO).

:- pred write_arg_size_constr(arg_size_constr::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_arg_size_constr(Constraint, Stream, !IO) :-
    (
        Constraint = le(Terms, Constant),
        OpStr = "le("
    ;
        Constraint = eq(Terms, Constant),
        OpStr = "eq("
    ),
    io.write_string(Stream, OpStr, !IO),
    io.write_char(Stream, '[', !IO),
    write_out_list(write_arg_size_term, ", ", Terms, Stream, !IO),
    io.write_string(Stream, "], ", !IO),
    rat.write_rat(Stream, Constant, !IO),
    io.write_char(Stream, ')', !IO).

:- pred write_arg_size_term(arg_size_term::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_arg_size_term(ArgSizeTerm, Stream, !IO) :-
    ArgSizeTerm = arg_size_term(VarId, Coefficient),
    io.write_string(Stream, "term(", !IO),
    io.write_int(Stream, VarId, !IO),
    io.write_string(Stream, ", ", !IO),
    rat.write_rat(Stream, Coefficient, !IO),
    io.write_char(Stream, ')', !IO).

:- pred write_maybe_pragma_termination_info(io.text_output_stream::in,
    maybe(pragma_termination_info)::in, io::di, io::uo) is det.

write_maybe_pragma_termination_info(Stream, MaybeTermination, !IO) :-
    (
        MaybeTermination = no,
        io.write_string(Stream, "not_set", !IO)
    ;
        MaybeTermination = yes(Termination),
        (
            Termination = can_loop(_),
            TerminationStr = "can_loop"
        ;
            Termination = cannot_loop(_),
            TerminationStr = "cannot_loop"
        ),
        io.write_string(Stream, TerminationStr, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output a structure_sharing pragma.
%

write_pragma_structure_sharing_info(Stream, Lang, SharingInfo, !IO) :-
    SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
        HeadVars, HeadVarTypes, VarSet, TVarSet, MaybeSharingAs),
    io.write_string(Stream, ":- pragma structure_sharing(", !IO),
    write_pred_name_modes_pf(Stream, Lang, PredNameModesPF, !IO),
    % write headvars and types:
    io.write_string(Stream, ", ", !IO),
    write_vars_and_types(Stream, VarSet, TVarSet, HeadVars, HeadVarTypes, !IO),
    % write structure sharing information.
    io.write_string(Stream, ", ", !IO),
    prog_ctgc.print_interface_structure_sharing_domain(Stream,
        VarSet, TVarSet, MaybeSharingAs, !IO),
    io.write_string(Stream, ").\n", !IO).

%---------------------------------------------------------------------------%
%
% Output a structure_reuse pragma.
%

write_pragma_structure_reuse_info(Stream, Lang, ReuseInfo, !IO) :-
    ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
        HeadVars, HeadVarTypes, VarSet, TVarSet, MaybeStructureReuseDomain),
    io.write_string(Stream, ":- pragma structure_reuse(", !IO),
    write_pred_name_modes_pf(Stream, Lang, PredNameModesPF, !IO),
    % write headvars and types:
    io.write_string(Stream, ", ", !IO),
    write_vars_and_types(Stream, VarSet, TVarSet, HeadVars, HeadVarTypes, !IO),
    % write structure reuse information.
    io.write_string(Stream, ", ", !IO),
    prog_ctgc.print_interface_maybe_structure_reuse_domain(Stream,
        VarSet, TVarSet, MaybeStructureReuseDomain, !IO),
    io.write_string(Stream, ").\n", !IO).

%---------------------------------------------------------------------------%
%
% Predicates used to help print both structure_sharing and structure_reuse
% pragmas.
%

:- pred write_vars_and_types(io.text_output_stream::in,
    prog_varset::in, tvarset::in, prog_vars::in, list(mer_type)::in,
    io::di, io::uo) is det.

write_vars_and_types(Stream, VarSet, TypeVarSet, HeadVars, HeadVarTypes,
        !IO) :-
    (
        HeadVars = [],
        io.write_string(Stream, "vars, types", !IO)
    ;
        HeadVars = [_ | _],
        io.write_string(Stream, "vars(", !IO),
        mercury_output_vars(VarSet, print_name_only, HeadVars, Stream, !IO),
        io.write_string(Stream, "), ", !IO),

        io.write_string(Stream, "types(", !IO),
        write_out_list(mercury_output_type(TypeVarSet, print_name_only),
            ", ", HeadVarTypes, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output a require_feature_set pragma.
%

:- pred mercury_format_pragma_require_feature_set(
    pragma_info_require_feature_set::in, S::in,
    U::di, U::uo) is det <= output(S, U).

mercury_format_pragma_require_feature_set(RFSInfo, S, !U) :-
    RFSInfo = pragma_info_require_feature_set(Features0),
    Features = set.to_sorted_list(Features0),
    add_string(":- pragma require_feature_set(", S, !U),
    add_list(mercury_format_required_feature, ", ", Features, S, !U),
    add_string(").\n", S, !U).

:- pred mercury_format_required_feature(required_feature::in, S::in,
    U::di, U::uo) is det <= output(S, U).

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

:- pred write_pred_name_modes_pf(io.text_output_stream::in, output_lang::in,
    pred_name_modes_pf::in, io::di, io::uo) is det.

write_pred_name_modes_pf(Stream, Lang, PredNameModesPF, !IO) :-
    PredNameModesPF = pred_name_modes_pf(SymName, Modes, PredOrFunc),
    varset.init(InstVarSet),
    MaybeDet = no,
    (
        PredOrFunc = pf_predicate,
        mercury_output_pred_mode_subdecl(Stream, Lang, InstVarSet, SymName,
            Modes, MaybeDet, !IO)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        mercury_output_func_mode_subdecl(Stream, Lang, InstVarSet, SymName,
            ArgModes, RetMode, MaybeDet, !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_pragma.
%---------------------------------------------------------------------------%
