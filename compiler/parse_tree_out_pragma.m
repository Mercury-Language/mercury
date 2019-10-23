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

:- pred mercury_output_item_pragma(merc_out_info::in,
    item_pragma_info::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mercury_output_pragma_decl(sym_name::in, int::in, pred_or_func::in,
    string::in, maybe(string)::in, io::di, io::uo) is det.
:- func mercury_pragma_decl_to_string(sym_name, int, pred_or_func, string,
    maybe(string)) = string.

:- pred mercury_output_pragma_foreign_decl(pragma_info_foreign_decl::in,
    io::di, io::uo) is det.
:- func mercury_pragma_foreign_decl_to_string(pragma_info_foreign_decl)
    = string.

:- pred mercury_output_pragma_foreign_proc(output_lang::in,
    pragma_info_foreign_proc::in, io::di, io::uo) is det.
:- func mercury_pragma_foreign_proc_to_string(output_lang,
    pragma_info_foreign_proc) = string.

:- pred mercury_output_pragma_type_spec(var_name_print::in, output_lang::in,
    pragma_info_type_spec::in, io::di, io::uo) is det.

:- pred mercury_output_pragma_unused_args(pragma_info_unused_args::in,
    io::di, io::uo) is det.

:- pred mercury_output_pragma_exceptions(pragma_info_exceptions::in,
    io::di, io::uo) is det.

:- pred mercury_output_pragma_trailing_info(pragma_info_trailing_info::in,
    io::di, io::uo) is det.

:- pred mercury_output_pragma_mm_tabling_info(pragma_info_mm_tabling_info::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % This predicate outputs termination_info pragmas;
    % such annotations can be part of .opt and .trans_opt files.
    %
:- pred write_pragma_termination_info(output_lang::in,
    pragma_info_termination_info::in, io::di, io::uo) is det.

    % Write the given arg size info. Verbose if the first arg is yes.
    %
:- pred write_maybe_arg_size_info(bool::in,
    maybe(generic_arg_size_info(T))::in, io::di, io::uo) is det.

    % Write the given termination info. Verbose if the first arg is yes.
    %
:- pred write_maybe_termination_info(bool::in,
    maybe(generic_termination_info(S, T))::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred write_pragma_structure_sharing_info(output_lang::in,
    maybe(prog_varset)::in, maybe(tvarset)::in,
    pragma_info_structure_sharing::in, io::di, io::uo) is det.

:- pred write_pragma_structure_reuse_info(output_lang::in,
    maybe(prog_varset)::in, maybe(tvarset)::in,
    pragma_info_structure_reuse::in, io::di, io::uo) is det.

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
:- import_module varset.

%---------------------------------------------------------------------------%

mercury_output_item_pragma(Info, ItemPragma, !IO) :-
    ItemPragma = item_pragma_info(Pragma, _, Context, _SeqNum),
    maybe_output_line_number(Info, Context, !IO),
    Lang = get_output_lang(Info),
    (
        Pragma = pragma_foreign_decl(FDInfo),
        mercury_output_pragma_foreign_decl(FDInfo, !IO)
    ;
        Pragma = pragma_foreign_code(FCInfo),
        mercury_output_pragma_foreign_code(FCInfo, !IO)
    ;
        Pragma = pragma_foreign_proc(FPInfo),
        mercury_output_pragma_foreign_proc(Lang, FPInfo, !IO)
    ;
        Pragma = pragma_foreign_proc_export(FPEInfo),
        mercury_format_pragma_foreign_proc_export(Lang, FPEInfo, !IO)
    ;
        Pragma = pragma_external_proc(ExternalInfo),
        mercury_format_pragma_external_proc(ExternalInfo, !IO)
    ;
        Pragma = pragma_type_spec(TypeSpecInfo),
        VarNamePrint = print_name_only,
        mercury_output_pragma_type_spec(VarNamePrint, Lang, TypeSpecInfo, !IO)
    ;
        Pragma = pragma_inline(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Pred, Arity, pf_predicate,
            "inline", no, !IO)
    ;
        Pragma = pragma_no_inline(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Pred, Arity, pf_predicate,
            "no_inline", no, !IO)
    ;
        Pragma = pragma_consider_used(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Pred, Arity, pf_predicate,
            "consider_used", no, !IO)
    ;
        Pragma = pragma_unused_args(UnusedArgsInfo),
        mercury_output_pragma_unused_args(UnusedArgsInfo, !IO)
    ;
        Pragma = pragma_exceptions(ExceptionsInfo),
        mercury_output_pragma_exceptions(ExceptionsInfo, !IO)
    ;
        Pragma = pragma_trailing_info(TrailingInfo),
        mercury_output_pragma_trailing_info(TrailingInfo, !IO)
    ;
        Pragma = pragma_mm_tabling_info(TablingInfo),
        mercury_output_pragma_mm_tabling_info(TablingInfo, !IO)
    ;
        Pragma = pragma_obsolete_pred(ObsoletePredInfo),
        mercury_output_pragma_obsolete_pred(ObsoletePredInfo, !IO)
    ;
        Pragma = pragma_obsolete_proc(ObsoleteProcInfo),
        mercury_output_pragma_obsolete_proc(Lang, ObsoleteProcInfo, !IO)
    ;
        Pragma = pragma_no_detism_warning(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Pred, Arity, pf_predicate,
            "no_determinism_warning", no, !IO)
    ;
        Pragma = pragma_require_tail_recursion(RequireTailrecPragma),
        mercury_output_pragma_require_tail_recursion(Lang,
            RequireTailrecPragma, !IO)
    ;
        Pragma = pragma_tabled(TabledInfo),
        mercury_output_pragma_tabled(TabledInfo, !IO)
    ;
        Pragma = pragma_fact_table(FactTableInfo),
        mercury_format_pragma_fact_table(FactTableInfo, !IO)
    ;
        Pragma = pragma_oisu(OISUInfo),
        mercury_output_pragma_oisu(OISUInfo, !IO)
    ;
        Pragma = pragma_promise_eqv_clauses(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Pred, Arity, pf_predicate,
            "promise_equivalent_clauses", no, !IO)
    ;
        Pragma = pragma_promise_pure(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Pred, Arity, pf_predicate,
            "promise_pure", no, !IO)
    ;
        Pragma = pragma_promise_semipure(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Pred, Arity, pf_predicate,
            "promise_semipure", no, !IO)
    ;
        Pragma = pragma_termination_info(TermInfo),
        write_pragma_termination_info(Lang, TermInfo, !IO)
    ;
        Pragma = pragma_termination2_info(Term2Info),
        write_pragma_termination2_info(Lang, Term2Info, !IO)
    ;
        Pragma = pragma_terminates(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Pred, Arity, pf_predicate,
            "terminates", no, !IO)
    ;
        Pragma = pragma_does_not_terminate(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Pred, Arity, pf_predicate,
            "does_not_terminate", no, !IO)
    ;
        Pragma = pragma_check_termination(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Pred, Arity, pf_predicate,
            "check_termination", no, !IO)
    ;
        Pragma = pragma_mode_check_clauses(PredNameArity),
        PredNameArity = pred_name_arity(Pred, Arity),
        mercury_output_pragma_decl(Pred, Arity, pf_predicate,
            "mode_check_clauses", no, !IO)
    ;
        Pragma = pragma_structure_sharing(SharingInfo),
        write_pragma_structure_sharing_info(Lang, no, no, SharingInfo, !IO)
    ;
        Pragma = pragma_structure_reuse(ReuseInfo),
        write_pragma_structure_reuse_info(Lang, no, no, ReuseInfo, !IO)
    ;
        Pragma = pragma_require_feature_set(RFSInfo),
        mercury_output_pragma_require_feature_set(RFSInfo, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output a generic pragma declaration. Used to implement
% several kinds of pragmas.
%

mercury_output_pragma_decl(PredName, Arity, PredOrFunc, PragmaName, MaybeAfter,
        !IO) :-
    mercury_format_pragma_decl(PredName, Arity, PredOrFunc, PragmaName,
        MaybeAfter, !IO).

mercury_pragma_decl_to_string(PredName, Arity, PredOrFunc, PragmaName,
        MaybeAfter) = String :-
    mercury_format_pragma_decl(PredName, Arity, PredOrFunc, PragmaName,
        MaybeAfter, "", String).

:- pred mercury_format_pragma_decl(sym_name::in, int::in, pred_or_func::in,
    string::in, maybe(string)::in, U::di, U::uo) is det <= output(U).

mercury_format_pragma_decl(PredName, Arity, PredOrFunc, PragmaName, MaybeAfter,
        !U) :-
    (
        PredOrFunc = pf_predicate,
        DeclaredArity = Arity
    ;
        PredOrFunc = pf_function,
        DeclaredArity = Arity - 1
    ),
    add_string(":- pragma ", !U),
    add_string(PragmaName, !U),
    add_string("(", !U),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, PredName, !U),
    add_string("/", !U),
    add_int(DeclaredArity, !U),
    (
        MaybeAfter = yes(After),
        add_string(", ", !U),
        add_string(After, !U)
    ;
        MaybeAfter = no
    ),
    add_string(").\n", !U).

%---------------------------------------------------------------------------%
%
% Output a foreign_decl pragma.
%

mercury_output_pragma_foreign_decl(FDInfo, !IO) :-
    mercury_format_pragma_foreign_decl(FDInfo, !IO).

mercury_pragma_foreign_decl_to_string(FDInfo) = String :-
    mercury_format_pragma_foreign_decl(FDInfo, "", String).

:- pred mercury_format_pragma_foreign_decl(pragma_info_foreign_decl::in,
    U::di, U::uo) is det <= output(U).

mercury_format_pragma_foreign_decl(FDInfo, !U) :-
    FDInfo = pragma_info_foreign_decl(Lang, IsLocal, LiteralOrInclude),
    add_string(":- pragma foreign_decl(", !U),
    mercury_format_foreign_language_string(Lang, !U),
    add_string(", ", !U),
    (
        IsLocal = foreign_decl_is_local,
        add_string("local", !U)
    ;
        IsLocal = foreign_decl_is_exported,
        add_string("exported", !U)
    ),
    add_string(", ", !U),
    mercury_format_foreign_literal_or_include(LiteralOrInclude, !U),
    add_string(").\n", !U).

%---------------------------------------------------------------------------%
%
% Output a foreign_code pragma.
%

:- pred mercury_output_pragma_foreign_code(pragma_info_foreign_code::in,
    io::di, io::uo) is det.

mercury_output_pragma_foreign_code(FCInfo, !IO) :-
    FCInfo = pragma_info_foreign_code(Lang, LiteralOrInclude),
    io.write_string(":- pragma foreign_code(", !IO),
    mercury_format_foreign_language_string(Lang, !IO),
    io.write_string(", ", !IO),
    mercury_format_foreign_literal_or_include(LiteralOrInclude, !IO),
    io.write_string(").\n", !IO).

:- pred mercury_format_foreign_literal_or_include(
    foreign_literal_or_include::in, U::di, U::uo) is det <= output(U).

mercury_format_foreign_literal_or_include(LiteralOrInclude, !U) :-
    (
        LiteralOrInclude = floi_literal(Code),
        mercury_format_foreign_code_string(Code, !U)
    ;
        LiteralOrInclude = floi_include_file(FileName),
        add_string("include_file(", !U),
        add_quoted_string(FileName, !U),
        add_string(")", !U)
    ).

%---------------------%

% The code here is similar to the code for term_io.quote_string,
% but \n and \t are output directly, rather than escaped.
% Any changes here may require corresponding changes to term_io and vice versa.

:- pred mercury_format_foreign_code_string(string::in,
    U::di, U::uo) is det <= output(U).

mercury_format_foreign_code_string(S, !U) :-
    add_string("""", !U),
    mercury_format_escaped_string(S, !U),
    add_string("""", !U).

:- pred mercury_format_escaped_string(string::in,
    U::di, U::uo) is det <= output(U).

mercury_format_escaped_string(String, !U) :-
    string.foldl(mercury_format_escaped_char, String, !U).

:- pred mercury_format_escaped_char(char::in,
    U::di, U::uo) is det <= output(U).

mercury_format_escaped_char(Char, !U) :-
    ( if escape_special_char(Char, QuoteChar) then
        add_char('\\', !U),
        add_char(QuoteChar, !U)
    else if mercury_is_source_char(Char) then
        add_char(Char, !U)
    else
        add_string(mercury_escape_char(Char), !U)
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

mercury_output_pragma_foreign_proc(Lang, FPInfo, !IO) :-
    mercury_format_pragma_foreign_proc(Lang, FPInfo, !IO).

mercury_pragma_foreign_proc_to_string(Lang, FPInfo) = String :-
    mercury_format_pragma_foreign_proc(Lang, FPInfo, "", String).

:- pred mercury_format_pragma_foreign_proc(output_lang::in,
    pragma_info_foreign_proc::in, U::di, U::uo) is det <= output(U).

mercury_format_pragma_foreign_proc(Lang, FPInfo, !U) :-
    FPInfo = pragma_info_foreign_proc(Attributes, PredName, PredOrFunc, Vars0,
        ProgVarSet, InstVarSet, PragmaCode),
    add_string(":- pragma foreign_proc(", !U),
    ForeignLang = get_foreign_language(Attributes),
    mercury_format_foreign_language_string(ForeignLang, !U),
    add_string(", ", !U),
    mercury_format_sym_name(PredName, !U),
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
        add_string("(", !U),
        mercury_format_pragma_foreign_proc_vars(Lang, ProgVarSet, InstVarSet,
            Vars, !U),
        add_string(")", !U)
    ),
    (
        PredOrFunc = pf_predicate
    ;
        PredOrFunc = pf_function,
        add_string(" = (", !U),
        mercury_format_pragma_foreign_proc_vars(Lang, ProgVarSet, InstVarSet,
            ResultVars, !U),
        add_string(")", !U)
    ),
    add_string(", ", !U),
    mercury_format_pragma_foreign_attributes(ProgVarSet, Attributes, !U),
    add_string(", ", !U),
    PragmaCode = fp_impl_ordinary(C_Code, _),
    mercury_format_foreign_code_string(C_Code, !U),
    add_string(").\n", !U).

%---------------------%

:- pred mercury_format_pragma_foreign_proc_vars(output_lang::in,
    prog_varset::in, inst_varset::in, list(pragma_var)::in, U::di, U::uo)
    is det <= output(U).

mercury_format_pragma_foreign_proc_vars(_, _, _, [], !U).
mercury_format_pragma_foreign_proc_vars(Lang, ProgVarSet, InstVarSet,
        [Var | Vars], !U) :-
    Var = pragma_var(_Var, VarName, Mode, _BoxPolicy),
    add_string(VarName, !U),
    add_string(" :: ", !U),
    mercury_format_mode(Lang, InstVarSet, Mode, !U),
    (
        Vars = []
    ;
        Vars = [_ | _],
        add_string(", ", !U)
    ),
    mercury_format_pragma_foreign_proc_vars(Lang, ProgVarSet, InstVarSet,
        Vars, !U).

%---------------------%

:- pred mercury_format_pragma_foreign_attributes( prog_varset::in,
    pragma_foreign_proc_attributes::in, U::di, U::uo) is det <= output(U).

mercury_format_pragma_foreign_attributes(VarSet, Attributes, !U) :-
    add_string("[", !U),
    add_list(foreign_proc_attributes_to_strings(Attributes, VarSet), ", ",
        add_string, !U),
    add_string("]", !U).

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
    pragma_info_foreign_proc_export::in, U::di, U::uo) is det <= output(U).

mercury_format_pragma_foreign_proc_export(Lang, FPEInfo, !U) :-
   FPEInfo = pragma_info_foreign_proc_export(ForeignLang, PredNameModesPF,
        ExportName),
    PredNameModesPF = pred_name_modes_pf(Name, ModeList, PredOrFunc),
    varset.init(InstVarSet), % The varset isn't really used.
    add_string(":- pragma foreign_export(", !U),
    mercury_format_foreign_language_string(ForeignLang, !U),
    add_string(", ", !U),
    mercury_format_sym_name(Name, !U),
    (
        PredOrFunc = pf_function,
        pred_args_to_func_args(ModeList, ArgModes, RetMode),
        add_string("(", !U),
        mercury_format_mode_list(Lang, InstVarSet, ArgModes, !U),
        add_string(") = ", !U),
        mercury_format_mode(Lang, InstVarSet, RetMode, !U)
    ;
        PredOrFunc = pf_predicate,
        add_string("(", !U),
        mercury_format_mode_list(Lang, InstVarSet, ModeList, !U),
        add_string(")", !U)
    ),
    add_string(", ", !U),
    add_string(ExportName, !U),
    add_string(").\n", !U).

%---------------------------------------------------------------------------%
%
% Output an external_proc pragma.
%

:- pred mercury_format_pragma_external_proc(pragma_info_external_proc::in,
    U::di, U::uo) is det <= output(U).

mercury_format_pragma_external_proc(ExternalInfo, !U) :-
    ExternalInfo = pragma_info_external_proc(PredName, Arity, PorF,
        MaybeBackend),
    PorFStr = pred_or_func_to_str(PorF),
    add_string(":- pragma external_", !U),
    add_string(PorFStr, !U),
    add_string("(", !U),
    mercury_format_sym_name(PredName, !U),
    add_string("/", !U),
    add_int(Arity, !U),
    (
        MaybeBackend = no
    ;
        MaybeBackend = yes(Backend),
        add_string(", [", !U),
        add_string(backend_to_string(Backend), !U),
        add_string("]", !U)
    ),
    add_string(").\n", !U).

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

mercury_output_pragma_type_spec(VarNamePrint, Lang, TypeSpecInfo, !IO) :-
    TypeSpecInfo = pragma_info_type_spec(PredName, SpecName, Arity,
        MaybePredOrFunc, MaybeModes, Subst, VarSet, _),
    io.write_string(":- pragma type_spec(", !IO),
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
        write_pred_name_modes_pf(Lang, PredNameModesPF, !IO)
    ;
        MaybeModes = no,
        mercury_output_bracketed_sym_name_ngt(next_to_graphic_token, PredName,
            !IO),
        io.write_string("/", !IO),
        io.write_int(Arity, !IO)
    ),
    io.write_string(", (", !IO),
    io.write_list(Subst, ", ",
        mercury_output_type_subst(VarSet, VarNamePrint), !IO),
    io.write_string("), ", !IO),
    mercury_output_bracketed_sym_name_ngt(not_next_to_graphic_token, SpecName,
        !IO),
    io.write_string(").\n", !IO).

:- pred mercury_output_type_subst(tvarset::in, var_name_print::in,
    pair(tvar, mer_type)::in, io::di, io::uo) is det.

mercury_output_type_subst(VarSet, VarNamePrint, Var - Type, !IO) :-
    mercury_output_var(VarSet, VarNamePrint, Var, !IO),
    io.write_string(" = ", !IO),
    mercury_output_type(VarSet, VarNamePrint, Type, !IO).

%---------------------------------------------------------------------------%
%
% Output an unused_args pragma.
%

mercury_output_pragma_unused_args(UnusedArgsInfo, !IO) :-
    UnusedArgsInfo = pragma_info_unused_args(PredNameArityPFMn, UnusedArgs),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    io.write_string(":- pragma unused_args(", !IO),
    write_pred_or_func(PredOrFunc, !IO),
    io.write_string(", ", !IO),
    mercury_output_bracketed_sym_name(SymName, !IO),
    io.write_string(", ", !IO),
    io.write_int(Arity, !IO),
    io.write_string(", ", !IO),
    io.write_int(ModeNum, !IO),
    io.write_string(", [", !IO),
    mercury_format_int_list(UnusedArgs, !IO),
    io.write_string("]).\n", !IO).

:- pred mercury_format_int_list(list(int)::in,
    U::di, U::uo) is det <= output(U).

mercury_format_int_list([], !U).
mercury_format_int_list([First | Rest], !U) :-
    add_int(First, !U),
    mercury_format_int_list_2(Rest, !U).

:- pred mercury_format_int_list_2(list(int)::in,
    U::di, U::uo) is det <= output(U).

mercury_format_int_list_2([], !U).
mercury_format_int_list_2([First | Rest], !U) :-
    add_string(", ", !U),
    add_int(First, !U),
    mercury_format_int_list_2(Rest, !U).

%---------------------------------------------------------------------------%
%
% Output an exceptions pragma.
%

mercury_output_pragma_exceptions(ExceptionsInfo, !IO) :-
    ExceptionsInfo = pragma_info_exceptions(PredNameArityPFMn, ThrowStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    io.write_string(":- pragma exceptions(", !IO),
    write_pred_or_func(PredOrFunc, !IO),
    io.write_string(", ", !IO),
    mercury_output_bracketed_sym_name(SymName, !IO),
    io.write_string(", ", !IO),
    io.write_int(Arity, !IO),
    io.write_string(", ", !IO),
    io.write_int(ModeNum, !IO),
    io.write_string(", ", !IO),
    (
        ThrowStatus = will_not_throw,
        io.write_string("will_not_throw", !IO)
    ;
        ThrowStatus = may_throw(ExceptionType),
        io.write_string("may_throw(", !IO),
        (
            ExceptionType = user_exception,
            io.write_string("user_exception)", !IO)
        ;
            ExceptionType = type_exception,
            io.write_string("type_exception)", !IO)
        )
    ;
        ThrowStatus = throw_conditional,
        io.write_string("conditional", !IO)
    ),
    io.write_string(").\n", !IO).

%---------------------------------------------------------------------------%
%
% Output a trailing_info pragma.
%

mercury_output_pragma_trailing_info(TrailingInfo, !IO) :-
    TrailingInfo =
        pragma_info_trailing_info(PredNameArityPFMn, TrailingStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    io.write_string(":- pragma trailing_info(", !IO),
    write_pred_or_func(PredOrFunc, !IO),
    io.write_string(", ", !IO),
    mercury_output_bracketed_sym_name(SymName, !IO),
    io.write_string(", ", !IO),
    io.write_int(Arity, !IO),
    io.write_string(", ", !IO),
    io.write_int(ModeNum, !IO),
    io.write_string(", ", !IO),
    (
        TrailingStatus = trail_may_modify,
        io.write_string("may_modify_trail", !IO)
    ;
        TrailingStatus = trail_will_not_modify,
        io.write_string("will_not_modify_trail", !IO)
    ;
        TrailingStatus = trail_conditional,
        io.write_string("conditional", !IO)
    ),
    io.write_string(").\n", !IO).

%---------------------------------------------------------------------------%
%
% Output a mm_tabling_info pragma.
%

mercury_output_pragma_mm_tabling_info(TablingInfo, !IO) :-
    TablingInfo = pragma_info_mm_tabling_info(PredNameArityPFMn,
        MM_TablingStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    io.write_string(":- pragma mm_tabling_info(", !IO),
    write_pred_or_func(PredOrFunc, !IO),
    io.write_string(", ", !IO),
    mercury_output_bracketed_sym_name(SymName, !IO),
    io.write_string(", ", !IO),
    io.write_int(Arity, !IO),
    io.write_string(", ", !IO),
    io.write_int(ModeNum, !IO),
    io.write_string(", ", !IO),
    (
        MM_TablingStatus = mm_tabled_may_call,
        io.write_string("mm_tabled_may_call", !IO)
    ;
        MM_TablingStatus = mm_tabled_will_not_call,
        io.write_string("mm_tabled_will_not_call", !IO)
    ;
        MM_TablingStatus = mm_tabled_conditional,
        io.write_string("mm_tabled_conditional", !IO)
    ),
    io.write_string(").\n", !IO).

%---------------------------------------------------------------------------%
%
% Output an obsolete_pred or obsolete_proc pragma.
%

:- pred mercury_output_pragma_obsolete_pred(pragma_info_obsolete_pred::in,
    io::di, io::uo) is det.

mercury_output_pragma_obsolete_pred(ObsoletePredInfo, !IO) :-
    ObsoletePredInfo =
        pragma_info_obsolete_pred(PredNameArity, ObsoleteInFavourOf),
    PredNameArity = pred_name_arity(SymName, Arity),
    PredSymNameArity = sym_name_arity(SymName, Arity),
    ObsoleteStrs = list.map(wrapped_sym_name_and_arity_to_string,
        ObsoleteInFavourOf),
    ObsoleteStr = string.join_list(", ", ObsoleteStrs),
    io.format(":- pragma obsolete(%s, [%s]).\n",
        [s(wrapped_sym_name_and_arity_to_string(PredSymNameArity)),
        s(ObsoleteStr)], !IO).

:- pred mercury_output_pragma_obsolete_proc(output_lang::in,
    pragma_info_obsolete_proc::in, io::di, io::uo) is det.

mercury_output_pragma_obsolete_proc(Lang, ObsoleteProcInfo, !IO) :-
    ObsoleteProcInfo =
        pragma_info_obsolete_proc(PredNameModesPF, ObsoleteInFavourOf),
    io.write_string(":- pragma obsolete_proc(", !IO),
    write_pred_name_modes_pf(Lang, PredNameModesPF, !IO),
    ObsoleteStrs = list.map(wrapped_sym_name_and_arity_to_string,
        ObsoleteInFavourOf),
    ObsoleteStr = string.join_list(", ", ObsoleteStrs),
    io.format(", [%s]).\n", [s(ObsoleteStr)], !IO).

:- func wrapped_sym_name_and_arity_to_string(sym_name_and_arity) = string.

wrapped_sym_name_and_arity_to_string(SNA) = Str :-
    SNA = sym_name_arity(SymName, Arity),
    Str = mercury_bracketed_sym_name_to_string(SymName) ++
        "/" ++ string.int_to_string(Arity).

%---------------------------------------------------------------------------%
%
% Output a require tail recursion pragma
%

:- pred mercury_output_pragma_require_tail_recursion(output_lang::in,
    pragma_info_require_tail_recursion::in, io::di, io::uo) is det.

mercury_output_pragma_require_tail_recursion(Lang, RequireTR, !IO) :-
    RequireTR = pragma_info_require_tail_recursion(Proc, Info),
    ProcSpecStr = format_pred_name_arity_mpf_mmode(Lang, Proc),

    (
        Info = suppress_tailrec_warnings(_),
        format(":- pragma warn_tail_recursion(%s, [none]).\n",
            [s(ProcSpecStr)], !IO)
    ;
        Info = enable_tailrec_warnings(WarnOrError, Type, _),
        warning_or_error_string(WarnOrError, WarnOrErrorStr),
        require_tailrec_type_string(Type, TypeStr),

        format(":- pragma warn_tail_recursion(%s, [%s, %s]).\n",
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

:- pred mercury_output_pragma_tabled(pragma_info_tabled::in,
    io::di, io::uo) is det.

mercury_output_pragma_tabled(TabledInfo, !IO) :-
    TabledInfo = pragma_info_tabled(EvalMethod, PredNameArityMPF, _Mode,
        MaybeAttributes),
    PredNameArityMPF = pred_name_arity_mpf(PredName, Arity, _MaybePorF),
    PragmaName = eval_method_to_pragma_name(EvalMethod),
    (
        MaybeAttributes = yes(Attributes),
        Attributes = table_attributes(Strictness, MaybeSizeLimit, Stats,
            AllowReset),
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
    mercury_output_pragma_decl(PredName, Arity, pf_predicate, PragmaName,
        MaybeAfter, !IO).

%---------------------------------------------------------------------------%
%
% Output a fact_table pragma.
%

:- pred mercury_format_pragma_fact_table(pragma_info_fact_table::in,
    U::di, U::uo) is det <= output(U).

mercury_format_pragma_fact_table(FactTableInfo, !U) :-
    FactTableInfo = pragma_info_fact_table(PredNameArity, FileName),
    PredNameArity = pred_name_arity(PredName, Arity),
    add_string(":- pragma fact_table(", !U),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, PredName, !U),
    add_string("/", !U),
    add_int(Arity, !U),
    add_string(", ", !U),
    add_quoted_string(FileName, !U),
    add_string(").\n", !U).

%---------------------------------------------------------------------------%
%
% Output an oisu (order independent state update) pragma.
%

:- pred mercury_output_pragma_oisu(pragma_info_oisu::in,
    io::di, io::uo) is det.

mercury_output_pragma_oisu(OISUInfo, !IO) :-
    mercury_format_pragma_oisu(OISUInfo, !IO).

:- pred mercury_format_pragma_oisu(pragma_info_oisu::in,
    U::di, U::uo) is det <= output(U).

mercury_format_pragma_oisu(OISUInfo, !U) :-
    OISUInfo = pragma_info_oisu(TypeCtor, CreatorPreds, MutatorPreds,
        DestructorPreds),
    add_string(":- pragma oisu(", !U),
    TypeCtor = type_ctor(TypeName, TypeArity),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, TypeName, !U),
    add_string("/", !U),
    add_int(TypeArity, !U),
    add_string(",\n", !U),
    add_string("\tcreators([\n", !U),
    mercury_format_pred_name_arity_list(CreatorPreds, !U),
    add_string("\t]),\n", !U),
    add_string("\tmutators([\n", !U),
    mercury_format_pred_name_arity_list(MutatorPreds, !U),
    add_string("\t]),\n", !U),
    add_string("\tdestructors([\n", !U),
    mercury_format_pred_name_arity_list(DestructorPreds, !U),
    add_string("\t])\n", !U),
    add_string(").\n", !U).

:- pred mercury_format_pred_name_arity_list(list(pred_name_arity)::in,
    U::di, U::uo) is det <= output(U).

mercury_format_pred_name_arity_list([], !U).
mercury_format_pred_name_arity_list([PredNameArity | PredNameArities], !U) :-
    mercury_format_pred_name_arity_list_lag(PredNameArity, PredNameArities,
        !U).

:- pred mercury_format_pred_name_arity_list_lag(pred_name_arity::in,
    list(pred_name_arity)::in, U::di, U::uo) is det <= output(U).

mercury_format_pred_name_arity_list_lag(PredNameArity, PredNameArities, !U) :-
    add_string("\t\t", !U),
    mercury_format_pred_name_arity(PredNameArity, !U),
    (
        PredNameArities = [],
        add_string("\n", !U)
    ;
        PredNameArities = [HeadPredNameArity | TailPredNameArities],
        add_string(",\n", !U),
        mercury_format_pred_name_arity_list_lag(HeadPredNameArity,
            TailPredNameArities, !U)
    ).

:- pred mercury_format_pred_name_arity(pred_name_arity::in, U::di, U::uo)
    is det <= output(U).

mercury_format_pred_name_arity(PredNameArity, !U) :-
    PredNameArity = pred_name_arity(PredName, Arity),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, PredName, !U),
    add_string("/", !U),
    add_int(Arity, !U).

%---------------------------------------------------------------------------%
%
% Output a termination_info pragma.
%

write_pragma_termination_info(Lang, TermInfo, !IO) :-
    TermInfo = pragma_info_termination_info(PredNameModesPF,
        MaybeArgSize, MaybeTermination),
    io.write_string(":- pragma termination_info(", !IO),
    write_pred_name_modes_pf(Lang, PredNameModesPF, !IO),
    io.write_string(", ", !IO),
    write_maybe_arg_size_info(no, MaybeArgSize, !IO),
    io.write_string(", ", !IO),
    write_maybe_termination_info(no, MaybeTermination, !IO),
    io.write_string(").\n", !IO).

write_maybe_arg_size_info(Verbose, MaybeArgSizeInfo, !IO) :-
    (
        MaybeArgSizeInfo = no,
        io.write_string("not_set", !IO)
    ;
        MaybeArgSizeInfo = yes(infinite(Error)),
        io.write_string("infinite", !IO),
        (
            Verbose = yes,
            io.write_string("(", !IO),
            io.write(Error, !IO),
            io.write_string(")", !IO)
        ;
            Verbose = no
        )
    ;
        MaybeArgSizeInfo = yes(finite(Const, UsedArgs)),
        io.write_string("finite(", !IO),
        io.write_int(Const, !IO),
        io.write_string(", ", !IO),
        write_used_args(UsedArgs, !IO),
        io.write_string(")", !IO)
    ).

:- pred write_used_args(list(bool)::in, io::di, io::uo) is det.

write_used_args([], !IO) :-
    io.write_string("[]", !IO).
write_used_args([UsedArg | UsedArgs], !IO) :-
    io.write_string("[", !IO),
    write_bool(UsedArg, !IO),
    write_used_comma_args(UsedArgs, !IO),
    io.write_string("]", !IO).

:- pred write_used_comma_args(list(bool)::in, io::di, io::uo) is det.

write_used_comma_args([], !IO).
write_used_comma_args([UsedArg | UsedArgs], !IO) :-
    io.write_string(", ", !IO),
    write_bool(UsedArg, !IO),
    write_used_comma_args(UsedArgs, !IO).

:- pred write_bool(bool::in, io::di, io::uo) is det.

write_bool(Bool, !IO) :-
    (
        Bool = no,
        io.write_string("no", !IO)
    ;
        Bool = yes,
        io.write_string("yes", !IO)
    ).

write_maybe_termination_info(Verbose, MaybeTerminationInfo, !IO) :-
    (
        MaybeTerminationInfo = no,
        io.write_string("not_set", !IO)
    ;
        MaybeTerminationInfo = yes(cannot_loop(_)),
        io.write_string("cannot_loop", !IO)
    ;
        MaybeTerminationInfo = yes(can_loop(Error)),
        io.write_string("can_loop", !IO),
        (
            Verbose = yes,
            io.write_string("(", !IO),
            io.write(Error, !IO),
            io.write_string(")", !IO)
        ;
            Verbose = no
        )
    ).

%---------------------------------------------------------------------------%
%
% Output a termination2_info pragma.
%

:- pred write_pragma_termination2_info(output_lang::in,
    pragma_info_termination2_info::in, io::di, io::uo) is det.

write_pragma_termination2_info(Lang, Term2Info, !IO) :-
    Term2Info = pragma_info_termination2_info(PredNameModesPF,
        MaybeSuccess, MaybeFailure, MaybeTermination),
    io.write_string(":- pragma termination2_info(", !IO),
    write_pred_name_modes_pf(Lang, PredNameModesPF, !IO),
    io.write_string(", ", !IO),
    write_maybe_pragma_constr_arg_size_info(MaybeSuccess, !IO),
    io.write_string(", ", !IO),
    write_maybe_pragma_constr_arg_size_info(MaybeFailure, !IO),
    io.write_string(", ", !IO),
    write_maybe_pragma_termination_info(MaybeTermination, !IO),
    io.write_string(").\n", !IO).

:- pred write_maybe_pragma_constr_arg_size_info(
    maybe(pragma_constr_arg_size_info)::in, io::di, io::uo) is det.

write_maybe_pragma_constr_arg_size_info(no, !IO) :-
    io.write_string("not_set", !IO).
write_maybe_pragma_constr_arg_size_info(yes(ArgSizeInfo), !IO) :-
    io.write_string("constaints(", !IO),
    io.write_char('[', !IO),
    io.write_list(ArgSizeInfo, ", ", write_arg_size_constr, !IO),
    io.write_string("])", !IO).

:- pred write_arg_size_constr(arg_size_constr::in, io::di, io::uo) is det.

write_arg_size_constr(Constraint, !IO) :-
    (
        Constraint = le(Terms, Constant),
        OpStr = "le("
    ;
        Constraint = eq(Terms, Constant),
        OpStr = "eq("
    ),
    io.write_string(OpStr, !IO),
    io.write_char('[', !IO),
    io.write_list(Terms, ", ", write_arg_size_term, !IO),
    io.write_string("], ", !IO),
    rat.write_rat(Constant, !IO),
    io.write_char(')', !IO).

:- pred write_arg_size_term(arg_size_term::in, io::di, io::uo) is det.

write_arg_size_term(ArgSizeTerm, !IO) :-
    ArgSizeTerm = arg_size_term(VarId, Coefficient),
    io.write_string("term(", !IO),
    io.write_int(VarId, !IO),
    io.write_string(", ", !IO),
    rat.write_rat(Coefficient, !IO),
    io.write_char(')', !IO).

:- pred write_maybe_pragma_termination_info(maybe(pragma_termination_info)::in,
    io::di, io::uo) is det.

write_maybe_pragma_termination_info(MaybeTermination, !IO) :-
    (
        MaybeTermination = no,
        io.write_string("not_set", !IO)
    ;
        MaybeTermination = yes(Termination),
        (
            Termination = can_loop(_),
            TerminationStr = "can_loop"
        ;
            Termination = cannot_loop(_),
            TerminationStr = "cannot_loop"
        ),
        io.write_string(TerminationStr, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output a structure_sharing pragma.
%

write_pragma_structure_sharing_info(Lang, MaybeVarSet, MaybeTVarSet,
        SharingInfo, !IO) :-
    SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
        HeadVars, HeadVarTypes, MaybeSharingAs),
    (
        MaybeVarSet = yes(VarSet)
    ;
        MaybeVarSet = no,
        varset.init(VarSet)
    ),
    (
        MaybeTVarSet = yes(TypeVarSet)
    ;
        MaybeTVarSet = no,
        varset.init(TypeVarSet)
    ),
    io.write_string(":- pragma structure_sharing(", !IO),
    write_pred_name_modes_pf(Lang, PredNameModesPF, !IO),
    % write headvars and types:
    io.write_string(", ", !IO),
    write_vars_and_types(VarSet, TypeVarSet, HeadVars, HeadVarTypes, !IO),
    % write structure sharing information.
    io.write_string(", ", !IO),
    prog_ctgc.print_interface_structure_sharing_domain(VarSet, TypeVarSet,
        MaybeSharingAs, !IO),
    io.write_string(").\n", !IO).

%---------------------------------------------------------------------------%
%
% Output a structure_reuse pragma.
%

write_pragma_structure_reuse_info(Lang, MaybeVarSet, MaybeTVarSet,
        ReuseInfo, !IO) :-
    ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
        HeadVars, HeadVarTypes, MaybeStructureReuseDomain),
    (
        MaybeVarSet = yes(VarSet)
    ;
        MaybeVarSet = no,
        varset.init(VarSet)
    ),
    (
        MaybeTVarSet = yes(TypeVarSet)
    ;
        MaybeTVarSet = no,
        varset.init(TypeVarSet)
    ),
    io.write_string(":- pragma structure_reuse(", !IO),
    write_pred_name_modes_pf(Lang, PredNameModesPF, !IO),
    % write headvars and types:
    io.write_string(", ", !IO),
    write_vars_and_types(VarSet, TypeVarSet, HeadVars, HeadVarTypes, !IO),
    % write structure reuse information.
    io.write_string(", ", !IO),
    prog_ctgc.print_interface_maybe_structure_reuse_domain(VarSet, TypeVarSet,
        MaybeStructureReuseDomain, !IO),
    io.write_string(").\n", !IO).

%---------------------------------------------------------------------------%
%
% Predicates used to help print both structure_sharing and structure_reuse
% pragmas.
%

:- pred write_vars_and_types(prog_varset::in, tvarset::in,
    prog_vars::in, list(mer_type)::in, io::di, io::uo) is det.

write_vars_and_types(VarSet, TypeVarSet, HeadVars, HeadVarTypes, !IO) :-
    (
        HeadVars = [],
        io.write_string("vars, types", !IO)
    ;
        HeadVars = [_ | _],
        io.write_string("vars(", !IO),
        mercury_output_vars(VarSet, print_name_only, HeadVars, !IO),
        io.write_string("), ", !IO),

        io.write_string("types(", !IO),
        io.write_list(HeadVarTypes, ",",
            mercury_output_type(TypeVarSet, print_name_only), !IO),
        io.write_string(")", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output a require_feature_set pragma.
%

:- pred mercury_output_pragma_require_feature_set(
    pragma_info_require_feature_set::in, U::di, U::uo) is det <= output(U).

mercury_output_pragma_require_feature_set(RFSInfo, !U) :-
    RFSInfo = pragma_info_require_feature_set(Features0),
    Features = set.to_sorted_list(Features0),
    add_string(":- pragma require_feature_set(", !U),
    add_list(Features, ",", mercury_format_required_feature, !U),
    add_string(").\n", !U).

:- pred mercury_format_required_feature(required_feature::in, U::di, U::uo)
    is det <= output(U).

mercury_format_required_feature(reqf_concurrency, !U) :-
    add_string("concurrency", !U).
mercury_format_required_feature(reqf_single_prec_float, !U) :-
    add_string("single_prec_float", !U).
mercury_format_required_feature(reqf_double_prec_float, !U) :-
    add_string("double_prec_float", !U).
mercury_format_required_feature(reqf_memo, !U) :-
    add_string("memo", !U).
mercury_format_required_feature(reqf_parallel_conj, !U) :-
    add_string("parallel_conj", !U).
mercury_format_required_feature(reqf_trailing, !U) :-
    add_string("trailing", !U).
mercury_format_required_feature(reqf_strict_sequential, !U) :-
    add_string("strict_sequential", !U).
mercury_format_required_feature(reqf_conservative_gc, !U) :-
    add_string("conservative_gc", !U).

%---------------------------------------------------------------------------%
%
% Utility predicates.
%

:- pred write_pred_name_modes_pf(output_lang::in,
    pred_name_modes_pf::in, io::di, io::uo) is det.

write_pred_name_modes_pf(Lang, PredNameModesPF, !IO) :-
    PredNameModesPF = pred_name_modes_pf(SymName, Modes, PredOrFunc),
    varset.init(InstVarSet),
    MaybeDet = no,
    (
        PredOrFunc = pf_predicate,
        mercury_output_pred_mode_subdecl(Lang, InstVarSet, SymName,
            Modes, MaybeDet, !IO)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        mercury_output_func_mode_subdecl(Lang, InstVarSet, SymName,
            ArgModes, RetMode, MaybeDet, !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_pragma.
%---------------------------------------------------------------------------%
