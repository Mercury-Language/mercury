%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2021, 2024-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module converts the top levels of the parse tree structure
% back into Mercury source text.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_item.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.canonicalize_interface.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_output.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module string.builder.

%---------------------------------------------------------------------------%

:- pred mercury_format_items(merc_out_info::in, S::in, list(item)::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_item(merc_out_info::in, S::in, item::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output declarations in type class definitions, which may be pred declarations
% or mode declarations.
%

:- pred mercury_format_pred_or_mode_decls(merc_out_info::in,
    var_name_print::in, S::in, list(pred_or_mode_decl_item)::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_pred_or_mode_decl(merc_out_info::in, var_name_print::in,
    S::in, pred_or_mode_decl_item::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output type, inst and mode definitions. For type definitions, we have
% predicate for writing out some components on their as well.
%

:- pred mercury_format_item_type_defn(merc_out_info::in, S::in,
    item_type_defn_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_where_attributes(merc_out_info::in, tvarset::in,
    maybe(solver_type_details)::in, maybe_canonical::in,
    maybe(list(sym_name_arity))::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

:- pred mercury_format_ctor(tvarset::in, constructor::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred maybe_cons_exist_constraints_to_prefix_suffix(tvarset::in,
    string::in, string::in, maybe_cons_exist_constraints::in,
    string::out, string::out) is det.

:- pred maybe_brace_for_name_prefix_suffix(arity::in, string::in,
    string::out, string::out) is det.

%---------------------%

:- pred mercury_format_item_inst_defn(merc_out_info::in, S::in,
    item_inst_defn_info::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_item_mode_defn(merc_out_info::in, S::in,
    item_mode_defn_info::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output predicates' (and functions') type and mode declarations,
% possibly after some preprocessing.
%

    % Output the given predicate declaration, after
    %
    % - Maybe Unqualifying the predicate name, and
    % - Maybe writing out the line number Context.
    %
:- pred mercury_format_item_pred_decl_mu_mc(merc_out_info::in,
    var_name_print::in, S::in, item_pred_decl_info::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_item_pred_decl(output_lang::in, var_name_print::in,
    S::in, item_pred_decl_info::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_item_mode_decl(merc_out_info::in,
    S::in, item_mode_decl_info::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output items related foreign enums.
%

:- pred mercury_format_item_foreign_enum(merc_out_info::in, S::in,
    item_foreign_enum_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_item_foreign_export_enum(merc_out_info::in,
    S::in, item_foreign_export_enum_info::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output promises.
%

:- pred mercury_format_item_promise(merc_out_info::in, S::in,
    item_promise_info::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output typeclass definitions.
%

:- pred mercury_format_item_typeclass(merc_out_info::in,
    S::in, item_typeclass_info::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_item_abstract_typeclass(merc_out_info::in,
    S::in, item_abstract_typeclass_info::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output instance definitions, and some of their components.
%

:- pred mercury_format_item_instance(merc_out_info::in, S::in,
    item_instance_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- func item_abstract_instance_to_string(merc_out_info,
    item_abstract_instance_info) = string.
:- pred mercury_format_item_abstract_instance(merc_out_info::in,
    S::in, item_abstract_instance_info::in, U::di, U::uo) is det
    <= pt_output(S, U).

:- pred mercury_format_instance_method(instance_method::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output initialise, finalise and mutable declarations.
%

:- pred mercury_format_item_initialise(merc_out_info::in, S::in,
    item_initialise_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_item_finalise(merc_out_info::in, S::in,
    item_finalise_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_item_mutable(merc_out_info::in, S::in,
    item_mutable_info::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output a foreign_import_module pragma.
%

:- pred mercury_format_item_foreign_import_module(S::in, item_fim::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_output_fim_spec(io.text_output_stream::in, fim_spec::in,
    io::di, io::uo) is det.
:- pred mercury_format_fim_spec(S::in, fim_spec::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%

    % mercury_output_module_decl(Stream, Decl, ModuleName, !IO)
    %
:- pred mercury_output_module_decl(io.text_output_stream::in,
    string::in, module_name::in, io::di, io::uo) is det.
:- pred mercury_format_module_decl(S::in, string::in, module_name::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

    % Print a blank line if the given list is not empty.
    %
:- pred maybe_format_block_start_blank_line(S::in, list(T)::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- pragma type_spec_constrained_preds([pt_output(Stream, State)],
    apply_to_superclasses,
    [subst([Stream => io.text_output_stream, State = io.state]),
    subst([Stream => string.builder.handle, State = string.builder.state])]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_tree_out_clause.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_pragma.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.parse_tree_out_type_repn.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module one_or_more.
:- import_module ops.
:- import_module pair.
:- import_module set.
:- import_module term.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

mercury_format_items(_, _, [], !U).
mercury_format_items(Info, S, [Item | Items], !U) :-
    mercury_format_item(Info, S, Item, !U),
    mercury_format_items(Info, S, Items, !U).

mercury_format_item(Info, S, Item, !U) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        mercury_format_item_type_defn(Info, S, ItemTypeDefn, !U)
    ;
        Item = item_inst_defn(ItemInstDefn),
        mercury_format_item_inst_defn(Info, S, ItemInstDefn, !U)
    ;
        Item = item_mode_defn(ItemModeDefn),
        mercury_format_item_mode_defn(Info, S, ItemModeDefn, !U)
    ;
        Item = item_pred_decl(ItemPredDecl),
        mercury_format_item_pred_decl_mu_mc(Info, print_name_only, S,
            ItemPredDecl, !U)
    ;
        Item = item_mode_decl(ItemModeDecl),
        mercury_format_item_mode_decl(Info, S, ItemModeDecl, !U)
    ;
        Item = item_clause(ItemClause),
        mercury_format_item_clause(Info, S, ItemClause, !U)
    ;
        Item = item_foreign_proc(ItemForeignProc),
        mercury_format_item_foreign_proc(S, get_output_lang(Info),
            ItemForeignProc, !U)
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        mercury_format_item_foreign_enum(Info, S, ItemForeignEnum, !U)
    ;
        Item = item_foreign_export_enum(ItemForeignExportEnum),
        mercury_format_item_foreign_export_enum(Info, S,
            ItemForeignExportEnum, !U)
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        mercury_format_item_decl_pragma(Info, S, ItemDeclPragma, !U)
    ;
        Item = item_decl_marker(ItemDeclPragma),
        mercury_format_item_decl_marker(S, ItemDeclPragma, !U)
    ;
        Item = item_impl_pragma(ItemImplPragma),
        mercury_format_item_impl_pragma(Info, S, ItemImplPragma, !U)
    ;
        Item = item_impl_marker(ItemImplPragma),
        mercury_format_item_impl_marker(S, ItemImplPragma, !U)
    ;
        Item = item_generated_pragma(ItemGenPragma),
        mercury_format_item_generated_pragma(Info, S, ItemGenPragma, !U)
    ;
        Item = item_promise(ItemPromise),
        mercury_format_item_promise(Info, S, ItemPromise, !U)
    ;
        Item = item_typeclass(ItemTypeClass),
        mercury_format_item_typeclass(Info, S, ItemTypeClass, !U)
    ;
        Item = item_instance(ItemInstance),
        mercury_format_item_instance(Info, S, ItemInstance, !U)
    ;
        Item = item_initialise(ItemInitialise),
        mercury_format_item_initialise(Info, S, ItemInitialise, !U)
    ;
        Item = item_finalise(ItemFinalise),
        mercury_format_item_finalise(Info, S, ItemFinalise, !U)
    ;
        Item = item_mutable(ItemMutable),
        mercury_format_item_mutable(Info, S, ItemMutable, !U)
    ;
        Item = item_type_repn(ItemTypeRepn),
        mercury_format_item_type_repn(Info, S, ItemTypeRepn, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_pred_or_mode_decls(_, _, _, [], !U).
mercury_format_pred_or_mode_decls(Info, VarNamePrint, S, [Item | Items], !U) :-
    mercury_format_pred_or_mode_decl(Info, VarNamePrint, S, Item, !U),
    mercury_format_pred_or_mode_decls(Info, VarNamePrint, S, Items, !U).

mercury_format_pred_or_mode_decl(Info, VarNamePrint, S, Item, !U) :-
    (
        Item = pomd_pred(ItemPredDecl),
        mercury_format_item_pred_decl_mu_mc(Info, VarNamePrint, S,
            ItemPredDecl, !U)
    ;
        Item = pomd_mode(ItemModeDecl),
        mercury_format_item_mode_decl(Info, S, ItemModeDecl, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_item_type_defn(Info, S, ItemTypeDefn, !U) :-
    % XXX We should not use the tvar names in TypeVarSet; we should be
    % using standard tvar names such as TV1, TV2 etc. This should allow
    % any automatically generated interface files to remain unchanged
    % when the names of the type variables change in the source code,
    % thus avoiding the cascade of module recompilations that would
    % otherwise result.
    ItemTypeDefn = item_type_defn_info(SymName0, TypeParams, TypeDefn,
        TypeVarSet, Context, _SeqNum),
    maybe_unqualify_sym_name(Info, SymName0, SymName),
    maybe_format_line_number(Info, Context, S, !U),
    Args = list.map((func(V) = term.variable(V, Context)), TypeParams),
    construct_qualified_term_with_context(SymName, Args, Context, TypeTerm),
    (
        TypeDefn = parse_tree_abstract_type(DetailsAbstract),
        (
            ( DetailsAbstract = abstract_type_general
            ; DetailsAbstract = abstract_dummy_type
            ; DetailsAbstract = abstract_notag_type
            ; DetailsAbstract = abstract_type_fits_in_n_bits(_)
            ; DetailsAbstract = abstract_subtype(_)
            ),
            add_string(":- type ", S, !U)
        ;
            DetailsAbstract = abstract_solver_type,
            add_string(":- solver type ", S, !U)
        ),
        mercury_format_term_nq_vs(TypeVarSet, print_name_only,
            next_to_graphic_token, TypeTerm, S, !U),
        (
            DetailsAbstract = abstract_type_fits_in_n_bits(NumBits),
            % XXX TYPE_REPN Instead of adding this information to the
            % generated type definition, generate and write out
            % a separate type_repn item instead.
            mercury_format_where_abstract_enum_type(S, NumBits, !U)
        ;
            ( DetailsAbstract = abstract_dummy_type
            ; DetailsAbstract = abstract_notag_type
            )
            % XXX TYPE_REPN The same concern applies here, but these
            % kinds of abstract types are not yet generated anywhere,
            % so we don't have anything to do for them.
        ;
            DetailsAbstract = abstract_subtype(SuperTypeCtor),
            mercury_format_where_abstract_subtype(S, SuperTypeCtor, !U)
        ;
            ( DetailsAbstract = abstract_type_general
            ; DetailsAbstract = abstract_solver_type
            )
        ),
        add_string(".\n", S, !U)
    ;
        TypeDefn = parse_tree_eqv_type(DetailsEqv),
        DetailsEqv = type_details_eqv(EqvType),
        add_string(":- type ", S, !U),
        mercury_format_term_vs(TypeVarSet, print_name_only, TypeTerm, S, !U),
        add_string(" == ", S, !U),
        mercury_format_type(TypeVarSet, print_name_only, EqvType, S, !U),
        add_string(".\n", S, !U)
    ;
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(OoMCtors, MaybeCanonical, MaybeDirectArgs),
        add_string(":- type ", S, !U),
        mercury_format_term_vs(TypeVarSet, print_name_only, TypeTerm, S, !U),
        OoMCtors = one_or_more(HeadCtor, TailCtors),
        mercury_format_ctors(TypeVarSet, yes, HeadCtor, TailCtors, S, !U),
        mercury_format_where_attributes(Info, TypeVarSet, maybe.no,
            MaybeCanonical, MaybeDirectArgs, S, !U),
        add_string(".\n", S, !U)
    ;
        TypeDefn = parse_tree_sub_type(DetailsDu),
        DetailsDu = type_details_sub(SuperType, OoMCtors),
        add_string(":- type ", S, !U),
        mercury_format_term_vs(TypeVarSet, print_name_only, TypeTerm, S, !U),
        add_string(" =< ", S, !U),
        mercury_format_type(TypeVarSet, print_name_only, SuperType, S, !U),
        OoMCtors = one_or_more(HeadCtor, TailCtors),
        mercury_format_ctors(TypeVarSet, yes, HeadCtor, TailCtors, S, !U),
        add_string(".\n", S, !U)
    ;
        TypeDefn = parse_tree_solver_type(DetailsSolver),
        DetailsSolver =
            type_details_solver(SolverTypeDetails, MaybeCanonical),
        add_string(":- solver type ", S, !U),
        mercury_format_term_vs(TypeVarSet, print_name_only, TypeTerm, S, !U),
        mercury_format_where_attributes(Info, TypeVarSet,
            yes(SolverTypeDetails), MaybeCanonical, maybe.no, S, !U),
        add_string(".\n", S, !U)
    ;
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        DetailsForeign = type_details_foreign(ForeignType, MaybeCanonical,
            foreign_type_assertions(Assertions)),
        add_string(":- pragma foreign_type(", S, !U),
        (
            ForeignType = c(_),
            add_string("c, ", S, !U)
        ;
            ForeignType = java(_),
            add_string("java, ", S, !U)
        ;
            ForeignType = csharp(_),
            add_string("csharp, ", S, !U)
        ),
        mercury_format_term_vs(TypeVarSet, print_name_only, TypeTerm, S, !U),
        add_string(", \"", S, !U),
        (
            ForeignType = c(c_type(ForeignTypeStr))
        ;
            ForeignType = java(java_type(ForeignTypeStr))
        ;
            ForeignType = csharp(csharp_type(ForeignTypeStr))
        ),
        add_string(ForeignTypeStr, S, !U),
        add_string("\"", S, !U),
        set.to_sorted_list(Assertions, AssertionsList),
        (
            AssertionsList = []
        ;
            AssertionsList = [_ | _],
            AssertionStrs =
                list.map(foreign_type_assertion_to_string, AssertionsList),
            AssertionsStr = string.join_list(", ", AssertionStrs),
            add_string(", [", S, !U),
            add_string(AssertionsStr, S, !U),
            add_string("]", S, !U)
        ),
        add_string(")", S, !U),
        mercury_format_where_attributes(Info, TypeVarSet, no,
            MaybeCanonical, no, S, !U),
        add_string(".\n", S, !U)
    ).

%---------------------%
%
% Predicates needed to output more than one kind of type.
%

mercury_format_where_attributes(Info, TypeVarSet, MaybeSolverTypeDetails,
        MaybeCanonical, MaybeDirectArgs, S, !U) :-
    some [!LineCord]
    (
        !:LineCord = cord.init,
        (
            MaybeCanonical = canon
        ;
            MaybeCanonical = noncanon(NonCanon),
            (
                NonCanon = noncanon_abstract(_),
                cord.snoc("type_is_abstract_noncanonical", !LineCord)
            ;
                NonCanon = noncanon_subtype
            ;
                NonCanon = noncanon_uni_cmp(UniPred, CmpPred),
                UniPredStr = mercury_bracketed_sym_name_to_string(UniPred),
                CmpPredStr = mercury_bracketed_sym_name_to_string(CmpPred),
                UniPredLine = "equality is " ++ UniPredStr,
                CmpPredLine = "comparison is " ++ CmpPredStr,
                cord.snoc(UniPredLine, !LineCord),
                cord.snoc(CmpPredLine, !LineCord)
            ;
                NonCanon = noncanon_uni_only(UniPred),
                UniPredStr = mercury_bracketed_sym_name_to_string(UniPred),
                UniPredLine = "equality is " ++ UniPredStr,
                cord.snoc(UniPredLine, !LineCord)
            ;
                NonCanon = noncanon_cmp_only(CmpPred),
                CmpPredStr = mercury_bracketed_sym_name_to_string(CmpPred),
                CmpPredLine = "comparison is " ++ CmpPredStr,
                cord.snoc(CmpPredLine, !LineCord)
            )
        ),
        (
            MaybeDirectArgs = yes(DirectArgFunctors),
            FunctorStrs =
                list.map(mercury_bracketed_sym_name_arity_to_string,
                    DirectArgFunctors),
            FunctorsStr = string.join_list(", ", FunctorStrs),
            string.format("direct_arg is [%s]", [s(FunctorsStr)],
                DirectArgLine),
            cord.snoc(DirectArgLine, !LineCord)
        ;
            MaybeDirectArgs = no
        ),
        Lines = cord.list(!.LineCord),
        ( if
            MaybeSolverTypeDetails = no,
            Lines = []
        then
            true
        else
            add_string("\n    where\n", S, !U),
            (
                MaybeSolverTypeDetails = yes(SolverTypeDetails),
                mercury_format_solver_type_details(Info, S, TypeVarSet,
                    SolverTypeDetails, !U),
                (
                    Lines = []
                ;
                    Lines = [_ | _],
                    add_string(",\n", S, !U)
                )
            ;
                MaybeSolverTypeDetails = no
            ),
            % We cannot curry string.append, because it has several modes.
            IndentLine =
                ( func(Line) = IndentedLine :-
                    string.append("        ", Line, IndentedLine)
                ),
            IndentedLines = list.map(IndentLine, Lines),
            AllLines = string.join_list(",\n", IndentedLines),
            add_string(AllLines, S, !U)
        )
    ).

:- pred mercury_format_solver_type_details(merc_out_info::in,
    S::in, tvarset::in, solver_type_details::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_solver_type_details(Info, S, TypeVarSet, Details, !U) :-
    Details = solver_type_details(RepresentationType, GroundInst,
        AnyInst, MutableInfos),
    add_string("        representation is ", S, !U),
    mercury_format_type(TypeVarSet, print_name_only, RepresentationType,
        S, !U),
    Lang = get_output_lang(Info),
    varset.init(EmptyInstVarSet),
    add_string(",\n        ground is ", S, !U),
    mercury_format_inst(Lang, EmptyInstVarSet, GroundInst, S, !U),
    add_string(",\n        any is ", S, !U),
    mercury_format_inst(Lang, EmptyInstVarSet, AnyInst, S, !U),
    (
        MutableInfos = []
    ;
        MutableInfos = [_ | _],
        add_string(",\n        constraint_store is [\n            ", S, !U),
        list.gap_foldl(mercury_format_item_mutable(Info, S),
            add_string(",\n            ", S), MutableInfos, !U),
        add_string("\n        ]", S, !U)
    ).

%---------------------%
%
% Predicates needed to output abstract types.
%

:- pred mercury_format_where_abstract_enum_type(S::in, int::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_where_abstract_enum_type(S, NumBits, !U) :-
    add_string("\n\twhere\t", S, !U),
    add_string("type_is_abstract_enum(", S, !U),
    % XXX TYPE_REPN
    % add_string("type_is_representable_in_n_bits(", S, !U),
    add_int(NumBits, S, !U),
    add_string(")", S, !U).

:- pred mercury_format_where_abstract_subtype(S::in, type_ctor::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_where_abstract_subtype(S, TypeCtor, !U) :-
    add_string("\n\twhere\t", S, !U),
    add_string("type_is_abstract_subtype(", S, !U),
    TypeCtor = type_ctor(SymName, Arity),
    mercury_format_sym_name(SymName, S, !U),
    add_string("/", S, !U),
    add_int(Arity, S, !U),
    add_string(")", S, !U).

%---------------------%
%
% Predicates needed to output discriminated union types.
%

:- pred mercury_format_ctors(tvarset::in, bool::in,
    constructor::in, list(constructor)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_ctors(VarSet, First, HeadCtor, TailCtors, S, !U) :-
    (
        First = yes,
        add_string("\n    --->    ", S, !U)
    ;
        First = no,
        add_string("\n    ;       ", S, !U)
    ),
    mercury_format_ctor(VarSet, HeadCtor, S, !U),
    (
        TailCtors = []
    ;
        TailCtors = [HeadTailCtor | TailTailCtors],
        mercury_format_ctors(VarSet, no, HeadTailCtor, TailTailCtors, S, !U)
    ).

mercury_format_ctor(TVarSet, Ctor, S, !U) :-
    % NOTE The code of this predicate is almost identical to the
    % code of write_ctor and write_ctor_repn in hlds_out_module.m.
    % Any changes made here will probably need to be made there as well.
    Ctor = ctor(_Ordinal, MaybeExistConstraints, SymName, Args, Arity, _Ctxt),

    % The module name in SymName must be the same as the module qualifier
    % of the type_ctor, so there is no point in printing it.
    Name = unqualify_name(SymName),
    maybe_cons_exist_constraints_to_prefix_suffix(TVarSet, "", "",
        MaybeExistConstraints, ExistConstraintsPrefix, ExistConstraintsSuffix),
    maybe_brace_for_name_prefix_suffix(Arity, Name, BracePrefix, BraceSuffix),
    add_string(ExistConstraintsPrefix, S, !U),
    add_string(BracePrefix, S, !U),
    (
        Args = [],
        mercury_format_bracketed_sym_name(unqualified(Name), S, !U),
        % This space prevents a terminating full stop from being confused
        % as part of the sym_name if the sym_name contains graphical
        % characters.
        add_string(" ", S, !U)
    ;
        Args = [HeadArg | TailArgs],
        mercury_format_sym_name(unqualified(Name), S, !U),
        add_string("(\n", S, !U),
        mercury_format_ctor_args(S, TVarSet, HeadArg, TailArgs, !U),
        add_string("            )", S, !U)
    ),
    add_string(BraceSuffix, S, !U),
    add_string(ExistConstraintsSuffix, S, !U).

maybe_cons_exist_constraints_to_prefix_suffix(TVarSet, SuffixStart, SuffixEnd,
        MaybeExistConstraints, Prefix, Suffix) :-
    (
        MaybeExistConstraints = no_exist_constraints,
        Prefix = "",
        Suffix = ""
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(ExistQVars, Constraints,
            _UnconstrainedQVars, _ConstrainedQVars),
        ExistQVarsStr = mercury_quantifier_to_string(TVarSet,
            print_name_only, ExistQVars),
        ConstraintsStr = mercury_prog_constraint_list_to_string(TVarSet,
            print_name_only, "=>", Constraints),
        Prefix = ExistQVarsStr ++ "(",
        Suffix = SuffixStart ++ ConstraintsStr ++ ")" ++ SuffixEnd
    ).

maybe_brace_for_name_prefix_suffix(Arity, Name, Prefix, Suffix) :-
    % We need to quote ';'/2, '{}'/2, '=>'/2, and 'some'/2.
    % XXX I (zs) think that we should not allow these as constructor names.
    ( if
        Arity = 2,
        ( Name = ";"
        ; Name = "{}"
        ; Name = "some"
        ; Name = "=>"
        )
    then
        Prefix = "{ ",
        Suffix = " }"
    else
        Prefix = "",
        Suffix = ""
    ).

:- pred mercury_format_ctor_args(S::in, tvarset::in,
    constructor_arg::in, list(constructor_arg)::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_ctor_args(S, TVarSet, HeadArg, TailArgs, !U) :-
    mercury_format_ctor_arg(S, TVarSet, HeadArg, !U),
    (
        TailArgs = [],
        add_string("\n", S, !U)
    ;
        TailArgs = [HeadTailArg | TailTailArgs],
        add_string(",\n", S, !U),
        mercury_format_ctor_args(S, TVarSet, HeadTailArg, TailTailArgs, !U)
    ).

:- pred mercury_format_ctor_arg(S::in, tvarset::in, constructor_arg::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_ctor_arg(S, TVarSet, Arg, !U) :-
    Arg = ctor_arg(Name, Type, _Context),
    add_string("                 ", S, !U),
    mercury_format_ctor_arg_name_prefix(S, Name, !U),
    mercury_format_type(TVarSet, print_name_only, Type, S, !U).

:- pred mercury_format_ctor_arg_name_prefix(S::in, maybe(ctor_field_name)::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_ctor_arg_name_prefix(_S, no, !U).
mercury_format_ctor_arg_name_prefix(S, yes(FieldName), !U) :-
    FieldName = ctor_field_name(SymName, _Ctxt),
    mercury_format_bracketed_sym_name(SymName, S, !U),
    add_string(" :: ", S, !U).

%---------------------------------------------------------------------------%

mercury_format_item_inst_defn(Info, S, ItemInstDefn, !U) :-
    ItemInstDefn = item_inst_defn_info(SymName0, InstParams, MaybeForTypeCtor,
        MaybeAbstractInstDefn, InstVarSet, Context, _SeqNum),
    % If the unqualified name is a builtin inst, then output the qualified
    % name. This prevents the compiler giving an error about redefining
    % builtin insts when an interface file is read back in.
    maybe_unqualify_sym_name(Info, SymName0, UnQualSymName),
    ( if is_builtin_inst_name(InstVarSet, UnQualSymName, InstParams) then
        SymName = SymName0
    else
        SymName = UnQualSymName
    ),
    maybe_format_line_number(Info, Context, S, !U),
    Lang = get_output_lang(Info),
    ArgTerms = list.map(func(V) = variable(V, Context), InstParams),
    construct_qualified_term_with_context(SymName, ArgTerms, Context,
        InstTerm),
    (
        MaybeAbstractInstDefn = abstract_inst_defn,
        add_string(":- abstract_inst((", S, !U),
        mercury_format_term_vs(InstVarSet, print_name_only, InstTerm,
            S, !U),
        add_string(")).\n", S, !U)
    ;
        MaybeAbstractInstDefn = nonabstract_inst_defn(eqv_inst(Inst)),
        ( if
            % Is it safe to print the inst name without parentheses around it?
            sym_name_is_simple(SymName),
            not (
                SymName = unqualified(Name),
                mercury_op(Name)
            )
        then
            % Yes it is, so print the inst and its parameters without
            % extra parentheses around them.
            add_string(":- inst ", S, !U),
            add_string(sym_name_to_string(SymName), S, !U),
            (
                ArgTerms = []
            ;
                ArgTerms = [HeadArgTerm | TailArgTerms],
                add_string("(", S, !U),
                mercury_format_comma_separated_terms_vs(InstVarSet,
                    print_name_only, HeadArgTerm, TailArgTerms, S, !U),
                add_string(")", S, !U)
            )
        else
            % No it isn't, so print the extra parentheses.
            add_string(":- inst (", S, !U),
            mercury_format_term_vs(InstVarSet, print_name_only, InstTerm,
                S, !U),
            add_string(")", S, !U)
        ),
        (
            MaybeForTypeCtor = no
        ;
            MaybeForTypeCtor = yes(ForTypeCtor),
            ForTypeCtor = type_ctor(ForTypeCtorSymName, ForTypeCtorArity),
            add_string(" for ", S, !U),
            mercury_format_sym_name(ForTypeCtorSymName, S, !U),
            add_string("/", S, !U),
            add_int(ForTypeCtorArity, S, !U)
        ),
        ( if
            % Can we print the inst using the syntax that resembles
            % type definitions?
            Inst = bound(Uniq, _, BoundFunctors),
            Uniq = shared,
            bound_functor_cons_ids_are_all_simple(BoundFunctors, SimpleBIs),
            SimpleBIs = [HeadSimpleBI | TailSimpleBIs]
        then
            % Yes, so use that syntax, which is more readable, partly
            % because it has less clutter, and partly because it can be
            % formatted to have meaningful indentation.
            add_string("\n", S, !U),
            format_bound_functor_being_defined(S, Lang, InstVarSet,
                "    --->    ", HeadSimpleBI, TailSimpleBIs, !U)
        else
            % No, so fall back to the less readable but more general syntax.
            add_string(" == ", S, !U),
            mercury_format_inst(Lang, InstVarSet, Inst, S, !U),
            add_string(".\n", S, !U)
        )
    ).

    % Succeed if the sym_name describes a builtin inst.
    %
:- pred is_builtin_inst_name(inst_varset::in, sym_name::in, list(inst_var)::in)
    is semidet.

is_builtin_inst_name(InstVarSet, unqualified(Name), Args0) :-
    Args1 = list.map(func(V) = variable(coerce_var(V), dummy_context), Args0),
    Term = term.functor(term.atom(Name), Args1, dummy_context),
    varset.coerce(InstVarSet, VarSet),
    ContextPieces = cord.init,  % Dummy; not used.
    parse_inst(no_allow_constrained_inst_var(wnciv_inst_defn_lhs), VarSet,
        ContextPieces, Term, MaybeInst),
    MaybeInst = ok1(Inst),
    Inst \= defined_inst(user_inst(_, _)).

:- type simple_bound_functor
    --->    simple_bound_functor(string, list(mer_inst)).

:- pred bound_functor_cons_ids_are_all_simple(list(bound_functor)::in,
    list(simple_bound_functor)::out) is semidet.

bound_functor_cons_ids_are_all_simple([], []).
bound_functor_cons_ids_are_all_simple([HeadBI | TailBIs],
        [HeadSimpleBI | TailSimpleBIs])  :-
    HeadBI = bound_functor(ConsId, ArgInsts),
    ConsId = du_data_ctor(du_ctor(SymName, _, _)),
    sym_name_is_simple(SymName),
    SimpleName = sym_name_to_string(SymName),
    HeadSimpleBI = simple_bound_functor(SimpleName, ArgInsts),
    bound_functor_cons_ids_are_all_simple(TailBIs, TailSimpleBIs).

:- pred sym_name_is_simple(sym_name::in) is semidet.

sym_name_is_simple(SymName) :-
    Names = sym_name_to_list(SymName),
    all_true(name_is_simple, Names).

:- pred name_is_simple(string::in) is semidet.

name_is_simple(Name) :-
    string.to_char_list(Name, Chars),
    Chars = [HeadChar | TailChars],
    char.is_lower(HeadChar),
    all_true(char.is_alnum_or_underscore, TailChars).

:- pred format_bound_functor_being_defined(S::in,
    output_lang::in, inst_varset::in, string::in,
    simple_bound_functor::in, list(simple_bound_functor)::in,
    U::di, U::uo) is det <= pt_output(S, U).

format_bound_functor_being_defined(S, Lang, InstVarSet, ArrowOrSemi,
        HeadBI, TailBIs, !U) :-
    HeadBI = simple_bound_functor(Name, ArgInsts),
    string.format("%s%s", [s(ArrowOrSemi), s(Name)], InitStr),
    add_string(InitStr, S, !U),
    (
        ArgInsts = []
    ;
        ArgInsts = [_ | _],
        add_string("(", S, !U),
        mercury_format_inst_list(Lang, InstVarSet, ArgInsts, S, !U),
        add_string(")", S, !U)
    ),
    (
        TailBIs = [],
        add_string(".\n", S, !U)
    ;
        TailBIs = [HeadTailBI | TailTailBIs],
        add_string("\n", S, !U),
        format_bound_functor_being_defined(S, Lang, InstVarSet,
            "    ;       ", HeadTailBI, TailTailBIs, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_item_mode_defn(Info, S, ItemModeDefn, !U) :-
    ItemModeDefn = item_mode_defn_info(SymName, InstParams,
        MaybeAbstractModeDefn, VarSet, Context, _SeqNum),
    maybe_unqualify_sym_name(Info, SymName, UnQualSymName),
    maybe_format_line_number(Info, Context, S, !U),
    Lang = get_output_lang(Info),
    mercury_format_mode_defn(Lang, VarSet, Context, UnQualSymName, InstParams,
        MaybeAbstractModeDefn, S, !U).

    % This is defined to work on !U instead of !IO so that we can call
    % mercury_format_mode with simple_inst_info.
    %
:- pred mercury_format_mode_defn(output_lang::in, inst_varset::in,
    prog_context::in, sym_name::in, list(inst_var)::in,
    maybe_abstract_mode_defn::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_mode_defn(Lang, InstVarSet, Context, Name, Args,
        MaybeAbstractModeDefn, S, !U) :-
    (
        MaybeAbstractModeDefn = abstract_mode_defn,
        add_string(":- abstract_mode((", S, !U),
        mercury_format_mode_defn_head(InstVarSet, Context, Name, Args, S, !U),
        add_string(")).\n", S, !U)
    ;
        MaybeAbstractModeDefn = nonabstract_mode_defn(eqv_mode(Mode)),
        add_string(":- mode (", S, !U),
        mercury_format_mode_defn_head(InstVarSet, Context, Name, Args, S, !U),
        add_string(") == ", S, !U),
        mercury_format_mode(Lang, InstVarSet, Mode, S, !U),
        add_string(".\n", S, !U)
    ).

:- pred mercury_format_mode_defn_head(inst_varset::in, prog_context::in,
    sym_name::in, list(inst_var)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_mode_defn_head(InstVarSet, Context, Name, Args, S, !U) :-
    ArgTerms = list.map(func(V) = variable(V, Context), Args),
    construct_qualified_term_with_context(Name, ArgTerms, Context, ModeTerm),
    mercury_format_term_vs(InstVarSet, print_name_only, ModeTerm, S, !U).

%---------------------------------------------------------------------------%

mercury_format_item_pred_decl_mu_mc(Info, VarNamePrint, S,
        ItemPredDecl0, !U) :-
    MaybeQualifiedItemNames = get_maybe_qualified_item_names(Info),
    (
        MaybeQualifiedItemNames = qualified_item_names,
        ItemPredDecl = ItemPredDecl0
    ;
        MaybeQualifiedItemNames = unqualified_item_names,
        PredSymName0 = ItemPredDecl0 ^ pf_name,
        PredSymName = unqualified(unqualify_name(PredSymName0)),
        ItemPredDecl = ItemPredDecl0 ^ pf_name := PredSymName
    ),
    maybe_format_line_number(Info, ItemPredDecl ^ pf_context, S, !U),
    Lang = get_output_lang(Info),
    mercury_format_item_pred_decl(Lang, VarNamePrint, S, ItemPredDecl, !U).

mercury_format_item_pred_decl(Lang, VarNamePrint, S, ItemPredDecl, !U) :-
    % Most of the code that outputs pred declarations is in
    % parse_tree_out_pred_decl.m.
    ItemPredDecl = item_pred_decl_info(PredSymName, PredOrFunc,
        TypesAndMaybeModes, WithType, WithInst, MaybeDetism, _Origin,
        TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints,
        _Context, _SeqNum),
    ( if
        % Function declarations using `with_type` have the same format
        % as predicate declarations, but with `func' instead of `pred'.
        PredOrFunc = pf_function,
        WithType = no
    then
        mercury_format_func_decl(Lang, VarNamePrint,
            TypeVarSet, InstVarSet, ExistQVars,
            PredSymName, TypesAndMaybeModes, MaybeDetism,
            Purity, Constraints, ":- ", ".\n", ".\n", S, !U)
    else
        mercury_format_pred_or_func_decl(Lang, VarNamePrint,
            TypeVarSet, InstVarSet, PredOrFunc, ExistQVars,
            PredSymName, TypesAndMaybeModes, WithType, WithInst, MaybeDetism,
            Purity, Constraints, ":- ", ".\n", ".\n", S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_item_mode_decl(Info, S, ItemModeDecl, !U) :-
    % Most of the code that outputs mode declarations is in
    % parse_tree_out_pred_decl.m.
    ItemModeDecl = item_mode_decl_info(PredSymName0, MaybePredOrFunc, ArgModes,
        MaybeWithInst, MaybeDetism, InstVarSet, Context, _SeqNum),
    maybe_unqualify_sym_name(Info, PredSymName0, PredSymName),
    maybe_format_line_number(Info, Context, S, !U),
    Lang = get_output_lang(Info),
    ( if
        MaybePredOrFunc = yes(pf_function),
        % Function mode declarations using `with_type` have the same format
        % as predicate mode declarations.
        MaybeWithInst = no
    then
        pred_args_to_func_args(ArgModes, FuncArgModes, ReturnMode),
        mercury_format_func_mode_decl(Lang, InstVarSet, PredSymName,
            FuncArgModes, ReturnMode, MaybeDetism, S, !U)
    else
        mercury_format_pred_mode_decl(Lang, InstVarSet, PredSymName,
            ArgModes, MaybeWithInst, MaybeDetism, S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_item_foreign_enum(_Info, S, ItemForeignEnum, !U) :-
    ItemForeignEnum = item_foreign_enum_info(Lang, TypeCtor, OoMValues,
        _Context, _SeqNum),
    add_string(":- pragma foreign_enum(", S, !U),
    mercury_format_foreign_language_string(Lang, S, !U),
    add_string(", ", S, !U),
    TypeCtor = type_ctor(TypeName, TypeArity),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, TypeName,
        S, !U),
    add_string("/", S, !U),
    add_int(TypeArity, S, !U),
    add_string(", ", S, !U),
    Values = one_or_more_to_list(OoMValues),
    mercury_format_unqual_sym_name_string_assoc_list(Values, S, !U),
    add_string(").\n", S, !U).

    % Output an association list of to-be-unqualified sym_names and strings.
    % The strings will be quoted in the output.
    %
:- pred mercury_format_unqual_sym_name_string_assoc_list(
    assoc_list(sym_name, string)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_unqual_sym_name_string_assoc_list(AssocList, S, !U) :-
    add_char('[', S, !U),
    add_list(mercury_format_unqual_sym_name_string_pair, ", ",
        AssocList, S, !U),
    add_char(']', S, !U).

:- pred mercury_format_unqual_sym_name_string_pair(
    pair(sym_name, string)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_unqual_sym_name_string_pair(SymName0 - String, S, !U) :-
    Name = unqualify_name(SymName0),
    SymName = unqualified(Name),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, SymName,
        S, !U),
    add_string(" - ", S, !U),
    add_quoted_string(String, S, !U).

%---------------------------------------------------------------------------%

mercury_format_item_foreign_export_enum(_Info, S, ItemForeignExportEnum, !U) :-
    ItemForeignExportEnum = item_foreign_export_enum_info(Lang, TypeCtor,
        Attributes, Overrides, _Context, _SeqNum),
    add_string(":- pragma foreign_export_enum(", S, !U),
    mercury_format_foreign_language_string(Lang, S, !U),
    add_string(", ", S, !U),
    TypeCtor = type_ctor(TypeName, TypeArity),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, TypeName,
        S, !U),
    add_string("/", S, !U),
    add_int(TypeArity, S, !U),
    add_string(", ", S, !U),
    mercury_format_foreign_export_enum_attributes(Attributes, S, !U),
    add_string(", ", S, !U),
    mercury_format_sym_name_string_assoc_list(Overrides, S, !U),
    add_string(").\n", S, !U).

:- pred mercury_format_foreign_export_enum_attributes(
    export_enum_attributes::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_foreign_export_enum_attributes(Attributes, S, !U) :-
    MaybePrefix = Attributes ^ ee_attr_prefix,
    add_string("[", S, !U),
    (
        MaybePrefix = no
    ;
        MaybePrefix = yes(Prefix),
        add_string("prefix(", S, !U),
        add_quoted_string(Prefix, S, !U),
        add_char(')', S, !U)
    ),
    add_string("]", S, !U).

    % Output an association list of sym_names and strings.
    % The strings will be quoted in the output.
    %
:- pred mercury_format_sym_name_string_assoc_list(
    assoc_list(sym_name, string)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_sym_name_string_assoc_list(AssocList, S, !U) :-
    add_char('[', S, !U),
    add_list(mercury_format_sym_name_string_pair, ", ", AssocList, S, !U),
    add_char(']', S, !U).

:- pred mercury_format_sym_name_string_pair(
    pair(sym_name, string)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_sym_name_string_pair(SymName - String, S, !U) :-
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, SymName,
        S, !U),
    add_string(" - ", S, !U),
    add_quoted_string(String, S, !U).

%---------------------------------------------------------------------------%

mercury_format_item_promise(_, S, ItemPromise, !U) :-
    % Any changes here may require similar changes in the write_promise
    % predicate in intermod.m.
    ItemPromise = item_promise_info(PromiseType, Goal, VarSet, UnivVars,
        _Context, _SeqNum),
    UnivVarStrs = list.map(varset.lookup_name(VarSet), UnivVars),
    UnivVarsStr = string.join_list(", ", UnivVarStrs),
    % The parentheses around the goal are required; without them,
    % operator precedence problems prevent the parser from being able
    % to read back in the promises we write out.
    (
        PromiseType = promise_type_true,
        string.format(":- promise all [%s] ", [s(UnivVarsStr)], PrefixStr)
    ;
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exhaustive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        string.format(":- all [%s]\n%s\n",
            [s(UnivVarsStr), s(promise_to_string(PromiseType))], PrefixStr)
    ),
    add_string(PrefixStr, S, !U),
    add_string("(\n", S, !U),
    Indent = 1u,
    mercury_format_goal(S, VarSet, Indent, Goal, !U),
    add_string("\n).\n", S, !U).

%---------------------------------------------------------------------------%

mercury_format_item_typeclass(Info, S, ItemTypeClass, !U) :-
    ItemTypeClass = item_typeclass_info(ClassName0, Vars, Constraints, FunDeps,
        Interface, VarSet, _Context, _SeqNum),
    maybe_unqualify_sym_name(Info, ClassName0, ClassName),
    ClassNameStr = mercury_sym_name_to_string(ClassName),
    VarStrs = list.map(varset.lookup_name(VarSet), Vars),
    VarsStr = string.join_list(", ", VarStrs),
    string.format(":- typeclass %s(%s)",
        [s(ClassNameStr), s(VarsStr)], DeclStr),
    add_string(DeclStr, S, !U),
    mercury_format_fundeps_and_prog_constraint_list(VarSet, print_name_only,
        FunDeps, Constraints, S, !U),
    (
        Interface = class_interface_abstract,
        add_string(".\n", S, !U)
    ;
        Interface = class_interface_concrete(ClassDecls),
        (
            ClassDecls = [],
            add_string(" where [].\n", S, !U)
        ;
            ClassDecls = [HeadClassDecl | TailClassDecls],
            add_string(" where [\n", S, !U),
            Lang = get_output_lang(Info),
            format_class_decls(S, Lang, print_name_only,
                HeadClassDecl, TailClassDecls, !U),
            add_string("].\n", S, !U)
        )
    ).

mercury_format_item_abstract_typeclass(Info, S, ItemTypeClass, !U) :-
    ItemTypeClass = item_typeclass_info(ClassName0, Vars, Constraints, FunDeps,
        class_interface_abstract, VarSet, _Context, _SeqNum),
    maybe_unqualify_sym_name(Info, ClassName0, ClassName),
    ClassNameStr = mercury_sym_name_to_string(ClassName),
    VarStrs = list.map(varset.lookup_name(VarSet), Vars),
    VarsStr = string.join_list(", ", VarStrs),
    string.format(":- typeclass %s(%s)", [s(ClassNameStr), s(VarsStr)],
        StartStr),
    add_string(StartStr, S, !U),
    mercury_format_fundeps_and_prog_constraint_list(VarSet, print_name_only,
        FunDeps, Constraints, S, !U),
    add_string(".\n", S, !U).

:- pred mercury_format_fundeps_and_prog_constraint_list(tvarset::in,
    var_name_print::in, list(prog_fundep)::in, list(prog_constraint)::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_fundeps_and_prog_constraint_list(VarSet, VarNamePrint,
        FunDeps, Constraints, S, !U) :-
    ( if
        FunDeps = [],
        Constraints = []
    then
        true
    else
        add_string(" <= (", S, !U),
        add_list(mercury_format_fundep(VarSet, VarNamePrint), ", ", FunDeps,
            S, !U),
        (
            Constraints = []
        ;
            Constraints = [_ | _],
            (
                FunDeps = []
            ;
                FunDeps = [_ | _],
                add_string(", ", S, !U)
            ),
            add_list(mercury_format_constraint(VarSet, VarNamePrint),
                ", ", Constraints, S, !U)
        ),
        add_string(")", S, !U)
    ).

:- pred mercury_format_fundep(tvarset::in, var_name_print::in, prog_fundep::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_fundep(TypeVarSet, VarNamePrint, FunDep, S, !U) :-
    FunDep = prog_fundep(Domain, Range),
    add_string("(", S, !U),
    add_list(mercury_format_var_vs(TypeVarSet, VarNamePrint), ", ", Domain,
        S, !U),
    add_string(" -> ", S, !U),
    add_list(mercury_format_var_vs(TypeVarSet, VarNamePrint), ", ", Range,
        S, !U),
    add_string(")", S, !U).

:- pred format_class_decls(S::in, output_lang::in, var_name_print::in,
    class_decl::in, list(class_decl)::in, U::di, U::uo) is det
    <= pt_output(S, U).

format_class_decls(S, Lang, VarNamePrint, HeadClassDecl, TailClassDecls, !U) :-
    format_class_decl(S, Lang, VarNamePrint, HeadClassDecl, !U),
    (
        TailClassDecls = [],
        add_string("\n", S, !U)
    ;
        TailClassDecls = [HeadTailClassDecl | TailTailClassDecls],
        add_string(",\n", S, !U),
        format_class_decls(S, Lang, VarNamePrint,
            HeadTailClassDecl, TailTailClassDecls, !U)
    ).

:- pred format_class_decl(S::in, output_lang::in, var_name_print::in,
    class_decl::in, U::di, U::uo) is det <= pt_output(S, U).

format_class_decl(S, Lang, VarNamePrint, Decl, !U) :-
    add_string("\t", S, !U),
    (
        Decl = class_decl_pred_or_func(PredOrFuncInfo),
        PredOrFuncInfo = class_pred_or_func_info(SymName0, PredOrFunc,
            TypesAndMaybeModes, WithType, WithInst, MaybeDetism,
            TypeVarSet, InstVarSet, ExistQVars, Purity,
            Constraints, _Context),

        % The module name is implied by the qualifier of the
        % `:- typeclass declaration'.
        SymName = unqualified(unqualify_name(SymName0)),
        ( if
            % Function declarations using `with_type` have the same format
            % as predicate declarations, but with `func' instead of `pred'.
            PredOrFunc = pf_function,
            WithType = no
        then
            mercury_format_func_decl(Lang, VarNamePrint,
                TypeVarSet, InstVarSet, ExistQVars,
                SymName, TypesAndMaybeModes, MaybeDetism,
                Purity, Constraints, "", ",\n\t", "", S, !U)
        else
            mercury_format_pred_or_func_decl(Lang, VarNamePrint,
                TypeVarSet, InstVarSet, PredOrFunc, ExistQVars,
                SymName, TypesAndMaybeModes, WithType, WithInst, MaybeDetism,
                Purity, Constraints, "", ",\n\t", "", S, !U)
        )
    ;
        Decl = class_decl_mode(ModeInfo),
        ModeInfo = class_mode_info(SymName0, PredOrFunc, Modes,
            WithInst, MaybeDetism, InstVarSet, _Context),

        % The module name is implied by the qualifier of the
        % `:- typeclass declaration'.
        SymName = unqualified(unqualify_name(SymName0)),
        ( if
            % Function mode declarations using `with_type` have the same format
            % as predicate mode declarations.
            PredOrFunc = yes(pf_function),
            WithInst = no
        then
            pred_args_to_func_args(Modes, FuncModes, RetMode),
            mercury_format_func_mode_decl_gen(Lang, InstVarSet,
                SymName, FuncModes, RetMode, MaybeDetism, "", "", S, !U)
        else
            mercury_format_pred_or_func_mode_decl_gen(Lang, InstVarSet,
                SymName, Modes, WithInst, MaybeDetism, "", "", S, !U)
        )
    ).

%---------------------------------------------------------------------------%

mercury_format_item_instance(_Info, S, ItemInstance, !U) :-
    ItemInstance = item_instance_info(_ClassName, _Types, _OriginalTypes,
        _Constraints, Body, _VarSet, _InstanceModuleName, _Context, _SeqNum),
    HeaderStr = mercury_instance_header_to_string(ItemInstance),
    (
        Body = instance_body_abstract,
        string.format("%s.\n", [s(HeaderStr)], DeclStr),
        add_string(DeclStr, S, !U)
    ;
        Body = instance_body_concrete(Methods),
        (
            Methods = [],
            string.format("%s where [].\n", [s(HeaderStr)], DeclStr),
            add_string(DeclStr, S, !U)
        ;
            Methods = [HeadMethod | TailMethods],
            string.format("%s where [\n", [s(HeaderStr)], DeclStartStr),
            add_string(DeclStartStr, S, !U),
            mercury_format_instance_methods(S, HeadMethod, TailMethods, !U),
            add_string("].\n", S, !U)
        )
    ).

item_abstract_instance_to_string(Info, ItemAbstractInstance) = Str :-
    State0 = string.builder.init,
    mercury_format_item_abstract_instance(Info, string.builder.handle,
        ItemAbstractInstance, State0, State),
    Str = string.builder.to_string(State).

mercury_format_item_abstract_instance(_Info, S, ItemAbstractInstance, !U) :-
    HeaderStr =
        mercury_instance_header_to_string(coerce(ItemAbstractInstance)),
    string.format("%s.\n", [s(HeaderStr)], DeclStr),
    add_string(DeclStr, S, !U).

:- func mercury_instance_header_to_string(item_instance_info) = string.

mercury_instance_header_to_string(ItemInstance) = Str :-
    % XXX When prettyprinting a Mercury module, we want to print the original
    % types. When generating interface files, we want to print the
    % equiv-type-expanded types. We do the latter.
    % XXX We could add an argument, or a field to merc_out_info,
    % that says which kind of file we are generating.
    ItemInstance = item_instance_info(ClassName, Types, _OriginalTypes,
        Constraints, _Body, VarSet, _InstanceModuleName, _Context, _SeqNum),
    ClassNameStr = mercury_sym_name_to_string(ClassName),
    TypeStrs =
        list.map(mercury_type_to_string(VarSet, print_name_only), Types),
    TypesStr = string.join_list(", ", TypeStrs),
    ConstraintsStr = mercury_prog_constraint_list_to_string(VarSet,
        print_name_only, "<=", Constraints),
    ( if
        ( ops.mercury_op_table_is_op(ClassNameStr)
        ; not is_all_alnum_or_underscore(ClassNameStr)
        )
    then
        % We put an extra set of brackets around the class name
        % if the name is an operator or contains non-alphanumeric characters.
        string.format(":- instance (%s(%s))%s",
            [s(ClassNameStr), s(TypesStr), s(ConstraintsStr)], Str)
    else
        string.format(":- instance %s(%s)%s",
            [s(ClassNameStr), s(TypesStr), s(ConstraintsStr)], Str)
    ).

:- pred mercury_format_instance_methods(S::in,
    instance_method::in, list(instance_method)::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_instance_methods(S, HeadMethod, TailMethods, !U) :-
    mercury_format_instance_method(HeadMethod, S, !U),
    (
        TailMethods = [],
        add_string("\n", S, !U)
    ;
        TailMethods = [HeadTailMethod | TailTailMethods],
        add_string(",\n", S, !U),
        mercury_format_instance_methods(S, HeadTailMethod, TailTailMethods, !U)
    ).

mercury_format_instance_method(Method, S, !U) :-
    Method = instance_method(MethodId, Defn, _Context),
    MethodId = pred_pf_name_arity(PredOrFunc, MethodSymName, UserArity),
    UserArity = user_arity(UserArityInt),
    (
        Defn = instance_proc_def_name(PredName),
        PFStr = pred_or_func_to_str(PredOrFunc),
        MethodSymNameStr = mercury_bracketed_sym_name_to_string_ngt(
            next_to_graphic_token, MethodSymName),
        PredNameStr = mercury_bracketed_sym_name_to_string(PredName),
        string.format("\t%s(%s/%d) is %s",
            [s(PFStr), s(MethodSymNameStr), i(UserArityInt), s(PredNameStr)],
            DeclStr),
        add_string(DeclStr, S, !U)
    ;
        Defn = instance_proc_def_clauses(ItemsCord),
        Items = cord.list(ItemsCord),
        % XXX should we output the term contexts?
        add_string("\t(", S, !U),
        add_list(mercury_format_instance_method_clause(MethodSymName),
            "),\n\t(", Items, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_item_initialise(_, S, ItemInitialise, !IO) :-
    ItemInitialise = item_initialise_info(PredSymName, UserArity, _, _, _),
    PredSymNameStr = mercury_bracketed_sym_name_to_string(PredSymName),
    UserArity = user_arity(UserArityInt),
    string.format(":- initialise %s/%d.\n",
        [s(PredSymNameStr), i(UserArityInt)], DeclStr),
    add_string(DeclStr, S, !IO).

%---------------------------------------------------------------------------%

mercury_format_item_finalise(_, S, ItemFinalise, !IO) :-
    ItemFinalise = item_finalise_info(PredSymName, UserArity, _, _, _),
    PredSymNameStr = mercury_bracketed_sym_name_to_string(PredSymName),
    UserArity = user_arity(UserArityInt),
    string.format(":- finalise %s/%d.\n",
        [s(PredSymNameStr), i(UserArityInt)], DeclStr),
    add_string(DeclStr, S, !IO).

%---------------------------------------------------------------------------%

mercury_format_item_mutable(Info, S, ItemMutable, !U) :-
    ItemMutable = item_mutable_info(Name, _OrigType, Type, _OrigInst, Inst,
        InitTerm, MutVarSet, Attrs, _Context, _SeqNum),
    add_string(":- mutable(", S, !U),
    add_string(Name, S, !U),
    add_string(", ", S, !U),
    mercury_format_type(varset.init, print_name_only, Type, S, !U),
    add_string(", ", S, !U),

    % See the comments for read_mutable_decl for the reason we _must_ use
    % MutVarSet here.
    mercury_format_term_vs(MutVarSet, print_name_only, InitTerm, S, !U),
    add_string(", ", S, !U),
    Lang = get_output_lang(Info),
    mercury_format_inst(Lang, varset.init, Inst, S, !U),
    add_string(", ", S, !U),
    add_string(string.string(Attrs), S, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%

mercury_format_item_foreign_import_module(S, ItemFIM, !U) :-
    ItemFIM = item_fim(Lang, ModuleName, _Context, _SeqNum),
    FIMSpec = fim_spec(Lang, ModuleName),
    mercury_format_fim_spec(S, FIMSpec, !U).

mercury_output_fim_spec(Stream, FIMSpec, !IO) :-
    mercury_format_fim_spec(Stream, FIMSpec, !IO).

mercury_format_fim_spec(S, FIMSpec, !U) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    add_string(":- pragma foreign_import_module(", S, !U),
    mercury_format_foreign_language_string(Lang, S, !U),
    add_string(", ", S, !U),
    mercury_format_bracketed_sym_name_ngt(not_next_to_graphic_token,
        ModuleName, S, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%

mercury_output_module_decl(Stream, Decl, ModuleName, !IO) :-
    mercury_format_module_decl(Stream, Decl, ModuleName, !IO).

mercury_format_module_decl(S, Decl, ModuleName, !U) :-
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    string.format(":- %s %s.\n", [s(Decl), s(ModuleNameStr)], DeclStr),
    add_string(DeclStr, S, !U).

%---------------------------------------------------------------------------%

maybe_format_block_start_blank_line(S, Items, !U) :-
    (
        Items = []
    ;
        Items = [_ | _],
        add_string("\n", S, !U)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_item.
%---------------------------------------------------------------------------%
