%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015, 2020-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module converts the top levels of the parse tree structure
% back into Mercury source text.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_type_repn.
:- interface.

:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

%---------------------------------------------------------------------------%

    % Output a type_repn item.
    %
    % With TypeRepnFor = type_repn_for_machines, we write out everything
    % on one line. This is as compact as possible for regular use,
    % and has some advantages for humans as well, e.g. it makes automatic
    % comparisons between type_repn items in different versions of an
    % interface files easy. However, it does make these items very hard
    % to read for humans. This is why we support the ability to set
    % TypeRepnFor to type_repn_for_humans, which causes us to print
    % type_repn items in a format with structured indentation and
    % limited length lines.
    %
:- pred mercury_format_item_type_repn(merc_out_info::in, S::in,
    item_type_repn_info::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- func fill_kind_size_to_string(fill_kind_size) = string.

%---------------------------------------------------------------------------%

:- func foreign_type_assertion_to_string(foreign_type_assertion) = string.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module one_or_more.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module uint8.
:- import_module varset.

%---------------------------------------------------------------------------%

mercury_format_item_type_repn(Info, S, ItemTypeRepn, !U) :-
    ItemTypeRepn = item_type_repn_info(TypeCtorSymName0, TypeParams, RepnInfo,
        TVarSet, Context, _SeqNum),
    add_string(":- type_representation(", S, !U),
    maybe_unqualify_sym_name(Info, TypeCtorSymName0, TypeCtorSymName),
    Args = list.map((func(V) = term.variable(V, Context)), TypeParams),
    construct_qualified_term_with_context(TypeCtorSymName, Args, Context,
        TypeTerm),
    mercury_format_term_nq_vs(TVarSet, print_num_only, next_to_graphic_token,
        TypeTerm, S, !U),
    add_string(",", S, !U),
    (
        RepnInfo = tcrepn_is_eqv_to(EqvType),
        add_string(" is_eqv_to(", S, !U),
        mercury_format_type(TVarSet, print_num_only, EqvType, S, !U),
        add_string(")", S, !U)
    ;
        RepnInfo = tcrepn_is_subtype_of(SuperTypeCtor),
        add_string(" is_subtype_of(", S, !U),
        SuperTypeCtor = type_ctor(SuperTypeCtorSymName, SuperTypeArity),
        mercury_format_sym_name(SuperTypeCtorSymName, S, !U),
        add_string("/", S, !U),
        add_int(SuperTypeArity, S, !U),
        add_string(")", S, !U)
    ;
        RepnInfo = tcrepn_is_word_aligned_ptr,
        add_string(" is_word_aligned_ptr", S, !U)
    ;
        RepnInfo = tcrepn_du(DuRepn),
        TypeRepnFor = get_type_repn_for(Info),
        (
            TypeRepnFor = type_repn_for_machines,
            add_string(" du_repn(", S, !U),
            mercury_format_du_type_repn(S, TypeRepnFor, 2, TVarSet,
                DuRepn, !U),
            add_string(")", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(1),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string("du_repn(", S, !U),
            mercury_format_du_type_repn(S, TypeRepnFor, 2, TVarSet,
                DuRepn, !U),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string(")", S, !U)
        )
    ;
        RepnInfo = tcrepn_foreign(MaybeCJCsRepn),
        TypeRepnFor = get_type_repn_for(Info),
        (
            TypeRepnFor = type_repn_for_machines,
            add_string(" foreign_type_repn(", S, !U),
            mercury_format_c_j_cs_repn(S, TypeRepnFor, 2, MaybeCJCsRepn, !U),
            add_string(")", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(1),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string("foreign_type_repn(\n", S, !U),
            mercury_format_c_j_cs_repn(S, TypeRepnFor, 2, MaybeCJCsRepn, !U),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string(")", S, !U)
        )
    ),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_du_type_repn(S::in, type_repn_for::in,
    int::in, tvarset::in, du_repn::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_du_type_repn(S, TypeRepnFor, Indent, TVarSet, DuRepn, !U) :-
    (
        DuRepn = dur_direct_dummy(DummyRepn),
        mercury_format_du_direct_dummy(S, TypeRepnFor, Indent,
            DummyRepn, !U)
    ;
        DuRepn = dur_enum(EnumRepn),
        mercury_format_du_enum(S, TypeRepnFor, Indent, EnumRepn, !U)
    ;
        DuRepn = dur_notag(NotagRepn),
        mercury_format_du_notag(S, TypeRepnFor, Indent, TVarSet,
            NotagRepn, !U)
    ;
        DuRepn = dur_gen_only_functor(OnlyFunctorRepn),
        mercury_format_du_only_functor(S, TypeRepnFor, Indent, TVarSet,
            OnlyFunctorRepn, !U)
    ;
        DuRepn = dur_gen_more_functors(MoreFunctorsRepn),
        mercury_format_du_more_functors(S, TypeRepnFor, Indent, TVarSet,
            MoreFunctorsRepn, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_du_direct_dummy(S::in, type_repn_for::in,
    int::in, direct_dummy_repn::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_du_direct_dummy(S, TypeRepnFor, Indent, DummyRepn, !U) :-
    DummyRepn = direct_dummy_repn(FunctorName, MaybeCJCsRepnOrEnum),
    (
        TypeRepnFor = type_repn_for_machines,
        add_string("direct_dummy(", S, !U),
        mercury_format_functor_name(FunctorName, S, !U),
        add_string(", ", S, !U),
        mercury_format_c_j_cs_repn_or_enum(S, TypeRepnFor, Indent + 1,
            MaybeCJCsRepnOrEnum, !U),
        add_string(")", S, !U)
    ;
        TypeRepnFor = type_repn_for_humans,
        I = indent(Indent),
        add_string("\n", S, !U),
        add_string(I, S, !U),
        add_string("direct_dummy(", S, !U),
        mercury_format_functor_name(FunctorName, S, !U),
        add_string(",\n", S, !U),
        mercury_format_c_j_cs_repn_or_enum(S, TypeRepnFor, Indent + 1,
            MaybeCJCsRepnOrEnum, !U),
        add_string("\n", S, !U),
        add_string(I, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_du_enum(S::in, type_repn_for::in,
    int::in, enum_repn::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_du_enum(S, TypeRepnFor, Indent, EnumRepn, !U) :-
    EnumRepn = enum_repn(Functor1, Functor2, OtherFunctors,
        MaybeCJCsRepnOrEnum),
    (
        TypeRepnFor = type_repn_for_machines,
        add_string("enum(", S, !U),
        mercury_format_functor_name(Functor1, S, !U),
        add_string(", ", S, !U),
        mercury_format_functor_name(Functor2, S, !U),
        add_string(", ", S, !U),
        (
            OtherFunctors = [],
            add_string("[]", S, !U)
        ;
            OtherFunctors = [HeadFunctor | TailFunctors],
            add_string("[", S, !U),
            add_list(mercury_format_functor_name, ", ",
                [HeadFunctor | TailFunctors], S, !U),
            add_string("]", S, !U)
        ),
        add_string(", ", S, !U),
        mercury_format_c_j_cs_repn_or_enum(S, TypeRepnFor, 0,
            MaybeCJCsRepnOrEnum, !U),
        add_string(")", S, !U)
    ;
        TypeRepnFor = type_repn_for_humans,
        Indent1 = Indent + 1,
        I = indent(Indent),
        NlI1 = nl_indent(Indent1),
        add_string("\n", S, !U),
        add_string(I, S, !U),
        add_string("enum(", S, !U),
        mercury_format_one_functor_name(NlI1, "", Functor1, S, !U),
        add_string(",", S, !U),
        mercury_format_one_functor_name(NlI1, "", Functor2, S, !U),
        add_string(",", S, !U),
        mercury_format_list_for_humans(Indent1,
            mercury_format_one_functor_name(nl_indent(Indent + 2), ""),
            OtherFunctors, S, !U),
        add_string(",\n", S, !U),
        mercury_format_c_j_cs_repn_or_enum(S, TypeRepnFor, Indent1,
            MaybeCJCsRepnOrEnum, !U),
        add_string("\n", S, !U),
        add_string(I, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_du_notag(S::in, type_repn_for::in, int::in,
    tvarset::in, notag_repn::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_du_notag(S, TypeRepnFor, Indent, TVarSet,
        NotagRepn, !U) :-
    NotagRepn = notag_repn(FunctorName, ArgType, MaybeCJCsRepn),
    (
        TypeRepnFor = type_repn_for_machines,
        add_string("notag(", S, !U),
        mercury_format_functor_name(FunctorName, S, !U),
        add_string(", ", S, !U),
        mercury_format_type(TVarSet, print_num_only, ArgType, S, !U),
        add_string(", ", S, !U),
        mercury_format_c_j_cs_repn(S, TypeRepnFor, 0, MaybeCJCsRepn, !U),
        add_string(")", S, !U)
    ;
        TypeRepnFor = type_repn_for_humans,
        I = indent(Indent),
        I1 = indent(Indent + 1),
        add_string("\n", S, !U),
        add_string(I, S, !U),
        add_string("notag(\n", S, !U),
        add_string(I1, S, !U),
        mercury_format_functor_name(FunctorName, S, !U),
        add_string(",\n", S, !U),
        add_string(I1, S, !U),
        mercury_format_type(TVarSet, print_num_only, ArgType, S, !U),
        add_string(",\n", S, !U),
        mercury_format_c_j_cs_repn(S, TypeRepnFor, Indent + 1,
            MaybeCJCsRepn, !U),
        add_string("\n", S, !U),
        add_string(I, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_du_only_functor(S::in, type_repn_for::in,
    int::in, tvarset::in, gen_du_only_functor_repn::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_du_only_functor(S, TypeRepnFor, Indent, TVarSet,
        OnlyFunctorRepn, !U) :-
    OnlyFunctorRepn = gen_du_only_functor_repn(FunctorName, ArgTypes, CRepns,
        MaybeCJCsRepn),
    (
        TypeRepnFor = type_repn_for_machines,
        add_string("gen_du_only_functor(", S, !U),
        mercury_format_functor_name(FunctorName, S, !U),
        add_string(", [", S, !U),
        add_list(mercury_format_type(TVarSet, print_num_only),
            ", ", ArgTypes, S, !U),
        add_string("],", S, !U),
        mercury_format_c_repns(TypeRepnFor, 0,
            mercury_format_only_nonconstant_repn(TypeRepnFor, 0),
            CRepns, S, !U),
        add_string(", ", S, !U),
        mercury_format_c_j_cs_repn(S, TypeRepnFor, 0, MaybeCJCsRepn, !U),
        add_string(")", S, !U)
    ;
        TypeRepnFor = type_repn_for_humans,
        Indent1 = Indent + 1,
        Indent2 = Indent + 2,
        I = indent(Indent),
        I1 = indent(Indent1),
        NlI2 = nl_indent(Indent2),
        add_string("\n", S, !U),
        add_string(I, S, !U),
        add_string("gen_du_only_functor(\n", S, !U),
        add_string(I1, S, !U),
        mercury_format_functor_name(FunctorName, S, !U),
        add_string(",", S, !U),
        mercury_format_list_for_humans(Indent1,
            mercury_format_one_type(NlI2, "", TVarSet), ArgTypes, S, !U),
        add_string(",", S, !U),
        mercury_format_c_repns(TypeRepnFor, Indent1,
            mercury_format_only_nonconstant_repn(TypeRepnFor, Indent2),
            CRepns, S, !U),
        add_string(",\n", S, !U),
        mercury_format_c_j_cs_repn(S, TypeRepnFor, Indent1,
            MaybeCJCsRepn, !U),
        add_string("\n", S, !U),
        add_string(I, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_du_more_functors(S::in, type_repn_for::in,
    int::in, tvarset::in, gen_du_more_functors_repn::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_du_more_functors(S, TypeRepnFor, Indent, TVarSet,
        MoreFunctorsRepn, !U) :-
    MoreFunctorsRepn = gen_du_more_functors_repn(Functor1, Functor2,
        OtherFunctors, MaybeCJCsRepn),
    (
        TypeRepnFor = type_repn_for_machines,
        add_string("gen_du_more_functors(", S, !U),
        mercury_format_gen_du_functor_repn(TypeRepnFor, 0, TVarSet, Functor1,
            S, !U),
        add_string(", ", S, !U),
        mercury_format_gen_du_functor_repn(TypeRepnFor, 0, TVarSet, Functor2,
            S, !U),
        add_string(", ", S, !U),
        (
            OtherFunctors = [],
            add_string("[]", S, !U)
        ;
            OtherFunctors = [HeadFunctor | TailFunctors],
            add_string("[", S, !U),
            add_list(
                mercury_format_gen_du_functor_repn(TypeRepnFor,
                    0, TVarSet),
                ", ", [HeadFunctor | TailFunctors], S, !U),
            add_string("]", S, !U)
        ),
        add_string(", ", S, !U),
        mercury_format_c_j_cs_repn(S, TypeRepnFor, 0, MaybeCJCsRepn, !U),
        add_string(")", S, !U)
    ;
        TypeRepnFor = type_repn_for_humans,
        Indent1 = Indent + 1,
        I = indent(Indent),
        add_string("\n", S, !U),
        add_string(I, S, !U),
        add_string("gen_du_more_functors(", S, !U),
        mercury_format_gen_du_functor_repn(TypeRepnFor, Indent1, TVarSet,
            Functor1, S, !U),
        add_string(",", S, !U),
        mercury_format_gen_du_functor_repn(TypeRepnFor, Indent1, TVarSet,
            Functor2, S, !U),
        add_string(",", S, !U),
        mercury_format_list_for_humans(Indent1,
            mercury_format_gen_du_functor_repn(TypeRepnFor, Indent + 2,
                TVarSet),
            OtherFunctors, S, !U),
        add_string(",\n", S, !U),
        mercury_format_c_j_cs_repn(S, TypeRepnFor, Indent1, MaybeCJCsRepn, !U),
        add_string("\n", S, !U),
        add_string(I, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_gen_du_functor_repn(type_repn_for::in, int::in,
    tvarset::in, gen_du_functor_repn::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_gen_du_functor_repn(TypeRepnFor, Indent, TVarSet, FunctorRepn,
        S, !U) :-
    (
        FunctorRepn = gen_du_constant_functor_repn(FunctorName, ConstantRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            add_string("constant_functor(", S, !U),
            mercury_format_functor_name(FunctorName, S, !U),
            add_string(", ", S, !U),
            mercury_format_c_repns(TypeRepnFor, 0,
                mercury_format_constant_repn(TypeRepnFor, 0),
                ConstantRepns, S, !U),
            add_string(")", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string("constant_functor(", S, !U),
            mercury_format_functor_name(FunctorName, S, !U),
            add_string(",", S, !U),
            Indent1 = Indent + 1,
            mercury_format_c_repns(TypeRepnFor, Indent1,
                mercury_format_constant_repn(TypeRepnFor, Indent + 2),
                ConstantRepns, S, !U),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string(")", S, !U)
        )
    ;
        FunctorRepn = gen_du_nonconstant_functor_repn(FunctorName,
            ArgTypes, NonConstantRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            add_string("nonconstant_functor(", S, !U),
            mercury_format_functor_name(FunctorName, S, !U),
            add_string(", [", S, !U),
            add_list(mercury_format_type(TVarSet, print_num_only),
                ", ", ArgTypes, S, !U),
            add_string("], ", S, !U),
            mercury_format_c_repns(TypeRepnFor, 0,
                mercury_format_more_nonconstant_repn(TypeRepnFor, 0),
                NonConstantRepns, S, !U),
            add_string(")", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string("nonconstant_functor(", S, !U),
            mercury_format_functor_name(FunctorName, S, !U),
            add_string(",", S, !U),
            Indent1 = Indent + 1,
            Indent2 = Indent + 2,
            NlI2 = nl_indent(Indent2),
            mercury_format_list_for_humans(Indent1,
                mercury_format_one_type(NlI2, "", TVarSet),
                ArgTypes, S, !U),
            add_string(",", S, !U),
            mercury_format_c_repns(TypeRepnFor, Indent1,
                mercury_format_more_nonconstant_repn(TypeRepnFor, Indent2),
                NonConstantRepns, S, !U),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string(")", S, !U)
        )
    ).

%---------------------%

:- pred mercury_format_constant_repn(type_repn_for::in, int::in,
    constant_repn::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_constant_repn(TypeRepnFor, Indent, ConstantRepn, S, !U) :-
    NlI = nl_indent_for_humans_space_for_machines(TypeRepnFor, Indent),
    ConstantRepn = constant_repn(Sectag, SectagWordOrSize),
    SectagWordOrSizeStr =
        local_sectag_word_or_size_to_string(SectagWordOrSize),
    string.format("%sconstant(%u, %s)",
        [s(NlI), u(Sectag), s(SectagWordOrSizeStr)], Str),
    add_string(Str, S, !U).

:- func local_sectag_word_or_size_to_string(lsectag_word_or_size) = string.

local_sectag_word_or_size_to_string(SectagWordOrSize) = Str :-
    (
        SectagWordOrSize = lsectag_rest_of_word(NumBits),
        string.format("lst_rest(%u)", [u8(NumBits)], Str)
    ;
        SectagWordOrSize = lsectag_part_of_word(NumBits),
        string.format("lst_part(%u)", [u8(NumBits)], Str)
    ).

%---------------------%

:- pred mercury_format_only_nonconstant_repn(type_repn_for::in, int::in,
    only_nonconstant_repn::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_only_nonconstant_repn(TypeRepnFor, Indent, NonConstantRepn,
        S, !U) :-
    (
        NonConstantRepn = oncr_local_cell(LocalRepn),
        LocalRepn = only_nonconstant_local_cell_repn(OoMLocalArgRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            add_string("only_local_cell([", S, !U),
            OoMLocalArgRepns =
                one_or_more(HeadLocalArgRepn, TailLocalArgRepns),
            add_list(mercury_format_local_arg_repn(TypeRepnFor, 0),
                ", ", [HeadLocalArgRepn | TailLocalArgRepns], S, !U),
            add_string("])", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string("only_local_cell([", S, !U),
            OoMLocalArgRepns =
                one_or_more(HeadLocalArgRepn, TailLocalArgRepns),
            mercury_format_list_for_humans(Indent + 1,
                mercury_format_local_arg_repn(TypeRepnFor, Indent + 2),
                [HeadLocalArgRepn | TailLocalArgRepns], S, !U),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string(")", S, !U)
        )
    ;
        NonConstantRepn = oncr_remote_cell(RemoteRepn),
        RemoteRepn = only_nonconstant_remote_cell_repn(OoMRemoteArgRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            add_string("only_remote_cell([", S, !U),
            OoMRemoteArgRepns =
                one_or_more(HeadRemoteArgRepn, TailRemoteArgRepns),
            add_list(mercury_format_remote_arg_repn(TypeRepnFor, 0),
                ", ", [HeadRemoteArgRepn | TailRemoteArgRepns], S, !U),
            add_string("])", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string("only_remote_cell([", S, !U),
            OoMRemoteArgRepns =
                one_or_more(HeadRemoteArgRepn, TailRemoteArgRepns),
            mercury_format_list_for_humans(Indent + 1,
                mercury_format_remote_arg_repn(TypeRepnFor, Indent + 2),
                [HeadRemoteArgRepn | TailRemoteArgRepns], S, !U),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string(")", S, !U)
        )
    ).

:- pred mercury_format_more_nonconstant_repn(type_repn_for::in, int::in,
    more_nonconstant_repn::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_more_nonconstant_repn(TypeRepnFor, Indent, NonConstantRepn,
        S, !U) :-
    (
        NonConstantRepn = mncr_local_cell(LocalRepn),
        LocalRepn = more_nonconstant_local_cell_repn(CellLocalSectag,
            OoMLocalArgRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            add_string("local_cell(", S, !U),
            mercury_format_cell_local_sectag(CellLocalSectag, S, !U),
            add_string(", [", S, !U),
            OoMLocalArgRepns =
                one_or_more(HeadLocalArgRepn, TailLocalArgRepns),
            add_list(mercury_format_local_arg_repn(TypeRepnFor, 0),
                ", ", [HeadLocalArgRepn | TailLocalArgRepns], S, !U),
            add_string("])", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string("local_cell(", S, !U),
            mercury_format_cell_local_sectag(CellLocalSectag, S, !U),
            add_string(",", S, !U),
            OoMLocalArgRepns =
                one_or_more(HeadLocalArgRepn, TailLocalArgRepns),
            mercury_format_list_for_humans(Indent + 1,
                mercury_format_local_arg_repn(TypeRepnFor, Indent + 2),
                [HeadLocalArgRepn | TailLocalArgRepns], S, !U),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string(")", S, !U)
        )
    ;
        NonConstantRepn = mncr_remote_cell(RemoteRepn),
        RemoteRepn = more_nonconstant_remote_cell_repn(Ptag, CellRemoteSectag,
            OoMRemoteArgRepns),
        Ptag = ptag(PtagUint8),
        PtagUint = uint8.cast_to_uint(PtagUint8),
        (
            TypeRepnFor = type_repn_for_machines,
            add_string("remote_cell(", S, !U),
            add_uint(PtagUint, S, !U),
            add_string(", ", S, !U),
            mercury_format_cell_remote_sectag(CellRemoteSectag, S, !U),
            add_string(", [", S, !U),
            OoMRemoteArgRepns =
                one_or_more(HeadRemoteArgRepn, TailRemoteArgRepns),
            add_list(mercury_format_remote_arg_repn(TypeRepnFor, 0),
                ", ", [HeadRemoteArgRepn | TailRemoteArgRepns], S, !U),
            add_string("])", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string("remote_cell(", S, !U),
            add_uint(PtagUint, S, !U),
            add_string(", ", S, !U),
            mercury_format_cell_remote_sectag(CellRemoteSectag, S, !U),
            add_string(",", S, !U),
            OoMRemoteArgRepns =
                one_or_more(HeadRemoteArgRepn, TailRemoteArgRepns),
            mercury_format_list_for_humans(Indent + 1,
                mercury_format_remote_arg_repn(TypeRepnFor, Indent + 2),
                [HeadRemoteArgRepn | TailRemoteArgRepns], S, !U),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string(")", S, !U)
        )
    ;
        NonConstantRepn = mncr_direct_arg(Ptag),
        Ptag = ptag(PtagUint8),
        PtagUint = uint8.cast_to_uint(PtagUint8),
        (
            TypeRepnFor = type_repn_for_machines,
            string.format("direct_arg(%u)", [u(PtagUint)], Str)
        ;
            TypeRepnFor = type_repn_for_humans,
            NlI = nl_indent(Indent),
            string.format("%sdirect_arg(%u)", [s(NlI), u(PtagUint)], Str)
        ),
        add_string(Str, S, !U)
    ).

%---------------------%

:- pred mercury_format_cell_local_sectag(cell_local_sectag::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_cell_local_sectag(CellLocalSectag, S, !U) :-
    CellLocalSectag = cell_local_sectag(Sectag, SectagNumBits),
    string.format("local_sectag(%u, %u)", [u(Sectag), u8(SectagNumBits)], Str),
    add_string(Str, S, !U).

:- pred mercury_format_cell_remote_sectag(cell_remote_sectag::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_cell_remote_sectag(CellLocalSectag, S, !U) :-
    (
        CellLocalSectag = cell_remote_no_sectag,
        Str = "remote_no_sectag"
    ;
        CellLocalSectag = cell_remote_sectag(Sectag, SectagWordOrSize),
        SectagWordOrSizeStr =
            remote_sectag_word_or_size_to_string(SectagWordOrSize),
        string.format("remote_sectag(%u, %s)",
            [u(Sectag), s(SectagWordOrSizeStr)], Str)
    ),
    add_string(Str, S, !U).

:- func remote_sectag_word_or_size_to_string(rsectag_word_or_size) = string.

remote_sectag_word_or_size_to_string(SectagWordOrSize) = Str :-
    (
        SectagWordOrSize = rsectag_full_word,
        Str = "rst_full"
    ;
        SectagWordOrSize = rsectag_part_of_word(NumBits),
        string.format("rst_part(%u)", [u8(NumBits)], Str)
    ).

%---------------------%

:- pred mercury_format_local_arg_repn(type_repn_for::in, int::in,
    local_arg_repn::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_local_arg_repn(TypeRepnFor, Indent, LocalArgRepn, S, !U) :-
    NlI = nl_indent_for_humans(TypeRepnFor, Indent),
    (
        LocalArgRepn = local_partial(Shift, FillKindSize),
        string.format("%slocal_partial(%u, %s)",
            [s(NlI), u(Shift), s(fill_kind_size_to_string(FillKindSize))],
            Str)
    ;
        LocalArgRepn = local_none,
        string.format("%slocal_none", [s(NlI)], Str)
    ),
    add_string(Str, S, !U).

:- pred mercury_format_remote_arg_repn(type_repn_for::in, int::in,
    remote_arg_repn::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_remote_arg_repn(TypeRepnFor, Indent, RemoteArgRepn, S, !U) :-
    NlI = nl_indent_for_humans(TypeRepnFor, Indent),
    (
        RemoteArgRepn = remote_full(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        string.format("%sfull(%d, %d)",
            [s(NlI), i(ArgOnlyOffsetInt), i(CellOffsetInt)], Str)
    ;
        RemoteArgRepn = remote_double(ArgOnlyOffset, CellOffset, DoubleKind),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        double_word_kind_string(DoubleKind, DKStr),
        string.format("%sdouble(%d, %d, %s)",
            [s(NlI), i(ArgOnlyOffsetInt), i(CellOffsetInt), s(DKStr)], Str)
    ;
        (
            RemoteArgRepn = remote_partial_first(ArgOnlyOffset, CellOffset,
                Shift, FillKindSize),
            FirstOrShifted = "first"
        ;
            RemoteArgRepn = remote_partial_shifted(ArgOnlyOffset, CellOffset,
                Shift, FillKindSize),
            FirstOrShifted = "shifted"
        ),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        string.format("%spartial_%s(%d, %d, %u, %s)",
            [s(NlI), s(FirstOrShifted), i(ArgOnlyOffsetInt), i(CellOffsetInt),
            u8(Shift), s(fill_kind_size_to_string(FillKindSize))], Str)
    ;
        RemoteArgRepn = remote_none_shifted(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        string.format("%snone_shifted(%d, %d)",
            [s(NlI), i(ArgOnlyOffsetInt), i(CellOffsetInt)], Str)
    ;
        RemoteArgRepn = remote_none_nowhere,
        string.format("%snone_nowhere", [s(NlI)], Str)
    ),
    add_string(Str, S, !U).

%---------------------------------------------------------------------------%

fill_kind_size_to_string(FillKindSize) = Str :-
    (
        FillKindSize = fk_enum(NumBits),
        string.format("enum(%u)", [u(NumBits)], Str)
    ;
        ( FillKindSize = fk_int8,   Str = "int8"
        ; FillKindSize = fk_int16,  Str = "int16"
        ; FillKindSize = fk_int32,  Str = "int32"
        ; FillKindSize = fk_uint8,  Str = "uint8"
        ; FillKindSize = fk_uint16, Str = "uint16"
        ; FillKindSize = fk_uint32, Str = "uint32"
        ; FillKindSize = fk_char21, Str = "char21"
        )
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_c_j_cs_repn_or_enum(S::in, type_repn_for::in,
    int::in, c_j_cs_enum_repn::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_c_j_cs_repn_or_enum(S, TypeRepnFor, Indent,
        MaybeCJCsRepnOrEnum, !U) :-
    MaybeCJCsRepnOrEnum = c_java_csharp(MaybeRepnOrEnumC, MaybeRepnOrEnumJava,
        MaybeRepnOrEnumCsharp),
    Indent1 = Indent + 1,
    I = indent(Indent),
    ( if
        MaybeRepnOrEnumC = no,
        MaybeRepnOrEnumJava = no,
        MaybeRepnOrEnumCsharp = no
    then
        (
            TypeRepnFor = type_repn_for_machines,
            add_string(" no_c_j_cs", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            add_string(I, S, !U),
            add_string("no_c_j_cs", S, !U)
        )
    else
        (
            TypeRepnFor = type_repn_for_machines,
            add_string(" c_j_cs(", S, !U),
            mercury_format_maybe_enum_foreign_repn(S, TypeRepnFor,
                Indent1, MaybeRepnOrEnumC, !U),
            add_string(", ", S, !U),
            mercury_format_maybe_enum_foreign_repn(S, TypeRepnFor,
                Indent1, MaybeRepnOrEnumJava, !U),
            add_string(", ", S, !U),
            mercury_format_maybe_enum_foreign_repn(S, TypeRepnFor,
                Indent1, MaybeRepnOrEnumCsharp, !U),
            add_string(")", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string("c_j_cs(\n", S, !U),
            mercury_format_maybe_enum_foreign_repn(S, TypeRepnFor,
                Indent1, MaybeRepnOrEnumC, !U),
            add_string(",\n", S, !U),
            mercury_format_maybe_enum_foreign_repn(S, TypeRepnFor,
                Indent1, MaybeRepnOrEnumJava, !U),
            add_string(",\n", S, !U),
            mercury_format_maybe_enum_foreign_repn(S, TypeRepnFor,
                Indent1, MaybeRepnOrEnumCsharp, !U),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string(")", S, !U)
        )
    ).

:- pred mercury_format_c_j_cs_repn(S::in, type_repn_for::in, int::in,
    c_j_cs_repn::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_c_j_cs_repn(S, TypeRepnFor, Indent, MaybeCJCsRepn, !U) :-
    MaybeCJCsRepn = c_java_csharp(MaybeRepnC, MaybeRepnJava, MaybeRepnCsharp),
    ( if
        MaybeRepnC = no,
        MaybeRepnJava = no,
        MaybeRepnCsharp = no
    then
        (
            TypeRepnFor = type_repn_for_machines,
            add_string(" no_c_j_cs", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            add_string(I, S, !U),
            add_string("no_c_j_cs", S, !U)
        )
    else
        Indent1 = Indent + 1,
        (
            TypeRepnFor = type_repn_for_machines,
            add_string(" c_j_cs(", S, !U),
            mercury_format_maybe_foreign_type_repn(S, TypeRepnFor,
                Indent1, MaybeRepnC, !U),
            add_string(", ", S, !U),
            mercury_format_maybe_foreign_type_repn(S, TypeRepnFor,
                Indent1, MaybeRepnJava, !U),
            add_string(", ", S, !U),
            mercury_format_maybe_foreign_type_repn(S, TypeRepnFor,
                Indent1, MaybeRepnCsharp, !U),
            add_string(")", S, !U)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            add_string(I, S, !U),
            add_string("c_j_cs(\n", S, !U),
            mercury_format_maybe_foreign_type_repn(S, TypeRepnFor,
                Indent1, MaybeRepnC, !U),
            add_string(",\n", S, !U),
            mercury_format_maybe_foreign_type_repn(S, TypeRepnFor,
                Indent1, MaybeRepnJava, !U),
            add_string(",\n", S, !U),
            mercury_format_maybe_foreign_type_repn(S, TypeRepnFor,
                Indent1, MaybeRepnCsharp, !U),
            add_string("\n", S, !U),
            add_string(I, S, !U),
            add_string(")", S, !U)
        )
    ).

%---------------------%

:- pred mercury_format_maybe_enum_foreign_repn(S::in,
    type_repn_for::in, int::in, maybe(enum_foreign_repn)::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_maybe_enum_foreign_repn(S, TypeRepnFor, Indent,
        MaybeTypeRepnOrEnum, !U) :-
    (
        MaybeTypeRepnOrEnum = no,
        I = indent_for_humans(TypeRepnFor, Indent),
        add_string(I, S, !U),
        add_string("no_foreign", S, !U)
    ;
        MaybeTypeRepnOrEnum = yes(enum_foreign_enum(EnumRepn)),
        % Top level functor will be "foreign_enum".
        mercury_format_foreign_enum_repn(S, TypeRepnFor, Indent, EnumRepn, !U)
    ;
        MaybeTypeRepnOrEnum = yes(enum_foreign_type(TypeRepn)),
        % Top level functor will be "foreign_type".
        mercury_format_foreign_type_repn(S, TypeRepnFor, Indent, TypeRepn, !U)
    ).

:- pred mercury_format_maybe_foreign_type_repn(S::in, type_repn_for::in,
    int::in, maybe(foreign_type_repn)::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_maybe_foreign_type_repn(S, TypeRepnFor, Indent,
        MaybeTypeRepn, !U) :-
    (
        MaybeTypeRepn = no,
        I = indent_for_humans(TypeRepnFor, Indent),
        add_string(I, S, !U),
        add_string("no_foreign", S, !U)
    ;
        MaybeTypeRepn = yes(TypeRepn),
        % Top level functor will be "foreign_type".
        mercury_format_foreign_type_repn(S, TypeRepnFor, Indent, TypeRepn, !U)
    ).

%---------------------%

:- pred mercury_format_foreign_enum_repn(S::in, type_repn_for::in,
    int::in, one_or_more(string)::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_foreign_enum_repn(S, TypeRepnFor, Indent, EnumRepn, !U) :-
    EnumRepn = one_or_more(HeadEnum, TailEnums),
    (
        TypeRepnFor = type_repn_for_machines,
        add_string("foreign_enum([", S, !U),
        add_list(mercury_format_functor_name, ", ",
            [HeadEnum | TailEnums], S, !U),
        add_string("])", S, !U)
    ;
        TypeRepnFor = type_repn_for_humans,
        I = indent(Indent),
        add_string(I, S, !U),
        add_string("foreign_enum(", S, !U),
        mercury_format_list_for_humans(Indent + 1,
            mercury_format_one_functor_name(nl_indent(Indent + 2), ""),
            [HeadEnum | TailEnums], S, !U),
        add_string("\n", S, !U),
        add_string(I, S, !U),
        add_string(")", S, !U)
    ).

%---------------------%

:- pred mercury_format_foreign_type_repn(S::in, type_repn_for::in,
    int::in, foreign_type_repn::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_foreign_type_repn(S, TypeRepnFor, Indent, TypeRepn, !U) :-
    TypeRepn = foreign_type_repn(ForeignTypeName, ForeignTypeAssertions),
    I = indent_for_humans(TypeRepnFor, Indent),
    ForeignTypeAssertions = foreign_type_assertions(Assertions),
    set.to_sorted_list(Assertions, AssertionsList),
    string.format("%sforeign_type(\"%s\",",
        [s(I), s(ForeignTypeName)], InitStr),
    add_string(InitStr, S, !U),
    AssertionStrs =
        list.map(foreign_type_assertion_to_string, AssertionsList),
    AssertionsStr = string.join_list(", ", AssertionStrs),
    (
        TypeRepnFor = type_repn_for_machines,
        string.format(" [%s])", [s(AssertionsStr)], FinalStr)
    ;
        TypeRepnFor = type_repn_for_humans,
        I1 = indent(Indent + 1),
        string.format("\n%s[%s]\n%s)", [s(I1), s(AssertionsStr), s(I)],
            FinalStr)
    ),
    add_string(FinalStr, S, !U).

foreign_type_assertion_to_string(Assertion) = Str :-
    (
        Assertion = foreign_type_can_pass_as_mercury_type,
        Str = "can_pass_as_mercury_type"
    ;
        Assertion = foreign_type_stable,
        Str = "stable"
    ;
        Assertion = foreign_type_word_aligned_pointer,
        Str = "word_aligned_pointer"
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_functor_name(string::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_functor_name(FunctorName, S, !U) :-
    term_io.format_quoted_string(S, FunctorName, !U).

    % Output one functor's name in a list of functor names.
    %
:- pred mercury_format_one_functor_name(string::in, string::in, string::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_one_functor_name(Prefix, Suffix, FunctorName, S, !U) :-
    add_string(Prefix, S, !U),
    mercury_format_functor_name(FunctorName, S, !U),
    add_string(Suffix, S, !U).

%---------------------------------------------------------------------------%

    % Output one type in a list of types.
    %
:- pred mercury_format_one_type(string::in, string::in, tvarset::in,
    mer_type::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_one_type(Prefix, Suffix, TVarSet, Type, S, !U) :-
    add_string(Prefix, S, !U),
    mercury_format_type(TVarSet, print_num_only, Type, S, !U),
    add_string(Suffix, S, !U).

%---------------------------------------------------------------------------%

:- func nl_indent_for_humans_space_for_machines(type_repn_for, int) = string.

nl_indent_for_humans_space_for_machines(TypeRepnFor, Indent) = Str :-
    (
        TypeRepnFor = type_repn_for_machines,
        Str = " "
    ;
        TypeRepnFor = type_repn_for_humans,
        Str = nl_indent(Indent)
    ).

:- func nl_indent_for_humans(type_repn_for, int) = string.

nl_indent_for_humans(TypeRepnFor, Indent) = Str :-
    (
        TypeRepnFor = type_repn_for_machines,
        Str = ""
    ;
        TypeRepnFor = type_repn_for_humans,
        Str = nl_indent(Indent)
    ).

:- func indent_for_humans(type_repn_for, int) = string.

indent_for_humans(TypeRepnFor, Indent) = Str :-
    (
        TypeRepnFor = type_repn_for_machines,
        Str = ""
    ;
        TypeRepnFor = type_repn_for_humans,
        Str = indent(Indent)
    ).

:- func nl_indent(int) = string.

nl_indent(N) = Str :-
    ( if
        ( N = 0, StrPrime = "\n"
        ; N = 1, StrPrime = "\n    "
        ; N = 2, StrPrime = "\n        "
        ; N = 3, StrPrime = "\n            "
        ; N = 4, StrPrime = "\n                "
        ; N = 5, StrPrime = "\n                    "
        ; N = 6, StrPrime = "\n                        "
        )
    then
        Str = StrPrime
    else
        Str = "\n                        " ++ indent(N - 6)
    ).

:- func indent(int) = string.

indent(N) = Str :-
    ( if
        ( N = 0, StrPrime = ""
        ; N = 1, StrPrime = "    "
        ; N = 2, StrPrime = "        "
        ; N = 3, StrPrime = "            "
        ; N = 4, StrPrime = "                "
        ; N = 5, StrPrime = "                    "
        ; N = 6, StrPrime = "                        "
        )
    then
        Str = StrPrime
    else
        Str = "                        " ++ indent(N - 6)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_list_for_humans(int::in,
    pred(T, S, U, U)::in(pred(in, in, di, uo) is det),
    list(T)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_list_for_humans(Indent, WriteX, Xs, S, !U) :-
    NlI = nl_indent(Indent),
    (
        Xs = [],
        add_string(NlI, S, !U),
        add_string("[]", S, !U)
    ;
        Xs = [HeadX | TailXs],
        add_string(NlI, S, !U),
        add_string("[", S, !U),
        mercury_format_list_for_humans_loop(WriteX, HeadX, TailXs, S, !U),
        add_string(NlI, S, !U),
        add_string("]", S, !U)
    ).

:- pred mercury_format_list_for_humans_loop(
    pred(T, S, U, U)::in(pred(in, in, di, uo) is det),
    T::in, list(T)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_list_for_humans_loop(WriteX, X, Xs, S, !U) :-
    WriteX(X, S, !U),
    (
        Xs = []
    ;
        Xs = [HeadX | TailXs],
        add_string(",", S, !U),
        mercury_format_list_for_humans_loop(WriteX, HeadX, TailXs, S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_c_repns(type_repn_for::in, int::in,
    pred(T, S, U, U)::in(pred(in, in, di, uo) is det),
    c_repns(T)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_c_repns(TypeRepnFor, Indent, WriteX, CRepns, S, !U) :-
    NlI = nl_indent_for_humans_space_for_machines(TypeRepnFor, Indent),
    (
        CRepns = c_repns_same(X1),
        add_string(NlI, S, !U),
        add_string("c_repns_same(", S, !U),
        WriteX(X1, S, !U),
        add_string(NlI, S, !U),
        add_string(")", S, !U)
    ;
        CRepns = c_repns_64_32(X1, X2),
        add_string(NlI, S, !U),
        add_string("c_repns_64_32(", S, !U),
        WriteX(X1, S, !U),
        add_string(", ", S, !U),
        WriteX(X2, S, !U),
        add_string(NlI, S, !U),
        add_string(")", S, !U)
    ;
        CRepns = c_repns_all(X1, X2, X3, X4, X5, X6),
        add_string(NlI, S, !U),
        add_string("c_repns_all(", S, !U),
        WriteX(X1, S, !U),
        add_string(", ", S, !U),
        WriteX(X2, S, !U),
        add_string(", ", S, !U),
        WriteX(X3, S, !U),
        add_string(", ", S, !U),
        WriteX(X4, S, !U),
        add_string(", ", S, !U),
        WriteX(X5, S, !U),
        add_string(", ", S, !U),
        WriteX(X6, S, !U),
        add_string(NlI, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_type_repn.
%---------------------------------------------------------------------------%
