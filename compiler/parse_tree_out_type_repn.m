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

:- import_module io.

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
:- pred mercury_output_item_type_repn(merc_out_info::in,
    io.text_output_stream::in, item_type_repn_info::in, io::di, io::uo) is det.

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

mercury_output_item_type_repn(Info, Stream, ItemTypeRepn, !IO) :-
    ItemTypeRepn = item_type_repn_info(TypeCtorSymName0, TypeParams, RepnInfo,
        TVarSet, Context, _SeqNum),
    io.write_string(Stream, ":- type_representation(", !IO),
    maybe_unqualify_sym_name(Info, TypeCtorSymName0, TypeCtorSymName),
    Args = list.map((func(V) = term.variable(V, Context)), TypeParams),
    construct_qualified_term_with_context(TypeCtorSymName, Args, Context,
        TypeTerm),
    mercury_output_term_nq_vs(TVarSet, print_num_only, next_to_graphic_token,
        TypeTerm, Stream, !IO),
    io.write_string(Stream, ",", !IO),
    (
        RepnInfo = tcrepn_is_eqv_to(EqvType),
        io.write_string(Stream, " is_eqv_to(", !IO),
        mercury_output_type(TVarSet, print_num_only, EqvType, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        RepnInfo = tcrepn_is_subtype_of(SuperTypeCtor),
        io.write_string(Stream, " is_subtype_of(", !IO),
        SuperTypeCtor = type_ctor(SuperTypeCtorSymName, SuperTypeArity),
        mercury_output_sym_name(SuperTypeCtorSymName, Stream, !IO),
        io.write_string(Stream, "/", !IO),
        io.write_int(Stream, SuperTypeArity, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        RepnInfo = tcrepn_is_word_aligned_ptr,
        io.write_string(Stream, " is_word_aligned_ptr", !IO)
    ;
        RepnInfo = tcrepn_du(DuRepn),
        TypeRepnFor = get_type_repn_for(Info),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(Stream, " du_repn(", !IO),
            mercury_output_du_type_repn(Stream, TypeRepnFor, 2,
                TVarSet, DuRepn, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(1),
            io.format(Stream, "\n%sdu_repn(", [s(I)], !IO),
            mercury_output_du_type_repn(Stream, TypeRepnFor, 2,
                TVarSet, DuRepn, !IO),
            io.format(Stream, "\n%s)", [s(I)], !IO)
        )
    ;
        RepnInfo = tcrepn_foreign(MaybeCJCsRepn),
        TypeRepnFor = get_type_repn_for(Info),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(Stream, " foreign_type_repn(", !IO),
            mercury_output_c_j_cs_repn(Stream, TypeRepnFor, 2,
                MaybeCJCsRepn, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(1),
            io.format(Stream, "\n%sforeign_type_repn(\n", [s(I)], !IO),
            mercury_output_c_j_cs_repn(Stream, TypeRepnFor, 2,
                MaybeCJCsRepn, !IO),
            io.format(Stream, "\n%s)", [s(I)], !IO)
        )
    ),
    io.write_string(Stream, ").\n", !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_type_repn(io.text_output_stream::in,
    type_repn_for::in, int::in, tvarset::in, du_repn::in,
    io::di, io::uo) is det.

mercury_output_du_type_repn(Stream, TypeRepnFor, Indent, TVarSet,
        DuRepn, !IO) :-
    (
        DuRepn = dur_direct_dummy(DummyRepn),
        mercury_output_du_direct_dummy(Stream, TypeRepnFor, Indent,
            DummyRepn, !IO)
    ;
        DuRepn = dur_enum(EnumRepn),
        mercury_output_du_enum(Stream, TypeRepnFor, Indent, EnumRepn, !IO)
    ;
        DuRepn = dur_notag(NotagRepn),
        mercury_output_du_notag(Stream, TypeRepnFor, Indent, TVarSet,
            NotagRepn, !IO)
    ;
        DuRepn = dur_gen_only_functor(OnlyFunctorRepn),
        mercury_output_du_only_functor(Stream, TypeRepnFor, Indent, TVarSet,
            OnlyFunctorRepn, !IO)
    ;
        DuRepn = dur_gen_more_functors(MoreFunctorsRepn),
        mercury_output_du_more_functors(Stream, TypeRepnFor, Indent, TVarSet,
            MoreFunctorsRepn, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_direct_dummy(io.text_output_stream::in,
    type_repn_for::in, int::in, direct_dummy_repn::in, io::di, io::uo) is det.

mercury_output_du_direct_dummy(Stream, TypeRepnFor, Indent, DummyRepn, !IO) :-
    DummyRepn = direct_dummy_repn(FunctorName, MaybeCJCsRepnOrEnum),
    (
        TypeRepnFor = type_repn_for_machines,
        io.write_string(Stream, "direct_dummy(", !IO),
        mercury_output_functor_name(FunctorName, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        mercury_output_c_j_cs_repn_or_enum(Stream, TypeRepnFor, Indent + 1,
            MaybeCJCsRepnOrEnum, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        I = indent(Indent),
        io.format(Stream, "\n%sdirect_dummy(", [s(I)], !IO),
        mercury_output_functor_name(FunctorName, Stream, !IO),
        io.write_string(Stream, ",\n", !IO),
        mercury_output_c_j_cs_repn_or_enum(Stream, TypeRepnFor, Indent + 1,
            MaybeCJCsRepnOrEnum, !IO),
        io.format(Stream, "\n%s)", [s(I)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_enum(io.text_output_stream::in, type_repn_for::in,
    int::in, enum_repn::in, io::di, io::uo) is det.

mercury_output_du_enum(Stream, TypeRepnFor, Indent, EnumRepn, !IO) :-
    EnumRepn = enum_repn(Functor1, Functor2, OtherFunctors,
        MaybeCJCsRepnOrEnum),
    (
        TypeRepnFor = type_repn_for_machines,
        io.write_string(Stream, "enum(", !IO),
        mercury_output_functor_name(Functor1, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        mercury_output_functor_name(Functor2, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        (
            OtherFunctors = [],
            io.write_string(Stream, "[]", !IO)
        ;
            OtherFunctors = [HeadFunctor | TailFunctors],
            io.write_string(Stream, "[", !IO),
            write_out_list(mercury_output_functor_name, ", ",
                [HeadFunctor | TailFunctors], Stream, !IO),
            io.write_string(Stream, "]", !IO)
        ),
        io.write_string(Stream, ", ", !IO),
        mercury_output_c_j_cs_repn_or_enum(Stream, TypeRepnFor, 0,
            MaybeCJCsRepnOrEnum, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        Indent1 = Indent + 1,
        I = indent(Indent),
        NlI1 = nl_indent(Indent1),
        io.format(Stream, "\n%senum(", [s(I)], !IO),
        mercury_output_one_functor_name(NlI1, "", Functor1, Stream, !IO),
        io.write_string(Stream, ",", !IO),
        mercury_output_one_functor_name(NlI1, "", Functor2, Stream, !IO),
        io.write_string(Stream, ",", !IO),
        mercury_output_list_for_humans(Indent1,
            mercury_output_one_functor_name(nl_indent(Indent + 2), ""),
            OtherFunctors, Stream, !IO),
        io.format(Stream, ",\n", [], !IO),
        mercury_output_c_j_cs_repn_or_enum(Stream, TypeRepnFor, Indent1,
            MaybeCJCsRepnOrEnum, !IO),
        io.format(Stream, "\n%s)", [s(I)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_notag(io.text_output_stream::in, type_repn_for::in,
    int::in, tvarset::in, notag_repn::in, io::di, io::uo) is det.

mercury_output_du_notag(Stream, TypeRepnFor, Indent, TVarSet,
        NotagRepn, !IO) :-
    NotagRepn = notag_repn(FunctorName, ArgType, MaybeCJCsRepn),
    (
        TypeRepnFor = type_repn_for_machines,
        io.write_string(Stream, "notag(", !IO),
        mercury_output_functor_name(FunctorName, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        mercury_output_type(TVarSet, print_num_only, ArgType, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        mercury_output_c_j_cs_repn(Stream, TypeRepnFor, 0, MaybeCJCsRepn, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        I = indent(Indent),
        I1 = indent(Indent + 1),
        io.format(Stream, "\n%snotag(\n%s", [s(I), s(I1)], !IO),
        mercury_output_functor_name(FunctorName, Stream, !IO),
        io.format(Stream, ",\n%s", [s(I1)], !IO),
        mercury_output_type(TVarSet, print_num_only, ArgType, Stream, !IO),
        io.format(Stream, ",\n", [], !IO),
        mercury_output_c_j_cs_repn(Stream, TypeRepnFor, Indent + 1,
            MaybeCJCsRepn, !IO),
        io.format(Stream, "\n%s)", [s(I)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_only_functor(io.text_output_stream::in,
    type_repn_for::in, int::in, tvarset::in, gen_du_only_functor_repn::in,
    io::di, io::uo) is det.

mercury_output_du_only_functor(Stream, TypeRepnFor, Indent, TVarSet,
        OnlyFunctorRepn, !IO) :-
    OnlyFunctorRepn = gen_du_only_functor_repn(FunctorName, ArgTypes, CRepns,
        MaybeCJCsRepn),
    (
        TypeRepnFor = type_repn_for_machines,
        io.write_string(Stream, "gen_du_only_functor(", !IO),
        mercury_output_functor_name(FunctorName, Stream, !IO),
        io.write_string(Stream, ", [", !IO),
        write_out_list(mercury_output_type(TVarSet, print_num_only),
            ", ", ArgTypes, Stream, !IO),
        io.write_string(Stream, "],", !IO),
        mercury_output_c_repns(TypeRepnFor, 0,
            mercury_output_only_nonconstant_repn(TypeRepnFor, 0),
            CRepns, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        mercury_output_c_j_cs_repn(Stream, TypeRepnFor, 0, MaybeCJCsRepn, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        Indent1 = Indent + 1,
        Indent2 = Indent + 2,
        I = indent(Indent),
        I1 = indent(Indent1),
        NlI2 = nl_indent(Indent2),
        io.format(Stream, "\n%sgen_du_only_functor(\n%s", [s(I), s(I1)], !IO),
        mercury_output_functor_name(FunctorName, Stream, !IO),
        io.format(Stream, ",", [], !IO),
        mercury_output_list_for_humans(Indent1,
            mercury_output_one_type(NlI2, "", TVarSet), ArgTypes, Stream, !IO),
        io.format(Stream, ",", [], !IO),
        mercury_output_c_repns(TypeRepnFor, Indent1,
            mercury_output_only_nonconstant_repn(TypeRepnFor, Indent2),
            CRepns, Stream, !IO),
        io.format(Stream, ",\n", [], !IO),
        mercury_output_c_j_cs_repn(Stream, TypeRepnFor, Indent1,
            MaybeCJCsRepn, !IO),
        io.format(Stream, "\n%s)", [s(I)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_more_functors(io.text_output_stream::in,
    type_repn_for::in, int::in, tvarset::in, gen_du_more_functors_repn::in,
    io::di, io::uo) is det.

mercury_output_du_more_functors(Stream, TypeRepnFor, Indent, TVarSet,
        MoreFunctorsRepn, !IO) :-
    MoreFunctorsRepn = gen_du_more_functors_repn(Functor1, Functor2,
        OtherFunctors, MaybeCJCsRepn),
    (
        TypeRepnFor = type_repn_for_machines,
        io.write_string(Stream, "gen_du_more_functors(", !IO),
        mercury_output_gen_du_functor_repn(TypeRepnFor, 0, TVarSet, Functor1,
            Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        mercury_output_gen_du_functor_repn(TypeRepnFor, 0, TVarSet, Functor2,
            Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        (
            OtherFunctors = [],
            io.write_string(Stream, "[]", !IO)
        ;
            OtherFunctors = [HeadFunctor | TailFunctors],
            io.write_string(Stream, "[", !IO),
            write_out_list(
                mercury_output_gen_du_functor_repn(TypeRepnFor,
                    0, TVarSet),
                ", ", [HeadFunctor | TailFunctors], Stream, !IO),
            io.write_string(Stream, "]", !IO)
        ),
        io.write_string(Stream, ", ", !IO),
        mercury_output_c_j_cs_repn(Stream, TypeRepnFor, 0, MaybeCJCsRepn, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        Indent1 = Indent + 1,
        I = indent(Indent),
        io.format(Stream, "\n%sgen_du_more_functors(", [s(I)], !IO),
        mercury_output_gen_du_functor_repn(TypeRepnFor, Indent1, TVarSet,
            Functor1, Stream, !IO),
        io.format(Stream, ",", [], !IO),
        mercury_output_gen_du_functor_repn(TypeRepnFor, Indent1, TVarSet,
            Functor2, Stream, !IO),
        io.format(Stream, ",", [], !IO),
        mercury_output_list_for_humans(Indent1,
            mercury_output_gen_du_functor_repn(TypeRepnFor, Indent + 2,
                TVarSet),
            OtherFunctors, Stream, !IO),
        io.format(Stream, ",\n", [], !IO),
        mercury_output_c_j_cs_repn(Stream, TypeRepnFor, Indent1,
            MaybeCJCsRepn, !IO),
        io.format(Stream, "\n%s)", [s(I)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_gen_du_functor_repn(type_repn_for::in, int::in,
    tvarset::in, gen_du_functor_repn::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_gen_du_functor_repn(TypeRepnFor, Indent, TVarSet, FunctorRepn,
        Stream, !IO) :-
    (
        FunctorRepn = gen_du_constant_functor_repn(FunctorName, ConstantRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(Stream, "constant_functor(", !IO),
            mercury_output_functor_name(FunctorName, Stream, !IO),
            io.write_string(Stream, ", ", !IO),
            mercury_output_c_repns(TypeRepnFor, 0,
                mercury_output_constant_repn(TypeRepnFor, 0),
                ConstantRepns, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format(Stream, "\n%sconstant_functor(", [s(I)], !IO),
            mercury_output_functor_name(FunctorName, Stream, !IO),
            io.format(Stream, ",", [], !IO),
            Indent1 = Indent + 1,
            mercury_output_c_repns(TypeRepnFor, Indent1,
                mercury_output_constant_repn(TypeRepnFor, Indent + 2),
                ConstantRepns, Stream, !IO),
            io.format(Stream, "\n%s)", [s(I)], !IO)
        )
    ;
        FunctorRepn = gen_du_nonconstant_functor_repn(FunctorName,
            ArgTypes, NonConstantRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(Stream, "nonconstant_functor(", !IO),
            mercury_output_functor_name(FunctorName, Stream, !IO),
            io.write_string(Stream, ", [", !IO),
            write_out_list(mercury_output_type(TVarSet, print_num_only),
                ", ", ArgTypes, Stream, !IO),
            io.write_string(Stream, "], ", !IO),
            mercury_output_c_repns(TypeRepnFor, 0,
                mercury_output_more_nonconstant_repn(TypeRepnFor, 0),
                NonConstantRepns, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format(Stream, "\n%snonconstant_functor(", [s(I)], !IO),
            mercury_output_functor_name(FunctorName, Stream, !IO),
            io.format(Stream, ",", [], !IO),
            Indent1 = Indent + 1,
            Indent2 = Indent + 2,
            NlI2 = nl_indent(Indent2),
            mercury_output_list_for_humans(Indent1,
                mercury_output_one_type(NlI2, "", TVarSet),
                ArgTypes, Stream, !IO),
            io.format(Stream, ",", [], !IO),
            mercury_output_c_repns(TypeRepnFor, Indent1,
                mercury_output_more_nonconstant_repn(TypeRepnFor, Indent2),
                NonConstantRepns, Stream, !IO),
            io.format(Stream, "\n%s)", [s(I)], !IO)
        )
    ).

%---------------------%

:- pred mercury_output_constant_repn(type_repn_for::in, int::in,
    constant_repn::in, io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_constant_repn(TypeRepnFor, Indent, ConstantRepn, Stream, !IO) :-
    NlI = nl_indent_for_humans_space_for_machines(TypeRepnFor, Indent),
    ConstantRepn = constant_repn(Sectag, SectagWordOrSize),
    io.format(Stream, "%sconstant(%u, ", [s(NlI), u(Sectag)], !IO),
    mercury_output_local_sectag_word_or_size(SectagWordOrSize, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred mercury_output_local_sectag_word_or_size(lsectag_word_or_size::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_local_sectag_word_or_size(SectagWordOrSize, Stream, !IO) :-
    (
        SectagWordOrSize = lsectag_rest_of_word(NumBits),
        io.format(Stream, "lst_rest(%u)", [u8(NumBits)], !IO)
    ;
        SectagWordOrSize = lsectag_part_of_word(NumBits),
        io.format(Stream, "lst_part(%u)", [u8(NumBits)], !IO)
    ).

%---------------------%

:- pred mercury_output_only_nonconstant_repn(type_repn_for::in, int::in,
    only_nonconstant_repn::in, io.text_output_stream::in,io::di, io::uo)
    is det.

mercury_output_only_nonconstant_repn(TypeRepnFor, Indent, NonConstantRepn,
        Stream, !IO) :-
    (
        NonConstantRepn = oncr_local_cell(LocalRepn),
        LocalRepn = only_nonconstant_local_cell_repn(OoMLocalArgRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(Stream, "only_local_cell([", !IO),
            OoMLocalArgRepns =
                one_or_more(HeadLocalArgRepn, TailLocalArgRepns),
            write_out_list(mercury_output_local_arg_repn(TypeRepnFor, 0),
                ", ", [HeadLocalArgRepn | TailLocalArgRepns], Stream, !IO),
            io.write_string(Stream, "])", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format(Stream, "\n%sonly_local_cell([", [s(I)], !IO),
            OoMLocalArgRepns =
                one_or_more(HeadLocalArgRepn, TailLocalArgRepns),
            mercury_output_list_for_humans(Indent + 1,
                mercury_output_local_arg_repn(TypeRepnFor, Indent + 2),
                [HeadLocalArgRepn | TailLocalArgRepns], Stream, !IO),
            io.format(Stream, "\n%s)", [s(I)], !IO)
        )
    ;
        NonConstantRepn = oncr_remote_cell(RemoteRepn),
        RemoteRepn = only_nonconstant_remote_cell_repn(OoMRemoteArgRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(Stream, "only_remote_cell([", !IO),
            OoMRemoteArgRepns =
                one_or_more(HeadRemoteArgRepn, TailRemoteArgRepns),
            write_out_list(mercury_output_remote_arg_repn(TypeRepnFor, 0),
                ", ", [HeadRemoteArgRepn | TailRemoteArgRepns], Stream, !IO),
            io.write_string(Stream, "])", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.write_string(Stream, "\n%sonly_remote_cell([", !IO),
            OoMRemoteArgRepns =
                one_or_more(HeadRemoteArgRepn, TailRemoteArgRepns),
            mercury_output_list_for_humans(Indent + 1,
                mercury_output_remote_arg_repn(TypeRepnFor, Indent + 2),
                [HeadRemoteArgRepn | TailRemoteArgRepns], Stream, !IO),
            io.format(Stream, "\n%s)", [s(I)], !IO)
        )
    ).

:- pred mercury_output_more_nonconstant_repn(type_repn_for::in, int::in,
    more_nonconstant_repn::in, io.text_output_stream::in,io::di, io::uo)
    is det.

mercury_output_more_nonconstant_repn(TypeRepnFor, Indent, NonConstantRepn,
        Stream, !IO) :-
    (
        NonConstantRepn = mncr_local_cell(LocalRepn),
        LocalRepn = more_nonconstant_local_cell_repn(CellLocalSectag,
            OoMLocalArgRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(Stream, "local_cell(", !IO),
            mercury_output_cell_local_sectag(CellLocalSectag, Stream, !IO),
            io.write_string(Stream, ", [", !IO),
            OoMLocalArgRepns =
                one_or_more(HeadLocalArgRepn, TailLocalArgRepns),
            write_out_list(mercury_output_local_arg_repn(TypeRepnFor, 0),
                ", ", [HeadLocalArgRepn | TailLocalArgRepns], Stream, !IO),
            io.write_string(Stream, "])", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format(Stream, "\n%slocal_cell(", [s(I)], !IO),
            mercury_output_cell_local_sectag(CellLocalSectag, Stream, !IO),
            io.write_string(Stream, ",", !IO),
            OoMLocalArgRepns =
                one_or_more(HeadLocalArgRepn, TailLocalArgRepns),
            mercury_output_list_for_humans(Indent + 1,
                mercury_output_local_arg_repn(TypeRepnFor, Indent + 2),
                [HeadLocalArgRepn | TailLocalArgRepns], Stream, !IO),
            io.format(Stream, "\n%s)", [s(I)], !IO)
        )
    ;
        NonConstantRepn = mncr_remote_cell(RemoteRepn),
        RemoteRepn = more_nonconstant_remote_cell_repn(Ptag, CellRemoteSectag,
            OoMRemoteArgRepns),
        Ptag = ptag(PtagUint8),
        PtagUint = uint8.cast_to_uint(PtagUint8),
        (
            TypeRepnFor = type_repn_for_machines,
            io.format(Stream, "remote_cell(%u, ", [u(PtagUint)], !IO),
            mercury_output_cell_remote_sectag(CellRemoteSectag, Stream, !IO),
            io.write_string(Stream, ", [", !IO),
            OoMRemoteArgRepns =
                one_or_more(HeadRemoteArgRepn, TailRemoteArgRepns),
            write_out_list(mercury_output_remote_arg_repn(TypeRepnFor, 0),
                ", ", [HeadRemoteArgRepn | TailRemoteArgRepns], Stream, !IO),
            io.write_string(Stream, "])", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format(Stream, "\n%sremote_cell(%u, ",
                [s(I), u(PtagUint)], !IO),
            mercury_output_cell_remote_sectag(CellRemoteSectag, Stream, !IO),
            io.write_string(Stream, ",", !IO),
            OoMRemoteArgRepns =
                one_or_more(HeadRemoteArgRepn, TailRemoteArgRepns),
            mercury_output_list_for_humans(Indent + 1,
                mercury_output_remote_arg_repn(TypeRepnFor, Indent + 2),
                [HeadRemoteArgRepn | TailRemoteArgRepns], Stream, !IO),
            io.format(Stream, "\n%s)", [s(I)], !IO)
        )
    ;
        NonConstantRepn = mncr_direct_arg(Ptag),
        Ptag = ptag(PtagUint8),
        PtagUint = uint8.cast_to_uint(PtagUint8),
        (
            TypeRepnFor = type_repn_for_machines,
            io.format(Stream, "direct_arg(%u)", [u(PtagUint)], !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            NlI = nl_indent(Indent),
            io.format(Stream, "%sdirect_arg(%u)", [s(NlI), u(PtagUint)], !IO)
        )
    ).

%---------------------%

:- pred mercury_output_cell_local_sectag(cell_local_sectag::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_cell_local_sectag(CellLocalSectag, Stream, !IO) :-
    CellLocalSectag = cell_local_sectag(Sectag, SectagNumBits),
    io.format(Stream, "local_sectag(%u, %u)",
        [u(Sectag), u8(SectagNumBits)], !IO).

:- pred mercury_output_cell_remote_sectag(cell_remote_sectag::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_cell_remote_sectag(CellLocalSectag, Stream, !IO) :-
    (
        CellLocalSectag = cell_remote_no_sectag,
        io.write_string(Stream, "remote_no_sectag", !IO)
    ;
        CellLocalSectag = cell_remote_sectag(Sectag, SectagWordOrSize),
        io.format(Stream, "remote_sectag(%u, ", [u(Sectag)], !IO),
        mercury_output_remote_sectag_word_or_size(SectagWordOrSize,
            Stream, !IO),
        io.format(Stream, ")", [], !IO)
    ).

:- pred mercury_output_remote_sectag_word_or_size(rsectag_word_or_size::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_remote_sectag_word_or_size(SectagWordOrSize, Stream, !IO) :-
    (
        SectagWordOrSize = rsectag_full_word,
        io.write_string(Stream, "rst_full", !IO)
    ;
        SectagWordOrSize = rsectag_part_of_word(NumBits),
        io.format(Stream, "rst_part(%u)", [u8(NumBits)], !IO)
    ).

%---------------------%

:- pred mercury_output_local_arg_repn(type_repn_for::in, int::in,
    local_arg_repn::in, io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_local_arg_repn(TypeRepnFor, Indent, LocalArgRepn,
        Stream, !IO) :-
    NlI = nl_indent_for_humans(TypeRepnFor, Indent),
    (
        LocalArgRepn = local_partial(Shift, FillKindSize),
        io.format(Stream, "%slocal_partial(%u, %s)",
            [s(NlI), u(Shift), s(fill_kind_size_to_string(FillKindSize))], !IO)
    ;
        LocalArgRepn = local_none,
        io.format(Stream, "%slocal_none", [s(NlI)], !IO)
    ).

:- pred mercury_output_remote_arg_repn(type_repn_for::in, int::in,
    remote_arg_repn::in, io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_remote_arg_repn(TypeRepnFor, Indent, RemoteArgRepn,
        Stream, !IO) :-
    NlI = nl_indent_for_humans(TypeRepnFor, Indent),
    (
        RemoteArgRepn = remote_full(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        io.format(Stream, "%sfull(%d, %d)",
            [s(NlI), i(ArgOnlyOffsetInt), i(CellOffsetInt)], !IO)
    ;
        RemoteArgRepn = remote_double(ArgOnlyOffset, CellOffset, DoubleKind),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        double_word_kind_string(DoubleKind, DKStr),
        io.format(Stream, "%sdouble(%d, %d, %s)",
            [s(NlI), i(ArgOnlyOffsetInt), i(CellOffsetInt), s(DKStr)], !IO)
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
        io.format(Stream, "%spartial_%s(%d, %d, %u, %s)",
            [s(NlI), s(FirstOrShifted), i(ArgOnlyOffsetInt), i(CellOffsetInt),
            u8(Shift), s(fill_kind_size_to_string(FillKindSize))], !IO)
    ;
        RemoteArgRepn = remote_none_shifted(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        io.format(Stream, "%snone_shifted(%d, %d)",
            [s(NlI), i(ArgOnlyOffsetInt), i(CellOffsetInt)], !IO)
    ;
        RemoteArgRepn = remote_none_nowhere,
        io.format(Stream, "%snone_nowhere", [s(NlI)],!IO)
    ).

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

:- pred mercury_output_c_j_cs_repn_or_enum(io.text_output_stream::in,
    type_repn_for::in, int::in, c_j_cs_enum_repn::in, io::di, io::uo) is det.

mercury_output_c_j_cs_repn_or_enum(Stream, TypeRepnFor, Indent,
        MaybeCJCsRepnOrEnum, !IO) :-
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
            io.write_string(Stream, " no_c_j_cs", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            io.format(Stream, "%sno_c_j_cs", [s(I)], !IO)
        )
    else
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(Stream, " c_j_cs(", !IO),
            mercury_output_maybe_enum_foreign_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnOrEnumC, !IO),
            io.write_string(Stream, ", ", !IO),
            mercury_output_maybe_enum_foreign_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnOrEnumJava, !IO),
            io.write_string(Stream, ", ", !IO),
            mercury_output_maybe_enum_foreign_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnOrEnumCsharp, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            io.format(Stream, "\n%sc_j_cs(\n", [s(I)], !IO),
            mercury_output_maybe_enum_foreign_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnOrEnumC, !IO),
            io.write_string(Stream, ",\n", !IO),
            mercury_output_maybe_enum_foreign_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnOrEnumJava, !IO),
            io.write_string(Stream, ",\n", !IO),
            mercury_output_maybe_enum_foreign_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnOrEnumCsharp, !IO),
            io.format(Stream, "\n%s)", [s(I)], !IO)
        )
    ).

:- pred mercury_output_c_j_cs_repn(io.text_output_stream::in,
    type_repn_for::in, int::in, c_j_cs_repn::in, io::di, io::uo) is det.

mercury_output_c_j_cs_repn(Stream, TypeRepnFor, Indent, MaybeCJCsRepn, !IO) :-
    MaybeCJCsRepn = c_java_csharp(MaybeRepnC, MaybeRepnJava, MaybeRepnCsharp),
    ( if
        MaybeRepnC = no,
        MaybeRepnJava = no,
        MaybeRepnCsharp = no
    then
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(Stream, " no_c_j_cs", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format(Stream, "%sno_c_j_cs", [s(I)], !IO)
        )
    else
        Indent1 = Indent + 1,
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(Stream, " c_j_cs(", !IO),
            mercury_output_maybe_foreign_type_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnC, !IO),
            io.write_string(Stream, ", ", !IO),
            mercury_output_maybe_foreign_type_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnJava, !IO),
            io.write_string(Stream, ", ", !IO),
            mercury_output_maybe_foreign_type_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnCsharp, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format(Stream, "%sc_j_cs(\n", [s(I)], !IO),
            mercury_output_maybe_foreign_type_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnC, !IO),
            io.write_string(Stream, ",\n", !IO),
            mercury_output_maybe_foreign_type_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnJava, !IO),
            io.write_string(Stream, ",\n", !IO),
            mercury_output_maybe_foreign_type_repn(Stream, TypeRepnFor,
                Indent1, MaybeRepnCsharp, !IO),
            io.format(Stream, "\n%s)", [s(I)], !IO)
        )
    ).

%---------------------%

:- pred mercury_output_maybe_enum_foreign_repn(io.text_output_stream::in,
    type_repn_for::in, int::in, maybe(enum_foreign_repn)::in,
    io::di, io::uo) is det.

mercury_output_maybe_enum_foreign_repn(Stream, TypeRepnFor, Indent,
        MaybeTypeRepnOrEnum, !IO) :-
    (
        MaybeTypeRepnOrEnum = no,
        I = indent_for_humans(TypeRepnFor, Indent),
        io.format(Stream, "%sno_foreign", [s(I)], !IO)
    ;
        MaybeTypeRepnOrEnum = yes(enum_foreign_enum(EnumRepn)),
        % Top level functor will be "foreign_enum".
        mercury_output_foreign_enum_repn(Stream, TypeRepnFor, Indent,
            EnumRepn, !IO)
    ;
        MaybeTypeRepnOrEnum = yes(enum_foreign_type(TypeRepn)),
        % Top level functor will be "foreign_type".
        mercury_output_foreign_type_repn(Stream, TypeRepnFor, Indent,
            TypeRepn, !IO)
    ).

:- pred mercury_output_maybe_foreign_type_repn(io.text_output_stream::in,
    type_repn_for::in, int::in, maybe(foreign_type_repn)::in,
    io::di, io::uo) is det.

mercury_output_maybe_foreign_type_repn(Stream, TypeRepnFor, Indent,
        MaybeTypeRepn, !IO) :-
    (
        MaybeTypeRepn = no,
        I = indent_for_humans(TypeRepnFor, Indent),
        io.format(Stream, "%sno_foreign", [s(I)], !IO)
    ;
        MaybeTypeRepn = yes(TypeRepn),
        % Top level functor will be "foreign_type".
        mercury_output_foreign_type_repn(Stream, TypeRepnFor, Indent,
            TypeRepn, !IO)
    ).

%---------------------%

:- pred mercury_output_foreign_enum_repn(io.text_output_stream::in,
    type_repn_for::in, int::in, one_or_more(string)::in,
    io::di, io::uo) is det.

mercury_output_foreign_enum_repn(Stream, TypeRepnFor, Indent, EnumRepn, !IO) :-
    EnumRepn = one_or_more(HeadEnum, TailEnums),
    (
        TypeRepnFor = type_repn_for_machines,
        io.format(Stream, "foreign_enum([", [], !IO),
        write_out_list(mercury_output_functor_name, ", ",
            [HeadEnum | TailEnums], Stream, !IO),
        io.format(Stream, "])", [], !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        I = indent(Indent),
        io.format(Stream, "%sforeign_enum(", [s(I)], !IO),
        mercury_output_list_for_humans(Indent + 1,
            mercury_output_one_functor_name(nl_indent(Indent + 2), ""),
            [HeadEnum | TailEnums], Stream, !IO),
        io.format(Stream, "\n%s)", [s(I)], !IO)
    ).

%---------------------%

:- pred mercury_output_foreign_type_repn(io.text_output_stream::in,
    type_repn_for::in, int::in, foreign_type_repn::in, io::di, io::uo) is det.

mercury_output_foreign_type_repn(Stream, TypeRepnFor, Indent, TypeRepn, !IO) :-
    TypeRepn = foreign_type_repn(ForeignTypeName, ForeignTypeAssertions),
    I = indent_for_humans(TypeRepnFor, Indent),
    ForeignTypeAssertions = foreign_type_assertions(Assertions),
    set.to_sorted_list(Assertions, AssertionsList),
    io.format(Stream, "%sforeign_type(\"%s\",",
        [s(I), s(ForeignTypeName)], !IO),
    AssertionStrs =
        list.map(foreign_type_assertion_to_string, AssertionsList),
    AssertionsStr = string.join_list(", ", AssertionStrs),
    (
        TypeRepnFor = type_repn_for_machines,
        io.format(Stream, " [%s])", [s(AssertionsStr)], !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        I1 = indent(Indent + 1),
        io.format(Stream, "\n%s[%s]\n%s)",
            [s(I1), s(AssertionsStr), s(I)], !IO)
    ).

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

:- pred mercury_output_functor_name(string::in, io.text_output_stream::in,
    io::di, io::uo) is det.

mercury_output_functor_name(FunctorName, Stream, !IO) :-
    term_io.quote_string(Stream, FunctorName, !IO).

    % Output one functor's name in a list of functor names.
    %
:- pred mercury_output_one_functor_name(string::in, string::in, string::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_one_functor_name(Prefix, Suffix, FunctorName, Stream, !IO) :-
    io.write_string(Stream, Prefix, !IO),
    mercury_output_functor_name(FunctorName, Stream, !IO),
    io.write_string(Stream, Suffix, !IO).

%---------------------------------------------------------------------------%

    % Output one type in a list of types.
    %
:- pred mercury_output_one_type(string::in, string::in, tvarset::in,
    mer_type::in, io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_one_type(Prefix, Suffix, TVarSet, Type, Stream, !IO) :-
    io.write_string(Stream, Prefix, !IO),
    mercury_output_type(TVarSet, print_num_only, Type, Stream, !IO),
    io.write_string(Stream, Suffix, !IO).

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

:- pred mercury_output_list_for_humans(int::in,
    pred(T, io.text_output_stream, io, io)::in(pred(in, in, di, uo) is det),
    list(T)::in, io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_list_for_humans(Indent, WriteX, Xs, Stream, !IO) :-
    NlI = nl_indent(Indent),
    (
        Xs = [],
        io.format(Stream, "%s[]", [s(NlI)], !IO)
    ;
        Xs = [HeadX | TailXs],
        io.format(Stream, "%s[", [s(NlI)], !IO),
        mercury_output_list_for_humans_loop(WriteX, HeadX, TailXs,
            Stream, !IO),
        io.format(Stream, "%s]", [s(NlI)], !IO)
    ).

:- pred mercury_output_list_for_humans_loop(
    pred(T, io.text_output_stream, io, io)::in(pred(in, in, di, uo) is det),
    T::in, list(T)::in, io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_list_for_humans_loop(WriteX, X, Xs, Stream, !IO) :-
    WriteX(X, Stream, !IO),
    (
        Xs = []
    ;
        Xs = [HeadX | TailXs],
        io.write_string(Stream, ",", !IO),
        mercury_output_list_for_humans_loop(WriteX, HeadX, TailXs, Stream, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_c_repns(type_repn_for::in, int::in,
    pred(T, io.text_output_stream, io, io)::in(pred(in, in, di, uo) is det),
    c_repns(T)::in, io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_c_repns(TypeRepnFor, Indent, WriteX, CRepns, Stream, !IO) :-
    NlI = nl_indent_for_humans_space_for_machines(TypeRepnFor, Indent),
    (
        CRepns = c_repns_same(X1),
        io.format(Stream, "%sc_repns_same(", [s(NlI)], !IO),
        WriteX(X1, Stream, !IO),
        io.format(Stream, "%s)", [s(NlI)], !IO)
    ;
        CRepns = c_repns_64_32(X1, X2),
        io.format(Stream, "%sc_repns_64_32(", [s(NlI)], !IO),
        WriteX(X1, Stream, !IO),
        io.format(Stream, ", ", [], !IO),
        WriteX(X2, Stream, !IO),
        io.format(Stream, "%s)", [s(NlI)], !IO)
    ;
        CRepns = c_repns_all(X1, X2, X3, X4, X5, X6),
        io.format(Stream, "%sc_repns_all(", [s(NlI)], !IO),
        WriteX(X1, Stream, !IO),
        io.format(Stream, ", ", [], !IO),
        WriteX(X2, Stream, !IO),
        io.format(Stream, ", ", [], !IO),
        WriteX(X3, Stream, !IO),
        io.format(Stream, ", ", [], !IO),
        WriteX(X4, Stream, !IO),
        io.format(Stream, ", ", [], !IO),
        WriteX(X5, Stream, !IO),
        io.format(Stream, ", ", [], !IO),
        WriteX(X6, Stream, !IO),
        io.format(Stream, "%s)", [s(NlI)], !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_type_repn.
%---------------------------------------------------------------------------%
