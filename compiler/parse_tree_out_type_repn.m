%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
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
:- import_module list.
:- import_module maybe.

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
    item_type_repn_info::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func fill_kind_size_to_string(fill_kind_size) = string.

%---------------------------------------------------------------------------%

:- pred mercury_output_foreign_type_assertion(foreign_type_assertion::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module one_or_more.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module uint8.
:- import_module varset.

%---------------------------------------------------------------------------%

mercury_output_item_type_repn(Info, ItemTypeRepn, !IO) :-
    ItemTypeRepn = item_type_repn_info(TypeCtorSymName0, TypeParams, RepnInfo,
        TVarSet, Context, _SeqNum),
    io.write_string(":- type_representation(", !IO),
    maybe_unqualify_sym_name(Info, TypeCtorSymName0, TypeCtorSymName),
    Args = list.map((func(V) = term.variable(V, Context)), TypeParams),
    construct_qualified_term_with_context(TypeCtorSymName, Args, Context,
        TypeTerm),
    mercury_output_term_nq(TVarSet, print_num_only, next_to_graphic_token,
        TypeTerm, !IO),
    io.write_string(",", !IO),
    (
        RepnInfo = tcrepn_is_direct_dummy,
        io.write_string(" is_direct_dummy", !IO)
    ;
        RepnInfo = tcrepn_is_notag,
        io.write_string(" is_notag", !IO)
    ;
        RepnInfo = tcrepn_fits_in_n_bits(NumBits, FillKind),
        fill_kind_string(FillKind, FillKindStr),
        io.format(" fits_in_n_bits(%d, %s)", [i(NumBits), s(FillKindStr)], !IO)
    ;
        RepnInfo = tcrepn_is_eqv_to(EqvType),
        io.write_string(" is_eqv_to(", !IO),
        mercury_output_type(TVarSet, print_num_only, EqvType, !IO),
        io.write_string(")", !IO)
    ;
        RepnInfo = tcrepn_is_word_aligned_ptr,
        io.write_string(" is_word_aligned_ptr", !IO)
    ;
        RepnInfo = tcrepn_has_direct_arg_functors(SymNameAndArities),
        io.write_string(" has_direct_arg_functors([", !IO),
        io.write_list(SymNameAndArities, ", ", write_sym_name_arity, !IO),
        io.write_string("])", !IO)
    ;
        RepnInfo = tcrepn_du(DuRepn),
        TypeRepnFor = get_type_repn_for(Info),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(" du_repn(", !IO),
            mercury_output_du_type_repn(TypeRepnFor, 2, TVarSet, DuRepn, !IO),
            io.write_string(")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(1),
            io.format("\n%sdu_repn(", [s(I)], !IO),
            mercury_output_du_type_repn(TypeRepnFor, 2, TVarSet, DuRepn, !IO),
            io.format("\n%s)", [s(I)], !IO)
        )
    ;
        RepnInfo = tcrepn_foreign(MaybeCJCsERepn),
        TypeRepnFor = get_type_repn_for(Info),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(" foreign_type_repn(", !IO),
            mercury_output_c_j_cs_e_repn(TypeRepnFor, 2, MaybeCJCsERepn, !IO),
            io.write_string(")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(1),
            io.format("\n%sforeign_type_repn(\n", [s(I)], !IO),
            mercury_output_c_j_cs_e_repn(TypeRepnFor, 2, MaybeCJCsERepn, !IO),
            io.format("\n%s)", [s(I)], !IO)
        )
    ),
    io.write_string(").\n", !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_type_repn(type_repn_for::in, int::in, tvarset::in,
    du_repn::in, io::di, io::uo) is det.

mercury_output_du_type_repn(TypeRepnFor, Indent, TVarSet, DuRepn, !IO) :-
    (
        DuRepn = dur_direct_dummy(DummyRepn),
        mercury_output_du_direct_dummy(TypeRepnFor, Indent, DummyRepn, !IO)
    ;
        DuRepn = dur_enum(EnumRepn),
        mercury_output_du_enum(TypeRepnFor, Indent, EnumRepn, !IO)
    ;
        DuRepn = dur_notag(NotagRepn),
        mercury_output_du_notag(TypeRepnFor, Indent, TVarSet, NotagRepn, !IO)
    ;
        DuRepn = dur_gen_only_functor(OnlyFunctorRepn),
        mercury_output_du_only_functor(TypeRepnFor, Indent, TVarSet,
            OnlyFunctorRepn, !IO)
    ;
        DuRepn = dur_gen_more_functors(MoreFunctorsRepn),
        mercury_output_du_more_functors(TypeRepnFor, Indent, TVarSet,
            MoreFunctorsRepn, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_direct_dummy(type_repn_for::in, int::in,
    direct_dummy_repn::in, io::di, io::uo) is det.

mercury_output_du_direct_dummy(TypeRepnFor, Indent, DummyRepn, !IO) :-
    DummyRepn = direct_dummy_repn(FunctorName, MaybeCJCsERepnOrEnum),
    (
        TypeRepnFor = type_repn_for_machines,
        io.write_string("direct_dummy(", !IO),
        mercury_output_functor_name(FunctorName, !IO),
        io.write_string(", ", !IO),
        mercury_output_c_j_cs_e_repn_or_enum(TypeRepnFor, Indent + 1,
            MaybeCJCsERepnOrEnum, !IO),
        io.write_string(")", !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        I = indent(Indent),
        io.format("\n%sdirect_dummy(", [s(I)], !IO),
        mercury_output_functor_name(FunctorName, !IO),
        io.write_string(",\n", !IO),
        mercury_output_c_j_cs_e_repn_or_enum(TypeRepnFor, Indent + 1,
            MaybeCJCsERepnOrEnum, !IO),
        io.format("\n%s)", [s(I)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_enum(type_repn_for::in, int::in, enum_repn::in,
    io::di, io::uo) is det.

mercury_output_du_enum(TypeRepnFor, Indent, EnumRepn, !IO) :-
    EnumRepn = enum_repn(Functor1, Functor2, OtherFunctors,
        MaybeCJCsERepnOrEnum),
    (
        TypeRepnFor = type_repn_for_machines,
        io.write_string("enum(", !IO),
        mercury_output_functor_name(Functor1, !IO),
        io.write_string(", ", !IO),
        mercury_output_functor_name(Functor2, !IO),
        io.write_string(", ", !IO),
        (
            OtherFunctors = [],
            io.write_string("[]", !IO)
        ;
            OtherFunctors = [HeadFunctor | TailFunctors],
            io.write_string("[", !IO),
            io.write_list([HeadFunctor | TailFunctors], ", ",
                mercury_output_functor_name, !IO),
            io.write_string("]", !IO)
        ),
        io.write_string(", ", !IO),
        mercury_output_c_j_cs_e_repn_or_enum(TypeRepnFor, 0,
            MaybeCJCsERepnOrEnum, !IO),
        io.write_string(")", !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        Indent1 = Indent + 1,
        I = indent(Indent),
        NlI1 = nl_indent(Indent1),
        io.format("\n%senum(", [s(I)], !IO),
        mercury_output_one_functor_name(NlI1, "", Functor1, !IO),
        io.write_string(",", !IO),
        mercury_output_one_functor_name(NlI1, "", Functor2, !IO),
        io.write_string(",", !IO),
        mercury_output_list_for_humans(Indent1,
            mercury_output_one_functor_name(nl_indent(Indent + 2), ""),
            OtherFunctors, !IO),
        io.format(",\n", [], !IO),
        mercury_output_c_j_cs_e_repn_or_enum(TypeRepnFor, Indent1,
            MaybeCJCsERepnOrEnum, !IO),
        io.format("\n%s)", [s(I)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_notag(type_repn_for::in, int::in, tvarset::in,
    notag_repn::in, io::di, io::uo) is det.

mercury_output_du_notag(TypeRepnFor, Indent, TVarSet, NotagRepn, !IO) :-
    NotagRepn = notag_repn(FunctorName, ArgType, MaybeCJCsERepn),
    (
        TypeRepnFor = type_repn_for_machines,
        io.write_string("notag(", !IO),
        mercury_output_functor_name(FunctorName, !IO),
        io.write_string(", ", !IO),
        mercury_output_type(TVarSet, print_num_only, ArgType, !IO),
        io.write_string(", ", !IO),
        mercury_output_c_j_cs_e_repn(TypeRepnFor, 0, MaybeCJCsERepn, !IO),
        io.write_string(")", !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        I = indent(Indent),
        I1 = indent(Indent + 1),
        io.format("\n%snotag(\n%s", [s(I), s(I1)], !IO),
        mercury_output_functor_name(FunctorName, !IO),
        io.format(",\n%s", [s(I1)], !IO),
        mercury_output_type(TVarSet, print_num_only, ArgType, !IO),
        io.format(",\n", [], !IO),
        mercury_output_c_j_cs_e_repn(TypeRepnFor, Indent + 1,
            MaybeCJCsERepn, !IO),
        io.format("\n%s)", [s(I)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_only_functor(type_repn_for::in, int::in,
    tvarset::in, gen_du_only_functor_repn::in, io::di, io::uo) is det.

mercury_output_du_only_functor(TypeRepnFor, Indent, TVarSet, OnlyFunctorRepn,
        !IO) :-
    OnlyFunctorRepn = gen_du_only_functor_repn(FunctorName, ArgTypes, CRepns,
        MaybeCJCsERepn),
    (
        TypeRepnFor = type_repn_for_machines,
        io.write_string("gen_du_only_functor(", !IO),
        mercury_output_functor_name(FunctorName, !IO),
        io.write_string(", [", !IO),
        io.write_list(ArgTypes, ", ",
            mercury_output_type(TVarSet, print_num_only), !IO),
        io.write_string("], ", !IO),
        mercury_output_c_repns(TypeRepnFor, 0,
            mercury_output_nonconstant_repn(TypeRepnFor, 0),
            CRepns, !IO),
        io.write_string(", ", !IO),
        mercury_output_c_j_cs_e_repn(TypeRepnFor, 0, MaybeCJCsERepn, !IO),
        io.write_string(")", !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        Indent1 = Indent + 1,
        Indent2 = Indent + 2,
        I = indent(Indent),
        I1 = indent(Indent1),
        NlI2 = nl_indent(Indent2),
        io.format("\n%sgen_du_only_functor(\n%s", [s(I), s(I1)], !IO),
        mercury_output_functor_name(FunctorName, !IO),
        io.format(",", [], !IO),
        mercury_output_list_for_humans(Indent1,
            mercury_output_one_type(NlI2, "", TVarSet), ArgTypes, !IO),
        io.format(",", [], !IO),
        mercury_output_c_repns(TypeRepnFor, Indent1,
            mercury_output_nonconstant_repn(TypeRepnFor, Indent2),
            CRepns, !IO),
        io.format(",\n", [], !IO),
        mercury_output_c_j_cs_e_repn(TypeRepnFor, Indent1,
            MaybeCJCsERepn, !IO),
        io.format("\n%s)", [s(I)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_du_more_functors(type_repn_for::in, int::in,
    tvarset::in, gen_du_more_functors_repn::in, io::di, io::uo) is det.

mercury_output_du_more_functors(TypeRepnFor, Indent, TVarSet,
        MoreFunctorsRepn, !IO) :-
    MoreFunctorsRepn = gen_du_more_functors_repn(Functor1, Functor2,
        OtherFunctors, MaybeCJCsERepn),
    (
        TypeRepnFor = type_repn_for_machines,
        io.write_string("gen_du_more_functors(", !IO),
        mercury_output_gen_du_functor_repn(TypeRepnFor, 0, TVarSet,
            Functor1, !IO),
        io.write_string(", ", !IO),
        mercury_output_gen_du_functor_repn(TypeRepnFor, 0, TVarSet,
            Functor2, !IO),
        io.write_string(", ", !IO),
        (
            OtherFunctors = [],
            io.write_string("[]", !IO)
        ;
            OtherFunctors = [HeadFunctor | TailFunctors],
            io.write_string("[", !IO),
            io.write_list([HeadFunctor | TailFunctors], ", ",
                mercury_output_gen_du_functor_repn(TypeRepnFor, 0, TVarSet),
                !IO),
            io.write_string("]", !IO)
        ),
        io.write_string(", ", !IO),
        mercury_output_c_j_cs_e_repn(TypeRepnFor, 0, MaybeCJCsERepn, !IO),
        io.write_string(")", !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        Indent1 = Indent + 1,
        I = indent(Indent),
        io.format("\n%sgen_du_more_functors(", [s(I)], !IO),
        mercury_output_gen_du_functor_repn(TypeRepnFor, Indent1, TVarSet,
            Functor1, !IO),
        io.format(",", [], !IO),
        mercury_output_gen_du_functor_repn(TypeRepnFor, Indent1, TVarSet,
            Functor2, !IO),
        io.format(",", [], !IO),
        mercury_output_list_for_humans(Indent1,
            mercury_output_gen_du_functor_repn(TypeRepnFor, Indent + 2,
                TVarSet),
            OtherFunctors, !IO),
        io.format(",\n", [], !IO),
        mercury_output_c_j_cs_e_repn(TypeRepnFor, Indent1,
            MaybeCJCsERepn, !IO),
        io.format("\n%s)", [s(I)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_gen_du_functor_repn(type_repn_for::in, int::in,
    tvarset::in, gen_du_functor_repn::in, io::di, io::uo) is det.

mercury_output_gen_du_functor_repn(TypeRepnFor, Indent, TVarSet,
        FunctorRepn, !IO) :-
    (
        FunctorRepn = gen_du_constant_functor_repn(FunctorName, ConstantRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string("constant_functor(", !IO),
            mercury_output_functor_name(FunctorName, !IO),
            io.write_string(", ", !IO),
            mercury_output_c_repns(TypeRepnFor, 0,
                mercury_output_constant_repn(TypeRepnFor, 0),
                ConstantRepns, !IO),
            io.write_string(")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format("\n%sconstant_functor(", [s(I)], !IO),
            mercury_output_functor_name(FunctorName, !IO),
            io.format(",", [], !IO),
            Indent1 = Indent + 1,
            mercury_output_c_repns(TypeRepnFor, Indent1,
                mercury_output_constant_repn(TypeRepnFor, Indent + 2),
                ConstantRepns, !IO),
            io.format("\n%s)", [s(I)], !IO)
        )
    ;
        FunctorRepn = gen_du_nonconstant_functor_repn(FunctorName,
            ArgTypes, NonConstantRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string("nonconstant_functor(", !IO),
            mercury_output_functor_name(FunctorName, !IO),
            io.write_string(", [", !IO),
            io.write_list(ArgTypes, ", ",
                mercury_output_type(TVarSet, print_num_only), !IO),
            io.write_string("], ", !IO),
            mercury_output_c_repns(TypeRepnFor, 0,
                mercury_output_nonconstant_repn(TypeRepnFor, 0),
                NonConstantRepns, !IO),
            io.write_string(")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format("\n%snonconstant_functor(", [s(I)], !IO),
            mercury_output_functor_name(FunctorName, !IO),
            io.format(",", [], !IO),
            Indent1 = Indent + 1,
            Indent2 = Indent + 2,
            NlI2 = nl_indent(Indent2),
            mercury_output_list_for_humans(Indent1,
                mercury_output_one_type(NlI2, "", TVarSet), ArgTypes, !IO),
            io.format(",", [], !IO),
            mercury_output_c_repns(TypeRepnFor, Indent1,
                mercury_output_nonconstant_repn(TypeRepnFor, Indent2),
                NonConstantRepns, !IO),
            io.format("\n%s)", [s(I)], !IO)
        )
    ).

%---------------------%

:- pred mercury_output_constant_repn(type_repn_for::in, int::in,
    constant_repn::in, io::di, io::uo) is det.

mercury_output_constant_repn(TypeRepnFor, Indent, ConstantRepn, !IO) :-
    NlI = nl_indent_for_humans_space_for_machines(TypeRepnFor, Indent),
    ConstantRepn = constant_repn(Sectag, SectagWordOrSize),
    io.format("%sconstant(%u, ", [s(NlI), u(Sectag)], !IO),
    mercury_output_sectag_word_or_size(SectagWordOrSize, !IO),
    io.write_string(")", !IO).

%---------------------%

:- pred mercury_output_nonconstant_repn(type_repn_for::in, int::in,
    nonconstant_repn::in, io::di, io::uo) is det.

mercury_output_nonconstant_repn(TypeRepnFor, Indent, NonConstantRepn, !IO) :-
    (
        NonConstantRepn = ncr_local_cell(CellLocalSectag, OoMLocalArgRepns),
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string("local_cell(", !IO),
            mercury_output_cell_local_sectag(CellLocalSectag, !IO),
            io.write_string(", [", !IO),
            OoMLocalArgRepns =
                one_or_more(HeadLocalArgRepn, TailLocalArgRepns),
            io.write_list([HeadLocalArgRepn | TailLocalArgRepns], ", ",
                mercury_output_local_arg_repn(TypeRepnFor, 0), !IO),
            io.write_string("])", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format("\n%slocal_cell(", [s(I)], !IO),
            mercury_output_cell_local_sectag(CellLocalSectag, !IO),
            io.write_string(",", !IO),
            OoMLocalArgRepns =
                one_or_more(HeadLocalArgRepn, TailLocalArgRepns),
            mercury_output_list_for_humans(Indent + 1,
                mercury_output_local_arg_repn(TypeRepnFor, Indent + 2),
                [HeadLocalArgRepn | TailLocalArgRepns], !IO),
            io.format("\n%s)", [s(I)], !IO)
        )
    ;
        NonConstantRepn =
            ncr_remote_cell(Ptag, CellRemoteSectag, OoMRemoteArgRepns),
        Ptag = ptag(PtagUint8),
        PtagUint = uint8.cast_to_uint(PtagUint8),
        (
            TypeRepnFor = type_repn_for_machines,
            io.format("remote_cell(%u, ", [u(PtagUint)], !IO),
            mercury_output_cell_remote_sectag(CellRemoteSectag, !IO),
            io.write_string(", [", !IO),
            OoMRemoteArgRepns =
                one_or_more(HeadRemoteArgRepn, TailRemoteArgRepns),
            io.write_list([HeadRemoteArgRepn | TailRemoteArgRepns], ", ",
                mercury_output_remote_arg_repn(TypeRepnFor, 0), !IO),
            io.write_string("])", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format("\n%sremote_cell(%u, ", [s(I), u(PtagUint)], !IO),
            mercury_output_cell_remote_sectag(CellRemoteSectag, !IO),
            io.write_string(",", !IO),
            OoMRemoteArgRepns =
                one_or_more(HeadRemoteArgRepn, TailRemoteArgRepns),
            mercury_output_list_for_humans(Indent + 1,
                mercury_output_remote_arg_repn(TypeRepnFor, Indent + 2),
                [HeadRemoteArgRepn | TailRemoteArgRepns], !IO),
            io.format("\n%s)", [s(I)], !IO)
        )
    ;
        NonConstantRepn = ncr_direct_arg(Ptag),
        Ptag = ptag(PtagUint8),
        PtagUint = uint8.cast_to_uint(PtagUint8),
        (
            TypeRepnFor = type_repn_for_machines,
            io.format("direct_arg(%u)", [u(PtagUint)], !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            NlI = nl_indent(Indent),
            io.format("%sdirect_arg(%u)", [s(NlI), u(PtagUint)], !IO)
        )
    ).

%---------------------%

:- pred mercury_output_cell_local_sectag(cell_local_sectag::in,
    io::di, io::uo) is det.

mercury_output_cell_local_sectag(CellLocalSectag, !IO) :-
    (
        CellLocalSectag = cell_local_no_sectag,
        io.write_string("local_no_sectag", !IO)
    ;
        CellLocalSectag = cell_local_sectag(Sectag, SectagNumBits),
        io.format("local_sectag(%u, %u)", [u(Sectag), u(SectagNumBits)], !IO)
    ).

:- pred mercury_output_cell_remote_sectag(cell_remote_sectag::in,
    io::di, io::uo) is det.

mercury_output_cell_remote_sectag(CellLocalSectag, !IO) :-
    (
        CellLocalSectag = cell_remote_no_sectag,
        io.write_string("remote_no_sectag", !IO)
    ;
        CellLocalSectag = cell_remote_sectag(Sectag, SectagWordOrSize),
        io.format("remote_sectag(%u, ", [u(Sectag)], !IO),
        mercury_output_sectag_word_or_size(SectagWordOrSize, !IO),
        io.format(")", [], !IO)
    ).

:- pred mercury_output_sectag_word_or_size(sectag_word_or_size::in,
    io::di, io::uo) is det.

mercury_output_sectag_word_or_size(SectagWordOrSize, !IO) :-
    (
        SectagWordOrSize = sectag_rest_of_word,
        io.write_string("rest", !IO)
    ;
        SectagWordOrSize = sectag_part_of_word(NumBits),
        io.format("part(%u)", [u(NumBits)], !IO)
    ).

%---------------------%

:- pred mercury_output_local_arg_repn(type_repn_for::in, int::in,
    local_arg_repn::in, io::di, io::uo) is det.

mercury_output_local_arg_repn(TypeRepnFor, Indent, LocalArgRepn, !IO) :-
    NlI = nl_indent_for_humans(TypeRepnFor, Indent),
    (
        LocalArgRepn = local_partial(Shift, FillKindSize),
        io.format("%slocal_partial(%u, %s)",
            [s(NlI), u(Shift), s(fill_kind_size_to_string(FillKindSize))], !IO)
    ;
        LocalArgRepn = local_none,
        io.format("%slocal_none", [s(NlI)], !IO)
    ).

:- pred mercury_output_remote_arg_repn(type_repn_for::in, int::in,
    remote_arg_repn::in, io::di, io::uo) is det.

mercury_output_remote_arg_repn(TypeRepnFor, Indent, RemoteArgRepn, !IO) :-
    NlI = nl_indent_for_humans(TypeRepnFor, Indent),
    (
        RemoteArgRepn = remote_full(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        io.format("%sfull(%d, %d)",
            [s(NlI), i(ArgOnlyOffsetInt), i(CellOffsetInt)], !IO)
    ;
        RemoteArgRepn = remote_double(ArgOnlyOffset, CellOffset, DoubleKind),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        double_word_kind_string(DoubleKind, DKStr),
        io.format("%sdouble(%d, %d, %s)",
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
        io.format("%spartial_%s(%d, %d, %u, %s)",
            [s(NlI), s(FirstOrShifted), i(ArgOnlyOffsetInt), i(CellOffsetInt),
            u(Shift), s(fill_kind_size_to_string(FillKindSize))], !IO)
    ;
        RemoteArgRepn = remote_none_shifted(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        io.format("%snone_shifted(%d, %d)",
            [s(NlI), i(ArgOnlyOffsetInt), i(CellOffsetInt)], !IO)
    ;
        RemoteArgRepn = remote_none_nowhere,
        io.format("%snone_nowhere", [s(NlI)],!IO)
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

:- pred mercury_output_c_j_cs_e_repn_or_enum(type_repn_for::in, int::in,
    c_j_cs_e_enum_repn::in, io::di, io::uo) is det.

mercury_output_c_j_cs_e_repn_or_enum(TypeRepnFor, Indent,
        MaybeCJCsERepnOrEnum, !IO) :-
    MaybeCJCsERepnOrEnum = c_java_csharp_erlang(MaybeRepnOrEnumC,
        MaybeRepnOrEnumJava, MaybeRepnOrEnumCsharp, MaybeRepnOrEnumErlang),
    Indent1 = Indent + 1,
    I = indent(Indent),
    ( if
        MaybeRepnOrEnumC = no,
        MaybeRepnOrEnumJava = no,
        MaybeRepnOrEnumCsharp = no,
        MaybeRepnOrEnumErlang = no
    then
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(" no_c_j_cs_e", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            io.format("%sno_c_j_cs_e", [s(I)], !IO)
        )
    else
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(" c_j_cs_e(", !IO),
            mercury_output_maybe_enum_foreign_repn(TypeRepnFor, Indent1,
                MaybeRepnOrEnumC, !IO),
            io.write_string(", ", !IO),
            mercury_output_maybe_enum_foreign_repn(TypeRepnFor, Indent1,
                MaybeRepnOrEnumJava, !IO),
            io.write_string(", ", !IO),
            mercury_output_maybe_enum_foreign_repn(TypeRepnFor, Indent1,
                MaybeRepnOrEnumCsharp, !IO),
            io.write_string(", ", !IO),
            mercury_output_maybe_enum_foreign_repn(TypeRepnFor, Indent1,
                MaybeRepnOrEnumErlang, !IO),
            io.write_string(")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            io.format("\n%sc_j_cs_e(\n", [s(I)], !IO),
            mercury_output_maybe_enum_foreign_repn(TypeRepnFor, Indent1,
                MaybeRepnOrEnumC, !IO),
            io.write_string(",\n", !IO),
            mercury_output_maybe_enum_foreign_repn(TypeRepnFor, Indent1,
                MaybeRepnOrEnumJava, !IO),
            io.write_string(",\n", !IO),
            mercury_output_maybe_enum_foreign_repn(TypeRepnFor, Indent1,
                MaybeRepnOrEnumCsharp, !IO),
            io.write_string(",\n", !IO),
            mercury_output_maybe_enum_foreign_repn(TypeRepnFor, Indent1,
                MaybeRepnOrEnumErlang, !IO),
            io.format("\n%s)", [s(I)], !IO)
        )
    ).

:- pred mercury_output_c_j_cs_e_repn(type_repn_for::in, int::in,
    c_j_cs_e_repn::in, io::di, io::uo) is det.

mercury_output_c_j_cs_e_repn(TypeRepnFor, Indent, MaybeCJCsERepn, !IO) :-
    MaybeCJCsERepn = c_java_csharp_erlang(MaybeRepnC, MaybeRepnJava,
        MaybeRepnCsharp, MaybeRepnErlang),
    ( if
        MaybeRepnC = no,
        MaybeRepnJava = no,
        MaybeRepnCsharp = no,
        MaybeRepnErlang = no
    then
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(" no_c_j_cs_e", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format("%sno_c_j_cs_e", [s(I)], !IO)
        )
    else
        Indent1 = Indent + 1,
        (
            TypeRepnFor = type_repn_for_machines,
            io.write_string(" c_j_cs_e(", !IO),
            mercury_output_maybe_foreign_type_repn(TypeRepnFor, Indent1,
                MaybeRepnC, !IO),
            io.write_string(", ", !IO),
            mercury_output_maybe_foreign_type_repn(TypeRepnFor, Indent1,
                MaybeRepnJava, !IO),
            io.write_string(", ", !IO),
            mercury_output_maybe_foreign_type_repn(TypeRepnFor, Indent1,
                MaybeRepnCsharp, !IO),
            io.write_string(", ", !IO),
            mercury_output_maybe_foreign_type_repn(TypeRepnFor, Indent1,
                MaybeRepnErlang, !IO),
            io.write_string(")", !IO)
        ;
            TypeRepnFor = type_repn_for_humans,
            I = indent(Indent),
            io.format("%sc_j_cs_e(\n", [s(I)], !IO),
            mercury_output_maybe_foreign_type_repn(TypeRepnFor, Indent1,
                MaybeRepnC, !IO),
            io.write_string(",\n", !IO),
            mercury_output_maybe_foreign_type_repn(TypeRepnFor, Indent1,
                MaybeRepnJava, !IO),
            io.write_string(",\n", !IO),
            mercury_output_maybe_foreign_type_repn(TypeRepnFor, Indent1,
                MaybeRepnCsharp, !IO),
            io.write_string(",\n", !IO),
            mercury_output_maybe_foreign_type_repn(TypeRepnFor, Indent1,
                MaybeRepnErlang, !IO),
            io.format("\n%s)", [s(I)], !IO)
        )
    ).

%---------------------%

:- pred mercury_output_maybe_enum_foreign_repn(type_repn_for::in, int::in,
    maybe(enum_foreign_repn)::in, io::di, io::uo) is det.

mercury_output_maybe_enum_foreign_repn(TypeRepnFor, Indent,
        MaybeTypeRepnOrEnum, !IO) :-
    (
        MaybeTypeRepnOrEnum = no,
        I = indent_for_humans(TypeRepnFor, Indent),
        io.format("%sno_foreign", [s(I)], !IO)
    ;
        MaybeTypeRepnOrEnum = yes(enum_foreign_enum(EnumRepn)),
        % Top level functor will be "foreign_enum".
        mercury_output_foreign_enum_repn(TypeRepnFor, Indent, EnumRepn, !IO)
    ;
        MaybeTypeRepnOrEnum = yes(enum_foreign_type(TypeRepn)),
        % Top level functor will be "foreign_type".
        mercury_output_foreign_type_repn(TypeRepnFor, Indent, TypeRepn, !IO)
    ).

:- pred mercury_output_maybe_foreign_type_repn(type_repn_for::in, int::in,
    maybe(foreign_type_repn)::in, io::di, io::uo) is det.

mercury_output_maybe_foreign_type_repn(TypeRepnFor, Indent,
        MaybeTypeRepn, !IO) :-
    (
        MaybeTypeRepn = no,
        I = indent_for_humans(TypeRepnFor, Indent),
        io.format("%sno_foreign", [s(I)], !IO)
    ;
        MaybeTypeRepn = yes(TypeRepn),
        % Top level functor will be "foreign_type".
        mercury_output_foreign_type_repn(TypeRepnFor, Indent, TypeRepn, !IO)
    ).

%---------------------%

:- pred mercury_output_foreign_enum_repn(type_repn_for::in, int::in,
    one_or_more(string)::in, io::di, io::uo) is det.

mercury_output_foreign_enum_repn(TypeRepnFor, Indent, EnumRepn, !IO) :-
    EnumRepn = one_or_more(HeadEnum, TailEnums),
    (
        TypeRepnFor = type_repn_for_machines,
        io.format("foreign_enum([", [], !IO),
        io.write_list([HeadEnum | TailEnums], ", ",
            mercury_output_functor_name, !IO),
        io.format("])", [], !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        I = indent(Indent),
        io.format("%sforeign_enum(", [s(I)], !IO),
        mercury_output_list_for_humans(Indent + 1,
            mercury_output_one_functor_name(nl_indent(Indent + 2), ""),
            [HeadEnum | TailEnums], !IO),
        io.format("\n%s)", [s(I)], !IO)
    ).

%---------------------%

:- pred mercury_output_foreign_type_repn(type_repn_for::in, int::in,
    foreign_type_repn::in, io::di, io::uo) is det.

mercury_output_foreign_type_repn(TypeRepnFor, Indent, TypeRepn, !IO) :-
    TypeRepn = foreign_type_repn(ForeignTypeName, ForeignTypeAssertions),
    I = indent_for_humans(TypeRepnFor, Indent),
    ForeignTypeAssertions = foreign_type_assertions(Assertions),
    set.to_sorted_list(Assertions, AssertionsList),
    io.format("%sforeign_type(\"%s\",", [s(I), s(ForeignTypeName)], !IO),
    (
        TypeRepnFor = type_repn_for_machines,
        io.write_string(" [", !IO),
        io.write_list(AssertionsList, ", ",
            mercury_output_foreign_type_assertion, !IO),
        io.write_string("]", !IO)
    ;
        TypeRepnFor = type_repn_for_humans,
        I1 = indent(Indent + 1),
        io.format("\n%s[", [s(I1)], !IO),
        io.write_list(AssertionsList, ", ",
            mercury_output_foreign_type_assertion, !IO),
        io.write_string("]", !IO)
    ),
    io.format("\n%s)", [s(I)], !IO).

:- pred mercury_output_prefix_foreign_type_assertion(string::in,
    foreign_type_assertion::in, io::di, io::uo) is det.

mercury_output_prefix_foreign_type_assertion(Prefix, Assertion, !IO) :-
    io.write_string(Prefix, !IO),
    mercury_output_foreign_type_assertion(Assertion, !IO).

mercury_output_foreign_type_assertion(Assertion, !IO) :-
    (
        Assertion = foreign_type_can_pass_as_mercury_type,
        io.write_string("can_pass_as_mercury_type", !IO)
    ;
        Assertion = foreign_type_stable,
        io.write_string("stable", !IO)
    ;
        Assertion = foreign_type_word_aligned_pointer,
        io.write_string("word_aligned_pointer", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_functor_name(string::in, io::di, io::uo) is det.

mercury_output_functor_name(FunctorName, !IO) :-
    term_io.quote_string(FunctorName, !IO).

    % Output one functor's name in a list of functor names.
    %
:- pred mercury_output_one_functor_name(string::in, string::in, string::in,
    io::di, io::uo) is det.

mercury_output_one_functor_name(Prefix, Suffix, FunctorName, !IO) :-
    io.write_string(Prefix, !IO),
    mercury_output_functor_name(FunctorName, !IO),
    io.write_string(Suffix, !IO).

%---------------------------------------------------------------------------%

    % Output one type in a list of types.
    %
:- pred mercury_output_one_type(string::in, string::in,
    tvarset::in, mer_type::in, io::di, io::uo) is det.

mercury_output_one_type(Prefix, Suffix, TVarSet, Type, !IO) :-
    io.write_string(Prefix, !IO),
    mercury_output_type(TVarSet, print_num_only, Type, !IO),
    io.write_string(Suffix, !IO).

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
    pred(T, io, io)::in(pred(in, di, uo) is det), list(T)::in,
    io::di, io::uo) is det.

mercury_output_list_for_humans(Indent, WriteX, Xs, !IO) :-
    NlI = nl_indent(Indent),
    (
        Xs = [],
        io.format("%s[]", [s(NlI)], !IO)
    ;
        Xs = [HeadX | TailXs],
        io.format("%s[", [s(NlI)], !IO),
        mercury_output_list_for_humans_loop(WriteX, HeadX, TailXs, !IO),
        io.format("%s]", [s(NlI)], !IO)
    ).

:- pred mercury_output_list_for_humans_loop(
    pred(T, io, io)::in(pred(in, di, uo) is det), T::in, list(T)::in,
    io::di, io::uo) is det.

mercury_output_list_for_humans_loop(WriteX, X, Xs, !IO) :-
    WriteX(X, !IO),
    (
        Xs = []
    ;
        Xs = [HeadX | TailXs],
        io.write_string(",", !IO),
        mercury_output_list_for_humans_loop(WriteX, HeadX, TailXs, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_c_repns(type_repn_for::in, int::in,
    pred(T, io, io)::in(pred(in, di, uo) is det),
    c_repns(T)::in, io::di, io::uo) is det.

mercury_output_c_repns(TypeRepnFor, Indent, WriteX, CRepns, !IO) :-
    NlI = nl_indent_for_humans_space_for_machines(TypeRepnFor, Indent),
    (
        CRepns = c_repns_same(X1),
        io.format("%sc_repns_same(", [s(NlI)], !IO),
        WriteX(X1, !IO),
        io.format("%s)", [s(NlI)], !IO)
    ;
        CRepns = c_repns_64_32(X1, X2),
        io.format("%sc_repns_64_32(", [s(NlI)], !IO),
        WriteX(X1, !IO),
        io.format(", ", [], !IO),
        WriteX(X2, !IO),
        io.format("%s)", [s(NlI)], !IO)
    ;
        CRepns = c_repns_all(X1, X2, X3, X4, X5, X6),
        io.format("%sc_repns_all(", [s(NlI)], !IO),
        WriteX(X1, !IO),
        io.format(", ", [], !IO),
        WriteX(X2, !IO),
        io.format(", ", [], !IO),
        WriteX(X3, !IO),
        io.format(", ", [], !IO),
        WriteX(X4, !IO),
        io.format(", ", [], !IO),
        WriteX(X5, !IO),
        io.format(", ", [], !IO),
        WriteX(X6, !IO),
        io.format("%s)", [s(NlI)], !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_type_repn.
%---------------------------------------------------------------------------%
