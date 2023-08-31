%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_inst_table.
:- interface.

:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.

:- import_module io.

:- type maybe_use_error_msg_inst
    --->    do_not_use_error_msg_inst
    ;       use_error_msg_inst(module_info).

:- pred write_inst_table(io.text_output_stream::in, output_lang::in,
    maybe_use_error_msg_inst::in, int::in, int::in, inst_table::in,
    io::di, io::uo) is det.

:- pred write_mode_table(io.text_output_stream::in, mode_table::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.error_msg_inst.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.write_error_spec.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

write_inst_table(Stream, Lang, MaybeUseErrorMsgInst,
        InstNumLimit, InstSizeLimit, InstTable, !IO) :-
    io.write_string(Stream, "%-------- Insts --------\n", !IO),

    inst_table_get_user_insts(InstTable, UserInstTable),
    inst_table_get_unify_insts(InstTable, UnifyInstTable),
    inst_table_get_merge_insts(InstTable, MergeInstTable),
    inst_table_get_ground_insts(InstTable, GroundInstTable),
    inst_table_get_any_insts(InstTable, AnyInstTable),
    inst_table_get_shared_insts(InstTable, SharedInstTable),
    inst_table_get_mostly_uniq_insts(InstTable, MostlyUniqInstTable),

    map.to_sorted_assoc_list(UserInstTable, UserInstPairs),
    unify_insts_to_sorted_pairs(UnifyInstTable, UnifyInstPairs),
    merge_insts_to_sorted_pairs(MergeInstTable, MergeInstPairs),
    ground_insts_to_sorted_pairs(GroundInstTable, GroundInstPairs),
    any_insts_to_sorted_pairs(AnyInstTable, AnyInstPairs),
    shared_insts_to_sorted_pairs(SharedInstTable, SharedInstPairs),
    mostly_uniq_insts_to_sorted_pairs(MostlyUniqInstTable,
        MostlyUniqInstPairs),

    io.write_string(Stream, "\n%-------- User defined insts --------\n", !IO),
    list.foldl(write_user_inst(Stream), UserInstPairs, !IO),

    io.write_string(Stream, "\n%-------- Unify insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst_det(MaybeUseErrorMsgInst, Stream, Lang, "Unify",
            InstNumLimit, InstSizeLimit, write_key_unify_inst),
        UnifyInstPairs, 0, NumUnifyInsts, !IO),
    io.format(Stream,
        "\nTotal number of unify insts: %d\n", [i(NumUnifyInsts)], !IO),

    io.write_string(Stream, "\n%-------- Merge insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst(MaybeUseErrorMsgInst, Stream, Lang, "Merge",
            InstNumLimit, InstSizeLimit, write_key_merge_inst),
        MergeInstPairs, 0, NumMergeInsts, !IO),
    io.format(Stream,
        "\nTotal number of merge insts: %d\n", [i(NumMergeInsts)], !IO),

    io.write_string(Stream, "\n%-------- Ground insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst_det(MaybeUseErrorMsgInst, Stream, Lang, "Ground",
            InstNumLimit, InstSizeLimit, write_key_ground_inst),
        GroundInstPairs, 0, NumGroundInsts, !IO),
    io.format(Stream, "\nTotal number of ground insts: %d\n",
        [i(NumGroundInsts)], !IO),

    io.write_string(Stream, "\n%-------- Any insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst_det( MaybeUseErrorMsgInst, Stream, Lang, "Any",
            InstNumLimit, InstSizeLimit, write_key_any_inst),
        AnyInstPairs, 0, NumAnyInsts, !IO),
    io.format(Stream,
        "\nTotal number of any insts: %d\n", [i(NumAnyInsts)], !IO),

    io.write_string(Stream, "\n%-------- Shared insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst(MaybeUseErrorMsgInst, Stream, Lang, "Shared",
            InstNumLimit, InstSizeLimit, write_inst_name_nl),
        SharedInstPairs, 0, NumSharedInsts, !IO),
    io.format(Stream, "\nTotal number of shared insts: %d\n",
        [i(NumSharedInsts)], !IO),

    io.write_string(Stream, "\n%-------- MostlyUniq insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst(MaybeUseErrorMsgInst, Stream, Lang, "MostlyUniq",
            InstNumLimit, InstSizeLimit, write_inst_name_nl),
        MostlyUniqInstPairs, 0, NumMostlyUniqInsts, !IO),
    io.format(Stream, "\nTotal number of mostly uniq insts: %d\n",
        [i(NumMostlyUniqInsts)], !IO),

    io.nl(Stream, !IO).

:- pred write_user_inst(io.text_output_stream::in,
    pair(inst_ctor, hlds_inst_defn)::in, io::di, io::uo) is det.

write_user_inst(Stream, InstCtor - InstDefn, !IO) :-
    InstCtor = inst_ctor(InstName, _InstArity),
    io.format(Stream, "\n:- inst %s", [s(sym_name_to_string(InstName))], !IO),
    InstDefn = hlds_inst_defn(InstVarSet, InstParams, InstBody,
        _MaybeMatchingTypeCtors, _Context, Status),
    (
        InstParams = []
    ;
        InstParams = [HeadInstParam | TailInstParams],
        io.write_string(Stream, "(", !IO),
        write_inst_params(Stream, HeadInstParam, TailInstParams, InstVarSet,
            !IO),
        io.write_string(Stream, ")", !IO)
    ),
    InstBody = eqv_inst(EqvInst),
    io.write_string(Stream, ":\n", !IO),
    mercury_output_inst(Stream, output_debug, InstVarSet, EqvInst, !IO),
    io.write_string(Stream, "\n", !IO),
    StatusStr = inst_import_status_to_string(Status),
    io.format(Stream, "%% status %s\n", [s(StatusStr)], !IO).

:- pred write_inst_params(io.text_output_stream::in, inst_var::in,
    list(inst_var)::in, inst_varset::in, io::di, io::uo) is det.

write_inst_params(Stream, InstVar, InstVars, InstVarSet, !IO) :-
    varset.lookup_name(InstVarSet, InstVar, InstVarName),
    io.write_string(Stream, InstVarName, !IO),
    (
        InstVars = []
    ;
        InstVars = [HeadInstVar | TailInstVars],
        io.write_string(Stream, ", ", !IO),
        write_inst_params(Stream, HeadInstVar, TailInstVars, InstVarSet, !IO)
    ).

:- pred write_key_maybe_inst(maybe_use_error_msg_inst::in,
    io.text_output_stream::in, output_lang::in, string::in, int::in, int::in,
    pred(maybe_use_error_msg_inst, io.text_output_stream, output_lang, int,
        Key, io, io)::
        in(pred(in, in, in, in, in, di, uo) is det),
    pair(Key, maybe_inst)::in, int::in, int::out, io::di, io::uo) is det.

write_key_maybe_inst(MaybeUseErrorMsgInst, Stream, Lang, Kind,
        InstNumLimit, InstSizeLimit, WriteKey, Key - MaybeInst, !N, !IO) :-
    !:N = !.N + 1,
    ( if !.N =< InstNumLimit then
        io.format(Stream, "\n%s entry %d key\n", [s(Kind), i(!.N)], !IO),
        WriteKey(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit, Key, !IO),
        (
            MaybeInst = inst_unknown,
            io.format(Stream, "%s entry %d value UNKNOWN\n",
                [s(Kind), i(!.N)], !IO)
        ;
            MaybeInst = inst_known(Inst),
            io.format(Stream, "%s entry %d value:\n", [s(Kind), i(!.N)], !IO),
            write_inst_entry(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
                Inst, !IO)
        )
    else
        true
    ).

:- pred write_key_maybe_inst_det(maybe_use_error_msg_inst::in,
    io.text_output_stream::in, output_lang::in, string::in, int::in, int::in,
    pred(maybe_use_error_msg_inst, io.text_output_stream, output_lang, int,
        Key, io, io)::
        in(pred(in, in, in, in, in, di, uo) is det),
    pair(Key, maybe_inst_det)::in, int::in, int::out,
    io::di, io::uo) is det.

write_key_maybe_inst_det(MaybeUseErrorMsgInst, Stream, Lang, Kind,
        InstNumLimit, InstSizeLimit, WriteKey, Key - MaybeInstDet, !N, !IO) :-
    !:N = !.N + 1,
    ( if !.N =< InstNumLimit then
        io.format(Stream, "\n%s entry %d key\n", [s(Kind), i(!.N)], !IO),
        WriteKey(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit, Key, !IO),
        (
            MaybeInstDet = inst_det_unknown,
            io.format(Stream, "%s entry %d value UNKNOWN\n",
                [s(Kind), i(!.N)], !IO)
        ;
            MaybeInstDet = inst_det_known(Inst, Detism),
            DetismStr = determinism_to_string(Detism),
            io.format(Stream, "%s entry %d value (%s):\n",
                [s(Kind), i(!.N), s(DetismStr)], !IO),
            write_inst_entry(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
                Inst, !IO)
        )
    else
        true
    ).

:- pred write_key_unify_inst(maybe_use_error_msg_inst::in,
    io.text_output_stream::in, output_lang::in, int::in,
    unify_inst_info::in, io::di, io::uo) is det.

write_key_unify_inst(MaybeUseErrorMsgInst, Stream, Lang,
        InstSizeLimit, UnifyInstInfo, !IO) :-
    UnifyInstInfo = unify_inst_info(Live, Real, InstA, InstB),
    (
        Live = is_live,
        io.write_string(Stream, "live ", !IO)
    ;
        Live = is_dead,
        io.write_string(Stream, "dead ", !IO)
    ),
    (
        Real = real_unify,
        io.write_string(Stream, "real unify\n", !IO)
    ;
        Real = fake_unify,
        io.write_string(Stream, "fake unify\n", !IO)
    ),
    io.write_string(Stream, "InstA: ", !IO),
    write_inst_entry(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
        InstA, !IO),
    io.write_string(Stream, "InstB: ", !IO),
    write_inst_entry(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
        InstB, !IO).

:- pred write_key_merge_inst(maybe_use_error_msg_inst::in,
    io.text_output_stream::in, output_lang::in, int::in,
    merge_inst_info::in, io::di, io::uo) is det.

write_key_merge_inst(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
        MergeInstInfo, !IO) :-
    MergeInstInfo = merge_inst_info(InstA, InstB),
    io.write_string(Stream, "InstA: ", !IO),
    write_inst_entry(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
        InstA, !IO),
    io.write_string(Stream, "InstB: ", !IO),
    write_inst_entry(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
        InstB, !IO).

:- pred write_key_ground_inst(maybe_use_error_msg_inst::in,
    io.text_output_stream::in, output_lang::in,
    int::in, ground_inst_info::in, io::di, io::uo) is det.

write_key_ground_inst(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
        GroundInstInfo, !IO) :-
    GroundInstInfo = ground_inst_info(InstName, Uniq, Live, Real),
    write_uniq_live_real(Stream, Uniq, Live, Real, !IO),
    write_inst_name_nl(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
        InstName, !IO).

:- pred write_key_any_inst(maybe_use_error_msg_inst::in,
    io.text_output_stream::in, output_lang::in,
    int::in, any_inst_info::in, io::di, io::uo) is det.

write_key_any_inst(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
        AnyInstInfo, !IO) :-
    AnyInstInfo = any_inst_info(InstName, Uniq, Live, Real),
    write_uniq_live_real(Stream, Uniq, Live, Real, !IO),
    write_inst_name_nl(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
        InstName, !IO).

:- pred write_uniq_live_real(io.text_output_stream::in,
    uniqueness::in, is_live::in, unify_is_real::in, io::di, io::uo) is det.

write_uniq_live_real(Stream, Uniq, Live, Real, !IO) :-
    (
        Uniq = shared,
        io.write_string(Stream, "shared ", !IO)
    ;
        Uniq = unique,
        io.write_string(Stream, "unique ", !IO)
    ;
        Uniq = mostly_unique,
        io.write_string(Stream, "mostly_unique ", !IO)
    ;
        Uniq = clobbered,
        io.write_string(Stream, "clobbered", !IO)
    ;
        Uniq = mostly_clobbered,
        io.write_string(Stream, "mostly_clobbered", !IO)
    ),
    (
        Live = is_live,
        io.write_string(Stream, "live ", !IO)
    ;
        Live = is_dead,
        io.write_string(Stream, "dead ", !IO)
    ),
    (
        Real = real_unify,
        io.write_string(Stream, "real unify\n", !IO)
    ;
        Real = fake_unify,
        io.write_string(Stream, "fake unify\n", !IO)
    ).

:- pred write_inst_name_nl(maybe_use_error_msg_inst::in,
    io.text_output_stream::in, output_lang::in, int::in, inst_name::in,
    io::di, io::uo) is det.

write_inst_name_nl(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit, InstName,
        !IO) :-
    (
        MaybeUseErrorMsgInst = do_not_use_error_msg_inst,
        write_inst_name_as_term_nl(Stream, Lang, InstSizeLimit, InstName, !IO)
    ;
        MaybeUseErrorMsgInst = use_error_msg_inst(ModuleInfo),
        varset.init(TVarSet),
        varset.init(InstVarSet),
        ShortInstSuffix = [nl],
        LongInstPrefix = [],
        LongInstSuffix = [nl],
        InstNamePieces = error_msg_inst_name(ModuleInfo, InstVarSet,
            expand_named_insts, uod_developer(TVarSet),
            fixed_short_inst, ShortInstSuffix,
            LongInstPrefix, LongInstSuffix, InstName),
        ShortInstNameStr = error_pieces_to_one_line_string(InstNamePieces),
        ( if string.count_code_points(ShortInstNameStr) < 80 then
            io.format(Stream, "%s\n", [s(ShortInstNameStr)], !IO)
        else
            Prefix = "",
            LongInstNameStr =
                error_pieces_to_multi_line_string(Prefix, InstNamePieces),
            io.format(Stream, "%s", [s(LongInstNameStr)], !IO)
        )
    ).

:- pred write_inst_entry(maybe_use_error_msg_inst::in,
    io.text_output_stream::in, output_lang::in, int::in, mer_inst::in,
    io::di, io::uo) is det.

write_inst_entry(MaybeUseErrorMsgInst, Stream, Lang, InstSizeLimit,
        Inst, !IO) :-
    (
        MaybeUseErrorMsgInst = do_not_use_error_msg_inst,
        write_inst_as_term_nl(Stream, Lang, InstSizeLimit, Inst, !IO)
    ;
        MaybeUseErrorMsgInst = use_error_msg_inst(ModuleInfo),
        varset.init(TVarSet),
        varset.init(InstVarSet),
        ShortInstSuffix = [nl],
        LongInstPrefix = [],
        LongInstSuffix = [nl],
        InstPieces = error_msg_inst(ModuleInfo, InstVarSet,
            expand_named_insts, uod_developer(TVarSet),
            fixed_short_inst, ShortInstSuffix,
            LongInstPrefix, LongInstSuffix, Inst),
        ShortInstStr = error_pieces_to_one_line_string(InstPieces),
        ( if string.count_code_points(ShortInstStr) < 80 then
            io.format(Stream, "%s\n", [s(ShortInstStr)], !IO)
        else
            Prefix = "",
            LongInstStr =
                error_pieces_to_multi_line_string(Prefix, InstPieces),
            io.format(Stream, "%s", [s(LongInstStr)], !IO)
        )
    ).

:- pred write_inst_name_as_term_nl(io.text_output_stream::in, output_lang::in,
    int::in, inst_name::in, io::di, io::uo) is det.

write_inst_name_as_term_nl(Stream, Lang, InstSizeLimit, InstName, !IO) :-
    InstNameTerm = inst_name_to_limited_size_term(Lang, InstSizeLimit,
        InstName),
    varset.init(InstVarSet),
    mercury_output_term_vs(InstVarSet, print_name_only, InstNameTerm,
        Stream, !IO),
    io.nl(Stream, !IO).

:- pred write_inst_as_term_nl(io.text_output_stream::in, output_lang::in,
    int::in, mer_inst::in, io::di, io::uo) is det.

write_inst_as_term_nl(Stream, Lang, InstSizeLimit, Inst, !IO) :-
    InstTerm = inst_to_limited_size_term(Lang, InstSizeLimit, Inst),
    varset.init(InstVarSet),
    mercury_output_term_vs(InstVarSet, print_name_only, InstTerm,
        Stream, !IO),
    io.nl(Stream, !IO).

%---------------------------------------------------------------------------%
%
% Write out the mode table.
%

write_mode_table(Stream, ModeTable, !IO) :-
    mode_table_get_mode_defns(ModeTable, ModeDefns),
    io.write_string(Stream, "%-------- Modes --------\n", !IO),
    map.foldl(write_mode_table_entry(Stream), ModeDefns, !IO),
    io.nl(Stream, !IO).

:- pred write_mode_table_entry(io.text_output_stream::in,
    mode_ctor::in, hlds_mode_defn::in, io::di, io::uo) is det.

write_mode_table_entry(Stream, ModeCtor, ModeDefn, !IO) :-
    ModeCtor = mode_ctor(ModeName, _ModeArity),
    io.format(Stream, "\n:- mode %s", [s(sym_name_to_string(ModeName))], !IO),
    ModeDefn = hlds_mode_defn(InstVarSet, InstParams, ModeBody, _Context,
        Status),
    (
        InstParams = []
    ;
        InstParams = [HeadInstParam | TailInstParams],
        io.write_string(Stream, "(", !IO),
        write_inst_params(Stream, HeadInstParam, TailInstParams,
            InstVarSet, !IO),
        io.write_string(Stream, ")", !IO)
    ),
    ModeBody = hlds_mode_body(EqvMode),
    io.write_string(Stream, ":\n", !IO),
    mercury_output_mode(Stream, output_debug, InstVarSet, EqvMode, !IO),
    io.write_string(Stream, "\n", !IO),
    StatusStr = mode_import_status_to_string(Status),
    io.format(Stream, "%% status %s\n", [s(StatusStr)], !IO).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_inst_table.
%---------------------------------------------------------------------------%
