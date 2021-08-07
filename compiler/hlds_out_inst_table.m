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
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.

:- pred write_inst_table(io.text_output_stream::in, output_lang::in,
    int::in, inst_table::in, io::di, io::uo) is det.

:- pred write_mode_table(io.text_output_stream::in, mode_table::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_out.

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

write_inst_table(Stream, Lang, Limit, InstTable, !IO) :-
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

    io.write_string(Stream, "%-------- User defined insts --------\n", !IO),
    list.foldl(write_user_inst(Stream), UserInstPairs, !IO),

    io.write_string(Stream, "%-------- Unify insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst_det(Stream, Lang, Limit, write_key_unify_inst),
        UnifyInstPairs, 0, NumUnifyInsts, !IO),
    io.format(Stream,
        "\nTotal number of unify insts: %d\n", [i(NumUnifyInsts)], !IO),

    io.write_string(Stream, "%-------- Merge insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst(Stream, Lang, Limit, write_key_merge_inst),
        MergeInstPairs, 0, NumMergeInsts, !IO),
    io.format(Stream,
        "\nTotal number of merge insts: %d\n", [i(NumMergeInsts)], !IO),

    io.write_string(Stream, "%-------- Ground insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst_det(Stream, Lang, Limit, write_key_ground_inst),
        GroundInstPairs, 0, NumGroundInsts, !IO),
    io.format(Stream, "\nTotal number of ground insts: %d\n",
        [i(NumGroundInsts)], !IO),

    io.write_string(Stream, "%-------- Any insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst_det(Stream, Lang, Limit, write_key_any_inst),
        AnyInstPairs, 0, NumAnyInsts, !IO),
    io.format(Stream,
        "\nTotal number of any insts: %d\n", [i(NumAnyInsts)], !IO),

    io.write_string(Stream, "%-------- Shared insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst(Stream, Lang, Limit, write_inst_name_nl),
        SharedInstPairs, 0, NumSharedInsts, !IO),
    io.format(Stream, "\nTotal number of shared insts: %d\n",
        [i(NumSharedInsts)], !IO),

    io.write_string(Stream, "%-------- MostlyUniq insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst(Stream, Lang, Limit, write_inst_name_nl),
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

:- pred write_key_maybe_inst(io.text_output_stream::in, output_lang::in,
    int::in,
    pred(output_lang, Key, io.text_output_stream, io, io)::
        in(pred(in, in, in, di, uo) is det),
    pair(Key, maybe_inst)::in, int::in, int::out, io::di, io::uo) is det.

write_key_maybe_inst(Stream, Lang, Limit, WriteKey, Key - MaybeInst,
        !N, !IO) :-
    !:N = !.N + 1,
    ( if !.N =< Limit then
        io.format(Stream, "\nEntry %d key\n", [i(!.N)], !IO),
        WriteKey(Lang, Key, Stream, !IO),
        (
            MaybeInst = inst_unknown,
            io.format(Stream, "Entry %d value UNKNOWN\n", [i(!.N)], !IO)
        ;
            MaybeInst = inst_known(Inst),
            io.format(Stream, "Entry %d value:\n", [i(!.N)], !IO),
            write_inst(Stream, Lang, Inst, !IO),
            io.nl(Stream, !IO)
        )
    else
        true
    ).

:- pred write_key_maybe_inst_det(io.text_output_stream::in, output_lang::in,
    int::in,
    pred(output_lang, Key, io.text_output_stream, io, io)::
        in(pred(in, in, in, di, uo) is det),
    pair(Key, maybe_inst_det)::in, int::in, int::out,
    io::di, io::uo) is det.

write_key_maybe_inst_det(Stream, Lang, Limit, WriteKey, Key - MaybeInstDet,
        !N, !IO) :-
    !:N = !.N + 1,
    ( if !.N =< Limit then
        io.format(Stream, "\nEntry %d key\n", [i(!.N)], !IO),
        WriteKey(Lang, Key, Stream, !IO),
        (
            MaybeInstDet = inst_det_unknown,
            io.format(Stream, "Entry %d value UNKNOWN\n", [i(!.N)], !IO)
        ;
            MaybeInstDet = inst_det_known(Inst, Detism),
            DetismStr = determinism_to_string(Detism),
            io.format(Stream, "Entry %d value (%s):\n",
                [i(!.N), s(DetismStr)], !IO),
            write_inst(Stream, Lang, Inst, !IO),
            io.nl(Stream, !IO)
        )
    else
        true
    ).

:- pred write_key_unify_inst(output_lang::in, unify_inst_info::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_key_unify_inst(Lang, UnifyInstInfo, Stream, !IO) :-
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
    write_inst(Stream, Lang, InstA, !IO),
    io.nl(Stream, !IO),
    io.write_string(Stream, "InstB: ", !IO),
    write_inst(Stream, Lang, InstB, !IO),
    io.nl(Stream, !IO).

:- pred write_key_merge_inst(output_lang::in, merge_inst_info::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_key_merge_inst(Lang, MergeInstInfo, Stream, !IO) :-
    MergeInstInfo = merge_inst_info(InstA, InstB),
    io.write_string(Stream, "InstA: ", !IO),
    write_inst(Stream, Lang, InstA, !IO),
    io.nl(Stream, !IO),
    io.write_string(Stream, "InstB: ", !IO),
    write_inst(Stream, Lang, InstB, !IO),
    io.nl(Stream, !IO).

:- pred write_key_ground_inst(output_lang::in, ground_inst_info::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_key_ground_inst(Lang, GroundInstInfo, Stream, !IO) :-
    GroundInstInfo = ground_inst_info(InstName, Uniq, Live, Real),
    write_uniq_live_real(Stream, Uniq, Live, Real, !IO),
    write_inst_name_nl(Lang, InstName, Stream, !IO).

:- pred write_key_any_inst(output_lang::in, any_inst_info::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_key_any_inst(Lang, AnyInstInfo, Stream, !IO) :-
    AnyInstInfo = any_inst_info(InstName, Uniq, Live, Real),
    write_uniq_live_real(Stream, Uniq, Live, Real, !IO),
    write_inst_name_nl(Lang, InstName, Stream, !IO).

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

:- pred write_inst_name_nl(output_lang::in, inst_name::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_inst_name_nl(Lang, InstName, Stream, !IO) :-
    InstNameTerm = inst_name_to_term(Lang, InstName),
    varset.init(VarSet),
    mercury_output_term(VarSet, print_name_only, InstNameTerm, Stream, !IO),
    io.nl(Stream, !IO).

:- pred write_inst(io.text_output_stream::in, output_lang::in, mer_inst::in,
    io::di, io::uo) is det.

write_inst(Stream, Lang, Inst, !IO) :-
    InstTerm = inst_to_term(Lang, Inst),
    varset.init(VarSet),
    mercury_output_term(VarSet, print_name_only, InstTerm, Stream, !IO).

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
