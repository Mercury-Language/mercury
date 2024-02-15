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
:- import_module string.
:- import_module string.builder.

:- type maybe_use_error_msg_inst
    --->    do_not_use_error_msg_inst
    ;       use_error_msg_inst(module_info).

:- pred write_inst_table(io.text_output_stream::in, output_lang::in,
    maybe_use_error_msg_inst::in, int::in, int::in, inst_table::in,
    io::di, io::uo) is det.
:- pred format_inst_table(output_lang::in,
    maybe_use_error_msg_inst::in, int::in, int::in, inst_table::in,
    string.builder.state::di, string.builder.state::uo) is det.

:- pred write_mode_table(io.text_output_stream::in, mode_table::in,
    io::di, io::uo) is det.
:- pred format_mode_table(mode_table::in,
    string.builder.state::di, string.builder.state::uo) is det.

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
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

write_inst_table(Stream, Lang, MaybeUseErrorMsgInst,
        InstNumLimit, InstSizeLimit, InstTable, !IO) :-
    State0 = string.builder.init,
    format_inst_table(Lang, MaybeUseErrorMsgInst, InstNumLimit, InstSizeLimit,
        InstTable, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_inst_table(Lang, MaybeUseErrorMsgInst, InstNumLimit, InstSizeLimit,
        InstTable, !State) :-
    string.builder.append_string("%-------- Insts --------\n", !State),

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

    string.builder.append_string(
        "\n%-------- User defined insts --------\n", !State),
    list.foldl(format_user_inst, UserInstPairs, !State),

    string.builder.append_string(
        "\n%-------- Unify insts --------\n", !State),
    list.foldl2(
        format_key_maybe_inst_det(MaybeUseErrorMsgInst, Lang, "Unify",
            InstNumLimit, InstSizeLimit, format_key_unify_inst),
        UnifyInstPairs, 0, NumUnifyInsts, !State),
    string.builder.format("\nTotal number of unify insts: %d\n",
        [i(NumUnifyInsts)], !State),

    string.builder.append_string(
        "\n%-------- Merge insts --------\n", !State),
    list.foldl2(
        format_key_maybe_inst(MaybeUseErrorMsgInst, Lang, "Merge",
            InstNumLimit, InstSizeLimit, format_key_merge_inst),
        MergeInstPairs, 0, NumMergeInsts, !State),
    string.builder.format("\nTotal number of merge insts: %d\n",
        [i(NumMergeInsts)], !State),

    string.builder.append_string(
        "\n%-------- Ground insts --------\n", !State),
    list.foldl2(
        format_key_maybe_inst_det(MaybeUseErrorMsgInst, Lang, "Ground",
            InstNumLimit, InstSizeLimit, format_key_ground_inst),
        GroundInstPairs, 0, NumGroundInsts, !State),
    string.builder.format("\nTotal number of ground insts: %d\n",
        [i(NumGroundInsts)], !State),

    string.builder.append_string(
        "\n%-------- Any insts --------\n", !State),
    list.foldl2(
        format_key_maybe_inst_det( MaybeUseErrorMsgInst, Lang, "Any",
            InstNumLimit, InstSizeLimit, format_key_any_inst),
        AnyInstPairs, 0, NumAnyInsts, !State),
    string.builder.format("\nTotal number of any insts: %d\n",
        [i(NumAnyInsts)], !State),

    string.builder.append_string(
        "\n%-------- Shared insts --------\n", !State),
    list.foldl2(
        format_key_maybe_inst(MaybeUseErrorMsgInst, Lang, "Shared",
            InstNumLimit, InstSizeLimit, format_inst_name_nl),
        SharedInstPairs, 0, NumSharedInsts, !State),
    string.builder.format("\nTotal number of shared insts: %d\n",
        [i(NumSharedInsts)], !State),

    string.builder.append_string(
        "\n%-------- MostlyUniq insts --------\n", !State),
    list.foldl2(
        format_key_maybe_inst(MaybeUseErrorMsgInst, Lang, "MostlyUniq",
            InstNumLimit, InstSizeLimit, format_inst_name_nl),
        MostlyUniqInstPairs, 0, NumMostlyUniqInsts, !State),
    string.builder.format("\nTotal number of mostly uniq insts: %d\n",
        [i(NumMostlyUniqInsts)], !State),

    string.builder.append_string("\n", !State).

:- pred format_user_inst(pair(inst_ctor, hlds_inst_defn)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_user_inst(InstCtor - InstDefn, !State) :-
    InstCtor = inst_ctor(InstName, _InstArity),
    string.builder.format("\n:- inst %s",
        [s(sym_name_to_string(InstName))], !State),
    InstDefn = hlds_inst_defn(InstVarSet, InstParams, InstBody,
        _MaybeMatchingTypeCtors, _Context, Status),
    (
        InstParams = []
    ;
        InstParams = [HeadInstParam | TailInstParams],
        string.builder.append_string("(", !State),
        format_inst_params(InstVarSet, HeadInstParam, TailInstParams, !State),
        string.builder.append_string(")", !State)
    ),
    InstBody = eqv_inst(EqvInst),
    string.builder.append_string(":\n", !State),
    mercury_format_inst(output_debug, InstVarSet, EqvInst,
        string.builder.handle, !State),
    string.builder.append_string("\n", !State),
    StatusStr = inst_import_status_to_string(Status),
    string.builder.format("%% status %s\n", [s(StatusStr)], !State).

:- pred format_inst_params(inst_varset::in, inst_var::in, list(inst_var)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_inst_params(InstVarSet, InstVar, InstVars, !State) :-
    varset.lookup_name(InstVarSet, InstVar, InstVarName),
    string.builder.append_string(InstVarName, !State),
    (
        InstVars = []
    ;
        InstVars = [HeadInstVar | TailInstVars],
        string.builder.append_string(", ", !State),
        format_inst_params(InstVarSet, HeadInstVar, TailInstVars, !State)
    ).

:- pred format_key_maybe_inst(maybe_use_error_msg_inst::in,
    output_lang::in, string::in, int::in, int::in,
    pred(maybe_use_error_msg_inst, output_lang, int, Key,
        string.builder.state, string.builder.state)::
        in(pred(in, in, in, in, di, uo) is det),
    pair(Key, maybe_inst)::in, int::in, int::out,
    string.builder.state::di, string.builder.state::uo) is det.

format_key_maybe_inst(MaybeUseErrorMsgInst, Lang, Kind,
        InstNumLimit, InstSizeLimit, WriteKey, Key - MaybeInst, !N, !State) :-
    !:N = !.N + 1,
    ( if !.N =< InstNumLimit then
        string.builder.format("\n%s entry %d key\n",
            [s(Kind), i(!.N)], !State),
        WriteKey(MaybeUseErrorMsgInst, Lang, InstSizeLimit, Key, !State),
        (
            MaybeInst = inst_unknown,
            string.builder.format("%s entry %d value UNKNOWN\n",
                [s(Kind), i(!.N)], !State)
        ;
            MaybeInst = inst_known(Inst),
            string.builder.format("%s entry %d value:\n",
                [s(Kind), i(!.N)], !State),
            format_inst_entry(MaybeUseErrorMsgInst, Lang, InstSizeLimit,
                Inst, !State)
        )
    else
        true
    ).

:- pred format_key_maybe_inst_det(maybe_use_error_msg_inst::in,
    output_lang::in, string::in, int::in, int::in,
    pred(maybe_use_error_msg_inst, output_lang, int, Key,
        string.builder.state, string.builder.state)::
        in(pred(in, in, in, in, di, uo) is det),
    pair(Key, maybe_inst_det)::in, int::in, int::out,
    string.builder.state::di, string.builder.state::uo) is det.

format_key_maybe_inst_det(MaybeUseErrorMsgInst, Lang, Kind,
        InstNumLimit, InstSizeLimit, WriteKey, Key - MaybeInstDet,
        !N, !State) :-
    !:N = !.N + 1,
    ( if !.N =< InstNumLimit then
        string.builder.format("\n%s entry %d key\n",
            [s(Kind), i(!.N)], !State),
        WriteKey(MaybeUseErrorMsgInst, Lang, InstSizeLimit, Key, !State),
        (
            MaybeInstDet = inst_det_unknown,
            string.builder.format("%s entry %d value UNKNOWN\n",
                [s(Kind), i(!.N)], !State)
        ;
            MaybeInstDet = inst_det_known(Inst, Detism),
            DetismStr = determinism_to_string(Detism),
            string.builder.format("%s entry %d value (%s):\n",
                [s(Kind), i(!.N), s(DetismStr)], !State),
            format_inst_entry(MaybeUseErrorMsgInst, Lang, InstSizeLimit,
                Inst, !State)
        )
    else
        true
    ).

:- pred format_key_unify_inst(maybe_use_error_msg_inst::in, output_lang::in,
    int::in, unify_inst_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_key_unify_inst(MaybeUseErrorMsgInst, Lang, InstSizeLimit,
        UnifyInstInfo, !State) :-
    UnifyInstInfo = unify_inst_info(Live, Real, InstA, InstB),
    (
        Live = is_live,
        string.builder.append_string("live ", !State)
    ;
        Live = is_dead,
        string.builder.append_string("dead ", !State)
    ),
    (
        Real = real_unify,
        string.builder.append_string("real unify\n", !State)
    ;
        Real = fake_unify,
        string.builder.append_string("fake unify\n", !State)
    ),
    string.builder.append_string("InstA: ", !State),
    format_inst_entry(MaybeUseErrorMsgInst, Lang, InstSizeLimit,
        InstA, !State),
    string.builder.append_string("InstB: ", !State),
    format_inst_entry(MaybeUseErrorMsgInst, Lang, InstSizeLimit,
        InstB, !State).

:- pred format_key_merge_inst(maybe_use_error_msg_inst::in, output_lang::in,
    int::in, merge_inst_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_key_merge_inst(MaybeUseErrorMsgInst, Lang, InstSizeLimit,
        MergeInstInfo, !State) :-
    MergeInstInfo = merge_inst_info(InstA, InstB),
    string.builder.append_string("InstA: ", !State),
    format_inst_entry(MaybeUseErrorMsgInst, Lang, InstSizeLimit,
        InstA, !State),
    string.builder.append_string("InstB: ", !State),
    format_inst_entry(MaybeUseErrorMsgInst, Lang, InstSizeLimit,
        InstB, !State).

:- pred format_key_ground_inst(maybe_use_error_msg_inst::in, output_lang::in,
    int::in, ground_inst_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_key_ground_inst(MaybeUseErrorMsgInst, Lang, InstSizeLimit,
        GroundInstInfo, !State) :-
    GroundInstInfo = ground_inst_info(InstName, Uniq, Live, Real),
    format_uniq_live_real(Uniq, Live, Real, !State),
    format_inst_name_nl(MaybeUseErrorMsgInst, Lang, InstSizeLimit,
        InstName, !State).

:- pred format_key_any_inst(maybe_use_error_msg_inst::in, output_lang::in,
    int::in, any_inst_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_key_any_inst(MaybeUseErrorMsgInst, Lang, InstSizeLimit, AnyInstInfo,
        !State) :-
    AnyInstInfo = any_inst_info(InstName, Uniq, Live, Real),
    format_uniq_live_real(Uniq, Live, Real, !State),
    format_inst_name_nl(MaybeUseErrorMsgInst, Lang, InstSizeLimit,
        InstName, !State).

:- pred format_uniq_live_real(uniqueness::in, is_live::in, unify_is_real::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_uniq_live_real(Uniq, Live, Real, !State) :-
    (
        Uniq = shared,
        string.builder.append_string("shared ", !State)
    ;
        Uniq = unique,
        string.builder.append_string("unique ", !State)
    ;
        Uniq = mostly_unique,
        string.builder.append_string("mostly_unique ", !State)
    ;
        Uniq = clobbered,
        string.builder.append_string("clobbered", !State)
    ;
        Uniq = mostly_clobbered,
        string.builder.append_string("mostly_clobbered", !State)
    ),
    (
        Live = is_live,
        string.builder.append_string("live ", !State)
    ;
        Live = is_dead,
        string.builder.append_string("dead ", !State)
    ),
    (
        Real = real_unify,
        string.builder.append_string("real unify\n", !State)
    ;
        Real = fake_unify,
        string.builder.append_string("fake unify\n", !State)
    ).

:- pred format_inst_name_nl(maybe_use_error_msg_inst::in,
    output_lang::in, int::in, inst_name::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_inst_name_nl(MaybeUseErrorMsgInst, Lang, InstSizeLimit, InstName,
        !State) :-
    (
        MaybeUseErrorMsgInst = do_not_use_error_msg_inst,
        format_inst_name_as_term_nl(Lang, InstSizeLimit, InstName, !State)
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
        InstNameLines = error_pieces_to_std_lines(InstNamePieces),
        ( if do_lines_fit_in_n_code_points(80, InstNameLines) then
            ShortInstNameStr = error_lines_to_one_line_string(InstNameLines),
            string.builder.format("%s\n", [s(ShortInstNameStr)], !State)
        else
            Prefix = "",
            LongInstNameStr =
                error_lines_to_multi_line_string(Prefix, InstNameLines),
            string.builder.format("%s", [s(LongInstNameStr)], !State)
        )
    ).

:- pred format_inst_entry(maybe_use_error_msg_inst::in,
    output_lang::in, int::in, mer_inst::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_inst_entry(MaybeUseErrorMsgInst, Lang, InstSizeLimit, Inst, !State) :-
    (
        MaybeUseErrorMsgInst = do_not_use_error_msg_inst,
        format_inst_as_term_nl(Lang, InstSizeLimit, Inst, !State)
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
        InstLines = error_pieces_to_std_lines(InstPieces),
        ( if do_lines_fit_in_n_code_points(80, InstLines) then
            ShortInstStr = error_lines_to_one_line_string(InstLines),
            string.builder.format("%s\n", [s(ShortInstStr)], !State)
        else
            Prefix = "",
            LongInstStr = error_lines_to_multi_line_string(Prefix, InstLines),
            string.builder.format("%s", [s(LongInstStr)], !State)
        )
    ).

:- pred format_inst_name_as_term_nl(output_lang::in, int::in, inst_name::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_inst_name_as_term_nl(Lang, InstSizeLimit, InstName, !State) :-
    InstNameTerm = inst_name_to_limited_size_term(Lang, InstSizeLimit,
        InstName),
    varset.init(InstVarSet),
    mercury_format_term_vs(InstVarSet, print_name_only, InstNameTerm,
        string.builder.handle, !State),
    string.builder.append_string("\n", !State).

:- pred format_inst_as_term_nl(output_lang::in, int::in, mer_inst::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_inst_as_term_nl(Lang, InstSizeLimit, Inst, !State) :-
    InstTerm = inst_to_limited_size_term(Lang, InstSizeLimit, Inst),
    varset.init(InstVarSet),
    mercury_format_term_vs(InstVarSet, print_name_only, InstTerm,
        string.builder.handle, !State),
    string.builder.append_string("\n", !State).

%---------------------------------------------------------------------------%
%
% Write out the mode table.
%

write_mode_table(Stream, ModeTable, !IO) :-
    State0 = string.builder.init,
    format_mode_table(ModeTable, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_mode_table(ModeTable, !State) :-
    mode_table_get_mode_defns(ModeTable, ModeDefns),
    string.builder.append_string("%-------- Modes --------\n", !State),
    map.foldl(format_mode_table_entry, ModeDefns, !State),
    string.builder.append_string("\n", !State).

:- pred format_mode_table_entry(mode_ctor::in, hlds_mode_defn::in,
    string.builder.state::di, string.builder.state::uo) is det.
 
format_mode_table_entry(ModeCtor, ModeDefn, !State) :-
    ModeCtor = mode_ctor(ModeName, _ModeArity),
    string.builder.format("\n:- mode %s",
        [s(sym_name_to_string(ModeName))], !State),
    ModeDefn = hlds_mode_defn(InstVarSet, InstParams, ModeBody, _Context,
        Status),
    (
        InstParams = []
    ;
        InstParams = [HeadInstParam | TailInstParams],
        string.builder.append_string("(", !State),
        format_inst_params(InstVarSet, HeadInstParam, TailInstParams, !State),
        string.builder.append_string(")", !State)
    ),
    ModeBody = hlds_mode_body(EqvMode),
    string.builder.append_string(":\n", !State),
    mercury_format_mode(output_debug, InstVarSet, EqvMode,
        string.builder.handle, !State),
    string.builder.append_string("\n", !State),
    StatusStr = mode_import_status_to_string(Status),
    string.builder.format("%% status %s\n", [s(StatusStr)], !State).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_inst_table.
%---------------------------------------------------------------------------%
