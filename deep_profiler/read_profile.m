%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module contains code for reading in a deep profiling data file.
% Such files, named Deep.data, are created by deep profiled executables.

:- module read_profile.

:- interface.

:- import_module profile.

:- import_module io.
:- import_module maybe.

:- pred read_call_graph(string::in, maybe_error(initial_deep)::out,
    io::di, io::uo) is det.

:- implementation.

:- import_module array_util.
:- import_module io_combinator.
:- import_module measurements.

:- import_module array.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- type maybe_error2(T1, T2)
    --->    ok2(T1, T2)
    ;       error2(string).

:- type ptr_kind
    --->    ps
    ;       pd
    ;       css
    ;       csd.

read_call_graph(FileName, Res, !IO) :-
    io.see_binary(FileName, Res0, !IO),
    (
        Res0 = ok,
        read_id_string(Res1, !IO),
        (
            Res1 = ok(_),
            io_combinator.maybe_error_sequence_11(
                read_fixed_size_int,
                read_fixed_size_int,
                read_fixed_size_int,
                read_fixed_size_int,
                read_num,
                read_num,
                read_num,
                read_num,
                read_deep_byte,
                read_deep_byte,
                read_ptr(pd),
                (pred(MaxCSD::in, MaxCSS::in,
                        MaxPD::in, MaxPS::in,
                        TicksPerSec::in,
                        InstrumentQuanta::in,
                        UserQuanta::in,
                        NumCallSeqs::in,
                        WordSize::in,
                        CanonicalFlag::in,
                        RootPDI::in,
                        ResInitDeep::out) is det :-
                    InitDeep0 = init_deep(MaxCSD, MaxCSS,
                        MaxPD, MaxPS,
                        TicksPerSec, InstrumentQuanta, UserQuanta,
                        NumCallSeqs,
                        WordSize, CanonicalFlag,
                        RootPDI),
                    ResInitDeep = ok(InitDeep0)
                ),
                Res2, !IO),
            (
                Res2 = ok(InitDeep),
                read_nodes(InitDeep, Res, !IO),
                io.seen_binary(!IO)
            ;
                Res2 = error(Err),
                Res = error(Err)
            )
        ;
            Res1 = error(Msg),
            Res = error(Msg)
        )
    ;
        Res0 = error(Err),
        io.error_message(Err, Msg),
        Res = error(Msg)
    ).

:- pred read_id_string(maybe_error(string)::out,
    io::di, io::uo) is det.

read_id_string(Res, !IO) :-
    read_n_byte_string(string.length(id_string), Res0, !IO),
    (
        Res0 = ok(String),
        ( String = id_string ->
            Res = ok(id_string)
        ;
            Res = error("not a deep profiling data file")
        )
    ;
        Res0 = error(Err),
        Res = error(Err)
    ).

:- func id_string = string.

% This must the same string as the one written by MR_write_out_id_string
% in runtime/mercury_deep_profiling.c.

id_string = "Mercury deep profiler data version 2\n".

:- func init_deep(int, int, int, int, int, int, int, int, int, int, int)
    = initial_deep.

init_deep(MaxCSD, MaxCSS, MaxPD, MaxPS, TicksPerSec, InstrumentQuanta,
        UserQuanta, NumCallSeqs, WordSize, CanonicalByte, RootPDI)
        = InitDeep :-
    ( CanonicalByte = 0 ->
        CanonicalFlag = no
    ;
        CanonicalFlag = yes
    ),
    InitStats = profile_stats(MaxCSD, MaxCSS, MaxPD, MaxPS, TicksPerSec,
        InstrumentQuanta, UserQuanta, NumCallSeqs, WordSize, CanonicalFlag),
    InitDeep = initial_deep(
        InitStats,
        make_pdptr(RootPDI),
        array.init(MaxCSD + 1,
            call_site_dynamic(
                make_dummy_pdptr,
                make_dummy_pdptr,
                zero_own_prof_info
            )),
        array.init(MaxPD + 1, proc_dynamic(make_dummy_psptr, array([]))),
        array.init(MaxCSS + 1,
            call_site_static(
                make_dummy_psptr, -1,
                normal_call_and_callee(make_dummy_psptr, ""), -1, ""
            )),
        array.init(MaxPS + 1,
            proc_static(dummy_proc_id, "", "", "", "", -1, no,
                array([]), not_zeroed))
    ).

:- pred read_nodes(initial_deep::in, maybe_error(initial_deep)::out,
    io::di, io::uo) is det.

read_nodes(InitDeep0, Res, !IO) :-
    read_byte(Res0, !IO),
    (
        Res0 = ok(Byte),
        ( Byte = token_call_site_dynamic ->
            read_call_site_dynamic(Res1, !IO),
            (
                Res1 = ok2(CallSiteDynamic, CSDI),
                deep_insert(InitDeep0 ^ init_call_site_dynamics,
                    CSDI, CallSiteDynamic, CSDs),
                InitDeep1 = InitDeep0 ^ init_call_site_dynamics := CSDs,
                read_nodes(InitDeep1, Res, !IO)
            ;
                Res1 = error2(Err),
                Res = error(Err)
            )
        ; Byte = token_proc_dynamic ->
            read_proc_dynamic(Res1, !IO),
            (
                Res1 = ok2(ProcDynamic, PDI),
                deep_insert(InitDeep0 ^ init_proc_dynamics,
                    PDI, ProcDynamic, PDs),
                InitDeep1 = InitDeep0 ^ init_proc_dynamics := PDs,
                read_nodes(InitDeep1, Res, !IO)
            ;
                Res1 = error2(Err),
                Res = error(Err)
            )
        ; Byte = token_call_site_static ->
            read_call_site_static(Res1, !IO),
            (
                Res1 = ok2(CallSiteStatic, CSSI),
                deep_insert(InitDeep0 ^ init_call_site_statics,
                    CSSI, CallSiteStatic, CSSs),
                InitDeep1 = InitDeep0 ^ init_call_site_statics := CSSs,
                read_nodes(InitDeep1, Res, !IO)
            ;
                Res1 = error2(Err),
                Res = error(Err)
            )
        ; Byte = token_proc_static ->
            read_proc_static(Res1, !IO),
            (
                Res1 = ok2(ProcStatic, PSI),
                deep_insert(InitDeep0 ^ init_proc_statics,
                    PSI, ProcStatic, PSs),
                InitDeep1 = InitDeep0 ^ init_proc_statics := PSs,
                read_nodes(InitDeep1, Res, !IO)
            ;
                Res1 = error2(Err),
                Res = error(Err)
            )
        ;
            format("unexpected token %d", [i(Byte)], Msg),
            Res = error(Msg)
        )
    ;
        Res0 = eof,
        Res = ok(InitDeep0)
    ;
        Res0 = error(Err),
        io.error_message(Err, Msg),
        Res = error(Msg)
    ).

:- pred read_call_site_static(maybe_error2(call_site_static, int)::out,
    io::di, io::uo) is det.

read_call_site_static(Res, !IO) :-
    % DEBUGSITE
    % io.write_string("reading call_site_static.\n"),
    io_combinator.maybe_error_sequence_4(
        read_ptr(css),
        read_call_site_kind_and_callee,
        read_num,
        read_string,
        (pred(CSSI0::in, Kind::in, LineNumber::in, Str::in, Res0::out)
                is det :-
            DummyPSPtr = make_dummy_psptr,
            DummySlotNum = -1,
            CallSiteStatic0 = call_site_static(DummyPSPtr,
                DummySlotNum, Kind, LineNumber, Str),
            Res0 = ok({CallSiteStatic0, CSSI0})
        ),
        Res1, !IO),
    (
        Res1 = ok({CallSiteStatic, CSSI}),
        Res = ok2(CallSiteStatic, CSSI)
        % DEBUGSITE
        % io.write_string("read call_site_static ", !IO),
        % io.write_int(CSSI, !IO),
        % io.write_string(": ", !IO),
        % io.write(CallSiteStatic, !IO),
        % io.write_string("\n", !IO)
    ;
        Res1 = error(Err),
        Res = error2(Err)
    ).


:- pred read_proc_static(maybe_error2(proc_static, int)::out,
    io::di, io::uo) is det.

read_proc_static(Res, !IO) :-
    % DEBUGSITE
    % io.write_string("reading proc_static.\n", !IO),
    io_combinator.maybe_error_sequence_6(
        read_ptr(ps),
        read_proc_id,
        read_string,
        read_num,
        read_deep_byte,
        read_num,
        (pred(PSI0::in, Id0::in, F0::in, L0::in, I0::in,
                N0::in, Stuff0::out) is det :-
            Stuff0 = ok({PSI0, Id0, F0, L0, I0, N0})
        ),
        Res1, !IO),
    (
        Res1 = ok({PSI, Id, FileName, LineNumber, Interface, N}),
        read_n_things(N, read_ptr(css), Res2, !IO),
        (
            Res2 = ok(CSSIs),
            CSSPtrs = list.map(make_cssptr, CSSIs),
            DeclModule = decl_module(Id),
            RefinedStr = refined_proc_id_to_string(Id),
            RawStr = raw_proc_id_to_string(Id),
            % The `not_zeroed' for whether the procedure's
            % proc_static is ever zeroed is the default. The
            % startup phase will set it to `zeroed' in the
            % proc_statics which are ever zeroed.
            ( Interface = 0 ->
                IsInInterface = no
            ;
                IsInInterface = yes
            ),
            ProcStatic = proc_static(Id, DeclModule,
                RefinedStr, RawStr, FileName, LineNumber,
                IsInInterface, array(CSSPtrs), not_zeroed),
            Res = ok2(ProcStatic, PSI)
            % DEBUGSITE
            % io.write_string("read proc_static ", !IO),
            % io.write_int(PSI, !IO),
            % io.write_string(": ", !IO),
            % io.write(ProcStatic, !IO),
            % io.write_string("\n", !IO)
        ;
            Res2 = error(Err),
            Res = error2(Err)
        )
    ;
        Res1 = error(Err),
        Res = error2(Err)
    ).

:- pred read_proc_id(maybe_error(proc_id)::out,
    io::di, io::uo) is det.

read_proc_id(Res, !IO) :-
    read_deep_byte(Res0, !IO),
    (
        Res0 = ok(Byte),
        ( Byte = token_isa_uci_pred ->
            read_proc_id_uci_pred(Res, !IO)
        ; Byte = token_isa_predicate ->
            read_proc_id_user_defined(predicate, Res, !IO)
        ; Byte = token_isa_function ->
            read_proc_id_user_defined(function, Res, !IO)
        ;
            format("unexpected proc_id_kind %d", [i(Byte)], Msg),
            Res = error(Msg)
        )
    ;
        Res0 = error(Err),
        Res = error(Err)
    ).

:- pred read_proc_id_uci_pred(maybe_error(proc_id)::out,
    io::di, io::uo) is det.

read_proc_id_uci_pred(Res, !IO) :-
    io_combinator.maybe_error_sequence_6(
        read_string,
        read_string,
        read_string,
        read_string,
        read_num,
        read_num,
        (pred(TypeName::in, TypeModule::in, DefModule::in,
                PredName::in, Arity::in, Mode::in, ProcId::out)
                is det :-
            ProcId = ok(uci_pred(TypeName, TypeModule,
                DefModule, PredName, Arity, Mode))
        ),
        Res, !IO).

:- pred read_proc_id_user_defined(pred_or_func::in, maybe_error(proc_id)::out,
    io::di, io::uo) is det.

read_proc_id_user_defined(PredOrFunc, Res, !IO) :-
    io_combinator.maybe_error_sequence_5(
        read_string,
        read_string,
        read_string,
        read_num,
        read_num,
        (pred(DeclModule::in, DefModule::in, Name::in,
                Arity::in, Mode::in, ProcId::out)
                is det :-
            ProcId = ok(user_defined(PredOrFunc, DeclModule,
                DefModule, Name, Arity, Mode))
        ),
        Res, !IO).

:- func raw_proc_id_to_string(proc_id) = string.

raw_proc_id_to_string(uci_pred(TypeName, TypeModule, _DefModule,
        PredName, Arity, Mode)) =
    string.append_list(
        [PredName, " for ", TypeModule, ".", TypeName,
        "/", string.int_to_string(Arity),
        " mode ", string.int_to_string(Mode)]).
raw_proc_id_to_string(user_defined(PredOrFunc, DeclModule, _DefModule,
        Name, Arity, Mode)) =
    string.append_list([DeclModule, ".", Name,
        "/", string.int_to_string(Arity),
        ( PredOrFunc = function -> "+1" ; "" ),
        "-", string.int_to_string(Mode)]).

:- func refined_proc_id_to_string(proc_id) = string.

refined_proc_id_to_string(uci_pred(TypeName, TypeModule, _DefModule,
        RawPredName, Arity, Mode)) = Name :-
    ( RawPredName = "__Unify__" ->
        PredName = "Unify"
    ; RawPredName = "__Compare__" ->
        PredName = "Compare"
    ; RawPredName = "__CompareRep__" ->
        PredName = "CompareRep"
    ; RawPredName = "__Index__" ->
        PredName = "Index"
    ; RawPredName = "__Initialise__" ->
        PredName = "Initialise"
    ;
        string.append("unknown special predicate name ", RawPredName, Msg),
        error(Msg)
    ),
    Name0 = string.append_list([PredName, " for ", TypeModule, ".", TypeName,
        "/", string.int_to_string(Arity)]),
    ( Mode = 0 ->
        Name = Name0
    ;
        Name = string.append_list([Name0, " mode ", int_to_string(Mode)])
    ).
refined_proc_id_to_string(user_defined(PredOrFunc, DeclModule, _DefModule,
        ProcName, Arity, Mode)) = Name :-
    (
        string.append("TypeSpecOf__", ProcName1, ProcName),
        ( string.append("pred__", ProcName2A, ProcName1) ->
            ProcName2 = ProcName2A
        ; string.append("func__", ProcName2B, ProcName1) ->
            ProcName2 = ProcName2B
        ; string.append("pred_or_func__", ProcName2C, ProcName1) ->
            ProcName2 = ProcName2C
        ;
            error("typespec: neither pred nor func")
        ),
        string.to_char_list(ProcName2, ProcName2Chars),
        fix_type_spec_suffix(ProcName2Chars, ProcNameChars, SpecInfo)
    ->
        RefinedProcName = string.from_char_list(ProcNameChars),
        Name = string.append_list([DeclModule, ".", RefinedProcName,
            "/", string.int_to_string(Arity),
            ( PredOrFunc = function -> "+1" ; "" ),
            "-", string.int_to_string(Mode),
            " [", SpecInfo, "]"])
    ;
        string.append("IntroducedFrom__", ProcName1, ProcName),
        ( string.append("pred__", ProcName2A, ProcName1) ->
            ProcName2 = ProcName2A
        ; string.append("func__", ProcName2B, ProcName1) ->
            ProcName2 = ProcName2B
        ;
            error("lambda: neither pred nor func")
        ),
        string.to_char_list(ProcName2, ProcName2Chars),
        split_lambda_name(ProcName2Chars, Segments),
        glue_lambda_name(Segments, ContainingNameChars,
            LineNumberChars)
    ->
        string.from_char_list(ContainingNameChars, ContainingName),
        string.from_char_list(LineNumberChars, LineNumber),
        Name = string.append_list([DeclModule, ".", ContainingName,
            " lambda line ", LineNumber,
            "/", string.int_to_string(Arity),
            ( PredOrFunc = function -> "+1" ; "" )])
    ;
        Name = string.append_list([DeclModule, ".", ProcName,
            "/", string.int_to_string(Arity),
            ( PredOrFunc = function -> "+1" ; "" ),
            "-", string.int_to_string(Mode)])
    ).

:- pred fix_type_spec_suffix(list(char)::in, list(char)::out, string::out)
    is semidet.

fix_type_spec_suffix(Chars0, Chars, SpecInfoStr) :-
    ( Chars0 = ['_', '_', '[' | SpecInfo0 ] ->
        Chars = [],
        list.takewhile(non_right_bracket, SpecInfo0, SpecInfo, _),
        string.from_char_list(SpecInfo, SpecInfoStr)
    ; Chars0 = [Char | TailChars0] ->
        fix_type_spec_suffix(TailChars0, TailChars, SpecInfoStr),
        Chars = [Char | TailChars]
    ;
        fail
    ).

:- pred non_right_bracket(char::in) is semidet.

non_right_bracket(C) :-
    C \= ']'.

:- pred split_lambda_name(list(char)::in, list(list(char))::out) is det.

split_lambda_name([], []).
split_lambda_name([Char0 | Chars0], StringList) :-
    ( Chars0 = ['_', '_' | Chars1 ] ->
        split_lambda_name(Chars1, StringList0),
        StringList = [[Char0] | StringList0]
    ;
        split_lambda_name(Chars0, StringList0),
        (
            StringList0 = [],
            StringList = [[Char0]]
        ;
            StringList0 = [String0 | StringList1],
            StringList = [[Char0 | String0] | StringList1]
        )
    ).

:- pred glue_lambda_name(list(list(char))::in, list(char)::out,
    list(char)::out) is semidet.

glue_lambda_name(Segments, PredName, LineNumber) :-
    ( Segments = [LineNumberPrime, _] ->
        PredName = [],
        LineNumber = LineNumberPrime
    ; Segments = [Segment | TailSegments] ->
        glue_lambda_name(TailSegments, PredName1, LineNumber),
        ( PredName1 = [] ->
            PredName = Segment
        ;
            list.append(Segment, ['_', '_' | PredName1], PredName)
        )
    ;
        fail
    ).

:- pred read_proc_dynamic(maybe_error2(proc_dynamic, int)::out,
    io::di, io::uo) is det.

read_proc_dynamic(Res, !IO) :-
    % DEBUGSITE
    % io.write_string("reading proc_dynamic.\n", !IO),
    io_combinator.maybe_error_sequence_3(
        read_ptr(pd),
        read_ptr(ps),
        read_num,
        (pred(PDI0::in, PSI0::in, N0::in, Stuff0::out) is det :-
            Stuff0 = ok({PDI0, PSI0, N0})
        ),
        Res1, !IO),
    (
        Res1 = ok({PDI, PSI, N}),
        read_n_things(N, read_call_site_slot, Res2, !IO),
        (
            Res2 = ok(Refs),
            PSPtr = make_psptr(PSI),
            ProcDynamic = proc_dynamic(PSPtr, array(Refs)),
            Res = ok2(ProcDynamic, PDI)
            % DEBUGSITE
            % io.write_string("read proc_dynamic ", !IO),
            % io.write_int(PDI, !IO),
            % io.write_string(": ", !IO),
            % io.write(ProcDynamic, !IO),
            % io.write_string("\n", !IO)
        ;
            Res2 = error(Err),
            Res = error2(Err)
        )
    ;
        Res1 = error(Err),
        Res = error2(Err)
    ).

:- pred read_call_site_dynamic(maybe_error2(call_site_dynamic, int)::out,
    io::di, io::uo) is det.

read_call_site_dynamic(Res, !IO) :-
    % DEBUGSITE
    % io.write_string("reading call_site_dynamic.\n", !IO),
    read_ptr(csd, Res1, !IO),
    (
        Res1 = ok(CSDI),
        read_ptr(pd, Res2, !IO),
        (
            Res2 = ok(PDI),
            read_profile(Res3, !IO),
            (
                Res3 = ok(Profile),
                PDPtr = make_pdptr(PDI),
                CallerPDPtr = make_dummy_pdptr,
                CallSiteDynamic = call_site_dynamic(
                    CallerPDPtr, PDPtr, Profile),
                Res = ok2(CallSiteDynamic, CSDI)
                % DEBUGSITE
                % io.write_string("read call_site_dynamic ",
                %   !IO),
                % io.write_int(CSDI, !IO),
                % io.write_string(": ", !IO),
                % io.write(CallSiteDynamic, !IO),
                % io.write_string("\n", !IO)
            ;
                Res3 = error(Err),
                Res = error2(Err)
            )
        ;
            Res2 = error(Err),
            Res = error2(Err)
        )
    ;
        Res1 = error(Err),
        Res = error2(Err)
    ).

:- pred read_profile(maybe_error(own_prof_info)::out,
    io::di, io::uo) is det.

read_profile(Res, !IO) :-
    read_num(Res0, !IO),
    (
        Res0 = ok(Mask),

        % The masks here must correspond exactly with the masks in
        % MR_write_out_call_site_dynamic in mercury_deep_profiling.c
        % in the runtime.
        some [!MaybeError] (
            !:MaybeError = no,
            % We normally assume that the configuration macro
            % MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS is not
            % defined, and thus mercury_deep_profiling.m never
            % writes out call counts (instead, call counts are
            % computed from other port counts in measurements.m).
            % maybe_read_num_handle_error(Mask, 0x0001, Calls,
            %   !MaybeError, !IO),
            maybe_read_num_handle_error(Mask, 0x0002, Exits,
                !MaybeError, !IO),
            maybe_read_num_handle_error(Mask, 0x0004, Fails,
                !MaybeError, !IO),
            maybe_read_num_handle_error(Mask, 0x0040, Redos,
                !MaybeError, !IO),
            maybe_read_num_handle_error(Mask, 0x0080, Excps,
                !MaybeError, !IO),
            maybe_read_num_handle_error(Mask, 0x0100, Quanta,
                !MaybeError, !IO),
            maybe_read_num_handle_error(Mask, 0x0008, CallSeqs,
                !MaybeError, !IO),
            maybe_read_num_handle_error(Mask, 0x0010, Allocs,
                !MaybeError, !IO),
            maybe_read_num_handle_error(Mask, 0x0020, Words,
                !MaybeError, !IO),
            LastMaybeError = !.MaybeError
        ),
        (
            LastMaybeError = yes(Error),
            Res = error(Error)
        ;
            LastMaybeError = no,
            Res = ok(compress_profile(Exits, Fails, Redos, Excps,
                Quanta, CallSeqs, Allocs, Words))
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred maybe_read_num_handle_error(int::in, int::in, int::out,
    maybe(string)::in, maybe(string)::out,
    io::di, io::uo) is det.

maybe_read_num_handle_error(MaskWord, MaskValue, Num, !MaybeError, !IO) :-
    ( MaskWord /\ MaskValue \= 0 ->
        read_num(Res, !IO),
        (
            Res = ok(Num)
        ;
            Res = error(Error),
            Num = 0,
            !:MaybeError = yes(Error)
        )
    ;
        Num = 0
    ).

:- pred read_call_site_slot(maybe_error(call_site_array_slot)::out,
    io::di, io::uo) is det.

read_call_site_slot(Res, !IO) :-
    % DEBUGSITE
    % io.write_string("reading call_site_slot.\n", !IO),
    read_call_site_kind(Res1, !IO),
    (
        Res1 = ok(Kind),
        ( Kind = normal_call ->
            read_ptr(csd, Res2, !IO),
            (
                Res2 = ok(CSDI),
                CSDPtr = make_csdptr(CSDI),
                Res = ok(slot_normal(CSDPtr))
                % DEBUGSITE
                % io.write_string("normal call_site slot ",
                %   !IO),
                % io.write_int(CSDI, !IO),
                % io.write_string("\n", !IO)
            ;
                Res2 = error(Err),
                Res = error(Err)
            )
        ;
            (
                ( Kind = higher_order_call
                ; Kind = method_call
                )
            ->
                Zeroed = zeroed
            ;
                Zeroed = not_zeroed
            ),
            read_multi_call_site_csdis(Res2, !IO),
            (
                Res2 = ok(CSDIs),
                CSDPtrs = list.map(make_csdptr, CSDIs),
                Res = ok(slot_multi(Zeroed, array(CSDPtrs)))
                % DEBUGSITE
                % io.write_string("multi call_site slots ",
                %   !IO),
                % io.write(CSDIs, !IO),
                % io.write_string("\n", !IO)
            ;
                Res2 = error(Err),
                Res = error(Err)
            )
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).

:- pred read_multi_call_site_csdis(maybe_error(list(int))::out,
    io::di, io::uo) is det.

read_multi_call_site_csdis(Res, !IO) :-
    read_multi_call_site_csdis_2([], Res, !IO).

    % We keep reading CSD node numbers until we find a zero byte.
    % The reason why a zero byte works as a sentinel is that a CSD node
    % number in the list of CSD node numbers at a multi call site cannot be
    % zero, and our encoding scheme ensures that non-zero node numbers
    % cannot start with a zero byte.
    %
    % We return the list of CSD node numbers in the reversed order, but
    % this is OK because our caller does not pay attention to the order
    % anyway.

:- pred read_multi_call_site_csdis_2(list(int)::in,
    maybe_error(list(int))::out, io::di, io::uo) is det.

read_multi_call_site_csdis_2(CSDIs0, Res, !IO) :-
    % DEBUGSITE
    % io.format("reading multi_call_site_csdi.\n", [], !IO),
    read_deep_byte(Res0, !IO),
    (
        Res0 = ok(Byte),
        ( Byte = 0 ->
            Res = ok(CSDIs0)
        ;
            putback_byte(Byte, !IO),
            read_ptr(csd, Res1, !IO),
            (
                Res1 = ok(CSDI),
                read_multi_call_site_csdis_2([CSDI | CSDIs0], Res, !IO)
            ;
                Res1 = error(Err),
                Res = error(Err)
            )
        )
    ;
        Res0 = error(Err),
        Res = error(Err)
    ).

:- pred read_call_site_kind(maybe_error(call_site_kind)::out,
    io::di, io::uo) is det.

read_call_site_kind(Res, !IO) :-
    read_deep_byte(Res0, !IO),
    (
        Res0 = ok(Byte),
        ( Byte = token_normal_call ->
            Res = ok(normal_call)
        ; Byte = token_special_call ->
            Res = ok(special_call)
        ; Byte = token_higher_order_call ->
            Res = ok(higher_order_call)
        ; Byte = token_method_call ->
            Res = ok(method_call)
        ; Byte = token_callback ->
            Res = ok(callback)
        ;
            format("unexpected call_site_kind %d",
                [i(Byte)], Msg),
            Res = error(Msg)
        )
        % DEBUGSITE
        % io.write_string("call_site_kind ", !IO),
        % io.write(Res, !IO),
        % io.write_string("\n", !IO)
    ;
        Res0 = error(Err),
        Res = error(Err)
    ).

:- pred read_call_site_kind_and_callee(
    maybe_error(call_site_kind_and_callee)::out,
    io::di, io::uo) is det.

read_call_site_kind_and_callee(Res, !IO) :-
    read_deep_byte(Res0, !IO),
    (
        Res0 = ok(Byte),
        ( Byte = token_normal_call ->
            read_num(Res1, !IO),
            (
                Res1 = ok(CalleeProcStatic),
                read_string(Res2, !IO),
                (
                    Res2 = ok(TypeSubst),
                    Res = ok(normal_call_and_callee(
                        proc_static_ptr(CalleeProcStatic), TypeSubst))
                ;
                    Res2 = error(Err),
                    Res = error(Err)
                )
            ;
                Res1 = error(Err),
                Res = error(Err)
            )
        ; Byte = token_special_call ->
            Res = ok(special_call_and_no_callee)
        ; Byte = token_higher_order_call ->
            Res = ok(higher_order_call_and_no_callee)
        ; Byte = token_method_call ->
            Res = ok(method_call_and_no_callee)
        ; Byte = token_callback ->
            Res = ok(callback_and_no_callee)
        ;
            format("unexpected call_site_kind %d", [i(Byte)], Msg),
            Res = error(Msg)
        )
        % DEBUGSITE
        % io.write_string("call_site_kind_and_callee ", !IO),
        % io.write(Res, !IO),
        % io.write_string("\n", !IO)
    ;
        Res0 = error(Err),
        Res = error(Err)
    ).

%-----------------------------------------------------------------------------%

:- pred read_n_things(int::in, pred(maybe_error(T), io, io)::
    in(pred(out, di, uo) is det), maybe_error(list(T))::out,
    io::di, io::uo) is det.

read_n_things(N, ThingReader, Res, !IO) :-
    read_n_things(N, ThingReader, [], Res0, !IO),
    (
        Res0 = ok(Things0),
        reverse(Things0, Things),
        Res = ok(Things)
    ;
        Res0 = error(Err),
        Res = error(Err)
    ).

:- pred read_n_things(int::in, pred(maybe_error(T), io, io)::
    in(pred(out, di, uo) is det), list(T)::in, maybe_error(list(T))::out,
    io::di, io::uo) is det.

read_n_things(N, ThingReader, Things0, Res, !IO) :-
    ( N =< 0 ->
        Res = ok(Things0)
    ;
        call(ThingReader, Res1, !IO),
        (
            Res1 = ok(Thing),
            read_n_things(N - 1, ThingReader, [Thing | Things0], Res, !IO)
        ;
            Res1 = error(Err),
            Res = error(Err)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred read_string(maybe_error(string)::out,
    io::di, io::uo) is det.

read_string(Res, !IO) :-
    read_num(Res0, !IO),
    (
        Res0 = ok(Length),
        ( Length = 0 ->
            Res = ok("")
        ;
            read_n_byte_string(Length, Res, !IO)
        )
    ;
        Res0 = error(Err),
        Res = error(Err)
    ).

:- pred read_n_byte_string(int::in, maybe_error(string)::out,
    io::di, io::uo) is det.

read_n_byte_string(Length, Res, !IO) :-
    read_n_bytes(Length, Res1, !IO),
    (
        Res1 = ok(Bytes),
        (
            map((pred(I::in, C::out) is semidet :- char.to_int(C, I)),
                Bytes, Chars)
        ->
            string.from_char_list(Chars, Str),
            Res = ok(Str)
        ;
            Res = error("string contained bad char")
        )
    ;
        Res1 = error(Err),
        Res = error(Err)
    ).
    % DEBUGSITE
    % io.write_string("string ", !IO),
    % io.write(Res, !IO),
    % io.write_string("\n", !IO).

:- pred read_ptr(ptr_kind::in, maybe_error(int)::out,
    io::di, io::uo) is det.

read_ptr(_Kind, Res, !IO) :-
    read_num1(0, Res, !IO).
    % DEBUGSITE
    % io.write_string("ptr ", !IO),
    % io.write(Res, !IO),
    % io.write_string("\n", !IO).

:- pred read_num(maybe_error(int)::out, io::di, io::uo) is det.

read_num(Res, !IO) :-
    read_num1(0, Res, !IO).
    % DEBUGSITE
    % io.write_string("num ", !IO),
    % io.write(Res, !IO),
    % io.write_string("\n", !IO).

:- pred read_num1(int::in, maybe_error(int)::out,
    io::di, io::uo) is det.

read_num1(Num0, Res, !IO) :-
    read_byte(Res0, !IO),
    (
        Res0 = ok(Byte),
        Num1 = (Num0 << 7) \/ (Byte /\ 0x7F),
        ( Byte /\ 0x80 \= 0 ->
            read_num1(Num1, Res, !IO)
        ;
            Res = ok(Num1)
        )
    ;
        Res0 = eof,
        Res = error("unexpected end of file")
    ;
        Res0 = error(Err),
        io.error_message(Err, Msg),
        Res = error(Msg)
    ).

:- func fixed_size_int_bytes = int.

% Must correspond to MR_FIXED_SIZE_INT_BYTES
% in runtime/mercury_deep_profiling.c.

fixed_size_int_bytes = 4.

:- pred read_fixed_size_int(maybe_error(int)::out,
    io::di, io::uo) is det.

read_fixed_size_int(Res, !IO) :-
    read_fixed_size_int1(fixed_size_int_bytes, 0, 0, Res, !IO).

:- pred read_fixed_size_int1(int::in, int::in, int::in, maybe_error(int)::out,
    io::di, io::uo) is det.

read_fixed_size_int1(BytesLeft, Num0, ShiftBy, Res, !IO) :-
    ( BytesLeft =< 0 ->
        Res = ok(Num0)
    ;
        read_deep_byte(Res0, !IO),
        (
            Res0 = ok(Byte),
            Num1 = Num0 \/ ( Byte << ShiftBy),
            read_fixed_size_int1(BytesLeft - 1, Num1, ShiftBy + 8, Res, !IO)
        ;
            Res0 = error(Err),
            Res = error(Err)
        )
    ).

:- pred read_n_bytes(int::in, maybe_error(list(int))::out,
    io::di, io::uo) is det.

read_n_bytes(N, Res, !IO) :-
    read_n_bytes(N, [], Res0, !IO),
    (
        Res0 = ok(Bytes0),
        reverse(Bytes0, Bytes),
        Res = ok(Bytes)
    ;
        Res0 = error(Err),
        Res = error(Err)
    ).

:- pred read_n_bytes(int::in, list(int)::in, maybe_error(list(int))::out,
    io::di, io::uo) is det.

read_n_bytes(N, Bytes0, Res, !IO) :-
    ( N =< 0 ->
        Res = ok(Bytes0)
    ;
        read_deep_byte(Res0, !IO),
        (
            Res0 = ok(Byte),
            read_n_bytes(N - 1, [Byte | Bytes0], Res, !IO)
        ;
            Res0 = error(Err),
            Res = error(Err)
        )
    ).

:- pred read_deep_byte(maybe_error(int)::out,
    io::di, io::uo) is det.

read_deep_byte(Res, !IO) :-
    read_byte(Res0, !IO),
    % DEBUGSITE
    % io.write_string("byte ", !IO),
    % io.write(Res, !IO),
    % io.write_string("\n", !IO),
    (
        Res0 = ok(Byte),
        Res = ok(Byte)
    ;
        Res0 = eof,
        Res = error("unexpected end of file")
    ;
        Res0 = error(Err),
        io.error_message(Err, Msg),
        Res = error(Msg)
    ).

%------------------------------------------------------------------------------%

:- pred deep_insert(array(T)::in, int::in, T::in, array(T)::out) is det.

deep_insert(A0, Ind, Thing, A) :-
    array.max(A0, Max),
    ( Ind > Max ->
        error("deep_insert: array bounds violation")
        % array.lookup(A0, 0, X),
        % array.resize(u(A0), 2 * (Max + 1), X, A1),
        % deep_insert(A1, Ind, Thing, A)
    ;
        set(u(A0), Ind, Thing, A)
    ).

%------------------------------------------------------------------------------%

:- func make_csdptr(int) = call_site_dynamic_ptr.
:- func make_cssptr(int) = call_site_static_ptr.
:- func make_pdptr(int) = proc_dynamic_ptr.
:- func make_psptr(int) = proc_static_ptr.

make_csdptr(CSDI) = call_site_dynamic_ptr(CSDI).
make_cssptr(CSSI) = call_site_static_ptr(CSSI).
make_pdptr(PDI) = proc_dynamic_ptr(PDI).
make_psptr(PSI) = proc_static_ptr(PSI).

:- func make_dummy_csdptr = call_site_dynamic_ptr.
:- func make_dummy_cssptr = call_site_static_ptr.
:- func make_dummy_pdptr = proc_dynamic_ptr.
:- func make_dummy_psptr = proc_static_ptr.

make_dummy_csdptr = call_site_dynamic_ptr(-1).
make_dummy_cssptr = call_site_static_ptr(-1).
make_dummy_pdptr = proc_dynamic_ptr(-1).
make_dummy_psptr = proc_static_ptr(-1).

%------------------------------------------------------------------------------%

:- pragma foreign_decl("C", "#include ""mercury_deep_profiling.h""").

:- func token_call_site_static = int.
:- pragma foreign_proc("C",
    token_call_site_static = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_call_site_static;
").

:- func token_call_site_dynamic = int.
:- pragma foreign_proc("C",
    token_call_site_dynamic = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_call_site_dynamic;
").

:- func token_proc_static = int.
:- pragma foreign_proc("C",
    token_proc_static = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_proc_static;
").

:- func token_proc_dynamic = int.
:- pragma foreign_proc("C",
    token_proc_dynamic = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_proc_dynamic;
").

:- func token_normal_call = int.
:- pragma foreign_proc("C",
    token_normal_call = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_normal_call;
").

:- func token_special_call = int.
:- pragma foreign_proc("C",
    token_special_call = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_special_call;
").

:- func token_higher_order_call = int.
:- pragma foreign_proc("C",
    token_higher_order_call = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_higher_order_call;
").

:- func token_method_call = int.
:- pragma foreign_proc("C",
    token_method_call = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_method_call;
").

:- func token_callback = int.
:- pragma foreign_proc("C",
    token_callback = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_callback;
").

:- func token_isa_predicate = int.
:- pragma foreign_proc("C",
    token_isa_predicate = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_isa_predicate;
").

:- func token_isa_function = int.
:- pragma foreign_proc("C",
    token_isa_function = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_isa_function;
").

:- func token_isa_uci_pred = int.
:- pragma foreign_proc("C",
    token_isa_uci_pred = (X::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    X = MR_deep_token_isa_uci_pred;
").

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
