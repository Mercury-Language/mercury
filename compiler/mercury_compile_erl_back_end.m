%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mercury_compile_erl_back_end.m.
%
% This module implements the Erlang backend for the top level of the Mercury
% compiler. It invokes the different passes of this backend as appropriate.
%
%-----------------------------------------------------------------------------%

:- module top_level.mercury_compile_erl_back_end.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.passes_aux.
:- import_module erl_backend.elds.

:- import_module bool.
:- import_module io.

:- pred erlang_backend(module_info::in, elds::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

:- pred elds_to_erlang(module_info::in, elds::in, bool::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.base_typeclass_info.
:- import_module backend_libs.type_class_info.
:- import_module backend_libs.type_ctor_info.
:- import_module erl_backend.elds.
:- import_module erl_backend.elds_to_erlang.
:- import_module erl_backend.erl_code_gen.
:- import_module erl_backend.erl_rtti.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.

:- import_module list.

%-----------------------------------------------------------------------------%

erlang_backend(HLDS, ELDS, !DumpInfo, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_write_string(Verbose, "% Converting HLDS to ELDS...\n", !IO),
    erl_code_gen(HLDS, ELDS0, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO),

    maybe_write_string(Verbose, "% Generating RTTI data...\n", !IO),
    elds_gen_rtti_data(HLDS, ELDS0, ELDS, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred elds_gen_rtti_data(module_info::in, elds::in, elds::out,
    io::di, io::uo) is det.

elds_gen_rtti_data(HLDS, !ELDS, !IO) :-
    % Generate the representations for various data structures
    % used for type classes.
    module_info_get_globals(HLDS, Globals),
    type_ctor_info.generate_rtti(HLDS, TypeCtorRttiData),
    generate_base_typeclass_info_rtti(HLDS, OldTypeClassInfoRttiData),
    globals.lookup_bool_option(Globals, new_type_class_rtti, NewTypeClassRtti),
    generate_type_class_info_rtti(HLDS, NewTypeClassRtti,
        NewTypeClassInfoRttiData),
    list.append(OldTypeClassInfoRttiData, NewTypeClassInfoRttiData,
        TypeClassInfoRttiData),
    RttiDatas = TypeCtorRttiData ++ TypeClassInfoRttiData,
    ErlangRttiDatas = list.map(erlang_rtti_data(HLDS), RttiDatas),

    RttiDefns0 = !.ELDS ^ elds_rtti_funcs,
    rtti_data_list_to_elds(HLDS, ErlangRttiDatas, RttiDefns),
    !ELDS ^ elds_rtti_funcs := RttiDefns0 ++ RttiDefns.

elds_to_erlang(HLDS, ELDS, Succeeded, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_write_string(Verbose, "% Converting ELDS to Erlang...\n", !IO),
    elds_to_erlang.output_elds(HLDS, ELDS, Succeeded, !IO),
    maybe_write_string(Verbose, "% Finished converting ELDS to Erlang.\n",
        !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_erl_back_end.
%-----------------------------------------------------------------------------%
