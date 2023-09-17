%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.util.m.
% Authors: stayl, wangp.
%
% Hashing predicates used to implement `mmc --make'.
%
%---------------------------------------------------------------------------%

:- module make.hash.
:- interface.

:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

%---------------------------------------------------------------------------%
%
% Hash functions.
%

:- pred module_name_hash(module_name::in, int::out) is det.

:- pred dependency_file_hash(dependency_file::in, int::out) is det.

:- pred dependency_file_with_module_index_hash(
    dependency_file_with_module_index::in, int::out) is det.

:- pred target_file_hash(target_file::in, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.
:- import_module libs.globals.

:- import_module enum.
:- import_module int.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

module_name_hash(SymName, Hash) :-
    (
        SymName = unqualified(Name),
        Hash = string.hash(Name)
    ;
        SymName = qualified(_ModuleName, Name),
        % Hashing the module name seems to be not worthwhile.
        Hash = string.hash(Name)
    ).

dependency_file_hash(DepFile, Hash) :-
    (
        DepFile = dep_target(TargetFile),
        target_file_hash(TargetFile, Hash)
    ;
        DepFile = dep_file(FileName),
        Hash = string.hash(FileName)
    ).

dependency_file_with_module_index_hash(DepFile, Hash) :-
    (
        DepFile = dfmi_target(ModuleIndex, Type),
        Hash0 = cast_to_int(to_uint(ModuleIndex)),
        Hash1 = module_target_type_to_nonce(Type),
        Hash = mix(Hash0, Hash1)
    ;
        DepFile = dfmi_file(FileName),
        Hash = string.hash(FileName)
    ).

target_file_hash(TargetFile, Hash) :-
    TargetFile = target_file(ModuleName, Type),
    module_name_hash(ModuleName, Hash0),
    Hash1 = module_target_type_to_nonce(Type),
    Hash = mix(Hash0, Hash1).

:- func module_target_type_to_nonce(module_target_type) = int.

module_target_type_to_nonce(Type) = X :-
    (
        Type = module_target_source,
        X = 1
    ;
        Type = module_target_errors,
        X = 2
    ;
        Type = module_target_int0,
        X = 3
    ;
        Type = module_target_int1,
        X = 4
    ;
        Type = module_target_int2,
        X = 5
    ;
        Type = module_target_int3,
        X = 6
    ;
        Type = module_target_opt,
        X = 7
    ;
        Type = module_target_analysis_registry,
        X = 8
    ;
        Type = module_target_c_header(header_mh),
        X = 9
    ;
        Type = module_target_c_header(header_mih),
        X = 10
    ;
        Type = module_target_c_code,
        X = 11
    ;
        Type = module_target_java_code,
        X = 12
%   ;
%       Type = module_target_erlang_header,
%       X = 13
%   ;
%       Type = module_target_erlang_code,
%       X = 14
%   ;
%       Type = module_target_erlang_beam_code,
%       X = 15
    ;
        Type = module_target_object_code(PIC),
        X = 16 `mix` pic_to_nonce(PIC)
    ;
        Type = module_target_foreign_object(_PIC, _ForeignLang),
        X = 17
    ;
        Type = module_target_fact_table_object(_PIC, _FileName),
        X = 18
    ;
        Type = module_target_xml_doc,
        X = 19
    ;
        Type = module_target_track_flags,
        X = 20
    ;
        Type = module_target_java_class_code,
        X = 21
    ;
        Type = module_target_csharp_code,
        X = 22
    ).

:- func pic_to_nonce(pic) = int.

pic_to_nonce(pic) = 1.
pic_to_nonce(non_pic) = 3.
% For compatibility; we used to have pic_to_nonce(link_with_pic) = 2.

:- func mix(int, int) = int.

mix(H0, X) = H :-
    H1 = H0 `xor` (H0 `unchecked_left_shift` 5),
    H = H1 `xor` X.

%---------------------------------------------------------------------------%
:- end_module make.hash.
%---------------------------------------------------------------------------%
