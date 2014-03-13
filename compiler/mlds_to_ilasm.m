%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mlds_to_ilasm.m.
% Main author: trd.
% 
% This code converts the MLDS representation into IL assembler.  This module
% takes care of creating the appropriate files and generating output, while
% mlds_to_il takes care of generated IL from MLDS.
% 
%-----------------------------------------------------------------------------%

:- module ml_backend.mlds_to_ilasm.
:- interface.

:- import_module libs.globals.
:- import_module ml_backend.mlds.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

    % Convert the MLDS to IL and write it to a file.
    %
:- pred output_mlds_via_ilasm(globals::in, mlds::in, bool::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.file_util.
:- import_module libs.options.
:- import_module ml_backend.ilasm.
:- import_module ml_backend.il_peephole.
:- import_module ml_backend.mlds_to_il.
:- import_module ml_backend.mlds_to_managed.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_foreign.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------------%

output_mlds_via_ilasm(Globals, MLDS, Succeeded, !IO) :-
    ModuleName = mlds_get_module_name(MLDS),
    module_name_to_file_name(Globals, ModuleName, ".il",
        do_create_dirs, ILAsmFile, !IO),
    output_to_file_return_result(Globals, ILAsmFile,
        output_assembler(Globals, MLDS), Result, !IO),

    (
        Result = yes(ForeignLangs),
        % Output any outline foreign_code to the appropriate foreign
        % language file.
        list.foldl2(output_foreign_file(Globals, MLDS),
            set.to_sorted_list(ForeignLangs), yes, Succeeded, !IO)
    ;
        % An I/O error occurred; output_to_file has already reported
        % an error message, so we don't need to do anything here.
        Result = no,
        Succeeded = no
    ).

:- pred output_foreign_file(globals::in, mlds::in, foreign_language::in,
    bool::in, bool::out, io::di, io::uo) is det.

output_foreign_file(Globals, MLDS, ForeignLang, !Succeeded, !IO) :-
    ModuleName = mlds_get_module_name(MLDS),
    (
        ForeignModuleName = foreign_language_module_name(ModuleName,
            ForeignLang),
        Extension = foreign_language_file_extension(ForeignLang)
    ->
        (
            ForeignLang = lang_csharp,
            module_name_to_file_name(Globals, ForeignModuleName, Extension,
                do_create_dirs, File, !IO),
            output_to_file(Globals, File, output_csharp_code(Globals, MLDS),
                TargetCodeSucceeded, !IO),
            bool.and(TargetCodeSucceeded, !Succeeded)
        ;
            ForeignLang = lang_c,
            sorry($module, $pred, "language C foreign code not supported")
        ;
            ForeignLang = lang_il,
            sorry($module, $pred, "language IL foreign code not supported")
        ;
            ForeignLang = lang_java,
            sorry($module, $pred, "language Java foreign code not supported")
        ;
            ForeignLang = lang_erlang,
            sorry($module, $pred, "language Erlang foreign code not supported")
        )
    ;
        unexpected($module, $pred, "output_foreign_file: unexpected language")
    ).

    % Generate the `.il' file.
    % Returns the set of foreign language
    %
:- pred output_assembler(globals::in, mlds::in, set(foreign_language)::out,
    io::di, io::uo) is det.

output_assembler(Globals, MLDS, ForeignLangs, !IO) :-
    MLDS = mlds(ModuleName, _ForeignCode, _Imports, _GlobalData, _Defns,
        _InitPreds, _FinalPreds, _ExportedEnums),
    output_src_start(ModuleName, !IO),
    io.nl(!IO),

    generate_il(Globals, MLDS, ILAsm0, ForeignLangs),

    % Perform peephole optimization if requested. If peephole optimization
    % was not requested, we may still need to invoke the peephole optimization
    % pass, because some of the peephole optimizations are actually needed
    % for verifiability of the generated IL.
    globals.lookup_bool_option(Globals, optimize_peep, Peephole),
    globals.lookup_bool_option(Globals, verifiable_code, Verifiable),
    ( Peephole = yes ->
        VerifyOnly = no,
        il_peephole_optimize(VerifyOnly, ILAsm0, ILAsm)
    ; Verifiable = yes ->
        VerifyOnly = yes,
        il_peephole_optimize(VerifyOnly, ILAsm0, ILAsm)
    ;
        ILAsm0 = ILAsm
    ),

    % Output the assembly.
    ilasm_output(Globals, ILAsm, !IO),
    output_src_end(ModuleName, !IO).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_ilasm.
%-----------------------------------------------------------------------------%
