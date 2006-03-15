%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mlds_to_ilasm.m.
% Main author: trd.

% This code converts the MLDS representation into IL assembler.  This module
% takes care of creating the appropriate files and generating output, while
% mlds_to_il takes care of generated IL from MLDS.

%-----------------------------------------------------------------------------%

:- module ml_backend.mlds_to_ilasm.
:- interface.

:- import_module ml_backend.mlds.

:- import_module io.

    % Convert the MLDS to IL and write it to a file.
    %
:- pred mlds_to_ilasm.output_mlds(mlds::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_pred. % for `pred_proc_id'.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ml_backend.ilasm.
:- import_module ml_backend.ilds.
:- import_module ml_backend.il_peephole.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.mlds_to_il.
:- import_module ml_backend.mlds_to_managed.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

output_mlds(MLDS, !IO) :-
    ModuleName = mlds.get_module_name(MLDS),
    module_name_to_file_name(ModuleName, ".il", yes, ILAsmFile, !IO),
    output_to_file(ILAsmFile, output_assembler(MLDS), Result, !IO),

    (
        Result = yes(ForeignLangs),
        % Output any outline foreign_code to the appropriate foreign
        % language file.
        list.foldl(output_foreign_file(MLDS),
            set.to_sorted_list(ForeignLangs), !IO)
    ;
        % An I/O error occurred; output_to_file has already reported
        % an error message, so we don't need to do anything here.
        Result = no
    ).

:- pred output_foreign_file(mlds::in, foreign_language::in,
    io::di, io::uo) is det.

output_foreign_file(MLDS, ForeignLang, !IO) :-
    ModuleName = mlds.get_module_name(MLDS),
    (
        ForeignModuleName = foreign_language_module_name(ModuleName,
            ForeignLang),
        Extension = foreign_language_file_extension(ForeignLang)
    ->
        handle_foreign_lang(ForeignLang, CodeGenerator),
        module_name_to_file_name(ForeignModuleName, Extension,
            yes, File, !IO),
        output_to_file(File,
            (pred(di, uo) is det --> CodeGenerator(MLDS)), !IO)
    ;
        unexpected(this_file, "output_foreign_file: " ++
            "unexpected language")
    ).

:- pred handle_foreign_lang(foreign_language::in,
    pred(mlds, io, io)::out(pred(in, di, uo) is det)) is det.

handle_foreign_lang(managed_cplusplus, output_managed_code(managed_cplusplus)).
handle_foreign_lang(csharp, output_managed_code(csharp)).
handle_foreign_lang(c, _) :-
    sorry(this_file, "language C foreign code not supported").
handle_foreign_lang(il, _) :-
    sorry(this_file, "language IL foreign code not supported").
handle_foreign_lang(java, _) :-
    sorry(this_file, "language Java foreign code not supported").

    % Generate the `.il' file.
    % Returns the set of foreign language
    %
:- pred output_assembler(mlds::in, set(foreign_language)::out,
    io::di, io::uo) is det.

output_assembler(MLDS, ForeignLangs, !IO) :-
    MLDS = mlds(ModuleName, _ForeignCode, _Imports, _Defns,
        _InitPreds, _FinalPreds),
    output_src_start(ModuleName, !IO),
    io.nl(!IO),

    generate_il(MLDS, ILAsm0, ForeignLangs, !IO),

        % Perform peephole optimization if requested.  If peephole
        % optimization was not requested, we may still need to invoke
        % the peephole optimization pass, because some of the peephole
        % optimizations are actually needed for verifiability of the
        % generated IL.
    globals.io_lookup_bool_option(optimize_peep, Peephole, !IO),
    globals.io_lookup_bool_option(verifiable_code, Verifiable, !IO),
    ( Peephole = yes ->
        VerifyOnly = no,
        il_peephole.optimize(VerifyOnly, ILAsm0, ILAsm)
    ; Verifiable = yes ->
        VerifyOnly = yes,
        il_peephole.optimize(VerifyOnly, ILAsm0, ILAsm)
    ;
        ILAsm0 = ILAsm
    ),

        % Output the assembly.
    ilasm.output(ILAsm, !IO),
    output_src_end(ModuleName, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "mlds_to_ilasm.m".

%-----------------------------------------------------------------------------%
:- end_module mlds_to_ilasm.
%-----------------------------------------------------------------------------%
