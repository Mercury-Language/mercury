%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% mlds_to_ilasm - Convert MLDS to IL assembler code.
% Main author: trd.
%
% This code converts the MLDS representation into IL assembler.
% This module takes care of creating the appropriate files and
% generating output, while mlds_to_il takes care of generated IL from
% MLDS.

:- module ml_backend__mlds_to_ilasm.
:- interface.

:- import_module ml_backend__mlds.

:- import_module io.

	% Convert the MLDS to IL and write it to a file.

:- pred mlds_to_ilasm__output_mlds(mlds::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops.
:- import_module backend_libs__c_util.
:- import_module backend_libs__foreign.
:- import_module backend_libs__rtti.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_pred. % for `pred_proc_id'.
:- import_module hlds__passes_aux.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module libs__tree.
:- import_module ml_backend__ilasm.
:- import_module ml_backend__ilds.
:- import_module ml_backend__il_peephole.
:- import_module ml_backend__ml_code_util.
:- import_module ml_backend__mlds_to_il.
:- import_module ml_backend__mlds_to_managed.
:- import_module ml_backend__ml_util.
:- import_module parse_tree__error_util.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_foreign.
:- import_module parse_tree__prog_out.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

output_mlds(MLDS, !IO) :-
	ModuleName = mlds__get_module_name(MLDS),
	module_name_to_file_name(ModuleName, ".il", yes, ILAsmFile, !IO),
	output_to_file(ILAsmFile, output_assembler(MLDS), Result, !IO),

	(
		Result = yes(ForeignLangs),
		% Output any outline foreign_code to the appropriate foreign
		% language file.
		list__foldl(output_foreign_file(MLDS),
			set__to_sorted_list(ForeignLangs), !IO)
	;
		% An I/O error occurred; output_to_file has already reported
		% an error message, so we don't need to do anything here.
		Result = no
	).

:- pred output_foreign_file(mlds::in, foreign_language::in,
	io::di, io::uo) is det.

output_foreign_file(MLDS, ForeignLang, !IO) :-
	ModuleName = mlds__get_module_name(MLDS),
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
		error("mlds_to_ilasm__output_foreign_file: " ++
			"unexpected language")
	).

:- pred handle_foreign_lang(foreign_language::in,
	pred(mlds, io__state, io__state)::out(pred(in, di, uo) is det)) is det.

handle_foreign_lang(managed_cplusplus, output_managed_code(managed_cplusplus)).
handle_foreign_lang(csharp, output_managed_code(csharp)).
handle_foreign_lang(c, _) :-
	sorry(this_file, "language C foreign code not supported").
handle_foreign_lang(il, _) :-
	sorry(this_file, "language IL foreign code not supported").
handle_foreign_lang(java, _) :-
	sorry(this_file, "language Java foreign code not supported").

	%
	% Generate the `.il' file.
	% Returns the set of foreign language
	%
:- pred output_assembler(mlds::in, set(foreign_language)::out,
	io::di, io::uo) is det.

output_assembler(MLDS, ForeignLangs, !IO) :-
	MLDS = mlds(ModuleName, _ForeignCode, _Imports, _Defns,
		_InitPreds, _FinalPreds),
	output_src_start(ModuleName, !IO),
	io__nl(!IO),

	generate_il(MLDS, ILAsm0, ForeignLangs, !IO),

		% Perform peephole optimization if requested.  If peephole
		% optimization was not requested, we may still need to invoke
		% the peephole optimization pass, because some of the peephole
		% optimizations are actually needed for verifiability of the
		% generated IL.
	globals__io_lookup_bool_option(optimize_peep, Peephole, !IO),
	globals__io_lookup_bool_option(verifiable_code, Verifiable, !IO),
	( Peephole = yes ->
		VerifyOnly = no,
		il_peephole__optimize(VerifyOnly, ILAsm0, ILAsm)
	; Verifiable = yes ->
		VerifyOnly = yes,
		il_peephole__optimize(VerifyOnly, ILAsm0, ILAsm)
	;
		ILAsm0 = ILAsm
	),

		% Output the assembly.
	ilasm__output(ILAsm, !IO),
	output_src_end(ModuleName, !IO).

:- func this_file = string.

this_file = "mlds_to_ilasm.m".

:- end_module mlds_to_ilasm.
