%------------------------------------------------------------------------------%
% Copyright (C) 1999-2000 INRIA/INSA de Rennes.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file License in the Morphine distribution.
% 
% Authors : Erwan Jahier <jahier@irisa.fr>, 
%           Mireille Ducassé <ducasse@irisa.fr>
% 
% This file builds the Morphine files. It is loaded from the INSTALL-MORPHINE
% script.

:- module(morphine).

% We are using some files (the scenario handler) that are part of Eclipse.
% To avoid licensing issues, we did not include them in the Morphine distribution.
% But since we need to patch them, we copy them and then apply the patches.
 
patch_opium_files(FileName) :-
	getenv('MERCURY_MORPHINE_DIR', MorphineDir),
	append_strings(MorphineDir, "/source/", SourceDir),
	atom_string(FileName, FileNameStr),
	get_flag(installation_directory, InstallDir),
	append_strings(InstallDir, "/lib_pd/opium_light/", OPIUM_LIGTH_DIR),
	( 
		exists(OPIUM_LIGTH_DIR),
		!
	;
		print("\n*** You need ECLiPSe 4.1 or later, sorry...\n"),
		print("(In particular, you need eclipse_misc.tgz)\n"),
		print("You can download it at http://www.icparc.ic.ac.uk/eclipse/\n"),
		abort
	),
	concat_string([FileNameStr, ".op"], File),
	append_strings("patch.", FileNameStr, PatchFile),

	concat_string(["cp ", OPIUM_LIGTH_DIR, File, " ", SourceDir], Copy),
	print(Copy),nl,
	exec(Copy, []),
	concat_string([SourceDir, File], AbsoluteFileName),
	get_file_info(AbsoluteFileName, size, Size),
	(	
		concat_string(["patch ", AbsoluteFileName, " ", SourceDir, 
			PatchFile], Patch),
		print(Patch), nl,
		exec(Patch, []), 
		% Make sure the patch was applied
		not get_file_info(AbsoluteFileName, size, Size), !
	;
		% Not all the patch commands use the same options; some use 
		% "-i PatchFiles", some others don't...
		% If the previous attempt to patch files fails, we try that:
		concat_string(["patch -i ", SourceDir, PatchFile, " ", 
			AbsoluteFileName], Patch2),
		print(Patch2), nl,
		exec(Patch2, []),
		not get_file_info(AbsoluteFileName, size, Size), !
	;
		print("\n*** Unable to use `patch' system command."),
		print("Make sure that you have `patch' installed "),
		print("and in your PATH. If you already have a "),
		print("version of `patch' installed, and it is "),
		print("not working, then try using the GNU version "),
		print("of patch instead.\n"),
		abort
	).

patch_all_files :-
	patch_opium_files(error),
	patch_opium_files(autoload),
	patch_opium_files(help),
	patch_opium_files(interface),
	patch_opium_files(make),
	patch_opium_files(parameter),
	patch_opium_files(scenario),
	patch_opium_files(scenario_handler),
	patch_opium_files(translate),
	patch_opium_files(types).


:- patch_all_files.

:- 
	getenv('MERCURY_MORPHINE_DIR', MorphineDir),
	append_strings(MorphineDir, "/source/", SourceStr),
	atom_string(Source, SourceStr),

	append_strings(SourceStr, "util.pl", Util),
	compile(Util, morphine),

	append_strings(MorphineDir, "/source/error.op", Error),
	append_strings(MorphineDir, "/source/scenario_handler.op", 
		Scenario_handler),
	append_strings(MorphineDir, "/source/make.op", Make),
	append_strings(MorphineDir, "/source/scenario.op", Scenario),
	append_strings(MorphineDir, "/source/translate.op", Translate),
	append_strings(MorphineDir, "/source/types.op", Types),
	append_strings(MorphineDir, "/source/parameter.op", Parameter),
	append_strings(MorphineDir, "/source/autoload.op", Autoload),
	append_strings(MorphineDir, "/source/interface.op", Interface),
	compile(Scenario_handler, morphine),
	compile(Error, morphine),
	compile(Make, morphine),
	compile(Parameter, morphine),
	compile(Scenario, morphine),
	compile(Autoload, morphine),
	compile(Translate, morphine),
	compile(Types, morphine),
	compile(Interface, morphine),

	build_obj_dir(OD),
	make(scenario_handler, morphine, [active, traceable, global], Source, OD),
	make('morphine_kernel', morphine, [active, traceable, global], Source, OD),
	make('source', morphine, [active, traceable, global], Source, OD),
	make('display', morphine, [active, traceable, global], Source, OD),
	make('step_by_step', morphine, [active, traceable, global], Source, OD),
	make(collect, morphine, [active, traceable, global], Source, OD),
	make(control_flow, morphine, [active, traceable, global], Source, OD),
	make(help, morphine, [active, traceable, global], Source, OD),

	concat_string([
		"\nThe installation of Morphine succeeded!\n",
		"Don't forget to add ", MorphineDir,"\nand ",
		MorphineDir, "/bin to your PATH.\n",
		"Make sure that the executables mmc and eclipse are ",
		"accessible from your PATH too.\n"], SuccessMsg),
	print(SuccessMsg),
	halt.







