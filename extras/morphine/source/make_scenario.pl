%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA de Rennes.
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
	sh(Copy),

	concat_string(["patch ", SourceDir, File, " ", SourceDir, PatchFile], Patch),
	print(Patch),nl,
	sh(Patch).

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

	halt.







