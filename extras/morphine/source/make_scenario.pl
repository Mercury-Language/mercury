%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA.
% 
% Authors : Erwan Jahier <jahier@irisa.fr>, 
%           Mireille Ducassé <ducasse@irisa.fr>
% 
% This file builds the Opium-M files. It is loaded from the INSTALL-OPIUM-M
% script.

:- module('Opium-M').

% We are using some files (the scenario handler) that are part of Eclipse.
% To avoid licensing issues, we did not include them in the Opium-M distribution.
% But since we need to patch them, we copy them and then apply the patches.
 
patch_opium_files(FileName) :-
	getenv('MERCURY_OPIUM_DIR', OpiumDir),
	append_strings(OpiumDir, "/source/", SourceDir),
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

	concat_string(["patch -i ", SourceDir, PatchFile, " ", SourceDir, File], Patch),
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
	getenv('MERCURY_OPIUM_DIR', OpiumDir),
	append_strings(OpiumDir, "/source/", SourceStr),
	atom_string(Source, SourceStr),

	append_strings(SourceStr, "util.pl", Util),
	compile(Util, 'Opium-M'),

	append_strings(OpiumDir, "/source/error.op", Error),
	append_strings(OpiumDir, "/source/scenario_handler.op", 
		Scenario_handler),
	append_strings(OpiumDir, "/source/make.op", Make),
	append_strings(OpiumDir, "/source/scenario.op", Scenario),
	append_strings(OpiumDir, "/source/translate.op", Translate),
	append_strings(OpiumDir, "/source/types.op", Types),
	append_strings(OpiumDir, "/source/parameter.op", Parameter),
	append_strings(OpiumDir, "/source/autoload.op", Autoload),
	append_strings(OpiumDir, "/source/interface.op", Interface),
	compile(Scenario_handler, 'Opium-M'),
	compile(Error, 'Opium-M'),
	compile(Make, 'Opium-M'),
	compile(Parameter, 'Opium-M'),
	compile(Scenario, 'Opium-M'),
	compile(Autoload, 'Opium-M'),
	compile(Translate, 'Opium-M'),
	compile(Types, 'Opium-M'),
	compile(Interface, 'Opium-M'),

	build_obj_dir(OD),
	make(scenario_handler, 'Opium-M', [active, traceable, global], Source, OD),
	make('opium_m_kernel', 'Opium-M', [active, traceable, global], Source, OD),
	make('source', 'Opium-M', [active, traceable, global], Source, OD),
	make('display', 'Opium-M', [active, traceable, global], Source, OD),
	make('step_by_step', 'Opium-M', [active, traceable, global], Source, OD),
	make(collect, 'Opium-M', [active, traceable, global], Source, OD),
	make(control_flow, 'Opium-M', [active, traceable, global], Source, OD),
	make(help, 'Opium-M', [active, traceable, global], Source, OD),

	halt.







