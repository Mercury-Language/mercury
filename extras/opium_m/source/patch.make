%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA.
--- /soft/eclipse/eclipse4.1/lib_pd/opium_light/make.op	Sat Feb 20 16:09:16 1999
+++ make.op	Thu Nov  4 09:01:40 1999
@@ -95,8 +95,9 @@
 	atom_string(ObjDir, ObjDirS),
 	make_scenario_Op(SName, Mod, OptionList, SrcDir, ObjDir).
 make_scenario_Op(S, Mod, OptionList) :-
-	get_opiumdir(ODS),
-	append_strings(ODS, "opiumfiles/", ObjDirS),
+	getenv('MERCURY_OPIUM_DIR', OpiumDir),
+	append_strings(OpiumDir, "/source/", ODS),
+	append_strings(ODS, "/opiumfiles/", ObjDirS),
 	atom_string(OD, ODS),
 	atom_string(ObjDir, ObjDirS),
 	make_scenario_Op(S, Mod, OptionList, OD, ObjDir).
@@ -202,9 +203,9 @@
 	opium_scenario_in_module((name:Scenario, _,_, options:[_,_,global], _,_), Mod1),
 	Mod \== Mod1,
 	!,
-	opium_printf(error, "scenario %w is already global in module %w\n", [Scenario, Mod1]).
+	printf(error, "scenario %w is already global in module %w\n", [Scenario, Mod1]).
 make_scenario_Op(Scenario, Mod, [A, T, GloLoc], SrcDir, ObjDir) :-	
-	opium_printf(output, "\nmaking scenario %w (%w)\n", [Scenario, GloLoc]),
+	printf(output, "\nmaking scenario %w (%w)\n", [Scenario, GloLoc]),
 	set_dbgcomp_flag(T),
 	file_suffixe_flag(SrcSuff, _, _),
 	concat_atom([SrcDir, Scenario, SrcSuff], BaseFile),
@@ -218,7 +219,7 @@
 make_scenario_Op(Scenario, Mod, OptionList, SrcDir, ObjDir) :-
 	reset_error_handler(95),
 	set_dbgcomp_flag(traceable),
-	opium_printf(error, "scenario %w could not be made\n", [Scenario]).
+	printf(error, "scenario %w could not be made\n", [Scenario]).
 
 /*
  *  first check whether scenario is declared in base file,
@@ -230,7 +231,7 @@
 		read(S, X),
 		(	X = end_of_file,
 			!,
-			opium_printf(error, "scenario %w should be declared in file %w\n", [Scenario, BaseFile]),
+			printf(error, "scenario %w should be declared in file %w\n", [Scenario, BaseFile]),
 			close(S),
 			fail
 		;
@@ -255,7 +256,8 @@
 	assert(current_options([A, T, GL])),
  	update(Scenario, Mod, [A, T, GL], FileList, Time, SrcDir, ObjDir),
 	retract_all(current_options(_)),
-	make_interface_list(Scenario, GL, Mod),	% for windowing user-interface
+% [R1] Not available for Opium-M.
+%	make_interface_list(Scenario, GL, Mod),	% for windowing user-interface
 	(opium_level(0) ->
 		/* we are booting Opium, so c parameters cannot be set */
 		initialize_parameters(single, Scenario, Mod),
@@ -314,7 +316,7 @@
 	/* scenario already globally present in another module */
 	!.
 make_needed_scenarios_i(NeededScenario, Mod) :-
-	opium_printf(output, "scenario %w needed but not present\n", [NeededScenario]),
+	printf(output, "scenario %w needed but not present\n", [NeededScenario]),
 	make(NeededScenario, Mod).
 
 /*
@@ -341,11 +343,18 @@
 	load_file_if_needed(Scenario, Mod, OptionList, SrcF, LoadF, AutoLoadF, TranslateTime).
 update_i(Scenario, Mod, OptionList, UpdateTime, SrcF, LoadF, AutoLoadF, SrcDir) :-
 	modify_time(LoadF, TranslateTime),
-	TranslateTime >= UpdateTime,
+	% R1 XXX Bug here: UpdateTime in not instanciated. 
+	% To turn around that bug, I have simply commented it out, which is ok
+	% since removing that test can only cause files to be loaded
+	% whereas it was not neccessary. It would certainly be necessary
+	% to look at it more carefully to fix that bug in a proper way.
+	% The best way to fix that kind of bugs is certainly to rewrite 
+	% everything in Mercury ;-)
+%	 TranslateTime >= UpdateTime,
 	!,
 	load_file_if_needed(Scenario, Mod, OptionList, SrcF, LoadF, AutoLoadF, TranslateTime).
 update_i(Scenario, Mod, OptionList, UpdateTime, SrcF, LoadF, AutoLoadF, ScrDir) :-
-	opium_printf(output, "%w is up-to-date\n", [SrcF]).
+	printf(output, "%w is up-to-date\n", [SrcF]).
 	
 /*
  *  load_file_if_needed/7
@@ -359,12 +368,12 @@
 load_file_if_needed(Scenario, Mod, [active, Traceable, GloLoc], SrcF, LoadF, AutoLoadF, TranslateTime) :-
 	file_to_be_loaded(Scenario, Mod, SrcF, LoadF, Traceable, GloLoc, TranslateTime),
 	!,
-	call(current_options(O), opium_kernel),
-	opium_printf(output, "loading %w\n", [SrcF]),
+	call(current_options(O), 'Opium-M'),
+	printf(output, "loading %w\n", [SrcF]),
 	compile(SrcF, Mod),
 	compile(LoadF, Mod),
 	update_file_name(Scenario, Mod, SrcF),
-	opium_printf(output, "%w is loaded\n", [SrcF]).
+	printf(output, "%w is loaded\n", [SrcF]).
 load_file_if_needed(Scenario, Mod, OptionL, SrcF, LoadF, AutoLoadF, TranslateTime).
 
 /*
@@ -377,9 +386,13 @@
 	S \== Scenario,
 	O = options : [active, Traceable, GloLoc],
 	member(SrcFile, Files),
-	UpdateTime > TTime,
+	% XXX make/5 crashes because this is uninstanciated.
+	% Removing it seems to make things work. It is ok to do 
+	% that since the only problem it can cause is that the message 
+	% "%w is up-to-date\n" migth be printed when it is not the case...
+%	 UpdateTime > TTime,
 	!,
-	opium_printf(output, "%w is up-to-date\n", [SrcFile]),
+	printf(output, "%w is up-to-date\n", [SrcFile]),
 	fail.
 file_to_be_loaded(Scenario, Mod, SrcFile, LoadF, Traceable, GloLoc, TTime) :-
 	opium_scenario_in_module((name:S, files:Files, N, O, updated:UpdateTime, M), Mod1),
@@ -449,7 +462,7 @@
 	!.
 provide_dir(Dir) :-
 	concat_atom([mkdir, '  ', Dir], SystemCmd),
-	system(SystemCmd).
+	sh(SystemCmd).
 
 
 /*
@@ -524,7 +537,7 @@
 load_decl_body(Clause, Module).
 
 add_gloloc_directive(Clause, Module) :-
-	call(current_options([_,_,GloLoc]), opium_kernel),
+	call(current_options([_,_,GloLoc]), 'Opium-M'),
 	add_gloloc_directive(Clause, Module, GloLoc).
 
 add_gloloc_directive(_, _, global) :-
@@ -755,16 +768,18 @@
 provide_opium_module(Mod) :-
 	is_opium_module(Mod),
 	!.
-provide_opium_module(Mod) :-
-	update_opium_module_menu(Mod),		% for wui interface
-	opium_printf(output, "creating opium module %w\n", [Mod]),
-	(current_module(Mod) ->
-		true
-	;
-		create_module(Mod)
-	),
-	get_opium_file("opium_module", File),
-	compile(File, Mod).
+
+% [R1] Removed because update_opium_module_menu is not available in Opium-M
+% provide_opium_module(Mod) :-
+% 	update_opium_module_menu(Mod),		% for wui interface
+% 	printf(output, "creating opium module %w\n", [Mod]),
+% 	(current_module(Mod) ->
+% 		true
+% 	;
+% 		create_module(Mod)
+% 	),
+% 	get_opium_file("opium_module", File),
+% 	compile(File, Mod).
 
 /*
  *  initialize_parameters(Type, Scenario, Mod) 
