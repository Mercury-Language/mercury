%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA de Rennes.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file License in the Morphine distribution.
% 
% Authors : Erwan Jahier <jahier@irisa.fr>, 
%           Mireille Ducassé <ducasse@irisa.fr>
% 
% This file loads the Morphine files. It is loaded from the load_morphine.pl.
 

:- module(morphine).
	

/* 
**	Caution : the order of compilation is relevant!
**	At least for the scenario handler.
*/

:-	
	getenv('MERCURY_MORPHINE_DIR', MorphineDir),
	append_strings(MorphineDir, "/source/morphinefiles/", MorphinefilesStr),
	append_strings(MorphineDir, "/source/", SourceStr),

	% compile the *.op files
	append_strings(SourceStr, "util.pl", Util),
	compile(Util, morphine),

	append_strings(MorphinefilesStr, "autoload.load", AutoloadLoad),
	append_strings(MorphinefilesStr, "scenario.load", ScenarioLoad),
	append_strings(MorphinefilesStr, "scenario_handler.load", ScenarioHLoad),
	append_strings(MorphinefilesStr, "parameter.load", ParameterLoad),
	append_strings(MorphinefilesStr, "translate.load", TranslateLoad),
	append_strings(MorphinefilesStr, "error.load", ErrorLoad),
	append_strings(MorphinefilesStr, "types.load", TypesLoad),
	append_strings(MorphinefilesStr, "make.load", MakeLoad),
	append_strings(MorphinefilesStr, "help.load", HelpLoad), 
	append_strings(MorphinefilesStr, "morphine_kernel.load", MorphineLoad),
	append_strings(MorphinefilesStr, "coprocess.load", CoprocessLoad),
	append_strings(MorphinefilesStr, "exec_control.load", ExecLoad),
	append_strings(MorphinefilesStr, "current_arg.load", Current_argLoad),
	append_strings(MorphinefilesStr, "current_slots.load", Current_slotsLoad),
	append_strings(MorphinefilesStr, "event_attributes.load", EventLoad),
	append_strings(MorphinefilesStr, "forward_move.load", ForwardLoad),
	append_strings(MorphinefilesStr, "display.load", DisplayLoad),
	append_strings(MorphinefilesStr, "browse.load", BrowseLoad),
	append_strings(MorphinefilesStr, "interactive_queries.load", IQLoad),
	append_strings(MorphinefilesStr, "source.load", SourceLoad),
	append_strings(MorphinefilesStr, "step_by_step.load", StepLoad),
	append_strings(MorphinefilesStr, "collect.load", CollectLoad),
	append_strings(MorphinefilesStr, "control_flow.load", CFLoad),

	append_strings(SourceStr, "autoload.op", AutoloadOp), 
	append_strings(SourceStr, "interface.op", InterfaceOp),
	append_strings(SourceStr, "error.op", ErrorOp), 
	append_strings(SourceStr, "help.op", HelpOp), 
	append_strings(SourceStr, "make.op", Makeop), 
	append_strings(SourceStr, "scenario.op", ScenarioOp), 
	append_strings(SourceStr, "scenario_handler.op", ScenarioHOp), 
	append_strings(SourceStr, "types.op", TypesOp), 
	append_strings(SourceStr, "translate.op", TranslateOp), 
	append_strings(SourceStr, "parameter.op", ParameterOp), 
	append_strings(SourceStr, "morphine_kernel.op", MorphineOp),
	append_strings(SourceStr, "coprocess.op", CoprocessOp),
	append_strings(SourceStr, "exec_control.op", ExecOp),
	append_strings(SourceStr, "current_arg.op", Current_argOp),
	append_strings(SourceStr, "current_slots.op", Current_slotsOp),
	append_strings(SourceStr, "event_attributes.op", EventOp),
	append_strings(SourceStr, "forward_move.op", ForwardOp),
	append_strings(SourceStr, "browse.op", BrowseOp),
	append_strings(SourceStr, "interactive_queries.op", IQOp),
	append_strings(SourceStr, "display.op", DisplayOp),
	append_strings(SourceStr, "source.op", SourceOp),
	append_strings(SourceStr, "step_by_step.op", StepOp),
	append_strings(SourceStr, "collect.op", CollectOp),
	append_strings(SourceStr, "control_flow.op", CFOp),

	compile([ScenarioHOp, ErrorOp, Makeop, ParameterOp, ScenarioOp, 
		AutoloadOp, TranslateOp, TypesOp]),

	assert(current_options([active, _, global])),
	setval(already_global, no),	%% XXX [md] pas completement satisfaisant

	compile([ScenarioHLoad, ErrorLoad, MakeLoad, ParameterLoad, ScenarioLoad, 
		AutoloadLoad, TranslateLoad, TypesLoad]),
	initialize_parameters(single, scenario_handler, morphine),
	initialize_parameters(multiple, scenario_handler, morphine),

	compile(InterfaceOp),

	compile([HelpOp, HelpLoad]),
	initialize_parameters(single, help, morphine),
	initialize_parameters(multiple, help, morphine),

	compile([MorphineOp, ForwardOp, Current_slotsOp, Current_argOp, 
		EventOp, ExecOp, CoprocessOp, BrowseOp, IQOp]),
	compile([MorphineLoad, ForwardLoad, Current_slotsLoad, Current_argLoad, 
		EventLoad, ExecLoad, CoprocessLoad, BrowseLoad, IQLoad]),	
	initialize_parameters(single, 'morphine_kernel', morphine),
	initialize_parameters(multiple, 'morphine_kernel', morphine),

	compile([DisplayOp, DisplayLoad]),
	initialize_parameters(single, 'display' , morphine),
	initialize_parameters(multiple, 'display', morphine),

	compile([StepOp, StepLoad]),
	initialize_parameters(single, 'step_by_step', morphine),
	initialize_parameters(multiple, 'step_by_step', morphine),

	compile([CollectOp, CollectLoad]),
	initialize_parameters(single, 'collect', morphine),
	initialize_parameters(multiple, 'collect', morphine),

	compile([CFOp, CFLoad]),
	initialize_parameters(single, 'control_flow', morphine),
	initialize_parameters(multiple, 'control_flow', morphine),

	compile([SourceOp, SourceLoad]),
	initialize_parameters(single, 'source', morphine),
	initialize_parameters(multiple, 'source', morphine),

	setval(already_global, yes).	%% XXX [md] pas completement  satisfaisant


