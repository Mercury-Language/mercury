%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA.
% 
% Authors : Erwan Jahier <jahier@irisa.fr>, 
%           Mireille Ducassé <ducasse@irisa.fr>
% 
% This file loads the Opium-M files. It is loaded from the load_opium.pl.
 

:- module('Opium-M').
	

/* 
**	Caution : the order of compilation is relevant!
**	At least for the scenario handler.
*/

:-	
	getenv('MERCURY_OPIUM_DIR', OpiumDir),
	append_strings(OpiumDir, "/source/opiumfiles/", OpiumfilesStr),
	append_strings(OpiumDir, "/source/", SourceStr),

	% compile the *.op files
	append_strings(SourceStr, "util.pl", Util),
	compile(Util, 'Opium-M'),

	append_strings(OpiumfilesStr, "autoload.load", AutoloadLoad),
	append_strings(OpiumfilesStr, "scenario.load", ScenarioLoad),
	append_strings(OpiumfilesStr, "scenario_handler.load", ScenarioHLoad),
	append_strings(OpiumfilesStr, "parameter.load", ParameterLoad),
	append_strings(OpiumfilesStr, "translate.load", TranslateLoad),
	append_strings(OpiumfilesStr, "error.load", ErrorLoad),
	append_strings(OpiumfilesStr, "types.load", TypesLoad),
	append_strings(OpiumfilesStr, "make.load", MakeLoad),
	append_strings(OpiumfilesStr, "help.load", HelpLoad), 
	append_strings(OpiumfilesStr, "opium_m_kernel.load", OpiumLoad),
	append_strings(OpiumfilesStr, "coprocess.load", CoprocessLoad),
	append_strings(OpiumfilesStr, "exec_control.load", ExecLoad),
	append_strings(OpiumfilesStr, "current_arg.load", Current_argLoad),
	append_strings(OpiumfilesStr, "current_slots.load", Current_slotsLoad),
	append_strings(OpiumfilesStr, "event_attributes.load", EventLoad),
	append_strings(OpiumfilesStr, "forward_move.load", ForwardLoad),
	append_strings(OpiumfilesStr, "display.load", DisplayLoad),
	append_strings(OpiumfilesStr, "browse.load", BrowseLoad),
	append_strings(OpiumfilesStr, "interactive_queries.load", IQLoad),
	append_strings(OpiumfilesStr, "source.load", SourceLoad),
	append_strings(OpiumfilesStr, "step_by_step.load", StepLoad),
	append_strings(OpiumfilesStr, "collect.load", CollectLoad),
	append_strings(OpiumfilesStr, "control_flow.load", CFLoad),

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
	append_strings(SourceStr, "opium_m_kernel.op", OpiumOp),
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
	initialize_parameters(single, scenario_handler, 'Opium-M'),
	initialize_parameters(multiple, scenario_handler, 'Opium-M'),

	compile(InterfaceOp),

	compile([HelpOp, HelpLoad]),
	initialize_parameters(single, help, 'Opium-M'),
	initialize_parameters(multiple, help, 'Opium-M'),

	compile([OpiumOp, ForwardOp, Current_slotsOp, Current_argOp, 
		EventOp, ExecOp, CoprocessOp, BrowseOp, IQOp]),
	compile([OpiumLoad, ForwardLoad, Current_slotsLoad, Current_argLoad, 
		EventLoad, ExecLoad, CoprocessLoad, BrowseLoad, IQLoad]),	
	initialize_parameters(single, 'opium_m_kernel', 'Opium-M'),
	initialize_parameters(multiple, 'opium_m_kernel', 'Opium-M'),

	compile([DisplayOp, DisplayLoad]),
	initialize_parameters(single, 'display' , 'Opium-M'),
	initialize_parameters(multiple, 'display', 'Opium-M'),

	compile([StepOp, StepLoad]),
	initialize_parameters(single, 'step_by_step', 'Opium-M'),
	initialize_parameters(multiple, 'step_by_step', 'Opium-M'),

	compile([CollectOp, CollectLoad]),
	initialize_parameters(single, 'collect', 'Opium-M'),
	initialize_parameters(multiple, 'collect', 'Opium-M'),

	compile([CFOp, CFLoad]),
	initialize_parameters(single, 'control_flow', 'Opium-M'),
	initialize_parameters(multiple, 'control_flow', 'Opium-M'),

	compile([SourceOp, SourceLoad]),
	initialize_parameters(single, 'source', 'Opium-M'),
	initialize_parameters(multiple, 'source', 'Opium-M'),

	setval(already_global, yes).	%% XXX [md] pas completement  satisfaisant


