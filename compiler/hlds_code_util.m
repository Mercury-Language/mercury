%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% file: hlds_code_util.m.
%
% various utilities routines for use during hlds generation.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- module hlds__hlds_code_util.
:- interface.

:- import_module hlds__hlds_module.

% XXX some of the stuff from code_util.m should be moved here.

:- type hlds_code_util ---> suppress_warning_about_nothing_exported.

	% Are equivalence types fully expanded on this backend?
:- pred are_equivalence_types_expanded(module_info::in) is semidet.

:- implementation.

:- import_module libs__globals, libs__options.
:- import_module bool.

are_equivalence_types_expanded(ModuleInfo) :-
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, highlevel_data, HighLevelData),
	HighLevelData = yes,
	globals__get_target(Globals, Target),
	( Target = il ; Target = java).

	
