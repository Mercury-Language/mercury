%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains the HLDS data structure, together with modules
% for creating and manipulating it.
%

:- module hlds.
:- interface.
:- import_module parse_tree.

%-----------------------------------------------------------------------------%

% The HLDS data structure itself
:- include_module assertion.
:- include_module hlds_data.
:- include_module hlds_goal.
:- include_module hlds_llds.
:- include_module hlds_module.
:- include_module hlds_pred.
:- include_module instmap.
:- include_module special_pred.

% Modules for creating the HLDS
:- include_module make_hlds.
:- include_module make_tags.
:- include_module quantification.

% Modules for pretty-printing it.
:- include_module hlds_out.

% Miscellaneous utilities.
:- include_module arg_info.
:- include_module code_model.
:- include_module goal_form.
:- include_module goal_util.
:- include_module hlds_code_util.
:- include_module hlds_error_util.
:- include_module passes_aux.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module check_hlds.		% needed for unify_proc__unify_proc_id,
					% etc.
:- import_module transform_hlds.	% needed for mmc_analysis, term_util

:- end_module hlds.

%-----------------------------------------------------------------------------%
