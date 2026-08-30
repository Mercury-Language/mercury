%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains our second termination analysis system.
%
% This system is described in Julien Fischer: Termination analysis
% for Mercury using convex constraints, Honours report,
% Department of Computer Science and Software Engineering,
% The University of Melbourne, August 2002. This paper is available
% from the Mercury project's papers page.
%
%-----------------------------------------------------------------------------%

:- module termination.term_constr.
:- interface.

:- include_module term_constr_main.
:- include_module term_constr_main_types.
:- include_module term_constr_data.
:- include_module term_constr_util.

:- implementation.

:- include_module term_constr_initial.
:- include_module term_constr_build.    % pass 1
:- include_module term_constr_fixpoint. % pass 1
:- include_module term_constr_pass2.
:- include_module term_constr_errors.

%-----------------------------------------------------------------------------%
:- end_module termination.term_constr.
%-----------------------------------------------------------------------------%
