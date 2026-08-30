%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains our two termination analysis systems.
%
% The two systems' code bases are *almost* wholly separate, but
% they do contain some shared code. This package is the natural home
% for that shared code.
%
%-----------------------------------------------------------------------------%

:- module termination.
:- interface.

:- include_module term_osi.     % Our first termination analyser.
:- include_module term_constr.  % Our second termination analyser.

:- include_module term_norm.

:- implementation.

:- include_module term_post_analysis.
:- include_module term_util.

%-----------------------------------------------------------------------------%
:- end_module termination.
%-----------------------------------------------------------------------------%
