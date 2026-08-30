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
% At the moment, they are wholly separate, but they do contain
% some very similar code. If these are ever factored out, this package
% is the natural home of the results.
%
%-----------------------------------------------------------------------------%

:- module termination.
:- interface.

:- include_module term_osi.     % Our first termination analyser.
:- include_module term_constr.  % Our second termination analyser.

:- include_module term_norm.

:- implementation.

:- include_module term_post_analysis.

%-----------------------------------------------------------------------------%
:- end_module termination.
%-----------------------------------------------------------------------------%
