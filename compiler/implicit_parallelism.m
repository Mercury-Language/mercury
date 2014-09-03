%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: implicit_parallelism.m.
%
% This package holds the modules that the compiler uses to automatically
% (implicitly) add parallelism to originally sequential code.
%
% The compiler does not support Jerome Tannier's old implicit parallelisation
% transformation anymore. If you want to know what it did, look at the CVS
% repository's record for versions of this module prior to the start of 2011.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.implicit_parallelism.
:- interface.

:- include_module introduce_parallelism.
:- include_module push_goals_together.

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.implicit_parallelism.
%-----------------------------------------------------------------------------%
