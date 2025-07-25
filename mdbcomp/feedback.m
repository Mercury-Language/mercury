%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018, 2021, 2023, 2025 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: feedback.m.
%
% The modules of this package define data structures for representing
% feedback information in Mercury code, as well as procedures for reading
% and writing the feedback files that represent such information on disk.
%
% These modules are included both in the compiler and in the tools that
% generate this feedback data.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdbcomp.feedback.

:- interface.

% If you add any modules here, you should update the definition of
% MDBCOMP_MODULES in deep_profiler/Mmakefile and slice/Mmakefile.
:- include_module feedback_info.
:- include_module automatic_parallelism.

%---------------------------------------------------------------------------%
:- end_module mdbcomp.feedback.
%---------------------------------------------------------------------------%
