%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2003-2004, 2006-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: analysis.m.
% Main authors: stayl, wangp.
%
% An inter-module analysis framework, as described in
%
%   Nicholas Nethercote. The Analysis Framework of HAL,
%   Chapter 7: Inter-module Analysis, Master's Thesis,
%   University of Melbourne, September 2001, revised April 2002.
%   <http://njn.valgrind.org/pubs/masters2001.ps>.
%
%---------------------------------------------------------------------------%

:- module analysis.
:- interface.

:- include_module framework.
:- include_module operations.

:- implementation.

:- include_module file.

%---------------------------------------------------------------------------%
:- end_module analysis.
%---------------------------------------------------------------------------%
