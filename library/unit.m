%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: unit.m.
% Main author: fjh.
% Stability: high.
% 
% The "unit" type -  stores no information at all.
% 
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module unit.
:- interface.

%---------------------------------------------------------------------------%

:- type unit ---> unit.

:- type unit(T) ---> unit1.

%---------------------------------------------------------------------------%
:- end_module unit.
%---------------------------------------------------------------------------%
