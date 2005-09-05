%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: enum.m.
% Author: stayl.
% Stability: medium
%
% This module provides the typeclass `enum', which describes types
% which can be converted to and from integers without loss of information.
%
%-----------------------------------------------------------------------------%

:- module enum.

:- interface.

	% For all instances the following must hold:
	%	all [X] (X = from_int(to_int(X)))
	%	all [Int] (some [Y] Int = to_int(Y) => from_int(Int) = Y)
:- typeclass enum(T) where [
	func to_int(T) = int,
	func from_int(int) = T is semidet
].

%-----------------------------------------------------------------------------%
