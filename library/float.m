%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: float.m.
% Main author: fjh.
%
% Floating point support.
%
% The interface provided here is rather low-level and not
% syntactically elegant.  It's a building block, not a finished tool.
%
% XXX - What should we do about unification of two Nan's?
%
%---------------------------------------------------------------------------%

:- module float.
:- interface.

:- pred builtin_float_plus(float, float, float).
:- mode builtin_float_plus(in, in, out) is det.

:- pred builtin_float_minus(float, float, float).
:- mode builtin_float_minus(in, in, out) is det.

:- pred builtin_float_times(float, float, float).
:- mode builtin_float_times(in, in, out) is det.

:- pred builtin_float_divide(float, float, float).
:- mode builtin_float_divide(in, in, out) is det.

:- pred builtin_float_gt(float, float).
:- mode builtin_float_gt(in, in) is semidet.

:- pred builtin_float_lt(float, float).
:- mode builtin_float_lt(in, in) is semidet.

:- pred builtin_float_ge(float, float).
:- mode builtin_float_ge(in, in) is semidet.

:- pred builtin_float_le(float, float).
:- mode builtin_float_le(in, in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

/* They're all builtins, which the compiler expands inline. */

%---------------------------------------------------------------------------%
