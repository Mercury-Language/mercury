%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module test_qcheck.

:- interface.

:- use_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module nrev.
:- import_module qcheck.

%---------------------------------------------------------------------------%

main -->
    qcheck(qcheck.f(testing), "sample testing").

%---------------------------------------------------------------------------%
%   Invariant test functions
%---------------------------------------------------------------------------%

:- func testing(list(int), list(int)) = property.

testing(Xs, Ys) =
    nrev(Xs ++ Ys) `===` (nrev(Ys) ++ nrev(Xs)).
