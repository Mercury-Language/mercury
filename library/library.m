%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module imports all the modules in the Mercury library.
%
% It is used as a way for the Makefiles to know which library interface
% files, objects, etc., need to be installed, and it is also linked to
% create the executable invoked by the `mnp' script.
% 
% ---------------------------------------------------------------------------%
% ---------------------------------------------------------------------------%

:- module library.

:- interface.

:- import_module array, bag, bimap, bintree, bintree_set, char, dir.
:- import_module eqvclass, float, graph, group, int, io.
:- import_module list, map, pqueue, queue, random, relation, require.
:- import_module set, stack, std_util, string, term, term_io.
:- import_module tree234, varset, store, rbtree.

:- import_module parser, lexer, ops.

:- implementation.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
