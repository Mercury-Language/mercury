%---------------------------------------------------------------------------%
% Copyright (C) 1993-1997 The University of Melbourne.
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

:- pred library__version(string::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array, assoc_list, bag, bimap, bintree, bintree_set, bool.
:- import_module bt_array, char, dir, eqvclass, float.
:- import_module math, getopt, graph, group, int.
:- import_module io, list, map, multi_map, pqueue, queue, random, relation.
:- import_module require, set, set_bbbtree, set_ordlist, set_unordlist, stack.
:- import_module std_util, string, term, term_io, tree234, varset.
:- import_module store, rbtree, parser, lexer, ops, time.
:- import_module prolog.

library__version("0.6").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
