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

:- import_module array, bag, bimap, bintree, bintree_set, bool, char.
:- import_module dir, eqvclass, float, math, getopt, graph, group, int, io.
:- import_module list, map, multi_map, pqueue, queue, random, relation, require.
:- import_module set, set_bbbtree, set_ordlist, set_unordlist, stack, std_util.
:- import_module string, term, term_io, tree234, uniq_array, varset, store.
:- import_module rbtree, parser, lexer, ops.

:- pred library__version(string::out) is det.


%---------------------------------------------------------------------------%

:- implementation.

library__version("0.6 alpha").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
