%---------------------------------------------------------------------------%
% Copyright (C) 1993-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module imports all the modules in the Mercury library.
%
% It is used as a way for the Makefiles to know which library interface
% files, objects, etc., need to be installed.
% 
% ---------------------------------------------------------------------------%
% ---------------------------------------------------------------------------%

:- module library.

:- interface.

:- pred library__version(string::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

% Note: if you add a new module to this list, you must also a new clause
% to mercury_std_library_module/1 in compiler/modules.m.

:- import_module array, assoc_list, bag, benchmarking.
:- import_module bimap, bintree, bintree_set, bool.
:- import_module bt_array, char, counter, dir, enum, eqvclass, float.
:- import_module math, getopt, graph, group, int.
:- import_module io, list, map, multi_map, pqueue, queue, random, relation.
:- import_module require, set, set_bbbtree, set_ordlist, set_unordlist.
:- import_module sparse_bitset, stack, std_util, string, term, term_io.
:- import_module tree234, varset.
:- import_module store, rbtree, parser, lexer, ops.
:- import_module prolog.
:- import_module integer, rational.
:- import_module exception, gc.
:- import_module time.
:- import_module pprint.
:- import_module bitmap.
:- import_module hash_table.

:- import_module rtti_implementation.
:- import_module builtin, private_builtin, table_builtin, profiling_builtin.

% library__version must be implemented using pragma c_code,
% so we can get at the MR_VERSION and MR_FULLARCH configuration
% parameters.  We can't just generate library.m from library.m.in
% at configuration time, because that would cause bootstrapping problems --
% might not have a Mercury compiler around to compile library.m with.

:- pragma foreign_proc("C",
	library__version(Version::out), will_not_call_mercury,
"
	MR_ConstString version_string = 
		MR_VERSION "", configured for "" MR_FULLARCH;
	/*
	** Cast away const needed here, because Mercury declares Version
	** with type String rather than MR_ConstString.
	*/
	Version = (MR_String) (MR_Word) version_string;
").

:- pragma foreign_code("MC++", "
	#include ""mercury_conf.h""
").

:- pragma foreign_proc("MC++",
	library__version(Version::out), will_not_call_mercury,
"
	// XXX we should use string literals with an S at the start
	// so this code uses just managed types.
	Version = MR_VERSION "", configured for "" MR_FULLARCH;
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
