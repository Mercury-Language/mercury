%---------------------------------------------------------------------------%
% Copyright (C) 1993-2002 The University of Melbourne.
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
% to mercury_std_library_module/1 in compiler/modules.m. Conversely, this
% should list all the modules named by mercury_std_library_module, except
% library itself.
%
% Please keep both parts of this list in alphabetical order.

% The modules intended for application programmers.
:- import_module array, assoc_list, bag, benchmarking.
:- import_module bimap, bintree, bintree_set, bitmap, bool, bt_array, builtin.
:- import_module char, construct, counter, deconstruct, dir.
:- import_module enum, eqvclass, exception.
:- import_module float, gc, getopt, graph, group, hash_table.
:- import_module int, integer, io, lexer, list, map, math, multi_map, ops.
:- import_module parser, pprint, pqueue, prolog, queue.
:- import_module random, rational, rbtree, relation, require.
:- import_module set, set_bbbtree, set_ordlist, set_unordlist, sparse_bitset.
:- import_module stack, std_util, store, string.
:- import_module term, term_io, tree234, time, type_desc, varset.

% The modules intended for Mercury system implementors.
:- import_module private_builtin, table_builtin, profiling_builtin.
:- import_module rtti_implementation.

% library__version must be implemented using pragma c_code,
% so we can get at the MR_VERSION and MR_FULLARCH configuration
% parameters.  We can't just generate library.m from library.m.in
% at configuration time, because that would cause bootstrapping problems --
% might not have a Mercury compiler around to compile library.m with.

:- pragma foreign_proc("C",
	library__version(Version::out), [will_not_call_mercury, promise_pure],
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
	library__version(Version::out), [will_not_call_mercury, promise_pure],
"
	// XXX we should use string literals with an S at the start
	// so this code uses just managed types.
	Version = MR_VERSION "", configured for "" MR_FULLARCH;
").

library__version(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("library__version").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
