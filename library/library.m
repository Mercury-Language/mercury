%---------------------------------------------------------------------------%
% Copyright (C) 1993-2004 The University of Melbourne.
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
:- import_module array.
:- import_module array2d.
:- import_module assoc_list.
:- import_module bag.
:- import_module benchmarking.
:- import_module bimap.
:- import_module bintree.
:- import_module bintree_set.
:- import_module bitmap.
:- import_module bool.
:- import_module bt_array.
:- import_module builtin.
:- import_module char.
:- import_module construct.
:- import_module cord.
:- import_module counter.
:- import_module deconstruct.
:- import_module dir.
:- import_module enum.
:- import_module eqvclass.
:- import_module exception.
:- import_module float.
:- import_module gc.
:- import_module getopt.
:- import_module graph.
:- import_module group.
:- import_module hash_table.
:- import_module int.
:- import_module integer.
:- import_module io.
:- import_module lexer.
:- import_module list.
:- import_module map.
:- import_module math.
:- import_module multi_map.
:- import_module ops.
:- import_module parser.
:- import_module pprint.
:- import_module pqueue.
:- import_module prolog.
:- import_module queue.
:- import_module random.
:- import_module rational.
:- import_module rbtree.
:- import_module relation.
:- import_module require.
:- import_module set.
:- import_module set_bbbtree.
:- import_module set_ordlist.
:- import_module set_unordlist.
:- import_module sparse_bitset.
:- import_module stack.
:- import_module std_util.
:- import_module store.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module time.
:- import_module tree234.
:- import_module type_desc.
:- import_module varset.
:- import_module version_array.
:- import_module version_array2d.
:- import_module version_bitmap.
:- import_module version_hash_table.
:- import_module version_store.
:- import_module version_types.

% The modules intended for Mercury system implementors.
:- import_module private_builtin.
:- import_module profiling_builtin.
:- import_module rtti_implementation.
:- import_module table_builtin.
:- import_module term_size_prof_builtin.

% library__version must be implemented using pragma foreign_proc,
% so we can get at the MR_VERSION and MR_FULLARCH configuration
% parameters.  We can't just generate library.m from library.m.in
% at configuration time, because that would cause bootstrapping problems --
% we might not have a Mercury compiler around to compile library.m with.

:- pragma foreign_proc("C",
	library__version(Version::out),
	[will_not_call_mercury, promise_pure],
"
	MR_ConstString version_string = 
		MR_VERSION "", configured for "" MR_FULLARCH;
	/*
	** Cast away const needed here, because Mercury declares Version
	** with type MR_String rather than MR_ConstString.
	*/
	Version = (MR_String) (MR_Word) version_string;
").

:- pragma foreign_proc("C#",
	library__version(Version::out),
	[will_not_call_mercury, promise_pure],
"
	Version = mercury.runtime.Constants.MR_VERSION + "" configured for ""
			+ mercury.runtime.Constants.MR_FULLARCH;
").

:- pragma foreign_proc("Java",
	library__version(Version::out),
	[will_not_call_mercury, promise_pure],
"
	Version = mercury.runtime.Constants.MR_VERSION + "" configured for ""
			+ mercury.runtime.Constants.MR_FULLARCH;
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
