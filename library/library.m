%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module imports all the modules in the Mercury library.
%
% It is used as a way for the Makefiles to know which library interface
% files, objects, etc., need to be installed.
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- module library.
:- interface.

:- pred library.version(string::out) is det.

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

    % Succeeds iff the string is the (unqualified) name of one of the
    % modules in the Mercury standard library.
    %
:- pred mercury_std_library_module(string::in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

% Note: if you add a new module to this list, you must also a new clause to
% mercury_std_library_module/1. Conversely, this should list all the modules
% named by mercury_std_library_module, except library itself.
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
:- import_module getopt_io.
:- import_module graph.
:- import_module group.
:- import_module hash_table.
:- import_module injection.
:- import_module int.
:- import_module integer.
:- import_module io.
:- import_module lexer.
:- import_module list.
:- import_module map.
:- import_module math.
:- import_module maybe.
:- import_module multi_map.
:- import_module ops.
:- import_module pair.
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
:- import_module robdd.
:- import_module rtree.
:- import_module set.
:- import_module set_bbbtree.
:- import_module set_ctree234.
:- import_module set_ordlist.
:- import_module set_tree234.
:- import_module set_unordlist.
:- import_module solutions.
:- import_module sparse_bitset.
:- import_module stack.
:- import_module std_util.
:- import_module store.
:- import_module stream.
:- import_module string.
:- import_module svarray.
:- import_module svbag.
:- import_module svbimap.
:- import_module sveqvclass.
:- import_module svmap.
:- import_module svmulti_map.
:- import_module svqueue.
:- import_module svrelation.
:- import_module svset.
:- import_module svvarset.
:- import_module term.
:- import_module term_io.
:- import_module term_to_xml.
:- import_module time.
:- import_module tree234.
:- import_module type_desc.
:- import_module univ.
:- import_module unit.
:- import_module varset.
:- import_module version_array.
:- import_module version_array2d.
:- import_module version_bitmap.
:- import_module version_hash_table.
:- import_module version_store.
:- import_module version_types.

% The modules intended for Mercury system implementors.
:- import_module mutvar.
:- import_module par_builtin.
:- import_module private_builtin.
:- import_module profiling_builtin.
:- import_module rtti_implementation.
:- import_module table_builtin.
:- import_module term_size_prof_builtin.

% If you need access to unsafe predicates, then in your own workspace,
% temporarily uncomment this import and the line for unsafe in the definition
% of mercury_std_library_module.

% :- import_module unsafe.

% library.version must be implemented using pragma foreign_proc,
% so we can get at the MR_VERSION and MR_FULLARCH configuration
% parameters.  We can't just generate library.m from library.m.in
% at configuration time, because that would cause bootstrapping problems --
% we might not have a Mercury compiler around to compile library.m with.

:- pragma foreign_proc("C",
    library.version(Version::out),
    [will_not_call_mercury, promise_pure, will_not_modify_trail],
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
    library.version(Version::out),
    [will_not_call_mercury, promise_pure],
"
    Version = mercury.runtime.Constants.MR_VERSION + "" configured for ""
        + mercury.runtime.Constants.MR_FULLARCH;
").

:- pragma foreign_proc("Java",
    library.version(Version::out),
    [will_not_call_mercury, promise_pure],
"
    Version = mercury.runtime.Constants.MR_VERSION + "" configured for ""
        + mercury.runtime.Constants.MR_FULLARCH;
").

%---------------------------------------------------------------------------%

mercury_std_library_module("array").
mercury_std_library_module("array2d").
mercury_std_library_module("assoc_list").
mercury_std_library_module("bag").
mercury_std_library_module("benchmarking").
mercury_std_library_module("bimap").
mercury_std_library_module("bintree").
mercury_std_library_module("bintree_set").
mercury_std_library_module("bitmap").
mercury_std_library_module("bool").
mercury_std_library_module("bt_array").
mercury_std_library_module("builtin").
mercury_std_library_module("char").
mercury_std_library_module("construct").
mercury_std_library_module("cord").
mercury_std_library_module("counter").
mercury_std_library_module("deconstruct").
mercury_std_library_module("dir").
mercury_std_library_module("enum").
mercury_std_library_module("eqvclass").
mercury_std_library_module("exception").
mercury_std_library_module("float").
mercury_std_library_module("gc").
mercury_std_library_module("getopt").
mercury_std_library_module("getopt_io").
mercury_std_library_module("graph").
mercury_std_library_module("group").
mercury_std_library_module("hash_table").
mercury_std_library_module("injection").
mercury_std_library_module("int").
mercury_std_library_module("integer").
mercury_std_library_module("io").
mercury_std_library_module("lexer").
mercury_std_library_module("library").
mercury_std_library_module("list").
mercury_std_library_module("map").
mercury_std_library_module("math").
mercury_std_library_module("maybe").
mercury_std_library_module("multi_map").
mercury_std_library_module("mutvar").
mercury_std_library_module("ops").
mercury_std_library_module("pair").
mercury_std_library_module("parser").
mercury_std_library_module("pprint").
mercury_std_library_module("pqueue").
mercury_std_library_module("par_builtin").
mercury_std_library_module("private_builtin").
mercury_std_library_module("profiling_builtin").
mercury_std_library_module("prolog").
mercury_std_library_module("queue").
mercury_std_library_module("random").
mercury_std_library_module("rational").
mercury_std_library_module("rbtree").
mercury_std_library_module("relation").
mercury_std_library_module("require").
mercury_std_library_module("robdd").
mercury_std_library_module("rtree").
mercury_std_library_module("rtti_implementation").
mercury_std_library_module("set").
mercury_std_library_module("set_bbbtree").
mercury_std_library_module("set_ordlist").
mercury_std_library_module("set_unordlist").
mercury_std_library_module("set_ctree234").
mercury_std_library_module("set_tree234").
mercury_std_library_module("solutions").
mercury_std_library_module("sparse_bitset").
mercury_std_library_module("stack").
mercury_std_library_module("std_util").
mercury_std_library_module("store").
mercury_std_library_module("stream").
mercury_std_library_module("string").
mercury_std_library_module("svarray").
mercury_std_library_module("svbag").
mercury_std_library_module("svbimap").
mercury_std_library_module("sveqvclass").
mercury_std_library_module("svmap").
mercury_std_library_module("svmulti_map").
mercury_std_library_module("svqueue").
mercury_std_library_module("svrelation").
mercury_std_library_module("svset").
mercury_std_library_module("svvarset").
mercury_std_library_module("table_builtin").
mercury_std_library_module("term").
mercury_std_library_module("term_io").
mercury_std_library_module("term_size_prof_builtin").
mercury_std_library_module("term_to_xml").
mercury_std_library_module("time").
mercury_std_library_module("tree234").
mercury_std_library_module("type_desc").
mercury_std_library_module("univ").
mercury_std_library_module("unit").
% mercury_std_library_module("unsafe").
mercury_std_library_module("varset").
mercury_std_library_module("version_array").
mercury_std_library_module("version_array2d").
mercury_std_library_module("version_bitmap").
mercury_std_library_module("version_hash_table").
mercury_std_library_module("version_store").
mercury_std_library_module("version_types").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
