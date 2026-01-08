%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2007, 2009-2014 The University of Melbourne.
% Copyright (C) 2013-2023, 2025-2026 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This module imports all the modules in the Mercury standard library.
%
% Its main use is telling build systems what modules are part of
% the Mercury standard library. This implicitly specifies what files
% (such as interface files, optimization files, header files, object files,
% and so on) need to be installed as part of installing the library.
%
% It also provides functions that return version and architecture
% information for the Mercury installation.
%
%---------------------------------------------------------------------------%
%
% Many modules in the Mercury standard library are standalone, but
% many others are part of clusters (i.e. groups) of related modules.
% Typically, the modules in each such cluster
%
% - either implement the exact same abstract data type,
% - or they implement slight variations on an abstract data type.
%
% In both cases, the different modules have different strengths and
% weaknesses, with different ones being suitable for different workloads.
% (If one was better than all the others for all workloads, then
% there would be no need for any of the others.)
%
% The main clusters are
%
% - sets
% - lists
% - queues
% - arrays
% - maps
%
% The following sections describe each cluster in detail.
%
%---------------------%
%
% The *set* cluster.
%
% This consists of modules that implement the set abstract data type.
% The modules in this cluster fall into two subclusters.
%
% The first subcluster consists of modules whose sets can store values
% of any type. These modules are
%
% - set_unordlist, which represents sets as unsorted lists
%   with possible duplicates.
%
%   This has the fastest insertion operation, but is suitable
%   only for very small sets.
%
% - set_ordlist, which represents sets as sorted lists without duplicates.
%
%   This has a slower insertion operation than set_unordlist, but faster
%   membership test operations. It is suitable only for small sets.
%
% - set_bbbtree, which represents sets as bounded-balance binary trees
% - set_tree234, which represents sets as 234-trees
%
%   These modules use balanced trees to achieve logarithmic complexity
%   for both insertion and membership test operations (and for some other
%   operations, such as deletion). This makes them suitable for
%   significantly larger sets than set_unordlist or set_ordlist, with
%   the tradeoff being that their higher complexity results in higher
%   constant factors.
%
% - set_ctree234, which represents sets as 234-trees with counts
%
%   This module differs from set_tree234 only in that it explicitly records
%   the count of items in the tree. This makes the "count items" operation
%   constant time, at the cost of extra work on every insertion and deletion
%   to update the count.
%
% - set, which is effectively a short name for one of the modules above.
%   For a long time now, it has been a shorthand for set_ordlist, which is
%   a good general choice for small sets. It has the separate, generic name
%   to allow this to be changed in the future.
%
% The second subcluster consists of modules whose sets can store only
% integers, or values that can be transformed into integers. These modules are
%
% - sparse_bitset, which represents sets as a list of words, with
%   each word being a bitmap that is constrained to represent the integers
%   in the range [N * wordsize, (N * wordsize) + wordsize-1]. The list
%   contains only bitmaps in which at least one bit is set.
%
%   This representation can be thought of a version of set_ordlist with
%   smaller constant factors whenever a bitmap has two or more bits set.
%
%   The point of the alignment constraint is to simplify and speed up
%   union, intersection and difference operations.
%
% - fat_sparse_bitset is a variant of sparse_bitset. Whereas sparse_bitset
%   uses two two-word cells on the heap to represent a bitmap (one cell
%   for the list constructor, and one for the <N,bitmap> pair), this module
%   uses just one three-word heap cell containing N, the bitmap, and the
%   rest of the list.
%
%   fat_sparse_bitset avoids a pointer indirection on each access,
%   but it gets its heap cells from a different list in the memory allocator
%   than sparse_bitset. This is the list of four-word cells, since the Boehm
%   collector, and most others, round up requests to the next multiple of two.
%
%   Whether fat_sparse_bitset is better than sparse_bitset itself depends
%   on the machine architecture and on the workload.
%
% - fatter_sparse_bitset is a variant of fat_sparse_bitset. It is intended
%   to make use of the otherwise-fallow fourth word in a four-word allocation,
%   by storing two adjacent bitmaps between the value of N and the
%   next pointer.
%
%   fatter_sparse_bitset can be faster than fat_sparse_bitset if the
%   probability of some set members falling into each extra bitmap
%   is high enough. If it is too low, then its higher constant factors
%   will make it slower than fat_sparse_bitset.
%
% - tree_bitset is another variant of sparse_bitset. While sparse_bitset
%   represents a set using a list of bitmaps of unlimited length, in which
%   getting to a bitmap representing a large integer requires following
%   a lot of pointers, tree_bitset represents them using a tree whose leaves
%   consist of relatively short lists (up to 32 bitmaps), and whose interior
%   nodes each contain lists of up to 32 nodes at the next lower level.
%
%   This structure can speed up operations by significantly reducing the
%   number of list element traversals, but it also has higher constant
%   factors.
%
% - ranges represents sets using a simple list of ranges, where every integer
%   in a range is in the set.
%
%   This representation is suitable only for sets in which most ranges contain
%   more than one integer. (If most contain only one integer, then either
%   set_ordlist or sparse_bitset would probably be faster.)
%
% - diet represents sets using discrete interval encoding trees.
%   This data structure is also based on ranges (which it calls intervals),
%   but the superstructure it builds on top of them is not a list (as with
%   the ranges module) but a balanced tree (specifically, an AVL tree).
%
%   This representation is also suitable only for sets in which most ranges
%   contain more than one integer, but it has better worst-case complexity
%   (logarithmic rather than linear) for membership test operations.
%
%---------------------%
%
% The *list* cluster.
%
% This consists of modules that implement ordered sequences.
%
% - list is the standard implementation of ordered sequences.
%
% - one_or_more is a variant of the list module that is specifically
%   intended to represent *nonempty* ordered sequences, with empty
%   sequences being ruled out by the type system.
%
% - cord represents lists using a data structure that can append two cords
%   in constant time. (In both the list and one_or_more modules, appending
%   two lists takes time that is proportional to the length of the
%   first list.) The cord representation uses a tree whose shape
%   corresponds to the append operations that created it.
%
%   If you need to build up a long list piece by piece, represent the pieces
%   using cords, and then convert the final cord to a list.
%
% - ra_list also uses trees (actually, a list of perfectly balanced
%   binary trees) to represent ordered sequences, but its objective is
%   to speed up random access to selected list elements (the "ra" part of
%   the name stands for "random access"), instead append operations.
%
% - assoc_list implements one popular use of lists, which is lists of
%   key/value pairs.
%
% - kv_list is a variant of assoc_list that is intended to have
%   better memory behavior. Whereas as assoc_list stores each key/value pair
%   two two-word cells on the heap (one cell for the list constructor,
%   and one for the <key,value> pair), this module uses just one three-word
%   heap cell containing the key, the value, and the pointer to the rest
%   of the list. This can yield better performance on some architectures
%   and for some workloads, at the price of not being able to apply standard
%   list operations to kv_lists (other than the ones supplied by the kv_list
%   module itself).
%
%---------------------%
%
% The *queue* cluster.
%
% This consists of modules that implement ordered sequences which have
% separate operations to *put* items into the sequence, and to *get* items
% out of the sequence.
%
% - queue is the standard implementation of this abstract data type.
%   The sequences it supports have two distinct ends: the tail where
%   new items are put onto the queue, and the front where items get
%   taken off the queue.
%
% - pqueue implements a variant of this abstract data type, the priority
%   queue, which stores key/value pairs, and whose get operation removes from
%   the queue not the pair at the front, but the pair whose key is the
%   smallest. In this abstract data type, the list is ordered not from
%   oldest to newest insertion time, but from low keys to high keys.
%
% - psqueue, which is short for "priority search queue", implements a variant
%   of priority queue which allows operations that return the value associated
%   with a key in the queue (if the key exists in the queue).
%
%---------------------%
%
% The *array* cluster.
%
% This consists of modules that implement arrays, which, conceptually,
% are maps from integers (in the range of 0 to some N) to values.
%
% - array defines the standard one-dimensional array data structure.
%   It supports constant time lookup and update, but guarantees
%   correct operation only if the user never performs any operation
%   whatsoever on any array that has had any updates applied to.
%   This is because its updates are destructive updates. Since the
%   Mercury mode system is not expressive enough to prevent access
%   to outdated versions of arrays, that task falls to programmers.
%
% - array2d defines two-dimensional arrays, but in every other respect,
%   its behavior matches that of the array module.
%
% - version_array is a version of the array module that allows both
%   efficient access to the latest version of the array, and less efficient
%   but still safe access to earlier versions of the array. In general,
%   the more outdated an array is, the less efficient access to its elements
%   will be. This is because a version array effectively consists of
%   the latest version of the array, and a list of "diffs" that allow
%   the reconstruction of earlier and earlier versions.
%
% - version_array2d is effectively a two-dimensional array version
%   of the version_array module.
%
% - bt_array solves the same problem as version_array (safe access
%   to non-current versions of the array), but using a different
%   data structure: random access lists, as in the ra_list module.
%   This has different performance characteristics: it has faster access
%   to old versions, at the cost of logarithmic and not constant time access
%   to the current version.
%
%---------------------%
%
% The *map* cluster.
%
% This consists of modules that store key/value pairs, and support
% the lookup of the value (if any) associated with a given key.
%
% - map is the standard implementation of maps. For a long time now,
%   it has been a shorthand for the tree234 implementation of maps.
%   It has the separate, generic name to allow this to be changed
%   in the future.
%
% - tree234 is the implementation of maps using 2-3-4 trees, which are
%   a kind of balanced tree.
%
% - rbtree is the implementation of maps using red-black trees, which are
%   a different kind of balanced tree.
%
% - multi_map implements one popular kind of maps: maps that allow each key
%   to be mapped to a *list* of values. If a key is mapped to the empty list
%   of values, then operations in this module will delete the key, but since
%   the definition of multi_map is visible from outside its module, it is
%   possible for users of the module to create map entries that map a key
%   to the empty list of values.
%
% - one_or_more_map is a version of multi_maps which does enforce
%   the invariant that if a key exists in the map, then the list of values
%   corresponding to it will be nonempty.
%
% - bimap implements a bidirectional map, which in mathematics is called
%   a bijection. This requires that each key maps to a unique value,
%   and for each value is mapped from a unique key. Bimaps allow
%   not just a lookup of a value given the key, but also the key given value.
%
% - injection implements an injection, which is closely related to, but
%   nevertheless different from, a bijection. Like a bijection, an injection
%   requires that each key map to a unique value, but unlike a bijection,
%   it allows a value to be mapped from more than one key.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module library.
:- interface.

    % version(VersionString, FullarchString)
    %
:- pred version(string::out, string::out) is det.

    % Return the Mercury version string.
    %
:- func mercury_version = string.

    % Return the architecture string.
    %
:- func architecture = string.

    % Return the package version string.
    %
:- func package_version = string.

%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

    % Is a module or submodule intended to be documented in the library
    % reference manual, or not?
    %
:- type doc_or_undoc
    --->    doc
    ;       undoc.

    % Succeeds if-and-only-if the string is the (unqualified) name
    % of one of the modules in the Mercury standard library.
    %
:- pred mercury_std_library_module(string::in) is semidet.

    % Maps the name of each module in the Mercury standard library
    % to an indication of whether we want to document it or not.
    %
    % Can also be used to get the names and doc status of *all* the
    % modules in the Mercury standard library.
    %
:- pred stdlib_module_doc_undoc(string, doc_or_undoc).
:- mode stdlib_module_doc_undoc(in, out) is semidet.
:- mode stdlib_module_doc_undoc(out, out) is multi.

%---------------------------------------------------------------------------%

:- implementation.

% NOTE: If you add a new module to the library, you also need to do the
% following.
%
% - Unless the new module is a private submodule of an existing library module,
%   import the new module in one of the two lists below: the list of modules
%   intended for application programmers, or the list of modules intended
%   only for Mercury system implementors.
%
% - Add the module name to the definition of mercury_std_library_module below.
%   This is regardless of whether
%
%   - the module is for implementors or not, and
%   - whether it is a private submodule or not.
%
%   The mercury_std_library_module predicate should list all the modules
%   in a Mercury source file in the library directory.
%
% - Add the file name either to MODULES_DOC (if it is intended for application
%   programmers, and should therefore be included in the automatically
%   generated user-facing documentation), or to MODULES_UNDOC (if it is
%   intended only for Mercury system implementors, in which case it should
%   not be included in that documentation).
%
% Please keep all these lists in alphabetical order.
% NOTES_TO_IMPLEMENTORS But do it manually. The standard Unix sort program
% NOTES_TO_IMPLEMENTORS screws up the ordering of e.g. int8 vs int16.

% The modules intended for application programmers.
:- import_module array.
:- import_module array2d.
:- import_module assoc_list.
:- import_module backjump.
:- import_module bag.
:- import_module benchmarking.
:- import_module bimap.
:- import_module bit_buffer.
:- import_module bit_buffer.read.
:- import_module bit_buffer.write.
:- import_module bitmap.
:- import_module bool.
:- import_module bt_array.
:- import_module builtin.
:- import_module calendar.
:- import_module char.
:- import_module construct.
:- import_module cord.
:- import_module counter.
:- import_module deconstruct.
:- import_module diet.
:- import_module digraph.
:- import_module dir.
:- import_module edit_distance.
:- import_module edit_seq.
:- import_module enum.
:- import_module eqvclass.
:- import_module exception.
:- import_module fat_sparse_bitset.
:- import_module fatter_sparse_bitset.
:- import_module float.
:- import_module gc.
:- import_module getopt.
:- import_module getopt_io.
:- import_module hash_table.
:- import_module injection.
:- import_module int.
:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module integer.
:- import_module io.
:- import_module io.call_system.
:- import_module io.environment.
:- import_module io.error_util.
:- import_module io.file.
:- import_module io.primitives_read.
:- import_module io.primitives_write.
:- import_module io.stream_db.
:- import_module io.stream_ops.
:- import_module kv_list.
:- import_module lazy.
:- import_module list.
:- import_module map.
:- import_module math.
:- import_module maybe.
:- import_module mercury_term_lexer.
:- import_module mercury_term_parser.
:- import_module multi_map.
:- import_module one_or_more.
:- import_module one_or_more_map.
:- import_module ops.
:- import_module pair.
:- import_module parsing_utils.
:- import_module pprint.
:- import_module pqueue.
:- import_module pretty_printer.
:- import_module prolog.
:- import_module psqueue.
:- import_module queue.
:- import_module ra_list.
:- import_module random.
:- import_module random.sfc16.
:- import_module random.sfc32.
:- import_module random.sfc64.
:- import_module random.system_rng.
:- import_module ranges.
:- import_module rational.
:- import_module rbtree.
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
:- import_module stream.string_writer.
:- import_module string.
:- import_module string.builder.
:- import_module string.format.
:- import_module string.parse_util.
:- import_module table_statistics.
:- import_module term.
:- import_module term_context.
:- import_module term_conversion.
:- import_module term_int.
:- import_module term_io.
:- import_module term_subst.
:- import_module term_to_xml.
:- import_module term_unify.
:- import_module term_vars.
:- import_module thread.
:- import_module time.
:- import_module tree234.
:- import_module tree_bitset.
:- import_module type_desc.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.
:- import_module unit.
:- import_module univ.
:- import_module varset.
:- import_module version_array.
:- import_module version_array2d.
:- import_module version_bitmap.
:- import_module version_hash_table.
:- import_module version_store.

% The modules intended for Mercury system implementors.
% NOTE: changes to this list may need to be reflected
% in mdbcomp/builtin_modules.m.
:- import_module mutvar.
:- import_module par_builtin.
:- import_module private_builtin.
:- import_module profiling_builtin.
:- import_module region_builtin.
:- import_module rtti_implementation.
:- import_module stm_builtin.
:- import_module table_builtin.
:- import_module term_size_prof_builtin.
:- import_module test_bitset.

%---------------------------------------------------------------------------%

version(Version, Fullarch) :-
    Version = mercury_version,
    Fullarch = architecture.

%---------------------%

% mercury_version, architecture and package_version must be implemented using
% pragma foreign_proc, so we can get at the MR_VERSION, MR_FULLARCH and
% MR_PACKAGE configuration parameters (and their C# and Java equivalents).
% We cannot just generate library.m from library.m.in at configuration time,
% because that would cause bootstrapping problems: we might not have a working
% Mercury compiler to compile library.m with.

:- pragma no_inline(func(mercury_version/0)).

:- pragma foreign_proc("C",
    mercury_version = (Version::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    MR_ConstString version_string = MR_VERSION;
    // We need to cast away const here, because Mercury declares Version to
    // have type MR_String, not MR_ConstString.
    Version = (MR_String) (MR_Word) version_string;
").

:- pragma foreign_proc("C#",
    mercury_version = (Version::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Version = runtime.Constants.MR_VERSION;
").

:- pragma foreign_proc("Java",
    mercury_version = (Version::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Version = jmercury.runtime.Constants.MR_VERSION;
").

%---------------------%

:- pragma no_inline(func(architecture/0)).

:- pragma foreign_proc("C",
    architecture = (Fullarch::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    MR_ConstString fullarch_string = MR_FULLARCH;
    // We need to cast away const here, because Mercury declares Fullarch to
    // have type MR_String, not MR_ConstString.
    Fullarch = (MR_String) (MR_Word) fullarch_string;
").

:- pragma foreign_proc("C#",
    architecture = (Fullarch::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Fullarch = runtime.Constants.MR_FULLARCH;
").

:- pragma foreign_proc("Java",
    architecture = (Fullarch::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Fullarch = jmercury.runtime.Constants.MR_FULLARCH;
").

%---------------------%

:- pragma no_inline(func(package_version/0)).

:- pragma foreign_proc("C",
    package_version = (Package::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    MR_ConstString package_string = MR_PKGVERSION;
    // We need to cast away const here, because Mercury declares Package to
    // have type MR_String, not MR_ConstString.
    Package = (MR_String) (MR_Word) package_string;
").

:- pragma foreign_proc("C#",
    package_version = (Package::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Package = runtime.Constants.MR_PKGVERSION;
").

:- pragma foreign_proc("Java",
    package_version = (Package::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Package = jmercury.runtime.Constants.MR_PKGVERSION;
").

%---------------------------------------------------------------------------%

mercury_std_library_module(ModuleName) :-
    stdlib_module_doc_undoc(ModuleName, _).

% NOTE If you ever add any module name here that contains more than two dots,
% i.e. it represents the submodule of a submodule of a submodule, then
% you will need to update the code of the "trusted" predicate in
% browse/declarative_oracle.m to handle such names.
stdlib_module_doc_undoc("array",                        doc).
stdlib_module_doc_undoc("array2d",                      doc).
stdlib_module_doc_undoc("assoc_list",                   doc).
stdlib_module_doc_undoc("backjump",                     undoc).
stdlib_module_doc_undoc("bag",                          doc).
stdlib_module_doc_undoc("benchmarking",                 doc).
stdlib_module_doc_undoc("bimap",                        doc).
stdlib_module_doc_undoc("bitmap",                       doc).
stdlib_module_doc_undoc("bit_buffer",                   doc).
stdlib_module_doc_undoc("bit_buffer.read",              doc).
stdlib_module_doc_undoc("bit_buffer.write",             doc).
stdlib_module_doc_undoc("bool",                         doc).
stdlib_module_doc_undoc("bt_array",                     doc).
stdlib_module_doc_undoc("builtin",                      doc).
stdlib_module_doc_undoc("calendar",                     doc).
stdlib_module_doc_undoc("char",                         doc).
stdlib_module_doc_undoc("construct",                    doc).
stdlib_module_doc_undoc("cord",                         doc).
stdlib_module_doc_undoc("counter",                      doc).
stdlib_module_doc_undoc("deconstruct",                  doc).
stdlib_module_doc_undoc("diet",                         doc).
stdlib_module_doc_undoc("digraph",                      doc).
stdlib_module_doc_undoc("dir",                          doc).
stdlib_module_doc_undoc("edit_distance",                doc).
stdlib_module_doc_undoc("edit_seq",                     doc).
stdlib_module_doc_undoc("enum",                         doc).
stdlib_module_doc_undoc("eqvclass",                     doc).
stdlib_module_doc_undoc("exception",                    doc).
stdlib_module_doc_undoc("fat_sparse_bitset",            doc).
stdlib_module_doc_undoc("fatter_sparse_bitset",         doc).
stdlib_module_doc_undoc("float",                        doc).
stdlib_module_doc_undoc("gc",                           doc).
stdlib_module_doc_undoc("getopt",                       doc).
stdlib_module_doc_undoc("getopt_io",                    doc).
stdlib_module_doc_undoc("hash_table",                   doc).
stdlib_module_doc_undoc("injection",                    doc).
stdlib_module_doc_undoc("int",                          doc).
stdlib_module_doc_undoc("int8",                         doc).
stdlib_module_doc_undoc("int16",                        doc).
stdlib_module_doc_undoc("int32",                        doc).
stdlib_module_doc_undoc("int64",                        doc).
stdlib_module_doc_undoc("integer",                      doc).
stdlib_module_doc_undoc("io",                           doc).
stdlib_module_doc_undoc("io.call_system",               doc).
stdlib_module_doc_undoc("io.environment",               doc).
stdlib_module_doc_undoc("io.error_util",                undoc).
stdlib_module_doc_undoc("io.file",                      doc).
stdlib_module_doc_undoc("io.primitives_read",           undoc).
stdlib_module_doc_undoc("io.primitives_write",          undoc).
stdlib_module_doc_undoc("io.stream_db",                 undoc).
stdlib_module_doc_undoc("io.stream_ops",                undoc).
stdlib_module_doc_undoc("io.text_read",                 undoc).
stdlib_module_doc_undoc("kv_list",                      doc).
stdlib_module_doc_undoc("lazy",                         doc).
stdlib_module_doc_undoc("library",                      doc).
stdlib_module_doc_undoc("list",                         doc).
stdlib_module_doc_undoc("map",                          doc).
stdlib_module_doc_undoc("math",                         doc).
stdlib_module_doc_undoc("maybe",                        doc).
stdlib_module_doc_undoc("mercury_term_lexer",           doc).
stdlib_module_doc_undoc("mercury_term_parser",          doc).
stdlib_module_doc_undoc("multi_map",                    doc).
stdlib_module_doc_undoc("one_or_more",                  doc).
stdlib_module_doc_undoc("one_or_more_map",              doc).
stdlib_module_doc_undoc("mutvar",                       undoc).
stdlib_module_doc_undoc("ops",                          doc).
stdlib_module_doc_undoc("pair",                         doc).
stdlib_module_doc_undoc("par_builtin",                  undoc).
stdlib_module_doc_undoc("parsing_utils",                doc).
stdlib_module_doc_undoc("pprint",                       doc).
stdlib_module_doc_undoc("pqueue",                       doc).
stdlib_module_doc_undoc("pretty_printer",               doc).
stdlib_module_doc_undoc("private_builtin",              undoc).
stdlib_module_doc_undoc("profiling_builtin",            undoc).
stdlib_module_doc_undoc("prolog",                       doc).
stdlib_module_doc_undoc("psqueue",                      doc).
stdlib_module_doc_undoc("queue",                        doc).
stdlib_module_doc_undoc("ra_list",                      doc).
stdlib_module_doc_undoc("random",                       doc).
stdlib_module_doc_undoc("random.sfc16",                 doc).
stdlib_module_doc_undoc("random.sfc32",                 doc).
stdlib_module_doc_undoc("random.sfc64",                 doc).
stdlib_module_doc_undoc("random.system_rng",            doc).
stdlib_module_doc_undoc("ranges",                       doc).
stdlib_module_doc_undoc("rational",                     doc).
stdlib_module_doc_undoc("rbtree",                       doc).
stdlib_module_doc_undoc("region_builtin",               undoc).
stdlib_module_doc_undoc("require",                      doc).
stdlib_module_doc_undoc("robdd",                        undoc).
stdlib_module_doc_undoc("rtree",                        doc).
stdlib_module_doc_undoc("rtti_implementation",          undoc).
stdlib_module_doc_undoc("set",                          doc).
stdlib_module_doc_undoc("set_bbbtree",                  doc).
stdlib_module_doc_undoc("set_ctree234",                 doc).
stdlib_module_doc_undoc("set_ordlist",                  doc).
stdlib_module_doc_undoc("set_tree234",                  doc).
stdlib_module_doc_undoc("set_unordlist",                doc).
stdlib_module_doc_undoc("solutions",                    doc).
stdlib_module_doc_undoc("sparse_bitset",                doc).
stdlib_module_doc_undoc("stack",                        doc).
stdlib_module_doc_undoc("std_util",                     doc).
stdlib_module_doc_undoc("stm_builtin",                  undoc).
stdlib_module_doc_undoc("store",                        doc).
stdlib_module_doc_undoc("stream",                       doc).
stdlib_module_doc_undoc("stream.string_writer",         doc).
stdlib_module_doc_undoc("string",                       doc).
stdlib_module_doc_undoc("string.builder",               doc).
stdlib_module_doc_undoc("string.format",                undoc).
stdlib_module_doc_undoc("string.parse_runtime",         undoc).
stdlib_module_doc_undoc("string.parse_util",            undoc).
stdlib_module_doc_undoc("string.to_string",             undoc).
stdlib_module_doc_undoc("table_builtin",                undoc).
stdlib_module_doc_undoc("table_statistics",             doc).
stdlib_module_doc_undoc("term",                         doc).
stdlib_module_doc_undoc("term_context",                 doc).
stdlib_module_doc_undoc("term_conversion",              doc).
stdlib_module_doc_undoc("term_int",                     doc).
stdlib_module_doc_undoc("term_subst",                   doc).
stdlib_module_doc_undoc("term_unify",                   doc).
stdlib_module_doc_undoc("term_vars",                    doc).
stdlib_module_doc_undoc("term_io",                      doc).
stdlib_module_doc_undoc("term_size_prof_builtin",       undoc).
stdlib_module_doc_undoc("term_to_xml",                  doc).
stdlib_module_doc_undoc("test_bitset",                  undoc).
stdlib_module_doc_undoc("time",                         doc).
stdlib_module_doc_undoc("thread",                       doc).
stdlib_module_doc_undoc("thread.barrier",               doc).
stdlib_module_doc_undoc("thread.channel",               doc).
stdlib_module_doc_undoc("thread.closeable_channel",     doc).
stdlib_module_doc_undoc("thread.future",                doc).
stdlib_module_doc_undoc("thread.mvar",                  doc).
stdlib_module_doc_undoc("thread.semaphore",             doc).
stdlib_module_doc_undoc("tree234",                      doc).
stdlib_module_doc_undoc("tree_bitset",                  doc).
stdlib_module_doc_undoc("type_desc",                    doc).
stdlib_module_doc_undoc("uint",                         doc).
stdlib_module_doc_undoc("uint8",                        doc).
stdlib_module_doc_undoc("uint16",                       doc).
stdlib_module_doc_undoc("uint32",                       doc).
stdlib_module_doc_undoc("uint64",                       doc).
stdlib_module_doc_undoc("unit",                         doc).
stdlib_module_doc_undoc("univ",                         doc).
stdlib_module_doc_undoc("varset",                       doc).
stdlib_module_doc_undoc("version_array",                doc).
stdlib_module_doc_undoc("version_array2d",              doc).
stdlib_module_doc_undoc("version_bitmap",               doc).
stdlib_module_doc_undoc("version_hash_table",           doc).
stdlib_module_doc_undoc("version_store",                doc).

%---------------------------------------------------------------------------%

    % Overall library initializer called before any user code,
    % including module local initializers.
    %
:- pred std_library_init(io::di, io::uo) is det.

:- pragma foreign_export("C", std_library_init(di, uo),
    "ML_std_library_init").
:- pragma foreign_export("C#", std_library_init(di, uo),
    "ML_std_library_init").
:- pragma foreign_export("Java", std_library_init(di, uo),
    "ML_std_library_init").

std_library_init(!IO) :-
    promise_pure (
        impure builtin.init_runtime_hooks,
        io.init_state(!IO)
    ).

    % Overall library finalizer.
    %
:- pred std_library_finalize(io::di, io::uo) is det.

:- pragma foreign_export("C", std_library_finalize(di, uo),
    "ML_std_library_finalize").

std_library_finalize(!IO) :-
    io.finalize_state(!IO).

%---------------------------------------------------------------------------%
:- end_module library.
%---------------------------------------------------------------------------%
