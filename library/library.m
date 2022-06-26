%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2007, 2009-2014 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This module imports all the modules in the Mercury library.
%
% It is used as a way for the Makefiles to know which library interface
% files, objects, etc., need to be installed.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module library.
:- interface.

    % version(VersionString, FullarchString)
    %
:- pred library.version(string::out, string::out) is det.

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

    % Is a module or submodule intended to be documented in the library
    % reference manual, or not?
:- type doc_or_undoc
    --->    doc
    ;       undoc.

    % Succeeds iff the string is the (unqualified) name of one of the
    % modules in the Mercury standard library.
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
%   This is regardless of whether the module is for implementors or not,
%   and whether it is a private submodule or not. The
%   mercury_std_library_module predicate should list all the modules
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
:- import_module bitmap.
:- import_module bit_buffer.
:- import_module bit_buffer.read.
:- import_module bit_buffer.write.
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
:- import_module edit_seq.
:- import_module enum.
:- import_module eqvclass.
:- import_module exception.
:- import_module fat_sparse_bitset.
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
:- import_module random.
:- import_module random.sfc16.
:- import_module random.sfc32.
:- import_module random.sfc64.
:- import_module random.system_rng.
:- import_module ra_list.
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
:- import_module term_conversion.
:- import_module term_io.
:- import_module term_to_xml.
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
%
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

% library.version must be implemented using pragma foreign_proc,
% so we can get at the MR_VERSION and MR_FULLARCH configuration
% parameters.  We can't just generate library.m from library.m.in
% at configuration time, because that would cause bootstrapping problems --
% we might not have a Mercury compiler around to compile library.m with.

:- pragma no_inline(pred(library.version/2)).

:- pragma foreign_proc("C",
    library.version(Version::out, Fullarch::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    MR_ConstString version_string = MR_VERSION;
    MR_ConstString fullarch_string = MR_FULLARCH;

    // Cast away const needed here, because Mercury declares Version
    // with type MR_String rather than MR_ConstString.
    Version = (MR_String) (MR_Word) version_string;
    Fullarch = (MR_String) (MR_Word) fullarch_string;
").

:- pragma foreign_proc("C#",
    library.version(Version::out, Fullarch::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Version = runtime.Constants.MR_VERSION;
    Fullarch = runtime.Constants.MR_FULLARCH;
").

:- pragma foreign_proc("Java",
    library.version(Version::out, Fullarch::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Version = jmercury.runtime.Constants.MR_VERSION;
    Fullarch = jmercury.runtime.Constants.MR_FULLARCH;
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
stdlib_module_doc_undoc("edit_seq",                     doc).
stdlib_module_doc_undoc("enum",                         doc).
stdlib_module_doc_undoc("eqvclass",                     doc).
stdlib_module_doc_undoc("exception",                    doc).
stdlib_module_doc_undoc("fat_sparse_bitset",            doc).
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
stdlib_module_doc_undoc("term_conversion",              doc).
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
