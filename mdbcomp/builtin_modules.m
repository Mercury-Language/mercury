%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2018, 2024 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This module contains information about various modules of the Mercury
% standard library that serve to implement various kinds of builtin operations.
% The compiler can generate references to types, predicates or functions
% in these modules, and the debugger and the profilers need access to
% many of these as well.
%
%---------------------------------------------------------------------------%

:- module mdbcomp.builtin_modules.
:- interface.

:- import_module mdbcomp.sym_name.

:- import_module list.

% XXX We do not document whether the functions in this section are
% intended to return, when we start add a "std" wrapper around the
% names of modules in the Mercury standard library,
%
% - the name without the wrapper, or
% - the name with the wrapper.
%
% At the moment, the two are identical, so the issue is moot.
% However, different parts of the rest of the system seem to make
% conflicting assumptions about the answer, and these will have to be
% sorted out if/when we add that wrapper.

    % Returns all the modules which are automatically imported.
    %
:- func all_builtin_modules = list(sym_name).

    % Returns the name of the module containing public builtins;
    % originally this was "mercury_builtin", but it later became
    % just "builtin", and it may eventually be renamed "std.builtin".
    % This module is automatically imported, as if via `import_module'.
    %
:- func mercury_public_builtin_module = sym_name.

    % Returns the name of the module containing private builtins;
    % traditionally this was "mercury_builtin", but it later became
    % "private_builtin", and it may eventually be renamed
    % "std.private_builtin". This module is automatically imported,
    % as if via `use_module'.
    %
:- func mercury_private_builtin_module = sym_name.

    % Returns the name of the module containing builtins for region-based
    % memory management.  This module is automatically imported iff
    % RBMM is enabled.
    %
:- func mercury_region_builtin_module = sym_name.

    % Returns the name of the module containing builtins for software
    % transactional memory.
    % This module is automatically imported iff STM is used in a module.
    %
:- func mercury_stm_builtin_module = sym_name.

    % Returns the name of the module implementing exceptions.
    % This module is automatically imported iff STM is used in a module.
    %
:- func mercury_exception_module = sym_name.

    % Returns the name of the module implementing univs.
    % This module is automatically imported iff STM is used in a module.
    %
:- func mercury_univ_module = sym_name.

    % Returns the name of the module containing builtins for tabling;
    % originally these were in "private_builtin", but were then moved into
    % a separate module. This module is automatically imported iff any
    % predicate is tabled.
    %
:- func mercury_table_builtin_module = sym_name.

    % Returns the name of the module that handles tabling statistics.
    % This is separate from table_builtin, since its contents need to be
    % visible to users, while the contents of table_builtin are private.
    % This module is automatically imported iff any tabled predicate
    % gather statistics.
    %
:- func mercury_table_statistics_module = sym_name.

    % Returns the name of the module containing the builtins for deep
    % profiling. This module is automatically imported iff deep profiling
    % is enabled.
    %
:- func mercury_profiling_builtin_module = sym_name.

    % Returns the name of the module containing the builtins for term size
    % profiling. This module is automatically imported iff term size profiling
    % is enabled.
    %
:- func mercury_term_size_prof_builtin_module = sym_name.

    % Returns the name of the module containing the builtins for parallelism.
    % This module is automatically imported iff building in a .par grade.
    %
:- func mercury_par_builtin_module = sym_name.

    % Returns the name of the module containing the RTTI implementation for
    % certain backends.
    %
:- func mercury_rtti_implementation_builtin_module = sym_name.

    % Returns the name of the module containing the builtins for the
    % source-to-source debugger.
    %
:- func mercury_ssdb_builtin_module = sym_name.

    % Returns the name of the list module.
    %
:- func mercury_list_module = sym_name.

    % Returns the name of the string module.
    %
:- func mercury_string_module = sym_name.

    % Returns the name of the string.builder module.
    %
:- func mercury_string_builder_module = sym_name.

    % Returns the name of the string.format module.
    %
:- func mercury_string_format_module = sym_name.

    % Returns the name of the string.parse_util module.
    %
:- func mercury_string_parse_util_module = sym_name.

    % Returns the name of the int module.
    %
:- func mercury_int_module = sym_name.

    % Returns the name of the I/O module.
    %
:- func mercury_io_module = sym_name.

    % Returns the name of the stream module.
    %
:- func mercury_stream_module = sym_name.

    % Succeeds iff the specified module is one of the builtin modules listed
    % above which may be automatically imported.
    %
:- pred any_mercury_builtin_module(sym_name::in) is semidet.

    % Succeeds iff the specified module will never be traced.
    %
:- pred non_traced_mercury_builtin_module(sym_name::in) is semidet.

%---------------------------------------------------------------------------%

    % Returns the sym_name of the module with the given name in the
    % Mercury standard library. In the future, this may add a wrapper
    % around the original module name, though for now, this function
    % is a no-op.
    %
    % NOTE: If you want to change the status quo, see the XXX above
    % about conflicting assumptions.
    %
:- func maybe_add_stdlib_wrapper(module_name) = module_name.

    % Remove any wrapper added by maybe_add_stdlib_wrapper, without checking
    % whether the module name inside is actually the name of a module
    % in the Mercury standard library or not. If you want that check,
    % call is_std_lib_module_name instead. This predicate is intended
    % for uses which follow a call to the predicate with tests for
    % individual, specific standard library modules. In such cases,
    % a generic test for "does this module name occur in the standard library"
    % would be useless work.
    %
    % At the moment, maybe_add_stdlib_wrapper does not add a wrapper,
    % so cannot fail to find it. This is why this predicate is deterministic.
    % When we change maybe_add_stdlib_wrapper to add a wrapper, this predicate
    % will change to be semidet.
    % 
:- pred maybe_remove_stdlib_wrapper(module_name::in, module_name::out) is det.

:- pred is_std_lib_module_name(sym_name::in, string::out) is semidet.

:- pred is_mdbcomp_module_name(sym_name::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module library.

%---------------------------------------------------------------------------%

all_builtin_modules = [
    mercury_public_builtin_module,
    mercury_private_builtin_module,
    mercury_region_builtin_module,
    mercury_stm_builtin_module,
    mercury_table_builtin_module,
    mercury_table_statistics_module,
    mercury_profiling_builtin_module,
    mercury_term_size_prof_builtin_module,
    mercury_par_builtin_module,
    mercury_rtti_implementation_builtin_module,
    mercury_ssdb_builtin_module
].

% We may eventually want to put the standard library into a package "std":
% mercury_public_builtin_module = qualified(unqualified("std"), "builtin").
% mercury_private_builtin_module(M) =
%       qualified(unqualified("std"), "private_builtin"))).
mercury_public_builtin_module = unqualified("builtin").
mercury_private_builtin_module = unqualified("private_builtin").
mercury_region_builtin_module = unqualified("region_builtin").
mercury_stm_builtin_module = unqualified("stm_builtin").
% Exception is a non-builtin module needed by the STM system.
mercury_exception_module = unqualified("exception").
% Univ is a non-builtin module needed by the STM system.
mercury_univ_module = unqualified("univ").
mercury_table_builtin_module = unqualified("table_builtin").
mercury_table_statistics_module = unqualified("table_statistics").
mercury_profiling_builtin_module = unqualified("profiling_builtin").
mercury_term_size_prof_builtin_module = unqualified("term_size_prof_builtin").
mercury_par_builtin_module = unqualified("par_builtin").
mercury_rtti_implementation_builtin_module =
    unqualified("rtti_implementation").
mercury_ssdb_builtin_module = unqualified("ssdb").
mercury_list_module = unqualified("list").
mercury_string_module = unqualified("string").
% string.builder, string.format and string.parse_util are non-builtin modules
% that the compiler needs when compiling away the string format
% interpretation overhead of string.format and similar functions.
mercury_string_builder_module = qualified(unqualified("string"), "builder").
mercury_string_format_module = qualified(unqualified("string"), "format").
mercury_string_parse_util_module =
    qualified(unqualified("string"), "parse_util").
mercury_int_module = unqualified("int").
mercury_io_module = unqualified("io").
mercury_stream_module = unqualified("stream").

any_mercury_builtin_module(Module) :-
    ( Module = mercury_public_builtin_module
    ; Module = mercury_private_builtin_module
    ; Module = mercury_region_builtin_module
    ; Module = mercury_table_builtin_module
    ; Module = mercury_profiling_builtin_module
    ; Module = mercury_term_size_prof_builtin_module
    ; Module = mercury_par_builtin_module
    ; Module = mercury_ssdb_builtin_module
    ).

non_traced_mercury_builtin_module(Module) :-
    ( Module = mercury_table_builtin_module
    ; Module = mercury_profiling_builtin_module
    ; Module = mercury_term_size_prof_builtin_module
    ; Module = mercury_par_builtin_module
    ; Module = mercury_ssdb_builtin_module
    ).

%---------------------------------------------------------------------------%

maybe_add_stdlib_wrapper(ModuleName) = WrappedModuleName :-
    WrappedModuleName = ModuleName.
    % -- not yet:
    % WrappedModuleName = qualified(unqualified("std"), ModuleName),

maybe_remove_stdlib_wrapper(WrappedModuleName, ModuleName) :-
    WrappedModuleName = ModuleName.
    % -- not yet:
    % WrappedModuleName = qualified(unqualified("std"), ModuleName),

is_std_lib_module_name(ModuleName, Name) :-
    maybe_remove_stdlib_wrapper(ModuleName, UnwrappedModuleName),
    Name = sym_name_to_string(UnwrappedModuleName),
    mercury_std_library_module(Name).

is_mdbcomp_module_name(ModuleName) :-
    Name = sym_name_to_string(ModuleName),
    mercury_mdbcomp_module(Name).

%---------------------------------------------------------------------------%
:- end_module mdbcomp.builtin_modules.
%---------------------------------------------------------------------------%
