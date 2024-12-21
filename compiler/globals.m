%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2024 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: globals.m.
% Main author: fjh.
%
% This module exports the `globals' type and associated access predicates.
% The globals type is used to collect together all the various data
% that would be global variables in an imperative language.
% This global data is stored in the io.state.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module libs.globals.
:- interface.

:- import_module libs.file_util.
:- import_module libs.op_mode.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.sym_name. % for module_name
:- import_module parse_tree.
:- import_module parse_tree.prog_data_pragma.

:- import_module bool.
:- import_module getopt.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

    % The main type this module defines. The following sections define
    % the types of some of the components of this type.
    %
:- type globals.

%---------------------%

:- type maybe_stdlib_grades
    --->    stdlib_grades_unknown
    ;       stdlib_grades_known(set(string)).

%---------------------%

:- type backend
    --->    high_level_backend
    ;       low_level_backend.

:- type compilation_target
    --->    target_c        % Generate C code (including GNU C).
    ;       target_csharp   % Generate C#.
    ;       target_java.    % Generate Java.

:- type foreign_language
    --->    lang_c
    ;       lang_csharp
    ;       lang_java.

    % A string representation of the compilation target suitable
    % for use in human-readable error messages.
    %
:- func compilation_target_string(compilation_target) = string.

    % Return if the compilation target uses high-level data.
    %
:- func compilation_target_high_level_data(compilation_target) = bool.

:- func target_lang_to_foreign_export_lang(compilation_target)
    = foreign_language.

    % A string representation of the foreign language suitable
    % for use in human-readable error messages.
    %
:- func foreign_language_string(foreign_language) = string.

    % A string representation of the foreign language suitable
    % for use in machine-readable name mangling.
    %
:- func simple_foreign_language_string(foreign_language) = string.
:- pred simple_foreign_language_string(foreign_language, string).
:- mode simple_foreign_language_string(in, out) is det.
:- mode simple_foreign_language_string(out, in) is semidet.

:- func all_foreign_language_strings = list(string).

%---------------------%

    % We support these two word sizes.
:- type word_size
    --->    word_size_32
    ;       word_size_64.

%---------------------%

    % The GC method specifies how we do garbage collection.
    % The last five alternatives are for the C back-ends;
    % the first alternative is for compiling to C# or Java
    % where the target language implementation handles garbage collection
    % automatically.
    %
:- type gc_method
    --->    gc_automatic
            % It is the responsibility of the target language that
            % we are compiling to handle GC.

    ;       gc_none
            % No garbage collection. However, memory may be recovered on
            % backtracking if the --reclaim-heap-on-*failure options are set.

    ;       gc_boehm
            % The Boehm et al conservative collector.

    ;       gc_boehm_debug
            % Boehm collector with debugging enabled.

    ;       gc_hgc
            % Ralph Becket's hgc conservative collector.

    ;       gc_accurate.
            % Our own Mercury-specific home-grown copying collector.
            % See runtime/mercury_accurate_gc.c and compiler/ml_elim_nested.m.

    % Returns yes if the GC method is conservative, i.e. if it is `boehm'.
    % Conservative GC methods do not support heap reclamation on failure.
    %
:- func gc_is_conservative(gc_method) = bool.

%---------------------%

:- type termination_norm
    --->    norm_simple
    ;       norm_total
    ;       norm_num_data_elems
    ;       norm_size_data_elems.

%---------------------%

:- type may_be_thread_safe == bool.

    % For the C backends, what type of C compiler are we using?
    %
:- type c_compiler_type
    --->    cc_gcc(
                gcc_major_ver :: maybe(int),
                % The major version number, if known.

                gcc_minor_ver :: maybe(int),
                % The minor version number, if known.

                gcc_patch_level :: maybe(int)
                % The patch level, if known.
                % This is only available since gcc 3.0.
            )
    ;       cc_clang(maybe(clang_version))
    ;       cc_cl_x86(maybe(int))  % MSVC targeting x86.
    ;       cc_cl_x64(maybe(int))  % MSVC targeting x86_64 (x64).
    ;       cc_unknown.

:- type clang_version
    --->    clang_version(int, int, int).

    % For the csharp backend, which csharp compiler are we using?
    %
:- type csharp_compiler_type
    --->    csharp_microsoft
    ;       csharp_mono
    ;       csharp_unknown.

%---------------------%

    % Which subdirectories should we put new files in, and which subdirectories
    % should we assume existing files are in?
:- type subdir_setting
    --->    use_cur_dir
            % All files are always in the current directory.
    ;       use_cur_ngs_subdir
            % Files can be either in the current directory,
            % or in a non-grade-specific subdirectory.
    ;       use_cur_ngs_gs_subdir.
            % Files can be in the current directory,
            % in a non-grade-specific subdirectory, or
            % in a grade-specific subdirectory.

%---------------------%

    % The strategy for determining the reuse possibilities, i.e., either
    % reuse is only allowed between terms that have exactly the same cons_id,
    % or reuse is also allowed between terms that have different cons_id, yet
    % where the difference in arity is not bigger than a given threshold.
    %
:- type reuse_strategy
    --->    same_cons_id
    ;       within_n_cells_difference(int).

%---------------------%

    % The env_type specifies the environment in which a Mercury program
    % is being run or is expected to be run. This is used both for the
    % compiler itself, via the --host-env-type option, and for the program
    % being compiled, via the --target-env-type option. Note that users
    % need to be able to specify the former because one Mercury install
    % (e.g., on Windows) can be called from different environments.
    %
:- type env_type
    --->    env_type_posix
            % A generic POSIX-like environment: this covers most Linux systems,
            % Mac OS X, FreeBSD, Solaris etc.

    ;       env_type_cygwin
            % The Cygwin shell and utilities on Windows.

    ;       env_type_msys
            % MinGW with the MSYS environment on Windows.

    ;       env_type_win_cmd
            % The Windows command-line interpreter (cmd.exe).

    ;       env_type_powershell.
            % Windows PowerShell.
            % (NOTE: COMSPEC must be pointing to powershell.exe not cmd.exe.)

%---------------------%

    % The tracing levels to use for a module when doing the source to source
    % debugging transformation.
:- type ssdb_trace_level
    --->    ssdb_none
            % No tracing of this module.

    ;       ssdb_shallow
            % Shallow trace all procedures in this module.

    ;       ssdb_deep.
            % Deep trace all procedures in this module.

%---------------------%

    % This type specifies how the compiler should install files and
    % directories. Values of this type only affect how the compiler itself does
    % file installation; they do not affect file installation done by mmake.
:- type install_method
    --->    install_method_external_cmd
            % Files and directories should be installed by invoking a command
            % (e.g. cp or cp -R) via the shell of the underlying operation
            % system. (See the file_install_cmd/0 type below.)

    ;       install_method_internal_code.
            % Files and directories should be installed by invoking an OS or
            % target language call, or by using predicates implemented using
            % standard Mercury file operations.

%---------------------%

    % This type specifies the external command the compiler uses to install
    % files.
    %
:- type file_install_cmd
    --->    install_cmd_user(
                string         % Cmd.
            )
            % Command specified by the user using --install-command.

    ;       install_cmd_cp.
            % POSIX conformant cp command.

%---------------------%

:- type line_number_range
    --->    line_number_range(
                maybe(int), % The minimum line number, if there is one.
                maybe(int)  % The maximum line number, if there is one.
            ).

    % Map file names to the line number ranges for which to report errors.
    % If a file name has no entry in this map, all its errors should be
    % reported.
    %
    % An entry with the empty string as the key applies to every file.
:- type limit_error_contexts_map == map(string, list(line_number_range)).

%---------------------%

    % Map each extension in the interface_ext, intermod_ext, c_incl_ext and
    % lib_ext types to the list of directories we should search.
:- type ext_dirs_maps
    --->    ext_dirs_maps(
                edm_interface       :: ext_interface_dirs_map,
                edm_intermod        :: ext_intermod_dirs_map,
                edm_c_incl          :: ext_c_incl_dirs_map,
                edm_lib             :: ext_lib_dirs_map,
                edm_stdlib          :: ext_stdlib_dirs_map
            ).

:- type ext_interface_dirs_map == map(interface_ext, list(dir_name)).
:- type ext_intermod_dirs_map == map(intermod_ext, list(dir_name)).
:- type ext_c_incl_dirs_map == map(c_incl_ext, list(dir_name)).
:- type ext_lib_dirs_map == map(lib_ext, list(dir_name)).
:- type ext_stdlib_dirs_map == map(stdlib_ext, list(dir_name)).

:- type interface_ext
    --->    ife_int0
    ;       ife_int1
    ;       ife_int2
    ;       ife_int3
    ;       ife_module_dep
    ;       ife_src.

:- type intermod_ext
    --->    ime_opt_plain
    ;       ime_opt_trans
    ;       ime_an_analysis_date
    ;       ime_an_analysis_status
    ;       ime_an_analysis
    ;       ime_an_imdg
    ;       ime_an_request
    ;       ime_src.         % XXX This option should not exist.

:- type c_incl_ext
    --->    cie_mh
    ;       cie_mih.

:- type lib_ext
    --->    le_a.

:- type stdlib_ext
    --->    sle_init
    ;       sle_jar
    ;       sle_dll.

%---------------------%

:- pred convert_target(string::in, compilation_target::out) is semidet.
:- pred convert_foreign_language(string::in, foreign_language::out) is semidet.
:- pred convert_gc_method(string::in, gc_method::out) is semidet.
:- pred convert_termination_norm(string::in, termination_norm::out) is semidet.
:- pred convert_maybe_thread_safe(string::in, may_be_thread_safe::out)
    is semidet.
:- pred convert_c_compiler_type(string::in, c_compiler_type::out)
    is semidet.
:- pred convert_csharp_compiler_type(string::in, csharp_compiler_type::out)
    is semidet.
:- pred convert_reuse_strategy(string::in, int::in, reuse_strategy::out)
    is semidet.
:- pred convert_env_type(string::in, env_type::out) is semidet.
:- pred convert_ssdb_trace_level(string::in, bool::in, ssdb_trace_level::out)
    is semidet.
:- pred convert_limit_error_contexts(list(string)::in, list(string)::out,
    limit_error_contexts_map::out) is det.

%---------------------%

:- type linked_target_ext_info_map == map(string, linked_target_ext_info).

:- type linked_target_ext_info
    --->    linked_target_ext_info(
                % The user-visible name of the option.
                string,

                % What kind of linked target does the option specify?
                linked_target_kind
            ).

:- type linked_target_kind
    --->    ltk_object_file
    ;       ltk_pic_object_file
    ;       ltk_all_object_file
    ;       ltk_all_pic_object_file
    ;       ltk_executable
    ;       ltk_static_library
    ;       ltk_shared_library
    ;       ltk_library_install
    ;       ltk_library_install_gs_gas.

%---------------------------------------------------------------------------%
%
% Access predicates for the `globals' structure.
%

:- pred globals_init(option_table::in, option_table::in, opt_tuple::in,
    op_mode::in, maybe(feedback_info)::in, file_install_cmd::in,
    trace_suppress_items::in, reuse_strategy::in,
    limit_error_contexts_map::in, linked_target_ext_info_map::in,
    c_compiler_type::in, csharp_compiler_type::in,
    maybe_stdlib_grades::in, compilation_target::in, subdir_setting::in,
    word_size::in, gc_method::in, termination_norm::in, termination_norm::in,
    trace_level::in, ssdb_trace_level::in, may_be_thread_safe::in,
    env_type::in, env_type::in, env_type::in, install_method::in,
    globals::out) is det.

:- pred get_default_options(globals::in, option_table::out) is det.
:- pred get_options(globals::in, option_table::out) is det.
:- pred get_opt_tuple(globals::in, opt_tuple::out) is det.
:- pred get_op_mode(globals::in, op_mode::out) is det.
:- pred get_maybe_feedback_info(globals::in,
    maybe(feedback_info)::out) is det.
:- pred get_file_install_cmd(globals::in, file_install_cmd::out) is det.
:- pred get_ext_dirs_maps(globals::in, ext_dirs_maps::out) is det.
:- pred get_maybe_stdlib_grades(globals::in, maybe_stdlib_grades::out) is det.
:- pred get_trace_suppress(globals::in, trace_suppress_items::out) is det.
:- pred get_reuse_strategy(globals::in, reuse_strategy::out) is det.
:- pred get_limit_error_contexts_map(globals::in,
    limit_error_contexts_map::out) is det.
:- pred get_linked_target_ext_map(globals::in,
    linked_target_ext_info_map::out) is det.
:- pred get_grade_dir(globals::in, string::out) is det.
:- pred get_c_compiler_type(globals::in, c_compiler_type::out) is det.
:- pred get_csharp_compiler_type(globals::in, csharp_compiler_type::out)
    is det.
:- pred get_target(globals::in, compilation_target::out) is det.
:- pred get_subdir_setting(globals::in, subdir_setting::out) is det.
:- pred get_word_size(globals::in, word_size::out) is det.
:- pred get_gc_method(globals::in, gc_method::out) is det.
:- pred get_termination_norm(globals::in, termination_norm::out) is det.
:- pred get_termination2_norm(globals::in, termination_norm::out) is det.
:- pred get_trace_level(globals::in, trace_level::out) is det.
:- pred get_ssdb_trace_level(globals::in, ssdb_trace_level::out) is det.
:- pred get_maybe_thread_safe(globals::in, may_be_thread_safe::out) is det.
:- pred get_host_env_type(globals::in, env_type::out) is det.
:- pred get_system_env_type(globals::in, env_type::out) is det.
:- pred get_target_env_type(globals::in, env_type::out) is det.
:- pred get_install_method(globals::in, install_method::out) is det.

%---------------------%

:- pred set_options(option_table::in, globals::in, globals::out) is det.
:- pred set_opt_tuple(opt_tuple::in, globals::in, globals::out) is det.
:- pred set_op_mode(op_mode::in, globals::in, globals::out) is det.
:- pred set_maybe_feedback_info(maybe(feedback_info)::in,
    globals::in, globals::out) is det.
:- pred set_file_install_cmd(file_install_cmd::in,
    globals::in, globals::out) is det.
:- pred set_ext_dirs_maps(ext_dirs_maps::in,
    globals::in, globals::out) is det.
:- pred set_maybe_stdlib_grades(maybe_stdlib_grades::in,
    globals::in, globals::out) is det.
:- pred set_subdir_setting(subdir_setting::in,
    globals::in, globals::out) is det.
:- pred set_word_size(word_size::in, globals::in, globals::out) is det.
:- pred set_gc_method(gc_method::in, globals::in, globals::out) is det.
:- pred set_trace_level(trace_level::in, globals::in, globals::out) is det.
:- pred set_trace_level_none(globals::in, globals::out) is det.
:- pred set_ssdb_trace_level(ssdb_trace_level::in,
    globals::in, globals::out) is det.

%---------------------%

:- pred get_backend_foreign_languages(globals::in,
    list(foreign_language)::out) is det.

:- pred set_option(option::in, option_data::in, globals::in, globals::out)
    is det.

%---------------------%

:- pred lookup_option(globals::in, option::in, option_data::out) is det.

:- pred lookup_bool_option(globals, option, bool).
:- mode lookup_bool_option(in, in, out) is det.
:- mode lookup_bool_option(in, in, in) is semidet. % implied
:- pred lookup_int_option(globals::in, option::in, int::out) is det.
:- pred lookup_maybe_int_option(globals::in, option::in, maybe(int)::out)
    is det.
:- pred lookup_string_option(globals::in, option::in, string::out) is det.
:- pred lookup_maybe_string_option(globals::in, option::in, maybe(string)::out)
    is det.
:- pred lookup_accumulating_option(globals::in, option::in, list(string)::out)
    is det.

%---------------------------------------------------------------------------%
%
% More complex options.
%

:- func lookup_current_backend(globals) = backend.

    % Check if we should include variable information in the layout
    % structures of call return sites.
    %
:- pred want_return_var_layouts(globals::in, eff_trace_level::in, bool::out)
    is det.

    % Check that the current grade supports tabling of the specified kind.
    %
:- pred current_grade_supports_tabling(globals::in, tabled_eval_method::in,
    bool::out) is det.

    % Check that code compiled in the current grade can execute
    % conjunctions in parallel.
    %
:- pred current_grade_supports_par_conj(globals::in, bool::out) is det.

    % Check that code compiled in the current grade supports concurrent
    % execution, i.e. that spawn/3 will create a new thread instead of
    % aborting execution.
    %
:- pred current_grade_supports_concurrency(globals::in, bool::out) is det.

:- pred get_any_intermod(globals::in, bool::out) is det.

    % Check if we may store double-width floats on the det stack.
    %
:- pred double_width_floats_on_det_stack(globals::in, bool::out) is det.

%---------------------------------------------------------------------------%

:- pred globals_init_mutables(globals::in, io::di, io::uo) is det.

    % Return the number of functions symbols at or above which a ground term's
    % superhomogeneous form should be wrapped in a from_ground_term scope.
    %
:- func get_maybe_from_ground_term_threshold = maybe(int).

    % Return whether this compiler invocation allows a higher order inst
    % to be used as a mode.
    %
:- func get_allow_ho_insts_as_modes = bool.

:- type maybe_smart_recompilation
    --->    do_not_disable_smart_recompilation
    ;       disable_smart_recompilation.

:- pred io_get_disable_smart_recompilation(maybe_smart_recompilation::out,
    io::di, io::uo) is det.
:- pred io_set_disable_smart_recompilation(maybe_smart_recompilation::in,
    io::di, io::uo) is det.

:- type maybe_item_version_numbers
    --->    do_not_disable_item_version_numbers
    ;       disable_item_version_numbers.

:- pred io_get_disable_generate_item_version_numbers(
    maybe_item_version_numbers::out, io::di, io::uo) is det.
:- pred io_set_disable_generate_item_version_numbers(
    maybe_item_version_numbers::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred get_error_output_stream(globals::in, module_name::in,
    io.text_output_stream::out, io::di, io::uo) is det.
:- pred get_inference_output_stream(globals::in, module_name::in,
    io.text_output_stream::out, io::di, io::uo) is det.
:- pred get_debug_output_stream(globals::in, module_name::in,
    io.text_output_stream::out, io::di, io::uo) is det.
:- pred get_recompile_output_stream(globals::in, module_name::in,
    io.text_output_stream::out, io::di, io::uo) is det.

:- pred close_any_specific_compiler_streams(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compute_grade.

:- import_module char.
:- import_module int.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

compilation_target_string(target_c) = "C".
compilation_target_string(target_csharp) = "C#".
compilation_target_string(target_java) = "Java".

compilation_target_high_level_data(target_c) = no.
compilation_target_high_level_data(target_csharp) = yes.
compilation_target_high_level_data(target_java) = yes.

target_lang_to_foreign_export_lang(target_c) = lang_c.
target_lang_to_foreign_export_lang(target_csharp) = lang_csharp.
target_lang_to_foreign_export_lang(target_java) = lang_java.

foreign_language_string(lang_c) = "C".
foreign_language_string(lang_csharp) = "C#".
foreign_language_string(lang_java) = "Java".

simple_foreign_language_string(Lang) = Str :-
    simple_foreign_language_string(Lang, Str).

simple_foreign_language_string(lang_c, "c").
simple_foreign_language_string(lang_csharp, "csharp").
simple_foreign_language_string(lang_java, "java").

all_foreign_language_strings = ["c", "C", "csharp", "C#", "java", "Java"].

gc_is_conservative(gc_boehm) = yes.
gc_is_conservative(gc_boehm_debug) = yes.
gc_is_conservative(gc_hgc) = yes.
gc_is_conservative(gc_none) = no.
gc_is_conservative(gc_accurate) = no.
gc_is_conservative(gc_automatic) = no.

%---------------------------------------------------------------------------%

convert_target(String, Target) :-
    convert_target_2(string.to_lower(String), Target).

:- pred convert_target_2(string::in, compilation_target::out) is semidet.

convert_target_2("csharp", target_csharp).
convert_target_2("java", target_java).
convert_target_2("c", target_c).

:- pred convert_foreign_language_det(string::in, foreign_language::out) is det.

convert_foreign_language_det(String, ForeignLang) :-
    ( if convert_foreign_language(String, ForeignLangPrime) then
        ForeignLang = ForeignLangPrime
    else
        unexpected($pred, "invalid foreign_language string")
    ).

convert_foreign_language(String, ForeignLanguage) :-
    convert_foreign_language_2(string.to_lower(String), ForeignLanguage).

:- pred convert_foreign_language_2(string::in, foreign_language::out)
    is semidet.

convert_foreign_language_2("c", lang_c).
convert_foreign_language_2("c#", lang_csharp).
convert_foreign_language_2("csharp", lang_csharp).
convert_foreign_language_2("c sharp", lang_csharp).
convert_foreign_language_2("java", lang_java).

convert_gc_method("none", gc_none).
convert_gc_method("conservative", gc_boehm).
convert_gc_method("boehm", gc_boehm).
convert_gc_method("boehm_debug", gc_boehm_debug).
convert_gc_method("hgc", gc_hgc).
convert_gc_method("accurate", gc_accurate).
convert_gc_method("automatic", gc_automatic).

convert_termination_norm("simple", norm_simple).
convert_termination_norm("total", norm_total).
convert_termination_norm("num-data-elems", norm_num_data_elems).
convert_termination_norm("size-data-elems", norm_size_data_elems).

convert_maybe_thread_safe("yes", yes).
convert_maybe_thread_safe("no",  no).

convert_c_compiler_type(CC_Str, C_CompilerType) :-
    ( if convert_c_compiler_type_simple(CC_Str, C_CompilerType0) then
        C_CompilerType = C_CompilerType0
    else
        convert_c_compiler_type_with_version(CC_Str, C_CompilerType)
    ).

:- pred convert_c_compiler_type_simple(string::in, c_compiler_type::out)
    is semidet.

convert_c_compiler_type_simple("gcc",      cc_gcc(no, no, no)).
convert_c_compiler_type_simple("clang",    cc_clang(no)).
% For backwards compatibility we currently recognise "msvc"
% as a synonym for "msvc_x86".
convert_c_compiler_type_simple("msvc",     cc_cl_x86(no)).
convert_c_compiler_type_simple("msvc_x86", cc_cl_x86(no)).
convert_c_compiler_type_simple("msvc_x64", cc_cl_x64(no)).
convert_c_compiler_type_simple("unknown",  cc_unknown).

:- pred convert_c_compiler_type_with_version(string::in, c_compiler_type::out)
    is semidet.

convert_c_compiler_type_with_version(CC_Str, C_CompilerType) :-
    Tokens = string.words_separator(unify('_'), CC_Str),
    ( if Tokens = ["gcc", Major, Minor, Patch] then
        convert_gcc_version(Major, Minor, Patch, C_CompilerType)
    else if Tokens = ["clang", Major, Minor, Patch] then
        convert_clang_version(Major, Minor, Patch, C_CompilerType)
    else if
        ( Tokens = ["msvc", Version]
        ; Tokens = ["msvc", "x86", Version]
        )
    then
        convert_msvc_x86_version(Version, C_CompilerType)
    else if Tokens = ["msvc", "x64", Version] then
        convert_msvc_x64_version(Version, C_CompilerType)
    else
        false
    ).

    % Create the value of C compiler type when we have (some) version
    % information for gcc available.
    % We only accept version information that has the following form:
    %
    %   u_u_u
    %   <major>_u_u
    %   <major>_<minor>_<u>
    %   <major>_<minor>_<patch>
    %
    % This means that setting the minor version number will not be accepted
    % when the major version number is unknown. (It wouldn't be useful
    % in any case.)
    %
    % <major> must be >= 2. (Mercury won't work with anything older than that)
    % and <minor> and <patch> must be non-negative.
    %
:- pred convert_gcc_version(string::in, string::in, string::in,
    c_compiler_type::out) is semidet.

convert_gcc_version(MajorStr, MinorStr, PatchStr, C_CompilerType) :-
    ( if
        MajorStr = "u",
        MinorStr = "u",
        PatchStr = "u"
    then
        C_CompilerType = cc_gcc(no, no, no)
    else if
        string.to_int(MajorStr, Major),
        Major >= 2
    then
        ( if
            MinorStr = "u"
        then
            C_CompilerType = cc_gcc(yes(Major), no, no)
        else if
            string.to_int(MinorStr, Minor),
            Minor >= 0
        then
            ( if
                PatchStr = "u"
            then
                C_CompilerType = cc_gcc(yes(Major), yes(Minor), no)
            else if
                string.to_int(PatchStr, Patch),
                Patch >= 0
            then
                C_CompilerType = cc_gcc(yes(Major), yes(Minor), yes(Patch))
            else
                false
            )
        else
            false
        )
    else
        false
    ).

    % Create the value of C compiler type when we have (some) version
    % information for clang available.
    % We only accept version information that has the following form:
    %
    %   <major>_<minor>_<patch>
    %
:- pred convert_clang_version(string::in, string::in, string::in,
    c_compiler_type::out) is semidet.

convert_clang_version(MajorStr, MinorStr, PatchStr, C_CompilerType) :-
    string.to_int(MajorStr, Major),
    string.to_int(MinorStr, Minor),
    string.to_int(PatchStr, Patch),
    Major >= 0, Minor >= 0, Patch >= 0,
    ClangVersion = clang_version(Major, Minor, Patch),
    C_CompilerType = cc_clang(yes(ClangVersion)).

    % Create the value of C compiler type when we have version information
    % for Visual C available.
    % The version number is an integer literal (corresponding to the value
    % of the builtin macro _MSC_VER).
    %
:- pred convert_msvc_x86_version(string::in, c_compiler_type::out) is semidet.

convert_msvc_x86_version(VersionStr, C_CompilerType) :-
    string.to_int(VersionStr, Version),
    Version > 0,
    C_CompilerType = cc_cl_x86(yes(Version)).

:- pred convert_msvc_x64_version(string::in, c_compiler_type::out) is semidet.

convert_msvc_x64_version(VersionStr, C_CompilerType) :-
    string.to_int(VersionStr, Version),
    Version > 0,
    C_CompilerType = cc_cl_x64(yes(Version)).

convert_csharp_compiler_type("microsoft", csharp_microsoft).
convert_csharp_compiler_type("mono", csharp_mono).
convert_csharp_compiler_type("unknown", csharp_unknown).

convert_reuse_strategy("same_cons_id", _, same_cons_id).
convert_reuse_strategy("within_n_cells_difference", NCells,
    within_n_cells_difference(NCells)).

convert_env_type("posix",   env_type_posix).
convert_env_type("cygwin",  env_type_cygwin).
convert_env_type("msys",    env_type_msys).
convert_env_type("windows", env_type_win_cmd).
convert_env_type("powershell", env_type_powershell).

convert_ssdb_trace_level("default", yes, ssdb_deep).
convert_ssdb_trace_level("default", no, ssdb_none).
convert_ssdb_trace_level("none", _, ssdb_none).
convert_ssdb_trace_level("shallow", _, ssdb_shallow).
convert_ssdb_trace_level("deep", _, ssdb_deep).

convert_limit_error_contexts(Options, BadOptions, Map) :-
    convert_limit_error_contexts_acc(Options, [], RevBadOptions,
        map.init, Map),
    list.reverse(RevBadOptions, BadOptions).

:- pred convert_limit_error_contexts_acc(list(string)::in,
    list(string)::in, list(string)::out,
    limit_error_contexts_map::in, limit_error_contexts_map::out) is det.

convert_limit_error_contexts_acc([], !RevBadOptions, !Map).
convert_limit_error_contexts_acc([Option | Options], !RevBadOptions, !Map) :-
    string.to_char_list(Option, OptionChars),
    % Break up the option at the last colon.
    % If there is no colon, the filename will be empty, and the line number
    % range will apply to all files.
    % If there is more than one colon, all but the last are will be considered
    % part of the filename, which can be useful e.g. for drive specifiers
    % on Windows.
    list.reverse(OptionChars, RevOptionChars),
    find_file_name_and_line_range_chars(RevOptionChars, [], LineRangeChars,
        RevFileNameChars),
    FileName = string.from_rev_char_list(RevFileNameChars),
    LineRangeStr = string.from_char_list(LineRangeChars),
    ( if
        string.split_at_char(',', LineRangeStr) = LineRangeStrs,
        list.map(convert_line_number_range, LineRangeStrs, LineNumberRanges)
    then
        map.set(FileName, LineNumberRanges, !Map)
    else
        !:RevBadOptions = [Option | !.RevBadOptions]
    ),
    convert_limit_error_contexts_acc(Options, !RevBadOptions, !Map).

:- pred find_file_name_and_line_range_chars(list(char)::in,
    list(char)::in, list(char)::out, list(char)::out) is det.

find_file_name_and_line_range_chars([], !LineRangeChars, []).
find_file_name_and_line_range_chars([RevChar | RevChars],
        !LineRangeChars, RevFileNameChars) :-
    ( if RevChar = (':') then
        RevFileNameChars = RevChars
    else
        % This un-reverses the line range characters.
        !:LineRangeChars = [RevChar | !.LineRangeChars],
        find_file_name_and_line_range_chars(RevChars,
            !LineRangeChars, RevFileNameChars)
    ).

:- pred convert_line_number_range(string::in, line_number_range::out)
    is semidet.

convert_line_number_range(RangeStr, line_number_range(MaybeMin, MaybeMax)) :-
    string.split_at_char('-', RangeStr) = [MinStr, MaxStr],
    ( if MinStr = "" then
        MaybeMin = no
    else
        string.to_int(MinStr, Min),
        MaybeMin = yes(Min)
    ),
    ( if MaxStr = "" then
        MaybeMax = no
    else
        string.to_int(MaxStr, Max),
        MaybeMax = yes(Max)
    ).

%---------------------------------------------------------------------------%

:- type read_only_globals
    --->    read_only_globals(
                rog_trace_suppress_items        :: trace_suppress_items,
                rog_reuse_strategy              :: reuse_strategy,
                rog_limit_error_contexts_map    :: limit_error_contexts_map,
                rog_linked_target_ext_map       :: linked_target_ext_info_map,
                rog_grade_dir                   :: string,
                rog_c_compiler_type             :: c_compiler_type
            ).

:- type globals
    --->    globals(
                % The default option table, with all options having their
                % default values.
                g_default_options           :: option_table,

                % The actual option table, in which the options we have
                % processed (from the command line, from Mercury.options files,
                % etc) have overridden the affected options' default values.
                g_options                   :: option_table,

                % The writeable fields that each require a full word.
                g_opt_tuple                 :: opt_tuple,
                g_op_mode                   :: op_mode,
                g_maybe_feedback            :: maybe(feedback_info),
                g_file_install_cmd          :: file_install_cmd,
                g_ext_dirs_maps             :: ext_dirs_maps,
                g_maybe_stdlib_grades       :: maybe_stdlib_grades,

                % The readonly fields that each require a full word.
                g_read_only                 :: read_only_globals,

                % The sub-word-sized arguments, clustered together
                % to allow them to be packed together.
                g_csharp_compiler_type      :: csharp_compiler_type,
                g_target                    :: compilation_target,
                g_subdir_setting            :: subdir_setting,
                g_word_size                 :: word_size,
                g_gc_method                 :: gc_method,
                g_termination_norm          :: termination_norm,
                g_termination2_norm         :: termination_norm,
                g_trace_level               :: trace_level,
                g_ssdb_trace_level          :: ssdb_trace_level,
                g_may_be_thread_safe        :: bool,
                g_host_env_type             :: env_type,
                g_system_env_type           :: env_type,
                g_target_env_type           :: env_type,
                g_install_method            :: install_method
            ).

globals_init(DefaultOptions, Options, OptTuple, OpMode,
        MaybeFeedback, FileInstallCmd, TraceSuppress, ReuseStrategy,
        LimitErrorContextsMap, LinkedTargetExtInfoMap,
        C_CompilerType, CSharp_CompilerType, MaybeStdLibGradeSet, Target,
        SubdirSetting, WordSize, GC_Method, TerminationNorm, Termination2Norm,
        TraceLevel, SSTraceLevel, MaybeThreadSafe,
        HostEnvType, SystemEnvType, TargetEnvType, InstallMethod, Globals) :-
    ExtDirsMaps0 = ext_dirs_maps(map.init, map.init, map.init,
        map.init, map.init),
    ReadOnlyGlobals0 = read_only_globals(TraceSuppress, ReuseStrategy,
         LimitErrorContextsMap, LinkedTargetExtInfoMap, "", C_CompilerType),
    Globals0 = globals(DefaultOptions, Options, OptTuple, OpMode,
        MaybeFeedback, FileInstallCmd, ExtDirsMaps0, MaybeStdLibGradeSet,
        ReadOnlyGlobals0, CSharp_CompilerType, Target, SubdirSetting,
        WordSize, GC_Method, TerminationNorm, Termination2Norm,
        TraceLevel, SSTraceLevel, MaybeThreadSafe,
        HostEnvType, SystemEnvType, TargetEnvType, InstallMethod),
    compute_grade(Globals0, GradeDir),
    % We used to strip out any `.picreg' part from GradeDir,
    % while we still had it.
    Globals = Globals0 ^ g_read_only ^ rog_grade_dir := GradeDir.

%---------------------%

get_default_options(Globals, X) :-
    X = Globals ^ g_default_options.
get_options(Globals, X) :-
    X = Globals ^ g_options.
get_opt_tuple(Globals, X) :-
    X = Globals ^ g_opt_tuple.
get_op_mode(Globals, X) :-
    X = Globals ^ g_op_mode.
get_maybe_feedback_info(Globals, X) :-
    X = Globals ^ g_maybe_feedback.
get_file_install_cmd(Globals, X) :-
    X = Globals ^ g_file_install_cmd.
get_ext_dirs_maps(Globals, X) :-
    X = Globals ^ g_ext_dirs_maps.
get_maybe_stdlib_grades(Globals, X) :-
    X = Globals ^ g_maybe_stdlib_grades.
get_trace_suppress(Globals, X) :-
    X = Globals ^ g_read_only ^ rog_trace_suppress_items.
get_reuse_strategy(Globals, X) :-
    X = Globals ^ g_read_only ^ rog_reuse_strategy.
get_limit_error_contexts_map(Globals, X) :-
    X = Globals ^ g_read_only ^ rog_limit_error_contexts_map.
get_linked_target_ext_map(Globals, X) :-
    X = Globals ^ g_read_only ^ rog_linked_target_ext_map.
get_grade_dir(Globals, X) :-
    X = Globals ^ g_read_only ^ rog_grade_dir.
get_c_compiler_type(Globals, X) :-
    X = Globals ^ g_read_only ^ rog_c_compiler_type.
get_csharp_compiler_type(Globals, X) :-
    X = Globals ^ g_csharp_compiler_type.
get_target(Globals, X) :-
    X = Globals ^ g_target.
get_subdir_setting(Globals, X) :-
    X = Globals ^ g_subdir_setting.
get_word_size(Globals, X) :-
    X = Globals ^ g_word_size.
get_gc_method(Globals, X) :-
    X = Globals ^ g_gc_method.
get_termination_norm(Globals, X) :-
    X = Globals ^ g_termination_norm.
get_termination2_norm(Globals, X) :-
    X = Globals ^ g_termination2_norm.
get_trace_level(Globals, X) :-
    X = Globals ^ g_trace_level.
get_ssdb_trace_level(Globals, X) :-
    X = Globals ^ g_ssdb_trace_level.
get_maybe_thread_safe(Globals, X) :-
    X = Globals ^ g_may_be_thread_safe.
get_host_env_type(Globals, X) :-
    X = Globals ^ g_host_env_type.
get_system_env_type(Globals, X) :-
    X = Globals ^ g_system_env_type.
get_target_env_type(Globals, X) :-
    X = Globals ^ g_target_env_type.
get_install_method(Globals, X) :-
    X = Globals ^ g_install_method.

set_options(X, !Globals) :-
    !Globals ^ g_options := X.
set_opt_tuple(X, !Globals) :-
    !Globals ^ g_opt_tuple := X.
set_op_mode(X, !Globals) :-
    !Globals ^ g_op_mode := X.
set_maybe_feedback_info(X, !Globals) :-
    !Globals ^ g_maybe_feedback := X.
set_file_install_cmd(X, !Globals) :-
    !Globals ^ g_file_install_cmd := X.
set_ext_dirs_maps(X, !Globals) :-
    !Globals ^ g_ext_dirs_maps := X.
set_maybe_stdlib_grades(X, !Globals) :-
    !Globals ^ g_maybe_stdlib_grades := X.
set_subdir_setting(X, !Globals) :-
    !Globals ^ g_subdir_setting := X.
set_word_size(X, !Globals) :-
    !Globals ^ g_word_size := X.
set_gc_method(X, !Globals) :-
    !Globals ^ g_gc_method := X.
set_trace_level(X, !Globals) :-
    !Globals ^ g_trace_level := X.
set_trace_level_none(!Globals) :-
    !Globals ^ g_trace_level := trace_level_none.
set_ssdb_trace_level(X, !Globals) :-
    !Globals ^ g_ssdb_trace_level := X.

%---------------------------------------------------------------------------%

get_backend_foreign_languages(Globals, ForeignLangs) :-
    lookup_accumulating_option(Globals, backend_foreign_languages, LangStrs),
    list.map(convert_foreign_language_det, LangStrs, ForeignLangs).

set_option(Option, OptionData, !Globals) :-
    get_options(!.Globals, OptionTable0),
    map.set(Option, OptionData, OptionTable0, OptionTable),
    set_options(OptionTable, !Globals).

%---------------------------------------------------------------------------%

lookup_option(Globals, Option, OptionData) :-
    get_options(Globals, OptionTable),
    map.lookup(OptionTable, Option, OptionData).

lookup_bool_option(Globals, Option, Value) :-
    lookup_option(Globals, Option, OptionData),
    ( if OptionData = bool(Bool) then
        Value = Bool
    else
        unexpected($pred,
            string.format("invalid bool option (%s is %s)",
                [s(string(Option)), s(string(OptionData))]))
    ).

lookup_int_option(Globals, Option, Value) :-
    lookup_option(Globals, Option, OptionData),
    ( if OptionData = int(Int) then
        Value = Int
    else
        unexpected($pred,
            string.format("invalid int option (%s is %s)",
                [s(string(Option)), s(string(OptionData))]))
    ).

lookup_maybe_int_option(Globals, Option, Value) :-
    lookup_option(Globals, Option, OptionData),
    ( if OptionData = maybe_int(MaybeInt) then
        Value = MaybeInt
    else
        unexpected($pred,
            string.format("invalid maybe_int option (%s is %s)",
                [s(string(Option)), s(string(OptionData))]))
    ).

lookup_string_option(Globals, Option, Value) :-
    lookup_option(Globals, Option, OptionData),
    ( if OptionData = string(String) then
        Value = String
    else
        unexpected($pred,
            string.format("invalid string option (%s is %s)",
                [s(string(Option)), s(string(OptionData))]))
    ).

lookup_maybe_string_option(Globals, Option, Value) :-
    lookup_option(Globals, Option, OptionData),
    ( if OptionData = maybe_string(MaybeString) then
        Value = MaybeString
    else
        unexpected($pred,
            string.format("invalid maybe_string option (%s is %s)",
                [s(string(Option)), s(string(OptionData))]))
    ).

lookup_accumulating_option(Globals, Option, Value) :-
    lookup_option(Globals, Option, OptionData),
    ( if OptionData = accumulating(Accumulating) then
        Value = Accumulating
    else
        unexpected($pred,
            string.format("invalid accumulating option (%s is %s)",
                [s(string(Option)), s(string(OptionData))]))
    ).

%---------------------------------------------------------------------------%

lookup_current_backend(Globals) = CurrentBackend :-
    globals.lookup_bool_option(Globals, highlevel_code, HighLevel),
    (
        HighLevel = yes,
        CurrentBackend = high_level_backend
    ;
        HighLevel= no,
        CurrentBackend = low_level_backend
    ).

want_return_var_layouts(Globals, EffTraceLevel, WantReturnLayouts) :-
    % We need to generate layout info for call return labels
    % if we are using accurate gc or if the user wants uplevel printing.
    ( if
        (
            get_gc_method(Globals, GC_Method),
            GC_Method = gc_accurate
        ;
            get_trace_suppress(Globals, TraceSuppress),
            eff_trace_needs_return_info(EffTraceLevel, TraceSuppress) = yes
        )
    then
        WantReturnLayouts = yes
    else
        WantReturnLayouts = no
    ).

current_grade_supports_tabling(Globals, TabledMethod, TablingSupported) :-
    % Please keep this code in sync with find_grade_problems_for_tabling
    % in table_gen.m.
    globals.get_target(Globals, Target),
    globals.get_gc_method(Globals, GC_Method),
    globals.lookup_bool_option(Globals, parallel, Parallel),
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    globals.lookup_bool_option(Globals, profile_calls, ProfileCalls),
    globals.lookup_bool_option(Globals, profile_deep, ProfileDeep),
    % There is no point in checking for threadscope profiling,
    % since it cannot be enabled without Parallel=yes.
    % There is no point in checking for term size profiling or RBMM,
    % since neither is production-ready.
    ( if
        Target = target_c,
        GC_Method \= gc_accurate,
        Parallel = no,
        require_complete_switch [TabledMethod]
        (
            TabledMethod = tabled_minimal(_),
            UseTrail = no,
            ProfileCalls = no,
            ProfileDeep = no
        ;
            ( TabledMethod = tabled_loop_check
            ; TabledMethod = tabled_memo(_)
            ; TabledMethod = tabled_io(_, _)
            )
        )
    then
        TablingSupported = yes
    else
        TablingSupported = no
    ).

current_grade_supports_par_conj(Globals, ParConjSupported) :-
    % Parallel conjunctions only supported on lowlevel C parallel grades.
    % They are not (currently) supported if trailing is enabled.
    %
    globals.get_target(Globals, Target),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.lookup_bool_option(Globals, parallel, Parallel),
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    ( if
        Target = target_c,
        HighLevelCode = no,
        Parallel = yes,
        UseTrail = no
    then
        ParConjSupported = yes
    else
        ParConjSupported = no
    ).

current_grade_supports_concurrency(Globals, ThreadsSupported) :-
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        % In high-level C grades we only support threads in .par grades.
        (
            HighLevelCode = no,
            ThreadsSupported = yes
        ;
            HighLevelCode = yes,
            globals.lookup_bool_option(Globals, parallel, Parallel),
            ThreadsSupported = Parallel
        )
    ;
        ( Target = target_java
        ; Target = target_csharp
        ),
        ThreadsSupported = yes
    ).

get_any_intermod(Globals, AnyIntermod) :-
    lookup_bool_option(Globals, intermodule_optimization, IntermodOpt),
    lookup_bool_option(Globals, intermodule_analysis, IntermodAnalysis),
    AnyIntermod = bool.or(IntermodOpt, IntermodAnalysis).

double_width_floats_on_det_stack(Globals, FloatDwords) :-
    globals.lookup_int_option(Globals, bits_per_word, TargetWordBits),
    globals.lookup_bool_option(Globals, single_prec_float, SinglePrecFloat),
    ( if TargetWordBits = 64 then
        FloatDwords = no
    else if TargetWordBits = 32 then
        (
            SinglePrecFloat = yes,
            FloatDwords = no
        ;
            SinglePrecFloat = no,
            FloatDwords = yes
        )
    else
        unexpected($pred, "bits_per_word not 32 or 64")
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % This mutable controls how big a ground term has to be before the code
    % in superhomogeneous.m wraps it up in a from_ground_term scope.
:- mutable(maybe_from_ground_term_threshold, maybe(int), no, ground,
    [untrailed, attach_to_io_state]).

:- mutable(allow_ho_insts_as_modes, bool, yes, ground,
    [untrailed, attach_to_io_state]).

:- mutable(disable_smart_recompilation,
    maybe_smart_recompilation, do_not_disable_smart_recompilation, ground,
    [untrailed, attach_to_io_state]).

:- mutable(disable_generate_item_version_numbers,
    maybe_item_version_numbers, do_not_disable_item_version_numbers, ground,
    [untrailed, attach_to_io_state]).

%---------------------------------------------------------------------------%

globals_init_mutables(Globals, !IO) :-
    globals.get_opt_tuple(Globals, OptTuple),
    FromGroundTermThreshold = OptTuple ^ ot_from_ground_term_threshold,
    set_maybe_from_ground_term_threshold(yes(FromGroundTermThreshold), !IO),
    globals.lookup_bool_option(Globals, allow_ho_insts_as_modes, Allow),
    set_allow_ho_insts_as_modes(Allow, !IO).

get_maybe_from_ground_term_threshold = MaybeThreshold :-
    promise_pure (
        semipure get_maybe_from_ground_term_threshold(MaybeThreshold)
    ).

get_allow_ho_insts_as_modes = Allow :-
    promise_pure (
        semipure get_allow_ho_insts_as_modes(Allow)
    ).

io_get_disable_smart_recompilation(DisableSmartRecomp, !IO) :-
    get_disable_smart_recompilation(DisableSmartRecomp, !IO).

io_set_disable_smart_recompilation(DisableSmartRecomp, !IO) :-
    set_disable_smart_recompilation(DisableSmartRecomp, !IO).

io_get_disable_generate_item_version_numbers(DisableItemVerions, !IO) :-
    get_disable_generate_item_version_numbers(DisableItemVerions, !IO).

io_set_disable_generate_item_version_numbers(DisableItemVerions, !IO) :-
    set_disable_generate_item_version_numbers(DisableItemVerions, !IO).

%---------------------------------------------------------------------------%

:- type compiler_output_stream
    --->    general_stream(io.text_output_stream)
            % A stream such as stdout or stderr, that is open before
            % the compiler begins running and does not have to closed.
    ;       specific_stream(io.text_output_stream)
            % A stream opened for a specific purpose the first time there was
            % a need for it. Must be closed before the compiler exits.
    ;       no_stream.
            % No stream has been set up for this purpose (yet).

:- mutable(output_stream_error, compiler_output_stream, no_stream, ground,
    [untrailed, attach_to_io_state]).
:- mutable(output_stream_inference, compiler_output_stream, no_stream, ground,
    [untrailed, attach_to_io_state]).
:- mutable(output_stream_debug, compiler_output_stream, no_stream, ground,
    [untrailed, attach_to_io_state]).
:- mutable(output_stream_recompile, compiler_output_stream, no_stream, ground,
    [untrailed, attach_to_io_state]).

get_error_output_stream(Globals, ModuleName, Stream, !IO) :-
    get_output_stream(Globals, ModuleName, error_output_suffix,
        get_output_stream_error, set_output_stream_error, Stream, !IO).

get_inference_output_stream(Globals, ModuleName, Stream, !IO) :-
    get_output_stream(Globals, ModuleName, inference_output_suffix,
        get_output_stream_inference, set_output_stream_inference, Stream, !IO).

get_debug_output_stream(Globals, ModuleName, Stream, !IO) :-
    get_output_stream(Globals, ModuleName, debug_output_suffix,
        get_output_stream_debug, set_output_stream_debug, Stream, !IO).

get_recompile_output_stream(Globals, ModuleName, Stream, !IO) :-
    get_output_stream(Globals, ModuleName, recompile_output_suffix,
        get_output_stream_recompile, set_output_stream_recompile, Stream, !IO).

:- pred get_output_stream(globals::in, module_name::in, option::in,
    pred(compiler_output_stream, io, io)::in(pred(out, di, uo) is det),
    pred(compiler_output_stream, io, io)::in(pred(in, di, uo) is det),
    io.text_output_stream::out, io::di, io::uo) is det.

get_output_stream(Globals, ModuleName, Option, Get, Set, Stream, !IO) :-
    Get(CompilerStream0, !IO),
    (
        CompilerStream0 = general_stream(Stream)
    ;
        CompilerStream0 = specific_stream(Stream)
    ;
        CompilerStream0 = no_stream,
        globals.lookup_string_option(Globals, Option, Suffix),
        ( if Suffix = "" then
            % The user does not want to redirect this kind output
            % to a module-specific file.
            io.stderr_stream(StdErr, !IO),
            Stream = StdErr,
            CompilerStream = general_stream(Stream)
        else
            FileName = sym_name_to_string(ModuleName) ++ "." ++ Suffix,
            io.open_output(FileName, OpenResult, !IO),
            (
                OpenResult = ok(Stream),
                % The user wants to redirect this kind output
                % to a module-specific file, and we can and do.
                CompilerStream = specific_stream(Stream)
            ;
                OpenResult = error(Error),
                % The user wants to redirect this kind output
                % to a module-specific file, but we can't open it.
                % Besides reporting the problem, acting as if
                % the request wasn't made is the best we can do.
                ErrorMsg = io.error_message(Error),
                io.stderr_stream(StdErr, !IO),
                io.format(StdErr, "can't open file `%s' for output: %s\n",
                    [s(FileName), s(ErrorMsg)], !IO),
                io.set_exit_status(1, !IO),
                Stream = StdErr,
                CompilerStream = general_stream(Stream)
            )
        ),
        Set(CompilerStream, !IO)
    ).

%---------------------%

close_any_specific_compiler_streams(!IO) :-
    get_output_stream_error(ErrorStream, !IO),
    get_output_stream_inference(InferenceStream, !IO),
    get_output_stream_debug(DebugStream, !IO),
    get_output_stream_recompile(RecompileStream, !IO),
    close_compiler_output_stream(ErrorStream, !IO),
    close_compiler_output_stream(InferenceStream, !IO),
    close_compiler_output_stream(DebugStream, !IO),
    close_compiler_output_stream(RecompileStream, !IO).

:- pred close_compiler_output_stream(compiler_output_stream::in,
    io::di, io::uo) is det.

close_compiler_output_stream(CompilerStream, !IO) :-
    (
        CompilerStream = general_stream(_Stream)
        % This stream does not need to be closed.
    ;
        CompilerStream = specific_stream(Stream),
        io.close_output(Stream, !IO)
    ;
        CompilerStream = no_stream
        % There is no stream to close.
    ).

%---------------------------------------------------------------------------%
:- end_module libs.globals.
%---------------------------------------------------------------------------%
