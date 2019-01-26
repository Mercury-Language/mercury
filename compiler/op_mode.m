%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module looks at the values of the options and decides what operation
% this invocation of the Mercury compiler should carry out.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module libs.op_mode.
:- interface.

:- import_module libs.options.
:- import_module list.

%---------------------------------------------------------------------------%

    % This type enumerates the compiler's modes of operation.
    %
    % The modes of operation are mutually exclusive, except for the loopholes
    % handled by decide_op_mode.
    %
    % The nest structure of this type mirrors the decisions
    % that the various top-level predicates of mercury_compile.m have to make,
    % with each type and subtype corresponding to one decision point.
    %
:- type op_mode
    --->    opm_top_make
    ;       opm_top_generate_source_file_mapping
    ;       opm_top_generate_standalone_interface(string)
    ;       opm_top_query(op_mode_query)
    ;       opm_top_args(op_mode_args).

%---------------------%

    % The modes of operation that ask the compiler
    % to output various properties.
:- type op_mode_query
    --->    opmq_output_cc                          % C compiler properties.
    ;       opmq_output_c_compiler_type
    ;       opmq_output_cflags
    ;       opmq_output_c_include_directory_flags
    ;       opmq_output_grade_defines

    ;       opmq_output_csharp_compiler             % C# compiler properties.
    ;       opmq_output_csharp_compiler_type

    ;       opmq_output_link_command                % Linker properties.
    ;       opmq_output_shared_lib_link_command
    ;       opmq_output_library_link_flags

    ;       opmq_output_class_dir                   % Java properties.

    ;       opmq_output_grade_string                % Grade information.
    ;       opmq_output_libgrades

    ;       opmq_output_target_arch.                % System information.

%---------------------%

    % The modes of operation that must be performed on each file or module
    % named in the argument list.
:- type op_mode_args
    --->    opma_generate_dependencies
    ;       opma_generate_dependency_file
    ;       opma_make_interface(op_mode_interface_file)
    ;       opma_convert_to_mercury
    ;       opma_augment(op_mode_augment).

:- type op_mode_interface_file
    --->    omif_int0
    ;       omif_int1_int2
    ;       omif_int3.

%---------------------%

    % The modes of operation that require the raw compilation units
    % read in from source files to be augmented, generating at least
    % an initial version of the HLDS.
:- type op_mode_augment
    --->    opmau_make_opt_int
    ;       opmau_make_trans_opt_int
    ;       opmau_make_analysis_registry
    ;       opmau_make_xml_documentation
    ;       opmau_typecheck_only
    ;       opmau_errorcheck_only
    ;       opmau_generate_code(op_mode_codegen).

%---------------------%

    % The modes of operation that require the Mercury code in the HLDS
    % to have code generated for it in a target language.
:- type op_mode_codegen
    --->    opmcg_target_code_only
    ;       opmcg_target_and_object_code_only
    ;       opmcg_target_object_and_executable.

%---------------------------------------------------------------------------%

    % Return the set of modes of operation implied by the command line options.
    %
:- pred decide_op_mode(option_table::in, op_mode::out, list(op_mode)::out)
    is det.

    % Return the option string that would cause the selection of the specified
    % op_mode, for use in error messages about the simultaneous specification
    % of options that call for incompatible op_modes.
    %
    % We can select the opm_top_make op_mode in response to two options:
    % --make and --rebuild. To let us return the right one, we also take
    % the value of the globals.
    % 
:- func op_mode_to_option_string(option_table, op_mode) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module getopt_io.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%

decide_op_mode(OptionTable, OpMode, OtherOpModes) :-
    some [!OpModeSet] (
        set.init(!:OpModeSet),
        list.foldl(gather_bool_op_mode(OptionTable), bool_op_modes,
            !OpModeSet),

        map.lookup(OptionTable, generate_standalone_interface,
            GenStandaloneOption),
        ( if GenStandaloneOption = maybe_string(MaybeBaseName) then
            (
                MaybeBaseName = no
            ;
                MaybeBaseName = yes(BaseName),
                set.insert(opm_top_generate_standalone_interface(BaseName),
                    !OpModeSet)
            )
        else
            unexpected($pred,
                "generate_standalone_interface is not maybe_string")
        ),

        % The option `--invoked-by-mmc-make' implicitly disables `--make'.
        %
        getopt_io.lookup_bool_option(OptionTable, invoked_by_mmc_make,
            InvokedByMMCMake),
        (
            InvokedByMMCMake = yes,
            set.delete(opm_top_make, !OpModeSet)
        ;
            InvokedByMMCMake = no
        ),
        set.to_sorted_list(!.OpModeSet, OpModes0),
        (
            OpModes0 = [],
            OpMode = opm_top_args(opma_augment(opmau_generate_code(
                opmcg_target_object_and_executable))),
            OtherOpModes = []
        ;
            OpModes0 = [OpMode],
            OtherOpModes = []
        ;
            OpModes0 = [_, _ | _],
            % The options select more than one operation to perform.
            % There are two possible reasons for this that are not errors.
            ( if
                % The first reason is that if --make is specified, we can set
                % the values of the other options that specify other op modes
                % to "yes", presumably to control what the code in the make
                % package does. (XXX We shouldn't, but that is another issue.)
                % In such cases, we just hand off control to the make package,
                % and let it take things from there.
                set.member(opm_top_make, !.OpModeSet)
            then
                OpMode = opm_top_make,
                OtherOpModes = []
            else if
                % The second reason is that Mercury.options file may specify
                % options that prevent code generation, but mmake will pass
                % these options to us even when we wouldn't have progressed
                % to code generation.
                set.delete(
                    opm_top_args(opma_augment(opmau_typecheck_only)),
                    !OpModeSet),
                set.delete(
                    opm_top_args(opma_augment(opmau_errorcheck_only)),
                    !OpModeSet),
                some [TogetherOpMode] (
                    set.member(TogetherOpMode, !.OpModeSet),
                    may_be_together_with_check_only(TogetherOpMode) = yes
                ),
                set.to_sorted_list(!.OpModeSet, FilteredOpModes),
                FilteredOpModes = [HeadFilteredOpMode | TailFilteredOpModes]
            then
                OpMode = HeadFilteredOpMode,
                % TailFilteredOpModes may still be nonempty, but if it is,
                % that represents a real error.
                OtherOpModes = TailFilteredOpModes
            else
                % Otherwise, we do have two or more contradictory options.
                % Return all the options we were given.
                OpModes0 = [OpMode | OtherOpModes]
            )
        )
    ).

:- func may_be_together_with_check_only(op_mode) = bool.

may_be_together_with_check_only(OpMode) = MayBeTogether :-
    (
        ( OpMode = opm_top_make
        ; OpMode = opm_top_generate_source_file_mapping
        ; OpMode = opm_top_generate_standalone_interface(_)
        ; OpMode = opm_top_query(_)
        ),
        MayBeTogether = yes
    ;
        OpMode = opm_top_args(OpModeArgs),
        (
            ( OpModeArgs = opma_generate_dependencies
            ; OpModeArgs = opma_generate_dependency_file
            ; OpModeArgs = opma_make_interface(_)
            ; OpModeArgs = opma_convert_to_mercury
            ),
            MayBeTogether = yes
        ;
            OpModeArgs = opma_augment(OpModeAugment),
            (
                ( OpModeAugment = opmau_make_opt_int
                ; OpModeAugment = opmau_make_trans_opt_int
                ; OpModeAugment = opmau_make_analysis_registry
                ; OpModeAugment = opmau_make_xml_documentation
                ),
                MayBeTogether = yes
            ;
                ( OpModeAugment = opmau_typecheck_only
                ; OpModeAugment = opmau_errorcheck_only
                ; OpModeAugment = opmau_generate_code(_)
                ),
                MayBeTogether = no
            )
        )
    ).

:- pred gather_bool_op_mode(option_table::in, pair(option, op_mode)::in,
    set(op_mode)::in, set(op_mode)::out) is det.

gather_bool_op_mode(OptionTable, Option - OpMode, !OpModeSet) :-
    map.lookup(OptionTable, Option, OptionValue),
    ( if OptionValue = bool(BoolValue) then
        (
            BoolValue = yes,
            set.insert(OpMode, !OpModeSet)
        ;
            BoolValue = no
        )
    else
        unexpected($pred, "not a boolean")
    ).

:- func bool_op_modes = assoc_list(option, op_mode).

bool_op_modes = [
    only_opmode_make -
        opm_top_make,
    % Although --rebuild on the command line implies --make, once the effect
    % of --make is suppressed by the compiler's internally-generated
    % --invoked-by-mmc-make, the value of --rebuild still affects the actions
    % of the other op_modes. This is why the compiler's internal name
    % of the --rebuild option is not only_opmode_rebuild.
    rebuild -
        opm_top_make,

    only_opmode_generate_source_file_mapping -
        opm_top_generate_source_file_mapping,

    only_opmode_output_cc -
        opm_top_query(opmq_output_cc),
    only_opmode_output_c_compiler_type -
        opm_top_query(opmq_output_c_compiler_type),
    only_opmode_output_cflags -
        opm_top_query(opmq_output_cflags),
    only_opmode_output_c_include_directory_flags -
        opm_top_query(opmq_output_c_include_directory_flags),
    only_opmode_output_grade_defines -
        opm_top_query(opmq_output_grade_defines),

    only_opmode_output_csharp_compiler -
        opm_top_query(opmq_output_csharp_compiler),
    only_opmode_output_csharp_compiler_type -
        opm_top_query(opmq_output_csharp_compiler_type),

    only_opmode_output_link_command -
        opm_top_query(opmq_output_link_command),
    only_opmode_output_shared_lib_link_command -
        opm_top_query(opmq_output_shared_lib_link_command),
    only_opmode_output_library_link_flags -
        opm_top_query(opmq_output_library_link_flags),

    only_opmode_output_class_dir -
        opm_top_query(opmq_output_class_dir),

    only_opmode_output_grade_string -
        opm_top_query(opmq_output_grade_string),
    only_opmode_output_libgrades -
        opm_top_query(opmq_output_libgrades),

    only_opmode_output_target_arch -
        opm_top_query(opmq_output_target_arch),

    only_opmode_generate_dependencies -
        opm_top_args(opma_generate_dependencies),
    only_opmode_generate_dependency_file -
        opm_top_args(opma_generate_dependency_file),
    only_opmode_make_private_interface -
        opm_top_args(opma_make_interface(omif_int0)),
    only_opmode_make_short_interface -
        opm_top_args(opma_make_interface(omif_int3)),
    only_opmode_make_interface -
        opm_top_args(opma_make_interface(omif_int1_int2)),
    only_opmode_convert_to_mercury -
        opm_top_args(opma_convert_to_mercury),

    only_opmode_make_optimization_interface -
        opm_top_args(opma_augment(opmau_make_opt_int)),
    only_opmode_make_transitive_opt_interface -
        opm_top_args(opma_augment(opmau_make_trans_opt_int)),
    only_opmode_make_analysis_registry -
        opm_top_args(opma_augment(opmau_make_analysis_registry)),
    only_opmode_make_xml_documentation -
        opm_top_args(opma_augment(opmau_make_xml_documentation)),
    only_opmode_typecheck_only -
        opm_top_args(opma_augment(opmau_typecheck_only)),
    only_opmode_errorcheck_only -
        opm_top_args(opma_augment(opmau_errorcheck_only)),
    only_opmode_target_code_only -
        opm_top_args(opma_augment(opmau_generate_code(
            opmcg_target_code_only))),
    only_opmode_compile_only -
        opm_top_args(opma_augment(opmau_generate_code(
            opmcg_target_and_object_code_only)))
].

%---------------------------------------------------------------------------%

op_mode_to_option_string(OptionTable, MOP) = Str :-
    (
        MOP = opm_top_make,
        map.lookup(OptionTable, rebuild, RebuildOption),
        ( if RebuildOption = bool(Rebuild) then
            (
                Rebuild = no,
                Str = "--make"
            ;
                Rebuild = yes,
                Str = "--rebuild"
            )
        else
            unexpected($pred, "rebuild option is not bool")
        )
    ;
        MOP = opm_top_generate_source_file_mapping,
        Str = "--generate-source-file-mapping"
    ;
        MOP = opm_top_generate_standalone_interface(_),
        Str = "--generate-standalone-interface"
    ;
        MOP = opm_top_query(MOPQ),
        (
            MOPQ = opmq_output_cc,
            Str = "--output-cc"
        ;
            MOPQ = opmq_output_c_compiler_type,
            Str = "--output-c-compiler-type"
        ;
            MOPQ = opmq_output_cflags,
            Str = "--output-cflags"
        ;
            MOPQ = opmq_output_c_include_directory_flags,
            Str = "--output-c-include-directory-flags"
        ;
            MOPQ = opmq_output_grade_defines,
            Str = "--output-grade-defines"
        ;
            MOPQ = opmq_output_csharp_compiler,
            Str = "--output-csharp-compiler"
        ;
            MOPQ = opmq_output_csharp_compiler_type,
            Str = "--output-csharp-compiler-type"
        ;
            MOPQ = opmq_output_link_command,
            Str = "--output-link-command"
        ;
            MOPQ = opmq_output_shared_lib_link_command,
            Str = "--output-shared-lib-link-command"
        ;
            MOPQ = opmq_output_library_link_flags,
            Str = "--output-library-link-flags"
        ;
            MOPQ = opmq_output_class_dir,
            Str = "--output-class-dir"
        ;
            MOPQ = opmq_output_grade_string,
            Str = "--output-grade-string"
        ;
            MOPQ = opmq_output_libgrades,
            Str = "--output-libgrades"
        ;
            MOPQ = opmq_output_target_arch,
            Str = "--output-target-arch"
        )
    ;
        MOP = opm_top_args(MOPA),
        (
            MOPA = opma_generate_dependencies,
            Str = "--generate-dependencies"
        ;
            MOPA = opma_generate_dependency_file,
            Str = "--generate-dependency_file"
        ;
            MOPA = opma_make_interface(InterfaceFile),
            (
                InterfaceFile = omif_int0,
                Str = "--make-private-interface"
            ;
                InterfaceFile = omif_int1_int2,
                Str = "--make-interface"
            ;
                InterfaceFile = omif_int3,
                Str = "--make-short-interface"
            )
        ;
            MOPA = opma_convert_to_mercury,
            Str = "--convert-to-mercury"
        ;
            MOPA = opma_augment(MOPAU),
            (
                MOPAU = opmau_make_opt_int,
                Str = "--make-opt-int"
            ;
                MOPAU = opmau_make_trans_opt_int,
                Str = "--make-trans-opt"
            ;
                MOPAU = opmau_make_analysis_registry,
                Str = "--make-analysis-registry"
            ;
                MOPAU = opmau_make_xml_documentation,
                Str = "--make-xml-doc"
            ;
                MOPAU = opmau_typecheck_only,
                Str = "--typecheck-only"
            ;
                MOPAU = opmau_errorcheck_only,
                Str = "--errorcheck-only"
            ;
                MOPAU = opmau_generate_code(MOPCG),
                (
                    MOPCG = opmcg_target_code_only,
                    Str = "--target-code-only"
                ;
                    MOPCG = opmcg_target_and_object_code_only,
                    Str = "--compile-only"
                ;
                    MOPCG = opmcg_target_object_and_executable,
                    % We only set this module of operation if none of the
                    % others is specified by options, so this op should
                    % NEVER conflict with any others.
                    unexpected($pred, "opmcg_target_object_and_executable")
                )
            )
        )
    ).

%---------------------------------------------------------------------------%
:- end_module libs.op_mode.
%---------------------------------------------------------------------------%
