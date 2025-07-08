%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: options_categories.m.
%
%---------------------------------------------------------------------------%

:- module libs.option_categories.
:- interface.

%---------------------------------------------------------------------------%

:- type option_category
    --->    oc_help
            % Options that call for the output of help text.
    ;       oc_cmdline
            % Options that manipulate the command line itself.
    ;       oc_opmode
            % Options that are used only to select an invocation's op_mode.
    ;       oc_grade_gen
    ;       oc_grade_target
    ;       oc_grade_llds
    ;       oc_grade_mlds
    ;       oc_grade_mdb
    ;       oc_grade_ssdb
    ;       oc_grade_mprof
    ;       oc_grade_mdprof
    ;       oc_grade_clprof
    ;       oc_grade_tsprof
    ;       oc_grade_etc
    ;       oc_grade_dev
            % Options that affect binary compatibility.
            % This category should subdivided into separate subcategories
            % for mdb, ssd, mprof, mdprof etc, but only after moving
            % the non-grade options out of the category altogether.
    ;       oc_infer
            % Options that tell the compiler what to infer.
    ;       oc_semantics
            % Options that specify which semantics variant to use.
    ;       oc_verbosity
            % Options that users can use to control how many progress updates
            % they want the compiler to give them.
    ;       oc_verb_dev
            % Developer-only kinds of oc_verbosity options.
    ;       oc_verb_dbg
            % Oc_verbosity options intended only for use by developers
            % to debug the compiler.
    ;       oc_diag_gen
            % Options for controlling diagnostics generally.
            % One class tells the compiler either under what circumstances
            % it should generate diagnostic output; another class specifies
            % how diagnostic output should be presented.
    ;       oc_diag_color
            % Options for controlling color in diagnostics.
    ;       oc_diag_int
            % Internal-use-only options for controlling diagnostics.
    ;       oc_warn_dodgy_mod
    ;       oc_warn_dodgy_pred
    ;       oc_warn_dodgy_prg
    ;       oc_warn_dodgy_goal
    ;       oc_warn_dodgy_inst
    ;       oc_warn_file
            % Warnings about code that is possibly incorrect.
    ;       oc_warn_perf
    ;       oc_warn_perf_c
            % Warnings about code that probably could be faster.
    ;       oc_warn_style_pred
    ;       oc_warn_style_goal
    ;       oc_warn_style_goal_c
    ;       oc_warn_style_order
    ;       oc_warn_style_ctg
    ;       oc_warn_style_ctg_c
            % Warnings about programming style.
    ;       oc_warn_ctrl
            % Options that *control* warnings.
            % XXX Split into subparts, one for each of oc_warn_*
            % that some now-oc_warn_ctrl option controls.
            % This should enable us to put the documentation of e.g.
            % inform_incomplete_switch_threshold, immediately after
            % the documentation of inform_incomplete_switch.
    ;       oc_warn_halt
            % Options that specify when warnings should treated as errors.
    ;       oc_inform
            % Requests for information.
    ;       oc_file_req
            % Options that request the compiler to generate files
            % containing information derived from the module being compiled.
    ;       oc_tracegoal
            % Options that control trace goals.
    ;       oc_mdb
            % Options that control how the compiler prepares for mdb debugging.
    ;       oc_mdb_dev
            % Developer-only options about mdb debugging.
            % XXX Some of these may be internal-use-only, not intended
            % even for developers.
    ;       oc_ssdb
    ;       oc_ssdb_dev
            % Options that control how the compiler prepares for ssdb.
    ;       oc_mdprof
            % Options that control deep profiling.
    ;       oc_opt_ctrl
            % Options that control optimization levels.
    ;       oc_opt_hh
    ;       oc_opt_hh_exp
            % HLDS->HLDS optimizations. The _exp suffix indicates that
            % the optimization is experimental.
    ;       oc_opt_hlm
            % HLDS->{LLDS,MLDS} optimizations.
    ;       oc_opt_mm
            % MLDS->MLDS optimizations.
    ;       oc_opt_hm
            % HLDS->MLDS optimizations.
    ;       oc_opt_hl
            % HLDS->LLDS optimizations.
    ;       oc_opt_ll
            % LLDS->LLDS optimizations.
    ;       oc_opt_lc
            % LLDS-> C optimizations. (There are no MLDS->C optimizations.)
    ;       oc_trans_opt
            % Options that control the operation of transitive intermodule
            % optimization.
    ;       oc_analysis
            % Options for user control of program analyses.
    ;       oc_output_mod
            % Options that ask the compiler to modify some aspect
            % of the generated target code.
    ;       oc_output_dev
            % Developer-only options that ask the compiler to modify
            % some aspect of the generated target code.
    ;       oc_make
            % Options controlling mmc --make.
    ;       oc_target_comp
    ;       oc_target_c
    ;       oc_target_java
    ;       oc_target_csharp
            % Options that control how the target language files we generate
            % are further compiled.
            % Subdivided for C, Java and C#.
    ;       oc_link_c_cs_j
    ;       oc_link_c_cs
    ;       oc_link_c
    ;       oc_link_java
    ;       oc_link_csharp
            % Options that control how executables, or their equivalents
            % for some target languages, are generated.
            % Subdivided for C, Java and C#, and for the combinations
            % that actually apply to some option.
    ;       oc_search
            % XXX Document me.
    ;       oc_buildsys
            % XXX Document me.
            % XXX We should separate search path options (the majority)
            % from everything else.
    ;       oc_env
            % Options that tell the compiler something about the environment,
            % or platform, on which it is operating.
    ;       oc_config
            % The results of autoconfiguration, or of executing
            % tools/configure_cross.
    ;       oc_mconfig
            % Options which are reserved for use by the Mercury.config file.
    ;       oc_dev_ctrl
            % Options developers can use to control what the compiler does.
    ;       oc_dev_debug
            % Options developers can use to debug compiler components.
    ;       oc_dev_dump
            % Options to control dumps of internal code representations.
    ;       oc_internal
            % Options for internal (and developer) use only.
    ;       oc_unused.
            % options that are now unused, and which are kept around
            % only for backward compatibility.
            % XXX Is this a good idea? If users want the effect of an
            % old option, then deleting the option and breaking their code
            % is less likely to mislead than keeping the option as a noop.

    % The job of this predicate is to provide an authoritative list
    % of all the option_categories for print_help.m. This is done by
    % calling solutions on the second mode.
    %
    % The first mode is unused; it is there only to ensure completeness.
    %
    % The int output is a never-used argument.
    %
:- pred option_categories(option_category, int).
:- mode option_categories(in, out) is det.
:- mode option_categories(out, out) is multi.

%---------------------------------------------------------------------------%

:- implementation.

option_categories(oc_help, 0).
option_categories(oc_cmdline, 0).
option_categories(oc_opmode, 0).
option_categories(oc_grade_gen, 0).
option_categories(oc_grade_target, 0).
option_categories(oc_grade_llds, 0).
option_categories(oc_grade_mlds, 0).
option_categories(oc_grade_mdb, 0).
option_categories(oc_grade_ssdb, 0).
option_categories(oc_grade_mprof, 0).
option_categories(oc_grade_mdprof, 0).
option_categories(oc_grade_clprof, 0).
option_categories(oc_grade_tsprof, 0).
option_categories(oc_grade_etc, 0).
option_categories(oc_grade_dev, 0).
option_categories(oc_infer, 0).
option_categories(oc_semantics, 0).
option_categories(oc_verbosity, 0).
option_categories(oc_verb_dev, 0).
option_categories(oc_verb_dbg, 0).
option_categories(oc_diag_gen, 0).
option_categories(oc_diag_color, 0).
option_categories(oc_diag_int, 0).
option_categories(oc_warn_dodgy_mod, 0).
option_categories(oc_warn_dodgy_pred, 0).
option_categories(oc_warn_dodgy_prg, 0).
option_categories(oc_warn_dodgy_goal, 0).
option_categories(oc_warn_dodgy_inst, 0).
option_categories(oc_warn_file, 0).
option_categories(oc_warn_perf, 0).
option_categories(oc_warn_perf_c, 0).
option_categories(oc_warn_style_pred, 0).
option_categories(oc_warn_style_order, 0).
option_categories(oc_warn_style_ctg, 0).
option_categories(oc_warn_style_ctg_c, 0).
option_categories(oc_warn_style_goal, 0).
option_categories(oc_warn_style_goal_c, 0).
option_categories(oc_warn_ctrl, 0).
option_categories(oc_warn_halt, 0).
option_categories(oc_inform, 0).
option_categories(oc_file_req, 0).
option_categories(oc_tracegoal, 0).
option_categories(oc_mdb, 0).
option_categories(oc_mdb_dev, 0).
option_categories(oc_ssdb, 0).
option_categories(oc_ssdb_dev, 0).
option_categories(oc_mdprof, 0).
option_categories(oc_opt_ctrl, 0).
option_categories(oc_opt_hh, 0).
option_categories(oc_opt_hh_exp, 0).
option_categories(oc_opt_hlm, 0).
option_categories(oc_opt_mm, 0).
option_categories(oc_opt_hm, 0).
option_categories(oc_opt_hl, 0).
option_categories(oc_opt_ll, 0).
option_categories(oc_opt_lc, 0).
option_categories(oc_trans_opt, 0).
option_categories(oc_analysis, 0).
option_categories(oc_output_mod, 0).
option_categories(oc_output_dev, 0).
option_categories(oc_make, 0).
option_categories(oc_target_comp, 0).
option_categories(oc_target_c, 0).
option_categories(oc_target_java, 0).
option_categories(oc_target_csharp, 0).
option_categories(oc_link_c_cs_j, 0).
option_categories(oc_link_c_cs, 0).
option_categories(oc_link_c, 0).
option_categories(oc_link_java, 0).
option_categories(oc_link_csharp, 0).
option_categories(oc_search, 0).
option_categories(oc_buildsys, 0).
option_categories(oc_env, 0).
option_categories(oc_config, 0).
option_categories(oc_mconfig, 0).
option_categories(oc_dev_ctrl, 0).
option_categories(oc_dev_debug, 0).
option_categories(oc_dev_dump, 0).
option_categories(oc_internal, 0).
option_categories(oc_unused, 0).

%---------------------------------------------------------------------------%
:- end_module libs.option_categories.
%---------------------------------------------------------------------------%
