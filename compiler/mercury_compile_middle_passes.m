%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mercury_compile_middle_passes.m.
%
% This module invokes the middle passes of the compiler as appropriate.
%
%---------------------------------------------------------------------------%

:- module top_level.mercury_compile_middle_passes.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.passes_aux.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module set.

:- pred middle_pass(io.text_output_stream::in, io.text_output_stream::in,
    module_info::in, module_info::out, dump_info::in, dump_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

:- pred middle_pass_for_opt_file(io.text_output_stream::in,
    module_info::in, module_info::out, set(pragma_info_unused_args)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

:- pred output_trans_opt_file(io.text_output_stream::in,
    io.text_output_stream::in, module_info::in,
    list(error_spec)::in, list(error_spec)::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

:- pred output_analysis_file(io.text_output_stream::in, module_info::in,
    list(error_spec)::in, list(error_spec)::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

:- pred maybe_unused_args(io.text_output_stream::in, bool::in, bool::in,
    set(pragma_info_unused_args)::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module backend_libs.
:- import_module backend_libs.type_ctor_info.
:- import_module bytecode_backend.
:- import_module bytecode_backend.bytecode.
:- import_module bytecode_backend.bytecode_gen.
:- import_module hlds.hlds_pred.
:- import_module hlds.mark_static_terms.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.deep_profiling.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.write_error_spec.
:- import_module top_level.mercury_compile_front_end.
:- import_module top_level.mercury_compile_llds_back_end.
:- import_module transform_hlds.
:- import_module transform_hlds.accumulator.
:- import_module transform_hlds.closure_analysis.
:- import_module transform_hlds.complexity.
:- import_module transform_hlds.ctgc.
:- import_module transform_hlds.ctgc.selector.
:- import_module transform_hlds.ctgc.structure_reuse.
:- import_module transform_hlds.ctgc.structure_reuse.analysis.
:- import_module transform_hlds.ctgc.structure_sharing.
:- import_module transform_hlds.ctgc.structure_sharing.analysis.
:- import_module transform_hlds.dead_proc_elim.
:- import_module transform_hlds.deforest.
:- import_module transform_hlds.delay_construct.
:- import_module transform_hlds.dep_par_conj.
:- import_module transform_hlds.direct_arg_in_out.
:- import_module transform_hlds.distance_granularity.
:- import_module transform_hlds.equiv_type_hlds.
:- import_module transform_hlds.exception_analysis.
:- import_module transform_hlds.float_regs.
:- import_module transform_hlds.granularity.
:- import_module transform_hlds.higher_order.
:- import_module transform_hlds.implicit_parallelism.
:- import_module transform_hlds.implicit_parallelism.introduce_parallelism.
:- import_module transform_hlds.inlining.
:- import_module transform_hlds.intermod_analysis.
:- import_module transform_hlds.lambda.
:- import_module transform_hlds.lco.
:- import_module transform_hlds.loop_inv.
:- import_module transform_hlds.mmc_analysis.
:- import_module transform_hlds.par_loop_control.
:- import_module transform_hlds.parallel_to_plain_conj.
:- import_module transform_hlds.rbmm.
:- import_module transform_hlds.rbmm.region_analysis.
:- import_module transform_hlds.size_prof.
:- import_module transform_hlds.ssdebug.
:- import_module transform_hlds.stm_expand.
:- import_module transform_hlds.table_gen.
:- import_module transform_hlds.tabling_analysis.
:- import_module transform_hlds.term_constr_main.
:- import_module transform_hlds.termination.
:- import_module transform_hlds.trailing_analysis.
:- import_module transform_hlds.tupling.
:- import_module transform_hlds.unneeded_code.
:- import_module transform_hlds.untupling.
:- import_module transform_hlds.unused_args.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module univ.

%---------------------------------------------------------------------------%

middle_pass(ProgressStream, ErrorStream, !HLDS, !DumpInfo, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_read_experimental_complexity_file(ErrorStream, !HLDS, !IO),

    tabling(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 105, "tabling", !DumpInfo, !IO),

    expand_lambdas(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 110, "lambda", !DumpInfo, !IO),

    maybe_do_direct_arg_in_out_transform(ProgressStream, Verbose, Stats,
        !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 111, "daio", !DumpInfo, !IO),

    expand_stm_goals(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 113, "stm", !DumpInfo, !IO),

    expand_equiv_types_hlds(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 115, "equiv_types",
        !DumpInfo, !IO),

    maybe_closure_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 117, "closure_analysis",
        !DumpInfo, !IO),

    % Uncomment the following code to check that unique mode analysis works
    % after simplification has been run. Currently it does not because common.m
    % does not preserve unique mode correctness (this test fails on about
    % five modules in the compiler and library). It is important that unique
    % mode analysis work most of the time after optimizations because
    % deforestation reruns it.

    % check_unique_modes(Verbose, Stats, !HLDS,
    %   FoundUniqError, !IO),
    % ( if FoundUniqError = yes then
    %   error("unique modes failed")
    % else
    %   true
    % ),

    % Exception analysis and termination analysis need to come before any
    % optimization passes that could benefit from the information that
    % they provide.

    maybe_exception_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 118, "exception_analysis",
        !DumpInfo, !IO),

    maybe_termination(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 120, "termination",
        !DumpInfo, !IO),

    maybe_termination2(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 121, "termination2",
        !DumpInfo, !IO),

    maybe_type_ctor_infos(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 125, "type_ctor_infos",
        !DumpInfo, !IO),

    % warn_dead_procs must come after type_ctor_infos, so that it handles
    % unification & comparison procedures correctly, but it must also come
    % before optimizations such as higher-order specialization and inlining,
    % which can make the original code for a procedure dead by
    % inlining/specializing all uses of it.
    maybe_warn_dead_procs(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 130, "warn_dead_procs",
        !DumpInfo, !IO),

    maybe_bytecodes(ProgressStream, !.HLDS, Verbose, Stats, !DumpInfo, !IO),

    maybe_untuple_arguments(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 133, "untupling", !DumpInfo, !IO),

    maybe_tuple_arguments(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 134, "tupling", !DumpInfo, !IO),

    maybe_higher_order(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 135, "higher_order",
        !DumpInfo, !IO),

    maybe_ssdb(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 137, "ssdb", !DumpInfo, !IO),

    maybe_introduce_accumulators(ProgressStream, Verbose, Stats,
        !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 140, "accum", !DumpInfo, !IO),

    maybe_do_inlining(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 145, "inlining", !DumpInfo, !IO),

    % Hoisting loop invariants first invokes pass 148, "mark_static".
    % "mark_static" is also run at stage 420.
    maybe_loop_inv(ProgressStream, Verbose, Stats, !HLDS, !DumpInfo, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 150, "loop_inv", !DumpInfo, !IO),

    maybe_deforestation(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 155, "deforestation",
        !DumpInfo, !IO),

    maybe_delay_construct(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 160, "delay_construct",
        !DumpInfo, !IO),

    maybe_structure_sharing_analysis(ProgressStream, Verbose, Stats,
        !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 162, "structure_sharing",
        !DumpInfo, !IO),

    maybe_structure_reuse_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 163, "structure_reuse",
        !DumpInfo, !IO),

    maybe_unused_args(ProgressStream, Verbose, Stats, _UnusedArgsInfos,
        !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 165, "unused_args",
        !DumpInfo, !IO),

    maybe_analyse_trail_usage(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 167, "trail_usage",
        !DumpInfo, !IO),

    maybe_unneeded_code(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 170, "unneeded_code",
        !DumpInfo, !IO),

    maybe_simplify(ProgressStream, maybe.no, bool.no,
        simplify_pass_pre_implicit_parallelism, Verbose, Stats,
        !HLDS, [], _SimplifySpecsPreImpPar, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 172,
        "pre_implicit_parallelism_simplify", !DumpInfo, !IO),

    maybe_implicit_parallelism(ProgressStream, ErrorStream, Verbose, Stats,
        !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 173, "implicit_parallelism",
        !DumpInfo, !IO),

    maybe_analyse_mm_tabling(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 185, "mm_tabling_analysis",
        !DumpInfo, !IO),

    maybe_control_granularity(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 200, "granularity",
        !DumpInfo, !IO),

    maybe_control_distance_granularity(ProgressStream, Verbose, Stats,
        !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 201, "distance_granularity",
        !DumpInfo, !IO),

    maybe_impl_dependent_par_conjs(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 205, "dependent_par_conj",
        !DumpInfo, !IO),

    maybe_par_loop_control(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 206, "par_loop_control",
        !DumpInfo, !IO),

    maybe_lco(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 210, "lco", !DumpInfo, !IO),

    maybe_float_reg_wrapper(ProgressStream, Verbose, Stats,
        !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 213, "float_reg_wrapper",
        !DumpInfo, !IO),

    % If we are compiling in a deep profiling grade then now rerun simplify.
    % The reason for doing this now is that we want to take advantage of any
    % opportunities the other optimizations have provided for constant
    % propagation and we cannot do that once the term-size profiling or deep
    % profiling transformations have been applied.
    maybe_simplify(ProgressStream, maybe.no, bool.no,
        simplify_pass_pre_prof_transforms, Verbose, Stats, !HLDS,
        [], _SimplifySpecsPreProf, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 215,
        "pre_prof_transforms_simplify", !DumpInfo, !IO),

    % The term size profiling transformation should be after all
    % transformations that construct terms of non-zero size. (Deep profiling
    % does not construct non-zero size terms.)
    maybe_term_size_prof(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 220, "term_size_prof",
        !DumpInfo, !IO),

    % The deep profiling transformation should be done late in the piece
    % since it munges the code a fair amount and introduces strange
    % disjunctions that might confuse other hlds->hlds transformations.
    maybe_deep_profiling(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 225, "deep_profiling",
        !DumpInfo, !IO),

    % Experimental complexity transformation should be done late in the
    % piece for the same reason as deep profiling. At the moment, they are
    % exclusive.
    maybe_experimental_complexity(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 230, "complexity",
        !DumpInfo, !IO),

    % XXX This may be moved to later.
    maybe_region_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 240, "region_analysis",
        !DumpInfo, !IO),

    maybe_eliminate_dead_procs(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 250, "dead_procs",
        !DumpInfo, !IO),

    maybe_dump_hlds(ProgressStream, !.HLDS, 299, "middle_pass",
        !DumpInfo, !IO).

%---------------------------------------------------------------------------%

middle_pass_for_opt_file(ProgressStream, !HLDS, UnusedArgsInfos,
        !Specs, !IO) :-
    % NOTE If you add any passes here, you will probably need to change the
    % code in mercury_compile_front_end.m that decides whether to call
    % this predicate.

    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    globals.lookup_bool_option(Globals, analyse_closures, ClosureAnalysis),
    globals.lookup_bool_option(Globals, structure_sharing_analysis,
        SharingAnalysis),
    ( if
        % Closure analysis assumes that lambda expressions have been converted
        % into separate predicates.
        % Structure sharing/reuse analysis results can be affected
        % by expand_lambdas.
        ( ClosureAnalysis = yes
        ; SharingAnalysis = yes
        )
    then
        expand_lambdas(ProgressStream, Verbose, Stats, !HLDS, !IO),
        expand_stm_goals(ProgressStream, Verbose, Stats, !HLDS, !IO)
    else
        true
    ),
    maybe_closure_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_exception_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_unused_args(ProgressStream, Verbose, Stats, UnusedArgsInfos,
        !HLDS, !Specs, !IO),
    maybe_termination(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_termination2(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_structure_sharing_analysis(ProgressStream, Verbose, Stats,
        !HLDS, !IO),
    maybe_structure_reuse_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_analyse_trail_usage(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_analyse_mm_tabling(ProgressStream, Verbose, Stats, !HLDS, !IO).

%---------------------------------------------------------------------------%

output_trans_opt_file(ProgressStream, ErrorStream, !.HLDS, !Specs,
        !DumpInfo, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    globals.lookup_bool_option(Globals, analyse_closures, ClosureAnalysis),
    globals.lookup_bool_option(Globals, structure_sharing_analysis,
        SharingAnalysis),

    % Closure analysis assumes that lambda expressions have
    % been converted into separate predicates.
    ( if
        ( ClosureAnalysis = yes
        ; SharingAnalysis = yes
        )
    then
        expand_lambdas(ProgressStream, Verbose, Stats, !HLDS, !IO)
    else
        true
    ),
    maybe_dump_hlds(ProgressStream, !.HLDS, 110, "lambda", !DumpInfo, !IO),
    maybe_closure_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 117, "closure_analysis",
        !DumpInfo, !IO),
    maybe_exception_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 118, "exception_analysis",
        !DumpInfo, !IO),
    maybe_termination(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 120, "termination",
        !DumpInfo, !IO),
    maybe_termination2(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 121, "termination_2",
        !DumpInfo, !IO),
    (
        SharingAnalysis = yes,
        % These affect the results we write out for structure sharing/reuse
        % analysis.
        maybe_higher_order(ProgressStream, Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(ProgressStream, !.HLDS, 135, "higher_order",
            !DumpInfo, !IO),
        maybe_do_inlining(ProgressStream, Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(ProgressStream, !.HLDS, 145, "inlining",
            !DumpInfo, !IO),
        maybe_loop_inv(ProgressStream, Verbose, Stats, !HLDS, !DumpInfo, !IO),
        maybe_dump_hlds(ProgressStream, !.HLDS, 150, "loop_inv",
            !DumpInfo, !IO),
        maybe_deforestation(ProgressStream, Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(ProgressStream, !.HLDS, 155, "deforestation",
            !DumpInfo, !IO)
    ;
        SharingAnalysis = no
    ),
    maybe_structure_sharing_analysis(ProgressStream, Verbose, Stats,
        !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 162, "structure_sharing",
        !DumpInfo, !IO),
    maybe_structure_reuse_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 163, "structure_reuse",
        !DumpInfo, !IO),
    maybe_analyse_trail_usage(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 167, "trail_usage",
        !DumpInfo, !IO),
    maybe_analyse_mm_tabling(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 185, "mm_tabling_analysis",
        !DumpInfo, !IO),

    module_info_get_name(!.HLDS, ModuleName),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".trans_opt")), ModuleName, OptFileName, !IO),
    TmpOptFileName = OptFileName ++ ".tmp",
    io.open_output(TmpOptFileName, TmpOptResult, !IO),
    (
        TmpOptResult = error(Error),
        io.progname_base("mmc", ProgName, !IO),
        io.error_message(Error, ErrorMsg),
        io.format(ErrorStream, "%s: cannot open `%s' for output: %s\n",
            [s(ProgName), s(TmpOptFileName), s(ErrorMsg)], !IO),
        io.set_exit_status(1, !IO)
    ;
        TmpOptResult = ok(TmpOptStream),
        write_trans_opt_file(TmpOptStream, !.HLDS, ParseTreeTransOpt, !IO),
        io.close_output(TmpOptStream, !IO),

        update_interface_report_any_error(Globals, ModuleName, OptFileName,
            _UpdateSucceeded, !IO),
        touch_interface_datestamp(Globals, ProgressStream, ErrorStream,
            ModuleName, other_ext(".trans_opt_date"), _TouchSucceeded, !IO),

        globals.lookup_bool_option(Globals, experiment5, Experiment5),
        (
            Experiment5 = no
        ;
            Experiment5 = yes,
            io.open_output(OptFileName ++ "x", OptXResult, !IO),
            (
                OptXResult = error(_)
            ;
                OptXResult = ok(OptXStream),
                Info = init_merc_out_info(Globals, unqualified_item_names,
                    output_mercury),
                mercury_output_parse_tree_trans_opt(Info, OptXStream,
                    ParseTreeTransOpt, !IO),
                io.close_output(OptXStream, !IO)
            )
        )
    ).

%---------------------------------------------------------------------------%

output_analysis_file(ProgressStream, !.HLDS, !Specs, !DumpInfo, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    globals.lookup_bool_option(Globals, analyse_closures, ClosureAnalysis),
    globals.lookup_bool_option(Globals, structure_sharing_analysis,
        SharingAnalysis),

    % Closure analysis assumes that lambda expressions have
    % been converted into separate predicates.
    ( if
        ( ClosureAnalysis = yes
        ; SharingAnalysis = yes
        )
    then
        expand_lambdas(ProgressStream, Verbose, Stats, !HLDS, !IO)
    else
        true
    ),
    maybe_dump_hlds(ProgressStream, !.HLDS, 110, "lambda", !DumpInfo, !IO),
    maybe_closure_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 117, "closure_analysis",
        !DumpInfo, !IO),
    maybe_exception_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 118, "exception_analysis",
        !DumpInfo, !IO),
    maybe_termination(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 120, "termination",
        !DumpInfo, !IO),
    maybe_termination2(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 121, "termination_2",
        !DumpInfo, !IO),
    (
        SharingAnalysis = yes,
        % These affect the results we write out for structure sharing/reuse
        % analysis.
        maybe_higher_order(ProgressStream, Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(ProgressStream, !.HLDS, 135, "higher_order",
            !DumpInfo, !IO),
        maybe_do_inlining(ProgressStream, Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(ProgressStream, !.HLDS, 145, "inlining",
            !DumpInfo, !IO),
        maybe_loop_inv(ProgressStream, Verbose, Stats, !HLDS, !DumpInfo, !IO),
        maybe_dump_hlds(ProgressStream, !.HLDS, 150, "loop_inv",
            !DumpInfo, !IO),
        maybe_deforestation(ProgressStream, Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(ProgressStream, !.HLDS, 155, "deforestation",
            !DumpInfo, !IO)
    ;
        SharingAnalysis = no
    ),
    maybe_structure_sharing_analysis(ProgressStream, Verbose, Stats,
        !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 162, "structure_sharing",
        !DumpInfo, !IO),
    maybe_structure_reuse_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 163, "structure_reuse",
        !DumpInfo, !IO),
    maybe_unused_args(ProgressStream, Verbose, Stats, _UnusedArgsInfos, !HLDS,
        !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 165, "unused_args",
        !DumpInfo, !IO),
    maybe_analyse_trail_usage(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 167, "trail_usage",
        !DumpInfo, !IO),
    maybe_analyse_mm_tabling(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 185, "mm_tabling_analysis",
        !DumpInfo, !IO),

    module_info_get_analysis_info(!.HLDS, AnalysisInfo),
    module_info_get_all_deps(!.HLDS, ImportedModules),
    analysis.write_analysis_files(mmc, !.HLDS, ImportedModules,
        AnalysisInfo, _AnalysisInfo, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The various component passes of the middle pass.
% Please keep these predicates in the order of their (first) invocation.
%

%---------------------------------------------------------------------------%

:- pred maybe_read_experimental_complexity_file(io.text_output_stream::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_read_experimental_complexity_file(ErrorStream, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_string_option(Globals, experimental_complexity, FileName),
    ( if FileName = "" then
%       While we could include the following sanity check, it is overly
%       strong. For example, a bootcheck in a term size profiling grade
%       would have to supply an --experimental-complexity option for
%       both the stage3 compiler and the test cases. Since the runtime
%       checks that all procedures mentioned in the files actually
%       exist and are transformed by the compiler, this would require
%       a different complexity experiment file for each test case,
%       which is impractical.
%       (
%           RecordTermSizes = yes,
%           report_error("term size profiling grades require " ++
%               "the --experimental-complexity option", !IO)
%       ;
%           RecordTermSizes = no
%       )
            true
    else
        complexity.read_spec_file(FileName, MaybeNumProcMap, !IO),
        (
            MaybeNumProcMap = ok(NumProcs - ProcMap),
            module_info_set_maybe_complexity_proc_map(yes(NumProcs - ProcMap),
                !HLDS)
        ;
            MaybeNumProcMap = error(Msg),
            report_error(ErrorStream, Msg, !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred tabling(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

tabling(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO) :-
    maybe_write_string(ProgressStream, Verbose,
        "% Transforming tabled predicates...", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    table_gen_process_module(!HLDS, !Specs),
    maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

%---------------------------------------------------------------------------%

:- pred expand_lambdas(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

expand_lambdas(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(ProgressStream, Verbose,
        "% Transforming lambda expressions...", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    expand_lambdas_in_module(!HLDS),
    maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

%---------------------------------------------------------------------------%

:- pred maybe_do_direct_arg_in_out_transform(io.text_output_stream::in,
    bool::in, bool::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_do_direct_arg_in_out_transform(ProgressStream, Verbose, Stats,
        !HLDS, !Specs, !IO) :-
    module_info_get_direct_arg_proc_map(!.HLDS, DirectArgProcMap),
    ( if map.is_empty(DirectArgProcMap) then
        true
    else
        maybe_write_string(ProgressStream, Verbose,
            "% Transforming direct arg in out procedures...\n", !IO),
        do_direct_arg_in_out_transform_in_module(ProgressStream,
            DirectArgProcMap, !HLDS, DirectArgSpecs),
        !:Specs = DirectArgSpecs ++ !.Specs,
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred expand_stm_goals(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

expand_stm_goals(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(ProgressStream, Verbose,
        "% Transforming stm expressions...", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    stm_process_module(!HLDS),
    maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

%---------------------------------------------------------------------------%

:- pred expand_equiv_types_hlds(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

expand_equiv_types_hlds(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(ProgressStream, Verbose,
        "% Fully expanding equivalence types...", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    replace_equiv_types_in_hlds(!HLDS),
    maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

%---------------------------------------------------------------------------%

:- pred maybe_closure_analysis(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_closure_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, analyse_closures, ClosureAnalysis),
    (
        ClosureAnalysis = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Analysing closures...\n", !IO),
        closure_analyse_module(ProgressStream, !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        ClosureAnalysis = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_exception_analysis(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_exception_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, analyse_exceptions, ExceptionAnalysis),
    (
        ExceptionAnalysis = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Analysing exceptions...\n", !IO),
        analyse_exceptions_in_module(!HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        ExceptionAnalysis = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_termination(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_termination(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, termination, Termination),
    (
        Termination = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Detecting termination...\n", !IO),
        analyse_termination_in_module(!HLDS, TermSpecs),
        !:Specs = TermSpecs ++ !.Specs,
        maybe_write_string(ProgressStream, Verbose,
            "% Termination checking done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        Termination = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_termination2(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_termination2(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, termination2, Termination2),
    (
        Termination2 = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Detecting termination 2...\n", !IO),
        term2_analyse_module(!HLDS, TermSpecs),
        !:Specs = TermSpecs ++ !.Specs,
        maybe_write_string(ProgressStream, Verbose,
            "% Termination 2 checking done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        Termination2 = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_type_ctor_infos(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_type_ctor_infos(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, type_ctor_info, TypeCtorInfo),
    (
        TypeCtorInfo = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Generating type_ctor_info structures...", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        type_ctor_info.generate_hlds(!HLDS),
        maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        TypeCtorInfo = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_warn_dead_procs(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_warn_dead_procs(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, warn_dead_procs, WarnDeadProcs),
    globals.lookup_bool_option(Globals, warn_dead_preds, WarnDeadPreds),
    ( if
        ( WarnDeadProcs = yes
        ; WarnDeadPreds = yes
        )
    then
        (
            WarnDeadProcs = yes,
            maybe_write_string(ProgressStream, Verbose,
                "% Warning about dead procedures...\n", !IO)
        ;
            WarnDeadProcs = no,
            maybe_write_string(ProgressStream, Verbose,
                "% Warning about dead predicates...\n", !IO)
        ),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        dead_proc_warn(!.HLDS, DeadSpecs),
        !:Specs = DeadSpecs ++ !.Specs,
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_bytecodes(io.text_output_stream::in, module_info::in,
    bool::in, bool::in, dump_info::in, dump_info::out, io::di, io::uo) is det.

maybe_bytecodes(ProgressStream, HLDS0, Verbose, Stats, !DumpInfo, !IO) :-
    module_info_get_globals(HLDS0, Globals),
    globals.lookup_bool_option(Globals, generate_bytecode, GenBytecode),
    (
        GenBytecode = yes,
        map_args_to_regs(ProgressStream, Verbose, Stats, HLDS0, HLDS1, !IO),
        maybe_dump_hlds(ProgressStream, HLDS1, 505,
            "bytecode_args_to_regs", !DumpInfo, !IO),
        maybe_write_string(ProgressStream, Verbose,
            "% Generating bytecodes...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        bytecode_gen.gen_module(HLDS1, Bytecode, !IO),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO),
        module_info_get_name(HLDS1, ModuleName),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".bytedebug")), ModuleName,
            BytedebugFile, !IO),
        maybe_write_string(ProgressStream, Verbose,
            "% Writing bytecodes to `", !IO),
        maybe_write_string(ProgressStream, Verbose, BytedebugFile, !IO),
        maybe_write_string(ProgressStream, Verbose, "'...", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        debug_bytecode_file(BytedebugFile, Bytecode, !IO),
        maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".mbc")), ModuleName, BytecodeFile, !IO),
        maybe_write_string(ProgressStream, Verbose,
            "% Writing bytecodes to `", !IO),
        maybe_write_string(ProgressStream, Verbose, BytecodeFile, !IO),
        maybe_write_string(ProgressStream, Verbose, "'...", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        output_bytecode_file(BytecodeFile, Bytecode, !IO),
        maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        GenBytecode = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_untuple_arguments(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_untuple_arguments(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    Untuple = OptTuple ^ ot_untuple,
    (
        Untuple = untuple,
        maybe_write_string(ProgressStream, Verbose, "% Untupling...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        untuple_arguments(!HLDS, !IO),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_simplify(ProgressStream, maybe.no, bool.no,
            simplify_pass_post_untuple, Verbose, Stats, !HLDS,
            [], _SimplifySpecs, !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        Untuple = do_not_untuple
    ).

%---------------------------------------------------------------------------%

:- pred maybe_tuple_arguments(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_tuple_arguments(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    Tuple = OptTuple ^ ot_tuple,
    (
        Tuple = tuple,
        maybe_write_string(ProgressStream, Verbose, "% Tupling...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        tuple_arguments(!HLDS, !IO),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        Tuple = do_not_tuple
    ).

%---------------------------------------------------------------------------%

:- pred maybe_higher_order(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_higher_order(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    HigherOrder = OptTuple ^ ot_opt_higher_order,
    % --type-specialization implies --user-guided-type-specialization.
    UserTypeSpec = OptTuple ^ ot_spec_types_user_guided,

    % Always produce the specialized versions for which
    % `:- pragma type_spec' declarations exist, because
    % importing modules might call them.
    module_info_get_type_spec_info(!.HLDS, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(TypeSpecPreds, _, _, _),
    ( if
        ( HigherOrder = opt_higher_order
        ; UserTypeSpec = spec_types_user_guided
        ; set.is_non_empty(TypeSpecPreds)
        )
    then
        maybe_write_string(ProgressStream, Verbose,
            "% Specializing higher-order and polymorphic predicates...\n",
            !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),

        specialize_higher_order(!HLDS, !IO),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_ssdb(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_ssdb(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, force_disable_ssdebug,
        ForceDisableSSDB),
    (
        ForceDisableSSDB = no,
        globals.get_ssdb_trace_level(Globals, SSTraceLevel),
        (
            SSTraceLevel = ssdb_none
        ;
            ( SSTraceLevel = ssdb_shallow
            ; SSTraceLevel = ssdb_deep
            ),
            maybe_write_string(ProgressStream, Verbose,
                "% Apply source to source debugging transformation ...\n",
                !IO),
            ssdebug_transform_module(SSTraceLevel, !HLDS, !IO),
            maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
            maybe_report_stats(ProgressStream, Stats, !IO)
        )
    ;
        ForceDisableSSDB = yes
    ).

%---------------------------------------------------------------------------%

:- pred maybe_implicit_parallelism(io.text_output_stream::in,
    io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_implicit_parallelism(ProgressStream, ErrorStream, Verbose, Stats,
        !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, implicit_parallelism,
        ImplicitParallelism),
    (
        ImplicitParallelism = yes,
        io_get_maybe_source_file_map(MaybeSourceFileMap, !IO),
        (
            MaybeSourceFileMap = yes(SourceFileMap)
        ;
            MaybeSourceFileMap = no,
            unexpected($pred, "could not retrieve the source file map")
        ),
        maybe_write_string(ProgressStream, Verbose,
            "% Applying implicit parallelism...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        apply_implicit_parallelism_transformation(SourceFileMap, Specs, !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        % XXX This is a bit late for printing errors.
        write_error_specs(ErrorStream, Globals, Specs, !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        % The user hasn't asked for implicit parallelism.
        ImplicitParallelism = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_introduce_accumulators(io.text_output_stream::in,
    bool::in, bool::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_introduce_accumulators(ProgressStream, Verbose, Stats,
        !HLDS, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    IntroduceAccumulators = OptTuple ^ ot_introduce_accumulators,
    (
        IntroduceAccumulators = introduce_accumulators,
        maybe_write_string(ProgressStream, Verbose,
            "% Attempting to introduce accumulators...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        type_to_univ([] : list(error_spec), Cookie0),
        Task0 = update_module_pred_cookie(accu_transform_proc, Cookie0),
        process_valid_nonimported_procs_update(Task0, Task, !HLDS),
        ( if
            Task = update_module_pred_cookie(_, Cookie),
            univ_to_type(Cookie, AccSpecs)
        then
            !:Specs = AccSpecs ++ !.Specs
        else
            unexpected($pred, "bad task")
        ),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        IntroduceAccumulators = do_not_introduce_accumulators
    ).

%---------------------------------------------------------------------------%

:- pred maybe_do_inlining(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_do_inlining(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    Allow = OptTuple ^ ot_allow_inlining,
    Simple = OptTuple ^ ot_inline_simple,
    SingleUse = OptTuple ^ ot_inline_single_use,
    Threshold = OptTuple ^ ot_inline_compound_threshold,
    LinearRec = OptTuple ^ ot_inline_linear_tail_rec_sccs,
    ( if
        Allow = allow_inlining,
        ( Simple = inline_simple
        ; SingleUse = inline_single_use
        ; Threshold > 0
        ; LinearRec = inline_linear_tail_rec_sccs
        )
    then
        maybe_write_string(ProgressStream, Verbose, "% Inlining...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        inline_in_module(ProgressStream, !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_loop_inv(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, dump_info::in, dump_info::out,
    io::di, io::uo) is det.

maybe_loop_inv(ProgressStream, Verbose, Stats, !HLDS, !DumpInfo, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    LoopInv = OptTuple ^ ot_opt_loop_invariants,
    (
        LoopInv = opt_loop_invariants,
        % We run the mark_static pass because we need the construct_how flag
        % to be valid.
        maybe_mark_static_terms(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(ProgressStream, !.HLDS, 148, "mark_static",
            !DumpInfo, !IO),

        maybe_write_string(ProgressStream, Verbose,
            "% Hoisting loop invariants...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        process_valid_nonimported_procs(
            update_module_pred(hoist_loop_invariants), !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        LoopInv = do_not_opt_loop_invariants
    ).

%---------------------------------------------------------------------------%

:- pred maybe_deforestation(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_deforestation(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    Deforest = OptTuple ^ ot_deforest,

    % --constraint-propagation implies --local-constraint-propagation.
    PropLocalConstraints = OptTuple ^ ot_prop_local_constraints,
    (
        Deforest = do_not_deforest,
        PropLocalConstraints = do_not_prop_local_constraints
    ;
        (
            Deforest = deforest,
            PropLocalConstraints = prop_local_constraints,
            Msg = "% Deforestation and constraint propagation...\n"
        ;
            Deforest = deforest,
            PropLocalConstraints = do_not_prop_local_constraints,
            Msg = "% Deforestation...\n"
        ;
            Deforest = do_not_deforest,
            PropLocalConstraints = prop_local_constraints,
            Msg = "% Constraint propagation...\n"
        ),
        maybe_write_string(ProgressStream, Verbose, Msg, !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        deforest_module(ProgressStream, !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred maybe_delay_construct(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_delay_construct(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    DelayConstruct = OptTuple ^ ot_delay_constructs,
    (
        DelayConstruct = delay_constructs,
        maybe_write_string(ProgressStream, Verbose,
            "% Delaying construction unifications ...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        process_valid_nonimported_procs(
            update_proc_ids(delay_construct_proc(ProgressStream)), !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        DelayConstruct = do_not_delay_constructs
    ).

%---------------------------------------------------------------------------%

:- pred maybe_structure_sharing_analysis(io.text_output_stream::in,
    bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

maybe_structure_sharing_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, structure_sharing_analysis, Sharing),
    (
        Sharing = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Structure sharing analysis...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        perform_structure_sharing_analysis(!HLDS),
        transform_hlds.ctgc.selector.reset_tables(!IO),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        Sharing = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_structure_reuse_analysis(io.text_output_stream::in,
    bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

maybe_structure_reuse_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, structure_reuse_analysis,
        ReuseAnalysis),
    (
        ReuseAnalysis = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Structure reuse analysis...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        perform_structure_reuse_analysis(!HLDS),
        transform_hlds.ctgc.selector.reset_tables(!IO),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        ReuseAnalysis = no
    ).

%---------------------------------------------------------------------------%

maybe_unused_args(ProgressStream, Verbose, Stats, UnusedArgsInfos,
        !HLDS, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    OptUnusedArgs = OptTuple ^ ot_opt_unused_args,
    OptUnusedArgsIntermod = OptTuple ^ ot_opt_unused_args_intermod,
    globals.lookup_bool_option(Globals, warn_unused_args, WarnUnusedArgs),
    ( if
        ( OptUnusedArgs = opt_unused_args
        ; OptUnusedArgsIntermod = opt_unused_args_intermod
        ; WarnUnusedArgs = yes
        )
    then
        maybe_write_string(ProgressStream, Verbose,
            "% Finding unused arguments ...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        unused_args_process_module(!HLDS, UnusedSpecs, UnusedArgsInfos),
        !:Specs = UnusedSpecs ++ !.Specs,
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    else
        set.init(UnusedArgsInfos)
    ).

%---------------------------------------------------------------------------%

:- pred maybe_analyse_trail_usage(io.text_output_stream::in,
    bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

maybe_analyse_trail_usage(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, analyse_trail_usage, AnalyseTrail),
    (
        AnalyseTrail = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Analysing trail usage...\n", !IO),
        analyse_trail_usage_in_module(!HLDS),
        maybe_write_string(ProgressStream, Verbose,
            "% Trail usage analysis done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        AnalyseTrail = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_unneeded_code(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_unneeded_code(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    UnneededCode = OptTuple ^ ot_opt_unneeded_code,
    (
        UnneededCode = opt_unneeded_code,
        maybe_write_string(ProgressStream, Verbose,
            "% Removing unneeded code from procedure bodies...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        process_valid_nonimported_procs(
            update_module(unneeded_process_proc_msg(ProgressStream)), !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        UnneededCode = do_not_opt_unneeded_code
    ).

%---------------------------------------------------------------------------%

:- pred maybe_lco(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_lco(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    LCMC = OptTuple ^ ot_opt_lcmc,
    (
        LCMC = opt_lcmc,
        maybe_write_string(ProgressStream, Verbose,
            "% Looking for LCO modulo constructor application ...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        lco_modulo_constructors(!HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        LCMC = do_not_opt_lcmc
    ).

%---------------------------------------------------------------------------%

:- pred maybe_analyse_mm_tabling(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_analyse_mm_tabling(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, analyse_mm_tabling, TablingAnalysis),
    (
        TablingAnalysis = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Analysing minimal model tabling...\n", !IO),
        analyse_mm_tabling_in_module(!HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        TablingAnalysis = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_control_granularity(io.text_output_stream::in,
    bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

maybe_control_granularity(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, parallel, Parallel),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.lookup_bool_option(Globals, control_granularity, Control),
    module_info_get_has_parallel_conj(!.HLDS, HasParallelConj),
    ( if
        % If either of these is false, there is no parallelism to control.
        Parallel = yes,
        HasParallelConj = has_parallel_conj,

        % Our mechanism for granularity control only works for the low level
        % backend.
        HighLevelCode = no,

        % If this is false, then the user hasn't asked for granularity control.
        Control = yes
    then
        globals.get_target(Globals, Target),
        (
            Target = target_c,
            maybe_write_string(ProgressStream, Verbose,
                "% Granularity control transformation...\n", !IO),
            maybe_flush_output(ProgressStream, Verbose, !IO),
            control_granularity(!HLDS),
            maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
            maybe_report_stats(ProgressStream, Stats, !IO)
        ;
            ( Target = target_csharp
            ; Target = target_java
            )
            % Leave the HLDS alone. We cannot implement parallelism,
            % so there is not point in controlling its granularity.
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_control_distance_granularity(io.text_output_stream::in,
    bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

maybe_control_distance_granularity(ProgressStream, Verbose, Stats,
        !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, parallel, Parallel),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.lookup_int_option(Globals, distance_granularity, Distance),
    module_info_get_has_parallel_conj(!.HLDS, HasParallelConj),
    ( if
        % If either of these is false, there is no parallelism to control.
        Parallel = yes,
        HasParallelConj = has_parallel_conj,

        % Our mechanism for granularity control only works for the low level
        % backend.
        HighLevelCode = no,

        % Distance must be greater than 0 to apply the distance granularity
        % transformation.
        Distance > 0
    then
        globals.get_target(Globals, Target),
        (
            Target = target_c,
            maybe_write_string(ProgressStream, Verbose,
                "% Distance granularity transformation...\n", !IO),
            maybe_flush_output(ProgressStream, Verbose, !IO),
            control_distance_granularity(!HLDS, Distance),
            maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
            maybe_report_stats(ProgressStream, Stats, !IO)
        ;
            ( Target = target_csharp
            ; Target = target_java
            )
            % Leave the HLDS alone. We cannot implement parallelism,
            % so there is not point in controlling its granularity.
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_impl_dependent_par_conjs(io.text_output_stream::in,
    bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

maybe_impl_dependent_par_conjs(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_has_parallel_conj(!.HLDS, HasParallelConj),
    (
        HasParallelConj = has_parallel_conj,
        module_info_get_globals(!.HLDS, Globals),
        current_grade_supports_par_conj(Globals, SupportsParConj),
        (
            SupportsParConj = no,
            process_valid_nonimported_procs(
                update_proc_ids(parallel_to_plain_conjs), !HLDS)
        ;
            SupportsParConj = yes,
            maybe_write_string(ProgressStream, Verbose,
                "% Dependent parallel conjunction transformation...\n", !IO),
            maybe_flush_output(ProgressStream, Verbose, !IO),
            impl_dep_par_conjs_in_module(!HLDS),
            dead_proc_elim(do_not_elim_opt_imported, _, _, !HLDS),
            maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
            maybe_report_stats(ProgressStream, Stats, !IO)
        )
    ;
        HasParallelConj = has_no_parallel_conj
    ).

%---------------------------------------------------------------------------%

:- pred maybe_par_loop_control(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_par_loop_control(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, par_loop_control, LoopControl),
    (
        LoopControl = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Applying parallel loop control transformation...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        maybe_par_loop_control_module(!HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        LoopControl = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_float_reg_wrapper(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_float_reg_wrapper(ProgressStream, Verbose, Stats, !HLDS, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, use_float_registers, UseFloatRegs),
    (
        UseFloatRegs = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Inserting float register wrappers...", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        float_regs.insert_reg_wrappers(!HLDS, RegSpecs),
        !:Specs = RegSpecs ++ !.Specs,
        maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        UseFloatRegs = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_term_size_prof(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_term_size_prof(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, record_term_sizes_as_words, AsWords),
    globals.lookup_bool_option(Globals, record_term_sizes_as_cells, AsCells),
    (
        AsWords = yes,
        AsCells = yes,
        unexpected($pred, "as_words and as_cells")
    ;
        AsWords = yes,
        AsCells = no,
        MaybeTransform = yes(term_words)
    ;
        AsWords = no,
        AsCells = yes,
        MaybeTransform = yes(term_cells)
    ;
        AsWords = no,
        AsCells = no,
        MaybeTransform = no
    ),
    (
        MaybeTransform = yes(Transform),
        maybe_write_string(ProgressStream, Verbose,
            "% Applying term size profiling transformation...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        process_valid_nonimported_procs(
            update_module(size_prof_process_proc_msg(ProgressStream,
                Transform)),
            !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        MaybeTransform = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_deep_profiling(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_deep_profiling(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, profile_deep, ProfileDeep),
    (
        ProfileDeep = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Applying deep profiling transformation...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        apply_deep_profiling_transform(!HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        ProfileDeep = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_experimental_complexity(io.text_output_stream::in,
    bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

maybe_experimental_complexity(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_maybe_complexity_proc_map(!.HLDS, MaybeNumProcMap),
    (
        MaybeNumProcMap = no
    ;
        MaybeNumProcMap = yes(NumProcs - ProcMap),
        maybe_write_string(ProgressStream, Verbose,
            "% Applying complexity experiment transformation...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        process_valid_nonimported_procs(
            update_module(complexity_process_proc_msg(ProgressStream,
                NumProcs, ProcMap)),
            !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred maybe_region_analysis(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_region_analysis(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    Analysis = OptTuple ^ ot_analyse_regions,
    (
        Analysis = analyse_regions,
        maybe_write_string(ProgressStream, Verbose,
            "% Analysing regions ...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        do_region_analysis(!HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        Analysis = do_not_analyse_regions
    ).

%---------------------------------------------------------------------------%

:- pred maybe_eliminate_dead_procs(io.text_output_stream::in,
    bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

maybe_eliminate_dead_procs(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    OptDeadProcs = OptTuple ^ ot_opt_dead_procs,
    (
        OptDeadProcs = opt_dead_procs,
        maybe_write_string(ProgressStream, Verbose,
            "% Eliminating dead procedures...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        BeforeHLDS = !.HLDS,
        dead_proc_elim(elim_opt_imported, NeededMap, ElimMap, !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        globals.lookup_bool_option(Globals, debug_dead_proc_elim,
            DebugDeadProcElim),
        (
            DebugDeadProcElim = no
        ;
            DebugDeadProcElim = yes,
            get_debug_output_stream(!.HLDS, DebugStream, !IO),
            output_needed_map(DebugStream, BeforeHLDS, NeededMap, !IO),
            output_elimination_msgs(DebugStream, BeforeHLDS, ElimMap, !IO)
        ),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        OptDeadProcs = do_not_opt_dead_procs
    ).

%---------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_middle_passes.
%---------------------------------------------------------------------------%
