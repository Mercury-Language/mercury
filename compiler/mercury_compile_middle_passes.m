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
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module io.
:- import_module set.

:- pred middle_pass(module_info::in, module_info::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

:- pred middle_pass_for_opt_file(module_info::in, module_info::out,
    io::di, io::uo) is det.

:- pred output_trans_opt_file(module_info::in, dump_info::in, dump_info::out,
    io::di, io::uo) is det.

:- pred output_analysis_file(module_info::in,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

:- pred maybe_unused_args(bool::in, bool::in,
    set(pragma_info_unused_args)::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

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
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.deep_profiling.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module top_level.mercury_compile_front_end.
:- import_module top_level.mercury_compile_llds_back_end.
:- import_module top_level.mercury_compile_mlds_back_end.
:- import_module transform_hlds.
:- import_module transform_hlds.accumulator.
:- import_module transform_hlds.closure_analysis.
:- import_module transform_hlds.complexity.
:- import_module transform_hlds.ctgc.
:- import_module transform_hlds.ctgc.structure_reuse.
:- import_module transform_hlds.ctgc.structure_reuse.analysis.
:- import_module transform_hlds.ctgc.structure_sharing.
:- import_module transform_hlds.ctgc.structure_sharing.analysis.
:- import_module transform_hlds.dead_proc_elim.
:- import_module transform_hlds.deforest.
:- import_module transform_hlds.delay_construct.
:- import_module transform_hlds.dep_par_conj.
:- import_module transform_hlds.distance_granularity.
:- import_module transform_hlds.equiv_type_hlds.
:- import_module transform_hlds.exception_analysis.
:- import_module transform_hlds.float_regs.
:- import_module transform_hlds.granularity.
:- import_module transform_hlds.higher_order.
:- import_module transform_hlds.implicit_parallelism.
:- import_module transform_hlds.implicit_parallelism.introduce_parallelism.
:- import_module transform_hlds.inlining.
:- import_module transform_hlds.intermod.
:- import_module transform_hlds.lambda.
:- import_module transform_hlds.lco.
:- import_module transform_hlds.loop_inv.
:- import_module transform_hlds.mmc_analysis.
:- import_module transform_hlds.par_loop_control.
:- import_module transform_hlds.parallel_to_plain_conj.
:- import_module transform_hlds.rbmm.
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
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module univ.

%---------------------------------------------------------------------------%

middle_pass(!HLDS, !DumpInfo, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_read_experimental_complexity_file(!HLDS, !IO),

    tabling(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 105, "tabling", !DumpInfo, !IO),

    expand_lambdas(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 110, "lambda", !DumpInfo, !IO),

    expand_stm_goals(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 113, "stm", !DumpInfo, !IO),

    expand_equiv_types_hlds(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 115, "equiv_types", !DumpInfo, !IO),

    maybe_closure_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 117, "closure_analysis", !DumpInfo, !IO),

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

    maybe_exception_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 118, "exception_analysis", !DumpInfo, !IO),

    maybe_termination(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 120, "termination", !DumpInfo, !IO),

    maybe_termination2(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 121, "termination2", !DumpInfo, !IO),

    maybe_type_ctor_infos(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 125, "type_ctor_infos", !DumpInfo, !IO),

    % warn_dead_procs must come after type_ctor_infos, so that it handles
    % unification & comparison procedures correctly, but it must also come
    % before optimizations such as higher-order specialization and inlining,
    % which can make the original code for a procedure dead by
    % inlining/specializing all uses of it.
    maybe_warn_dead_procs(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 130, "warn_dead_procs", !DumpInfo, !IO),

    maybe_bytecodes(!.HLDS, Verbose, Stats, !DumpInfo, !IO),

    maybe_untuple_arguments(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 133, "untupling", !DumpInfo, !IO),

    maybe_tuple_arguments(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 134, "tupling", !DumpInfo, !IO),

    maybe_higher_order(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 135, "higher_order", !DumpInfo, !IO),

    maybe_ssdb(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 137, "ssdb", !DumpInfo, !IO),

    maybe_introduce_accumulators(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 140, "accum", !DumpInfo, !IO),

    maybe_do_inlining(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 145, "inlining", !DumpInfo, !IO),

    % Hoisting loop invariants first invokes pass 148, "mark_static".
    % "mark_static" is also run at stage 420.
    maybe_loop_inv(Verbose, Stats, !HLDS, !DumpInfo, !IO),
    maybe_dump_hlds(!.HLDS, 150, "loop_inv", !DumpInfo, !IO),

    maybe_deforestation(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 155, "deforestation", !DumpInfo, !IO),

    maybe_delay_construct(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 160, "delay_construct", !DumpInfo, !IO),

    maybe_structure_sharing_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 162, "structure_sharing", !DumpInfo, !IO),

    maybe_structure_reuse_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 163, "structure_reuse", !DumpInfo, !IO),

    maybe_unused_args(Verbose, Stats, _UnusedArgsInfos, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 165, "unused_args", !DumpInfo, !IO),

    maybe_analyse_trail_usage(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 167, "trail_usage", !DumpInfo, !IO),

    maybe_unneeded_code(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 170, "unneeded_code", !DumpInfo, !IO),

    maybe_simplify(no, simplify_pass_pre_implicit_parallelism, Verbose, Stats,
        !HLDS, [], _SimplifySpecsPreImpPar, !IO),
    maybe_dump_hlds(!.HLDS, 172, "pre_implicit_parallelism_simplify",
        !DumpInfo, !IO),

    maybe_implicit_parallelism(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 173, "implicit_parallelism", !DumpInfo, !IO),

    maybe_analyse_mm_tabling(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 185, "mm_tabling_analysis", !DumpInfo, !IO),

    maybe_control_granularity(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 200, "granularity", !DumpInfo, !IO),

    maybe_control_distance_granularity(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 201, "distance_granularity", !DumpInfo, !IO),

    maybe_impl_dependent_par_conjs(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 205, "dependent_par_conj", !DumpInfo, !IO),

    maybe_par_loop_control(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 206, "par_loop_control", !DumpInfo, !IO),

    maybe_lco(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 210, "lco", !DumpInfo, !IO),

    maybe_float_reg_wrapper(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 213, "float_reg_wrapper", !DumpInfo, !IO),

    % If we are compiling in a deep profiling grade then now rerun simplify.
    % The reason for doing this now is that we want to take advantage of any
    % opportunities the other optimizations have provided for constant
    % propagation and we cannot do that once the term-size profiling or deep
    % profiling transformations have been applied.
    maybe_simplify(no, simplify_pass_pre_prof_transforms, Verbose, Stats,
        !HLDS, [], _SimplifySpecsPreProf, !IO),
    maybe_dump_hlds(!.HLDS, 215, "pre_prof_transforms_simplify", !DumpInfo,
        !IO),

    % The term size profiling transformation should be after all
    % transformations that construct terms of non-zero size. (Deep profiling
    % does not construct non-zero size terms.)
    maybe_term_size_prof(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 220, "term_size_prof", !DumpInfo, !IO),

    % The deep profiling transformation should be done late in the piece
    % since it munges the code a fair amount and introduces strange
    % disjunctions that might confuse other hlds->hlds transformations.
    maybe_deep_profiling(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 225, "deep_profiling", !DumpInfo, !IO),

    % Experimental complexity transformation should be done late in the
    % piece for the same reason as deep profiling. At the moment, they are
    % exclusive.
    maybe_experimental_complexity(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 230, "complexity", !DumpInfo, !IO),

    % XXX This may be moved to later.
    maybe_region_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 240, "region_analysis", !DumpInfo, !IO),

    maybe_eliminate_dead_procs(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 250, "dead_procs", !DumpInfo, !IO),

    maybe_dump_hlds(!.HLDS, 299, "middle_pass", !DumpInfo, !IO).

%---------------------------------------------------------------------------%

middle_pass_for_opt_file(!HLDS, !IO) :-
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
        expand_lambdas(Verbose, Stats, !HLDS, !IO),
        expand_stm_goals(Verbose, Stats, !HLDS, !IO)
    else
        true
    ),
    maybe_closure_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_exception_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_unused_args(Verbose, Stats, UnusedArgsInfos, !HLDS, !IO),
    maybe_termination(Verbose, Stats, !HLDS, !IO),
    maybe_termination2(Verbose, Stats, !HLDS, !IO),
    maybe_structure_sharing_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_structure_reuse_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_analyse_trail_usage(Verbose, Stats, !HLDS, !IO),
    maybe_analyse_mm_tabling(Verbose, Stats, !HLDS, !IO),

    append_analysis_pragmas_to_opt_file(!.HLDS, UnusedArgsInfos, !IO).

%---------------------------------------------------------------------------%

output_trans_opt_file(!.HLDS, !DumpInfo, !IO) :-
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
        expand_lambdas(Verbose, Stats, !HLDS, !IO)
    else
        true
    ),
    maybe_dump_hlds(!.HLDS, 110, "lambda", !DumpInfo, !IO),
    maybe_closure_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 117, "closure_analysis", !DumpInfo, !IO),
    maybe_exception_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 118, "exception_analysis", !DumpInfo, !IO),
    maybe_termination(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 120, "termination", !DumpInfo, !IO),
    maybe_termination2(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 121, "termination_2", !DumpInfo, !IO),
    (
        SharingAnalysis = yes,
        % These affect the results we write out for structure sharing/reuse
        % analysis.
        maybe_higher_order(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 135, "higher_order", !DumpInfo, !IO),
        maybe_do_inlining(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 145, "inlining", !DumpInfo, !IO),
        maybe_loop_inv(Verbose, Stats, !HLDS, !DumpInfo, !IO),
        maybe_dump_hlds(!.HLDS, 150, "loop_inv", !DumpInfo, !IO),
        maybe_deforestation(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 155, "deforestation", !DumpInfo, !IO)
    ;
        SharingAnalysis = no
    ),
    maybe_structure_sharing_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 162, "structure_sharing", !DumpInfo, !IO),
    maybe_structure_reuse_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 163, "structure_reuse", !DumpInfo, !IO),
    maybe_analyse_trail_usage(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 167, "trail_usage", !DumpInfo, !IO),
    maybe_analyse_mm_tabling(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 185, "mm_tabling_analysis", !DumpInfo, !IO),
    write_trans_opt_file(!.HLDS, !IO).

%---------------------------------------------------------------------------%

output_analysis_file(!.HLDS, !DumpInfo, !IO) :-
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
        expand_lambdas(Verbose, Stats, !HLDS, !IO)
    else
        true
    ),
    maybe_dump_hlds(!.HLDS, 110, "lambda", !DumpInfo, !IO),
    maybe_closure_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 117, "closure_analysis", !DumpInfo, !IO),
    maybe_exception_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 118, "exception_analysis", !DumpInfo, !IO),
    maybe_termination(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 120, "termination", !DumpInfo, !IO),
    maybe_termination2(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 121, "termination_2", !DumpInfo, !IO),
    (
        SharingAnalysis = yes,
        % These affect the results we write out for structure sharing/reuse
        % analysis.
        maybe_higher_order(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 135, "higher_order", !DumpInfo, !IO),
        maybe_do_inlining(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 145, "inlining", !DumpInfo, !IO),
        maybe_loop_inv(Verbose, Stats, !HLDS, !DumpInfo, !IO),
        maybe_dump_hlds(!.HLDS, 150, "loop_inv", !DumpInfo, !IO),
        maybe_deforestation(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 155, "deforestation", !DumpInfo, !IO)
    ;
        SharingAnalysis = no
    ),
    maybe_structure_sharing_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 162, "structure_sharing", !DumpInfo, !IO),
    maybe_structure_reuse_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 163, "structure_reuse", !DumpInfo, !IO),
    maybe_unused_args(Verbose, Stats, _UnusedArgsInfos, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 165, "unused_args", !DumpInfo, !IO),
    maybe_analyse_trail_usage(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 167, "trail_usage", !DumpInfo, !IO),
    maybe_analyse_mm_tabling(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 185, "mm_tabling_analysis", !DumpInfo, !IO),

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

:- pred maybe_read_experimental_complexity_file(
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_read_experimental_complexity_file(!HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_string_option(Globals, experimental_complexity, FileName),
    globals.lookup_bool_option(Globals, record_term_sizes_as_words,
        RecordTermSizesAsWords),
    globals.lookup_bool_option(Globals, record_term_sizes_as_cells,
        RecordTermSizesAsCells),
    bool.or(RecordTermSizesAsWords, RecordTermSizesAsCells, RecordTermSizes),
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
        (
            RecordTermSizes = yes
        ;
            RecordTermSizes = no,
            report_error("the --experimental-complexity option " ++
                "requires a term size profiling grade", !IO)
        ),
        complexity.read_spec_file(FileName, MaybeNumProcMap, !IO),
        (
            MaybeNumProcMap = ok(NumProcs - ProcMap),
            module_info_set_maybe_complexity_proc_map(yes(NumProcs - ProcMap),
                !HLDS)
        ;
            MaybeNumProcMap = error(Msg),
            report_error(Msg, !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred tabling(bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

tabling(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Transforming tabled predicates...", !IO),
    maybe_flush_output(Verbose, !IO),
    table_gen_process_module(!HLDS, [], Specs),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO),
    module_info_get_globals(!.HLDS, Globals),
    write_error_specs(Specs, Globals, 0, _NumWarnings, 0, NumErrors, !IO),
    module_info_incr_num_errors(NumErrors, !HLDS).

%---------------------------------------------------------------------------%

:- pred expand_lambdas(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

expand_lambdas(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Transforming lambda expressions...", !IO),
    maybe_flush_output(Verbose, !IO),
    expand_lambdas_in_module(!HLDS),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%---------------------------------------------------------------------------%

:- pred expand_stm_goals(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

expand_stm_goals(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Transforming stm expressions...", !IO),
    maybe_flush_output(Verbose, !IO),
    stm_process_module(!HLDS),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%---------------------------------------------------------------------------%

:- pred expand_equiv_types_hlds(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

expand_equiv_types_hlds(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Fully expanding equivalence types...", !IO),
    maybe_flush_output(Verbose, !IO),
    equiv_type_hlds.replace_in_hlds(!HLDS),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%---------------------------------------------------------------------------%

:- pred maybe_closure_analysis(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_closure_analysis(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, analyse_closures, ClosureAnalysis),
    (
        ClosureAnalysis = yes,
        maybe_write_string(Verbose, "% Analysing closures...\n", !IO),
        closure_analyse_module(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        ClosureAnalysis = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_exception_analysis(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_exception_analysis(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, analyse_exceptions, ExceptionAnalysis),
    (
        ExceptionAnalysis = yes,
        maybe_write_string(Verbose, "% Analysing exceptions...\n", !IO),
        analyse_exceptions_in_module(!HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        ExceptionAnalysis = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_termination(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_termination(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, termination, Termination),
    (
        Termination = yes,
        maybe_write_string(Verbose, "% Detecting termination...\n", !IO),
        analyse_termination_in_module(!HLDS, Specs),
        maybe_write_string(Verbose, "% Termination checking done.\n", !IO),
        write_error_specs(Specs, Globals, 0, _NumWarnings, 0, NumErrors, !IO),
        module_info_incr_num_errors(NumErrors, !HLDS),
        maybe_report_stats(Stats, !IO)
    ;
        Termination = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_termination2(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_termination2(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, termination2, Termination2),
    (
        Termination2 = yes,
        maybe_write_string(Verbose, "% Detecting termination 2...\n", !IO),
        term2_analyse_module(!HLDS, Specs),
        write_error_specs(Specs, Globals, 0, _NumWarnings, 0, NumErrors, !IO),
        module_info_incr_num_errors(NumErrors, !HLDS),
        maybe_write_string(Verbose, "% Termination 2 checking done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Termination2 = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_type_ctor_infos(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_type_ctor_infos(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, type_ctor_info, TypeCtorInfo),
    (
        TypeCtorInfo = yes,
        maybe_write_string(Verbose,
            "% Generating type_ctor_info structures...", !IO),
        maybe_flush_output(Verbose, !IO),
        type_ctor_info.generate_hlds(!HLDS),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        TypeCtorInfo = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_warn_dead_procs(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_warn_dead_procs(Verbose, Stats, !HLDS, !IO) :-
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
            maybe_write_string(Verbose, "% Warning about dead procedures...\n",
                !IO)
        ;
            WarnDeadProcs = no,
            maybe_write_string(Verbose, "% Warning about dead predicates...\n",
                !IO)
        ),
        maybe_flush_output(Verbose, !IO),
        dead_proc_warn(!.HLDS, Specs),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO),
        write_error_specs(Specs, Globals, 0, _NumWarnings, 0, NumErrors, !IO),
        module_info_incr_num_errors(NumErrors, !HLDS)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_bytecodes(module_info::in, bool::in, bool::in,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

maybe_bytecodes(HLDS0, Verbose, Stats, !DumpInfo, !IO) :-
    module_info_get_globals(HLDS0, Globals),
    globals.lookup_bool_option(Globals, generate_bytecode, GenBytecode),
    (
        GenBytecode = yes,
        map_args_to_regs(Verbose, Stats, HLDS0, HLDS1, !IO),
        maybe_dump_hlds(HLDS1, 505, "bytecode_args_to_regs", !DumpInfo, !IO),
        maybe_write_string(Verbose, "% Generating bytecodes...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        bytecode_gen.gen_module(HLDS1, Bytecode, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO),
        module_info_get_name(HLDS1, ModuleName),
        module_name_to_file_name(Globals, ModuleName, ".bytedebug",
            do_create_dirs, BytedebugFile, !IO),
        maybe_write_string(Verbose, "% Writing bytecodes to `", !IO),
        maybe_write_string(Verbose, BytedebugFile, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        maybe_flush_output(Verbose, !IO),
        debug_bytecode_file(BytedebugFile, Bytecode, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        module_name_to_file_name(Globals, ModuleName, ".mbc",
            do_create_dirs, BytecodeFile, !IO),
        maybe_write_string(Verbose, "% Writing bytecodes to `", !IO),
        maybe_write_string(Verbose, BytecodeFile, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        maybe_flush_output(Verbose, !IO),
        output_bytecode_file(BytecodeFile, Bytecode, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        GenBytecode = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_untuple_arguments(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_untuple_arguments(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, untuple, Untuple),
    (
        Untuple = yes,
        maybe_write_string(Verbose, "% Untupling...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        untuple_arguments(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_simplify(no, simplify_pass_post_untuple, Verbose, Stats, !HLDS,
            [], SimplifySpecs, !IO),
        expect(unify(contains_errors(Globals, SimplifySpecs), no),
            $module, $pred, "simplify has errors"),
        maybe_report_stats(Stats, !IO)
    ;
        Untuple = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_tuple_arguments(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_tuple_arguments(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, tuple, Tuple),
    (
        Tuple = yes,
        maybe_write_string(Verbose, "% Tupling...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        tuple_arguments(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Tuple = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_higher_order(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_higher_order(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, optimize_higher_order, HigherOrder),
    % --type-specialization implies --user-guided-type-specialization.
    globals.lookup_bool_option(Globals, user_guided_type_specialization,
        Types),

    % Always produce the specialized versions for which
    % `:- pragma type_spec' declarations exist, because
    % importing modules might call them.
    module_info_get_type_spec_info(!.HLDS, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(TypeSpecPreds, _, _, _),
    ( if
        ( HigherOrder = yes
        ; Types = yes
        ; set.is_non_empty(TypeSpecPreds)
        )
    then
        maybe_write_string(Verbose,
            "% Specializing higher-order and polymorphic predicates...\n",
            !IO),
        maybe_flush_output(Verbose, !IO),

        specialize_higher_order(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_ssdb(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_ssdb(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, force_disable_ssdebug,
        ForceDisableSSDB),
    (
        ForceDisableSSDB = no,
        globals.get_ssdb_trace_level(Globals, SSTraceLevel),
        (
            SSTraceLevel = none
        ;
            ( SSTraceLevel = shallow
            ; SSTraceLevel = deep
            ),
            maybe_write_string(Verbose,
                "% Apply source to source debugging transformation ...\n",
                !IO),
            ssdebug_transform_module(SSTraceLevel, !HLDS, !IO),
            maybe_write_string(Verbose, "% done.\n", !IO),
            maybe_report_stats(Stats, !IO)
        )
    ;
        ForceDisableSSDB = yes
    ).

%---------------------------------------------------------------------------%

:- pred maybe_implicit_parallelism(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_implicit_parallelism(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, implicit_parallelism,
        ImplicitParallelism),
    (
        ImplicitParallelism = yes,
        maybe_write_string(Verbose, "% Applying implicit " ++
            "parallelism...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        apply_implicit_parallelism_transformation(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        % The user hasn't asked for implicit parallelism.
        ImplicitParallelism = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_introduce_accumulators(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_introduce_accumulators(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, introduce_accumulators, Optimize),
    (
        Optimize = yes,
        maybe_write_string(Verbose,
            "% Attempting to introduce accumulators...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        type_to_univ([] : list(error_spec), Cookie0),
        Task0 = update_module_pred_cookie(accu_transform_proc, Cookie0),
        process_all_nonimported_procs_update(Task0, Task, !HLDS),
        ( if
            Task = update_module_pred_cookie(_, Cookie),
            univ_to_type(Cookie, SpecsPrime)
        then
            Specs = SpecsPrime
        else
            unexpected($module, $pred, "bad task")
        ),
        write_error_specs(Specs, Globals, 0, _NumWarnings, 0, NumErrors, !IO),
        module_info_incr_num_errors(NumErrors, !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Optimize = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_do_inlining(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_do_inlining(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, allow_inlining, Allow),
    globals.lookup_bool_option(Globals, inline_simple, Simple),
    globals.lookup_bool_option(Globals, inline_single_use, SingleUse),
    globals.lookup_int_option(Globals, inline_compound_threshold, Threshold),
    globals.lookup_bool_option(Globals, inline_linear_tail_rec_sccs,
        LinearRec),
    ( if
        Allow = yes,
        ( Simple = yes
        ; SingleUse = yes
        ; Threshold > 0
        ; LinearRec = yes
        )
    then
        maybe_write_string(Verbose, "% Inlining...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        inline_in_module(!HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_loop_inv(bool::in, bool::in,
    module_info::in, module_info::out, dump_info::in, dump_info::out,
    io::di, io::uo) is det.

maybe_loop_inv(Verbose, Stats, !HLDS, !DumpInfo, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, loop_invariants, LoopInv),
    (
        LoopInv = yes,
        % We run the mark_static pass because we need the construct_how flag
        % to be valid.
        maybe_mark_static_terms(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 148, "mark_static", !DumpInfo, !IO),

        maybe_write_string(Verbose, "% Hoisting loop invariants...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(
            update_module_pred(hoist_loop_invariants), !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        LoopInv = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_deforestation(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_deforestation(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, deforestation, Deforest),

    % --constraint-propagation implies --local-constraint-propagation.
    globals.lookup_bool_option(Globals, local_constraint_propagation,
        Constraints),
    ( if
        ( Deforest = yes
        ; Constraints = yes
        )
    then
        (
            Deforest = no,
            Constraints = no,
            unexpected($module, $pred, "no no")
        ;
            Deforest = yes,
            Constraints = yes,
            Msg = "% Deforestation and constraint propagation...\n"
        ;
            Deforest = yes,
            Constraints = no,
            Msg = "% Deforestation...\n"
        ;
            Deforest = no,
            Constraints = yes,
            Msg = "% Constraint propagation...\n"
        ),
        maybe_write_string(Verbose, Msg, !IO),
        maybe_flush_output(Verbose, !IO),
        deforestation(!HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_delay_construct(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_delay_construct(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, delay_construct, DelayConstruct),
    (
        DelayConstruct = yes,
        maybe_write_string(Verbose,
            "% Delaying construction unifications ...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_proc_ids(delay_construct_proc),
            !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        DelayConstruct = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_structure_sharing_analysis(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_structure_sharing_analysis(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, structure_sharing_analysis, Sharing),
    (
        Sharing = yes,
        maybe_write_string(Verbose, "% Structure sharing analysis...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        structure_sharing_analysis(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Sharing = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_structure_reuse_analysis(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_structure_reuse_analysis(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, structure_reuse_analysis,
        ReuseAnalysis),
    (
        ReuseAnalysis = yes,
        maybe_write_string(Verbose, "% Structure reuse analysis...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        perform_structure_reuse_analysis(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        ReuseAnalysis = no
    ).

%---------------------------------------------------------------------------%

maybe_unused_args(Verbose, Stats, UnusedArgsInfos, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, intermod_unused_args, Intermod),
    globals.lookup_bool_option(Globals, optimize_unused_args, Optimize),
    globals.lookup_bool_option(Globals, warn_unused_args, Warn),
    ( if
        ( Optimize = yes
        ; Warn = yes
        ; Intermod = yes
        )
    then
        maybe_write_string(Verbose, "% Finding unused arguments ...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        unused_args_process_module(!HLDS, Specs, UnusedArgsInfos),
        write_error_specs(Specs, Globals, 0, _NumWarnings, 0, NumErrors, !IO),
        module_info_incr_num_errors(NumErrors, !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    else
        set.init(UnusedArgsInfos)
    ).

%---------------------------------------------------------------------------%

:- pred maybe_analyse_trail_usage(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_analyse_trail_usage(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, analyse_trail_usage, AnalyseTrail),
    (
        AnalyseTrail = yes,
        maybe_write_string(Verbose, "% Analysing trail usage...\n", !IO),
        analyse_trail_usage(!HLDS),
        maybe_write_string(Verbose, "% Trail usage analysis done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        AnalyseTrail = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_unneeded_code(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_unneeded_code(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, unneeded_code, UnneededCode),
    (
        UnneededCode = yes,
        maybe_write_string(Verbose,
            "% Removing unneeded code from procedure bodies...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_module(unneeded_process_proc_msg),
            !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        UnneededCode = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_lco(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_lco(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, optimize_constructor_last_call, LCO),
    (
        LCO = yes,
        maybe_write_string(Verbose,
            "% Looking for LCO modulo constructor application ...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        lco_modulo_constructors(!HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        LCO = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_analyse_mm_tabling(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_analyse_mm_tabling(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, analyse_mm_tabling, TablingAnalysis),
    (
        TablingAnalysis = yes,
        maybe_write_string(Verbose, "% Analysing minimal model tabling...\n",
            !IO),
        analyse_mm_tabling_in_module(!HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        TablingAnalysis = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_control_granularity(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_control_granularity(Verbose, Stats, !HLDS, !IO) :-
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
            maybe_write_string(Verbose,
                "% Granularity control transformation...\n", !IO),
            maybe_flush_output(Verbose, !IO),
            control_granularity(!HLDS),
            maybe_write_string(Verbose, "% done.\n", !IO),
            maybe_report_stats(Stats, !IO)
        ;
            ( Target = target_csharp
            ; Target = target_java
            ; Target = target_erlang
            )
            % Leave the HLDS alone. We cannot implement parallelism,
            % so there is not point in controlling its granularity.
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_control_distance_granularity(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_control_distance_granularity(Verbose, Stats, !HLDS, !IO) :-
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
            maybe_write_string(Verbose,
                "% Distance granularity transformation...\n", !IO),
            maybe_flush_output(Verbose, !IO),
            control_distance_granularity(!HLDS, Distance),
            maybe_write_string(Verbose, "% done.\n", !IO),
            maybe_report_stats(Stats, !IO)
        ;
            ( Target = target_csharp
            ; Target = target_java
            ; Target = target_erlang
            )
            % Leave the HLDS alone. We cannot implement parallelism,
            % so there is not point in controlling its granularity.
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_impl_dependent_par_conjs(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_impl_dependent_par_conjs(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_has_parallel_conj(!.HLDS, HasParallelConj),
    (
        HasParallelConj = has_parallel_conj,
        module_info_get_globals(!.HLDS, Globals),
        current_grade_supports_par_conj(Globals, SupportsParConj),
        (
            SupportsParConj = no,
            process_all_nonimported_procs(
                update_proc_ids(parallel_to_plain_conjs), !HLDS)
        ;
            SupportsParConj = yes,
            maybe_write_string(Verbose,
                "% Dependent parallel conjunction transformation...\n", !IO),
            maybe_flush_output(Verbose, !IO),
            impl_dep_par_conjs_in_module(!HLDS),
            dead_proc_elim(do_not_elim_opt_imported, !HLDS),
            maybe_write_string(Verbose, "% done.\n", !IO),
            maybe_report_stats(Stats, !IO)
        )
    ;
        HasParallelConj = has_no_parallel_conj
    ).

%---------------------------------------------------------------------------%

:- pred maybe_par_loop_control(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_par_loop_control(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, par_loop_control, LoopControl),
    (
        LoopControl = yes,
        maybe_write_string(Verbose,
            "% Applying parallel loop control transformation...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        maybe_par_loop_control_module(!HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        LoopControl = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_float_reg_wrapper(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_float_reg_wrapper(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, use_float_registers, UseFloatRegs),
    (
        UseFloatRegs = yes,
        maybe_write_string(Verbose,
            "% Inserting float register wrappers...", !IO),
        maybe_flush_output(Verbose, !IO),
        float_regs.insert_reg_wrappers(!HLDS, Specs),
        write_error_specs(Specs, Globals, 0, _NumWarnings, 0, NumErrors, !IO),
        module_info_incr_num_errors(NumErrors, !HLDS),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        UseFloatRegs = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_term_size_prof(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_term_size_prof(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, record_term_sizes_as_words, AsWords),
    globals.lookup_bool_option(Globals, record_term_sizes_as_cells, AsCells),
    (
        AsWords = yes,
        AsCells = yes,
        unexpected($module, $pred, "as_words and as_cells")
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
        maybe_write_string(Verbose,
            "% Applying term size profiling transformation...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(
            update_module(size_prof_process_proc_msg(Transform)), !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        MaybeTransform = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_deep_profiling(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_deep_profiling(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, profile_deep, ProfileDeep),
    (
        ProfileDeep = yes,
        maybe_write_string(Verbose,
            "% Applying deep profiling transformation...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        apply_deep_profiling_transform(!HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        ProfileDeep = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_experimental_complexity(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_experimental_complexity(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_maybe_complexity_proc_map(!.HLDS, MaybeNumProcMap),
    (
        MaybeNumProcMap = no
    ;
        MaybeNumProcMap = yes(NumProcs - ProcMap),
        maybe_write_string(Verbose,
            "% Applying complexity experiment transformation...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(
            update_module(complexity_process_proc_msg(NumProcs, ProcMap)),
            !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred maybe_region_analysis(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_region_analysis(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, region_analysis, Analysis),
    (
        Analysis = yes,
        maybe_write_string(Verbose, "% Analysing regions ...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        do_region_analysis(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Analysis = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_eliminate_dead_procs(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_eliminate_dead_procs(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, optimize_dead_procs, Dead),
    (
        Dead = yes,
        maybe_write_string(Verbose, "% Eliminating dead procedures...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        dead_proc_elim(elim_opt_imported, !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Dead = no
    ).

%---------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_middle_passes.
%---------------------------------------------------------------------------%
