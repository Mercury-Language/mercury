%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_item_pragma.m.
% Original author: fjh.
% Main author of the current version: zs.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_item_pragma.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_data_rare.
:- import_module parse_tree.prog_item_pred_proc_id.
:- import_module recompilation.
:- import_module recompilation.item_types.

:- import_module list.
:- import_module maybe.
:- import_module one_or_more.
:- import_module set.

%---------------------------------------------------------------------------%

:- type item_decl_pragma_info
    --->    decl_pragma_obsolete_pred(decl_pragma_obsolete_pred_info)
    ;       decl_pragma_obsolete_proc(decl_pragma_obsolete_proc_info)
    ;       decl_pragma_format_call(decl_pragma_format_call_info)
    ;       decl_pragma_type_spec_constr(decl_pragma_type_spec_constr_info)
    ;       decl_pragma_type_spec(decl_pragma_type_spec_info)
    ;       decl_pragma_input_spec(decl_pragma_input_spec_info)
    ;       decl_pragma_oisu(decl_pragma_oisu_info)
    ;       decl_pragma_termination(decl_pragma_termination_info)
    ;       decl_pragma_termination2(decl_pragma_termination2_info)
    ;       decl_pragma_struct_sharing(decl_pragma_struct_sharing_info)
    ;       decl_pragma_struct_reuse(decl_pragma_struct_reuse_info).

:- type item_impl_pragma_info
    --->    impl_pragma_foreign_decl(impl_pragma_foreign_decl_info)
    ;       impl_pragma_foreign_code(impl_pragma_foreign_code_info)
    ;       impl_pragma_fproc_export(impl_pragma_fproc_export_info)
    ;       impl_pragma_external_proc(impl_pragma_external_proc_info)
    ;       impl_pragma_fact_table(impl_pragma_fact_table_info)
    ;       impl_pragma_tabled(impl_pragma_tabled_info)
    ;       impl_pragma_req_tail_rec(impl_pragma_req_tail_rec_info)
    ;       impl_pragma_req_feature_set(impl_pragma_req_feature_set_info).

:- type item_generated_pragma_info
    --->    gen_pragma_unused_args(gen_pragma_unused_args_info)
    ;       gen_pragma_exceptions(gen_pragma_exceptions_info)
    ;       gen_pragma_trailing(gen_pragma_trailing_info)
    ;       gen_pragma_mm_tabling(gen_pragma_mm_tabling_info).

:- func get_decl_pragma_context(item_decl_pragma_info) = prog_context.
:- func get_impl_pragma_context(item_impl_pragma_info) = prog_context.
:- func get_gen_pragma_context(item_generated_pragma_info) = prog_context.

%---------------------------------------------------------------------------%
%
% Decl pragmas.
%

:- type decl_pragma_obsolete_pred_info
    --->    decl_pragma_obsolete_pred_info(
                obspred_obsolete_pred   :: pred_pfu_name_arity,
                obspred_in_favour_of    :: list(sym_name_arity),
                obspred_context         :: prog_context,
                obspred_seq_num         :: item_seq_num
            ).

%---------------------%

:- type decl_pragma_obsolete_proc_info
    --->    decl_pragma_obsolete_proc_info(
                obsproc_obsolete_proc   :: proc_pf_name_modes,
                obsproc_in_favour_of    :: list(sym_name_arity),
                obsproc_context         :: prog_context,
                obsproc_seq_num         :: item_seq_num
            ).

%---------------------%

:- type decl_pragma_format_call_info
    --->    decl_pragma_format_call_info(
                format_pred             :: pf_sym_name_user_arity,
                format_values           :: one_or_more(format_string_values),
                format_context          :: prog_context,
                format_seq_num          :: item_seq_num
            ).

%---------------------%

:- type decl_pragma_type_spec_constr_info
    --->    decl_pragma_type_spec_constr_info(
                % The name of the module from whose (source or interface) file
                % we read the type_spec_constrained_preds pragma. This will
                % always name the module that contains the pragma, because
                % we never put a type_spec_constrained_preds pragma into
                % any interface file other than an interface file of the
                % module containing the pragma.
                tsc_module_name         :: module_name,

                % The list of constraints in the first argument of the pragma.
                % The pragma asks for the type specialization of any predicates
                % whose class context includes any nonempty subset of these
                % constraints, and possibly (see the next field) their
                % superclasses, as instances.
                tsc_constraints     :: one_or_more(var_or_ground_constraint),

                % The second argument of the pragma, which specifies whether
                % the constraints in the first argument also implicitly specify
                % their superclasses, *their* superclasses, and so on.
                % If e.g. tc1(A, B, C) has tc2(A, B) as one of its
                % superclasses, then a setting of apply_to_supers in this field
                % means that the pragma asks us to specialize not only
                % predicates whose class context includes tc1(A, char, B)
                % (if that is has as its instance of one of the constraints),
                % but also e.g. tc2(A, char).
                tsc_apply_to_supers     :: maybe_apply_to_supers,

                % The third argument of the pragma, which specifies the list
                % of type substitutions for which the pragma asks us to create
                % type-specialized versions of each predicate that matches
                % the requirements described by the first and second args.
                %
                % Each type var on the left-hand-side of a substitution
                % must occur in tsc_constraints, while all type vars that
                % occur in a type on the right-hand-side of a substitution
                % must be anonymous. These requirements are enforced by the
                % code that parses these pragmas.
                tsc_tsubst              :: one_or_more(type_subst),

                % The varset of the term containing the pragma, coerced
                % to being a tvarset (since all variables in the pragma
                % are type variables).
                %
                % All variables in this tvarset have to have explicit names.
                % If the original pragma contains anonymous variables, the
                % code constructing this decl_pragma_type_spec will give
                % those variable names. See the comment on the tspec_tvarset
                % field below for the reason behind this requirement.
                tsc_tvarset             :: tvarset,

                % The equivalence types used.
                tsc_items               :: set(recomp_item_id),

                tsc_context             :: prog_context,
                tsc_seq_num             :: item_seq_num
            ).

:- type maybe_apply_to_supers
    --->    do_not_apply_to_supers
    ;       apply_to_supers.

%---------------------%

:- type decl_pragma_type_spec_info
    --->    decl_pragma_type_spec_info(
                tspec_pfumm             :: pred_func_or_unknown_maybe_modes,

                % The existing predicate name.
                tspec_pred_name         :: sym_name,

                % The name of the module from whose (source or interface) file
                % we read the type_spec pragma. This will always name
                % the module that contains the pragma, because we never put
                % a type_spec pragma into any interface file other than
                % an interface file of the module containing the pragma.
                tspec_module_name       :: module_name,

                % The type substitution (using the variable names
                % from the pred declaration).
                tspec_tsubst            :: type_subst,

                % The varset of the term containing the pragma, coerced
                % to being a tvarset (since no part of the pragma except
                % the type substitution may contain variables).
                %
                % All variables in this tvarset have to have explicit names.
                % If the original pragma contains anonymous variables, the
                % code constructing this decl_pragma_type_spec will give
                % those variable names.
                %
                % The reason for this requirement is that the process
                % of writing out an anonymous variable and reading it back in
                % will produce a non-anonymous variable. Since the names
                % (if any) of the variables in tspec_tsubst are an input
                % to the code that constructs the name of the type-specialized
                % predicate, we would get a discrepancy between the predicate
                % name constructed by compiler invocations that know the
                % variable as unnamed (this will be the invocation that
                % compiles the module containing the type_spec pragma,
                % which constructs the code of the type specialized predicate),
                % and compiler invocations that know that variable as named
                % (this will be all the invocations that read the original
                % module's .int file, which will be constructing many of
                % the *calls* to the type specialized predicate). The result
                % will be calls to the type specialized predicate that refer
                % to it by the wrong name, leading to link errors.
                %
                % By giving all anonymous variables in the type_spec pragma
                % in the original source file as soon as we have parsed it,
                % and then always using the resulting names, we avoid this
                % problem.
                tspec_tvarset           :: tvarset,

                % The equivalence types used.
                tspec_items             :: set(recomp_item_id),

                tspec_context           :: prog_context,
                tspec_seq_num           :: item_seq_num
            ).

%---------------------%

:- type decl_pragma_input_spec_info
    --->    decl_pragma_input_spec_info(
                % This pragma tells the compiler to replace code
                % that switches on values of a control type at runtime
                % with code that switches on those values at compile time.
                %
                % Given a type such as
                %
                %   :- type action
                %       --->    hoist_nested_funcs
                %       ;       chain_gc_stack_frames.
                %
                % and its insts
                %
                %   :- inst hoist for action/0
                %       --->    hoist_nested_funcs.
                %   :- inst chain for action/0
                %       --->    chain_gc_stack_frames.
                %
                % input_spec pragma for type action with insts
                % hoist and chain can replace a mode that contains
                % an "in" argument of the action type with two modes
                % that contain "in(hoist)" and "in(chain)" respectively.
                % This effectively allows switches on that argument
                % to be performed at compile time. (This is a real example
                % from ml_elim_nested.m.)

                % The name of the module that this pragma occurs in.
                % The pragma applies to the predicates and functions
                % defined in this module, and *only* those defined
                % in this module.
                %
                % If and when we start --intermod-opt to include
                % input_spec pragmas in .opt files, we may also
                % need to record the section (interface vs implementation)
                % in which the pragma occurred.
                ispec_module_name       :: module_name,

                % We input specialize arguments of this type.
                ispec_arg_type          :: mer_type,

                % Do we replace the generic "in" mode with the set of
                % specialized "in(inst_n)" modes, or do keep the old "in"
                % mode as well? Only the latter preserves the ability to call
                % the transformed predicate or function without knowing
                % which of the specialized insts is applicable.
                ispec_replace_or_add    :: replace_or_add_in_mode,

                % The insts we specialize arguments of the selected type for.
                % The pragma in the code contains each inst_ctor as simply
                % a name; the parser adds the arity, which will be zero.
                % (Input specialization is not applicable to any inst_ctor
                % that takes any arguments.)
                %
                % We keep each inst_ctor in two forms: an inst_ctor,
                % and an inst that applies that inst_ctor to the empty list
                % of arguments.
                %
                % Both forms start out as just containing the inst name
                % that the program contains, and then both get module
                % qualified along with the rest of the compilation unit.
                % The difference between them is that the inst form
                % then also gets any inst equivalences in it expanded out.
                % It is the inst form that input_specialization.m uses
                % to actually implement the pragma, but for writing out
                % the pragma, we want the non-equivalence-expanded form
                % (since the expansion result can change if the set of
                % visible inst equivalences changes.)
                ispec_spec_inst_ctors   :: one_or_more(inst_ctor),
                ispec_spec_insts        :: one_or_more(mer_inst),

                % The equivalence types and insts used.
                %
                % At the moment, we gather this info, but then ignore it.
                % For smart recompilation to work, we need to fix the latter.
                ispec_items             :: set(recomp_item_id),

                ispec_tvarset           :: tvarset,
                % We do not need an inst_varset.

                ispec_context           :: prog_context,
                ispec_seq_num           :: item_seq_num
            ).

    % Do we want to *replace* the "in" mode with the specialized
    % "in(inst1)", "in(inst2)" modes, or do we want to *add* them?
    %
    % Note that add_to_in_mode is the only allowed value if the pragma
    % occurs in the interface (and therefore applies to predicates
    % in the interface).
:- type replace_or_add_in_mode
    --->    replace_in_mode
    ;       add_to_in_mode.

%---------------------%

:- type decl_pragma_oisu_info
    --->    decl_pragma_oisu_info(
                oisu_type_ctor          :: type_ctor,
                oisu_creator_preds      :: list(pf_sym_name_user_arity),
                oisu_transformer_preds  :: list(pf_sym_name_user_arity),
                oisu_destroyer_preds    :: list(pf_sym_name_user_arity),
                oisu_context            :: prog_context,
                oisu_seq_num            :: item_seq_num
            ).

%---------------------%

% The termination/termination2 pragmas record information
% about a predicate's or function's termination properties for our
% two different termination analyzers. Even though they are usually
% compiler generated, they are decl pragmas, not gen pragmas, because
% we allow users to include them in Mercury source programs, to tell
% the analyzers some things that they cannot figure out for themselves,
% such as the termination properties of foreign language code in
% foreign_procs.

:- type decl_pragma_termination_info
    --->    decl_pragma_termination_info(
                % The modes represent the declared argmodes of the procedure,
                % unless there are no declared argmodes, in which case
                % we use the inferred argmodes.
                terminfo_pred_id        :: proc_pf_name_modes,
                terminfo_args           :: maybe(pragma_arg_size_info),
                terminfo_term           :: maybe(pragma_termination_info),
                terminfo_context        :: prog_context,
                terminfo_seq_num        :: item_seq_num
            ).

%---------------------%

:- type decl_pragma_termination2_info
    --->    decl_pragma_termination2_info(
                terminfo2_pred_id       :: proc_pf_name_modes,
                terminfo2_args          :: maybe(pragma_constr_arg_size_info),
                terminfo2_args2         :: maybe(pragma_constr_arg_size_info),
                terminfo2_term          :: maybe(pragma_termination_info),
                terminfo2_context       :: prog_context,
                terminfo2_seq_num       :: item_seq_num
            ).

%---------------------%

% The sharing/reuse pragmas record information about a predicate's or
% function's properties that are relevant for compile-time garbage
% collection (ctgx). Even though they are usually compiler generated,
% they are decl pragmas, not gen pragmas, because we allow users
% to include them in Mercury source programs, to tell the compiler some things
% that it cannot figure out for itself, such as the ctgc properties
% of foreign language code in foreign_procs.

:- type decl_pragma_struct_sharing_info
    --->    decl_pragma_struct_sharing_info(
                % After structure sharing analysis, the compiler generates
                % structure sharing pragmas to be stored in and read from
                % optimization interface files.
                %
                % The list of modes consists of the declared argmodes
                % (or inferred argmodes if there are no declared ones).
                sharing_pred_id         :: proc_pf_name_modes,
                sharing_headvars        :: list(prog_var),
                sharing_headvar_types   :: list(mer_type),

                % The prog_varset and tvarset are meaningful only when
                % writing out this pragma; add_pragma.m ignores both varsets.
                sharing_varset          :: prog_varset,
                sharing_tvarset         :: tvarset,

                % As of 2019 10 29, and probably long before then,
                % the compiler *always* fills this slot with `yes(...)'.
                % A `no' would mean that the relevant information is not
                % available, but in that case, we simply do not write out
                % this pragma.
                sharing_description     :: maybe(structure_sharing_domain),

                sharing_context         :: prog_context,
                sharing_seq_num         :: item_seq_num
            ).

%---------------------%

:- type decl_pragma_struct_reuse_info
    --->    decl_pragma_struct_reuse_info(
                % After reuse analysis, the compiler generates structure reuse
                % pragmas to be stored in and read from optimization interface
                % files.
                %
                % The list of modes consists of the declared argmodes
                % (or inferred argmodes if there are no declared ones).
                %
                % The last sym_name (reuse_optimised_name) stores the name
                % of the optimised version of the exported predicate.
                % XXX As of 2019 10 29, the word "reuse_optimised_name"
                % appears nowhere in the compiler apart from this comment.
                reuse_pred_id           :: proc_pf_name_modes,
                reuse_headvars          :: list(prog_var),
                reuse_headvar_types     :: list(mer_type),

                % The prog_varset and tvarset are meaningful only when
                % writing out this pragma; add_pragma.m ignores both varsets.
                reuse_varset            :: prog_varset,
                reuse_tvarset           :: tvarset,

                % As of 2019 10 29, and probably long before then,
                % the compiler *always* fills this slot with `yes(...)'.
                % A `no' would mean that the relevant information is not
                % available, but in that case, we simply do not write out
                % this pragma.
                reuse_description       :: maybe(structure_reuse_domain),

                reuse_context           :: prog_context,
                reuse_seq_num           :: item_seq_num
            ).

%---------------------%

:- type item_decl_marker_info
    --->    item_decl_marker_info(
                dm_marker_kind          :: decl_pragma_marker_kind,
                dm_pred_spec            :: pred_pfu_name_arity,
                dm_context              :: prog_context,
                dm_seq_num              :: item_seq_num
            ).

:- type item_decl_marker_info_opt =< item_decl_marker_info
    --->    item_decl_marker_info(
                dm_marker_kind          :: decl_pragma_marker_kind_opt,
                dm_pred_spec            :: pred_pfu_name_arity_pf,
                dm_context              :: prog_context,
                dm_seq_num              :: item_seq_num
            ).

    % XXX The "terminates" and "does_not_terminate" markers are assertions
    % about the behavior of a given predicate that the compiler may be able
    % to exploit when compiling other modules. The "check_termination" marker
    % is not like that: it is a directive that is useful only while
    % the compiler is working on the module in which it occurs. We should
    % therefore consider making this an *impl* marker, which would entail
    % allowing the "check_termination" pragma to occur only in implementation
    % sections, even when the predicate/function they name is exported.
:- type decl_pragma_marker_kind
    --->    dpmk_terminates
    ;       dpmk_does_not_terminate
    ;       dpmk_check_termination.

:- type decl_pragma_marker_kind_opt =< decl_pragma_marker_kind
    --->    dpmk_terminates
    ;       dpmk_does_not_terminate.

%---------------------------------------------------------------------------%
%
% Impl pragmas.
%

:- type impl_pragma_foreign_decl_info
    --->    impl_pragma_foreign_decl_info(
                % A foreign language declaration, such as C header code.
                decl_lang               :: foreign_language,
                decl_is_local           :: foreign_decl_is_local,
                decl_decl               :: foreign_literal_or_include,
                decl_context            :: prog_context,
                decl_seq_num            :: item_seq_num
            ).

%---------------------%

:- type impl_pragma_foreign_code_info
    --->    impl_pragma_foreign_code_info(
                code_lang               :: foreign_language,
                code_code               :: foreign_literal_or_include,
                code_context            :: prog_context,
                code_seq_num            :: item_seq_num
            ).

%---------------------%

:- type impl_pragma_fproc_export_info
    --->    impl_pragma_fproc_export_info(
                exp_maybe_attrs         :: item_maybe_attrs,

                exp_language            :: foreign_language,
                % Predname, Predicate/function, Modes, foreign function name.
                exp_pred_id             :: proc_pf_name_modes,
                exp_foreign_name        :: string,

                % Specified the names of any variables in the modes above.
                % Used for generating error messages about foreign_export
                % pragmas for undeclared modes.
                exp_varset              :: prog_varset,

                exp_context             :: prog_context,
                exp_seq_num             :: item_seq_num
            ).

%---------------------%

:- type impl_pragma_external_proc_info
    --->    impl_pragma_external_proc_info(
                % The specified procedure(s) is/are implemented outside
                % of Mercury code, for the named backend if there is one,
                % or if there isn't a named backend, then for all backends.
                external_name           :: pf_sym_name_user_arity,
                external_maybe_backend  :: maybe(backend),
                external_context        :: prog_context,
                external_seq_num        :: item_seq_num
            ).

%---------------------%

:- type impl_pragma_fact_table_info
    --->    impl_pragma_fact_table_info(
                % Predname and Arity, Fact file name.
                fact_table_pred         :: pred_pfu_name_arity,
                fact_table_filename     :: string,
                fact_table_context      :: prog_context,
                fact_table_seq_num      :: item_seq_num
            ).

%---------------------%

:- type impl_pragma_tabled_info
    --->    impl_pragma_tabled_info(
                % Tabling type, Predname, Arity, PredOrFunc?, Mode?
                tabled_method           :: tabled_eval_method,
                tabled_name             :: pred_or_proc_pfumm_name,
                tabled_attributes       :: maybe(table_attributes),
                tabled_context          :: prog_context,
                tabled_seq_num          :: item_seq_num
            ).

%---------------------%

:- type impl_pragma_req_tail_rec_info
    --->    impl_pragma_req_tail_rec_info(
                rtr_proc_id             :: pred_or_proc_pfumm_name,
                rtr_require_tailrec     :: require_tail_recursion,
                % This parameter only makes sense when options contains
                % either rtro_mutual_rec_only or rtro_all_recursion.
                % TODO, currently unused, may be used later to implement one
                % of Zoltan's suggestions here:
                % http://www.mercurylang.org/list-archives/developers/
                %   2015-November/016482.html
                % rtr_maybe_scc           :: maybe(list(
                %                             pred_or_proc_pfumm_name))
                rtr_context             :: prog_context,
                rtr_seq_num             :: item_seq_num
            ).

%---------------------%

:- type impl_pragma_req_feature_set_info
    --->    impl_pragma_req_feature_set_info(
                rfs_feature_set         :: set(required_feature),
                rfs_context             :: prog_context,
                rfs_seq_num             :: item_seq_num
            ).

%---------------------%

:- type item_impl_marker_info
    --->    item_impl_marker_info(
                im_marker_kind          :: impl_pragma_marker_kind,
                im_pred_spec            :: pred_pfu_name_arity,
                im_context              :: prog_context,
                im_seq_num              :: item_seq_num
            ).

:- type item_impl_marker_info_opt =< item_impl_marker_info
    --->    item_impl_marker_info(
                im_marker_kind          :: impl_pragma_marker_kind_opt,
                im_pred_spec            :: pred_pfu_name_arity_pf,
                im_context              :: prog_context,
                im_seq_num              :: item_seq_num
            ).

:- type impl_pragma_marker_kind
    --->    ipmk_inline
    ;       ipmk_no_inline
    ;       ipmk_consider_used
    ;       ipmk_mode_check_clauses
    ;       ipmk_no_detism_warning
    ;       ipmk_promise_pure
    ;       ipmk_promise_semipure
    ;       ipmk_promise_eqv_clauses
    ;       ipmk_req_sw_arms_type_order.

    % These are the kinds of impl markers that we put into .opt files.
:- type impl_pragma_marker_kind_opt =< impl_pragma_marker_kind
    --->    ipmk_inline
    ;       ipmk_no_inline
    ;       ipmk_mode_check_clauses
    ;       ipmk_promise_pure
    ;       ipmk_promise_semipure
    ;       ipmk_promise_eqv_clauses.

%---------------------------------------------------------------------------%
%
% Generated pragmas.
%

:- type gen_pragma_unused_args_info
    --->    gen_pragma_unused_args_info(
                % This pragma should only appear in .opt files.
                unused_proc_id          :: proc_pf_name_arity_mn,

                % The argument positions of the unused arguments.
                % Used for intermodule unused argument removal.
                unused_args             :: list(int),

                unused_context          :: prog_context,
                unused_seq_num          :: item_seq_num
            ).

:- type gen_pragma_exceptions_info
    --->    gen_pragma_exceptions_info(
                % This pragma should only appear in `.opt' and
                % `.trans_opt' files.
                exceptions_proc_id      :: proc_pf_name_arity_mn,
                exceptions_status       :: exception_status,

                exceptions_context      :: prog_context,
                exceptions_seq_num      :: item_seq_num
            ).

:- type gen_pragma_trailing_info
    --->    gen_pragma_trailing_info(
                % This pragma should only appear in `.trans_opt' files.
                trailing_proc_id        :: proc_pf_name_arity_mn,
                trailing_status         :: trailing_status,

                trailing_context        :: prog_context,
                trailing_seq_num        :: item_seq_num
            ).

:- type gen_pragma_mm_tabling_info
    --->    gen_pragma_mm_tabling_info(
                % This pragma should only appear in `.opt' and
                % `.trans_opt' files.
                mm_tabling_proc_id      :: proc_pf_name_arity_mn,
                mm_tabling_status       :: mm_tabling_status,

                mm_tabling_context      :: prog_context,
                mm_tabling_seq_num      :: item_seq_num
            ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

get_decl_pragma_context(DeclPragma) = Context :-
    (
        DeclPragma = decl_pragma_obsolete_pred(ObsPred),
        Context = ObsPred ^ obspred_context
    ;
        DeclPragma = decl_pragma_obsolete_proc(ObsProc),
        Context = ObsProc ^ obsproc_context
    ;
        DeclPragma = decl_pragma_format_call(FormatCall),
        Context = FormatCall ^ format_context
    ;
        DeclPragma = decl_pragma_type_spec_constr(TypeSpecConstr),
        Context = TypeSpecConstr ^ tsc_context
    ;
        DeclPragma = decl_pragma_type_spec(TypeSpec),
        Context = TypeSpec ^ tspec_context
    ;
        DeclPragma = decl_pragma_input_spec(InputSpec),
        Context = InputSpec ^ ispec_context
    ;
        DeclPragma = decl_pragma_oisu(OISU),
        Context = OISU ^ oisu_context
    ;
        DeclPragma = decl_pragma_termination(Term),
        Context = Term ^ terminfo_context
    ;
        DeclPragma = decl_pragma_termination2(Term2),
        Context = Term2 ^ terminfo2_context
    ;
        DeclPragma = decl_pragma_struct_sharing(Sharing),
        Context = Sharing ^ sharing_context
    ;
        DeclPragma = decl_pragma_struct_reuse(Reuse),
        Context = Reuse ^ reuse_context
    ).

get_impl_pragma_context(ImplPragma) = Context :-
    (
        ImplPragma = impl_pragma_foreign_decl(ForeignDecl),
        Context = ForeignDecl ^ decl_context
    ;
        ImplPragma = impl_pragma_foreign_code(ForeignCode),
        Context = ForeignCode ^ code_context
    ;
        ImplPragma = impl_pragma_fproc_export(Export),
        Context = Export ^ exp_context
    ;
        ImplPragma = impl_pragma_external_proc(ExternalProc),
        Context = ExternalProc ^ external_context
    ;
        ImplPragma = impl_pragma_fact_table(FactTable),
        Context = FactTable ^ fact_table_context
    ;
        ImplPragma = impl_pragma_tabled(Tabled),
        Context = Tabled ^ tabled_context
    ;
        ImplPragma = impl_pragma_req_tail_rec(TailRec),
        Context = TailRec ^ rtr_context
    ;
        ImplPragma = impl_pragma_req_feature_set(FeatureSet),
        Context = FeatureSet ^ rfs_context
    ).

get_gen_pragma_context(GenPragma) = Context :-
    (
        GenPragma = gen_pragma_unused_args(UnusedArgs),
        Context = UnusedArgs ^ unused_context
    ;
        GenPragma = gen_pragma_exceptions(Excps),
        Context = Excps ^ exceptions_context
    ;
        GenPragma = gen_pragma_trailing(Trailing),
        Context = Trailing ^ trailing_context
    ;
        GenPragma = gen_pragma_mm_tabling(MMTabling),
        Context = MMTabling ^ mm_tabling_context
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_item_pragma.
%---------------------------------------------------------------------------%
