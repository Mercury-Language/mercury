%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_out_util.m.
% Author: zs.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_util.
:- interface.

:- import_module hlds.hlds_class.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.indent.
:- import_module libs.maybe_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module string.builder.
:- import_module term.

%---------------------------------------------------------------------------%

:- func empty_dump_options = hlds_dump_options.

:- type hlds_dump_options
    --->    hlds_dump_options(
                dump_unify_argmodes             :: bool,    % a
                dump_call_builtin_status        :: bool,    % b
                dump_goal_type_contexts         :: bool,    % c
                dump_goal_determinism           :: bool,    % d
                dump_region_annotations         :: bool,    % e
                dump_follow_vars                :: bool,    % f
                dump_goal_features              :: bool,    % g
                dump_goal_instmap_vars          :: bool,    % i
                dump_call_pred_ids              :: bool,    % l
                dump_clause_modes               :: bool,    % m
                dump_goal_nonlocals             :: bool,    % n
                dump_goal_birth_death_sets      :: bool,    % p
                dump_goal_resume_points         :: bool,    % r
                dump_goal_store_maps            :: bool,    % s
                dump_termination_analysis       :: bool,    % t
                dump_unification_details        :: bool,    % u
                dump_var_numbers_in_names       :: bool,    % v
                dump_predicates                 :: bool,    % x
                dump_unify_argmodes_struct      :: bool,    % y
                dump_goal_purity_markers        :: bool,    % z
                dump_arg_passing_info           :: bool,    % A
                dump_mode_constraints           :: bool,    % B
                dump_clauses                    :: bool,    % C
                dump_goal_instmap_deltas        :: bool,    % D
                dump_deep_profiling             :: bool,    % E
                dump_ctgc                       :: bool,    % G
                % The dump_imports field controls both
                % - whether we dump the contents of {import,use}_module decls,
                % - and whether we dump the code of imported preds.
                dump_imports                    :: bool,    % I
                dump_type_table_only_local      :: bool,    % L
                dump_inst_mode_tables           :: bool,    % M
                dump_dependency_ordering        :: bool,    % O
                dump_goal_ids_paths             :: bool,    % P
                dump_use_reuse_info             :: bool,    % R
                dump_struct_sharing_info        :: bool,    % S
                dump_type_typeclass_tables      :: bool,    % T
                dump_unify_compare_preds        :: bool,    % U
                dump_cons_table                 :: bool,    % W
                dump_constant_structures        :: bool,    % X
                dump_structured_insts           :: bool,    % Y
                dump_call_answer_tables         :: bool     % Z
            ).

%---------------------------------------------------------------------------%

:- type hlds_out_info
    --->    hlds_out_info(
                hoi_dump_hlds_options           :: hlds_dump_options,
                hoi_dump_hlds_options_backup    :: hlds_dump_options,
                hoi_dump_hlds_pred_ids          :: list(string),
                hoi_dump_hlds_pred_names        :: list(string),
                hoi_merc_out_info               :: merc_out_info
            ).

:- func init_hlds_out_info(globals, output_lang) = hlds_out_info.

%---------------------------------------------------------------------------%

    % pred_id_to_user_string returns a string that is suitable to identify
    % a predicate to a user
    %
    % - in progress messages,
    % - in error messages, and
    % - as the expansion of $pred.
    %
    % For user written predicates, the result will look like this:
    %
    %       predicate `foo.bar'/3
    %       function `foo.myfoo'/5
    %
    % For predicates created by the compiler, the result be a description
    % such as
    %
    %       unification predicate for `map'/2
    %
    % For predicates that are the transformed version of another predicate,
    % the result will identify the original predicate at the start of the
    % transformation chain (which may contain more than one transformation)
    % and will say that a transformation happened, but will not say
    % how many transformations happened, or what the transformations were,
    % because
    %
    % - such details are not needed in progress messages, and
    % - they *should* not be needed for error messages, since we should
    %   not be reporting any errors for transformed predicates at all.
    %
    % The versions that also specify a proc_id do the same job, only
    % they also append the procedure's mode number.
    %
:- func pred_id_to_user_string(module_info, pred_id) = string.
:- func pred_proc_id_to_user_string(module_info, pred_proc_id) = string.
:- func pred_proc_id_pair_to_user_string(module_info, pred_id, proc_id)
    = string.

    % pred_id_to_dev_string returns a string that is suitable to identify
    % a predicate to a developer
    %
    % - in HLDS dumps (the parts other than the full pred provenance),
    % - in compiler output intended to debug the compiler itself, and
    % - in progress messages intended to help make sense of such output.
    %
    % The output will differ from pred_id_to_user_string in two main ways:
    %
    % - it will contain no quotes, because the unbalanced `' quote style
    %   we usually use screws up syntax highlighting in HLDS dumps, and
    %
    % - it will contain a description of each transformation applied to
    %   the base predicate.
    %
    % The versions that also specify a proc_id do the same job, only
    % they also append the procedure's mode number.
    %
:- func pred_id_to_dev_string(module_info, pred_id) = string.
:- func pred_proc_id_to_dev_string(module_info, pred_proc_id) = string.
:- func pred_proc_id_pair_to_dev_string(module_info, pred_id, proc_id)
    = string.

%---------------------------------------------------------------------------%

:- type last_context_word
    --->    lcw_none
    ;       lcw_call
    ;       lcw_result
    ;       lcw_argument
    ;       lcw_element.

    % unify_context_to_pieces generates a message such as
    %   foo.m:123:   in argument 3 of functor `foo/5':
    %   foo.m:123:   in unification of `X' and `blah':
    % based on the unify_context and prog_context.
    %
:- pred unify_context_to_pieces(unify_context::in, last_context_word::out,
    list(format_piece)::in, list(format_piece)::out) is det.

    % unify_context_first_to_pieces is the same as above, except that
    % it also takes and returns a flag which specifies whether this is the
    % start of a sentence. If the first argument is `is_first', then it means
    % this is the first line of an error message, so the message starts with
    % a capital letter, e.g.
    %   foo.m:123:   In argument 3 of functor `foo/5':
    %   foo.m:123:   in unification of `X' and `blah':
    % The flag returned as the second argument will be `is_not_first'
    % unless nothing was generated, in which case it will be the same
    % as the first argument.
    %
:- pred unify_context_first_to_pieces(is_first::in, is_first::out,
    unify_context::in, last_context_word::out,
    list(format_piece)::in, list(format_piece)::out) is det.

%---------------------------------------------------------------------------%

    % maybe_output_context_comment(Stream, Indent, Suffix, Context, !IO):
    %
    % If the given context is meaningful, output it in a form suitable
    % for a comment in HLDS dumps, followed by Suffix.
    %
:- pred maybe_output_context_comment(io.text_output_stream::in, indent::in,
    string::in, term.context::in, io::di, io::uo) is det.
:- pred maybe_format_context_comment(indent::in, string::in, term.context::in,
    string.builder.state::di, string.builder.state::uo) is det.

:- func context_to_brief_string(term.context) = string.

%---------------------------------------------------------------------------%

    % When a higher order call uses either P(A, B, C) or C = F(A, B) syntax,
    % we normally identify the call as being to "the predicate P" or to
    % "the function F". However, there is a category of errors for which
    % this is inappropriate: when the error is calling a function-valued
    % variable as if it were a predicate, and vice versa. In such cases,
    % we don't want the description of the error's context to say e.g.
    % "in the call to the predicate P", and the description of the error
    % itself to say "P is a function, but should be a predicate".
    % Code that wants to report such errors should call the functions below
    % with do_not_print_ho_var_name; pretty much all other callers should
    % pass print_ho_var_name.
:- type maybe_print_ho_var_name
    --->    do_not_print_ho_var_name
    ;       print_ho_var_name.

:- func call_id_to_pieces(maybe_print_ho_var_name, call_id) =
    list(format_piece).

    % generic_call_to_pieces(PrintHoVarName, VarNameSrc, GenericCall) = Pieces:
    %
    % Return a description of GenericCall as Pieces.
    %
    % For a description of the semantics of PrintHoVarName, see the
    % definition of its type above.
    %
    % We use VarNameSrc for describing the callee of higher order calls.
    % The type of this argument is var_name_source because we use this
    % function both during the type analysis pass (which occurs before
    % we construct var_tables, since it actually constructs var_tables),
    % and during mode and determinism analysis, which do use var_tables.
    %
:- func generic_call_to_pieces(maybe_print_ho_var_name, var_name_source,
    generic_call) = list(format_piece).

    % This variant of generic_call_to_string returns a string that
    % specifically describes the *callee* of the call, not the call
    % as a whole.
    %
:- func generic_callee_to_pieces(maybe_print_ho_var_name, var_name_source,
    generic_call) = list(format_piece).

:- func cast_type_to_string(cast_kind) = string.

    % Generate a message of the form "argument %i of call to pred_or_func
    % `foo/n'". The pred_markers argument is used to tell if the calling
    % predicate is a type class method implementation; if so, we omit the
    % "call to" part, since the user didn't write any explicit call.
    %
:- func call_arg_id_to_pieces(maybe_print_ho_var_name, call_id, int,
    pred_markers) = list(format_piece).

%---------------------------------------------------------------------------%

    % Return a printable representation of a functor and its arguments.
    % The var_name_source gives the names of any variables.
    %
:- func functor_to_string(var_name_source, var_name_print, const,
    list(prog_var)) = string.

:- func functor_to_string_maybe_needs_quotes(var_name_source, var_name_print,
    needs_quotes, const, list(prog_var)) = string.

:- func qualified_functor_to_string(var_name_source, var_name_print,
    module_name, const, list(prog_var)) = string.

:- func qualified_functor_with_term_args_to_string(var_name_source,
    var_name_print, module_name, const, list(prog_term)) = string.

    % Return a printable representation of a cons_id and arguments.
    % The var_name_source gives the names of any variables, while
    % the module_info allows the interpretation of cons_ids
    % that are shrouded references to procedures.
    %
:- func functor_cons_id_to_string(module_info, var_name_source, var_name_print,
    cons_id, list(prog_var)) = string.

:- type maybe_qualify_cons_id
    --->    qualify_cons_id
    ;       do_not_qualify_cons_id.

:- func cons_id_and_vars_or_arity_to_string(var_table, maybe_qualify_cons_id,
    cons_id, maybe(list(prog_var))) = string.

%---------------------------------------------------------------------------%

:- pred write_constraint_proof_map(io.text_output_stream::in, indent::in,
    var_name_print::in, tvarset::in, constraint_proof_map::in,
    io::di, io::uo) is det.
:- pred format_constraint_proof_map(indent::in,
    var_name_print::in, tvarset::in, constraint_proof_map::in,
    string.builder.state::di, string.builder.state::uo) is det.

%---------------------------------------------------------------------------%

    % Return a string representing a list of variables and their
    % corresponding modes (e.g. for a lambda expressions).
    %
:- func var_modes_to_string(output_lang, var_name_source, inst_varset,
    var_name_print, assoc_list(prog_var, mer_mode)) = string.

:- func var_mode_to_string(output_lang, var_name_source, inst_varset,
    var_name_print, pair(prog_var, mer_mode)) = string.

%---------------------------------------------------------------------------%

:- func type_import_status_to_string(type_status) = string.
:- func inst_import_status_to_string(inst_status) = string.
:- func mode_import_status_to_string(mode_status) = string.
:- func typeclass_import_status_to_string(typeclass_status) = string.
:- func instance_import_status_to_string(instance_status) = string.
:- func pred_import_status_to_string(pred_status) = string.

%---------------------------------------------------------------------------%

    % Write out a list of integers as a Mercury list.
    %
:- pred write_intlist(io.text_output_stream::in, list(int)::in,
    io::di, io::uo) is det.
:- pred format_intlist(list(int)::in,
    string.builder.state::di, string.builder.state::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.pred_name.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_parse_tree.    % undesirable dependency

:- import_module char.
:- import_module int.
:- import_module map.
:- import_module term_context.
:- import_module term_io.
:- import_module term_subst.
:- import_module varset.

%---------------------------------------------------------------------------%

empty_dump_options =
    hlds_dump_options(
        bool.no, bool.no, bool.no, bool.no, bool.no, bool.no, bool.no, bool.no,
        bool.no, bool.no, bool.no, bool.no, bool.no, bool.no, bool.no, bool.no,
        bool.no, bool.no, bool.no, bool.no, bool.no, bool.no, bool.no, bool.no,
        bool.no, bool.no, bool.no, bool.no, bool.no, bool.no, bool.no, bool.no,
        bool.no, bool.no, bool.no, bool.no, bool.no, bool.no, bool.no
    ).

:- func setup_hlds_dump_options(string) = hlds_dump_options.

setup_hlds_dump_options(Str) = DumpOptions :-
    DumpOptions0 = empty_dump_options,
    string.to_char_list(Str, Chars),
    list.foldl(apply_dump_option, Chars, DumpOptions0, DumpOptions).

:- pred apply_dump_option(char::in,
    hlds_dump_options::in, hlds_dump_options::out) is det.

apply_dump_option(Char, DumpOptions0, DumpOptions) :-
    ( if dump_opt(Char, SetFunc) then
        DumpOptions = SetFunc(DumpOptions0)
    else
        % XXX We probably should report this, though not reporting such errors
        % has not been much of an issue in 30+ years ...
        DumpOptions = DumpOptions0
    ).

:- pred dump_opt(char::in, (func(hlds_dump_options) = hlds_dump_options)::out)
    is semidet.

dump_opt('a', set_dump_unify_argmodes).
dump_opt('b', set_dump_call_builtin_status).
dump_opt('c', set_dump_goal_type_contexts).
dump_opt('d', set_dump_goal_determinism).
dump_opt('e', set_dump_region_annotations).
dump_opt('f', set_dump_follow_vars).
dump_opt('g', set_dump_goal_features).
dump_opt('i', set_dump_goal_instmap_vars).
dump_opt('l', set_dump_call_pred_ids).
dump_opt('m', set_dump_clause_modes).
dump_opt('n', set_dump_goal_nonlocals).
dump_opt('p', set_dump_goal_birth_death_sets).
dump_opt('r', set_dump_goal_resume_points).
dump_opt('s', set_dump_goal_store_maps).
dump_opt('t', set_dump_termination_analysis).
dump_opt('u', set_dump_unification_details).
dump_opt('v', set_dump_var_numbers_in_names).
dump_opt('x', set_dump_predicates).
dump_opt('y', set_dump_unify_argmodes_struct).
dump_opt('z', set_dump_goal_purity_markers).
dump_opt('A', set_dump_arg_passing_info).
dump_opt('B', set_dump_mode_constraints).
dump_opt('C', set_dump_clauses).
dump_opt('D', set_dump_goal_instmap_deltas).
dump_opt('E', set_dump_deep_profiling).
dump_opt('G', set_dump_ctgc).
dump_opt('I', set_dump_imports).
dump_opt('L', set_dump_type_table_only_local).
dump_opt('M', set_dump_inst_mode_tables).
dump_opt('O', set_dump_dependency_ordering).
dump_opt('P', set_dump_goal_ids_paths).
dump_opt('R', set_dump_use_reuse_info).
dump_opt('S', set_dump_struct_sharing_info).
dump_opt('T', set_dump_type_typeclass_tables).
dump_opt('U', set_dump_unify_compare_preds).
dump_opt('W', set_dump_cons_table).
dump_opt('X', set_dump_constant_structures).
dump_opt('Y', set_dump_structured_insts).
dump_opt('Z', set_dump_call_answer_tables).

:- func set_dump_unify_argmodes(hlds_dump_options) = hlds_dump_options.
:- func set_dump_call_builtin_status(hlds_dump_options) = hlds_dump_options.
:- func set_dump_goal_type_contexts(hlds_dump_options) = hlds_dump_options.
:- func set_dump_goal_determinism(hlds_dump_options) = hlds_dump_options.
:- func set_dump_region_annotations(hlds_dump_options) = hlds_dump_options.
:- func set_dump_follow_vars(hlds_dump_options) = hlds_dump_options.
:- func set_dump_goal_features(hlds_dump_options) = hlds_dump_options.
:- func set_dump_goal_instmap_vars(hlds_dump_options) = hlds_dump_options.
:- func set_dump_call_pred_ids(hlds_dump_options) = hlds_dump_options.
:- func set_dump_clause_modes(hlds_dump_options) = hlds_dump_options.
:- func set_dump_goal_nonlocals(hlds_dump_options) = hlds_dump_options.
:- func set_dump_goal_birth_death_sets(hlds_dump_options) = hlds_dump_options.
:- func set_dump_goal_resume_points(hlds_dump_options) = hlds_dump_options.
:- func set_dump_goal_store_maps(hlds_dump_options) = hlds_dump_options.
:- func set_dump_termination_analysis(hlds_dump_options) = hlds_dump_options.
:- func set_dump_unification_details(hlds_dump_options) = hlds_dump_options.
:- func set_dump_var_numbers_in_names(hlds_dump_options) = hlds_dump_options.
:- func set_dump_predicates(hlds_dump_options) = hlds_dump_options.
:- func set_dump_unify_argmodes_struct(hlds_dump_options) = hlds_dump_options.
:- func set_dump_goal_purity_markers(hlds_dump_options) = hlds_dump_options.
:- func set_dump_arg_passing_info(hlds_dump_options) = hlds_dump_options.
:- func set_dump_mode_constraints(hlds_dump_options) = hlds_dump_options.
:- func set_dump_clauses(hlds_dump_options) = hlds_dump_options.
:- func set_dump_goal_instmap_deltas(hlds_dump_options) = hlds_dump_options.
:- func set_dump_deep_profiling(hlds_dump_options) = hlds_dump_options.
:- func set_dump_ctgc(hlds_dump_options) = hlds_dump_options.
:- func set_dump_imports(hlds_dump_options) = hlds_dump_options.
:- func set_dump_type_table_only_local(hlds_dump_options) = hlds_dump_options.
:- func set_dump_inst_mode_tables(hlds_dump_options) = hlds_dump_options.
:- func set_dump_dependency_ordering(hlds_dump_options) = hlds_dump_options.
:- func set_dump_goal_ids_paths(hlds_dump_options) = hlds_dump_options.
:- func set_dump_use_reuse_info(hlds_dump_options) = hlds_dump_options.
:- func set_dump_struct_sharing_info(hlds_dump_options) = hlds_dump_options.
:- func set_dump_type_typeclass_tables(hlds_dump_options) = hlds_dump_options.
:- func set_dump_unify_compare_preds(hlds_dump_options) = hlds_dump_options.
:- func set_dump_cons_table(hlds_dump_options) = hlds_dump_options.
:- func set_dump_constant_structures(hlds_dump_options) = hlds_dump_options.
:- func set_dump_structured_insts(hlds_dump_options) = hlds_dump_options.
:- func set_dump_call_answer_tables(hlds_dump_options) = hlds_dump_options.

set_dump_unify_argmodes(X) = X ^ dump_unify_argmodes := yes.
set_dump_call_builtin_status(X) = X ^ dump_call_builtin_status := yes.
set_dump_goal_type_contexts(X) = X ^ dump_goal_type_contexts := yes.
set_dump_goal_determinism(X) = X ^ dump_goal_determinism := yes.
set_dump_region_annotations(X) = X ^ dump_region_annotations := yes.
set_dump_follow_vars(X) = X ^ dump_follow_vars := yes.
set_dump_goal_features(X) = X ^ dump_goal_features := yes.
set_dump_goal_instmap_vars(X) = X ^ dump_goal_instmap_vars := yes.
set_dump_call_pred_ids(X) = X ^ dump_call_pred_ids := yes.
set_dump_clause_modes(X) = X ^ dump_clause_modes := yes.
set_dump_goal_nonlocals(X) = X ^ dump_goal_nonlocals := yes.
set_dump_goal_birth_death_sets(X) = X ^ dump_goal_birth_death_sets := yes.
set_dump_goal_resume_points(X) = X ^ dump_goal_resume_points := yes.
set_dump_goal_store_maps(X) = X ^ dump_goal_store_maps := yes.
set_dump_termination_analysis(X) = X ^ dump_termination_analysis := yes.
set_dump_unification_details(X) = X ^ dump_unification_details := yes.
set_dump_var_numbers_in_names(X) = X ^ dump_var_numbers_in_names := yes.
set_dump_predicates(X) = X ^ dump_predicates := yes.
set_dump_unify_argmodes_struct(X) = X ^ dump_unify_argmodes_struct := yes.
set_dump_goal_purity_markers(X) = X ^ dump_goal_purity_markers := yes.
set_dump_arg_passing_info(X) = X ^ dump_arg_passing_info := yes.
set_dump_mode_constraints(X) = X ^ dump_mode_constraints := yes.
set_dump_clauses(X) = X ^ dump_clauses := yes.
set_dump_goal_instmap_deltas(X) = X ^ dump_goal_instmap_deltas := yes.
set_dump_deep_profiling(X) = X ^ dump_deep_profiling := yes.
set_dump_ctgc(X) = X ^ dump_ctgc := yes.
set_dump_imports(X) = X ^ dump_imports := yes.
set_dump_type_table_only_local(X) = X ^ dump_type_table_only_local := yes.
set_dump_inst_mode_tables(X) = X ^ dump_inst_mode_tables := yes.
set_dump_dependency_ordering(X) = X ^ dump_dependency_ordering := yes.
set_dump_goal_ids_paths(X) = X ^ dump_goal_ids_paths := yes.
set_dump_use_reuse_info(X) = X ^ dump_use_reuse_info := yes.
set_dump_struct_sharing_info(X) = X ^ dump_struct_sharing_info := yes.
set_dump_type_typeclass_tables(X) = X ^ dump_type_typeclass_tables := yes.
set_dump_unify_compare_preds(X) = X ^ dump_unify_compare_preds := yes.
set_dump_cons_table(X) = X ^ dump_cons_table := yes.
set_dump_constant_structures(X) = X ^ dump_constant_structures := yes.
set_dump_structured_insts(X) = X ^ dump_structured_insts := yes.
set_dump_call_answer_tables(X) = X ^ dump_call_answer_tables := yes.

%---------------------------------------------------------------------------%

init_hlds_out_info(Globals, Lang) = Info :-
    globals.lookup_string_option(Globals, dump_hlds_options, DumpOptionsStr),
    DumpOptions = setup_hlds_dump_options(DumpOptionsStr),
    globals.lookup_accumulating_option(Globals, dump_hlds_pred_id, Ids),
    globals.lookup_accumulating_option(Globals, dump_hlds_pred_name, Names),
    MercInfo = init_merc_out_info(Globals, unqualified_item_names, Lang),
    Info = hlds_out_info(DumpOptions, DumpOptions, Ids, Names, MercInfo).

%---------------------------------------------------------------------------%
%
% Write out the ids of predicates and procedures.
%

pred_id_to_user_string(ModuleInfo, PredId) = Str :-
    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    ( if map.search(PredIdTable, PredId, PredInfo) then
        pred_info_get_origin(PredInfo, PredOrigin),
        Str = pred_origin_to_user_string(PredOrigin)
    else
        % The predicate has been deleted, so we print what we can.
        pred_id_to_int(PredId, PredIdInt),
        Str = "deleted predicate " ++ int_to_string(PredIdInt)
    ).

pred_proc_id_to_user_string(ModuleInfo, proc(PredId, ProcId)) =
    pred_proc_id_pair_to_user_string(ModuleInfo, PredId, ProcId).

pred_proc_id_pair_to_user_string(ModuleInfo, PredId, ProcId) = Str :-
    proc_id_to_int(ProcId, ModeNum),
    Str = pred_id_to_user_string(ModuleInfo, PredId)
        ++ " mode " ++ int_to_string(ModeNum).

pred_id_to_dev_string(ModuleInfo, PredId) = Str :-
    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    ( if map.search(PredIdTable, PredId, PredInfo) then
        pred_info_get_origin(PredInfo, PredOrigin),
        Str = pred_origin_to_user_string(PredOrigin)
    else
        % The predicate has been deleted, so we print what we can.
        pred_id_to_int(PredId, PredIdInt),
        Str = "deleted predicate " ++ int_to_string(PredIdInt)
    ).

pred_proc_id_to_dev_string(ModuleInfo, proc(PredId, ProcId)) =
    pred_proc_id_pair_to_dev_string(ModuleInfo, PredId, ProcId).

pred_proc_id_pair_to_dev_string(ModuleInfo, PredId, ProcId) = Str :-
    proc_id_to_int(ProcId, ModeNum),
    Str = pred_id_to_dev_string(ModuleInfo, PredId)
        ++ " mode " ++ int_to_string(ModeNum).

%---------------------------------------------------------------------------%
%
% Write out the contexts of unifications.
%

unify_context_to_pieces(UnifyContext, LastContextWord, !Pieces) :-
    unify_context_first_to_pieces(is_not_first, _, UnifyContext,
        LastContextWord, !Pieces).

unify_context_first_to_pieces(!First, UnifyContext, LastContextWord,
        !Pieces) :-
    UnifyContext = unify_context(MainContext, RevSubContexts),
    list.reverse(RevSubContexts, SubContexts),
    unify_main_context_to_pieces(!First, MainContext,
        LastContextWord0, !Pieces),
    unify_sub_contexts_to_pieces(!First, SubContexts,
        LastContextWord0, LastContextWord, !Pieces).

:- pred unify_main_context_to_pieces(is_first::in, is_first::out,
    unify_main_context::in, last_context_word::out,
    list(format_piece)::in, list(format_piece)::out) is det.

unify_main_context_to_pieces(!First, MainContext, LastContextWord, !Pieces) :-
    (
        MainContext = umc_explicit,
        LastContextWord = lcw_none
    ;
        MainContext = umc_head(ArgNum),
        start_in_message_to_pieces(!.First, !Pieces),
        !:First = is_not_first,
        LastContextWord = lcw_argument,
        ArgNumStr = int_to_string(ArgNum),
        !:Pieces = !.Pieces ++
            [words("argument"), fixed(ArgNumStr), words("of clause head:"), nl]
    ;
        MainContext = umc_head_result,
        start_in_message_to_pieces(!.First, !Pieces),
        !:First = is_not_first,
        LastContextWord = lcw_result,
        !:Pieces = !.Pieces ++
            [words("function result term of clause head:"), nl]
    ;
        MainContext = umc_call(CallId, ArgNum),
        start_in_message_to_pieces(!.First, !Pieces),
        !:First = is_not_first,
        LastContextWord = lcw_call,
        % The markers argument below is used only for type class method
        % implementations defined using the named syntax rather than
        % the clause syntax, and the bodies of such procedures should
        % only contain a single call, so we shouldn't get unifications
        % nested inside calls. Hence we can safely initialize the
        % markers to empty here. (Anyway the worst possible consequence
        % is slightly sub-optimal text for an error message.)
        init_markers(Markers),
        ArgIdPieces = call_arg_id_to_pieces(print_ho_var_name, CallId,
            ArgNum, Markers),
        !:Pieces = !.Pieces ++ ArgIdPieces ++ [suffix(":"), nl]
    ;
        MainContext = umc_implicit(Source),
        LastContextWord = lcw_none,
        start_in_message_to_pieces(!.First, !Pieces),
        string.format("implicit %s unification:", [s(Source)], Msg),
        !:Pieces = !.Pieces ++ [words(Msg), nl]
    ).

:- pred unify_sub_contexts_to_pieces(is_first::in, is_first::out,
    list(unify_sub_context)::in, last_context_word::in, last_context_word::out,
    list(format_piece)::in, list(format_piece)::out) is det.

unify_sub_contexts_to_pieces(!First, [], !LastContextWord, !Pieces).
unify_sub_contexts_to_pieces(!First, [SubContext | SubContexts],
        _, !:LastContextWord, !Pieces) :-
    ( if
        contexts_describe_list_element([SubContext | SubContexts],
            0, ElementNum, AfterContexts)
    then
        in_element_to_pieces(!.First, ElementNum, !Pieces),
        !:First = is_not_first,
        !:LastContextWord = lcw_element,
        unify_sub_contexts_to_pieces(!First, AfterContexts,
            !LastContextWord, !Pieces)
    else
        in_argument_to_pieces(!.First, SubContext, !Pieces),
        !:First = is_not_first,
        !:LastContextWord = lcw_argument,
        unify_sub_contexts_to_pieces(!First, SubContexts,
            !LastContextWord, !Pieces)
    ).

:- pred contexts_describe_list_element(list(unify_sub_context)::in,
    int::in, int::out, list(unify_sub_context)::out) is semidet.

contexts_describe_list_element([SubContext | SubContexts],
        NumElementsBefore, ElementNum, AfterContexts) :-
    SubContext = unify_sub_context(ConsId, ArgNum),
    ConsId = du_data_ctor(du_ctor(Functor, 2, _TypeCtor)),
    % We ignore _TypeCtor since it may not have been set yet.
    (
        Functor = unqualified("[|]")
    ;
        Functor = qualified(ModuleSymName, "[|]"),
        is_std_lib_module_name(ModuleSymName, "list")
    ),
    (
        ArgNum = 1,
        % If there were zero elements before this element,
        % then this is element #1.
        ElementNum = NumElementsBefore + 1,
        AfterContexts = SubContexts
    ;
        ArgNum = 2,
        contexts_describe_list_element(SubContexts,
            NumElementsBefore + 1, ElementNum, AfterContexts)
    ).

:- pred in_argument_to_pieces(is_first::in, unify_sub_context::in,
    list(format_piece)::in, list(format_piece)::out) is det.

in_argument_to_pieces(First, SubContext, !Pieces) :-
    start_in_message_to_pieces(First, !Pieces),
    SubContext = unify_sub_context(ConsId, ArgNum),
    ArgNumStr = int_to_string(ArgNum),
    % XXX Using cons_id_and_arity_to_string here results in the
    % quotes being in the wrong place.
    ConsIdStr = cons_id_and_arity_to_string(ConsId),
    !:Pieces = !.Pieces ++ [words("argument"), fixed(ArgNumStr),
        words("of functor"), quote(ConsIdStr), suffix(":"), nl].

:- pred in_element_to_pieces(is_first::in, int::in,
    list(format_piece)::in, list(format_piece)::out) is det.

in_element_to_pieces(First, ElementNum, !Pieces) :-
    start_in_message_to_pieces(First, !Pieces),
    ElementNumStr = int_to_string(ElementNum),
    !:Pieces = !.Pieces ++ [words("list element"),
        prefix("#"), fixed(ElementNumStr), suffix(":"), nl].

:- pred start_in_message_to_pieces(is_first::in,
    list(format_piece)::in, list(format_piece)::out) is det.

start_in_message_to_pieces(First, !Pieces) :-
    (
        First = is_first,
        % It is possible for First to be yes and !.Pieces to be nonempty,
        % since !.Pieces may contain stuff from before the unify context.
        !:Pieces = !.Pieces ++ [words("In")]
    ;
        First = is_not_first,
        !:Pieces = !.Pieces ++ [words("in")]
    ).

%---------------------------------------------------------------------------%

maybe_output_context_comment(Stream, Indent, Suffix, Context, !IO) :-
    FileName = term_context.context_file(Context),
    LineNumber = term_context.context_line(Context),
    ( if FileName = "" then
        true
    else
        IndentStr = indent2_string(Indent),
        io.format(Stream, "%s%% context: file \"%s\", line %d%s\n",
            [s(IndentStr), s(FileName), i(LineNumber), s(Suffix)], !IO)
    ).

maybe_format_context_comment(Indent, Suffix, Context, !State) :-
    FileName = term_context.context_file(Context),
    LineNumber = term_context.context_line(Context),
    ( if FileName = "" then
        true
    else
        IndentStr = indent2_string(Indent),
        string.builder.format("%s%% context: file \"%s\", line %d%s\n",
            [s(IndentStr), s(FileName), i(LineNumber), s(Suffix)], !State)
    ).

context_to_brief_string(Context) = Str :-
    FileName = term_context.context_file(Context),
    LineNumber = term_context.context_line(Context),
    ( if FileName = "" then
        Str = "dummy context"
    else
        Str = string.format("<%s>:%d", [s(FileName), i(LineNumber)])
    ).

%---------------------------------------------------------------------------%
%
% Write out ids of calls.
%

call_id_to_pieces(_PrintHoVarName, plain_call_id(PFSNA)) =
    [qual_pf_sym_name_pred_form_arity(PFSNA)].
call_id_to_pieces(PrintHoVarName, generic_call_id(VarNameSrc, GenericCall)) =
    generic_call_to_pieces(PrintHoVarName, VarNameSrc, GenericCall).

generic_call_to_pieces(PrintHoVarName, VarNameSrc, GenericCall) = Pieces :-
    (
        GenericCall = higher_order(Var, Purity, PredOrFunc, _, Syntax),
        (
            Syntax = hos_var,
            (
                PrintHoVarName = do_not_print_ho_var_name,
                Pieces = [words("the higher order"), p_or_f(PredOrFunc),
                    words("call")]
            ;
                PrintHoVarName = print_ho_var_name,
                lookup_var_name_in_source(VarNameSrc, Var, VarName),
                Pieces = [words("the higher order call to the"),
                    p_or_f(PredOrFunc), words("variable"), quote(VarName)]
            )
        ;
            Syntax = hos_call_or_apply,
            (
                PredOrFunc = pf_predicate,
                Pieces = [words("the call to the"), quote("call"),
                    words("builtin predicate")]
            ;
                PredOrFunc = pf_function,
                ApplyFuncName = apply_func_name(Purity),
                Pieces = [words("the call to the"), quote(ApplyFuncName),
                    words("builtin function")]
            )
        )
    ;
        GenericCall = class_method(_TCI, _MethodNum, _ClassId, MethodId),
        Pieces = [qual_pf_sym_name_pred_form_arity(MethodId)]
    ;
        GenericCall = event_call(EventName),
        Pieces = [words("event"), words(EventName)]
    ;
        GenericCall = cast(CastType),
        Pieces = [words(cast_type_to_string(CastType))]
    ).

generic_callee_to_pieces(PrintHoVarName, VarNameSrc, GenericCall) = Pieces :-
    (
        GenericCall = higher_order(Var, Purity, PredOrFunc, _, Syntax),
        (
            Syntax = hos_var,
            (
                PrintHoVarName = do_not_print_ho_var_name,
                Pieces = [words("the higher order"), p_or_f(PredOrFunc),
                    words("variable")]
            ;
                PrintHoVarName = print_ho_var_name,
                lookup_var_name_in_source(VarNameSrc, Var, VarName),
                Pieces = [words("the higher order"), p_or_f(PredOrFunc),
                    words("variable"), quote(VarName)]
            )
        ;
            Syntax = hos_call_or_apply,
            (
                PredOrFunc = pf_predicate,
                Pieces = [words("the predicate argument of the"),
                    quote("call"), words("builtin predicate")]
            ;
                PredOrFunc = pf_function,
                Pieces = [words("the function argument of the"),
                    quote(apply_func_name(Purity)), words("builtin function")]
            )
        )
    ;
        GenericCall = class_method(_TCI, _MethodNum, _ClassId, MethodId),
        Pieces = [qual_pf_sym_name_pred_form_arity(MethodId)]
    ;
        GenericCall = event_call(EventName),
        Pieces = [words("event"), words(EventName)]
    ;
        GenericCall = cast(CastType),
        Pieces = [words(cast_type_to_string(CastType))]
    ).

:- func apply_func_name(purity) = string.

apply_func_name(purity_pure) = "apply".
apply_func_name(purity_semipure) = "semipure_apply".
apply_func_name(purity_impure) = "impure_apply".

cast_type_to_string(unsafe_type_cast) = "unsafe_type_cast".
cast_type_to_string(unsafe_type_inst_cast) = "unsafe_type_inst_cast".
cast_type_to_string(equiv_type_cast) = "equiv_type_cast".
cast_type_to_string(exists_cast) = "exists_cast".
cast_type_to_string(subtype_coerce) = "coerce expression".

call_arg_id_to_pieces(PrintHoVarName, CallId, ArgNum, PredMarkers) = Pieces :-
    ( if ArgNum =< 0 then
        % Argument numbers that are less than or equal to zero
        % are used for the type_info and typeclass_info arguments
        % that are introduced by polymorphism.m.
        % I think argument zero might also be used in some other cases
        % when we just don't have any information about which argument it is.
        % For both of these, we just say "in call to"
        % rather than "in argument N of call to".
        ArgNumPieces = []
    else
        ArgNumPieces = arg_number_to_pieces(CallId, ArgNum) ++ [words("of")]
    ),
    ( if
        (
            % The text printed for generic calls other than
            % `class_method' does not need the "call to"
            % prefix ("in call to higher-order call" is redundant,
            % it's much better to just say "in higher-order call").
            CallId = generic_call_id(_, GenericCallId),
            not GenericCallId = class_method(_, _, _, _)
        ;
            % For calls from type class instance implementations
            % that were defined using the named syntax rather
            % than the clause syntax, we also omit the "call to",
            % since in that case there was no explicit call in
            % the user's source code.
            marker_is_present(PredMarkers, marker_named_class_instance_method)
        )
    then
        CallToPieces = []
    else
        CallToPieces = [words("call to")]
    ),
    CallIdPieces = call_id_to_pieces(PrintHoVarName, CallId),
    Pieces = ArgNumPieces ++ CallToPieces ++ CallIdPieces.

:- func arg_number_to_pieces(call_id, int) = list(format_piece).

arg_number_to_pieces(CallId, ArgNum) = Pieces :-
    (
        CallId = plain_call_id(PFSymNameArity),
        PFSymNameArity = pf_sym_name_arity(PredOrFunc, _, PredFormArity),
        PredFormArity = pred_form_arity(Arity),
        ( if
            PredOrFunc = pf_function,
            Arity = ArgNum
        then
            Pieces = [words("the return value")]
        else
            Pieces = [words("argument"), int_fixed(ArgNum)]
        )
    ;
        CallId = generic_call_id(_VarNameSrc, GenericCall),
        (
            GenericCall = higher_order(_Var, _Purity, PredOrFunc,
                PredFormArity, Syntax),
            PredFormArity = pred_form_arity(PredFormArityInt),
            ( if
                PredOrFunc = pf_function,
                ArgNum = PredFormArityInt
            then
                Pieces = [words("the return value")]
            else
                (
                    Syntax = hos_var,
                    ( if ArgNum = 1 then
                        Pieces = [words("the"), p_or_f(PredOrFunc),
                            words("term")]
                    else
                        Pieces = [words("argument"), int_fixed(ArgNum - 1)]
                    )
                ;
                    Syntax = hos_call_or_apply,
                    Pieces = [words("argument"), int_fixed(ArgNum)]
                )
            )
        ;
            ( GenericCall = class_method(_, _, _, _)
            ; GenericCall = event_call(_)
            ; GenericCall = cast(unsafe_type_cast)
            ; GenericCall = cast(unsafe_type_inst_cast)
            ; GenericCall = cast(equiv_type_cast)
            ; GenericCall = cast(exists_cast)
            ),
            Pieces = [words("argument"), int_fixed(ArgNum)]
        ;
            GenericCall = cast(subtype_coerce),
            ( if ArgNum = 2 then
                Pieces = [words("the result")]
            else
                Pieces = [words("the argument")]
            )
        )
    ).

%---------------------------------------------------------------------------%
%
% Write out functors.
%

functor_to_string(VarNameSrc, VarNamePrint, Functor, ArgVars)  =
    functor_to_string_maybe_needs_quotes(VarNameSrc, VarNamePrint,
        not_next_to_graphic_token, Functor, ArgVars).

functor_to_string_maybe_needs_quotes(VarNameSrc, VarNamePrint,
        NextToGraphicToken, Functor, ArgVars) = Str :-
    term_subst.var_list_to_term_list(ArgVars, ArgTerms),
    Term = term.functor(Functor, ArgTerms, dummy_context),
    Str = mercury_term_nq_to_string_src(VarNameSrc, VarNamePrint,
        NextToGraphicToken, Term).

qualified_functor_to_string(VarNameSrc, VarNamePrint, ModuleName, Functor,
        ArgVars) = Str :-
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    FunctorStr = functor_to_string_maybe_needs_quotes(VarNameSrc, VarNamePrint,
        next_to_graphic_token, Functor, ArgVars),
    Str = ModuleNameStr ++ "." ++ FunctorStr.

qualified_functor_with_term_args_to_string(VarNameSrc, VarNamePrint,
        ModuleName, Functor, ArgTerms) = Str :-
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    Term = term.functor(Functor, ArgTerms, dummy_context),
    TermStr = mercury_term_nq_to_string_src(VarNameSrc, VarNamePrint,
        next_to_graphic_token, Term),
    Str = ModuleNameStr ++ "." ++ TermStr.

functor_cons_id_to_string(ModuleInfo, VarNameSrc, VarNamePrint,
        ConsId, ArgVars) = Str :-
    (
        ConsId = du_data_ctor(du_ctor(SymName, _, _)),
        (
            SymName = qualified(Module, Name),
            Str = qualified_functor_to_string(VarNameSrc, VarNamePrint,
                Module, term.atom(Name), ArgVars)
        ;
            SymName = unqualified(Name),
            Str = functor_to_string_maybe_needs_quotes(VarNameSrc,
                VarNamePrint, next_to_graphic_token, term.atom(Name), ArgVars)
        )
    ;
        ConsId = tuple_cons(_),
        Str = functor_to_string_maybe_needs_quotes(VarNameSrc, VarNamePrint,
            next_to_graphic_token, term.atom("{}"), ArgVars)
    ;
        ConsId = some_int_const(IntConst),
        Str = int_const_to_string_with_suffix(IntConst)
    ;
        ConsId = float_const(Float),
        Str = functor_to_string(VarNameSrc, VarNamePrint,
            term.float(Float), ArgVars)
    ;
        ConsId = char_const(Char),
        % XXX The strings ('z') and ('\n') should always denote
        % the last letter of the alphabet and the newline character
        % respectively. We need to decide whether forms such as (z)
        % and 'z' should acceptable too. I (zs) think that 'z' should
        % be acceptable to the scanner and parser (which currently it isn't),
        % but (z) should not be.
        Str = "(" ++ term_io.quoted_char_to_string(Char) ++ ")"
    ;
        ConsId = string_const(String),
        Str = functor_to_string(VarNameSrc, VarNamePrint,
            term.string(String), ArgVars)
    ;
        ConsId = impl_defined_const(IDCKind),
        Str = impl_defined_const_kind_to_str(IDCKind)
    ;
        ConsId = closure_cons(ShroudedPredProcId),
        proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        PredModule = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        PredSymName = qualified(PredModule, PredName),
        PredConsId = du_data_ctor(du_ctor(PredSymName, list.length(ArgVars),
            cons_id_dummy_type_ctor)),
        Str = functor_cons_id_to_string(ModuleInfo, VarNameSrc, VarNamePrint,
            PredConsId, ArgVars)
    ;
        ConsId = type_ctor_info_const(Module, Name, Arity),
        Str = string.format("type_ctor_info(%s, %s, %d)",
            [s(escaped_sym_name_to_string(Module)), s(Name), i(Arity)])
    ;
        ConsId = base_typeclass_info_const(Module, ClassId, _, Instance),
        ClassId = class_id(Name, Arity),
        ClassIdStr = string.format("class_id(%s, %d)",
            [s(escaped_sym_name_to_string(Name)), i(Arity)]),
        Str = string.format("base_typeclass_info(%s, %s, %s)",
            [s(escaped_sym_name_to_string(Module)),
            s(ClassIdStr), s(Instance)])
    ;
        ConsId = type_info_cell_constructor(_),
        Str = functor_to_string_maybe_needs_quotes(VarNameSrc, VarNamePrint,
            next_to_graphic_token,
            term.atom("type_info_cell_constructor"), ArgVars)
    ;
        ConsId = typeclass_info_cell_constructor,
        Str = functor_to_string_maybe_needs_quotes(VarNameSrc, VarNamePrint,
            next_to_graphic_token,
            term.atom("typeclass_info_cell_constructor"), ArgVars)
    ;
        ConsId = type_info_const(TIConstNum),
        Str = string.format("type_info_const(%d)", [i(TIConstNum)])
    ;
        ConsId = typeclass_info_const(TCIConstNum),
        Str = string.format("typeclass_info_const(%d)", [i(TCIConstNum)])
    ;
        ConsId = ground_term_const(ConstNum, SubConsId),
        SubStr = functor_cons_id_to_string(ModuleInfo, VarNameSrc,
            VarNamePrint, SubConsId, []),
        Str = string.format("ground_term_const(%d, %s)",
            [i(ConstNum), s(SubStr)])
    ;
        ConsId = tabling_info_const(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        proc_id_to_int(ProcId, ProcIdInt),
        Str = string.format("tabling_info_const(%s, mode %d)",
            [s(pred_id_to_dev_string(ModuleInfo, PredId)), i(ProcIdInt)])
    ;
        ConsId = table_io_entry_desc(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        proc_id_to_int(ProcId, ProcIdInt),
        Str = string.format("table_io_entry_desc(%s, mode %d)",
            [s(pred_id_to_dev_string(ModuleInfo, PredId)), i(ProcIdInt)])
    ;
        ConsId = deep_profiling_proc_layout(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        proc_id_to_int(ProcId, ProcIdInt),
        Str = string.format("deep_profiling_proc_layout(%s mode %d)",
            [s(pred_id_to_dev_string(ModuleInfo, PredId)), i(ProcIdInt)])
    ).

cons_id_and_vars_or_arity_to_string(VarTable, Qual, ConsId, MaybeArgVars)
        = String :-
    (
        ConsId = du_data_ctor(du_ctor(SymName0, Arity, _TypeCtor)),
        (
            Qual = qualify_cons_id,
            SymName = SymName0
        ;
            Qual = do_not_qualify_cons_id,
            SymName = unqualified(unqualify_name(SymName0))
        ),
        SymNameString0 = sym_name_to_string(SymName),
        ( if string.contains_char(SymNameString0, '*') then
            % We need to protect against the * appearing next to a /
            Stuff = (pred(Char::in, Str0::in, Str::out) is det :-
                ( if Char = ('*') then
                    string.append(Str0, "star", Str)
                else
                    string.char_to_string(Char, CharStr),
                    string.append(Str0, CharStr, Str)
                )
            ),
            string.foldl(Stuff, SymNameString0, "", SymNameString1)
        else
            SymNameString1 = SymNameString0
        ),
        SymNameString = term_io.escaped_string(SymNameString1),
        (
            MaybeArgVars = no,
            String = SymNameString ++ "/" ++ string.int_to_string(Arity)
        ;
            MaybeArgVars = yes(ArgVars),
            (
                ArgVars = [],
                String = SymNameString ++ "/" ++ string.int_to_string(Arity)
            ;
                ArgVars = [_ | _],
                ArgStr = mercury_vars_to_name_only(VarTable, ArgVars),
                String = SymNameString ++ "(" ++ ArgStr ++ ")"
            )
        )
    ;
        ConsId = tuple_cons(Arity),
        (
            MaybeArgVars = no,
            String = "{}/" ++ string.int_to_string(Arity)
        ;
            MaybeArgVars = yes(ArgVars),
            (
                ArgVars = [],
                String = "{}/" ++ string.int_to_string(Arity)
            ;
                ArgVars = [_ | _],
                ArgStr = mercury_vars_to_name_only(VarTable, ArgVars),
                String = "{" ++ ArgStr ++ "}"
            )
        )
    ;
        ConsId = some_int_const(IntConst),
        String = int_const_to_string_with_suffix(IntConst)
    ;
        ConsId = float_const(Float),
        String = float_to_string(Float)
    ;
        ConsId = char_const(CharConst),
        String = term_io.quoted_char_to_string(CharConst)
    ;
        ConsId = string_const(StringConst),
        String = term_io.quoted_string(StringConst)
    ;
        ConsId = impl_defined_const(IDCKind),
        String = impl_defined_const_kind_to_str(IDCKind)
    ;
        ConsId = closure_cons(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<pred " ++ int_to_string(PredId) ++
            " proc " ++ int_to_string(ProcId) ++ ">"
    ;
        ConsId = type_ctor_info_const(Module, Ctor, Arity),
        String =
            "<type_ctor_info " ++ sym_name_to_string(Module) ++ "." ++
            Ctor ++ "/" ++ int_to_string(Arity) ++ ">"
    ;
        ConsId = base_typeclass_info_const(_, _, _, _),
        String = "<base_typeclass_info>"
    ;
        ConsId = type_info_cell_constructor(_),
        String = "<type_info_cell_constructor>"
    ;
        ConsId = typeclass_info_cell_constructor,
        String = "<typeclass_info_cell_constructor>"
    ;
        ConsId = type_info_const(_),
        String = "<type_info_const>"
    ;
        ConsId = typeclass_info_const(_),
        String = "<typeclass_info_const>"
    ;
        ConsId = ground_term_const(_, _),
        String = "<ground_term_const>"
    ;
        ConsId = tabling_info_const(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<tabling_info " ++ int_to_string(PredId) ++
            ", " ++ int_to_string(ProcId) ++ ">"
    ;
        ConsId = table_io_entry_desc(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<table_io_entry_desc " ++ int_to_string(PredId) ++ ", " ++
            int_to_string(ProcId) ++ ">"
    ;
        ConsId = deep_profiling_proc_layout(PredProcId),
        PredProcId = shrouded_pred_proc_id(PredId, ProcId),
        String =
            "<deep_profiling_proc_layout " ++ int_to_string(PredId) ++ ", " ++
            int_to_string(ProcId) ++ ">"
    ).

:- func int_const_to_string_with_suffix(some_int_const) = string.

int_const_to_string_with_suffix(IntConst) = Str :-
    int_const_to_string_and_suffix(IntConst, BaseStr, Suffix),
    Str = BaseStr ++ Suffix.

%---------------------------------------------------------------------------%
%
% Write out constraint proofs.
%

write_constraint_proof_map(Stream, Indent, VarNamePrint, TVarSet,
        ProofMap, !IO) :-
    State0 = string.builder.init,
    format_constraint_proof_map(Indent, VarNamePrint, TVarSet, ProofMap,
        State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_constraint_proof_map(Indent, VarNamePrint, TVarSet, ProofMap, !State) :-
    map.to_assoc_list(ProofMap, ProofsList),
    IndentStr = indent2_string(Indent),
    string.builder.format("%s%% Proofs:\n", [s(IndentStr)], !State),
    list.foldl(
        format_constraint_proof(IndentStr, VarNamePrint, TVarSet),
        ProofsList, !State).

:- pred format_constraint_proof(string::in, var_name_print::in, tvarset::in,
    pair(prog_constraint, constraint_proof)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_constraint_proof(IndentStr, VarNamePrint, TVarSet,
        Constraint - Proof, !State) :-
    ConstraintStr = mercury_constraint_to_string(TVarSet, VarNamePrint,
        Constraint),
    string.builder.format("%s%% %s: ",
        [s(IndentStr), s(ConstraintStr)], !State),
    (
        Proof = apply_instance(instance_id(InstanceNum)),
        string.builder.format("apply instance decl #%d\n",
            [i(InstanceNum)], !State)
    ;
        Proof = superclass(Super),
        SuperStr = mercury_constraint_to_string(TVarSet, VarNamePrint, Super),
        string.builder.format("super class of %s\n", [s(SuperStr)], !State)
    ).

%---------------------------------------------------------------------------%
%
% Write out modes.
%

var_modes_to_string(Lang, VarNameSrc, InstVarSet, VarNamePrint, VarModes)
        = Str :-
    Strs = list.map(
        var_mode_to_string(Lang, VarNameSrc, InstVarSet, VarNamePrint),
        VarModes),
    Str = string.join_list(", ", Strs).

var_mode_to_string(Lang, VarNameSrc, InstVarSet, VarNamePrint, Var - Mode) =
    mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var) ++ "::" ++
        mercury_mode_to_string(Lang, InstVarSet, Mode).

%---------------------------------------------------------------------------%
%
% Write out statuses.
%

type_import_status_to_string(type_status(OldImportStatus)) =
    old_import_status_to_string(OldImportStatus).
inst_import_status_to_string(inst_status(InstModeStatus)) =
    instmode_status_to_string(InstModeStatus).
mode_import_status_to_string(mode_status(InstModeStatus)) =
    instmode_status_to_string(InstModeStatus).
typeclass_import_status_to_string(typeclass_status(OldImportStatus)) =
    old_import_status_to_string(OldImportStatus).
instance_import_status_to_string(instance_status(OldImportStatus)) =
    old_import_status_to_string(OldImportStatus).
pred_import_status_to_string(pred_status(OldImportStatus)) =
    old_import_status_to_string(OldImportStatus).

:- func instmode_status_to_string(new_instmode_status) = string.

instmode_status_to_string(InstModeStatus) = Str :-
    (
        InstModeStatus = instmode_defined_in_this_module(InstModeExport),
        (
            InstModeExport = instmode_export_nowhere,
            Str = "this_module(export_nowhere)"
        ;
            InstModeExport = instmode_export_only_submodules,
            Str = "this_module(export_only_submodules)"
        ;
            InstModeExport = instmode_export_anywhere,
            Str = "this_module(export_anywhere)"
        )
    ;
        InstModeStatus = instmode_defined_in_other_module(InstModeImport),
        (
            InstModeImport = instmode_import_plain,
            Str = "other_module(import_plain)"
        ;
            InstModeImport = instmode_import_abstract,
            Str = "other_module(import_abstract)"
        ;
            InstModeImport = instmode_import_opt,
            Str = "other_module(import_opt)"
        )
    ).

:- func old_import_status_to_string(old_import_status) = string.

old_import_status_to_string(status_local) =
    "local".
old_import_status_to_string(status_exported) =
    "exported".
old_import_status_to_string(status_opt_exported) =
    "opt_exported".
old_import_status_to_string(status_abstract_exported) =
    "abstract_exported".
old_import_status_to_string(status_pseudo_exported) =
    "pseudo_exported".
old_import_status_to_string(status_imported(import_locn_interface)) =
    "imported in the interface".
old_import_status_to_string(status_imported(import_locn_implementation)) =
    "imported in the implementation".
old_import_status_to_string(status_imported(
        import_locn_ancestor_int0_interface)) =
    "imported by an ancestor in its interface".
old_import_status_to_string(status_imported(
        import_locn_ancestor_int0_implementation)) =
    "imported by an ancestor in its implementation".
old_import_status_to_string(status_imported(import_locn_import_by_ancestor)) =
    "imported by a module imported by an ancestor".
old_import_status_to_string(status_external(Status)) =
    "external (and " ++ old_import_status_to_string(Status) ++ ")".
old_import_status_to_string(status_abstract_imported) =
    "abstract_imported".
old_import_status_to_string(status_opt_imported) =
    "opt_imported".
old_import_status_to_string(status_pseudo_imported) =
    "pseudo_imported".
old_import_status_to_string(status_exported_to_submodules) =
    "exported_to_submodules".

%---------------------------------------------------------------------------%
%
% Write out lists of integers as Mercury terms.
%

write_intlist(Stream, IntList, !IO) :-
    State0 = string.builder.init,
    format_intlist(IntList, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_intlist(IntList, !State) :-
    (
        IntList = [],
        string.builder.append_string("[]", !State)
    ;
        IntList = [H | T],
        string.builder.append_string("[", !State),
        format_intlist_lag(H, T, !State),
        string.builder.append_string("]", !State)
    ).

:- pred format_intlist_lag(int::in, list(int)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_intlist_lag(H, T, !State) :-
    string.builder.append_string(string.int_to_string(H), !State),
    (
        T = [TH | TT],
        string.builder.append_string(", ", !State),
        format_intlist_lag(TH, TT, !State)
    ;
        T = []
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_util.
%---------------------------------------------------------------------------%
