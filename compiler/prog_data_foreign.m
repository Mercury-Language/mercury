%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines the types that represent information related to
% foreign languages in the parse tree.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_data_foreign.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module list.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%
%
% Stuff for the foreign language interface pragmas.
%

:- interface.

    % Is the foreign code declarations local to this module or
    % exported?
    %
:- type foreign_decl_is_local
    --->    foreign_decl_is_local
    ;       foreign_decl_is_exported.

:- type foreign_literal_or_include
    --->    floi_literal(string)
    ;       floi_include_file(
                string      % The file name written in the source code.
            ).

%---------------------------------------------------------------------------%
%
% Stuff for `foreign_import_module' pragma.
%

:- interface.

    % For each foreign language, we store the set of modules we need to import
    % in that language.
    %
    % C++ is commented out while lang_cplusplus is commented out
    % in the foreign_language type.
:- type c_j_cs_fims
    --->    c_j_cs_fims(
                fim_c           :: set(module_name),
                % fim_cplusplus :: set(module_name),
                fim_java        :: set(module_name),
                fim_csharp      :: set(module_name)
            ).

:- type fim_spec
    --->    fim_spec(
                % The specification of a foreign_import_module declaration,
                % without information about where it came from, used where
                % it *does not matter* where it came from.
                fimspec_lang                    :: foreign_language,
                fimspec_module_name             :: module_name
            ).

:- func init_foreign_import_modules = c_j_cs_fims.

:- pred add_fim(foreign_language::in, module_name::in,
    c_j_cs_fims::in, c_j_cs_fims::out) is det.
:- pred add_fim_for_module(module_name::in, foreign_language::in,
    c_j_cs_fims::in, c_j_cs_fims::out) is det.

:- pred add_fim_spec(fim_spec::in,
    c_j_cs_fims::in, c_j_cs_fims::out) is det.

:- func get_all_fim_specs(c_j_cs_fims) = set(fim_spec).

:- func get_all_foreign_import_modules(c_j_cs_fims) = set(module_name).

:- func get_lang_fim_specs(c_j_cs_fims, foreign_language) = set(fim_spec).

:- func get_lang_fim_modules(c_j_cs_fims, foreign_language)
    = set(module_name).

:- implementation.

init_foreign_import_modules =
    c_j_cs_fims(set.init, set.init, set.init).

add_fim(Lang, ModuleName, !FIM) :-
    (
        Lang = lang_c,
        ModuleNames0 = !.FIM ^ fim_c,
        ( if set.insert_new(ModuleName, ModuleNames0, ModuleNames) then
            !FIM ^ fim_c := ModuleNames
        else
            true
        )
    ;
        Lang = lang_csharp,
        ModuleNames0 = !.FIM ^ fim_csharp,
        ( if set.insert_new(ModuleName, ModuleNames0, ModuleNames) then
            !FIM ^ fim_csharp := ModuleNames
        else
            true
        )
    ;
        Lang = lang_java,
        ModuleNames0 = !.FIM ^ fim_java,
        ( if set.insert_new(ModuleName, ModuleNames0, ModuleNames) then
            !FIM ^ fim_java := ModuleNames
        else
            true
        )
    ).

add_fim_for_module(Lang, ModuleName, !FIM) :-
    add_fim(ModuleName, Lang, !FIM).

add_fim_spec(FIMSpec, !FIM) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    add_fim(Lang, ModuleName, !FIM).

get_all_fim_specs(FIM) = FIMSpecs :-
    FIM = c_j_cs_fims(ModuleNamesC, ModuleNamesJava, ModuleNamesCSharp),
    FIMSpecs = set.union_list([
        set.map(make_fim_spec(lang_c), ModuleNamesC),
        set.map(make_fim_spec(lang_java), ModuleNamesJava),
        set.map(make_fim_spec(lang_csharp), ModuleNamesCSharp)
        ]).

get_all_foreign_import_modules(FIM) = ModuleNames :-
    FIM = c_j_cs_fims(ModuleNamesC, ModuleNamesJava,
        ModuleNamesCSharp),
    ModuleNames = set.union_list([ModuleNamesC, ModuleNamesJava,
        ModuleNamesCSharp]).

get_lang_fim_specs(FIM, Lang) = ImportInfos :-
    ModuleNames = get_lang_fim_modules(FIM, Lang),
    ImportInfos = set.map(make_fim_spec(Lang), ModuleNames).

get_lang_fim_modules(FIM, Lang) = ModuleNames :-
    (
        Lang = lang_c,
        ModuleNames = FIM ^ fim_c
    ;
        Lang = lang_csharp,
        ModuleNames = FIM ^ fim_csharp
    ;
        Lang = lang_java,
        ModuleNames = FIM ^ fim_java
    ).

:- func make_fim_spec(foreign_language, module_name) = fim_spec.

make_fim_spec(Lang, ModuleName) = fim_spec(Lang, ModuleName).

%---------------------------------------------------------------------------%
%
% Stuff for the `foreign_decl' and `foreign_code' pragmas.
%

:- interface.

:- type foreign_include_file_info
    --->    foreign_include_file_info(
                fifi_lang       :: foreign_language,
                fifi_filename   :: string
            ).

%---------------------------------------------------------------------------%
%
% Stuff for the `foreign_export_enum' pragma.
%

:- interface.

:- type uppercase_export_enum
    --->    uppercase_export_enum
    ;       do_not_uppercase_export_enum.

:- type export_enum_attributes
    --->    export_enum_attributes(
                ee_attr_prefix :: maybe(string),
                ee_attr_upper  :: uppercase_export_enum
            ).

:- func default_export_enum_attributes = export_enum_attributes.

:- implementation.

default_export_enum_attributes =
    export_enum_attributes(no, do_not_uppercase_export_enum).

%---------------------------------------------------------------------------%
%
% Stuff for `foreign_proc' pragma.
%

:- interface.

    % This type holds information about the implementation details
    % of procedures defined via `pragma foreign_proc'.
    %
    % All the strings in this type may be accompanied by the context of their
    % appearance in the source code. These contexts are used to tell the
    % foreign language compiler where the included code comes from, to allow it
    % to generate error messages that refer to the original appearance of the
    % code in the Mercury program. The context is missing if the foreign code
    % was constructed by the compiler.
    %
:- type pragma_foreign_proc_impl
    --->    fp_impl_ordinary(
                % This is a foreign language definition of a model_det or
                % model_semi procedure. (We used to allow model_non, but
                % do not any more.)

                string,             % The code of the procedure.
                maybe(prog_context)
            ).

    % The use of this type is explained in the comment at the top of
    % pragma_c_gen.m.
    %
:- type foreign_proc_shared_code_treatment
    --->    shared_code_duplicate
    ;       shared_code_share
    ;       shared_code_automatic.

    % An abstract type for representing a set of
    % `pragma_foreign_proc_attribute's.
    %
:- type foreign_proc_attributes.

:- func default_attributes(foreign_language) = foreign_proc_attributes.

:- func get_foreign_language(foreign_proc_attributes) =
    foreign_language.
:- func get_may_call_mercury(foreign_proc_attributes) =
    proc_may_call_mercury.
:- func get_thread_safe(foreign_proc_attributes) = proc_thread_safe.
:- func get_tabled_for_io(foreign_proc_attributes)
    = proc_tabled_for_io.
:- func get_purity(foreign_proc_attributes) = purity.
:- func get_terminates(foreign_proc_attributes) = proc_terminates.
:- func get_user_annotated_sharing(foreign_proc_attributes)
    = user_annotated_sharing.
:- func get_ordinary_despite_detism(foreign_proc_attributes)
    = maybe_ordinary_despite_detism.
:- func get_may_throw_exception(foreign_proc_attributes)
    = proc_may_throw_exception.
:- func get_may_modify_trail(foreign_proc_attributes)
    = proc_may_modify_trail.
:- func get_may_call_mm_tabled(foreign_proc_attributes)
    = proc_may_call_mm_tabled.
:- func get_box_policy(foreign_proc_attributes) = box_policy.
:- func get_affects_liveness(foreign_proc_attributes)
    = proc_affects_liveness.
:- func get_allocates_memory(foreign_proc_attributes)
    = proc_allocates_memory.
:- func get_registers_roots(foreign_proc_attributes)
    = proc_registers_roots.
:- func get_refers_to_llds_stack(foreign_proc_attributes)
    = maybe_refers_to_llds_stack.
:- func get_call_std_out_regs(foreign_proc_attributes)
    = maybe_call_std_out_regs.
:- func get_may_duplicate(foreign_proc_attributes)
    = maybe(proc_may_duplicate).
:- func get_may_export_body(foreign_proc_attributes)
    = maybe(proc_may_export_body).
:- func get_for_specific_backend(foreign_proc_attributes)
    = maybe(backend).

:- pred set_may_call_mercury(proc_may_call_mercury::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_thread_safe(proc_thread_safe::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_foreign_language(foreign_language::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_tabled_for_io(proc_tabled_for_io::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_purity(purity::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_terminates(proc_terminates::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_user_annotated_sharing(user_annotated_sharing::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_ordinary_despite_detism(maybe_ordinary_despite_detism::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_may_throw_exception(proc_may_throw_exception::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_may_modify_trail(proc_may_modify_trail::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_may_call_mm_tabled(proc_may_call_mm_tabled::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_box_policy(box_policy::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_affects_liveness(proc_affects_liveness::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_allocates_memory(proc_allocates_memory::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_registers_roots(proc_registers_roots::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_refers_to_llds_stack(maybe_refers_to_llds_stack::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_call_std_out_regs(maybe_call_std_out_regs::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_may_duplicate(maybe(proc_may_duplicate)::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_may_export_body(maybe(proc_may_export_body)::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.
:- pred set_for_specific_backend(maybe(backend)::in,
    foreign_proc_attributes::in,
    foreign_proc_attributes::out) is det.

    % For foreign_procs, there are two different calling conventions,
    % one for foreign code that may recursively call Mercury code, and another
    % more efficient one for the case when we know that the foreign code will
    % not recursively invoke Mercury code.
:- type proc_may_call_mercury
    --->    proc_may_call_mercury
    ;       proc_will_not_call_mercury.

    % If thread_safe execution is enabled, then we need to put a mutex
    % around the foreign code for each foreign_proc, unless it is declared
    % to be thread_safe. If a piece of foreign code is declared to be
    % maybe_thread_safe whether we put the mutex around the foreign code
    % depends upon the `--maybe-thread-safe' compiler flag.
    %
:- type proc_thread_safe
    --->    proc_not_thread_safe
    ;       proc_thread_safe
    ;       proc_maybe_thread_safe.

:- type proc_tabled_for_io
    --->    proc_not_tabled_for_io
    ;       proc_tabled_for_io
    ;       proc_tabled_for_io_unitize
    ;       proc_tabled_for_descendant_io.

:- type proc_may_modify_trail
    --->    proc_may_modify_trail
    ;       proc_will_not_modify_trail.

:- type proc_may_call_mm_tabled
    --->    proc_may_call_mm_tabled
            % The foreign code may make callbacks to minimal model tabled
            % procedures.

    ;       proc_will_not_call_mm_tabled
            % The foreign code may make callbacks to Mercury, but they will
            % not be to minimal model tabled code.

    ;       proc_default_calls_mm_tabled.
            % If either of the above are not specified:
            % - for `will_not_call_mercury' set `proc_will_not_call_mm_tabled'
            % - for `may_call_mercury' set `proc_may_call_mm_tabled'

:- type pragma_var
    --->    pragma_var(prog_var, string, mer_mode, box_policy).
            % variable, name, mode
            % We explicitly store the name because we need the real
            % name in code_gen.

:- type foreign_arg_name_mode
    --->    foreign_arg_name_mode(string, mer_mode).

    % box_policy only makes sense in high-level C grades using low-level data.
    %
:- type box_policy
    --->    bp_native_if_possible
    ;       bp_always_boxed.

:- type foreign_arg_name_mode_box
    --->    foreign_arg_name_mode_box(
                maybe(foreign_arg_name_mode),
                box_policy
            ).

:- func foreign_arg_name_mode_box_project_maybe_name_mode(
    foreign_arg_name_mode_box) = maybe(foreign_arg_name_mode).

    % Extract the modes from the list of pragma_vars.
    %
:- pred pragma_get_modes(list(pragma_var)::in, list(mer_mode)::out) is det.

    % Extract the names from the list of pragma_vars.
    %
:- pred pragma_get_var_infos(list(pragma_var)::in,
    list(foreign_arg_name_mode_box)::out) is det.

    % Extract the vars and the names from the list of pragma_vars.
    %
:- pred pragma_get_vars_and_var_infos(list(pragma_var)::in,
    list(prog_var)::out, list(foreign_arg_name_mode_box)::out) is det.

:- type maybe_ordinary_despite_detism
    --->    not_ordinary_despite_detism
    ;       ordinary_despite_detism.

:- type proc_affects_liveness
    --->    proc_affects_liveness
    ;       proc_does_not_affect_liveness
    ;       proc_default_affects_liveness.

:- type proc_allocates_memory
    --->    proc_does_not_allocate_memory
    ;       proc_allocates_bounded_memory
    ;       proc_allocates_unbounded_memory
    ;       proc_default_allocates_memory.

:- type proc_registers_roots
    --->    proc_registers_roots
    ;       proc_does_not_register_roots
    ;       proc_does_not_have_roots
    ;       proc_default_registers_roots.

:- type proc_may_duplicate
    --->    proc_may_duplicate
    ;       proc_may_not_duplicate.

:- type proc_may_export_body
    --->    proc_may_export_body
    ;       proc_may_not_export_body.

    % This type specifies the termination property of a procedure
    % defined using pragma foreign_proc.
    %
:- type proc_terminates
    --->    proc_terminates
            % The foreign code will terminate for all input assuming
            % that any input streams are finite.

    ;       proc_does_not_terminate
            % The foreign code will not necessarily terminate for some
            % (possibly all) input.

    ;       depends_on_mercury_calls.
            % The termination of the foreign code depends on whether the code
            % makes calls back to Mercury (See termination.m for details).

:- type proc_may_throw_exception
    --->    proc_will_not_throw_exception
            % The foreign code will not result in an exception being thrown.

    ;       default_exception_behaviour.
            % If the foreign_proc is erroneous then mark it as throwing an
            % exception. Otherwise mark it as throwing an exception if it
            % makes calls back to Mercury and not throwing an exception
            % otherwise.

:- type maybe_refers_to_llds_stack
    --->    does_not_refer_to_llds_stack
    ;       refers_to_llds_stack.

:- type maybe_call_std_out_regs
    --->    no_request_for_call_std_out_regs
    ;       needs_call_std_out_regs.
            % On the LLDS backend, this foreign_proc needs to put its outputs
            % into the same registers as if it were a call. This is useful
            % if the code of the foreign procedure being invoked can suspend
            % for a while, resume at a label in the runtime system, and then
            % return from code at that label. The code that places the outputs
            % must put them where calls expect them, but without this
            % attribute, the LLDS code generator could try to put the output
            % somewhere else.

%---------------------------------------------------------------------------%

:- implementation.

    % If you add an attribute, you may need to modify
    % `foreign_proc_attributes_to_strings'.
    %
:- type foreign_proc_attributes
    --->    foreign_proc_attributes(
                attr_foreign_language           :: foreign_language,
                attr_may_call_mercury           :: proc_may_call_mercury,
                attr_thread_safe                :: proc_thread_safe,
                attr_tabled_for_io              :: proc_tabled_for_io,
                attr_purity                     :: purity,
                attr_terminates                 :: proc_terminates,
                attr_user_annotated_sharing     :: user_annotated_sharing,
                % The ordinary_despite_detism attribute is not publicly
                % documented, but it is used in the implementation of
                % catch_impl for C# and Java in library/exception.m.
                attr_ordinary_despite_detism    ::
                                            maybe_ordinary_despite_detism,
                attr_may_throw_exception        :: proc_may_throw_exception,
                attr_may_modify_trail           :: proc_may_modify_trail,
                attr_may_call_mm_tabled         :: proc_may_call_mm_tabled,
                attr_box_policy                 :: box_policy,
                attr_affects_liveness           :: proc_affects_liveness,
                attr_allocates_memory           :: proc_allocates_memory,
                attr_registers_roots            :: proc_registers_roots,
                attr_refers_to_llds_stack       :: maybe_refers_to_llds_stack,
                attr_call_std_out_regs          :: maybe_call_std_out_regs,
                attr_may_duplicate              :: maybe(proc_may_duplicate),
                attr_may_export_body            :: maybe(proc_may_export_body),
                attr_for_specific_backend       :: maybe(backend)
            ).

default_attributes(Language) =
    foreign_proc_attributes(Language,
        proc_may_call_mercury, proc_not_thread_safe,
        proc_not_tabled_for_io, purity_impure, depends_on_mercury_calls,
        no_user_annotated_sharing, not_ordinary_despite_detism,
        default_exception_behaviour, proc_may_modify_trail,
        proc_default_calls_mm_tabled, bp_native_if_possible,
        proc_default_affects_liveness, proc_default_allocates_memory,
        proc_default_registers_roots, does_not_refer_to_llds_stack,
        no_request_for_call_std_out_regs, no, no, no).

get_foreign_language(Attrs) = Attrs ^ attr_foreign_language.
get_may_call_mercury(Attrs) = Attrs ^ attr_may_call_mercury.
get_thread_safe(Attrs) = Attrs ^ attr_thread_safe.
get_tabled_for_io(Attrs) = Attrs ^ attr_tabled_for_io.
get_purity(Attrs) = Attrs ^ attr_purity.
get_terminates(Attrs) = Attrs ^ attr_terminates.
get_user_annotated_sharing(Attrs) = Attrs ^ attr_user_annotated_sharing.
get_ordinary_despite_detism(Attrs) = Attrs ^ attr_ordinary_despite_detism.
get_may_throw_exception(Attrs) = Attrs ^ attr_may_throw_exception.
get_may_modify_trail(Attrs) = Attrs ^ attr_may_modify_trail.
get_may_call_mm_tabled(Attrs) = Attrs ^ attr_may_call_mm_tabled.
get_box_policy(Attrs) = Attrs ^ attr_box_policy.
get_affects_liveness(Attrs) = Attrs ^ attr_affects_liveness.
get_allocates_memory(Attrs) = Attrs ^ attr_allocates_memory.
get_registers_roots(Attrs) = Attrs ^ attr_registers_roots.
get_refers_to_llds_stack(Attrs) = Attrs ^ attr_refers_to_llds_stack.
get_call_std_out_regs(Attrs) = Attrs ^ attr_call_std_out_regs.
get_may_duplicate(Attrs) = Attrs ^ attr_may_duplicate.
get_may_export_body(Attrs) = Attrs ^ attr_may_export_body.
get_for_specific_backend(Attrs) = Attrs ^ attr_for_specific_backend.

set_may_call_mercury(X, !Attrs) :-
    !Attrs ^ attr_may_call_mercury := X.
set_thread_safe(X, !Attrs) :-
    !Attrs ^ attr_thread_safe := X.
set_foreign_language(X, !Attrs) :-
    !Attrs ^ attr_foreign_language := X.
set_tabled_for_io(X, !Attrs) :-
    !Attrs ^ attr_tabled_for_io := X.
set_purity(X, !Attrs) :-
    !Attrs ^ attr_purity := X.
set_terminates(X, !Attrs) :-
    !Attrs ^ attr_terminates := X.
set_user_annotated_sharing(X, !Attrs) :-
    !Attrs ^ attr_user_annotated_sharing := X.
set_ordinary_despite_detism(X, !Attrs) :-
    !Attrs ^ attr_ordinary_despite_detism := X.
set_may_throw_exception(X, !Attrs) :-
    !Attrs ^ attr_may_throw_exception := X.
set_may_modify_trail(X, !Attrs) :-
    !Attrs ^ attr_may_modify_trail := X.
set_may_call_mm_tabled(X, !Attrs) :-
    !Attrs ^ attr_may_call_mm_tabled := X.
set_box_policy(X, !Attrs) :-
    !Attrs ^ attr_box_policy := X.
set_affects_liveness(X, !Attrs) :-
    !Attrs ^ attr_affects_liveness := X.
set_allocates_memory(X, !Attrs) :-
    !Attrs ^ attr_allocates_memory := X.
set_registers_roots(X, !Attrs) :-
    !Attrs ^ attr_registers_roots := X.
set_refers_to_llds_stack(X, !Attrs) :-
    !Attrs ^ attr_refers_to_llds_stack := X.
set_call_std_out_regs(X, !Attrs) :-
    !Attrs ^ attr_call_std_out_regs := X.
set_may_duplicate(X, !Attrs) :-
    !Attrs ^ attr_may_duplicate := X.
set_may_export_body(X, !Attrs) :-
    !Attrs ^ attr_may_export_body := X.
set_for_specific_backend(X, !Attrs) :-
    !Attrs ^ attr_for_specific_backend := X.

foreign_arg_name_mode_box_project_maybe_name_mode(MaybeNameModeBox)
        = MaybeNameMode :-
    MaybeNameModeBox = foreign_arg_name_mode_box(MaybeNameMode, _).

pragma_get_modes([], []).
pragma_get_modes([PragmaVar | PragmaVars], [Mode | Modes]) :-
    PragmaVar = pragma_var(_Var, _Name, Mode, _BoxPolicy),
    pragma_get_modes(PragmaVars, Modes).

pragma_get_var_infos([], []).
pragma_get_var_infos([PragmaVar | PragmaVars], [Info | Infos]) :-
    PragmaVar = pragma_var(_Var, Name, Mode, BoxPolicy),
    NameMode = foreign_arg_name_mode(Name, Mode),
    Info = foreign_arg_name_mode_box(yes(NameMode), BoxPolicy),
    pragma_get_var_infos(PragmaVars, Infos).

pragma_get_vars_and_var_infos([], [], []).
pragma_get_vars_and_var_infos([PragmaVar | PragmaVars],
        [Var | Vars], [Info | Infos]) :-
    PragmaVar = pragma_var(Var, Name, Mode, BoxPolicy),
    NameMode = foreign_arg_name_mode(Name, Mode),
    Info = foreign_arg_name_mode_box(yes(NameMode), BoxPolicy),
    pragma_get_vars_and_var_infos(PragmaVars, Vars, Infos).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_data_foreign.
%---------------------------------------------------------------------------%
