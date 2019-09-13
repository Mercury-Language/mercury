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

:- import_module cord.
:- import_module bool.
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
:- type foreign_import_modules
    --->    foreign_import_modules(
                fim_c           :: set(module_name),
                % fim_cplusplus :: set(module_name),
                fim_csharp      :: set(module_name),
                fim_java        :: set(module_name),
                fim_erlang      :: set(module_name)
            ).

:- type fim_spec
    --->    fim_spec(
                % The specification of a foreign_import_module declaration,
                % without information about where it came from, used where
                % it *does not matter* where it came from.
                fimspec_lang                    :: foreign_language,
                fimspec_module_name             :: module_name
            ).

:- func init_foreign_import_modules = foreign_import_modules.

:- pred add_foreign_import_module(foreign_language::in, module_name::in,
    foreign_import_modules::in, foreign_import_modules::out) is det.

:- pred add_fim_spec(fim_spec::in,
    foreign_import_modules::in, foreign_import_modules::out) is det.

:- func get_all_fim_specs(foreign_import_modules) =
    set(fim_spec).

:- func get_all_foreign_import_modules(foreign_import_modules) =
    set(module_name).

:- func get_lang_fim_specs(foreign_import_modules,
    foreign_language) = set(fim_spec).

:- func get_lang_fim_modules(foreign_import_modules,
    foreign_language) = set(module_name).

:- implementation.

init_foreign_import_modules =
    foreign_import_modules(set.init, set.init, set.init, set.init).

add_foreign_import_module(Lang, ModuleName, !FIM) :-
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
    ;
        Lang = lang_erlang,
        ModuleNames0 = !.FIM ^ fim_erlang,
        ( if set.insert_new(ModuleName, ModuleNames0, ModuleNames) then
            !FIM ^ fim_erlang := ModuleNames
        else
            true
        )
    ).

add_fim_spec(FIMSpec, !FIM) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    add_foreign_import_module(Lang, ModuleName, !FIM).

get_all_fim_specs(FIM) = FIMSpecs :-
    FIM = foreign_import_modules(ModuleNamesC, ModuleNamesCSharp,
        ModuleNamesJava, ModuleNamesErlang),
    FIMSpecs = set.union_list([
        set.map(make_fim_spec(lang_c), ModuleNamesC),
        set.map(make_fim_spec(lang_csharp), ModuleNamesCSharp),
        set.map(make_fim_spec(lang_java), ModuleNamesJava),
        set.map(make_fim_spec(lang_erlang), ModuleNamesErlang)
        ]).

get_all_foreign_import_modules(FIM) = ModuleNames :-
    FIM = foreign_import_modules(ModuleNamesC, ModuleNamesCSharp,
        ModuleNamesJava, ModuleNamesErlang),
    ModuleNames = set.union_list([ModuleNamesC, ModuleNamesCSharp,
        ModuleNamesJava, ModuleNamesErlang]).

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
    ;
        Lang = lang_erlang,
        ModuleNames = FIM ^ fim_erlang
    ).

:- func make_fim_spec(foreign_language, module_name) = fim_spec.

make_fim_spec(Lang, ModuleName) = fim_spec(Lang, ModuleName).

%---------------------------------------------------------------------------%
%
% Stuff for the `foreign_decl' and `foreign_code' pragmas.
%

:- interface.

:- type foreign_include_file_infos == cord(foreign_include_file_info).

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
:- type pragma_foreign_proc_attributes.

:- func default_attributes(foreign_language) = pragma_foreign_proc_attributes.

:- func get_foreign_language(pragma_foreign_proc_attributes) =
    foreign_language.
:- func get_may_call_mercury(pragma_foreign_proc_attributes) =
    proc_may_call_mercury.
:- func get_thread_safe(pragma_foreign_proc_attributes) = proc_thread_safe.
:- func get_tabled_for_io(pragma_foreign_proc_attributes) =
    proc_tabled_for_io.
:- func get_purity(pragma_foreign_proc_attributes) = purity.
:- func get_terminates(pragma_foreign_proc_attributes) = proc_terminates.
:- func get_user_annotated_sharing(pragma_foreign_proc_attributes) =
    user_annotated_sharing.
:- func get_may_throw_exception(pragma_foreign_proc_attributes) =
    proc_may_throw_exception.
:- func get_ordinary_despite_detism(pragma_foreign_proc_attributes) = bool.
:- func get_may_modify_trail(pragma_foreign_proc_attributes) =
    proc_may_modify_trail.
:- func get_may_call_mm_tabled(pragma_foreign_proc_attributes) =
    proc_may_call_mm_tabled.
:- func get_box_policy(pragma_foreign_proc_attributes) = box_policy.
:- func get_affects_liveness(pragma_foreign_proc_attributes) =
    proc_affects_liveness.
:- func get_allocates_memory(pragma_foreign_proc_attributes) =
    proc_allocates_memory.
:- func get_registers_roots(pragma_foreign_proc_attributes) =
    proc_registers_roots.
:- func get_may_duplicate(pragma_foreign_proc_attributes) =
    maybe(proc_may_duplicate).
:- func get_extra_attributes(pragma_foreign_proc_attributes)
    = pragma_foreign_proc_extra_attributes.

:- pred set_may_call_mercury(proc_may_call_mercury::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_thread_safe(proc_thread_safe::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_foreign_language(foreign_language::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_tabled_for_io(proc_tabled_for_io::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_purity(purity::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_terminates(proc_terminates::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_user_annotated_sharing(user_annotated_sharing::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_may_throw_exception(proc_may_throw_exception::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_ordinary_despite_detism(bool::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_may_modify_trail(proc_may_modify_trail::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_may_call_mm_tabled(proc_may_call_mm_tabled::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_box_policy(box_policy::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_affects_liveness(proc_affects_liveness::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_allocates_memory(proc_allocates_memory::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_registers_roots(proc_registers_roots::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred set_may_duplicate(maybe(proc_may_duplicate)::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.
:- pred add_extra_attribute(pragma_foreign_proc_extra_attribute::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

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

    % Extract the vars from the list of pragma_vars.
    %
:- pred pragma_get_vars(list(pragma_var)::in, list(prog_var)::out) is det.

    % Extract the names from the list of pragma_vars.
    %
:- pred pragma_get_var_infos(list(pragma_var)::in,
    list(foreign_arg_name_mode_box)::out) is det.

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

:- type pragma_foreign_proc_extra_attribute
    --->    refers_to_llds_stack
    ;       backend(backend)
    ;       needs_call_standard_output_registers.
            % On the LLDS backend, this foreign_proc needs to put its outputs
            % into the same registers as if it were a call. This is useful
            % if the code of the foreign procedure being invoked can suspend
            % for a while, resume at a label in the runtime system, and then
            % return from code at that label. The code that places the outputs
            % must put them where calls expect them, but without this
            % attribute, the LLDS code generator could try to put the output
            % somewhere else.

:- type pragma_foreign_proc_extra_attributes ==
    list(pragma_foreign_proc_extra_attribute).

%---------------------------------------------------------------------------%

:- implementation.

    % If you add an attribute, you may need to modify
    % `foreign_proc_attributes_to_strings'.
    %
:- type pragma_foreign_proc_attributes
    --->    attributes(
                attr_foreign_language           :: foreign_language,
                attr_may_call_mercury           :: proc_may_call_mercury,
                attr_thread_safe                :: proc_thread_safe,
                attr_tabled_for_io              :: proc_tabled_for_io,
                attr_purity                     :: purity,
                attr_terminates                 :: proc_terminates,
                attr_user_annotated_sharing     :: user_annotated_sharing,
                attr_may_throw_exception        :: proc_may_throw_exception,
                attr_ordinary_despite_detism    :: bool,
                attr_may_modify_trail           :: proc_may_modify_trail,
                attr_may_call_mm_tabled         :: proc_may_call_mm_tabled,
                attr_box_policy                 :: box_policy,
                attr_affects_liveness           :: proc_affects_liveness,
                attr_allocates_memory           :: proc_allocates_memory,
                attr_registers_roots            :: proc_registers_roots,
                attr_may_duplicate              :: maybe(proc_may_duplicate),
                attr_extra_attributes ::
                    list(pragma_foreign_proc_extra_attribute)
            ).

default_attributes(Language) =
    attributes(Language, proc_may_call_mercury, proc_not_thread_safe,
        proc_not_tabled_for_io, purity_impure, depends_on_mercury_calls,
        no_user_annotated_sharing, default_exception_behaviour,
        no, proc_may_modify_trail, proc_default_calls_mm_tabled,
        bp_native_if_possible, proc_default_affects_liveness,
        proc_default_allocates_memory, proc_default_registers_roots,
        no, []).

get_foreign_language(Attrs) = Attrs ^ attr_foreign_language.
get_may_call_mercury(Attrs) = Attrs ^ attr_may_call_mercury.
get_thread_safe(Attrs) = Attrs ^ attr_thread_safe.
get_tabled_for_io(Attrs) = Attrs ^ attr_tabled_for_io.
get_purity(Attrs) = Attrs ^ attr_purity.
get_terminates(Attrs) = Attrs ^ attr_terminates.
get_user_annotated_sharing(Attrs) = Attrs ^ attr_user_annotated_sharing.
get_may_throw_exception(Attrs) = Attrs ^ attr_may_throw_exception.
get_ordinary_despite_detism(Attrs) = Attrs ^ attr_ordinary_despite_detism.
get_may_modify_trail(Attrs) = Attrs ^ attr_may_modify_trail.
get_may_call_mm_tabled(Attrs) = Attrs ^ attr_may_call_mm_tabled.
get_box_policy(Attrs) = Attrs ^ attr_box_policy.
get_affects_liveness(Attrs) = Attrs ^ attr_affects_liveness.
get_allocates_memory(Attrs) = Attrs ^ attr_allocates_memory.
get_registers_roots(Attrs) = Attrs ^ attr_registers_roots.
get_may_duplicate(Attrs) = Attrs ^ attr_may_duplicate.
get_extra_attributes(Attrs) = Attrs ^ attr_extra_attributes.

set_may_call_mercury(MayCallMercury, !Attrs) :-
    !Attrs ^ attr_may_call_mercury := MayCallMercury.
set_thread_safe(ThreadSafe, !Attrs) :-
    !Attrs ^ attr_thread_safe := ThreadSafe.
set_foreign_language(ForeignLanguage, !Attrs) :-
    !Attrs ^ attr_foreign_language := ForeignLanguage.
set_tabled_for_io(TabledForIo, !Attrs) :-
    !Attrs ^ attr_tabled_for_io := TabledForIo.
set_purity(Purity, !Attrs) :-
    !Attrs ^ attr_purity := Purity.
set_terminates(Terminates, !Attrs) :-
    !Attrs ^ attr_terminates := Terminates.
set_user_annotated_sharing(UserSharing, !Attrs) :-
    !Attrs ^ attr_user_annotated_sharing := UserSharing.
set_may_throw_exception(MayThrowException, !Attrs) :-
    !Attrs ^ attr_may_throw_exception := MayThrowException.
set_ordinary_despite_detism(OrdinaryDespiteDetism, !Attrs) :-
    !Attrs ^ attr_ordinary_despite_detism := OrdinaryDespiteDetism.
set_may_modify_trail(MayModifyTrail, !Attrs) :-
    !Attrs ^ attr_may_modify_trail := MayModifyTrail.
set_may_call_mm_tabled(MayCallMM_Tabled, !Attrs) :-
    !Attrs ^ attr_may_call_mm_tabled := MayCallMM_Tabled.
set_box_policy(BoxPolicyStr, !Attrs) :-
    !Attrs ^ attr_box_policy := BoxPolicyStr.
set_affects_liveness(AffectsLiveness, !Attrs) :-
    !Attrs ^ attr_affects_liveness := AffectsLiveness.
set_allocates_memory(AllocatesMemory, !Attrs) :-
    !Attrs ^ attr_allocates_memory := AllocatesMemory.
set_registers_roots(RegistersRoots, !Attrs) :-
    !Attrs ^ attr_registers_roots := RegistersRoots.
set_may_duplicate(MayDuplicate, !Attrs) :-
    !Attrs ^ attr_may_duplicate := MayDuplicate.

add_extra_attribute(NewAttribute, !Attrs) :-
    !Attrs ^ attr_extra_attributes :=
        [NewAttribute | !.Attrs ^ attr_extra_attributes].

foreign_arg_name_mode_box_project_maybe_name_mode(MaybeNameModeBox)
        = MaybeNameMode :-
    MaybeNameModeBox = foreign_arg_name_mode_box(MaybeNameMode, _).

pragma_get_modes([], []).
pragma_get_modes([PragmaVar | PragmaVars], [Mode | Modes]) :-
    PragmaVar = pragma_var(_Var, _Name, Mode, _BoxPolicy),
    pragma_get_modes(PragmaVars, Modes).

pragma_get_vars([], []).
pragma_get_vars([PragmaVar | PragmaVars], [Var | Vars]) :-
    PragmaVar = pragma_var(Var, _Name, _Mode, _BoxPolicy),
    pragma_get_vars(PragmaVars, Vars).

pragma_get_var_infos([], []).
pragma_get_var_infos([PragmaVar | PragmaVars], [Info | Infos]) :-
    PragmaVar = pragma_var(_Var, Name, Mode, BoxPolicy),
    NameMode = foreign_arg_name_mode(Name, Mode),
    Info = foreign_arg_name_mode_box(yes(NameMode), BoxPolicy),
    pragma_get_var_infos(PragmaVars, Infos).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_data_foreign.
%---------------------------------------------------------------------------%
