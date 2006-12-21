%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prim_data.m.
% Main authors: fjh, zs.
%
% This module contains some types and predicates that are, or are planned to
% be, shared between the compiler and the debugger.

%-----------------------------------------------------------------------------%

:- module mdbcomp.prim_data.

:- interface.

    % This enumeration must be EXACTLY the same as the MR_PredFunc enum
    % in runtime/mercury_stack_layout.h, and in the same order, since the
    % code (in browser) assumes the representation is the same.
:- type pred_or_func
    --->    predicate
    ;       function.

    % The kinds of events with which MR_trace may be called, either
    % by compiler-generated code, or by code in the standard library
    % referring to compiler-generated data structures.
    %
    % This enumeration must be EXACTLY the same as the MR_trace_port enum
    % in runtime/mercury_trace_base.h, and in the same order, since the
    % code (in browser) assumes the representation is the same.
:- type trace_port
    --->    port_call
    ;       port_exit
    ;       port_redo
    ;       port_fail
    ;       port_exception
    ;       port_ite_cond
    ;       port_ite_then
    ;       port_ite_else
    ;       port_neg_enter
    ;       port_neg_success
    ;       port_neg_failure
    ;       port_disj
    ;       port_switch
    ;       port_nondet_pragma_first
    ;       port_nondet_pragma_later
    ;       port_user.

% was in compiler/prog_data.m

    % The order that the sym_name function symbols appear in can be significant
    % for module dependency ordering.
:- type sym_name
    --->    unqualified(string)
    ;       qualified(sym_name, string).

:- type module_name == sym_name.

% was in compiler/proc_label.m

    % A proc_label is a data structure a backend can use to as the basis
    % of the label used as the entry point of a procedure.
    %
    % The defining module is the module that provides the code for the
    % predicate, the declaring module contains the `:- pred' declaration.
    % When these are different, as for specialised versions of predicates
    % from `.opt' files, the defining module's name may need to be added
    % as a qualifier to the label.
:- type proc_label
    --->    ordinary_proc_label(
                ord_defining_module     :: module_name,
                ord_p_or_f              :: pred_or_func,
                ord_declaring_module    :: module_name,
                ord_pred_name           :: string,
                ord_arity               :: int,
                ord_mode_number         :: int
            )
    ;       special_proc_label(
                spec_defining_module    :: module_name,
                spec_spec_id            :: special_pred_id,
                                        % The special_pred_id indirectly
                                        % defines the predicate name.
                spec_type_module        :: module_name,
                spec_type_name          :: string,
                spec_type_arity         :: int,
                spec_mode_number        :: int
            ).

:- type special_pred_id
    --->    spec_pred_unify
    ;       spec_pred_index
    ;       spec_pred_compare
    ;       spec_pred_init.

    % special_pred_name_arity(SpecialPredId, GenericPredName, TargetName,
    %   Arity):
    %
    % True iff there is a special predicate of category SpecialPredId,
    % called builtin.GenericPredName/Arity, and for which the name of the
    % predicate in the target language is TargetName.
    %
:- pred special_pred_name_arity(special_pred_id, string, string, int).
:- mode special_pred_name_arity(in, out, out, out) is det.
:- mode special_pred_name_arity(out, in, out, out) is semidet.
:- mode special_pred_name_arity(out, out, in, out) is semidet.

    % get_special_pred_id_generic_name(SpecialPredId) = GenericPredName:
    %
    % The name of the generic predicate for SpecialPredId is
    % builtin.GenericPredName.
    %
:- func get_special_pred_id_generic_name(special_pred_id) = string.

    % get_special_pred_id_target_name(SpecialPredId) = TargetName:
    %
    % The name of the predicate in the target language for SpecialPredId is
    % TargetName.
    %
:- func get_special_pred_id_target_name(special_pred_id) = string.

    % get_special_pred_id_name(SpecialPredId) = Arity:
    %
    % The arity of the SpecialPredId predicate is Arity.
    %
:- func get_special_pred_id_arity(special_pred_id) = int.

    % string_to_sym_name_sep(String, Separator) = SymName:
    %
    % Convert a string, possibly prefixed with module qualifiers (separated
    % by Separator), into a symbol name.
    %
:- func string_to_sym_name_sep(string, string) = sym_name.

    % string_to_sym_name(String) = SymName:
    %
    % Convert a string, possibly prefixed with module qualifiers (separated
    % by the standard Mercury module qualifier separator), into a symbol name.
    %
:- func string_to_sym_name(string) = sym_name.

    % sym_name_to_string_sep(SymName, Separator) = String:
    %
    % Convert a symbol name to a string, with module qualifiers separated
    % by Separator.
    %
:- func sym_name_to_string_sep(sym_name, string) = string.

    % sym_name_to_string(SymName) = String:
    %
    % Convert a symbol name to a string, with module qualifiers separated by
    % the standard Mercury module qualifier operator.
    %
:- func sym_name_to_string(sym_name) = string.

    % is_submodule(SymName1, SymName2):
    %
    % True iff SymName1 is a submodule of SymName2.
    % For example mod1.mod2.mod3 is a submodule of mod1.mod2.
    %
:- pred is_submodule(module_name::in, module_name::in) is semidet.

    % insert_module_qualifier(ModuleName, SymName0) = SymName:
    %
    % Prepend the specified ModuleName onto the module qualifiers in SymName0,
    % giving SymName.
    %
:- func insert_module_qualifier(string, sym_name) = sym_name.

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

    % Returns the name of the module containing builtins for tabling;
    % originally these were in "private_builtin", but were then moved into
    % a separate module. This module is automatically imported iff any
    % predicate is tabled.
    %
:- func mercury_table_builtin_module = sym_name.

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

    % Returns the sym_name of the module with the given name in the
    % Mercury standard library.
    %
:- func mercury_std_lib_module_name(sym_name) = sym_name.

:- pred is_std_lib_module_name(sym_name::in, string::out) is semidet.

    % Succeeds iff the specified module is one of the builtin modules listed
    % above which may be automatically imported.
    %
:- pred any_mercury_builtin_module(sym_name::in) is semidet.

    % Succeeds iff the specified module will never be traced.
    %
:- pred non_traced_mercury_builtin_module(sym_name::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module library.
:- import_module list.
:- import_module string.

string_to_sym_name_sep(String, ModuleSeparator) = Result :-
    % This would be simpler if we had a string.rev_sub_string_search/3 pred.
    % With that, we could search for underscores right-to-left, and construct
    % the resulting symbol directly. Instead, we search for them left-to-right,
    % and then call insert_module_qualifier to fix things up.
    (
        string.sub_string_search(String, ModuleSeparator, LeftLength),
        LeftLength > 0
    ->
        string.left(String, LeftLength, ModuleName),
        string.length(String, StringLength),
        string.length(ModuleSeparator, SeparatorLength),
        RightLength = StringLength - LeftLength - SeparatorLength,
        string.right(String, RightLength, Name),
        NameSym = string_to_sym_name_sep(Name, ModuleSeparator),
        Result = insert_module_qualifier(ModuleName, NameSym)
    ;
        Result = unqualified(String)
    ).

string_to_sym_name(String) = string_to_sym_name_sep(String, ".").

sym_name_to_string_sep(unqualified(Name), _Separator) = Name.
sym_name_to_string_sep(qualified(ModuleSym, Name), Separator) = QualName :-
    ModuleName = sym_name_to_string_sep(ModuleSym, Separator),
    string.append_list([ModuleName, Separator, Name], QualName).

sym_name_to_string(SymName) = sym_name_to_string_sep(SymName, ".").

insert_module_qualifier(ModuleName, unqualified(PlainName)) =
        qualified(unqualified(ModuleName), PlainName).
insert_module_qualifier(ModuleName, qualified(ModuleQual0, PlainName)) =
        qualified(ModuleQual, PlainName) :-
    insert_module_qualifier(ModuleName, ModuleQual0) = ModuleQual.

is_submodule(SymName, SymName).
is_submodule(qualified(SymNameA, _), SymNameB) :-
    is_submodule(SymNameA, SymNameB).

special_pred_name_arity(spec_pred_unify, "unify", "__Unify__", 2).
special_pred_name_arity(spec_pred_index, "index", "__Index__", 2).
special_pred_name_arity(spec_pred_compare, "compare", "__Compare__", 3).
special_pred_name_arity(spec_pred_init, "initialise", "__Initialise__", 1).

get_special_pred_id_generic_name(Id) = Name :-
        special_pred_name_arity(Id, Name, _, _).

get_special_pred_id_target_name(Id) = Name :-
        special_pred_name_arity(Id, _, Name, _).

get_special_pred_id_arity(Id) = Arity :-
        special_pred_name_arity(Id, _, _, Arity).

% We may eventually want to put the standard library into a package "std":
% mercury_public_builtin_module = qualified(unqualified("std"), "builtin").
% mercury_private_builtin_module(M) =
%       qualified(unqualified("std"), "private_builtin"))).
mercury_public_builtin_module = unqualified("builtin").
mercury_private_builtin_module = unqualified("private_builtin").
mercury_table_builtin_module = unqualified("table_builtin").
mercury_profiling_builtin_module = unqualified("profiling_builtin").
mercury_term_size_prof_builtin_module = unqualified("term_size_prof_builtin").
mercury_par_builtin_module = unqualified("par_builtin").
mercury_std_lib_module_name(Name) = Name.

is_std_lib_module_name(SymName, Name) :-
    Name = sym_name_to_string(SymName),
    mercury_std_library_module(Name).

any_mercury_builtin_module(Module) :-
    ( Module = mercury_public_builtin_module
    ; Module = mercury_private_builtin_module
    ; Module = mercury_table_builtin_module
    ; Module = mercury_profiling_builtin_module
    ; Module = mercury_term_size_prof_builtin_module
    ; Module = mercury_par_builtin_module
    ).

non_traced_mercury_builtin_module(Module) :-
    ( Module = mercury_table_builtin_module
    ; Module = mercury_profiling_builtin_module
    ; Module = mercury_term_size_prof_builtin_module
    ; Module = mercury_par_builtin_module
    ).
