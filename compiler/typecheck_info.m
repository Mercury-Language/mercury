%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: typecheck_info.m.
% Main author: fjh.
%
% This module defines the typecheck_info and type_assign types, plus some
% useful predicates that work with those types.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.typecheck_info.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module map.
:- import_module set_tree234.

%-----------------------------------------------------------------------------%
%
% The typecheck_info data structure's initializer.
%

:- type typecheck_info.

:- pred typecheck_info_init(module_info::in, pred_id::in, bool::in,
    prog_varset::in, pred_status::in, pred_markers::in,
    list(error_spec)::in, typecheck_info::out) is det.

%-----------------------------------------------------------------------------%
%
% The purpose-specific types of the values held in the typecheck_info.
%

% XXX Values of type cons_type_info are not held in the typecheck_info,
% though values of type cons_type_info_source are. Values of this type
% are currently computed on demand, though they should be stored precomputed
% for each data constructor for each type in the HLDS.
:- type cons_type_info
    --->    cons_type_info(
                % Type variables.
                cti_varset          :: tvarset,

                % Existentially quantified type vars.
                cti_exit_tvars      :: existq_tvars,

                % Constructor type.
                cti_result_type     :: mer_type,

                % Types of the arguments.
                cti_arg_types       :: list(mer_type),

                % Constraints introduced by this constructor (e.g. if it is
                % actually a function, or if it is an existentially quantified
                % data constructor).
                cti_constraints     :: hlds_constraints,

                cti_source          :: cons_type_info_source
            ).

:- type cons_type_info_source
    --->    source_type(type_ctor)
    ;       source_builtin_type(string)
    ;       source_get_field_access(type_ctor)
    ;       source_set_field_access(type_ctor)
    ;       source_apply(string)
    ;       source_pred(pred_id).

:- func project_cons_type_info_source(cons_type_info) = cons_type_info_source.

:- type overloaded_symbol_map == map(overloaded_symbol, list(prog_context)).

:- type overloaded_symbol
    --->    overloaded_pred(
                pf_sym_name_arity,
                list(pred_id)
            )
    ;       overloaded_func(
                cons_id,
                list(cons_type_info_source)
            ).

:- type type_error_clause_context
    --->    type_error_clause_context(
                % The outermost context for a type error is the clause
                % that was being typechecked when the error was found.

                % The tecc_pred_id field gives the identity of the predicate,
                % and the tecc_module_info field allows us to convert that
                % into the information we actually need about the predicate,
                % such as its name.
                tecc_module_info                :: module_info,
                tecc_pred_id                    :: pred_id,

                % The markers of the pred being checked. The code that
                % needs to know this to generate good error messages could
                % get the pred_info using the two fields above and then
                % look up the pred_markers in there, but we have them anyway
                % when we construct the typecheck_info, and the space it takes
                % up is not worth worrying about.
                tecc_pred_markers               :: pred_markers,

                % Which clause of the predicate are we checking?
                tecc_clause_num                 :: int,

                % The context of the clause, which will be the context
                % of its head.
                tecc_clause_context             :: prog_context,

                % Variable names in the clause being checked.
                tecc_varset                     :: prog_varset
            ).

%-----------------------------------------------------------------------------%
%
% Basic access predicates for typecheck_info.
%

:- pred typecheck_info_get_error_clause_context(typecheck_info::in,
    type_error_clause_context::out) is det.
:- pred typecheck_info_get_overloaded_symbol_map(typecheck_info::in,
    overloaded_symbol_map::out) is det.
:- pred typecheck_info_get_ambiguity_warn_limit(typecheck_info::in,
    int::out) is det.

:- pred typecheck_info_get_module_info(typecheck_info::in,
    module_info::out) is det.
:- pred typecheck_info_get_pred_id(typecheck_info::in,
    pred_id::out) is det.

:- pred typecheck_info_get_calls_are_fully_qualified(typecheck_info::in,
    is_fully_qualified::out) is det.
:- pred typecheck_info_get_ambiguity_error_limit(typecheck_info::in,
    int::out) is det.
:- pred typecheck_info_get_is_field_access_function(typecheck_info::in,
    maybe(pred_status)::out) is det.
:- pred typecheck_info_get_non_overload_errors(typecheck_info::in,
    list(error_spec)::out) is det.
:- pred typecheck_info_get_overload_error(typecheck_info::in,
    maybe(error_spec)::out) is det.
:- pred typecheck_info_get_nosuffix_integer_vars(typecheck_info::in,
    set_tree234(prog_var)::out) is det.

:- pred typecheck_info_set_overloaded_symbol_map(overloaded_symbol_map::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_non_overload_errors(list(error_spec)::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_overload_error(maybe(error_spec)::in,
    typecheck_info::in, typecheck_info::out) is det.

%-----------------------------------------------------------------------------%
%
% Utility predicates for typecheck_info.
%

:- pred typecheck_info_get_module_name(typecheck_info::in, module_name::out)
    is det.
:- pred typecheck_info_get_pred_table(typecheck_info::in, predicate_table::out)
    is det.
:- pred typecheck_info_get_type_table(typecheck_info::in, type_table::out)
    is det.
:- pred typecheck_info_get_cons_table(typecheck_info::in, cons_table::out)
    is det.

:- pred typecheck_info_add_overloaded_symbol(overloaded_symbol::in,
    prog_context::in, typecheck_info::in, typecheck_info::out) is det.

:- pred typecheck_info_add_nosuffix_integer_var(prog_var::in,
    typecheck_info::in, typecheck_info::out) is det.

:- pred typecheck_info_add_error(error_spec::in,
    typecheck_info::in, typecheck_info::out) is det.

:- pred typecheck_info_get_all_errors(typecheck_info::in,
    list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.

:- import_module term.

%-----------------------------------------------------------------------------%

:- type typecheck_info
    --->    typecheck_info(
                % The four most frequently used parts of the conceptual
                % typecheck_info are here in this cell. The less frequently
                % used parts are in the typecheck_sub_info, or (if they are
                % needed for the generation of type error messages) in the
                % type_error_clause_context.

                tci_sub_info                    :: typecheck_sub_info,

                % The part of the information needed to generate good error
                % messages for type errors that remains valid during the
                % typechecking of an entire clause. As for the Information
                % needed to generate good error messages that changes as
                % we traverse the body of a clause, that is passed around
                % separately in typecheck.m.
                tci_error_clause_context        :: type_error_clause_context,

                % The symbols used by the current predicate that have
                % more than one accessible definition, mapped to the unsorted
                % list of the locations that refer to them.
                tci_overloaded_symbol_map       :: overloaded_symbol_map,

                % The value of the option --typecheck-ambiguity-warn-limit.
                tci_ambiguity_warn_limit        :: int
            ).

:- type typecheck_sub_info
    --->    typecheck_sub_info(
                % Are calls from the body of the predicate we are checking
                % guaranteed to be already fully qualified? In some cases,
                % such as when the body was read in from a .opt file,
                % they will be.
                tcsi_calls_are_fully_qualified  :: is_fully_qualified,

                % The value of the option --typecheck-ambiguity-error-limit.
                tcsi_ambiguity_error_limit      :: int,

                % Is the pred we are checking a field access function? If so,
                % there should only be a field access function application
                % in the body, not predicate or function calls or constructor
                % applications, and we will need to know the predicate's
                % import status, so we don't generate errors when the function
                % is opt-imported into other modules.
                tcsi_is_field_access_function   :: maybe(pred_status),

                % The list of errors found so far (if any), with one exception:
                % any errors about overloading are in the overload_error field.
                tcsi_non_overload_errors        :: list(error_spec),

                % Have we already generated a warning or error message about
                % highly ambiguous overloading? If yes, this has the message.
                tcsi_overload_error             :: maybe(error_spec),

                % The set of variables that have been unified with integer
                % constants without suffixes. If a variable in this set
                % is used in a context that expects either an unsigned integer
                % or a sized integer (either signed or unsigned), then
                % we extend the error message with a reminder about the
                % need for the right suffix.
                tcsi_nosuffix_integer_vars      :: set_tree234(prog_var)
            ).

%-----------------------------------------------------------------------------%

typecheck_info_init(ModuleInfo, PredId, IsFieldAccessFunction,
        ClauseVarSet, Status, PredMarkers, NonOverloadErrors, Info) :-
    CallsAreFullyQualified = calls_are_fully_qualified(PredMarkers),
    (
        IsFieldAccessFunction = no,
        MaybeFieldAccessFunction = no
    ;
        IsFieldAccessFunction = yes,
        MaybeFieldAccessFunction = yes(Status)
    ),
    OverloadErrors = no,
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_int_option(Globals, typecheck_ambiguity_warn_limit,
        AmbiguityWarnLimit),
    globals.lookup_int_option(Globals, typecheck_ambiguity_error_limit,
        AmbiguityErrorLimit),
    NoSuffixIntegerMap = set_tree234.init,
    SubInfo = typecheck_sub_info(CallsAreFullyQualified, AmbiguityErrorLimit,
        MaybeFieldAccessFunction, NonOverloadErrors, OverloadErrors,
        NoSuffixIntegerMap),
    ClauseNum = 0,
    ClauseContext = type_error_clause_context(ModuleInfo, PredId,
        PredMarkers, ClauseNum, term.context_init, ClauseVarSet),
    map.init(OverloadedSymbolMap),
    Info = typecheck_info(SubInfo, ClauseContext, OverloadedSymbolMap,
        AmbiguityWarnLimit).

%-----------------------------------------------------------------------------%

project_cons_type_info_source(CTI) = CTI ^ cti_source.

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_nosuffix_integer_vars(set_tree234(prog_var)::in,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_info_get_error_clause_context(Info, X) :-
    X = Info ^ tci_error_clause_context.
typecheck_info_get_overloaded_symbol_map(Info, X) :-
    X = Info ^ tci_overloaded_symbol_map.
typecheck_info_get_ambiguity_warn_limit(Info, X) :-
    X = Info ^ tci_ambiguity_warn_limit.

typecheck_info_get_module_info(Info, X) :-
    X = Info ^ tci_error_clause_context ^ tecc_module_info.
typecheck_info_get_pred_id(Info, X) :-
    X = Info ^ tci_error_clause_context ^ tecc_pred_id.

typecheck_info_get_calls_are_fully_qualified(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_calls_are_fully_qualified.
typecheck_info_get_ambiguity_error_limit(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_ambiguity_error_limit.
typecheck_info_get_is_field_access_function(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_is_field_access_function.
typecheck_info_get_non_overload_errors(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_non_overload_errors.
typecheck_info_get_overload_error(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_overload_error.
typecheck_info_get_nosuffix_integer_vars(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_nosuffix_integer_vars.

typecheck_info_set_overloaded_symbol_map(X, !Info) :-
    !Info ^ tci_overloaded_symbol_map := X.

typecheck_info_set_non_overload_errors(X, !Info) :-
    !Info ^ tci_sub_info ^ tcsi_non_overload_errors := X.
typecheck_info_set_overload_error(X, !Info) :-
    !Info ^ tci_sub_info ^ tcsi_overload_error := X.
typecheck_info_set_nosuffix_integer_vars(X, !Info) :-
    !Info ^ tci_sub_info ^ tcsi_nosuffix_integer_vars := X.

% Access statistics from before the change on 2015 jan 9.
%
%  i      read      same      diff   same%
%  0   7325016         0         0              module_info
%  1        62         0    519838   0.000%     called_pred_id
%  2        62    878563   3641386  19.437%     arg_num
%  3    240136    773992   2491358  23.703%     context
%  4        86        53   1245023   0.004%     unify_context
%  5   9555205        22   5883791   0.000%     type_assign_set
%  6   2421093         0         0              ambiguity_warn_limit
%  7    681078         0         0              pred_id
%  8       129         0         0              import_status
%  9   1704110         0         0              pred_markers
% 10    856891         0         0              is_field_access_function
% 11        43         0         0              varset
% 12    401290         0       124   0.000%     non_overload_errors
% 13    401218         0       164   0.000%     overload_error
% 14    188296         0    188132   0.000%     overloaded_symbols
% 15       204         0         0              ambiguity_error_limit

% Access statistics from during the change on 2015 jan 9.
% (The final commit changed the set of fields and getters/setters
% still further.)
%
%  i      read      same      diff   same%
%  0   1046873         0         0          error_clause_stats
%  1  10228916        23   6337890   0.00%  type_assign_set
%  2   2550889         0         0          ambiguity_warn_limit
%  3   7802441         0         0          module_info
%  4    735390         0         0          pred_id
%  5   1821662         0         0          pred_markers
%  6         0         0         0          varset
%  7       129         0         0          pred_import_status
%  8    914248         0         0          is_field_access_function
%  9    431258         0       125   0.00%  non_overload_errors
% 10    431185         0       164   0.00%  overload_error
% 11    192779         0    192575   0.00%  overloaded_symbol_map
% 12       204         0         0          ambiguity_error_limit

%-----------------------------------------------------------------------------%

typecheck_info_get_module_name(Info, Name) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_name(ModuleInfo, Name).
typecheck_info_get_pred_table(Info, Preds) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, Preds).
typecheck_info_get_type_table(Info, Types) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_type_table(ModuleInfo, Types).
typecheck_info_get_cons_table(Info, Ctors) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_cons_table(ModuleInfo, Ctors).

typecheck_info_add_overloaded_symbol(Symbol, Context, !Info) :-
    typecheck_info_get_overloaded_symbol_map(!.Info, OverloadedSymbolMap0),
    ( if map.search(OverloadedSymbolMap0, Symbol, OldContexts) then
        Contexts = [Context | OldContexts],
        map.det_update(Symbol, Contexts,
            OverloadedSymbolMap0, OverloadedSymbolMap)
    else
        Contexts = [Context],
        map.det_insert(Symbol, Contexts,
            OverloadedSymbolMap0, OverloadedSymbolMap)
    ),
    typecheck_info_set_overloaded_symbol_map(OverloadedSymbolMap, !Info).

typecheck_info_add_nosuffix_integer_var(Var, !Info) :-
    typecheck_info_get_nosuffix_integer_vars(!.Info, NoSuffixIntegerMap0),
    set_tree234.insert(Var, NoSuffixIntegerMap0, NoSuffixIntegerMap),
    typecheck_info_set_nosuffix_integer_vars(NoSuffixIntegerMap, !Info).

typecheck_info_add_error(Error, !Info) :-
    typecheck_info_get_non_overload_errors(!.Info, Errors0),
    Errors = [Error | Errors0],
    typecheck_info_set_non_overload_errors(Errors, !Info).

typecheck_info_get_all_errors(Info, Errors) :-
    typecheck_info_get_non_overload_errors(Info, Errors0),
    typecheck_info_get_overload_error(Info, MaybeOverloadError),
    (
        MaybeOverloadError = no,
        Errors = Errors0
    ;
        MaybeOverloadError = yes(OverloadError),
        Errors = [OverloadError | Errors0]
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_info.
%-----------------------------------------------------------------------------%
