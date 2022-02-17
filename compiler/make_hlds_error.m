%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2006, 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make_hlds_error.m.
%
% Utility predicates for writing out warning and error messages when
% building the HLDS. Error messages specific to a given submodule of
% make_hlds.m are in that specific submodule; this module is for error messages
% that are either needed by more than one submodule of make_hlds.m, or are
% needed outside make_hlds.m.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds_error.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred report_multiple_def_error(sym_name::in, int::in, string::in,
    prog_context::in, prog_context::in, list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred report_undefined_pred_or_func_error(maybe(pred_or_func)::in,
    sym_name::in, arity::in, list(arity)::in, prog_context::in,
    list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Similar to report_undeclared_mode_error, but gives less information.
    % XXX perhaps we should get rid of this, and change the callers to
    % instead call undeclared_mode_error.
    %
:- pred report_undefined_mode_error(sym_name::in, int::in, prog_context::in,
    list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred maybe_report_undefined_pred_error(module_info::in,
    pred_or_func::in, sym_name::in, pred_form_arity::in, pred_status::in,
    maybe_class_method::in, prog_context::in, list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

report_multiple_def_error(SymName, Arity, DefType, Context, OrigContext,
        ExtraPieces, !Specs) :-
    % The flattening of source item blocks by modules.m puts
    % all items in a given section together. Since the original
    % source code may have had the contents of the different sections
    % intermingled, this may change the relative order of items.
    % Put them back in the original order for this error message.
    compare(CmpRes, OrigContext, Context),
    (
        ( CmpRes = (<)
        ; CmpRes = (=)
        ),
        FirstContext = OrigContext,
        SecondContext = Context
    ;
        CmpRes = (>),
        FirstContext = Context,
        SecondContext = OrigContext
    ),

    SNA = sym_name_arity(SymName, Arity),
    SecondDeclPieces = [words("Error:"), fixed(DefType),
        qual_sym_name_arity(SNA), words("multiply defined."), nl],
    FirstDeclPieces = [words("Here is the previous definition of"),
        fixed(DefType), qual_sym_name_arity(SNA), suffix("."), nl],
    SecondDeclMsg = simplest_msg(SecondContext, SecondDeclPieces),
    FirstDeclMsg = simplest_msg(FirstContext, FirstDeclPieces),
    (
        ExtraPieces = [],
        ExtraMsgs = []
    ;
        ExtraPieces = [_ | _],
        ExtraMsgs = [simplest_msg(SecondContext, ExtraPieces)]
    ),
    Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
        [SecondDeclMsg, FirstDeclMsg | ExtraMsgs]),
    !:Specs = [Spec | !.Specs].

report_undefined_pred_or_func_error(MaybePorF, SymName, Arity, OtherArities,
        Context, DescPieces, !Specs) :-
    (
        MaybePorF = no,
        SNAPrefixPieces = [],
        PredOrFuncPieces = [decl("pred"), words("or"), decl("func")]
    ;
        MaybePorF = yes(pf_predicate),
        SNAPrefixPieces = [words("predicate")],
        PredOrFuncPieces = [decl("pred")]
    ;
        MaybePorF = yes(pf_function),
        SNAPrefixPieces = [words("function")],
        PredOrFuncPieces = [decl("func")]
    ),
    SNA = sym_name_arity(SymName, Arity),
    MainPieces = [words("Error:") | DescPieces] ++
        [words("for")] ++ SNAPrefixPieces ++ [unqual_sym_name_arity(SNA),
        words("without corresponding")] ++ PredOrFuncPieces ++
        [words("declaration."), nl],
    (
        OtherArities = [],
        OtherArityPieces = []
    ;
        OtherArities = [_ | _],
        list.map(string.int_to_string, OtherArities, OtherArityStrs),
        OtherArityPieces = [unqual_sym_name(SymName), words("does exist with"),
            words(choose_number(OtherArityStrs, "arity", "arities"))] ++
            list_to_pieces(OtherArityStrs) ++
            [suffix("."), nl]
    ),
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, MainPieces ++ OtherArityPieces),
    !:Specs = [Spec | !.Specs].

report_undefined_mode_error(SymName, Arity, Context, DescPieces, !Specs) :-
    SNA = sym_name_arity(SymName, Arity),
    Pieces = [words("Error:") | DescPieces] ++ [words("for"),
        qual_sym_name_arity(SNA), words("specifies non-existent mode."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

maybe_report_undefined_pred_error(ModuleInfo, PredOrFunc, SymName,
        PredFormArity, Status, IsClassMethod, Context, DescPieces, !Specs) :-
    % Our caller (or one of its ancestors) will add an implicit declaration
    % for every undeclared predicate or function that has a reference to it
    % either in a clause or in some other declaration (e.g. a tabling pragma).
    % It will also mark the predicate as one whose type should be inferred.

    % We allow programmers to define predicates without declaring them first
    % if the user has specified the `--infer-types' option, unless
    % circumstances force us to require a predicate declaration anyway.
    %
    % The two relevant circumstances are:
    %
    % - predicates exported from their defining module, which must be declared
    %   to allow the compiler to put that declaration into the module's
    %   interface file without running the typechecker, and
    %
    % - predicates which implement type class methods.
    %   XXX Document the reason for the requirement here.

    DefinedInThisModule = pred_status_defined_in_this_module(Status),
    IsExported = pred_status_is_exported(Status),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, infer_types, InferTypes),
    ( if
        DefinedInThisModule = yes,
        IsExported = no,
        IsClassMethod = is_not_a_class_method,
        InferTypes = yes
    then
        true
    else
        PFSymNameArity = pf_sym_name_arity(PredOrFunc, SymName, PredFormArity),
        PredOrFuncStr = pred_or_func_to_str(PredOrFunc),
        MainPieces = [invis_order_default_start(1),
            words("Error:") | DescPieces] ++ [words("for"),
            unqual_pf_sym_name_orig_arity(PFSymNameArity), nl,
            words("without corresponding"),
            decl(PredOrFuncStr), words("declaration."), nl],
        MainMsg = simplest_msg(Context, MainPieces),

        module_info_get_predicate_table(ModuleInfo, PredicateTable),
        predicate_table_lookup_pf_sym(PredicateTable,
            is_fully_qualified, PredOrFunc, SymName, AllArityPredIds),
        gather_porf_arities(ModuleInfo, AllArityPredIds, PredOrFunc,
            PorFArities),
        set.delete(PredFormArity, PorFArities, OtherPredFormArities),
        % The sorting is to make the error message easier to read.
        % There should not be any duplicates among OtherArities, but better
        % safe than sorry ...
        set.to_sorted_list(OtherPredFormArities, OtherPredFormAritiesList),
        FullPredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
        (
            OtherPredFormAritiesList = [],
            Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
                [MainMsg])
        ;
            (
                OtherPredFormAritiesList = [OtherPredFormArity],
                OtherAritiesPieces = [words("However, a"),
                    words(FullPredOrFuncStr), words("of that name"),
                    words("does exist with arity"),
                        pred_form_arity_to_int_fixed(PredOrFunc,
                            OtherPredFormArity),
                    suffix("."), nl]
            ;
                OtherPredFormAritiesList = [_, _ | _],
                OtherAritiesPieces = [words("However,"),
                    words(FullPredOrFuncStr), suffix("s"),
                    words("of that name do exist with arities") |
                    component_list_to_pieces("and",
                        list.map(pred_form_arity_to_int_fixed(PredOrFunc),
                            OtherPredFormAritiesList))] ++
                    [suffix("."), nl]
            ),
            OtherAritiesMsg = simplest_msg(Context, OtherAritiesPieces),
            Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
                [MainMsg, OtherAritiesMsg])
        ),
        !:Specs = [Spec | !.Specs]
    ).

    % Given a list of pred ids, find out which of them represent
    % procedures which have the right pred_or_func field (WantedPorF),
    % and return their original arities.
    %
:- pred gather_porf_arities(module_info::in, list(pred_id)::in,
    pred_or_func::in, set(pred_form_arity)::out) is det.

gather_porf_arities(_ModuleInfo, [], _WantedPorF, set.init).
gather_porf_arities(ModuleInfo, [PredId | PredIds], WantedPorF,
        !:PorFArities) :-
    gather_porf_arities(ModuleInfo, PredIds, WantedPorF, !:PorFArities),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PorF = pred_info_is_pred_or_func(PredInfo),
    ( if PorF = WantedPorF then
        pred_info_get_markers(PredInfo, Markers),
        ( if check_marker(Markers, marker_no_pred_decl) then
            % This pred has no declaration, so including its arity in the list
            % would be misleading.
            true
        else
            PredFormArity = pred_info_pred_form_arity(PredInfo),
            set.insert(PredFormArity, !PorFArities)
        )
    else
        true
    ).

:- func pred_form_arity_to_int_fixed(pred_or_func, pred_form_arity)
    = format_component.

pred_form_arity_to_int_fixed(PredOrFunc, PredFormArity) = Component :-
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    UserArity = user_arity(UserArityInt),
    Component = int_fixed(UserArityInt).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds_error.
%---------------------------------------------------------------------------%
