%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: higher_order.m.
% Main author: stayl.
%
% This module is the top layer of its package. The job of the package is to
% specializes calls to higher order or polymorphic predicates where we know
% the value of one or more arguments that
%
% - have a higher order type,
% - contain a type_info, or
% - contain a typeclass_info.
%
% We do this in two circumstances. The first is when the programmer has
% *requested* a type specialization of the callee. The second is when
% there is no such request, but the size of the callee predicate's body
% is below a configurable limit. We have such a limit because as the size
% of the body increases, the overhead of e.g. a higher order call becomes
% less significant relative to the cost of execution the body code itself,
% while the increase in code size becomes more significant.
%
% If a specialization creates new opportunities for specialization, we will
% continue iterating the specialization process until we find no further
% opportunities, i.e. until we reach a fixpoint.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.higher_order.specialize_in_module.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred specialize_higher_order(io.text_output_stream::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module parse_tree.
:- import_module transform_hlds.higher_order.higher_order_global_info.
:- import_module transform_hlds.higher_order.make_specialized_preds.
:- import_module transform_hlds.higher_order.specialize_calls.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

specialize_higher_order(ProgressStream, !ModuleInfo, !IO) :-
    % Iterate collecting requests and process them until there are no more
    % requests remaining.

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    HigherOrder = OptTuple ^ ot_opt_higher_order,
    TypeSpec = OptTuple ^ ot_spec_types,
    UserTypeSpec = OptTuple ^ ot_spec_types_user_guided,
    SizeLimit = OptTuple ^ ot_higher_order_size_limit,
    ArgLimit = OptTuple ^ ot_higher_order_arg_limit,
    Params0 =
        ho_params(HigherOrder, TypeSpec, UserTypeSpec, SizeLimit, ArgLimit),
    some [!GlobalInfo] (
        module_info_get_valid_pred_ids(!.ModuleInfo, ValidPredIds),
        module_info_get_type_spec_tables(!.ModuleInfo, TypeSpecTables),
        TypeSpecTables = type_spec_tables(_, UserSpecPredIdSet, _, _),

        globals.lookup_bool_option(Globals, debug_higher_order_specialization,
            DebugSpec),
        (
            DebugSpec = no,
            MaybeProgressStream = no
        ;
            DebugSpec = yes,
            MaybeProgressStream = yes(ProgressStream)
        ),

        % Make sure the user requested specializations are processed first,
        % since we don't want to create more versions if one of these matches.
        % We need to process these even if the options don't call for
        % specialization being done for this module, in case any of the
        % specialized versions (whose existence is advertised by type_spec
        % pragmas in the module interface) are called from other modules,
        % whose options *do* call for specialization being done.

        set.to_sorted_list(UserSpecPredIdSet, UserSpecPredIds),
        (
            UserSpecPredIds = [],
            !:GlobalInfo =
                init_higher_order_global_info(Params0, !.ModuleInfo),
            NonUserSpecPredIds = ValidPredIds
        ;
            UserSpecPredIds = [_ | _],
            Params = Params0 ^ param_do_user_type_spec
                := spec_types_user_guided,
            !:GlobalInfo = init_higher_order_global_info(Params, !.ModuleInfo),

            set.list_to_set(ValidPredIds, ValidPredIdSet),
            set.difference(ValidPredIdSet, UserSpecPredIdSet,
                NonUserSpecPredIdSet),
            set.to_sorted_list(NonUserSpecPredIdSet, NonUserSpecPredIds),
            list.foldl(get_specialization_requests, UserSpecPredIds,
                !GlobalInfo),
            process_ho_spec_requests(MaybeProgressStream, !GlobalInfo, !IO)
        ),

        ( if
            ( HigherOrder = opt_higher_order
            ; TypeSpec = spec_types
            ; UserTypeSpec = spec_types_user_guided
            )
        then
            % Process all other specializations until no more requests
            % are generated.
            list.foldl(get_specialization_requests, NonUserSpecPredIds,
                !GlobalInfo),
            process_ho_spec_requests_to_fixpoint(MaybeProgressStream,
                !GlobalInfo, !IO)
        else
            true
        ),

        % Remove the predicates which were used to force the production of
        % user-requested type specializations, since they are not called
        % from anywhere and are no longer needed.
        !:ModuleInfo = hogi_get_module_info(!.GlobalInfo),
        list.foldl(module_info_remove_predicate, UserSpecPredIds, !ModuleInfo)
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.higher_order.specialize_in_module.
%---------------------------------------------------------------------------%
