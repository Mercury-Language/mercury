%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.make_hlds_passes.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.qual_info.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

    % When adding an item to the HLDS we need to know both its
    % import_status and whether uses of it must be module qualified.
    %
:- type item_status
    --->    item_status(import_status, need_qualifier).

    % do_parse_tree_to_hlds(Globals, DumpBaseFileName, ParseTree, MQInfo,
    %   EqvMap, UsedModules, QualInfo, InvalidTypes, InvalidModes, HLDS,
    %   Specs):
    %
    % Given MQInfo (returned by module_qual.m) and EqvMap and UsedModules
    % (both returned by equiv_type.m), converts ParseTree to HLDS.
    % Any errors found are returned in Specs.
    % Returns InvalidTypes = yes if undefined types found.
    % Returns InvalidModes = yes if undefined or cyclic insts or modes found.
    % QualInfo is an abstract type that is then passed back to
    % produce_instance_method_clauses (see below).
    %
:- pred do_parse_tree_to_hlds(globals::in, string::in, compilation_unit::in,
    mq_info::in, eqv_map::in, used_modules::in, qual_info::out,
    bool::out, bool::out, module_info::out, list(error_spec)::out) is det.

    % The bool records whether any cyclic insts or modes were detected.
    %
:- pred add_item_decl_pass_1(item::in, bool::out,
    item_status::in, item_status::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_item_pass_3(item::in, import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_stratified_pred(string::in, sym_name::in, arity::in,
    term.context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % add_pred_marker(PragmaName, Name, Arity, Status,
    %   Context, Marker, ConflictMarkers, !ModuleInfo, !Specs):
    %
    % Adds Marker to the marker list of the pred(s) with give Name and Arity,
    % updating the ModuleInfo. If the named pred does not exist, or the pred
    % already has a marker in ConflictMarkers, report an error.
    %
:- pred add_pred_marker(string::in, sym_name::in, arity::in, import_status::in,
    prog_context::in, marker::in, list(marker)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- type add_marker_pred_info == pred(pred_info, pred_info).
:- inst add_marker_pred_info == (pred(in, out) is det).

:- pred do_add_pred_marker(string::in, sym_name::in, arity::in,
    import_status::in, bool::in, term.context::in,
    add_marker_pred_info::in(add_marker_pred_info),
    module_info::in, module_info::out, list(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred module_mark_as_external(sym_name::in, int::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred maybe_check_field_access_function(module_info::in,
    sym_name::in, arity::in, import_status::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.make_hlds.add_class.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.add_mode.
:- import_module hlds.make_hlds.add_pragma.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.add_solver.
:- import_module hlds.make_hlds.add_special_pred.
:- import_module hlds.make_hlds.add_type.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_mutable.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module recompilation.

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

do_parse_tree_to_hlds(Globals, DumpBaseFileName, unit_module(Name, Items),
        MQInfo0, EqvMap, UsedModules, QualInfo, InvalidTypes, InvalidModes,
        !:ModuleInfo, !:Specs) :-
    mq_info_get_partial_qualifier_info(MQInfo0, PQInfo),
    module_info_init(Name, DumpBaseFileName, Items, Globals, PQInfo, no,
        !:ModuleInfo),
    module_info_set_used_modules(UsedModules, !ModuleInfo),
    !:Specs = [],
    add_item_list_decls_pass_1(Items,
        item_status(status_local, may_be_unqualified), !ModuleInfo,
        no, InvalidModes0, !Specs),
    globals.lookup_bool_option(Globals, statistics, Statistics),
    trace [io(!IO)] (
        maybe_write_string(Statistics, "% Processed all items in pass 1\n",
            !IO),
        maybe_report_stats(Statistics, !IO)
    ),

    add_item_list_decls_pass_2(Items,
        item_status(status_local, may_be_unqualified),
        !ModuleInfo, [], Pass2Specs),
    (
        Pass2Specs = [],
        InvalidTypes1 = no
    ;
        Pass2Specs = [_ | _],
        InvalidTypes1 = yes
    ),
    !:Specs = Pass2Specs ++ !.Specs,

    (
        InvalidTypes1 = no,
        some [!TypeTable] (
            % Figure out how arguments should be stored into fields
            % before constructors are added to the HLDS.
            module_info_get_type_table(!.ModuleInfo, !:TypeTable),
            foldl_over_type_ctor_defns(decide_du_type_layout(!.ModuleInfo),
                !.TypeTable, !TypeTable),
            module_info_set_type_table(!.TypeTable, !ModuleInfo),

            % Add constructors and special preds to the HLDS. This must be done
            % after adding all type and `:- pragma foreign_type' declarations.
            % If there were errors in foreign type type declarations, doing
            % this may cause a compiler abort.
            foldl3_over_type_ctor_defns(process_type_defn, !.TypeTable,
                no, InvalidTypes2, !ModuleInfo, !Specs)
        )
    ;
        InvalidTypes1 = yes,
        InvalidTypes2 = yes
    ),

    % Add the special preds for the builtin types which don't have a
    % type declaration, hence no hlds_type_defn is generated for them.
    (
        Name = mercury_public_builtin_module,
        compiler_generated_rtti_for_builtins(!.ModuleInfo)
    ->
        list.foldl(add_builtin_type_ctor_special_preds,
            builtin_type_ctors_with_no_hlds_type_defn, !ModuleInfo)
    ;
        true
    ),

    % Balance any data structures that need it.
    module_info_optimize(!ModuleInfo),
    trace [io(!IO)] (
        maybe_write_string(Statistics, "% Processed all items in pass 2\n",
            !IO),
        maybe_report_stats(Statistics, !IO)
    ),

    init_qual_info(MQInfo0, EqvMap, QualInfo0),
    add_item_list_pass_3(Items, status_local, !ModuleInfo, QualInfo0, QualInfo,
        !Specs),
    trace [io(!IO)] (
        maybe_write_string(Statistics, "% Processed all items in pass 3\n",
            !IO)
    ),

    qual_info_get_mq_info(QualInfo, MQInfo),
    mq_info_get_type_error_flag(MQInfo, InvalidTypes3),
    InvalidTypes = InvalidTypes1 `or` InvalidTypes2 `or` InvalidTypes3,
    mq_info_get_mode_error_flag(MQInfo, InvalidModes1),
    InvalidModes = InvalidModes0 `or` InvalidModes1.

%-----------------------------------------------------------------------------%

:- pred decide_du_type_layout(module_info::in, type_ctor::in,
    hlds_type_defn::in, type_table::in, type_table::out) is det.

decide_du_type_layout(ModuleInfo, TypeCtor, TypeDefn, !TypeTable) :-
    get_type_defn_body(TypeDefn, Body0),
    (
        Body0 = hlds_du_type(Ctors0, ConsTagValues, MaybeCheaperTagTest,
            DuKind, MaybeUserEqComp, DirectArgFunctors, ReservedTag,
            ReservedAddr, MaybeForeign),
        list.map(layout_du_ctor_args(ModuleInfo, DuKind), Ctors0, Ctors),
        Body = hlds_du_type(Ctors, ConsTagValues, MaybeCheaperTagTest,
            DuKind, MaybeUserEqComp, DirectArgFunctors, ReservedTag,
            ReservedAddr, MaybeForeign),
        set_type_defn_body(Body, TypeDefn, PackedTypeDefn),
        replace_type_ctor_defn(TypeCtor, PackedTypeDefn, !TypeTable)
    ;
        ( Body0 = hlds_eqv_type(_)
        ; Body0 = hlds_foreign_type(_)
        ; Body0 = hlds_solver_type(_, _)
        ; Body0 = hlds_abstract_type(_)
        )
        % Leave these types alone.
    ).

:- pred layout_du_ctor_args(module_info::in, du_type_kind::in,
    constructor::in, constructor::out) is det.

layout_du_ctor_args(ModuleInfo, DuKind, Ctor0, Ctor) :-
    Ctor0 = ctor(ExistTVars, Constraints, Name, Args0, Context),
    module_info_get_globals(ModuleInfo, Globals),
    (
        ( DuKind = du_type_kind_mercury_enum
        ; DuKind = du_type_kind_foreign_enum(_)
        ; DuKind = du_type_kind_direct_dummy
        ; DuKind = du_type_kind_notag(_, _, _)
        ),
        Args1 = Args0
    ;
        DuKind = du_type_kind_general,
        % A functor with a single float argument can have a double-width word
        % if it is not a no-tag functor. An example is `poly_type.f(float)'.
        ( use_double_word_floats(Globals, yes) ->
            set_double_word_floats(ModuleInfo, Args0, Args1)
        ;
            Args1 = Args0
        )
    ),
    globals.lookup_int_option(Globals, arg_pack_bits, ArgPackBits),
    ( ArgPackBits > 0 ->
        pack_du_ctor_args(ModuleInfo, ArgPackBits, 0, Args1, Args2, _),
        WorthPacking = worth_arg_packing(Args1, Args2),
        (
            WorthPacking = yes,
            Args = Args2
        ;
            WorthPacking = no,
            Args = Args1
        )
    ;
        Args = Args1
    ),
    Ctor = ctor(ExistTVars, Constraints, Name, Args, Context).

:- pred use_double_word_floats(globals::in, bool::out) is det.

use_double_word_floats(Globals, DoubleWordFloats) :-
    globals.lookup_bool_option(Globals, allow_double_word_fields,
        AllowDoubleWords),
    (
        AllowDoubleWords = yes,
        globals.lookup_int_option(Globals, bits_per_word, TargetWordBits),
        globals.lookup_bool_option(Globals, single_prec_float, SinglePrec),
        (
            TargetWordBits = 32,
            SinglePrec = no
        ->
            DoubleWordFloats = yes
        ;
            DoubleWordFloats = no
        )
    ;
        AllowDoubleWords = no,
        DoubleWordFloats = no
    ).

:- pred set_double_word_floats(module_info::in,
    list(constructor_arg)::in, list(constructor_arg)::out) is det.

set_double_word_floats(_ModuleInfo, [], []).
set_double_word_floats(ModuleInfo, [Arg0 | Args0], [Arg | Args]) :-
    Arg0 = ctor_arg(Name, Type, _, Context),
    ( type_is_float_eqv(ModuleInfo, Type) ->
        ArgWidth = double_word,
        Arg = ctor_arg(Name, Type, ArgWidth, Context)
    ;
        Arg = Arg0
    ),
    set_double_word_floats(ModuleInfo, Args0, Args).

:- pred pack_du_ctor_args(module_info::in, int::in, int::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    arg_width::out) is det.

pack_du_ctor_args(_ModuleInfo, _TargetWordBits, _Shift, [], [],
        full_word).
pack_du_ctor_args(ModuleInfo, TargetWordBits, Shift,
        [Arg0 | Args0], [Arg | Args], ArgWidth) :-
    Arg0 = ctor_arg(Name, Type, ArgWidth0, Context),
    ( type_is_enum_bits(ModuleInfo, Type, NumBits) ->
        Mask = int.pow(2, NumBits) - 1,
        % Try to place the argument in the current word, otherwise move on to
        % the next word.
        ( Shift + NumBits > TargetWordBits ->
            ArgWidth1 = partial_word_first(Mask),
            NextShift = NumBits
        ; Shift = 0 ->
            ArgWidth1 = partial_word_first(Mask),
            NextShift = NumBits
        ;
            ArgWidth1 = partial_word_shifted(Shift, Mask),
            NextShift = Shift + NumBits
        ),
        pack_du_ctor_args(ModuleInfo, TargetWordBits, NextShift, Args0, Args,
            NextArgWidth),
        % If this argument starts a word but the next argument is not packed
        % with it, then this argument is not packed.
        (
            ArgWidth1 = partial_word_first(_),
            NextArgWidth \= partial_word_shifted(_, _)
        ->
            ArgWidth = full_word
        ;
            ArgWidth = ArgWidth1
        ),
        Arg = ctor_arg(Name, Type, ArgWidth, Context)
    ;
        Arg = Arg0,
        ArgWidth = ArgWidth0,
        NextShift = 0,
        pack_du_ctor_args(ModuleInfo, TargetWordBits, NextShift, Args0, Args,
            _)
    ).

:- pred type_is_enum_bits(module_info::in, mer_type::in, int::out) is semidet.

type_is_enum_bits(ModuleInfo, Type, NumBits) :-
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    TypeCategory = classify_type_defn_body(TypeBody),
    (
        TypeCategory = ctor_cat_enum(cat_enum_mercury),
        NumBits = cons_tags_bits(TypeBody ^ du_type_cons_tag_values)
    ;
        TypeCategory = ctor_cat_user(cat_user_general),
        TypeBody = hlds_abstract_type(abstract_enum_type(NumBits))
    ).

:- func cons_tags_bits(cons_tag_values) = int.

cons_tags_bits(ConsTagValues) = NumBits :-
    map.foldl_values(max_int_tag, ConsTagValues, 0, MaxFunctor),
    int.log2(MaxFunctor + 1, NumBits).

:- pred max_int_tag(cons_tag::in, int::in, int::out) is det.

max_int_tag(ConsTag, !Max) :-
    ( ConsTag = int_tag(Int) ->
        int.max(Int, !Max)
    ;
        unexpected($module, $pred, "non-integer value for enumeration")
    ).

:- func worth_arg_packing(list(constructor_arg), list(constructor_arg)) = bool.

worth_arg_packing(UnpackedArgs, PackedArgs) = Worthwhile :-
    count_words(UnpackedArgs, 0, UnpackedLength),
    count_words(PackedArgs, 0, PackedLength),
    expect(PackedLength =< UnpackedLength, $module, $pred,
        "packed length exceeds unpacked length"),
    % Boehm GC will round up allocations (at least) to the next even number
    % of words. There is no point saving a single word if that word will be
    % allocated anyway.
    ( round_to_even(PackedLength) < round_to_even(UnpackedLength) ->
        Worthwhile = yes
    ;
        Worthwhile = no
    ).

:- pred count_words(list(constructor_arg)::in, int::in, int::out) is det.

count_words([], !Count).
count_words([Arg | Args], !Count) :-
    ArgWidth = Arg ^ arg_width,
    (
        ArgWidth = full_word,
        !:Count = !.Count + 1
    ;
        ArgWidth = double_word,
        !:Count = !.Count + 2
    ;
        ArgWidth = partial_word_first(_),
        !:Count = !.Count + 1
    ;
        ArgWidth = partial_word_shifted(_Shift, _Mask)
    ),
    count_words(Args, !Count).

:- func round_to_even(int) = int.

round_to_even(I) = (int.even(I) -> I ; I + 1).

%-----------------------------------------------------------------------------%

:- pred add_builtin_type_ctor_special_preds(type_ctor::in,
    module_info::in, module_info::out) is det.

add_builtin_type_ctor_special_preds(TypeCtor, !ModuleInfo) :-
    varset.init(TVarSet),
    Body = hlds_abstract_type(abstract_type_general),
    term.context_init(Context),
    Status = status_local,
    construct_type(TypeCtor, [], Type),

    % XXX We call `eagerly_add_special_preds' instead of `add_special_preds'
    % to bypass a call to `special_pred_is_generated_lazily' which calls
    % `classify_type_ctor'. `classify_type_ctor' knows about unqualified
    % builtin types, but not the qualified types like `builtin.int'/0 from
    % `builtin_type_ctors_with_no_hlds_type_defn'. Eventually it tries to
    % look up the builtin type from the type definition table, and aborts as
    % it won't find it.
    %
    % The special preds for these types shouldn't be generated lazily anyway.

    eagerly_add_special_preds(TVarSet, Type, TypeCtor, Body, Context, Status,
        !ModuleInfo).

%-----------------------------------------------------------------------------%

    % pass 1:
    % Add the declarations one by one to the module,
    % except for type definitions and pragmas.
    %
    % The `InvalidModes' bool records whether we detected
    % any cyclic insts or modes.
    %
:- pred add_item_list_decls_pass_1(list(item)::in, item_status::in,
    module_info::in, module_info::out, bool::in, bool::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_list_decls_pass_1([], _, !ModuleInfo, !InvalidModes, !Specs).
add_item_list_decls_pass_1([Item | Items], !.Status, !ModuleInfo,
        !InvalidModes, !Specs) :-
    add_item_decl_pass_1(Item, NewInvalidModes, !Status, !ModuleInfo, !Specs),
    !:InvalidModes = bool.or(!.InvalidModes, NewInvalidModes),
    add_item_list_decls_pass_1(Items, !.Status, !ModuleInfo, !InvalidModes,
        !Specs).

    % pass 2:
    % Add the type definitions and pragmas one by one to the module,
    % and add default modes for functions with no mode declaration.
    %
    % Adding type definitions needs to come after we have added the pred
    % declarations, since we need to have the pred_id for `index/2' and
    % `compare/3' when we add compiler-generated clauses for `compare/3'.
    % (And similarly for other compiler-generated predicates like that.)
    %
    % Adding pragmas needs to come after we have added the
    % pred declarations, in order to allow the pragma declarations
    % for a predicate to syntactically precede the pred declaration.
    %
    % Adding default modes for functions needs to come after we have
    % processed all the mode declarations, since otherwise we can't be
    % sure that there isn't a mode declaration for the function.
    %
:- pred add_item_list_decls_pass_2(list(item)::in, item_status::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_list_decls_pass_2([], _, !ModuleInfo, !Specs).
add_item_list_decls_pass_2([Item | Items], !.Status, !ModuleInfo, !Specs) :-
    add_item_decl_pass_2(Item, !Status, !ModuleInfo, !Specs),
    add_item_list_decls_pass_2(Items, !.Status, !ModuleInfo, !Specs).

    % pass 3:
    % Add the clauses one by one to the module.
    %
    % Check that the declarations for field extraction and update functions
    % are sensible.
    %
    % Check that predicates listed in `:- initialise' and `:- finalise'
    % declarations exist and have the correct signature, introduce
    % pragma foreign_export declarations for them and record their exported
    % name in the module_info so that we can generate code to call it at
    % initialisation/finalisation time.
    %
:- pred add_item_list_pass_3(list(item)::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_list_pass_3([], _Status, !ModuleInfo, !QualInfo, !Specs).
add_item_list_pass_3([Item | Items], Status0, !ModuleInfo, !QualInfo,
        !Specs) :-
    add_item_pass_3(Item, Status0, Status1, !ModuleInfo, !QualInfo, !Specs),
    add_item_list_pass_3(Items, Status1, !ModuleInfo, !QualInfo, !Specs).

%-----------------------------------------------------------------------------%

add_item_decl_pass_1(Item, FoundError, !Status, !ModuleInfo, !Specs) :-
    (
        Item = item_module_start(_),
        FoundError = no
    ;
        Item = item_module_end(_),
        FoundError = no
    ;
        Item = item_module_defn(ItemModuleDefn),
        add_pass_1_module_defn(ItemModuleDefn, !Status, !ModuleInfo, !Specs),
        FoundError = no
    ;
        Item = item_type_defn(ItemTypeDefnInfo),
        add_pass_1_type_defn(ItemTypeDefnInfo, !Status, !ModuleInfo, !Specs),
        FoundError = no
    ;
        Item = item_inst_defn(ItemInstDefnInfo),
        module_add_inst_defn(ItemInstDefnInfo, FoundError,
            !.Status, !ModuleInfo, !Specs)
    ;
        Item = item_mode_defn(ItemModeDefnInfo),
        module_add_mode_defn(ItemModeDefnInfo, FoundError,
            !.Status, !ModuleInfo, !Specs)
    ;
        Item = item_pred_decl(ItemPredDecl),
        add_pass_1_pred_decl(ItemPredDecl, !.Status, !ModuleInfo, !Specs),
        FoundError = no
    ;
        Item = item_mode_decl(ItemModeDecl),
        add_pass_1_mode_decl(ItemModeDecl, !.Status, !ModuleInfo, !Specs),
        FoundError = no
    ;
        Item = item_typeclass(ItemTypeClass),
        module_add_class_defn(ItemTypeClass, !.Status, !ModuleInfo, !Specs),
        FoundError = no
    ;
        Item = item_mutable(ItemMutable),
        add_pass_1_mutable(ItemMutable, !.Status, !ModuleInfo, !Specs),
        FoundError = no
    ;
        ( Item = item_clause(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_nothing(_)
        ),
        % These will be processed only in later passes.
        %
        % We don't want to add clauses or pragma foreign_procs before we add
        % the declarations of the predicates they implement.
        %
        % We don't want to add instance declarations before the typeclass
        % declaration it implements.
        FoundError = no
    ).

:- pred add_pass_1_type_defn(item_type_defn_info::in,
    item_status::in, item_status::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_1_type_defn(ItemTypeDefnInfo, !Status, !ModuleInfo, !Specs) :-
    % If this is a solver type then we need to also add the declarations
    % for the compiler generated construction function and deconstruction
    % predicate for the special constrained data constructor.
    %
    % In pass 3 we add the corresponding clauses.
    %
    % Before switch detection, we turn calls to these functions/predicates
    % into ordinary constructions/deconstructions, but preserve the
    % corresponding impurity annotations.
    ItemTypeDefnInfo = item_type_defn_info(TVarSet, SymName, TypeParams,
        TypeDefn, _Cond, Context, _SeqNum),
    ( TypeDefn = parse_tree_solver_type(SolverTypeDetails, _MaybeUserEqComp) ->
        add_solver_type_decl_items(TVarSet, SymName, TypeParams,
            SolverTypeDetails, Context, !Status, !ModuleInfo, !Specs),
        MutableItems = SolverTypeDetails ^ std_mutable_items,
        add_solver_type_mutable_items_pass_1(MutableItems, !.Status,
            !ModuleInfo, !Specs)
    ;
        true
    ).

:- pred add_pass_1_pred_decl(item_pred_decl_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_1_pred_decl(ItemPredDecl, Status, !ModuleInfo, !Specs) :-
    ItemPredDecl = item_pred_decl_info(Origin, TypeVarSet, InstVarSet,
        ExistQVars, PredOrFunc, PredName, TypesAndModes, _WithType, _WithInst,
        MaybeDet, _Cond, Purity, ClassContext, Context, _SeqNum),
    init_markers(Markers0),

    % If this predicate was added as a result of the mutable transformation
    % then mark this predicate as a mutable access pred. We do this so that
    % we can tell optimizations, like inlining, to treat it specially.
    (
        Origin = compiler(Reason),
        (
            Reason = mutable_decl,
            add_marker(marker_mutable_access_pred, Markers0, Markers)
        ;
            ( Reason = initialise_decl
            ; Reason = finalise_decl
            ; Reason = solver_type
            ; Reason = pragma_memo_attribute
            ; Reason = foreign_imports
            ),
            Markers = Markers0
        )
    ;
        Origin = user,
        Markers = Markers0
    ),
    module_add_pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
        PredOrFunc, PredName, TypesAndModes, MaybeDet, Purity, ClassContext,
        Markers, Context, Status, _, !ModuleInfo, !Specs).

:- pred add_pass_1_mode_decl(item_mode_decl_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_1_mode_decl(ItemModeDecl, Status, !ModuleInfo, !Specs) :-
    ItemModeDecl = item_mode_decl_info(VarSet, MaybePredOrFunc, PredName,
        Modes, _WithInst, MaybeDet, _Cond, Context, _SeqNum),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        Status = item_status(ImportStatus, _),
        IsClassMethod = no,
        module_add_mode(VarSet, PredName, Modes, MaybeDet, ImportStatus,
            Context, PredOrFunc, IsClassMethod, _, !ModuleInfo, !Specs)
    ;
        MaybePredOrFunc = no,
        % equiv_type.m should have either set the pred_or_func
        % or removed the item from the list.
        unexpected($module, $pred, "no pred_or_func on mode declaration")
    ).

:- pred add_pass_1_module_defn(item_module_defn_info::in,
    item_status::in, item_status::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_1_module_defn(ItemModuleDefn, !Status, !ModuleInfo, !Specs) :-
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, _SeqNum),
    ( module_defn_update_import_status(ModuleDefn, StatusPrime) ->
        !:Status = StatusPrime
    ;
        (
            ( ModuleDefn = md_import(ModuleSpecifiers)
            ; ModuleDefn = md_use(ModuleSpecifiers)
            ),
            !.Status = item_status(IStat, _),
            add_module_specifiers(ModuleSpecifiers, IStat, !ModuleInfo)
        ;
            ModuleDefn = md_external(MaybeBackend, External),
            ( External = name_arity(Name, Arity) ->
                module_info_get_globals(!.ModuleInfo, Globals),
                CurrentBackend = lookup_current_backend(Globals),
                (
                    (
                        MaybeBackend = no
                    ;
                        MaybeBackend = yes(Backend),
                        Backend = CurrentBackend
                    )
                ->
                    module_mark_as_external(Name, Arity, Context, !ModuleInfo,
                        !Specs)
                ;
                    true
                )
            ;
                Pieces = [words("Warning:"), quote("external"),
                    words("declaration requires arity."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            ( ModuleDefn = md_include_module(_)
            ; ModuleDefn = md_version_numbers(_, _)
            ; ModuleDefn = md_transitively_imported
            )
        ;
            ( ModuleDefn = md_interface
            ; ModuleDefn = md_implementation
            ; ModuleDefn = md_implementation_but_exported_to_submodules
            ; ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            ; ModuleDefn = md_opt_imported
            ; ModuleDefn = md_abstract_imported
            ),
            unexpected($module, $pred,
                "module_defn_update_import_status missed something")
        ;
            ModuleDefn = md_export(_),
            Pieces = [words("Warning: declaration not yet implemented."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_warning, phase_parse_tree_to_hlds,
                [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

:- pred add_pass_1_mutable(item_mutable_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_1_mutable(Item, Status, !ModuleInfo, !Specs) :-
    % We add the initialise decl and the foreign_decl on the second pass and
    % the foreign_proc clauses on the third pass.
    Item = item_mutable_info(Name, Type, _InitValue, Inst, MutAttrs,
        _MutVarset, Context, _SeqNum),
    Status = item_status(ImportStatus, _),
    DefinedThisModule = status_defined_in_this_module(ImportStatus),
    (
        DefinedThisModule = yes,
        module_info_get_name(!.ModuleInfo, ModuleName),

        % The predicate declarations we produce depends on the compilation
        % target, which use different source-to-source transformations for
        % mutables.
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.get_target(Globals, CompilationTarget),
        (
            CompilationTarget = target_c,
            WantPreInitDecl = yes,
            WantLockDecls = yes,
            WantUnsafeAccessDecls = yes
        ;
            CompilationTarget = target_csharp,
            IsThreadLocal = mutable_var_thread_local(MutAttrs),
            (
                IsThreadLocal = mutable_thread_local,
                WantPreInitDecl = yes
            ;
                IsThreadLocal = mutable_not_thread_local,
                WantPreInitDecl = no
            ),
            WantLockDecls = no,
            WantUnsafeAccessDecls = yes
        ;
            CompilationTarget = target_java,
            WantPreInitDecl = no,
            WantLockDecls = no,
            WantUnsafeAccessDecls = yes
        ;
            CompilationTarget = target_erlang,
            WantPreInitDecl = no,
            WantLockDecls = no,
            WantUnsafeAccessDecls = no
        ;
            ( CompilationTarget = target_il
            ; CompilationTarget = target_x86_64
            ),
            % Not supported yet.
            WantPreInitDecl = yes,
            WantLockDecls = yes,
            WantUnsafeAccessDecls = yes
        ),

        % Create the mutable initialisation predicate.
        InitPredDeclItem = mutable_init_pred_decl(ModuleName, Name, Context),
        add_item_decl_pass_1(InitPredDeclItem, _, Status, _,
            !ModuleInfo, !Specs),

        IsConstant = mutable_var_constant(MutAttrs),
        (
            IsConstant = no,

            % Create the pre-initialisation predicate. This is called
            % by the mutable initialisation predicate.
            (
                WantPreInitDecl = yes,
                PreInitPredDeclItem = mutable_pre_init_pred_decl(ModuleName,
                    Name, Context),
                add_item_decl_pass_1(PreInitPredDeclItem, _, Status, _,
                    !ModuleInfo, !Specs)
            ;
                WantPreInitDecl = no
            ),

            % Create the primitive access and locking predicates.
            (
                WantLockDecls = yes,
                LockPredDeclItem = lock_pred_decl(ModuleName, Name, Context),
                UnlockPredDecl = unlock_pred_decl(ModuleName, Name, Context),
                add_item_decl_pass_1(LockPredDeclItem, _, Status, _,
                    !ModuleInfo, !Specs),
                add_item_decl_pass_1(UnlockPredDecl, _, Status, _,
                    !ModuleInfo, !Specs)
            ;
                WantLockDecls = no
            ),
            (
                WantUnsafeAccessDecls = yes,
                UnsafeGetPredDeclItem = unsafe_get_pred_decl(ModuleName, Name,
                    Type, Inst, Context),
                UnsafeSetPredDeclItem = unsafe_set_pred_decl(ModuleName, Name,
                    Type, Inst, Context),
                add_item_decl_pass_1(UnsafeGetPredDeclItem, _, Status, _,
                    !ModuleInfo, !Specs),
                add_item_decl_pass_1(UnsafeSetPredDeclItem, _, Status, _,
                    !ModuleInfo, !Specs)
            ;
                WantUnsafeAccessDecls = no
            ),

            % Create the standard, non-pure access predicates. These are
            % always created for non-constant mutables, even if the
            % `attach_to_io_state' attribute has been specified.
            StdGetPredDeclItem = std_get_pred_decl(ModuleName, Name,
                Type, Inst, Context),
            StdSetPredDeclItem = std_set_pred_decl(ModuleName, Name,
                Type, Inst, Context),
            add_item_decl_pass_1(StdGetPredDeclItem, _, Status, _,
                !ModuleInfo, !Specs),
            add_item_decl_pass_1(StdSetPredDeclItem, _, Status, _,
                !ModuleInfo, !Specs),

            % If requested, create the pure access predicates using
            % the I/O state as well.
            CreateIOInterface = mutable_var_attach_to_io_state(MutAttrs),
            (
                CreateIOInterface = yes,
                IOGetPredDeclItem = io_get_pred_decl(ModuleName, Name,
                    Type, Inst, Context),
                IOSetPredDeclItem = io_set_pred_decl(ModuleName, Name,
                    Type, Inst, Context),
                add_item_decl_pass_1(IOGetPredDeclItem, _, Status, _,
                    !ModuleInfo, !Specs),
                add_item_decl_pass_1(IOSetPredDeclItem, _, Status, _,
                    !ModuleInfo, !Specs)
            ;
                CreateIOInterface = no
            )
        ;
            IsConstant = yes,

            % We create the "get" access predicate, which is pure since
            % it always returns the same value, but we must also create
            % a secret "set" predicate for use by the initialization code.
            ConstantGetPredDeclItem = constant_get_pred_decl(ModuleName, Name,
                Type, Inst, Context),
            ConstantSetPredDeclItem = constant_set_pred_decl(ModuleName, Name,
                Type, Inst, Context),
            add_item_decl_pass_1(ConstantGetPredDeclItem, _, Status, _,
                !ModuleInfo, !Specs),
            add_item_decl_pass_1(ConstantSetPredDeclItem, _, Status, _,
                !ModuleInfo, !Specs)
        )
    ;
        DefinedThisModule = no
    ).

:- pred add_solver_type_mutable_items_pass_1(list(item_mutable_info)::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_solver_type_mutable_items_pass_1([], _, !ModuleInfo, !Specs).
add_solver_type_mutable_items_pass_1([MutableInfo | MutableInfos], Status,
        !ModuleInfo, !Specs) :-
    add_pass_1_mutable(MutableInfo, Status, !ModuleInfo, !Specs),
    add_solver_type_mutable_items_pass_1(MutableInfos, Status,
        !ModuleInfo, !Specs).

:- pred add_module_specifiers(list(module_specifier)::in, import_status::in,
    module_info::in, module_info::out) is det.

add_module_specifiers(Specifiers, IStat, !ModuleInfo) :-
    ( status_defined_in_this_module(IStat) = yes ->
        module_add_imported_module_specifiers(IStat, Specifiers, !ModuleInfo)
    ; IStat = status_imported(import_locn_ancestor_private_interface_proper) ->
        module_add_imported_module_specifiers(IStat, Specifiers, !ModuleInfo),

            % Any import_module which comes from a private interface
            % must by definition be a module used by the parent module.
        module_info_add_parents_to_used_modules(Specifiers, !ModuleInfo)
    ;
        module_add_indirectly_imported_module_specifiers(Specifiers,
            !ModuleInfo)
    ).

%-----------------------------------------------------------------------------%

:- pred add_item_decl_pass_2(item::in,
    item_status::in, item_status::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_decl_pass_2(Item, !Status, !ModuleInfo, !Specs) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _SeqNum),
        ( module_defn_update_import_status(ModuleDefn, NewStatus) ->
            !:Status = NewStatus
        ;
            true
        )
    ;
        Item = item_type_defn(ItemTypeDefn),
        add_pass_2_type_defn(ItemTypeDefn, !.Status, !ModuleInfo, !Specs)
    ;
        Item = item_pred_decl(ItemPredDecl),
        add_pass_2_pred_decl(ItemPredDecl, !.Status, !ModuleInfo, !Specs)
    ;
        Item = item_pragma(ItemPragma),
        add_pass_2_pragma(ItemPragma, !Status, !ModuleInfo, !Specs)
    ;
        Item = item_instance(ItemInstance),
        add_pass_2_instance(ItemInstance, !.Status, !ModuleInfo, !Specs)
    ;
        Item = item_initialise(ItemInitialise),
        add_pass_2_initialise(ItemInitialise, !.Status, !ModuleInfo, !Specs)
    ;
        Item = item_finalise(ItemFinalise),
        add_pass_2_finalise(ItemFinalise, !.Status, !ModuleInfo, !Specs)
    ;
        Item = item_mutable(ItemMutable),
        add_pass_2_mutable(ItemMutable, !.Status, !ModuleInfo, !Specs)
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_clause(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_nothing(_)
        )
        % Do nothing in pass 2 for these kinds of items.
    ).

:- pred add_pass_2_type_defn(item_type_defn_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_type_defn(ItemTypeDefn, Status, !ModuleInfo, !Specs) :-
    ItemTypeDefn = item_type_defn_info(VarSet, Name, Args, TypeDefn, Cond,
        Context, _SeqNum),
    module_add_type_defn(VarSet, Name, Args, TypeDefn, Cond, Context,
        Status, !ModuleInfo, !Specs),
    ( TypeDefn = parse_tree_solver_type(SolverTypeDetails, _MaybeUserEqComp) ->
        MutableItems = SolverTypeDetails ^ std_mutable_items,
        add_solver_type_mutable_items_pass_2(MutableItems, Status,
            !ModuleInfo, !Specs)
    ;
        true
    ).

:- pred add_pass_2_pred_decl(item_pred_decl_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_pred_decl(ItemPredDecl, _Status, !ModuleInfo, !Specs) :-
    ItemPredDecl = item_pred_decl_info(_Origin, _TypeVarSet, _InstVarSet,
        _ExistQVars, PredOrFunc, SymName, TypesAndModes, _WithType, _WithInst,
        _MaybeDet, _Cond, _Purity, _ClassContext, _Context, _SeqNum),
    % Add default modes for function declarations, if necessary.
    (
        PredOrFunc = pf_predicate
    ;
        PredOrFunc = pf_function,
        list.length(TypesAndModes, Arity),
        adjust_func_arity(pf_function, FuncArity, Arity),
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        predicate_table_lookup_func_sym_arity(PredTable0,
            is_fully_qualified, SymName, FuncArity, PredIds),
        (
            PredIds = [_ | _],
            predicate_table_get_preds(PredTable0, Preds0),
            maybe_add_default_func_modes(PredIds, Preds0, Preds),
            predicate_table_set_preds(Preds, PredTable0, PredTable),
            module_info_set_predicate_table(PredTable, !ModuleInfo)
        ;
            PredIds = [],
            unexpected($module, $pred, "can't find func declaration")
        )
    ).

:- pred add_pass_2_instance(item_instance_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_instance(ItemInstance, Status, !ModuleInfo, !Specs) :-
    ItemInstance = item_instance_info(Constraints, Name, Types, OriginalTypes,
        Body, VarSet, InstanceModuleName, Context, _SeqNum),
    Status = item_status(ImportStatus, _),
    (
        Body = instance_body_abstract,
        make_status_abstract(ImportStatus, BodyStatus)
    ;
        Body = instance_body_concrete(_),
        BodyStatus = ImportStatus
    ),
    module_add_instance_defn(InstanceModuleName, Constraints, Name,
        Types, OriginalTypes, Body, VarSet, BodyStatus, Context,
        !ModuleInfo, !Specs).

:- pred add_pass_2_initialise(item_initialise_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_initialise(ItemInitialise, Status, !ModuleInfo, !Specs) :-
    % These are processed properly during pass 3, we just do some
    % error checking at this point.
    ItemInitialise = item_initialise_info(Origin, _, _, Context, _SeqNum),
    Status = item_status(ImportStatus, _),
    ( ImportStatus = status_exported ->
        (
            Origin = user,
            error_is_exported(Context, "`initialise' declaration", !Specs)
        ;
            Origin = compiler(Details),
            (
                % Ignore the error if this initialise declaration was
                % introduced because of a mutable declaration.
                Details = mutable_decl
            ;
                ( Details = initialise_decl
                ; Details = finalise_decl
                ; Details = solver_type
                ; Details = foreign_imports
                ; Details = pragma_memo_attribute
                ),
                unexpected($module, $pred,
                    "Bad introduced initialise declaration")
            )
        )
    ;
        true
    ).

:- pred add_pass_2_finalise(item_finalise_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_finalise(ItemFinalise, Status, !ModuleInfo, !Specs) :-
    % There are processed properly during pass 3, we just do some error
    % checking at this point.
    ItemFinalise = item_finalise_info(Origin, _, _, Context, _SeqNum),
    Status = item_status(ImportStatus, _),
    ( ImportStatus = status_exported ->
        (
            Origin = user,
            error_is_exported(Context, "`finalise' declaration", !Specs)
        ;
            % There are no source-to-source transformations that introduce
            % finalise declarations.
            Origin = compiler(_),
            unexpected($module, $pred, "Bad introduced finalise declaration.")
        )
    ;
        true
    ).

:- pred add_pass_2_mutable(item_mutable_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_mutable(ItemMutable, Status, !ModuleInfo, !Specs) :-
    ItemMutable = item_mutable_info(Name, _Type, _InitTerm, Inst,
        MutAttrs, _MutVarset, Context, _SeqNum),
    Status = item_status(ImportStatus, _),
    ( ImportStatus = status_exported ->
        error_is_exported(Context, "`mutable' declaration", !Specs)
    ;
        true
    ),

    % We don't implement the `mutable' declaration unless it is defined in
    % this module. Not having this check means that we might end up up
    % duplicating the definition of the global variable in any submodules.

    DefinedThisModule = status_defined_in_this_module(ImportStatus),
    (
        DefinedThisModule = yes,
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.get_target(Globals, CompilationTarget),

        % XXX We don't currently support the foreign_name attribute
        % for all languages.
        (
            (
                CompilationTarget = target_c,
                ForeignLanguage = lang_c
            ;
                CompilationTarget = target_java,
                ForeignLanguage = lang_java
            ;
                CompilationTarget = target_csharp,
                ForeignLanguage = lang_csharp
            ;
                CompilationTarget = target_erlang,
                ForeignLanguage = lang_erlang
            ),
            mutable_var_maybe_foreign_names(MutAttrs) = MaybeForeignNames,
            module_info_get_name(!.ModuleInfo, ModuleName),
            (
                MaybeForeignNames = no
            ;
                MaybeForeignNames = yes(ForeignNames),

                % Report any errors with the foreign_name attributes
                % during this pass.
                ReportErrors = yes,
                get_global_name_from_foreign_names(!.ModuleInfo, ReportErrors,
                    Context, ModuleName, Name, ForeignLanguage, ForeignNames,
                    _TargetMutableName, !Specs)
            ),

            % If we are creating the I/O version of the set predicate then we
            % need to add a promise_pure pragma for it. This needs to be done
            % here (in stage 2) rather than in stage 3 where the rest of the
            % mutable transformation is.

            IOStateInterface = mutable_var_attach_to_io_state(MutAttrs),
            (
                IOStateInterface = yes,
                SetPredName = mutable_set_pred_sym_name(ModuleName, Name),
                SetPredNameArity = pred_name_arity(SetPredName, 3),
                IOSetPromisePurePragma = pragma_promise_pure(SetPredNameArity),
                IOSetPromisePureItemPragma = item_pragma_info(
                    compiler(mutable_decl), IOSetPromisePurePragma, Context,
                    -1),
                add_pass_2_pragma(IOSetPromisePureItemPragma, Status, _,
                    !ModuleInfo, !Specs)
            ;
                IOStateInterface = no
            )
        ;
            ( CompilationTarget = target_il
            ; CompilationTarget = target_x86_64
            ),
            Pieces = [words("Error: foreign_name mutable attribute not yet"),
                words("implemented for the"),
                fixed(compilation_target_string(CompilationTarget)),
                words("backend."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ),

        % Check that the inst in the mutable declaration is a valid inst for a
        % mutable declaration.
        ( is_valid_mutable_inst(!.ModuleInfo, Inst) ->
            true
        ;
            % It is okay to pass a dummy varset in here since any attempt
            % to use inst variables in a mutable declaration should already
            % been dealt with when the mutable declaration was parsed.
            DummyInstVarset = varset.init,
            InstStr = mercury_expanded_inst_to_string(Inst, DummyInstVarset,
                !.ModuleInfo),
            InvalidInstPieces = [
                words("Error: the inst"),
                quote(InstStr),
                words("is not a valid inst for a mutable declaration.")
            ],
            % XXX We could provide more information about exactly *why* the
            % inst was not valid here as well.
            InvalidInstMsg = simple_msg(Context, [always(InvalidInstPieces)]),
            InvalidInstSpec = error_spec(severity_error,
                phase_parse_tree_to_hlds, [InvalidInstMsg]),
            !:Specs = [ InvalidInstSpec | !.Specs ]
        )
    ;
        DefinedThisModule = no
    ).

:- pred add_solver_type_mutable_items_pass_2(list(item_mutable_info)::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_solver_type_mutable_items_pass_2([], _, !ModuleInfo, !Specs).
add_solver_type_mutable_items_pass_2([MutableInfo | MutableInfos], Status,
        !ModuleInfo, !Specs) :-
    add_pass_2_mutable(MutableInfo, Status, !ModuleInfo, !Specs),
    add_solver_type_mutable_items_pass_2(MutableInfos, Status,
        !ModuleInfo, !Specs).

    % Check to see if there is a valid foreign_name attribute for this backend.
    % If so, use it as the name of the global variable in the target code,
    % otherwise take the Mercury name for the mutable and mangle it into
    % an appropriate variable name.
    %
 :- pred get_global_name_from_foreign_names(module_info::in, bool::in,
    prog_context::in, module_name::in, string::in, foreign_language::in,
    list(foreign_name)::in, string::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_global_name_from_foreign_names(ModuleInfo, ReportErrors, Context,
        ModuleName, MercuryMutableName, ForeignLanguage, ForeignNames,
        TargetMutableName, !Specs) :-
    solutions(get_matching_foreign_name(ForeignNames, ForeignLanguage),
        TargetMutableNames),
    (
        TargetMutableNames = [],
        % This works for Erlang as well.
        TargetMutableName = mutable_c_var_name(ModuleName, MercuryMutableName)
    ;
        TargetMutableNames = [foreign_name(_, TargetMutableName)]
        % XXX We should really check that this is a valid identifier
        % in the target language here.
    ;
        TargetMutableNames = [_, _ | _],
        (
            ReportErrors = yes,
            module_info_get_globals(ModuleInfo, Globals),
            globals.get_target(Globals, CompilationTarget),
            Pieces = [words("Error: multiple foreign_name attributes"),
                words("specified for the"),
                fixed(compilation_target_string(CompilationTarget)),
                words("backend."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            ReportErrors = no
        ),
        % This works for Erlang as well.
        TargetMutableName = mutable_c_var_name(ModuleName, MercuryMutableName)
    ).

:- pred get_matching_foreign_name(list(foreign_name)::in,
    foreign_language::in, foreign_name::out) is nondet.

get_matching_foreign_name(ForeignNames, ForeignLanguage, ForeignName) :-
    list.member(ForeignName, ForeignNames),
    ForeignName = foreign_name(ForeignLanguage, _).

%-----------------------------------------------------------------------------%

add_item_pass_3(Item, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        add_pass_3_module_defn(ItemModuleDefn, !Status, !ModuleInfo, !QualInfo,
            !Specs)
    ;
        Item = item_clause(ItemClause),
        add_pass_3_clause(ItemClause, !.Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        Item = item_type_defn(ItemTypeDefn),
        add_pass_3_type_defn(ItemTypeDefn, !.Status, !ModuleInfo, !QualInfo,
            !Specs)
    ;
        Item = item_pred_decl(ItemPredDecl),
        add_pass_3_pred_decl(ItemPredDecl, !.Status, !ModuleInfo, !QualInfo,
            !Specs)
    ;
        Item = item_pragma(ItemPragma),
        add_pass_3_pragma(ItemPragma, !Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        Item = item_promise(ItemPromise),
        add_pass_3_promise(ItemPromise, !.Status, !ModuleInfo, !QualInfo,
            !Specs)
    ;
        Item = item_initialise(ItemInitialise),
        add_pass_3_initialise(ItemInitialise, !.Status, !ModuleInfo, !QualInfo,
            !Specs)
    ;
        Item = item_finalise(ItemFinalise),
        add_pass_3_finalise(ItemFinalise, !.Status, !ModuleInfo, !QualInfo,
            !Specs)
    ;
        Item = item_mutable(ItemMutable),
        add_pass_3_mutable(ItemMutable, !.Status, !ModuleInfo, !QualInfo,
            !Specs)
    ;
        ( Item = item_module_start(_)
        ; Item = item_module_end(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_mode_decl(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_nothing(_)
        )
        % Do nothing.
    ).

:- pred add_pass_3_clause(item_clause_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_clause(ItemClause, Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemClause = item_clause_info(Origin, VarSet, PredOrFunc,
        PredName, Args, Body, Context, SeqNum),
    ( Status = status_exported ->
        (
            Origin = user,
            list.length(Args, Arity),

            % There is no point printing out the qualified name since that
            % information is already in the context.
            UnqualifiedPredName = unqualify_name(PredName),
            ClauseId = simple_call_id_to_string(PredOrFunc,
                unqualified(UnqualifiedPredName) / Arity),
            error_is_exported(Context, "clause for " ++ ClauseId, !Specs)
        ;
            Origin = compiler(Details),
            (
                % Ignore clauses that are introduced as a result of
                % `initialise', `finalise' or `mutable' declarations
                % or pragma memos.
                ( Details = initialise_decl
                ; Details = finalise_decl
                ; Details = mutable_decl
                ; Details = pragma_memo_attribute
                )
            ;
                ( Details = solver_type
                ; Details = foreign_imports
                ),
                unexpected($module, $pred, "Bad introduced clauses")
            )
        )
    ;
        true
    ),
    % At this stage we only need know that it's not a promise declaration.
    module_add_clause(VarSet, PredOrFunc, PredName, Args, Body, Status,
        Context, yes(SeqNum), goal_type_none, !ModuleInfo, !QualInfo, !Specs).

:- pred add_pass_3_type_defn(item_type_defn_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_type_defn(ItemTypeDefn, Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemTypeDefn = item_type_defn_info(_TVarSet, SymName, TypeParams, TypeDefn,
        _Cond, Context, _SeqNum),
    % If this is a solver type, then we need to also add the clauses for
    % the compiler generated inst cast predicate (the declaration for which
    % was added in pass 1). We should only add the clauses if this is the
    % module in which the solver type was defined though.
    (
        TypeDefn = parse_tree_solver_type(SolverTypeDetails, _MaybeUserEqComp),
        status_defined_in_this_module(Status) = yes
    ->
        add_solver_type_clause_items(SymName, TypeParams, SolverTypeDetails,
            Context, Status, _, !ModuleInfo, !QualInfo, !Specs),
        MutableItems = SolverTypeDetails ^ std_mutable_items,
        add_solver_type_mutable_items_clauses(MutableItems, Status, _,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        true
    ).

:- pred add_pass_3_pred_decl(item_pred_decl_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_pred_decl(ItemPredDecl, Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemPredDecl = item_pred_decl_info(_, _, _, _, PredOrFunc, SymName,
        TypesAndModes, _WithType, _WithInst, _, _, _, _, Context, _SeqNum),
    (
        PredOrFunc = pf_predicate
    ;
        PredOrFunc = pf_function,
        list.length(TypesAndModes, PredArity),
        adjust_func_arity(pf_function, FuncArity, PredArity),
        maybe_check_field_access_function(!.ModuleInfo, SymName, FuncArity,
            Status, Context, !Specs)
    ).

:- pred add_pass_3_module_defn(item_module_defn_info::in,
    import_status::in, import_status::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_module_defn(ItemModuleDefn, !Status, !ModuleInfo, !QualInfo,
        !Specs) :-
    ItemModuleDefn = item_module_defn_info(ModuleDefn, _Context, _SeqNum),
    ( ModuleDefn = md_version_numbers(ModuleName, ModuleVersionNumbers) ->
        % Record the version numbers for each imported module
        % if smart recompilation is enabled.
        RecordPred = (pred(RecompInfo0::in, RecompInfo::out) is det :-
            RecompInfo = RecompInfo0 ^ version_numbers ^
                map.elem(ModuleName) := ModuleVersionNumbers
        ),
        apply_to_recompilation_info(RecordPred, !QualInfo)
    ; module_defn_update_import_status(ModuleDefn, ItemStatus1) ->
        ItemStatus1 = item_status(!:Status, NeedQual),
        qual_info_get_mq_info(!.QualInfo, MQInfo0),
        mq_info_set_need_qual_flag(NeedQual, MQInfo0, MQInfo),
        qual_info_set_mq_info(MQInfo, !QualInfo)
    ;
        true
    ).

:- pred add_pass_3_promise(item_promise_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_promise(ItemPromise, Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemPromise = item_promise_info(PromiseType, Goal, VarSet, UnivVars,
        Context, _SeqNum),
    % If the outermost universally quantified variables are placed in the head
    % of the dummy predicate, the typechecker will avoid warning about unbound
    % type variables as this implicitly adds a universal quantification of the
    % type variables needed.
    term.var_list_to_term_list(UnivVars, HeadVars),
    (
        % Extra error checking for promise ex declarations.
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exhaustive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        check_promise_ex_decl(UnivVars, PromiseType, Goal, Context, !Specs)
    ;
        PromiseType = promise_type_true
    ),
    % Add as dummy predicate.
    add_promise_clause(PromiseType, HeadVars, VarSet, Goal, Context,
        Status, !ModuleInfo, !QualInfo, !Specs).

:- pred add_pass_3_initialise(item_initialise_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_initialise(ItemInitialise, Status, !ModuleInfo, !QualInfo,
        !Specs) :-
    ItemInitialise = item_initialise_info(Origin, SymName, Arity, Context,
        _SeqNum),
    Origin = user,

    % To handle a `:- initialise initpred.' declaration for C backends we need
    % to:
    % (1) construct a new C function name, CName, to use to export initpred,
    % (2) add the export pragma that does this
    % (3) record the initpred/cname pair in the ModuleInfo so that
    % code generation can ensure cname is called during module initialisation.
    %
    % For the Erlang backend, we need to have the initpred recorded in the
    % ModuleInfo. This is implied by the handling for the C backends.

    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    predicate_table_lookup_pred_sym_arity(PredTable,
        may_be_partially_qualified, SymName, Arity, PredIds),
    (
        PredIds = [],
        Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
            words("used in initialise declaration"),
            words("does not have a corresponding pred declaration."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],
            PredId = HeadPredId,
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            pred_info_get_arg_types(PredInfo, ArgTypes),
            pred_info_get_procedures(PredInfo, ProcTable),
            ProcInfos = map.values(ProcTable),
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.get_target(Globals, CompilationTarget),
            ExportLang = target_lang_to_foreign_export_lang(CompilationTarget),
            (
                ArgTypes = [Arg1Type, Arg2Type],
                type_is_io_state(Arg1Type),
                type_is_io_state(Arg2Type),
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo,
                    MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [ di_mode, uo_mode ],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det ; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_pure
            ->
                module_info_new_user_init_pred(SymName, Arity, CName,
                    !ModuleInfo),
                PredNameModesPF = pred_name_modes_pf(SymName,
                    [di_mode, uo_mode], pf_predicate),
                FPEInfo = pragma_info_foreign_proc_export(ExportLang,
                    PredNameModesPF, CName),
                ExportPragma = pragma_foreign_proc_export(FPEInfo),
                ExportItemPragma = item_pragma_info(compiler(initialise_decl),
                    ExportPragma, Context, -1),
                ExportItem = item_pragma(ExportItemPragma),
                add_item_pass_3(ExportItem, Status, _,
                    !ModuleInfo, !QualInfo, !Specs)
            ;
                ArgTypes = [],
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo,
                    MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det ; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_impure
            ->
                module_info_new_user_init_pred(SymName, Arity, CName,
                    !ModuleInfo),
                PredNameModesPF = pred_name_modes_pf(SymName, [],
                    pf_predicate),
                FPEInfo = pragma_info_foreign_proc_export(ExportLang,
                    PredNameModesPF, CName),
                ExportPragma = pragma_foreign_proc_export(FPEInfo),
                ExportItemPragma = item_pragma_info(compiler(initialise_decl),
                    ExportPragma, Context, -1),
                ExportItem = item_pragma(ExportItemPragma),
                add_item_pass_3(ExportItem, Status, _,
                    !ModuleInfo, !QualInfo, !Specs)
            ;
                Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                    words("used in initialise declaration has"),
                    words("invalid signature."), nl],
                % TODO: provide verbose error information here.
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            TailPredIds = [_ | _],
            Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                words("used in initialise declaration"),
                words("multiple pred declarations."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).
add_pass_3_initialise(ItemInitialise, Status, !ModuleInfo, !QualInfo,
        !Specs) :-
    ItemInitialise = item_initialise_info(Origin, SymName, Arity, Context,
        _SeqNum),
    Origin = compiler(Details),

    % The compiler introduces initialise declarations that call impure
    % predicates as part of the source-to-source transformation for mutable
    % variables. These predicates *must* be impure in order to prevent the
    % compiler optimizing them away.

    (
        Details = mutable_decl,
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.get_target(Globals, CompilationTarget),
        (
            CompilationTarget = target_c,
            MaybeExportLang = yes(lang_c)
        ;
            CompilationTarget = target_java,
            MaybeExportLang = yes(lang_java)
        ;
            CompilationTarget = target_csharp,
            MaybeExportLang = yes(lang_csharp)
        ;
            CompilationTarget = target_erlang,
            MaybeExportLang = yes(lang_erlang)
        ;
            % Untested.
            ( CompilationTarget = target_il
            ; CompilationTarget = target_x86_64
            ),
            MaybeExportLang = no
        ),
        (
            MaybeExportLang = yes(ExportLang),
            module_info_new_user_init_pred(SymName, Arity, CName, !ModuleInfo),
            PredNameModesPF = pred_name_modes_pf(SymName, [], pf_predicate),
            FPEInfo = pragma_info_foreign_proc_export(ExportLang,
                PredNameModesPF, CName),
            ExportPragma = pragma_foreign_proc_export(FPEInfo),
            ExportItemPragma = item_pragma_info(compiler(mutable_decl),
                ExportPragma, Context, -1),
            ExportItem = item_pragma(ExportItemPragma),
            add_item_pass_3(ExportItem, Status, _,
                !ModuleInfo, !QualInfo, !Specs)
        ;
            MaybeExportLang = no
        )
    ;
        ( Details = initialise_decl
        ; Details = finalise_decl
        ; Details = solver_type
        ; Details = pragma_memo_attribute
        ; Details = foreign_imports
        ),
        unexpected($module, $pred, "Bad introduced initialise declaration")
    ).

:- pred add_pass_3_finalise(item_finalise_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_finalise(ItemFinalise, Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemFinalise = item_finalise_info(Origin, SymName, Arity, Context,
        _SeqNum),

    % To handle a `:- finalise finalpred.' declaration for C backends we need
    % to:
    % (1) construct a new C function name, CName, to use to export finalpred,
    % (2) add `:- pragma foreign_export("C", finalpred(di, uo), CName).',
    % (3) record the finalpred/cname pair in the ModuleInfo so that
    % code generation can ensure cname is called during module finalisation.
    %
    % For the Erlang backend, we need to have the finalpred recorded in the
    % ModuleInfo. This is implied by the handling for the C backends.

    (
        Origin = compiler(_),
        unexpected($module, $pred, "Bad introduced finalise declaration")
    ;
        Origin = user
    ),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    predicate_table_lookup_pred_sym_arity(PredTable,
        may_be_partially_qualified, SymName, Arity, PredIds),
    (
        PredIds = [],
        Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
            words("used in finalise declaration"),
            words("does not have a corresponding pred declaration."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],
            PredId = HeadPredId,
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            pred_info_get_arg_types(PredInfo, ArgTypes),
            pred_info_get_procedures(PredInfo, ProcTable),
            ProcInfos = map.values(ProcTable),
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.get_target(Globals, CompilationTarget),
            ExportLang = target_lang_to_foreign_export_lang(CompilationTarget),
            (
                ArgTypes = [Arg1Type, Arg2Type],
                type_is_io_state(Arg1Type),
                type_is_io_state(Arg2Type),
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo,
                    MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [ di_mode, uo_mode ],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det ; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_pure
            ->
                module_info_new_user_final_pred(SymName, Arity, CName,
                    !ModuleInfo),
                PredNameModesPF = pred_name_modes_pf(SymName,
                    [di_mode, uo_mode], pf_predicate),
                FPEInfo = pragma_info_foreign_proc_export(ExportLang,
                    PredNameModesPF, CName),
                ExportPragma = pragma_foreign_proc_export(FPEInfo),
                ExportItemPragma = item_pragma_info(compiler(finalise_decl),
                    ExportPragma, Context, -1),
                ExportItem = item_pragma(ExportItemPragma),
                add_item_pass_3(ExportItem, Status, _,
                    !ModuleInfo, !QualInfo, !Specs)
            ;
                ArgTypes = [],
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo,
                    MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_impure
            ->
                module_info_new_user_final_pred(SymName, Arity, CName,
                    !ModuleInfo),
                PredNameModesPF = pred_name_modes_pf(SymName,
                    [], pf_predicate),
                FPEInfo = pragma_info_foreign_proc_export(ExportLang,
                    PredNameModesPF, CName),
                ExportPragma = pragma_foreign_proc_export(FPEInfo),
                ExportItemPragma = item_pragma_info(compiler(finalise_decl),
                    ExportPragma, Context, -1),
                ExportItem = item_pragma(ExportItemPragma),
                add_item_pass_3(ExportItem, Status, _,
                    !ModuleInfo, !QualInfo, !Specs)
            ;
                Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                    words("used in finalise declaration"),
                    words("has invalid signature."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            TailPredIds = [_ | _],
            Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                words("used in finalise declaration"),
                words("has multiple pred declarations."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

:- func target_lang_to_foreign_export_lang(compilation_target)
    = foreign_language.

target_lang_to_foreign_export_lang(CompilationTarget) = ExportLang :-
    (
        ( CompilationTarget = target_c
        ; CompilationTarget = target_x86_64
        ),
        ExportLang = lang_c
    ;
        CompilationTarget = target_erlang,
        ExportLang = lang_erlang
    ;
        CompilationTarget = target_il,
        ExportLang = lang_il
    ;
        CompilationTarget = target_csharp,
        ExportLang = lang_csharp
    ;
        CompilationTarget = target_java,
        ExportLang = lang_java
    ).

:- pred add_pass_3_mutable(item_mutable_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_mutable(ItemMutable, Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemMutable = item_mutable_info(MercuryMutableName, Type, _InitTerm, _Inst,
        MutAttrs, _MutVarset, Context, _SeqNum),
    IsConstant = mutable_var_constant(MutAttrs),

    % The transformation here is documented in the comments at the
    % beginning of prog_mutable.m.

    DefinedThisModule = status_defined_in_this_module(Status),
    (
        DefinedThisModule = yes,
        module_info_get_name(!.ModuleInfo, ModuleName),

        module_info_get_globals(!.ModuleInfo, Globals),
        globals.get_target(Globals, CompilationTarget),
        (
            CompilationTarget = target_c,

            % Work out what name to give the global in the target language.
            decide_mutable_target_var_name(!.ModuleInfo, MutAttrs,
                ModuleName, MercuryMutableName, lang_c, Context,
                TargetMutableName, !Specs),

            % Add foreign_decl and foreign_code items that declare/define the
            % global variable used to implement the mutable. If the mutable is
            % not constant then add a mutex to synchronize access to it as
            % well.
            IsThreadLocal = mutable_var_thread_local(MutAttrs),
            add_c_mutable_defn_and_decl(TargetMutableName, Type, IsConstant,
                IsThreadLocal, Context, !ModuleInfo, !QualInfo, !Specs),

            % Add all the predicates related to mutables.
            add_c_mutable_preds(ItemMutable, TargetMutableName,
                Status, _, !ModuleInfo, !QualInfo, !Specs)
        ;
            (
                CompilationTarget = target_java,
                Lang = lang_java
            ;
                CompilationTarget = target_csharp,
                Lang = lang_csharp
            ),

            % Work out what name to give the global in the target language.
            decide_mutable_target_var_name(!.ModuleInfo, MutAttrs,
                ModuleName, MercuryMutableName, Lang, Context,
                TargetMutableName, !Specs),

            % Add foreign_code item that defines the global variable used to
            % implement the mutable.
            IsThreadLocal = mutable_var_thread_local(MutAttrs),
            add_csharp_java_mutable_defn(Lang, TargetMutableName, Type,
                IsThreadLocal, Context, !ModuleInfo, !QualInfo, !Specs),

            % Add all the predicates related to mutables.
            add_csharp_java_mutable_preds(ItemMutable, Lang, TargetMutableName,
                Status, _, !ModuleInfo, !QualInfo, !Specs)
        ;
            CompilationTarget = target_erlang,

            % Work out what name to give the global in the target language.
            decide_mutable_target_var_name(!.ModuleInfo, MutAttrs,
                ModuleName, MercuryMutableName, lang_erlang, Context,
                TargetMutableName, !Specs),

            % Add all the predicates related to mutables.
            add_erlang_mutable_preds(ItemMutable, TargetMutableName,
                Status, _, !ModuleInfo, !QualInfo, !Specs)
        ;
            ( CompilationTarget = target_il
            ; CompilationTarget = target_x86_64
            )
            % Not supported yet.
        )
    ;
        DefinedThisModule = no
    ).

    % Decide what the name of the underlying global used to implement the
    % mutable should be. If there is a foreign_name attribute then use that
    % otherwise construct one based on the Mercury name for the mutable
    %
:- pred decide_mutable_target_var_name(module_info::in,
    mutable_var_attributes::in, module_name::in, string::in,
    foreign_language::in, prog_context::in, string::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_mutable_target_var_name(ModuleInfo, MutAttrs, ModuleName, Name,
        ForeignLanguage, Context, TargetMutableName, !Specs) :-
    mutable_var_maybe_foreign_names(MutAttrs) = MaybeForeignNames,
    (
        MaybeForeignNames = no,
        % This works for Erlang as well.
        TargetMutableName = mutable_c_var_name(ModuleName, Name)
    ;
        MaybeForeignNames = yes(ForeignNames),
        ReportErrors = no, % We've already reported them during pass 2.
        get_global_name_from_foreign_names(ModuleInfo, ReportErrors, Context,
            ModuleName, Name, ForeignLanguage, ForeignNames, TargetMutableName,
            !Specs)
    ).

%-----------------------------------------------------------------------------%
%
% C mutables.
%

    % Add the foreign_decl and foreign_code items that declare/define
    % the global variable used to hold the mutable.
    %
:- pred add_c_mutable_defn_and_decl(string::in, mer_type::in, bool::in,
    mutable_thread_local::in, prog_context::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_c_mutable_defn_and_decl(TargetMutableName, Type, IsConstant, IsThreadLocal,
        Context, !ModuleInfo, !QualInfo, !Specs) :-
    % We add the foreign code declaration and definition here rather than
    % in pass 2 because the target-language-specific type name depends on
    % whether there are any foreign_type declarations for Type.
    get_c_mutable_global_foreign_decl(!.ModuleInfo, Type,
        TargetMutableName, IsConstant, IsThreadLocal, Context,
        ForeignDecl),
    get_c_mutable_global_foreign_defn(!.ModuleInfo, Type,
        TargetMutableName, IsConstant, IsThreadLocal, Context,
        ForeignDefn),
    ItemStatus0 = item_status(status_local, may_be_unqualified),
    add_item_decl_pass_2(ForeignDecl, ItemStatus0, _, !ModuleInfo, !Specs),
    add_item_decl_pass_2(ForeignDefn, ItemStatus0, _, !ModuleInfo, !Specs).

    % Create the C foreign_decl for the mutable.
    % The bool argument says whether the mutable is a constant mutable or not.
    %
:- pred get_c_mutable_global_foreign_decl(module_info::in, mer_type::in,
    string::in, bool::in, mutable_thread_local::in, prog_context::in,
    item::out) is det.

get_c_mutable_global_foreign_decl(ModuleInfo, Type, TargetMutableName,
        IsConstant, IsThreadLocal, Context, DeclItem) :-
    % This declaration will be included in the .mh files. Since these are
    % grade independent we need to output both the high- and low-level C
    % declarations for the global used to implement the mutable and make
    % the choice conditional on whether MR_HIGHLEVEL_CODE is defined.
    %
    (
        IsThreadLocal = mutable_not_thread_local,
        % The first argument in the following calls to
        % global_foreign_type_name says whether the mutable should always be
        % boxed or not. The only difference between the high- and low-level
        % C backends is that in the latter mutables are *always* boxed,
        % whereas in the former they may not be.
        HighLevelTypeName = global_foreign_type_name(no, lang_c, ModuleInfo,
            Type),
        LowLevelTypeName = global_foreign_type_name(yes, lang_c, ModuleInfo,
            Type)
    ;
        IsThreadLocal = mutable_thread_local,
        % For thread-local mutables, the variable holds an index into an
        % array.
        HighLevelTypeName = "MR_Unsigned",
        LowLevelTypeName  = "MR_Unsigned"
    ),

    % Constant mutables do not require mutexes as their values are never
    % updated. Thread-local mutables do not require mutexes either.
    (
        ( IsConstant = yes
        ; IsThreadLocal = mutable_thread_local
        )
    ->
        LockDecl = []
    ;
        LockDecl = [
            "#ifdef MR_THREAD_SAFE\n",
            "    extern MercuryLock ",
            mutable_mutex_var_name(TargetMutableName), ";\n",
            "#endif\n"
        ]
    ),

    DeclBody = string.append_list([
        "#ifdef MR_HIGHLEVEL_CODE\n",
        "    extern ", HighLevelTypeName, " ", TargetMutableName, ";\n",
        "#else\n",
        "    extern ", LowLevelTypeName, " ", TargetMutableName, ";\n",
        "#endif\n" | LockDecl]),

    FDInfo = pragma_info_foreign_decl(lang_c, foreign_decl_is_exported,
        literal(DeclBody)),
    DeclPragma = pragma_foreign_decl(FDInfo),
    DeclItemPragma = item_pragma_info(compiler(mutable_decl), DeclPragma,
        Context, -1),
    DeclItem = item_pragma(DeclItemPragma).

    % Create the C foreign_defn for the mutable.
    % The bool argument says whether the mutable is a constant mutable
    % or not.
    %
:- pred get_c_mutable_global_foreign_defn(module_info::in, mer_type::in,
    string::in, bool::in, mutable_thread_local::in, prog_context::in,
    item::out) is det.

get_c_mutable_global_foreign_defn(ModuleInfo, Type, TargetMutableName,
        IsConstant, IsThreadLocal, Context, DefnItem) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, mutable_always_boxed, AlwaysBoxed),
    (
        IsThreadLocal = mutable_not_thread_local,
        TypeName = global_foreign_type_name(AlwaysBoxed, lang_c, ModuleInfo,
            Type)
    ;
        IsThreadLocal = mutable_thread_local,
        % For thread-local mutables, the variable holds an index into an
        % array.
        TypeName = "MR_Unsigned"
    ),

    % Constant mutables do not require mutexes as their values are never
    % updated. Thread-local mutables do not require mutexes either.
    (
        ( IsConstant = yes
        ; IsThreadLocal = mutable_thread_local
        )
    ->
        LockDefn = []
    ;
        LockDefn = [
            "#ifdef MR_THREAD_SAFE\n",
            "    MercuryLock ",
            mutable_mutex_var_name(TargetMutableName), ";\n",
            "#endif\n"
        ]
    ),

    DefnBody = string.append_list([
        TypeName, " ", TargetMutableName, ";\n" | LockDefn]),
    FCInfo = pragma_info_foreign_code(lang_c, literal(DefnBody)),
    DefnPragma = pragma_foreign_code(FCInfo),
    DefnItemPragma = item_pragma_info(compiler(mutable_decl), DefnPragma,
        Context, -1),
    DefnItem = item_pragma(DefnItemPragma).

:- func global_foreign_type_name(bool, foreign_language, module_info,
    mer_type) = string.

global_foreign_type_name(yes, _, _, _) = "MR_Word".
global_foreign_type_name(no, Lang, ModuleInfo, Type) =
    mercury_exported_type_to_string(ModuleInfo, Lang, Type).

%-----------------------------------------------------------------------------%

:- pred add_c_mutable_preds(item_mutable_info::in, string::in,
    import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_c_mutable_preds(ItemMutableInfo, TargetMutableName, !Status, !ModuleInfo,
        !QualInfo, !Specs) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ItemMutableInfo = item_mutable_info(MercuryMutableName, Type, InitTerm,
        Inst, MutAttrs, MutVarset, Context, _SeqNum),
    IsConstant = mutable_var_constant(MutAttrs),
    IsThreadLocal = mutable_var_thread_local(MutAttrs),

    % Set up the default attributes for the foreign_procs used for the
    % access predicates.
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, mutable_always_boxed, AlwaysBoxed),
    (
        AlwaysBoxed = yes,
        BoxPolicy = always_boxed
    ;
        AlwaysBoxed = no,
        BoxPolicy = native_if_possible
    ),
    Attrs0 = default_attributes(lang_c),
    set_box_policy(BoxPolicy, Attrs0, Attrs1),
    set_may_call_mercury(proc_will_not_call_mercury, Attrs1, Attrs),

    (
        IsConstant = yes,
        InitSetPredName = mutable_secret_set_pred_sym_name(ModuleName,
            MercuryMutableName),
        add_ccsj_constant_mutable_access_preds(TargetMutableName,
            ModuleName, MercuryMutableName, Attrs, Inst, BoxPolicy,
            Context, !Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        IsConstant = no,
        InitSetPredName = mutable_set_pred_sym_name(ModuleName,
            MercuryMutableName),
        TypeName = global_foreign_type_name(AlwaysBoxed, lang_c,
            !.ModuleInfo, Type),
        add_c_mutable_primitive_preds(TargetMutableName, ModuleName,
            MercuryMutableName, MutAttrs, Attrs, Inst, BoxPolicy, TypeName,
            Context, !Status, !ModuleInfo, !QualInfo, !Specs),
        add_ccsj_mutable_user_access_preds(ModuleName, MercuryMutableName,
            MutAttrs, lang_c, Context, !Status, !ModuleInfo, !QualInfo, !Specs)
    ),
    add_c_mutable_initialisation(IsConstant, IsThreadLocal,
        TargetMutableName, ModuleName, MercuryMutableName, MutVarset,
        InitSetPredName, InitTerm, Attrs,
        Context, !Status, !ModuleInfo, !QualInfo, !Specs).

    % Add the access predicates for constant mutables.
    % Shared between C, C# and Java.
    %
:- pred add_ccsj_constant_mutable_access_preds(string::in, module_name::in,
    string::in, pragma_foreign_proc_attributes::in, mer_inst::in,
    box_policy::in, prog_context::in, import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_ccsj_constant_mutable_access_preds(TargetMutableName,
        ModuleName, MutableName, Attrs, Inst, BoxPolicy, Context,
        !Status, !ModuleInfo, !QualInfo, !Specs) :-
    varset.new_named_var("X", X, varset.init, ProgVarSet),
    InstVarSet = varset.init,
    set_purity(purity_pure, Attrs, ConstantGetAttrs0),
    set_thread_safe(proc_thread_safe, ConstantGetAttrs0, ConstantGetAttrs),
    ConstantGetFCInfo = pragma_info_foreign_proc(
        ConstantGetAttrs,
        mutable_get_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", out_mode(Inst), BoxPolicy)],
        ProgVarSet,
        InstVarSet,
        fp_impl_ordinary("X = " ++ TargetMutableName ++ ";\n", yes(Context))
    ),
    ConstantGetForeignProc = pragma_foreign_proc(ConstantGetFCInfo),
    ConstantGetItemPragma = item_pragma_info(compiler(mutable_decl),
        ConstantGetForeignProc, Context, -1),
    ConstantGetItem = item_pragma(ConstantGetItemPragma),
    add_item_pass_3(ConstantGetItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    % NOTE: we don't need to trail the set action, since it is executed
    % only once at initialization time.

    ConstantSetFCInfo = pragma_info_foreign_proc(Attrs,
        mutable_secret_set_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", in_mode(Inst), BoxPolicy)],
        ProgVarSet,
        InstVarSet,
        fp_impl_ordinary(TargetMutableName ++ " = X;\n", yes(Context))
    ),
    ConstantSetForeignProc = pragma_foreign_proc(ConstantSetFCInfo),
    ConstantSetItemPragma = item_pragma_info(compiler(mutable_decl),
        ConstantSetForeignProc, Context, -1),
    ConstantSetItem = item_pragma(ConstantSetItemPragma),
    add_item_pass_3(ConstantSetItem, !Status, !ModuleInfo, !QualInfo, !Specs).

    % Add the foreign clauses for the mutable's primitive access and
    % locking predicates.
    %
:- pred add_c_mutable_primitive_preds(string::in, module_name::in, string::in,
    mutable_var_attributes::in, pragma_foreign_proc_attributes::in,
    mer_inst::in, box_policy::in, string::in, prog_context::in,
    import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_c_mutable_primitive_preds(TargetMutableName, ModuleName, MutableName,
        MutAttrs, Attrs, Inst, BoxPolicy, TypeName,
        Context, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    IsThreadLocal = mutable_var_thread_local(MutAttrs),
    set_thread_safe(proc_thread_safe, Attrs, LockAndUnlockAttrs),

    % Construct the lock predicate.

    MutableMutexVarName = mutable_mutex_var_name(TargetMutableName),
    % XXX the second argument should be the name of the mercury predicate,
    % with chars escaped as appropriate.
    (
        IsThreadLocal = mutable_not_thread_local,
        LockForeignProcBody = string.append_list([
            "#ifdef MR_THREAD_SAFE\n",
            "  MR_LOCK(&" ++ MutableMutexVarName ++ ",
                \"" ++ MutableMutexVarName ++ "\");\n" ++
            "#endif\n"
        ])
    ;
        IsThreadLocal = mutable_thread_local,
        LockForeignProcBody = ""
    ),
    LockFCInfo = pragma_info_foreign_proc(LockAndUnlockAttrs,
        mutable_lock_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [],
        varset.init,    % Prog varset.
        varset.init,    % Inst varset.
        fp_impl_ordinary(LockForeignProcBody, yes(Context))
    ),
    LockForeignProc = pragma_foreign_proc(LockFCInfo),
    LockItemPragma = item_pragma_info(compiler(mutable_decl),
        LockForeignProc, Context, -1),
    LockItem = item_pragma(LockItemPragma),
    add_item_pass_3(LockItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    % Construct the unlock predicate.
    % XXX as above regarding the second argument to MR_UNLOCK.

    (
        IsThreadLocal = mutable_not_thread_local,
        UnlockForeignProcBody = string.append_list([
            "#ifdef MR_THREAD_SAFE\n",
            "  MR_UNLOCK(&" ++ MutableMutexVarName ++ ",
                \"" ++ MutableMutexVarName ++ "\");\n" ++
            "#endif\n"
        ])
    ;
        IsThreadLocal = mutable_thread_local,
        UnlockForeignProcBody = ""
    ),
    UnlockFCInfo = pragma_info_foreign_proc(LockAndUnlockAttrs,
        mutable_unlock_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [],
        varset.init,    % Prog varset.
        varset.init,    % Inst varset.
        fp_impl_ordinary(UnlockForeignProcBody, yes(Context))
    ),
    UnlockForeignProc = pragma_foreign_proc(UnlockFCInfo),
    UnlockItemPragma = item_pragma_info(compiler(mutable_decl),
        UnlockForeignProc, Context, -1),
    UnlockItem = item_pragma(UnlockItemPragma),
    add_item_pass_3(UnlockItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    % Construct the semipure unsafe_get_predicate.

    set_purity(purity_semipure, Attrs, UnsafeGetAttrs0),
    set_thread_safe(proc_thread_safe, UnsafeGetAttrs0, UnsafeGetAttrs),
    varset.new_named_var("X", X, varset.init, ProgVarSet),
    (
        IsThreadLocal = mutable_not_thread_local,
        UnsafeGetCode = "X = " ++ TargetMutableName ++ ";\n"
    ;
        IsThreadLocal = mutable_thread_local,
        UnsafeGetCode = "MR_get_thread_local_mutable(" ++
            TypeName ++ ", X, " ++ TargetMutableName ++ ");\n"
    ),
    UnsafeGetFCInfo = pragma_info_foreign_proc(UnsafeGetAttrs,
        mutable_unsafe_get_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", out_mode(Inst), BoxPolicy)],
        ProgVarSet,
        varset.init, % Inst varset.
        fp_impl_ordinary(UnsafeGetCode, yes(Context))
    ),
    UnsafeGetForeignProc = pragma_foreign_proc(UnsafeGetFCInfo),
    UnsafeGetItemPragma = item_pragma_info(compiler(mutable_decl),
        UnsafeGetForeignProc, Context, -1),
    UnsafeGetItem = item_pragma(UnsafeGetItemPragma),
    add_item_pass_3(UnsafeGetItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    % Construct the impure unsafe_set_predicate.

    set_thread_safe(proc_thread_safe, Attrs, UnsafeSetAttrs),
    TrailMutableUpdates = mutable_var_trailed(MutAttrs),
    (
        TrailMutableUpdates = mutable_untrailed,
        TrailCode = ""
    ;
        TrailMutableUpdates = mutable_trailed,

        % If we require that the mutable to be trailed then we need to be
        % compiling in a trailing grade.
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, use_trail, UseTrail),
        (
            UseTrail = yes,
            TrailCode = "MR_trail_current_value(&" ++
                TargetMutableName ++ ");\n"
        ;
            UseTrail = no,
            Pieces =
                [words("Error: trailed mutable in non-trailing grade."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs],

            % This is just a dummy value.
            TrailCode = ""
        )
    ),
    (
        IsThreadLocal = mutable_not_thread_local,
        SetCode = TargetMutableName ++ " = X;\n"
    ;
        IsThreadLocal = mutable_thread_local,
        SetCode = "MR_set_thread_local_mutable(" ++
            TypeName ++ ", X, " ++ TargetMutableName ++ ");\n"
    ),
    UnsafeSetFCInfo = pragma_info_foreign_proc(UnsafeSetAttrs,
        mutable_unsafe_set_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", in_mode(Inst), BoxPolicy)],
        ProgVarSet,
        varset.init, % Inst varset.
        fp_impl_ordinary(TrailCode ++ SetCode, yes(Context))
    ),
    UnsafeSetForeignProc = pragma_foreign_proc(UnsafeSetFCInfo),
    UnsafeSetItemPragma = item_pragma_info(compiler(mutable_decl),
        UnsafeSetForeignProc, Context, -1),
    UnsafeSetItem = item_pragma(UnsafeSetItemPragma),
    add_item_pass_3(UnsafeSetItem, !Status, !ModuleInfo, !QualInfo, !Specs).

:- inst lang_ccsj
    --->    lang_c
    ;       lang_csharp
    ;       lang_java.

    % Add the access predicates for a non-constant mutable.
    % If the mutable has the `attach_to_io_state' attribute then add the
    % versions of the access preds that take the I/O state as well.
    % Shared between C, C# and Java.
    %
:- pred add_ccsj_mutable_user_access_preds(module_name::in, string::in,
    mutable_var_attributes::in, foreign_language::in(lang_ccsj),
    prog_context::in, import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_ccsj_mutable_user_access_preds(ModuleName, MutableName, MutAttrs,
        Lang, Context, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    varset.new_named_var("X", X, varset.init, ProgVarSet0),

    LockPredName   = mutable_lock_pred_sym_name(ModuleName, MutableName),
    UnlockPredName = mutable_unlock_pred_sym_name(ModuleName, MutableName),
    CallLock   = call_expr(LockPredName, [], purity_impure) - Context,
    CallUnlock = call_expr(UnlockPredName, [], purity_impure) - Context,

    GetterPredName = mutable_unsafe_get_pred_sym_name(ModuleName, MutableName),
    SetterPredName = mutable_unsafe_set_pred_sym_name(ModuleName, MutableName),
    CallGetter = call_expr(GetterPredName, [variable(X, Context)],
        purity_semipure) - Context,
    CallSetter = call_expr(SetterPredName, [variable(X, context_init)],
        purity_impure) - Context,

    GetPredName = mutable_get_pred_sym_name(ModuleName, MutableName),
    SetPredName = mutable_set_pred_sym_name(ModuleName, MutableName),

    (
        Lang = lang_c,
        GetBody = goal_list_to_conj(Context, [CallLock, CallGetter,
            CallUnlock]),
        StdSetBody = goal_list_to_conj(Context, [CallLock, CallSetter,
            CallUnlock])
    ;
        ( Lang = lang_java
        ; Lang = lang_csharp
        ),
        % There are no separate lock predicates for Java; the synchronisation
        % is performed within the "unsafe" predicates.
        % XXX C# needs investigation
        GetBody = CallGetter,
        StdSetBody = CallSetter
    ),

    % Construct the semipure get predicate.
    StdGetBody = promise_purity_expr(purity_semipure, GetBody) - Context,
    StdGetItemClause = item_clause_info(compiler(mutable_decl),
        ProgVarSet0, pf_predicate, GetPredName,
        [variable(X, context_init)], StdGetBody, Context, -1),
    StdGetItem = item_clause(StdGetItemClause),
    add_item_pass_3(StdGetItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    % Construct the impure set predicate.
    StdSetItemClause = item_clause_info(compiler(mutable_decl),
        ProgVarSet0, pf_predicate, SetPredName,
        [variable(X, context_init)], StdSetBody, Context, -1),
    StdSetItem = item_clause(StdSetItemClause),
    add_item_pass_3(StdSetItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    IOStateInterface = mutable_var_attach_to_io_state(MutAttrs),
    (
        IOStateInterface = yes,
        varset.new_named_var("IO", IO, ProgVarSet0, ProgVarSet),

        % Construct the pure get predicate.
        IOGetBody = promise_purity_expr(purity_pure, GetBody) - Context,
        Ctxt = context_init,
        IOGetItemClause = item_clause_info(compiler(mutable_decl), ProgVarSet,
            pf_predicate, GetPredName,
            [variable(X, Ctxt), variable(IO, Ctxt), variable(IO, Ctxt)],
            IOGetBody, Context, -1
        ),
        IOGetItem = item_clause(IOGetItemClause),
        add_item_pass_3(IOGetItem, !Status, !ModuleInfo, !QualInfo, !Specs),

        % Construct the pure set predicate.
        %
        % We just use the body of impure version and attach a promise_pure
        % pragma to the predicate. (The purity pragma was added during
        % stage 2.)
        IOSetBody = StdSetBody,
        IOSetItemClause = item_clause_info(compiler(mutable_decl), ProgVarSet,
            pf_predicate, SetPredName,
            [variable(X, Ctxt), variable(IO, Ctxt), variable(IO, Ctxt)],
            IOSetBody, Context, -1
        ),
        IOSetItem = item_clause(IOSetItemClause),
        add_item_pass_3(IOSetItem, !Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        IOStateInterface = no
    ).

    % Add the code required to initialise a mutable.
    %
:- pred add_c_mutable_initialisation(bool::in, mutable_thread_local::in,
    string::in, module_name::in, string::in, prog_varset::in,
    sym_name::in, prog_term::in, pragma_foreign_proc_attributes::in,
    prog_context::in, import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_c_mutable_initialisation(IsConstant, IsThreadLocal, TargetMutableName,
        ModuleName, MutableName, MutVarset0, InitSetPredName, InitTerm, Attrs,
        Context, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    % Add the `:- initialise' declaration for the mutable initialisation
    % predicate.
    InitPredName = mutable_init_pred_sym_name(ModuleName, MutableName),
    InitPredArity = 0,

    InitItemInitialise = item_initialise_info(compiler(mutable_decl),
        InitPredName, InitPredArity, Context, -1),
    InitItem = item_initialise(InitItemInitialise),
    add_item_pass_3(InitItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    % Add the clause for the mutable initialisation predicate.
    varset.new_named_var("X", X, MutVarset0, MutVarset),
    UnifyExpr =
        unify_expr(variable(X, Context), InitTerm, purity_impure)
            - Context,
    (
        IsConstant = yes,
        CallExpr =
            call_expr(InitSetPredName, [variable(X, Context)], purity_impure)
                - Context,
        InitClauseExpr = conj_expr(UnifyExpr, CallExpr) - Context
    ;
        IsConstant = no,
        (
            IsThreadLocal = mutable_not_thread_local,
            % Construct the clause for the mutex initialisation predicate.
            PreInitCode = string.append_list([
                "#ifdef MR_THREAD_SAFE\n",
                "   pthread_mutex_init(&",
                        mutable_mutex_var_name(TargetMutableName),
                        ", MR_MUTEX_ATTR);\n",
                "#endif\n"
            ])
        ;
            IsThreadLocal = mutable_thread_local,
            PreInitCode = string.append_list([
                TargetMutableName,
                " = MR_new_thread_local_mutable_index();\n"
            ])
        ),
        PreInitPredName = mutable_pre_init_pred_sym_name(ModuleName,
            MutableName),
        PreInitFCInfo = pragma_info_foreign_proc(Attrs,
            PreInitPredName,
            pf_predicate,
            [],
            varset.init,    % ProgVarSet
            varset.init,    % InstVarSet
            fp_impl_ordinary(PreInitCode, yes(Context))
        ),
        PreInitForeignProc = pragma_foreign_proc(PreInitFCInfo),
        PreInitItemPragma = item_pragma_info(compiler(mutable_decl),
            PreInitForeignProc, Context, -1),
        PreInitItem = item_pragma(PreInitItemPragma),
        add_item_pass_3(PreInitItem, !Status, !ModuleInfo, !QualInfo,
            !Specs),

        CallPreInitExpr =
            call_expr(PreInitPredName, [], purity_impure) - Context,
        CallSetPredExpr =
            call_expr(InitSetPredName, [variable(X, Context)], purity_impure)
                - Context,
        InitClauseExpr = goal_list_to_conj(Context,
            [CallPreInitExpr, UnifyExpr, CallSetPredExpr])
    ),

    % See the comments for prog_io.parse_mutable_decl for the reason
    % why we _must_ use MutVarset here.
    PredItemClause = item_clause_info(compiler(mutable_decl), MutVarset,
        pf_predicate, InitPredName, [], InitClauseExpr, Context, -1),
    PredItem = item_clause(PredItemClause),
    add_item_pass_3(PredItem, !Status, !ModuleInfo, !QualInfo, !Specs).

%-----------------------------------------------------------------------------%
%
% C#/Java mutables
%

:- inst lang_csharp_java
    --->    lang_csharp
    ;       lang_java.

    % Add foreign_code item that defines the global variable used to hold the
    % mutable.
    %
:- pred add_csharp_java_mutable_defn(foreign_language::in(lang_csharp_java),
    string::in, mer_type::in, mutable_thread_local::in, prog_context::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_csharp_java_mutable_defn(Lang, TargetMutableName, Type, IsThreadLocal,
        Context, !ModuleInfo, !QualInfo, !Specs) :-
    get_csharp_java_mutable_global_foreign_defn(Lang, TargetMutableName,
        Type, IsThreadLocal, Context, DefnBody),
    DefnFCInfo = pragma_info_foreign_code(Lang, literal(DefnBody)),
    DefnPragma = pragma_foreign_code(DefnFCInfo),
    DefnItemPragma = item_pragma_info(compiler(mutable_decl), DefnPragma,
        Context, -1),
    ForeignDefn = item_pragma(DefnItemPragma),
    ItemStatus0 = item_status(status_local, may_be_unqualified),
    add_item_decl_pass_2(ForeignDefn, ItemStatus0, _, !ModuleInfo, !Specs).

:- pred get_csharp_java_mutable_global_foreign_defn(
    foreign_language::in(lang_csharp_java), string::in, mer_type::in,
    mutable_thread_local::in, prog_context::in, string::out) is det.

get_csharp_java_mutable_global_foreign_defn(Lang, TargetMutableName, Type,
        IsThreadLocal, _Context, DefnBody) :-
    (
        Lang = lang_csharp,
        (
            IsThreadLocal = mutable_not_thread_local,
            ( Type = int_type ->
                TypeStr = "int"
            ;
                TypeStr = "object"
            )
        ;
            IsThreadLocal = mutable_thread_local,
            TypeStr = "int"
        ),
        DefnBody = string.append_list([
            "static ", TypeStr, " ", TargetMutableName, ";\n"])
    ;
        Lang = lang_java,
        IsThreadLocal = mutable_not_thread_local,
        % Synchronization is only required for double and long values, which
        % Mercury does not expose. We could also use the volatile keyword.
        % (Java Language Specification, 2nd Ed., 17.4).
        ( Type = int_type ->
            TypeStr = "int"
        ;
            TypeStr = "java.lang.Object"
        ),
        DefnBody = string.append_list([
            "static ", TypeStr, " ", TargetMutableName, ";\n"])
    ;
        Lang = lang_java,
        IsThreadLocal = mutable_thread_local,
        ( Type = int_type ->
            TypeStr = "java.lang.Integer"
        ;
            TypeStr = "java.lang.Object"
        ),
        DefnBody = string.append_list([
            "static java.lang.ThreadLocal<", TypeStr, "> ",
            TargetMutableName,
            " = new java.lang.InheritableThreadLocal<", TypeStr, ">();\n"
        ])
    ).

:- pred add_csharp_java_mutable_preds(item_mutable_info::in,
    foreign_language::in(lang_csharp_java), string::in,
    import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_csharp_java_mutable_preds(ItemMutable, Lang, TargetMutableName,
        !Status, !ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ItemMutable = item_mutable_info(MercuryMutableName, Type, InitTerm, Inst,
        MutAttrs, MutVarset, Context, _SeqNum),
    IsConstant = mutable_var_constant(MutAttrs),
    Attrs0 = default_attributes(Lang),
    % The mutable variable name is not module-qualified so cannot be exported
    % to `.opt' files. We could add the qualification but it would be better
    % to move the mutable code generation into the backends first.
    set_may_duplicate(yes(proc_may_not_duplicate), Attrs0, Attrs),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, mutable_always_boxed, AlwaysBoxed),
    (
        AlwaysBoxed = yes,
        BoxPolicy = always_boxed
    ;
        AlwaysBoxed = no,
        BoxPolicy = native_if_possible
    ),
    (
        IsConstant = yes,
        InitSetPredName = mutable_secret_set_pred_sym_name(ModuleName,
            MercuryMutableName),
        add_ccsj_constant_mutable_access_preds(TargetMutableName,
            ModuleName, MercuryMutableName, Attrs, Inst, BoxPolicy,
            Context, !Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        IsConstant = no,
        InitSetPredName = mutable_set_pred_sym_name(ModuleName,
            MercuryMutableName),
        add_csharp_java_mutable_primitive_preds(Lang, TargetMutableName,
            ModuleName, MercuryMutableName, Type, MutAttrs, Attrs, Inst,
            BoxPolicy, Context, !Status, !ModuleInfo, !QualInfo, !Specs),
        add_ccsj_mutable_user_access_preds(ModuleName, MercuryMutableName,
            MutAttrs, Lang,
            Context, !Status, !ModuleInfo, !QualInfo, !Specs)
    ),
    % The C# thread-local mutable implementation requires array indices to be
    % allocated in pre-init predicates.
    (
        Lang = lang_csharp,
        mutable_var_thread_local(MutAttrs) = mutable_thread_local
    ->
        add_csharp_thread_local_mutable_pre_init_pred(TargetMutableName,
            ModuleName, MercuryMutableName, Attrs, CallPreInitExpr,
            Context, !Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        CallPreInitExpr = true_expr - Context
    ),
    add_csharp_java_mutable_initialisation(ModuleName, MercuryMutableName,
        MutVarset, CallPreInitExpr, InitSetPredName, InitTerm,
        Context, !Status, !ModuleInfo, !QualInfo, !Specs).

:- pred add_csharp_thread_local_mutable_pre_init_pred(string::in,
    module_name::in, string::in, pragma_foreign_proc_attributes::in, goal::out,
    prog_context::in, import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_csharp_thread_local_mutable_pre_init_pred(TargetMutableName,
        ModuleName, MutableName, Attrs, CallPreInitExpr,
        Context, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    PreInitCode = string.append_list([
        TargetMutableName, " = runtime.ThreadLocalMutables.new_index();\n"
    ]),
    PreInitPredName = mutable_pre_init_pred_sym_name(ModuleName,
        MutableName),
    PreInitFCInfo = pragma_info_foreign_proc(Attrs,
        PreInitPredName,
        pf_predicate,
        [],
        varset.init,    % ProgVarSet
        varset.init,    % InstVarSet
        fp_impl_ordinary(PreInitCode, yes(Context))
    ),
    PreInitForeignProc = pragma_foreign_proc(PreInitFCInfo),
    PreInitItemPragma = item_pragma_info(compiler(mutable_decl),
        PreInitForeignProc, Context, -1),
    PreInitItem = item_pragma(PreInitItemPragma),
    add_item_pass_3(PreInitItem, !Status, !ModuleInfo, !QualInfo,
        !Specs),
    CallPreInitExpr =
        call_expr(PreInitPredName, [], purity_impure) - Context.

    % Add the foreign clauses for the mutable's primitive access and
    % locking predicates.
    %
:- pred add_csharp_java_mutable_primitive_preds(
    foreign_language::in(lang_csharp_java), string::in, module_name::in,
    string::in, mer_type::in, mutable_var_attributes::in,
    pragma_foreign_proc_attributes::in, mer_inst::in, box_policy::in,
    prog_context::in, import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_csharp_java_mutable_primitive_preds(Lang, TargetMutableName, ModuleName,
        MutableName, Type, MutAttrs, Attrs, Inst, BoxPolicy,
        Context, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    IsThreadLocal = mutable_var_thread_local(MutAttrs),

    % Construct the semipure get predicate.

    set_purity(purity_semipure, Attrs, GetAttrs0),
    set_thread_safe(proc_thread_safe, GetAttrs0, GetAttrs),
    varset.new_named_var("X", X, varset.init, ProgVarSet),
    (
        IsThreadLocal = mutable_not_thread_local,
        GetCode = "\tX = " ++ TargetMutableName ++ ";\n"
    ;
        IsThreadLocal = mutable_thread_local,
        Lang = lang_java,
        IsThreadLocal = mutable_thread_local,
        GetCode = "\tX = " ++ TargetMutableName ++ ".get();\n"
    ;
        IsThreadLocal = mutable_thread_local,
        Lang = lang_csharp,
        ( Type = int_type ->
            Cast = "(int) "
        ;
            Cast = ""
        ),
        GetCode = string.append_list([
            "\tX = ", Cast, "runtime.ThreadLocalMutables.get(",
            TargetMutableName, ");\n"
        ])
    ),
    GetFCInfo = pragma_info_foreign_proc(GetAttrs,
        mutable_unsafe_get_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", out_mode(Inst), BoxPolicy)],
        ProgVarSet,
        varset.init, % Inst varset.
        fp_impl_ordinary(GetCode, yes(Context))
    ),
    GetForeignProc = pragma_foreign_proc(GetFCInfo),
    GetItemPragma = item_pragma_info(compiler(mutable_decl),
        GetForeignProc, Context, -1),
    GetItem = item_pragma(GetItemPragma),
    add_item_pass_3(GetItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    % Construct the impure set predicate.

    set_thread_safe(proc_thread_safe, Attrs, SetAttrs),
    TrailMutableUpdates = mutable_var_trailed(MutAttrs),
    (
        TrailMutableUpdates = mutable_untrailed,
        TrailCode = ""
    ;
        TrailMutableUpdates = mutable_trailed,
        Pieces = [words("Error: trailed mutable in non-trailed grade."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs],
        % This is just a dummy value.
        TrailCode = ""
    ),
    (
        IsThreadLocal = mutable_not_thread_local,
        SetCode = "\t" ++ TargetMutableName ++ " = X;\n"
    ;
        IsThreadLocal = mutable_thread_local,
        Lang = lang_java,
        SetCode = "\t" ++ TargetMutableName ++ ".set(X);\n"
    ;
        IsThreadLocal = mutable_thread_local,
        Lang = lang_csharp,
        SetCode = "\truntime.ThreadLocalMutables.set(" ++
            TargetMutableName ++ ", X);\n"
    ),
    SetFCInfo = pragma_info_foreign_proc(SetAttrs,
        mutable_unsafe_set_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", in_mode(Inst), BoxPolicy)],
        ProgVarSet,
        varset.init, % Inst varset.
        fp_impl_ordinary(TrailCode ++ SetCode, yes(Context))
    ),
    SetForeignProc = pragma_foreign_proc(SetFCInfo),
    SetItemPragma = item_pragma_info(compiler(mutable_decl),
        SetForeignProc, Context, -1),
    SetItem = item_pragma(SetItemPragma),
    add_item_pass_3(SetItem, !Status, !ModuleInfo, !QualInfo, !Specs).

    % Add the code required to initialise a mutable.
    %
:- pred add_csharp_java_mutable_initialisation(module_name::in, string::in,
    prog_varset::in, goal::in, sym_name::in, prog_term::in,
    prog_context::in, import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_csharp_java_mutable_initialisation(ModuleName, MutableName, MutVarset0,
        CallPreInitExpr, InitSetPredName, InitTerm,
        Context, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    % Add the `:- initialise' declaration for the mutable initialisation
    % predicate.
    InitPredName = mutable_init_pred_sym_name(ModuleName, MutableName),
    InitPredArity = 0,

    InitItemInitialise = item_initialise_info(compiler(mutable_decl),
        InitPredName, InitPredArity, Context, -1),
    InitItem = item_initialise(InitItemInitialise),
    add_item_pass_3(InitItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    % Add the clause for the mutable initialisation predicate.
    varset.new_named_var("X", X, MutVarset0, MutVarset),
    UnifyExpr =
        unify_expr(variable(X, Context), InitTerm, purity_impure)
            - Context,
    CallSetPredExpr =
        call_expr(InitSetPredName, [variable(X, Context)], purity_impure)
            - Context,
    InitClauseExpr = goal_list_to_conj(Context,
        [CallPreInitExpr, UnifyExpr, CallSetPredExpr]),

    % See the comments for prog_io.parse_mutable_decl for the reason
    % why we _must_ use MutVarset here.
    PredItemClause = item_clause_info(compiler(mutable_decl), MutVarset,
        pf_predicate, InitPredName, [], InitClauseExpr, Context, -1),
    PredItem = item_clause(PredItemClause),
    add_item_pass_3(PredItem, !Status, !ModuleInfo, !QualInfo, !Specs).

%-----------------------------------------------------------------------------%
%
% Erlang mutables
%

:- pred add_erlang_mutable_preds(item_mutable_info::in, string::in,
    import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_erlang_mutable_preds(ItemMutable, TargetMutableName,
        !Status, !ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ItemMutable = item_mutable_info(MutableName, _Type, InitTerm, Inst,
        MutAttrs, MutVarset, Context, _SeqNum),
    IsConstant = mutable_var_constant(MutAttrs),
    (
        IsConstant = yes,
        InitSetPredName = mutable_secret_set_pred_sym_name(ModuleName,
            MutableName),
        add_erlang_constant_mutable_access_preds(TargetMutableName,
            ModuleName, MutableName, Inst,
            Context, !Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        IsConstant = no,
        InitSetPredName = mutable_set_pred_sym_name(ModuleName,
            MutableName),
        add_erlang_mutable_user_access_preds(TargetMutableName,
            ModuleName, MutableName, MutAttrs, Inst,
            Context, !Status, !ModuleInfo, !QualInfo, !Specs)
    ),
    add_erlang_mutable_initialisation(ModuleName, MutableName,
        MutVarset, InitSetPredName, InitTerm,
        Context, !Status, !ModuleInfo, !QualInfo, !Specs).

    % Add the access predicates for constant mutables.
    %
:- pred add_erlang_constant_mutable_access_preds(string::in,
    module_name::in, string::in, mer_inst::in, prog_context::in,
    import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_erlang_constant_mutable_access_preds(TargetMutableName,
        ModuleName, MutableName, Inst,
        Context, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    varset.new_named_var("X", X, varset.init, ProgVarSet),
    InstVarSet = varset.init,
    Attrs = default_attributes(lang_erlang),
    set_purity(purity_pure, Attrs, ConstantGetAttrs0),
    set_thread_safe(proc_thread_safe, ConstantGetAttrs0, ConstantGetAttrs),

    % Getter.
    GetCode = erlang_mutable_get_code(TargetMutableName),
    ConstantGetFCInfo = pragma_info_foreign_proc(
        ConstantGetAttrs,
        mutable_get_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", out_mode(Inst), native_if_possible)],
        ProgVarSet,
        InstVarSet,
        fp_impl_ordinary(GetCode, yes(Context))
    ),
    ConstantGetForeignProc = pragma_foreign_proc(ConstantGetFCInfo),
    ConstantGetItemPragma = item_pragma_info(compiler(mutable_decl),
        ConstantGetForeignProc, Context, -1),
    ConstantGetItem = item_pragma(ConstantGetItemPragma),
    add_item_pass_3(ConstantGetItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    % Secret setter.
    SetCode = erlang_mutable_set_code(TargetMutableName),
    ConstantSetFCInfo = pragma_info_foreign_proc(Attrs,
        mutable_secret_set_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", in_mode(Inst), native_if_possible)],
        ProgVarSet,
        InstVarSet,
        fp_impl_ordinary(SetCode, yes(Context))
    ),
    ConstantSetForeignProc = pragma_foreign_proc(ConstantSetFCInfo),
    ConstantSetItemPragma = item_pragma_info(compiler(mutable_decl),
        ConstantSetForeignProc, Context, -1),
    ConstantSetItem = item_pragma(ConstantSetItemPragma),
    add_item_pass_3(ConstantSetItem, !Status, !ModuleInfo, !QualInfo, !Specs).

    % Add the access predicates for a non-constant mutable.
    % If the mutable has the `attach_to_io_state' attribute then add the
    % versions of the access preds that take the I/O state as well.
    %
:- pred add_erlang_mutable_user_access_preds(string::in,
    module_name::in, string::in, mutable_var_attributes::in, mer_inst::in,
    prog_context::in, import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_erlang_mutable_user_access_preds(TargetMutableName,
        ModuleName, MutableName, MutAttrs, Inst, Context,
        !Status, !ModuleInfo, !QualInfo, !Specs) :-
    IsThreadLocal = mutable_var_thread_local(MutAttrs),
    Attrs = default_attributes(lang_erlang),
    varset.new_named_var("X", X, varset.init, ProgVarSet0),

    % Construct the semipure get predicate.
    set_purity(purity_semipure, Attrs, GetAttrs0),
    set_thread_safe(proc_thread_safe, GetAttrs0, GetAttrs),
    (
        IsThreadLocal = mutable_not_thread_local,
        GetCode = erlang_mutable_get_code(TargetMutableName)
    ;
        IsThreadLocal = mutable_thread_local,
        % XXX this will need to change. `thread_local' mutables are supposed
        % to be inherited when a child process is spawned, but Erlang process
        % dictionary values are not automatically inherited. Hence we will
        % probably need another level of indirection.
        GetCode = "X = get({'MR_thread_local_mutable', " ++
            TargetMutableName ++ "})"
    ),
    GetPredName = mutable_get_pred_sym_name(ModuleName, MutableName),
    GetFCInfo = pragma_info_foreign_proc(GetAttrs,
        GetPredName,
        pf_predicate,
        [pragma_var(X, "X", out_mode(Inst), native_if_possible)],
        ProgVarSet0,
        varset.init, % Inst varset.
        fp_impl_ordinary(GetCode, yes(Context))
    ),
    GetForeignProc = pragma_foreign_proc(GetFCInfo),
    GetItemPragma = item_pragma_info(compiler(mutable_decl), GetForeignProc,
        Context, -1),
    GetItem = item_pragma(GetItemPragma),
    add_item_pass_3(GetItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    % Construct the impure set predicate.
    set_purity(purity_impure, Attrs, SetAttrs0),
    set_thread_safe(proc_thread_safe, SetAttrs0, SetAttrs),
    (
        IsThreadLocal = mutable_not_thread_local,
        SetCode = erlang_mutable_set_code(TargetMutableName)
    ;
        IsThreadLocal = mutable_thread_local,
        % XXX this will need to change (see the comment for the getter)
        SetCode = "put({'MR_thread_local_mutable', " ++
            TargetMutableName ++ "}, X)"
    ),
    SetPredName = mutable_set_pred_sym_name(ModuleName, MutableName),
    SetFCInfo = pragma_info_foreign_proc(SetAttrs,
        SetPredName,
        pf_predicate,
        [pragma_var(X, "X", in_mode(Inst), native_if_possible)],
        ProgVarSet0,
        varset.init, % Inst varset.
        fp_impl_ordinary(SetCode, yes(Context))
    ),
    SetForeignProc = pragma_foreign_proc(SetFCInfo),
    SetItemPragma = item_pragma_info(compiler(mutable_decl), SetForeignProc,
        Context, -1),
    SetItem = item_pragma(SetItemPragma),
    add_item_pass_3(SetItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    IOStateInterface = mutable_var_attach_to_io_state(MutAttrs),
    (
        IOStateInterface = yes,
        varset.new_named_var("IO", IO, ProgVarSet0, ProgVarSet),
        Ctxt = context_init,

        % Construct the pure get predicate.
        % This just calls the semipure get predicate with a promise_pure
        % around it.
        CallSemipureGet = call_expr(GetPredName, [variable(X, Context)],
            purity_semipure) - Context,
        IOGetBody = promise_purity_expr(purity_pure, CallSemipureGet)
            - Context,

        IOGetItemClause = item_clause_info(compiler(mutable_decl), ProgVarSet,
            pf_predicate, GetPredName,
            [variable(X, Ctxt), variable(IO, Ctxt), variable(IO, Ctxt)],
            IOGetBody, Context, -1),
        IOGetItem = item_clause(IOGetItemClause),
        add_item_pass_3(IOGetItem, !Status, !ModuleInfo, !QualInfo, !Specs),

        % Construct the pure set predicate.
        %
        % We just call the impure version and attach a promise_pure
        % pragma to the predicate. (The purity pragma was added during
        % stage 2.)
        CallImpureSet = call_expr(SetPredName, [variable(X, Context)],
            purity_impure) - Context,
        IOSetItemClause = item_clause_info(compiler(mutable_decl), ProgVarSet,
            pf_predicate, SetPredName,
            [variable(X, Ctxt), variable(IO, Ctxt), variable(IO, Ctxt)],
            CallImpureSet, Context, -1),
        IOSetItem = item_clause(IOSetItemClause),
        add_item_pass_3(IOSetItem, !Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        IOStateInterface = no
    ).

:- func erlang_mutable_get_code(string) = string.

erlang_mutable_get_code(TargetMutableName) =
    string.append_list([
        "'ML_erlang_global_server' ! {get_mutable, ",
            TargetMutableName, ", self()},\n",
        "receive\n",
        "   {get_mutable_ack, Value} ->\n",
        "       X = Value\n",
        "end\n"
    ]).

:- func erlang_mutable_set_code(string) = string.

erlang_mutable_set_code(TargetMutableName) =
    "'ML_erlang_global_server' ! {set_mutable, " ++
        TargetMutableName ++ ", X}".

    % Add the code required to initialise a mutable.
    %
:- pred add_erlang_mutable_initialisation(module_name::in, string::in,
    prog_varset::in, sym_name::in, prog_term::in, prog_context::in,
    import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_erlang_mutable_initialisation(ModuleName, MutableName,
        MutVarset0, InitSetPredName, InitTerm,
        Context, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    % Add the `:- initialise' declaration for the mutable initialisation
    % predicate.
    InitPredName = mutable_init_pred_sym_name(ModuleName, MutableName),
    InitPredArity = 0,
    InitItemInitialise = item_initialise_info(compiler(mutable_decl),
        InitPredName, InitPredArity, Context, -1),
    InitItem = item_initialise(InitItemInitialise),
    add_item_pass_3(InitItem, !Status, !ModuleInfo, !QualInfo, !Specs),

    % Add the clause for the mutable initialisation predicate.
    %
    % See the comments for prog_io.parse_mutable_decl for the reason
    % why we _must_ use MutVarset here.
    varset.new_named_var("X", X, MutVarset0, MutVarset),
    UnifyExpr =
        unify_expr(variable(X, Context), InitTerm, purity_impure)
            - Context,
    CallExpr =
        call_expr(InitSetPredName, [variable(X, Context)], purity_impure)
            - Context,
    InitClauseExpr = conj_expr(UnifyExpr, CallExpr) - Context,
    PredItemClause = item_clause_info(compiler(mutable_decl), MutVarset,
        pf_predicate, InitPredName, [], InitClauseExpr, Context, -1),
    PredItem = item_clause(PredItemClause),
    add_item_pass_3(PredItem, !Status, !ModuleInfo, !QualInfo, !Specs).

%-----------------------------------------------------------------------------%

:- pred add_solver_type_mutable_items_clauses(list(item_mutable_info)::in,
    import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_solver_type_mutable_items_clauses([], !Status,
        !ModuleInfo, !QualInfo, !Specs).
add_solver_type_mutable_items_clauses([MutableInfo | MutableInfos], !Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    add_pass_3_mutable(MutableInfo, !.Status, !ModuleInfo, !QualInfo, !Specs),
    add_solver_type_mutable_items_clauses(MutableInfos, !Status,
        !ModuleInfo, !QualInfo, !Specs).

%-----------------------------------------------------------------------------%

    % If a module_defn updates the import_status, return the new status
    % and whether uses of the following items must be module qualified,
    % otherwise fail.
    %
:- pred module_defn_update_import_status(module_defn::in, item_status::out)
    is semidet.

module_defn_update_import_status(ModuleDefn, Status) :-
    (
        ModuleDefn = md_interface,
        Status = item_status(status_exported, may_be_unqualified)
    ;
        ModuleDefn = md_implementation,
        Status = item_status(status_local, may_be_unqualified)
    ;
        ModuleDefn = md_implementation_but_exported_to_submodules,
        Status = item_status(status_exported_to_submodules, may_be_unqualified)
    ;
        ModuleDefn = md_imported(Section),
        Status = item_status(status_imported(Section), may_be_unqualified)
    ;
        ModuleDefn = md_used(Section),
        Status = item_status(status_imported(Section), must_be_qualified)
    ;
        ModuleDefn = md_opt_imported,
        Status = item_status(status_opt_imported, must_be_qualified)
    ;
        ModuleDefn = md_abstract_imported,
        Status = item_status(status_abstract_imported, must_be_qualified)
    ).

%-----------------------------------------------------------------------------%

:- pred add_promise_clause(promise_type::in, list(term(prog_var_type))::in,
    prog_varset::in, goal::in, prog_context::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_promise_clause(PromiseType, HeadVars, VarSet, Goal, Context, Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    term.context_line(Context, Line),
    term.context_file(Context, File),
    string.format(prog_out.promise_to_string(PromiseType) ++
        "__%d__%s", [i(Line), s(File)], Name),

    % Promise declarations are recorded as a predicate with a goal_type
    % of goal_type_promise(X), where X is of promise_type. This allows us
    % to leverage off all the other checks in the compiler that operate
    % on predicates.
    %
    % :- promise all [A,B,R] ( R = A + B <=> R = B + A ).
    %
    % becomes
    %
    % promise.lineno_filename(A, B, R) :-
    %   ( R = A + B <=> R = B + A ).

    module_info_get_name(!.ModuleInfo, ModuleName),
    module_add_clause(VarSet, pf_predicate, qualified(ModuleName, Name),
        HeadVars, Goal, Status, Context, no, goal_type_promise(PromiseType),
        !ModuleInfo, !QualInfo, !Specs).

add_stratified_pred(PragmaName, Name, Arity, Context, !ModuleInfo, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_lookup_sym_arity(PredTable0, is_fully_qualified,
        Name, Arity, PredIds),
    (
        PredIds = [_ | _],
        module_info_get_stratified_preds(!.ModuleInfo, StratPredIds0),
        set.insert_list(PredIds, StratPredIds0, StratPredIds),
        module_info_set_stratified_preds(StratPredIds, !ModuleInfo)
    ;
        PredIds = [],
        DescPieces = [quote(":- pragma " ++ PragmaName), words("declaration")],
        undefined_pred_or_func_error(Name, Arity, Context, DescPieces, !Specs)
    ).

%-----------------------------------------------------------------------------%

add_pred_marker(PragmaName, Name, Arity, Status, Context, Marker,
        ConflictMarkers, !ModuleInfo, !Specs) :-
    ( marker_must_be_exported(Marker) ->
        MustBeExported = yes
    ;
        MustBeExported = no
    ),
    do_add_pred_marker(PragmaName, Name, Arity, Status, MustBeExported,
        Context, add_marker_pred_info(Marker), !ModuleInfo, PredIds, !Specs),
    module_info_get_preds(!.ModuleInfo, Preds),
    pragma_check_markers(Preds, PredIds, ConflictMarkers, Conflict),
    (
        Conflict = yes,
        pragma_conflict_error(Name, Arity, Context, PragmaName, !Specs)
    ;
        Conflict = no
    ).

do_add_pred_marker(PragmaName, Name, Arity, Status, MustBeExported, Context,
        UpdatePredInfo, !ModuleInfo, PredIds, !Specs) :-
    get_matching_pred_ids(!.ModuleInfo, Name, Arity, PredIds),
    (
        PredIds = [_ | _],
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        predicate_table_get_preds(PredTable0, Preds0),

        pragma_add_marker(PredIds, UpdatePredInfo, Status,
            MustBeExported, Preds0, Preds, WrongStatus),
        (
            WrongStatus = yes,
            pragma_status_error(Name, Arity, Context, PragmaName, !Specs)
        ;
            WrongStatus = no
        ),

        predicate_table_set_preds(Preds, PredTable0, PredTable),
        module_info_set_predicate_table(PredTable, !ModuleInfo)
    ;
        PredIds = [],
        DescPieces = [quote(":- pragma " ++ PragmaName), words("declaration")],
        undefined_pred_or_func_error(Name, Arity, Context, DescPieces, !Specs)
    ).

:- pred get_matching_pred_ids(module_info::in, sym_name::in, arity::in,
    list(pred_id)::out) is det.

get_matching_pred_ids(Module0, Name, Arity, PredIds) :-
    module_info_get_predicate_table(Module0, PredTable0),
    % Check that the pragma is module qualified.
    (
        Name = unqualified(_),
        unexpected($module, $pred, "unqualified name")
    ;
        Name = qualified(_, _),
        predicate_table_lookup_sym_arity(PredTable0, is_fully_qualified,
            Name, Arity, PredIds)
    ).

module_mark_as_external(PredName, Arity, Context, !ModuleInfo, !Specs) :-
    % `external' declarations can only apply to things defined in this module,
    % since everything else is already external.
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    predicate_table_lookup_sym_arity(PredicateTable0, is_fully_qualified,
        PredName, Arity, PredIds),
    (
        PredIds = [_ | _],
        module_mark_preds_as_external(PredIds, !ModuleInfo)
    ;
        PredIds = [],
        undefined_pred_or_func_error(PredName, Arity, Context,
            [quote(":- external"), words("declaration")], !Specs)
    ).

:- pred module_mark_preds_as_external(list(pred_id)::in,
    module_info::in, module_info::out) is det.

module_mark_preds_as_external([], !ModuleInfo).
module_mark_preds_as_external([PredId | PredIds], !ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),
    pred_info_mark_as_external(PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, Preds0, Preds),
    module_info_set_preds(Preds, !ModuleInfo),
    module_mark_preds_as_external(PredIds, !ModuleInfo).

    % For each pred_id in the list, check whether markers present in the list
    % of conflicting markers are also present in the corresponding pred_info.
    % The bool indicates whether there was a conflicting marker present.
    %
:- pred pragma_check_markers(pred_table::in, list(pred_id)::in,
    list(marker)::in, bool::out) is det.

pragma_check_markers(_, [], _, no).
pragma_check_markers(PredTable, [PredId | PredIds], ConflictList,
        WasConflict) :-
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    (
        list.member(Marker, ConflictList),
        check_marker(Markers, Marker)
    ->
        WasConflict = yes
    ;
        pragma_check_markers(PredTable, PredIds, ConflictList, WasConflict)
    ).

    % For each pred_id in the list, add the given markers to the
    % list of markers in the corresponding pred_info.
    %
:- pred pragma_add_marker(list(pred_id)::in,
    add_marker_pred_info::in(add_marker_pred_info), import_status::in,
    bool::in, pred_table::in, pred_table::out, bool::out) is det.

pragma_add_marker([], _, _, _, !PredTable, no).
pragma_add_marker([PredId | PredIds], UpdatePredInfo, Status, MustBeExported,
        !PredTable, WrongStatus) :-
    map.lookup(!.PredTable, PredId, PredInfo0),
    UpdatePredInfo(PredInfo0, PredInfo),
    (
        pred_info_is_exported(PredInfo),
        MustBeExported = yes,
        Status \= status_exported
    ->
        WrongStatus0 = yes
    ;
        WrongStatus0 = no
    ),
    map.det_update(PredId, PredInfo, !PredTable),
    pragma_add_marker(PredIds, UpdatePredInfo, Status, MustBeExported,
        !PredTable, WrongStatus1),
    bool.or(WrongStatus0, WrongStatus1, WrongStatus).

:- pred add_marker_pred_info(marker::in, pred_info::in, pred_info::out) is det.

add_marker_pred_info(Marker, !PredInfo) :-
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(Marker, Markers0, Markers),
    pred_info_set_markers(Markers, !PredInfo).

    % Succeed if a marker for an exported procedure must also be exported.
    %
:- pred marker_must_be_exported(marker::in) is semidet.

marker_must_be_exported(_) :-
    semidet_fail.

maybe_check_field_access_function(ModuleInfo, FuncName, FuncArity, Status,
        Context, !Specs) :-
    (
        is_field_access_function_name(ModuleInfo, FuncName, FuncArity,
            AccessType, FieldName)
    ->
        check_field_access_function(ModuleInfo, AccessType, FieldName,
            FuncName, FuncArity, Status, Context, !Specs)
    ;
        true
    ).

:- pred check_field_access_function(module_info::in, field_access_type::in,
    ctor_field_name::in, sym_name::in, arity::in, import_status::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

check_field_access_function(ModuleInfo, _AccessType, FieldName, FuncName,
        FuncArity, FuncStatus, Context, !Specs) :-
    adjust_func_arity(pf_function, FuncArity, PredArity),
    FuncCallId = simple_call_id(pf_function, FuncName, PredArity),

    % Check that a function applied to an exported type is also exported.
    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    (
        % Abstract types have status `abstract_exported', so errors won't be
        % reported for local field access functions for them.
        map.search(CtorFieldTable, FieldName, [FieldDefn]),
        FieldDefn = hlds_ctor_field_defn(_, DefnStatus, _, _, _),
        DefnStatus = status_exported,
        FuncStatus \= status_exported
    ->
        report_field_status_mismatch(Context, FuncCallId, !Specs)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred report_field_status_mismatch(prog_context::in, simple_call_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_field_status_mismatch(Context, CallId, !Specs) :-
    Pieces = [words("In declaration of"), simple_call(CallId), suffix(":"), nl,
        words("error: a field access function for an exported field"),
        words("must also be exported."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred report_unexpected_decl(string::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unexpected_decl(Descr, Context, !Specs) :-
    Pieces = [words("Error: unexpected or incorrect"),
        quote(Descr), words("declaration."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred pragma_status_error(sym_name::in, int::in, prog_context::in,
    string::in, list(error_spec)::in, list(error_spec)::out) is det.

pragma_status_error(Name, Arity, Context, PragmaName, !Specs) :-
    Pieces = [words("Error: `:- pragma " ++ PragmaName ++ "'"),
        words("declaration for exported predicate or function"),
        sym_name_and_arity(Name / Arity),
        words("must also be exported."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred pragma_conflict_error(sym_name::in, int::in, prog_context::in,
    string::in, list(error_spec)::in, list(error_spec)::out) is det.

pragma_conflict_error(Name, Arity, Context, PragmaName, !Specs) :-
    Pieces = [words("Error: `:- pragma " ++ PragmaName ++ "'"),
        words("declaration conflicts with previous pragma for"),
        sym_name_and_arity(Name / Arity), suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.make_hlds_passes.
%-----------------------------------------------------------------------------%
