%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mlds_to_target_util.m.
%
% This module contains utility types and predicates that are useful
% in more than one of mlds_to_{c,cs,java}.m.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_target_util.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module digraph.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module set_tree234.

    % Options to adjust the behaviour of the output predicates.
    %
:- type output_aux
    --->    oa_none
            % Nothing special.

    ;       oa_cname(mlds_type_name)
            % Pass down the class name if a definition is a constructor; this
            % is needed since the class name is not available for a constructor
            % in the MLDS.

    ;       oa_alloc_only
            % When writing out RTTI structure definitions, initialise members
            % with allocated top-level structures but don't fill in the fields
            % yet.

    ;       oa_force_init.
            % Used to force local variables to be initialised even if an
            % initialiser is not provided.

%---------------------------------------------------------------------------%

:- type output_generics
    --->    do_output_generics
    ;       do_not_output_generics.

%---------------------------------------------------------------------------%

:- func convert_qual_kind(mlds_qual_kind) = csj_qual_kind.

%---------------------------------------------------------------------------%

    % These types are used by many of the output_stmt style predicates to
    % return information about the statement's control flow,
    % i.e. about the different ways in which the statement can exit.
    % In general we only output the current statement if the previous
    % statement could complete normally (fall through).
    % We keep a set of exit methods since some statements (like an
    % if-then-else) could potentially break, and also fall through.
:- type exit_methods == set.set(exit_method).

:- type exit_method
    --->    can_break
    ;       can_continue
    ;       can_return
    ;       can_throw

    ;       can_fall_through.
            % Where the instruction can complete normally and execution
            % can continue with the following statement.

%---------------------------------------------------------------------------%

:- type func_info_csj
    --->    func_info_csj(
                func_info_params    :: mlds_func_params
            ).

%---------------------------------------------------------------------------%

    % Test whether one of the members of an mlds_enum class
    % is an enumeration constant.
    %
:- pred defn_is_enum_const(mlds_defn::in, mlds_field_var_defn::out) is semidet.

    % Succeeds iff this definition is a data definition which
    % defines a type_ctor_info constant.
    %
:- pred global_var_defn_is_type_ctor_info(mlds_global_var_defn::in) is semidet.

:- pred global_var_defn_is_private(mlds_global_var_defn::in) is semidet.
:- pred function_defn_is_private(mlds_function_defn::in) is semidet.
:- pred class_defn_is_private(mlds_class_defn::in) is semidet.

    % Succeeds iff the type of the local variable being defined
    % is mlds_commit_type.
    %
:- pred local_var_defn_is_commit_type(mlds_local_var_defn::in) is semidet.

%---------------------------------------------------------------------------%

:- pred remove_sym_name_prefix(sym_name::in, sym_name::in,
    sym_name::out) is det.

%---------------------------------------------------------------------------%

    % Return is_array if the corresponding C# or Java type is an array type.
    %
:- func type_category_is_array(type_ctor_category) = is_array.

%---------------------------------------------------------------------------%

:- pred output_array_dimensions(list(int)::in, io::di, io::uo) is det.

:- pred array_dimension_to_string(int::in, string::out) is det.

%---------------------------------------------------------------------------%

:- pred output_generic_tvars(list(tvar)::in, io::di, io::uo) is det.

:- pred generic_tvar_to_string(tvar::in, string::out) is det.

%---------------------------------------------------------------------------%

:- pred maybe_output_pred_proc_id_comment(bool::in, pred_proc_id::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % A value of type `indent' records the number of levels of indentation
    % to indent the next piece of code. Currently we output two spaces
    % for each level of indentation.
:- type indent == int.

:- pred output_n_indents(indent::in, io::di, io::uo) is det.

:- pred write_indented_line(indent::in, string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Output a Java/C# comment saying that the file was automatically
    % generated and give details such as the compiler version.
    %
:- pred output_auto_gen_comment(string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred add_scalar_inits(mlds_module_name::in, mlds_type::in,
    ml_scalar_common_type_num::in, mlds_initializer::in, int::in, int::out,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out,
    map(mlds_scalar_common, mlds_initializer)::in,
    map(mlds_scalar_common, mlds_initializer)::out) is det.

%---------------------------------------------------------------------------%

:- pred accumulate_env_var_names(mlds_function_defn::in,
    set(string)::in, set(string)::out) is det.

%---------------------------------------------------------------------------%

:- type code_addrs_in_consts
    --->    code_addrs_in_consts(
                % The set of code addresses we have seen so far
                % as arguments of ml_const rvals.
                set_tree234(mlds_code_addr),

                % The sequence number we will assign to the next mlds_code_addr
                % we will see as the argument of an ml_const rval.
                counter,

                % A list of the mlds_code_addrs we have seen so far
                % as the arguments of ml_const rvals, each with its
                % order-of-first-occurrence sequence number.
                % The list is ordered in reverse: if seqnumA > seqnumB,
                % then seqnumA, and its code address, will appear *before*
                % seqnumB in the list. This is to adding a new code address
                % and its sequence number an O(1) operation.
                assoc_list(int, mlds_code_addr)
            ).

:- func init_code_addrs_in_consts = code_addrs_in_consts.

    % Accumulate the method pointers (mlds_code_addrs stored in ml_const
    % rvals) in definitions or initializers.
    %
    % These predicates expect MLDS generated for C# or Java.
    %
:- pred method_ptrs_in_global_var_defns(list(mlds_global_var_defn)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.
:- pred method_ptrs_in_function_defns(list(mlds_function_defn)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.
:- pred method_ptrs_in_class_defns(list(mlds_class_defn)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.
:- pred method_ptrs_in_scalars(cord(mlds_initializer)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module library.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

convert_qual_kind(module_qual) = module_qual.
convert_qual_kind(type_qual) = type_qual.

%---------------------------------------------------------------------------%

defn_is_enum_const(Defn, FieldVarDefn) :-
    Defn = mlds_field_var(FieldVarDefn),
    FieldVarDefn ^ mfvd_decl_flags ^ mfvdf_constness = const.

global_var_defn_is_type_ctor_info(GlobalVarDefn) :-
    GlobalVarDefn = mlds_global_var_defn(_Name, _Context, _Flags, Type, _, _),
    Type = mlds_rtti_type(item_type(RttiId)),
    RttiId = ctor_rtti_id(_, RttiName),
    RttiName = type_ctor_type_ctor_info.

global_var_defn_is_private(GlobalVarDefn) :-
    GlobalVarDefn ^ mgvd_decl_flags ^ mgvdf_access = gvar_acc_module_only.

function_defn_is_private(FuncDefn) :-
    FuncFlags = FuncDefn ^ mfd_decl_flags,
    get_function_access(FuncFlags) = acc_private.

class_defn_is_private(ClassDefn) :-
    ClassFlags = ClassDefn ^ mcd_decl_flags,
    get_class_access(ClassFlags) = class_private.

local_var_defn_is_commit_type(LocalVarDefn) :-
    LocalVarDefn ^ mlvd_type = mlds_commit_type.

%---------------------------------------------------------------------------%

remove_sym_name_prefix(SymName0, Prefix, SymName) :-
    (
        SymName0 = qualified(Qual, Name),
        ( if Qual = Prefix then
            SymName = unqualified(Name)
        else
            remove_sym_name_prefix(Qual, Prefix, SymName1),
            SymName = qualified(SymName1, Name)
        )
    ;
        SymName0 = unqualified(_),
        unexpected($pred, "prefix not found")
    ).

%---------------------------------------------------------------------------%

type_category_is_array(CtorCat) = IsArray :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_system(cat_system_type_info)
        ; CtorCat = ctor_cat_system(cat_system_type_ctor_info)
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_user(_)
        ),
        IsArray = not_array
    ;
        ( CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_system(cat_system_typeclass_info)
        ; CtorCat = ctor_cat_system(cat_system_base_typeclass_info)
        ),
        IsArray = is_array
    ).

%---------------------------------------------------------------------------%

output_array_dimensions(ArrayDims, !IO) :-
    list.map(array_dimension_to_string, ArrayDims, Strings),
    list.foldr(io.write_string, Strings, !IO).

array_dimension_to_string(N, String) :-
    ( if N = 0 then
        String = "[]"
    else
        String = string.format("[%d]", [i(N)])
    ).

%---------------------------------------------------------------------------%

output_generic_tvars(Vars, !IO) :-
    (
        Vars = []
    ;
        Vars = [_ | _],
        io.write_string("<", !IO),
        io.write_list(Vars, ", ", output_generic_tvar, !IO),
        io.write_string(">", !IO)
    ).

:- pred output_generic_tvar(tvar::in, io::di, io::uo) is det.

output_generic_tvar(Var, !IO) :-
    generic_tvar_to_string(Var, VarName),
    io.write_string(VarName, !IO).

generic_tvar_to_string(Var, VarName) :-
    varset.lookup_name(varset.init, Var, "MR_tvar_", VarName).

%---------------------------------------------------------------------------%

maybe_output_pred_proc_id_comment(AutoComments, PredProcId, !IO) :-
    (
        AutoComments = yes,
        PredProcId = proc(PredId, ProcId),
        pred_id_to_int(PredId, PredIdNum),
        proc_id_to_int(ProcId, ProcIdNum),
        io.format("// pred_id: %d, proc_id: %d\n",
            [i(PredIdNum), i(ProcIdNum)], !IO)
    ;
        AutoComments = no
    ).

%---------------------------------------------------------------------------%

output_n_indents(N, !IO) :-
    ( if N =< 0 then
        true
    else
        io.write_string("  ", !IO),
        output_n_indents(N - 1, !IO)
    ).

write_indented_line(Indent, Line, !IO) :-
    output_n_indents(Indent, !IO),
    io.write_string(Line, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

output_auto_gen_comment(SourceFileName, !IO)  :-
    library.version(Version, Fullarch),
    io.write_string("//\n//\n// Automatically generated from ", !IO),
    io.write_string(SourceFileName, !IO),
    io.write_string(" by the Mercury Compiler,\n", !IO),
    io.write_string("// version ", !IO),
    io.write_string(Version, !IO),
    io.nl(!IO),
    io.write_string("// configured for ", !IO),
    io.write_string(Fullarch, !IO),
    io.nl(!IO),
    io.write_string("//\n", !IO),
    io.write_string("//\n", !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

add_scalar_inits(MLDS_ModuleName, Type, TypeNum, Initializer,
        RowNum, RowNum + 1, !Graph, !Map) :-
    Scalar = ml_scalar_common(MLDS_ModuleName, Type, TypeNum, RowNum),
    map.det_insert(Scalar, Initializer, !Map),
    digraph.add_vertex(Scalar, _Key, !Graph),
    add_scalar_deps(Scalar, Initializer, !Graph).

:- pred add_scalar_deps(mlds_scalar_common::in, mlds_initializer::in,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out) is det.

add_scalar_deps(FromScalar, Initializer, !Graph) :-
    (
        Initializer = init_obj(Rval),
        add_scalar_deps_rval(FromScalar, Rval, !Graph)
    ;
        Initializer = init_struct(_Type, Initializers),
        list.foldl(add_scalar_deps(FromScalar), Initializers, !Graph)
    ;
        Initializer = init_array(Initializers),
        list.foldl(add_scalar_deps(FromScalar), Initializers, !Graph)
    ;
        Initializer = no_initializer
    ).

:- pred add_scalar_deps_rval(mlds_scalar_common::in, mlds_rval::in,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out) is det.

add_scalar_deps_rval(FromScalar, Rval, !Graph) :-
    (
        ( Rval = ml_mkword(_, SubRvalA)
        ; Rval = ml_unop(_, SubRvalA)
        ; Rval = ml_vector_common_row_addr(_, SubRvalA)
        ),
        add_scalar_deps_rval(FromScalar, SubRvalA, !Graph)
    ;
        Rval = ml_binop(_, SubRvalA, SubRvalB),
        add_scalar_deps_rval(FromScalar, SubRvalA, !Graph),
        add_scalar_deps_rval(FromScalar, SubRvalB, !Graph)
    ;
        ( Rval = ml_scalar_common(ToScalar)
        ; Rval = ml_scalar_common_addr(ToScalar)
        ),
        digraph.add_vertices_and_edge(FromScalar, ToScalar, !Graph)
    ;
        Rval = ml_const(_)
    ;
        Rval = ml_self(_)
    ;
        ( Rval = ml_lval(_Lval)
        ; Rval = ml_mem_addr(_Lval)
        ),
        unexpected($module, $pred, "lval or mem_addr")
    ).

%---------------------------------------------------------------------------%

accumulate_env_var_names(FuncDefn, !EnvVarNames) :-
    FuncDefn = mlds_function_defn(_, _, _, _, _, _, _, EnvVarNames, _),
    set.union(EnvVarNames, !EnvVarNames).

%---------------------------------------------------------------------------%

init_code_addrs_in_consts =
    code_addrs_in_consts(set_tree234.init, counter.init(0), []).

%---------------------%

:- pred method_ptrs_in_defns(list(mlds_defn)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_defns([], !CodeAddrsInConsts).
method_ptrs_in_defns([Defn | Defns], !CodeAddrsInConsts) :-
    method_ptrs_in_defn(Defn, !CodeAddrsInConsts),
    method_ptrs_in_defns(Defns, !CodeAddrsInConsts).

:- pred method_ptrs_in_defn(mlds_defn::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_defn(Defn, !CodeAddrsInConsts) :-
    (
        Defn = mlds_global_var(GlobalVarDefn),
        method_ptrs_in_global_var_defn(GlobalVarDefn, !CodeAddrsInConsts)
    ;
        Defn = mlds_local_var(LocalVarDefn),
        method_ptrs_in_local_var_defn(LocalVarDefn, !CodeAddrsInConsts)
    ;
        Defn = mlds_field_var(FieldVarDefn),
        method_ptrs_in_field_var_defn(FieldVarDefn, !CodeAddrsInConsts)
    ;
        Defn = mlds_function(FuncDefn),
        method_ptrs_in_function_defn(FuncDefn, !CodeAddrsInConsts)
    ;
        Defn = mlds_class(ClassDefn),
        method_ptrs_in_class_defn(ClassDefn, !CodeAddrsInConsts)
    ).

%---------------------%

method_ptrs_in_global_var_defns([], !CodeAddrsInConsts).
method_ptrs_in_global_var_defns([GlobalVarDefn | GlobalVarDefns],
        !CodeAddrsInConsts) :-
    method_ptrs_in_global_var_defn(GlobalVarDefn, !CodeAddrsInConsts),
    method_ptrs_in_global_var_defns(GlobalVarDefns, !CodeAddrsInConsts).

:- pred method_ptrs_in_global_var_defn(mlds_global_var_defn::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_global_var_defn(GlobalVarDefn, !CodeAddrsInConsts) :-
    GlobalVarDefn = mlds_global_var_defn(_, _, _, _, Initializer, _),
    method_ptrs_in_initializer(Initializer, !CodeAddrsInConsts).

%---------------------%

:- pred method_ptrs_in_local_var_defns(list(mlds_local_var_defn)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_local_var_defns([], !CodeAddrsInConsts).
method_ptrs_in_local_var_defns([LocalVarDefn | LocalVarDefns],
        !CodeAddrsInConsts) :-
    method_ptrs_in_local_var_defn(LocalVarDefn, !CodeAddrsInConsts),
    method_ptrs_in_local_var_defns(LocalVarDefns, !CodeAddrsInConsts).

:- pred method_ptrs_in_local_var_defn(mlds_local_var_defn::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_local_var_defn(LocalVarDefn, !CodeAddrsInConsts) :-
    LocalVarDefn = mlds_local_var_defn(_, _, _, Initializer, _),
    method_ptrs_in_initializer(Initializer, !CodeAddrsInConsts).

%---------------------%

:- pred method_ptrs_in_field_var_defn(mlds_field_var_defn::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_field_var_defn(FieldVarDefn, !CodeAddrsInConsts) :-
    FieldVarDefn = mlds_field_var_defn(_, _, _, _, Initializer, _),
    method_ptrs_in_initializer(Initializer, !CodeAddrsInConsts).

%---------------------%

method_ptrs_in_function_defns([], !CodeAddrsInConsts).
method_ptrs_in_function_defns([FuncDefn | FuncDefns], !CodeAddrsInConsts) :-
    method_ptrs_in_function_defn(FuncDefn, !CodeAddrsInConsts),
    method_ptrs_in_function_defns(FuncDefns, !CodeAddrsInConsts).

:- pred method_ptrs_in_function_defn(mlds_function_defn::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_function_defn(FuncDefn, !CodeAddrsInConsts) :-
    FuncDefn = mlds_function_defn(_, _, _, _MaybeID, _Params, Body,
        _Attributes, _EnvVars, _MaybeRequireTailrecInfo),
    (
        Body = body_defined_here(Stmt),
        method_ptrs_in_statement(Stmt, !CodeAddrsInConsts)
    ;
        Body = body_external
    ).

%---------------------%

method_ptrs_in_class_defns([], !CodeAddrsInConsts).
method_ptrs_in_class_defns([ClassDefn | ClassDefns], !CodeAddrsInConsts) :-
    method_ptrs_in_class_defn(ClassDefn, !CodeAddrsInConsts),
    method_ptrs_in_class_defns(ClassDefns, !CodeAddrsInConsts).

:- pred method_ptrs_in_class_defn(mlds_class_defn::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_class_defn(ClassDefn, !CodeAddrsInConsts) :-
    ClassDefn = mlds_class_defn(_, _, _, _, _, _, _, _, Ctors, Members),
    method_ptrs_in_function_defns(Ctors, !CodeAddrsInConsts),
    method_ptrs_in_defns(Members, !CodeAddrsInConsts).

%---------------------%

:- pred method_ptrs_in_statements(list(mlds_stmt)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_statements([], !CodeAddrsInConsts).
method_ptrs_in_statements([Stmt | Stmts], !CodeAddrsInConsts) :-
    method_ptrs_in_statement(Stmt, !CodeAddrsInConsts),
    method_ptrs_in_statements(Stmts, !CodeAddrsInConsts).

:- pred method_ptrs_in_statement(mlds_stmt::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_statement(Stmt, !CodeAddrsInConsts) :-
    (
        Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, _Context),
        method_ptrs_in_local_var_defns(LocalVarDefns, !CodeAddrsInConsts),
        method_ptrs_in_function_defns(FuncDefns, !CodeAddrsInConsts),
        method_ptrs_in_statements(SubStmts, !CodeAddrsInConsts)
    ;
        Stmt = ml_stmt_while(_Kind, Rval, SubStmt, _Context),
        method_ptrs_in_rval(Rval, !CodeAddrsInConsts),
        method_ptrs_in_statement(SubStmt, !CodeAddrsInConsts)
    ;
        Stmt = ml_stmt_if_then_else(SubRval, ThenStmt, MaybeElseStmt,
            _Context),
        method_ptrs_in_rval(SubRval, !CodeAddrsInConsts),
        method_ptrs_in_statement(ThenStmt, !CodeAddrsInConsts),
        (
            MaybeElseStmt = yes(ElseStmt),
            method_ptrs_in_statement(ElseStmt, !CodeAddrsInConsts)
        ;
            MaybeElseStmt = no
        )
    ;
        Stmt = ml_stmt_switch(_Type, SubRval, _Range, Cases, Default,
            _Context),
        method_ptrs_in_rval(SubRval, !CodeAddrsInConsts),
        method_ptrs_in_switch_cases(Cases, !CodeAddrsInConsts),
        method_ptrs_in_switch_default(Default, !CodeAddrsInConsts)
    ;
        Stmt = ml_stmt_label(_, _Context),
        unexpected($pred, "labels are not supported in C# or Java.")
    ;
        Stmt = ml_stmt_goto(Target, _Context),
        (
            ( Target = goto_break
            ; Target = goto_continue
            )
        ;
            Target = goto_label(_),
            unexpected($pred, "goto label is not supported in C# or Java.")
        )
    ;
        Stmt = ml_stmt_computed_goto(_, _, _Context),
        unexpected($pred, "computed gotos are not supported in C# or Java.")
    ;
        Stmt = ml_stmt_try_commit(_Lval, BodyStmt, HandlerStmt, _Context),
        % We don't check "_Lval" here as we expect it to be a local variable
        % of type mlds_commit_type.
        method_ptrs_in_statement(BodyStmt, !CodeAddrsInConsts),
        method_ptrs_in_statement(HandlerStmt, !CodeAddrsInConsts)
    ;
        Stmt = ml_stmt_do_commit(_Rval, _Context)
        % We don't check "_Rval" here as we expect it to be a local variable
        % of type mlds_commit_type.
    ;
        Stmt = ml_stmt_return(Rvals, _Context),
        method_ptrs_in_rvals(Rvals, !CodeAddrsInConsts)
    ;
        Stmt = ml_stmt_call(_FuncSig, _Rval, _MaybeThis, Rvals, _ReturnVars,
            _IsTailCall, _Markers, _Context),
        % We don't check "_Rval" - it may be a code address but is a
        % standard call rather than a function pointer use.
        method_ptrs_in_rvals(Rvals, !CodeAddrsInConsts)
    ;
        Stmt = ml_stmt_atomic(AtomicStmt, _Context),
        ( if
            AtomicStmt = new_object(Lval, _MaybeTag, _Bool,
                _Type, _MemRval, _MaybeCtorName, Rvals, _Types, _MayUseAtomic,
                _AllocId)
        then
            % We don't need to check "_MemRval" since this just stores
            % the amount of memory needed for the new object.
            method_ptrs_in_lval(Lval, !CodeAddrsInConsts),
            method_ptrs_in_rvals(Rvals, !CodeAddrsInConsts)
        else if
            AtomicStmt = assign(Lval, Rval)
        then
            method_ptrs_in_lval(Lval, !CodeAddrsInConsts),
            method_ptrs_in_rval(Rval, !CodeAddrsInConsts)
        else
            true
        )
    ).

:- pred method_ptrs_in_switch_default(mlds_switch_default::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_switch_default(Default, !CodeAddrsInConsts) :-
    (
        ( Default = default_is_unreachable
        ; Default = default_do_nothing
        )
    ;
        Default = default_case(Stmt),
        method_ptrs_in_statement(Stmt, !CodeAddrsInConsts)
    ).

:- pred method_ptrs_in_switch_cases(list(mlds_switch_case)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_switch_cases([], !CodeAddrsInConsts).
method_ptrs_in_switch_cases([Case | Cases], !CodeAddrsInConsts) :-
    Case = mlds_switch_case(_FirstCond, _LaterConds, Stmt),
    method_ptrs_in_statement(Stmt, !CodeAddrsInConsts),
    method_ptrs_in_switch_cases(Cases, !CodeAddrsInConsts).

method_ptrs_in_scalars(Cord, !CodeAddrsInConsts) :-
    cord.foldl_pred(method_ptrs_in_initializer, Cord, !CodeAddrsInConsts).

:- pred method_ptrs_in_initializer(mlds_initializer::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_initializer(Initializer, !CodeAddrsInConsts) :-
    (
        Initializer = no_initializer
    ;
        Initializer = init_struct(_Type, SubInitializers),
        method_ptrs_in_initializers(SubInitializers, !CodeAddrsInConsts)
    ;
        Initializer = init_array(SubInitializers),
        method_ptrs_in_initializers(SubInitializers, !CodeAddrsInConsts)
    ;
        Initializer = init_obj(Rval),
        method_ptrs_in_rval(Rval, !CodeAddrsInConsts)
    ).

:- pred method_ptrs_in_initializers(list(mlds_initializer)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_initializers([], !CodeAddrsInConsts).
method_ptrs_in_initializers([Initializer | Initializers],
        !CodeAddrsInConsts) :-
    method_ptrs_in_initializer(Initializer, !CodeAddrsInConsts),
    method_ptrs_in_initializers(Initializers, !CodeAddrsInConsts).

:- pred method_ptrs_in_rvals(list(mlds_rval)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_rvals([], !CodeAddrsInConsts).
method_ptrs_in_rvals([Rval | Rvals], !CodeAddrsInConsts) :-
    method_ptrs_in_rval(Rval, !CodeAddrsInConsts),
    method_ptrs_in_rvals(Rvals, !CodeAddrsInConsts).

:- pred method_ptrs_in_rval(mlds_rval::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_rval(Rval, !CodeAddrsInConsts) :-
    (
        Rval = ml_lval(Lval),
        method_ptrs_in_lval(Lval, !CodeAddrsInConsts)
    ;
        Rval = ml_mkword(_Tag, SubRval),
        method_ptrs_in_rval(SubRval, !CodeAddrsInConsts)
    ;
        Rval = ml_const(RvalConst),
        (
            RvalConst = mlconst_code_addr(CodeAddr),
            !.CodeAddrsInConsts = code_addrs_in_consts(Seen0, Counter0, Rev0),
            ( if set_tree234.insert_new(CodeAddr, Seen0, Seen) then
                counter.allocate(SeqNum, Counter0, Counter),
                Rev = [SeqNum - CodeAddr | Rev0],
                !:CodeAddrsInConsts = code_addrs_in_consts(Seen, Counter, Rev)
            else
                true
            )
        ;
            ( RvalConst = mlconst_true
            ; RvalConst = mlconst_false
            ; RvalConst = mlconst_int(_)
            ; RvalConst = mlconst_uint(_)
            ; RvalConst = mlconst_int8(_)
            ; RvalConst = mlconst_uint8(_)
            ; RvalConst = mlconst_int16(_)
            ; RvalConst = mlconst_uint16(_)
            ; RvalConst = mlconst_int32(_)
            ; RvalConst = mlconst_uint32(_)
            ; RvalConst = mlconst_char(_)
            ; RvalConst = mlconst_enum(_, _)
            ; RvalConst = mlconst_foreign(_, _, _)
            ; RvalConst = mlconst_float(_)
            ; RvalConst = mlconst_string(_)
            ; RvalConst = mlconst_multi_string(_)
            ; RvalConst = mlconst_named_const(_, _)
            ; RvalConst = mlconst_data_addr_local_var(_, _)
            ; RvalConst = mlconst_data_addr_global_var(_, _)
            ; RvalConst = mlconst_data_addr_rtti(_, _)
            ; RvalConst = mlconst_data_addr_tabling(_, _, _)
            ; RvalConst = mlconst_null(_)
            )
        )
    ;
        Rval = ml_unop(_UnaryOp, SubRval),
        method_ptrs_in_rval(SubRval, !CodeAddrsInConsts)
    ;
        Rval = ml_binop(_BinaryOp, SubRvalA, SubRvalB),
        method_ptrs_in_rval(SubRvalA, !CodeAddrsInConsts),
        method_ptrs_in_rval(SubRvalB, !CodeAddrsInConsts)
    ;
        Rval = ml_vector_common_row_addr(_, RowRval),
        method_ptrs_in_rval(RowRval, !CodeAddrsInConsts)
    ;
        ( Rval = ml_scalar_common(_)
        ; Rval = ml_scalar_common_addr(_)
        ; Rval = ml_mem_addr(_Address)
        ; Rval = ml_self(_Type)
        )
    ).

:- pred method_ptrs_in_lval(mlds_lval::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_lval(Lval, !CodeAddrsInConsts) :-
    (
        Lval = ml_mem_ref(_Rval, _Type)
        % Here, "_Rval" is the address of a variable so we don't check it.
    ;
        Lval = ml_field(_MaybeTag, _Rval, _FieldId, _FieldType, _PtrType)
        % Here, "_Rval" is a pointer to a cell on the heap, and doesn't need
        % to be considered.
    ;
        ( Lval = ml_local_var(_Variable, _Type)
        ; Lval = ml_global_var(_Variable, _Type)
        ; Lval = ml_target_global_var_ref(_)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_target_util.
%---------------------------------------------------------------------------%
