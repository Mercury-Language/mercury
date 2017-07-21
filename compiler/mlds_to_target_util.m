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

:- import_module bool.
:- import_module digraph.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module set.

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

:- pred collect_env_var_names(list(mlds_defn)::in, list(string)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module library.
:- import_module require.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

convert_qual_kind(module_qual) = module_qual.
convert_qual_kind(type_qual) = type_qual.

%---------------------------------------------------------------------------%

defn_is_enum_const(Defn, FieldVarDefn) :-
    Defn = mlds_field_var(FieldVarDefn),
    Flags = FieldVarDefn ^ mfvd_decl_flags,
    get_data_constness(Flags) = const.

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

collect_env_var_names(Defns, EnvVarNames) :-
    list.foldl(accumulate_env_var_names, Defns, set.init, EnvVarNamesSet),
    EnvVarNames = set.to_sorted_list(EnvVarNamesSet).

:- pred accumulate_env_var_names(mlds_defn::in,
    set(string)::in, set(string)::out) is det.

accumulate_env_var_names(Defn, !EnvVarNames) :-
    (
        ( Defn = mlds_global_var(_)
        ; Defn = mlds_local_var(_)
        ; Defn = mlds_field_var(_)
        ; Defn = mlds_class(_)
        )
    ;
        Defn = mlds_function(FunctionDefn),
        FunctionDefn = mlds_function_defn(_, _, _, _, _, _, _, EnvVarNames, _),
        set.union(EnvVarNames, !EnvVarNames)
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_target_util.
%---------------------------------------------------------------------------%
