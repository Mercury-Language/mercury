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
:- import_module libs.
:- import_module libs.indent.
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

    ;       oa_cname(mlds_class_name, arity)
            % Pass down the class name if a definition is a constructor;
            % this is needed because the class name is not available
            % for a constructor in the MLDS.

    ;       oa_alloc_only
            % When writing out RTTI structure definitions, initialise members
            % with allocated top-level structures but don't fill in the fields
            % yet.

    ;       oa_force_init.
            % Used to force local variables to be initialised even if an
            % initialiser is not provided.

%---------------------------------------------------------------------------%

    % Have we printed anything on the current line before we start printing
    % an initializer?
    %
:- type initializer_starts
    --->    not_at_start_of_line    % Yes.
    ;       at_start_of_line.       % No.

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

    % The modules that write out MLDS statements use this type to check
    % whether goto_break_switch and goto_break_loop are used in the context
    % they were meant for.
    %
:- type break_context
    --->    bc_none
            % We are inside neither a loop nor a switch.

    ;       bc_switch
            % The nearest enclosing structure we can break from is a switch.

    ;       bc_loop.
            % The nearest enclosing structure we can break from is a loop.

%---------------------------------------------------------------------------%

:- type func_info_csj
    --->    func_info_csj(
                func_info_params    :: mlds_func_params
            ).

%---------------------------------------------------------------------------%

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

    % Return the suffix to add to the name of a Java or C# type
    % to transform it to represent an array of N elements of that type.
    % N being zero means the number of elements is unknown.
    %
:- func array_dimension_to_string(int) = string.

    % Return the suffix to add to the name of a Java or C# type
    % to transform it to represent an array of array of ... elements
    % of that type. The first dimension gets printed last.
    %
:- func array_dimensions_to_string(list(int)) = string.

    % Given the name of an element type and a list of array dimensions,
    % return a string representing the type of array of those dimensions.
    %
:- func add_array_dimensions(string, list(int)) = string.

    % Replace the innermost array dimension (which is expected to be
    % initially unknown) by the given known size.
    %
:- pred make_last_dimension_known_size(list(int)::in, int::in,
    list(int)::out) is det.

%---------------------------------------------------------------------------%

    % init_arg_wrappers_cs_java(ArrayDims, LParen, RParen):
    %
    % Return the kinds of parentheses you need to wrap around an initializer
    % (for something that either is or is not an array) in C# and Java.
    %
:- pred init_arg_wrappers_cs_java(list(int)::in, string::out, string::out)
    is det.

%---------------------------------------------------------------------------%

:- func generic_tvar_to_string(tvar) = string.

:- func generic_tvars_to_string(list(tvar)) = string.
:- pred output_generic_tvars(io.text_output_stream::in, list(tvar)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred maybe_output_pre_function_comment(io.text_output_stream::in,
    bool::in, string::in,string::in,  string::in, mlds_function_defn::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred write_indentstr_line(io.text_output_stream::in, string::in, string::in,
    io::di, io::uo) is det.

:- pred scope_indent(mlds_stmt::in, indent::in, indent::out) is det.

%---------------------------------------------------------------------------%

    % Output a Java/C# comment saying that the file was automatically
    % generated and give details such as the compiler version.
    %
:- pred output_auto_gen_comment(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

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

:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_util.

:- import_module char.
:- import_module int.
:- import_module library.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%

convert_qual_kind(module_qual) = module_qual.
convert_qual_kind(type_qual) = type_qual.

%---------------------------------------------------------------------------%

global_var_defn_is_type_ctor_info(GlobalVarDefn) :-
    GlobalVarDefn = mlds_global_var_defn(_Name, _Context, _Flags, Type, _, _),
    Type = mlds_rtti_type(item_type(RttiId)),
    RttiId = ctor_rtti_id(_, RttiName),
    RttiName = type_ctor_type_ctor_info.

global_var_defn_is_private(GlobalVarDefn) :-
    GlobalVarDefn ^ mgvd_decl_flags ^ mgvdf_access = gvar_acc_module_only.

function_defn_is_private(FuncDefn) :-
    FuncDefn ^ mfd_decl_flags = mlds_function_decl_flags(Access, _PerInstance),
    Access = func_private.

class_defn_is_private(ClassDefn) :-
    ClassDefn ^ mcd_decl_flags =
        mlds_class_decl_flags(Access, _Overridability, _Constness),
    Access = class_private.

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

array_dimension_to_string(ArrayDim) = String :-
    ( if ArrayDim = 0 then
        String = "[]"
    else
        String = string.format("[%d]", [i(ArrayDim)])
    ).

array_dimensions_to_string(ArrayDims) = ArrayDimsStr :-
    ArrayDimStrs = list.map(array_dimension_to_string, ArrayDims),
    list.reverse(ArrayDimStrs, RevArrayDimStrs),
    string.append_list(RevArrayDimStrs, ArrayDimsStr).

add_array_dimensions(TypeName, ArrayDims) = ArrayTypeName :-
    (
        ArrayDims = [],
        % Optimize the common case.
        ArrayTypeName = TypeName
    ;
        ArrayDims = [_ | _],
        ArrayTypeName = TypeName ++ array_dimensions_to_string(ArrayDims)
    ).

make_last_dimension_known_size(ArrayDims0, Size, ArrayDims) :-
    ( if list.split_last(ArrayDims0, InitDims, 0) then
        ArrayDims = InitDims ++ [Size]
    else
        unexpected($pred, "missing unknown array dimension")
    ).

%---------------------------------------------------------------------------%

init_arg_wrappers_cs_java(ArrayDims, LParen, RParen) :-
    (
        ArrayDims = [_ | _],
        % The new object will be an array, so we need to initialise it
        % using array literals syntax.
        LParen = " {",
        RParen = "}"
    ;
        ArrayDims = [],
        % The new object will not be an array.
        LParen = "(",
        RParen = ")"
    ).

%---------------------------------------------------------------------------%

generic_tvar_to_string(Var) = VarName :-
    varset.lookup_name_default_prefix(varset.init, Var, "MR_tvar_", VarName).

generic_tvars_to_string(Vars) = VarNamesStr :-
    (
        Vars = [],
        VarNamesStr = ""
    ;
        Vars = [_ | _],
        VarNameStrs = list.map(generic_tvar_to_string, Vars),
        string.format("<%s>", [s(string.join_list(", ", VarNameStrs))],
            VarNamesStr)
    ).

output_generic_tvars(Stream, Vars, !IO) :-
    VarNamesStr = generic_tvars_to_string(Vars),
    io.write_string(Stream, VarNamesStr, !IO).

%---------------------------------------------------------------------------%

maybe_output_pre_function_comment(Stream, AutoComments, IndentStr,
        SC, EC, FunctionDefn, !IO) :-
    (
        AutoComments = no
    ;
        AutoComments = yes,
        FunctionDefn = mlds_function_defn(FuncName, _Context, _Flags, Source,
            _Params, _MaybeBody, _EnvVarNames, _MaybeRequireTailrecInfo),
        (
            ( Source = mlds_func_source_constructor,    Desc = "constructor"
            ; Source = mlds_func_source_continuation,   Desc = "continuation"
            ; Source = mlds_func_source_trace,          Desc = "trace"
            ; Source = mlds_func_source_wrapper,        Desc = "wrapper"
            ),
            io.format(Stream, "%s%s%s%s\n",
                [s(IndentStr), s(SC), s(Desc), s(EC)], !IO)
        ;
            Source = mlds_func_source_proc(PredProcId),
            PredProcId = proc(PredId, ProcId),
            PredIdNum = pred_id_to_int(PredId),
            ProcIdNum = proc_id_to_int(ProcId),
            io.format(Stream, "%s%spred_id: %d, proc_id: %d%s\n",
                [s(IndentStr), s(SC), i(PredIdNum), i(ProcIdNum), s(EC)], !IO),
            (
                FuncName = mlds_function_name(PlainFuncName),
                PlainFuncName = mlds_plain_func_name(FuncLabel, _PredId),
                FuncLabel = mlds_func_label(ProcLabel, _MaybeAuxFuncId),
                ProcLabel = mlds_proc_label(PredLabel, _ProcId),
                (
                    PredLabel = mlds_user_pred_label(PorF, _MaybeModuleName,
                        PredName0, PredFormArity),
% We used to print the pre-function comment in .c files using /* */ syntax.
% For a predicate whose name ends in '*', such as the usual multiplication
% operators for the various types in the standard library, the predname/arity
% we print below will end the comment prematurely. We can use this code
% to mangle those predicate names in a way that leaves them recognizable,
% and does not affect names that do not contain '*' chars.
%
% We used to use /* */ for these comments because we used to put this comment
% in the middle of the function declaration. Putting it *before* the function
% declaration allows us to use // syntax for the comment, which is a simpler
% solution than the commented-out code here.
%
%                   ( is string.contains_char('*', PredName0) then
%                       string.to_char_list(PredName0, PredNameChars0),
%                       replace_all_stars(PredNameChars0, PredNameChars),
%                       string.from_char_list(PredNameChars, PredName),
%                   else
%                       PredName = PredName0
%                   ),
                    PredName = PredName0,
                    PorFStr = pred_or_func_to_str(PorF),
                    user_arity_pred_form_arity(PorF, UserArity, PredFormArity),
                    UserArity = user_arity(Arity),
                    io.format(Stream, "%s%s%s %s/%d%s\n",
                        [s(IndentStr), s(SC), s(PorFStr),
                        s(PredName), i(Arity), s(EC)], !IO)
                ;
                    PredLabel = mlds_special_pred_label(PredName,
                        _MaybeModuleName, TypeName, TypeArity),
                    io.format(Stream, "%s%s%s for %s/%d%s\n",
                        [s(IndentStr), s(SC), s(PredName),
                        s(TypeName), i(TypeArity), s(EC)], !IO)
                )
            ;
                FuncName = mlds_function_export(Name),
                io.format(Stream, "%s%sexport %s%s\n",
                    [s(IndentStr), s(SC), s(Name), s(EC)], !IO)
            )
        )
    ).

:- pred replace_all_stars(list(char)::in, list(char)::out) is det.
:- pragma consider_used(pred(replace_all_stars/2)).

replace_all_stars([], []).
replace_all_stars([HeadChar0 | TailChars0], Chars) :-
    replace_all_stars(TailChars0, TailChars),
    ( if HeadChar0 = ('*') then
        Chars = ['s', 't', 'a', 'r' | TailChars]
    else
        Chars = [HeadChar0 | TailChars]
    ).

%---------------------------------------------------------------------------%

write_indentstr_line(Stream, IndentStr, Line, !IO) :-
    io.format(Stream, "%s%s\n", [s(IndentStr), s(Line)], !IO).

scope_indent(Stmt, CurIndent, ScopeIndent) :-
    ( if Stmt = ml_stmt_block(_, _, _, _) then
        % We want the statements in the block to be indented by CurIndent + 1;
        % it is ok for the braces around the block to be indented by CurIndent.
        ScopeIndent = CurIndent
    else
        ScopeIndent = CurIndent + 1u
    ).

%---------------------------------------------------------------------------%

output_auto_gen_comment(Stream, SourceFileName, !IO)  :-
    library.version(Version, Fullarch),
    io.format(Stream, "//\n//\n", [], !IO),
    io.format(Stream, "// Automatically generated from %s by %s,\n",
        [s(SourceFileName), s("the Mercury Compiler")], !IO),
    io.format(Stream, "// version %s\n", [s(Version)], !IO),
    io.format(Stream, "// configured for %s\n", [s(Fullarch)], !IO),
    io.format(Stream, "//\n//\n\n", [], !IO).

%---------------------------------------------------------------------------%

add_scalar_inits(MLDS_ModuleName, Type, TypeNum, Initializer,
        RowNum, RowNum + 1, !Graph, !Map) :-
    Scalar = mlds_scalar_common(MLDS_ModuleName, Type, TypeNum, RowNum),
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
        ; Rval = ml_box(_, SubRvalA)
        ; Rval = ml_unbox(_, SubRvalA)
        ; Rval = ml_cast(_, SubRvalA)
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
        unexpected($pred, "lval or mem_addr")
    ).

%---------------------------------------------------------------------------%

accumulate_env_var_names(FuncDefn, !EnvVarNames) :-
    FuncDefn = mlds_function_defn(_, _, _, _, _, _, EnvVarNames, _),
    set.union(EnvVarNames, !EnvVarNames).

%---------------------------------------------------------------------------%

init_code_addrs_in_consts =
    code_addrs_in_consts(set_tree234.init, counter.init(0), []).

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

:- pred method_ptrs_in_field_var_defns(list(mlds_field_var_defn)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_field_var_defns([], !CodeAddrsInConsts).
method_ptrs_in_field_var_defns([FieldVarDefn | FieldVarDefns],
        !CodeAddrsInConsts) :-
    method_ptrs_in_field_var_defn(FieldVarDefn, !CodeAddrsInConsts),
    method_ptrs_in_field_var_defns(FieldVarDefns, !CodeAddrsInConsts).

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
        _EnvVars, _MaybeRequireTailrecInfo),
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
    ClassDefn = mlds_class_defn(_, _, _, _, _, _, _, _,
        MemberFields, MemberClasses, MemberMethods, Ctors),
    method_ptrs_in_field_var_defns(MemberFields, !CodeAddrsInConsts),
    method_ptrs_in_class_defns(MemberClasses, !CodeAddrsInConsts),
    method_ptrs_in_function_defns(MemberMethods, !CodeAddrsInConsts),
    method_ptrs_in_function_defns(Ctors, !CodeAddrsInConsts).

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
        Stmt = ml_stmt_while(_Kind, Rval, SubStmt, _LocalLoopVars, _Context),
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
            ( Target = goto_break_switch
            ; Target = goto_break_loop
            ; Target = goto_continue_loop
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
        Stmt = ml_stmt_call(_FuncSig, _Rval, Rvals, _ReturnVars,
            _IsTailCall, _Context),
        % We don't check "_Rval" - it may be a code address but is a
        % standard call rather than a function pointer use.
        method_ptrs_in_rvals(Rvals, !CodeAddrsInConsts)
    ;
        Stmt = ml_stmt_atomic(AtomicStmt, _Context),
        ( if
            AtomicStmt = new_object(Lval, _MaybeTag, _Bool,
                _Type, _MemRval, _MaybeCtorName, TypedRvals, _MayUseAtomic,
                _AllocId)
        then
            % We don't need to check "_MemRval" since this just stores
            % the amount of memory needed for the new object.
            method_ptrs_in_lval(Lval, !CodeAddrsInConsts),
            method_ptrs_in_typed_rvals(TypedRvals, !CodeAddrsInConsts)
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

:- pred method_ptrs_in_typed_rvals(list(mlds_typed_rval)::in,
    code_addrs_in_consts::in, code_addrs_in_consts::out) is det.

method_ptrs_in_typed_rvals([], !CodeAddrsInConsts).
method_ptrs_in_typed_rvals([TypedRval | TypedRvals], !CodeAddrsInConsts) :-
    TypedRval = ml_typed_rval(Rval, _Type),
    method_ptrs_in_rval(Rval, !CodeAddrsInConsts),
    method_ptrs_in_typed_rvals(TypedRvals, !CodeAddrsInConsts).

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
            ; RvalConst = mlconst_int64(_)
            ; RvalConst = mlconst_uint64(_)
            ; RvalConst = mlconst_char(_)
            ; RvalConst = mlconst_enum(_, _)
            ; RvalConst = mlconst_foreign(_, _, _)
            ; RvalConst = mlconst_float(_)
            ; RvalConst = mlconst_string(_)
            ; RvalConst = mlconst_multi_string(_)
            ; RvalConst = mlconst_named_const(_, _)
            ; RvalConst = mlconst_data_addr_local_var(_)
            ; RvalConst = mlconst_data_addr_global_var(_, _)
            ; RvalConst = mlconst_data_addr_rtti(_, _)
            ; RvalConst = mlconst_data_addr_tabling(_, _)
            ; RvalConst = mlconst_null(_)
            )
        )
    ;
        ( Rval = ml_box(_Type, SubRval)
        ; Rval = ml_unbox(_Type, SubRval)
        ; Rval = ml_cast(_Type, SubRval)
        ; Rval = ml_unop(_UnaryOp, SubRval)
        ),
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
        Lval = ml_field(_MaybeTag, _PtrRval, _PtrType, _FieldId, _FieldType)
        % Here, "_PtrRval" is a pointer to a cell on the heap, and doesn't need
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
