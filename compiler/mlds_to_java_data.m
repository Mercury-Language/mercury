%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018, 2020 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS lvals, rvals and initializers in Java.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_data.
:- interface.

:- import_module libs.
:- import_module libs.indent.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_java_util.
:- import_module ml_backend.mlds_to_target_util.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred output_lval_for_java(java_out_info::in, mlds_lval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_call_rval_for_java(java_out_info::in, mlds_rval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

:- pred output_bracketed_rval_for_java(java_out_info::in, mlds_rval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

:- pred output_rval_for_java(java_out_info::in, mlds_rval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

:- pred output_boxed_rval_for_java(java_out_info::in, mlds_type::in,
    mlds_rval::in, io.text_output_stream::in, io::di, io::uo) is det.

    % Output an Rval, and if the Rval is an enumeration object,
    % append the string ".MR_value", so we can access its value field.
    %
    % XXX Note that this is necessary in some places, but not in others.
    % For example, it is important to do so for switch statements, as the
    % argument of a switch _must_ be an integer in Java. However, adding
    % the .MR_value to assignments breaks some casting... At some point, we
    % need to go through all the places where output_rval and
    % output_rval_maybe_with_enum are called and make sure the correct one
    % is being used.
    %
    % XXX At the moment, this predicate is called from only two places.
    % If the search mentioned above is ever done (that XXX comment
    % has been there since 2018) but it does not find any more callers,
    % then this predicate should be probably be inlined at its call sites
    % and then deleted. Alternatively, the comment on the definition of
    % the ml_cast mlds_rval suggests another, possibly simpler/better
    % approach to solving this problem.
    %
:- pred output_rval_maybe_with_enum_for_java(java_out_info::in, mlds_rval::in,
    io.text_output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Output " = " followed by the given initializer, if any, followed
    % by the given suffix string and a newline.
    %
    % The initializer is printed using output_initializer_body_for_java with
    % not_at_start_of_line (see below).
    %
:- pred output_initializer_for_java(java_out_info::in,
    io.text_output_stream::in, output_aux::in, indent::in, mlds_type::in,
    mlds_initializer::in, string::in, io::di, io::uo) is det.

    % Output the allocation part of the given initializer on the rest
    % of the current line.
    %
:- pred output_initializer_alloc_only_for_java(java_out_info::in,
    io.text_output_stream::in, mlds_initializer::in, maybe(mlds_type)::in,
    string::in, io::di, io::uo) is det.

    % Output the given initializer. The formatting depends on whether
    % the caller tells us that it has printed something on the current line
    % already (not_at_start_of_line) or not (at_start_of_line).
    %
    % If the initializer is for a struct or an array, we put the initializer
    % on separate lines, each indented by the given indent level, regardless
    % of where we start.
    %
    % If the initializer is for a single object, then we put its initializer
    % immediately after the previous of the current line if there is one
    % (not_at_start_of_line); otherwise (at_start_of_line), we indent it by
    % the specified level.
    %
    % In either case, we end the initializer with the given suffix
    % (which will usually be a semicolon or a comma) and a newline.
    %
:- pred output_initializer_body_for_java(java_out_info::in,
    io.text_output_stream::in, initializer_starts::in, indent::in,
    mlds_initializer::in, maybe(mlds_type)::in, string::in,
    io::di, io::uo) is det.

    % Output the given list of initializers with commas between them,
    % putting each initializer on its own line with the given indent.
    % Put the given suffix after the last initializer.
    %
:- pred output_nonempty_initializer_body_list_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_initializer)::in,
    string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % We need to provide initializers for local variables to avoid problems
    % with Java's rules for definite assignment. This mirrors the default
    % Java initializers for class and instance variables.
    %
:- func get_default_initializer_for_java(mlds_type) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_java_name.
:- import_module ml_backend.mlds_to_java_type.
:- import_module parse_tree.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module int8.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module uint.
:- import_module uint32.

%---------------------------------------------------------------------------%

output_lval_for_java(Info, Lval, Stream, !IO) :-
    (
        Lval = ml_field(_MaybeTag, PtrRval, _PtrType, FieldId, FieldType),
        (
            FieldId = ml_field_offset(OffsetRval),
            ( if
                ( FieldType = mlds_generic_type
                ; FieldType = mercury_nb_type(type_variable(_, _), _)
                )
            then
                true
            else
                % The field type for field(_, _, offset(_), _, _) lvals
                % must be something that maps to MR_Box.
                unexpected($pred, "unexpected field type")
            ),
            % XXX We shouldn't need this cast here, but there are cases where
            % it is needed and the MLDS doesn't seem to generate it.
            io.write_string(Stream, "((java.lang.Object[]) ", !IO),
            output_rval_for_java(Info, PtrRval, Stream, !IO),
            io.write_string(Stream, ")[", !IO),
            output_rval_for_java(Info, OffsetRval, Stream, !IO),
            io.write_string(Stream, "]", !IO)
        ;
            FieldId = ml_field_named(QualFieldVarName, CtorType),
            QualFieldVarName = qual_field_var_name(_, _, FieldVarName),
            ( if FieldVarName = fvn_data_tag then
                % If the field we are trying to access is just a `data_tag'
                % then it is a member of the base class.
                output_bracketed_rval_for_java(Info, PtrRval, Stream, !IO),
                io.write_string(Stream, ".", !IO)
            else if PtrRval = ml_self(_) then
                % Suppress type cast on `this' keyword. This makes a difference
                % when assigning to `final' member variables in constructor
                % functions.
                output_rval_for_java(Info, PtrRval, Stream, !IO),
                io.write_string(Stream, ".", !IO)
            else
                % Otherwise the field we are trying to access may be
                % in a derived class. Objects are manipulated as instances
                % of their base class, so we need to downcast to the derived
                % class to access some fields.
                io.format(Stream, "((%s) ",
                    [s(type_to_string_for_java(Info, CtorType))], !IO),
                output_bracketed_rval_for_java(Info, PtrRval, Stream, !IO),
                io.write_string(Stream, ").", !IO)
            ),
            FieldVarNameStr = field_var_name_to_string_for_java(FieldVarName),
            io.write_string(Stream, FieldVarNameStr, !IO)
        )
    ;
        Lval = ml_mem_ref(Rval, _Type),
        output_bracketed_rval_for_java(Info, Rval, Stream, !IO)
    ;
        Lval = ml_target_global_var_ref(GlobalVarRef),
        GlobalVarRef = env_var_ref(EnvVarName),
        io.format(Stream, "mercury_envvar_%s", [s(EnvVarName)], !IO)
    ;
        Lval = ml_global_var(GlobalVarName, _),
        GlobalVarNameStr = maybe_qualified_global_var_name_to_string_for_java(
            Info, GlobalVarName),
        io.write_string(Stream, GlobalVarNameStr, !IO)
    ;
        Lval = ml_local_var(LocalVarName, _),
        LocalVarNameStr = local_var_name_to_string_for_java(LocalVarName),
        io.write_string(Stream, LocalVarNameStr, !IO)
    ).

%---------------------------------------------------------------------------%

output_call_rval_for_java(Info, Rval, Stream, !IO) :-
    ( if
        Rval = ml_const(Const),
        Const = mlconst_code_addr(CodeAddr)
    then
        mlds_output_call_code_addr_for_java(Stream, CodeAddr, !IO)
    else
        output_bracketed_rval_for_java(Info, Rval, Stream, !IO)
    ).

output_bracketed_rval_for_java(Info, Rval, Stream, !IO) :-
    ( if
        % If it is just a variable name, then we don't need parentheses.
        ( Rval = ml_lval(ml_local_var(_,_))
        ; Rval = ml_lval(ml_global_var(_,_))
        ; Rval = ml_const(mlconst_code_addr(_))
        )
    then
        output_rval_for_java(Info, Rval, Stream, !IO)
    else
        io.write_char(Stream, '(', !IO),
        output_rval_for_java(Info, Rval, Stream, !IO),
        io.write_char(Stream, ')', !IO)
    ).

output_rval_for_java(Info, Rval, Stream, !IO) :-
    (
        Rval = ml_lval(Lval),
        output_lval_for_java(Info, Lval, Stream, !IO)
    ;
        Rval = ml_mkword(_, _),
        unexpected($pred, "tags not supported in Java")
    ;
        Rval = ml_const(Const),
        output_rval_const_for_java(Info, Stream, Const, !IO)
    ;
        Rval = ml_cast(Type, SubRval),
        output_cast_rval_for_java(Info, Type, SubRval, Stream, !IO)
    ;
        Rval = ml_box(Type, SubRval),
        output_boxed_rval_for_java(Info, Type, SubRval, Stream, !IO)
    ;
        Rval = ml_unbox(Type, SubRval),
        output_unboxed_rval_for_java(Info, Type, SubRval, Stream, !IO)
    ;
        Rval = ml_unop(Unop, SubRval),
        output_unop_for_java(Info, Stream, Unop, SubRval, !IO)
    ;
        Rval = ml_binop(BinOp, RvalA, RvalB),
        output_binop_for_java(Info, Stream, BinOp, RvalA, RvalB, !IO)
    ;
        Rval = ml_mem_addr(_Lval),
        unexpected($pred, "mem_addr(_) not supported")
    ;
        Rval = ml_scalar_common(_),
        % This reference is not the same as a mlds_data_addr const.
        unexpected($pred, "ml_scalar_common")
    ;
        Rval = ml_scalar_common_addr(ScalarCommon),
        ScalarCommon = mlds_scalar_common(ModuleName, _Type,
            ml_scalar_common_type_num(TypeNum), RowNum),
        ModuleSymName = mlds_module_name_to_sym_name(ModuleName),
        mangle_sym_name_for_java(ModuleSymName, module_qual, "__",
            MangledModuleName),
        io.format(Stream, "%s.MR_scalar_common_%d[%d]",
            [s(MangledModuleName),i(TypeNum), i(RowNum)], !IO)
    ;
        Rval = ml_vector_common_row_addr(VectorCommon, RowRval),
        VectorCommon = mlds_vector_common(_ModuleName, _Type,
            ml_vector_common_type_num(TypeNum), StartRowNum, _NumRows),
        % XXX Why do we print a "MangledModuleName." prefix for scalar common
        % addresses but not for vector common addresses?
        io.format(Stream, "MR_vector_common_%d[%d + ",
            [i(TypeNum), i(StartRowNum)], !IO),
        output_rval_for_java(Info, RowRval, Stream, !IO),
        io.write_string(Stream, "]", !IO)
    ;
        Rval = ml_self(_),
        io.write_string(Stream, "this", !IO)
    ).

:- pred output_cast_rval_for_java(java_out_info::in, mlds_type::in,
    mlds_rval::in, io.text_output_stream::in, io::di, io::uo) is det.

output_cast_rval_for_java(Info, Type, Rval, Stream, !IO) :-
    % rtti_to_mlds.m generates casts from int to
    % jmercury.runtime.PseudoTypeInfo, but for Java
    % we need to treat these as constructions, not casts.
    % Similarly for conversions from TypeCtorInfo to TypeInfo.
    ( if
        Type = mlds_pseudo_type_info_type,
        Rval = ml_const(mlconst_int(N))
    then
        maybe_output_inline_comment_for_java(Info, Stream, "cast", !IO),
        ( if have_preallocated_pseudo_type_var_for_java(N) then
            io.write_string(Stream, "jmercury.runtime.PseudoTypeInfo.K", !IO),
            io.write_int(Stream, N, !IO)
        else
            io.write_string(Stream,
                "new jmercury.runtime.PseudoTypeInfo(", !IO),
            output_rval_for_java(Info, Rval, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        )
    else if
        ( Type = mercury_nb_type(_, ctor_cat_system(cat_system_type_info))
        ; Type = mlds_type_info_type
        )
    then
        % XXX We really should be able to tell if we are casting a
        % TypeCtorInfo or a TypeInfo. Julien says that's probably going to
        % be rather difficult as the compiler doesn't keep track of where
        % type_ctor_infos are acting as type_infos properly. (zs agrees.)
        maybe_output_inline_comment_for_java(Info, Stream, "cast", !IO),
        io.write_string(Stream,
            "jmercury.runtime.TypeInfo_Struct.maybe_new(", !IO),
        output_rval_for_java(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    else if
        java_builtin_type(Type, "int", _, _)
    then
        io.write_string(Stream, "(int) ", !IO),
        % If Rval is an enum, it is an object with its value in a field,
        % which means that we need to get that field.
        output_rval_maybe_with_enum_for_java(Info, Rval, Stream, !IO)
    else
        io.format(Stream, "(%s) ",
            [s(type_to_string_for_java(Info, Type))], !IO),
        % XXX We don't call output_rval_maybe_with_enum_for_java here.
        % This means that we better not cast enum values to any type
        % other than "int".
        output_rval_for_java(Info, Rval, Stream, !IO)
    ).

:- pred have_preallocated_pseudo_type_var_for_java(int::in) is semidet.

have_preallocated_pseudo_type_var_for_java(N) :-
    % Corresponds to static members in class PseudoTypeInfo.
    N >= 1,
    N =< 5.

output_boxed_rval_for_java(Info, Type, Rval, Stream, !IO) :-
    ( if java_builtin_type(Type, _, JavaBoxedTypeName, _) then
        % valueOf may return cached instances instead of creating new objects.
        io.format(Stream, "%s.valueOf(", [s(JavaBoxedTypeName)], !IO),
        output_rval_for_java(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    else
        io.write_string(Stream, "((java.lang.Object) (", !IO),
        output_rval_for_java(Info, Rval, Stream, !IO),
        io.write_string(Stream, "))", !IO)
    ).

:- pred output_unboxed_rval_for_java(java_out_info::in, mlds_type::in,
    mlds_rval::in, io.text_output_stream::in, io::di, io::uo) is det.

output_unboxed_rval_for_java(Info, Type, Rval, Stream, !IO) :-
    ( if java_builtin_type(Type, _, JavaBoxedTypeName, UnboxMethod) then
        io.format(Stream, "((%s) ", [s(JavaBoxedTypeName)], !IO),
        output_bracketed_rval_for_java(Info, Rval, Stream, !IO),
        io.format(Stream, ").%s()", [s(UnboxMethod)], !IO)
    else
        io.write_string(Stream, "((", !IO),
        output_type_for_java(Info, Stream, Type, !IO),
        io.write_string(Stream, ") ", !IO),
        output_rval_for_java(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ).

:- pred output_unop_for_java(java_out_info::in, io.text_output_stream::in,
    builtin_ops.unary_op::in, mlds_rval::in, io::di, io::uo) is det.

output_unop_for_java(Info, Stream, UnaryOp, Rval, !IO) :-
    % For the Java back-end, there are no tags, so all the tagging operators
    % are no-ops, except for `tag', which always returns zero (a tag of zero
    % means there is no tag).
    (
        UnaryOp = tag,
        io.write_string(Stream, "/* tag */  0", !IO)
    ;
        ( UnaryOp = strip_tag, UnaryOpStr = "/* strip_tag */ "
        ; UnaryOp = mkbody,    UnaryOpStr = "/* mkbody */ "
        ; UnaryOp = unmkbody,  UnaryOpStr = "/* unmkbody */ "
        ; UnaryOp = logical_not, UnaryOpStr = "!"
        ; UnaryOp = hash_string,  UnaryOpStr = "mercury.String.hash_1_f_0"
        ; UnaryOp = hash_string2, UnaryOpStr = "mercury.String.hash2_1_f_0"
        ; UnaryOp = hash_string3, UnaryOpStr = "mercury.String.hash3_1_f_0"
        ; UnaryOp = hash_string4, UnaryOpStr = "mercury.String.hash4_1_f_0"
        ; UnaryOp = hash_string5, UnaryOpStr = "mercury.String.hash5_1_f_0"
        ; UnaryOp = hash_string6, UnaryOpStr = "mercury.String.hash6_1_f_0"
        ),
        io.format(Stream, "%s(", [s(UnaryOpStr)], !IO),
        output_rval_for_java(Info, Rval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        UnaryOp = bitwise_complement(IntType),
        (
            ( IntType = int_type_int
            ; IntType = int_type_uint
            ; IntType = int_type_int32
            ; IntType = int_type_uint32
            ; IntType = int_type_int64
            ; IntType = int_type_uint64
            ),
            io.write_string(Stream, "~(", !IO),
            output_rval_for_java(Info, Rval, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            ( IntType = int_type_int8
            ; IntType = int_type_uint8
            ),
            io.write_string(Stream, "(byte) (~(", !IO),
            output_rval_for_java(Info, Rval, Stream, !IO),
            io.write_string(Stream, "))", !IO)
    ;
            ( IntType = int_type_int16
            ; IntType = int_type_uint16
            ),
            io.write_string(Stream, "(short) (~(", !IO),
            output_rval_for_java(Info, Rval, Stream, !IO),
            io.write_string(Stream, "))", !IO)
        )
    ;
        ( UnaryOp = dword_float_get_word0
        ; UnaryOp = dword_float_get_word1
        ; UnaryOp = dword_int64_get_word0
        ; UnaryOp = dword_int64_get_word1
        ; UnaryOp = dword_uint64_get_word0
        ; UnaryOp = dword_uint64_get_word1
        ),
        unexpected($pred, "invalid unary operator")
    ).

:- pred output_binop_for_java(java_out_info::in, io.text_output_stream::in,
    binary_op::in, mlds_rval::in, mlds_rval::in, io::di, io::uo) is det.

output_binop_for_java(Info, Stream, Op, X, Y, !IO) :-
    (
        Op = array_index(_Type),
        output_bracketed_rval_for_java(Info, X, Stream, !IO),
        io.write_string(Stream, "[", !IO),
        output_rval_for_java(Info, Y, Stream, !IO),
        io.write_string(Stream, "]", !IO)
    ;
        Op = str_eq,
        output_rval_for_java(Info, X, Stream, !IO),
        io.write_string(Stream, ".equals(", !IO),
        output_rval_for_java(Info, Y, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        ( Op = str_ne, OpStr = "!="
        ; Op = str_lt, OpStr = "<"
        ; Op = str_gt, OpStr = ">"
        ; Op = str_le, OpStr = "<="
        ; Op = str_ge, OpStr = ">="
        ),
        io.write_string(Stream, "(", !IO),
        output_rval_for_java(Info, X, Stream, !IO),
        io.write_string(Stream, ".compareTo(", !IO),
        output_rval_for_java(Info, Y, Stream, !IO),
        io.format(Stream, ") %s 0)", [s(OpStr)], !IO)
    ;
        Op = str_cmp,
        io.write_string(Stream, "(", !IO),
        output_rval_for_java(Info, X, Stream, !IO),
        io.write_string(Stream, ".compareTo(", !IO),
        output_rval_for_java(Info, Y, Stream, !IO),
        io.write_string(Stream, ")) ", !IO)
    ;
        Op = pointer_equal_conservative,
        io.write_string(Stream, "(", !IO),
        output_rval_for_java(Info, X, Stream, !IO),
        io.write_string(Stream, " == ", !IO),
        output_rval_for_java(Info, Y, Stream, !IO),
        io.write_string(Stream, ") ", !IO)
    ;
        ( Op = int_add(_)
        ; Op = int_sub(_)
        ; Op = int_mul(_)
        ; Op = int_div(_)
        ; Op = int_mod(_)
        ; Op = unchecked_left_shift(_, _)
        ; Op = unchecked_right_shift(_, _)
        ; Op = bitwise_and(_)
        ; Op = bitwise_or(_)
        ; Op = bitwise_xor(_)
        ; Op = int_lt(_)
        ; Op = int_gt(_)
        ; Op = int_le(_)
        ; Op = int_ge(_)
        ),
        % Handle these in a separate switch to reduce gcc memory requirements,
        % particularly when building in deep profiling grades.
        output_int_binop_for_java(Info, Stream, Op, X, Y, !IO)
    ;
        ( Op = unsigned_lt, OpStr = "<"
        ; Op = unsigned_le, OpStr = "<="
        ),
        ( if rval_is_enum_object(X) then
            % The bit masking won't be needed in the vast majority of cases,
            % but I (zs) believe that it *could* be possible for a
            % foreign_enum pragma to assign a negative value to
            % a functor in an enum type.
            io.write_string(Stream, "((", !IO),
            output_rval_for_java(Info, X, Stream, !IO),
            io.format(Stream, ".MR_value & 0xffffffffL) %s (",
                [s(OpStr)], !IO),
            output_rval_for_java(Info, Y, Stream, !IO),
            io.write_string(Stream, ".MR_value) & 0xffffffffL)", !IO)
        else
            io.write_string(Stream, "((", !IO),
            output_rval_for_java(Info, X, Stream, !IO),
            io.format(Stream, " & 0xffffffffL) %s (", [s(OpStr)], !IO),
            output_rval_for_java(Info, Y, Stream, !IO),
            io.write_string(Stream, " & 0xffffffffL))", !IO)
        )
    ;
        ( Op = logical_and,     OpStr = "&&"
        ; Op = logical_or,      OpStr = "||"
        ; Op = eq(_),           OpStr = "=="
        ; Op = ne(_),           OpStr = "!="
        ; Op = float_add,       OpStr = "+"
        ; Op = float_sub,       OpStr = "-"
        ; Op = float_mul,       OpStr = "*"
        ; Op = float_div,       OpStr = "/"
        ; Op = float_eq,        OpStr = "=="
        ; Op = float_ne,        OpStr = "!="
        ; Op = float_lt,        OpStr = "<"
        ; Op = float_gt,        OpStr = ">"
        ; Op = float_le,        OpStr = "<="
        ; Op = float_ge,        OpStr = ">="
        ),
        output_basic_binop_maybe_with_enum_for_java(Info, Stream,
            OpStr, X, Y, !IO)
    ;
        ( Op = body
        ; Op = string_unsafe_index_code_unit
        ; Op = offset_str_eq(_)
        ; Op = float_from_dword
        ; Op = int64_from_dword
        ; Op = uint64_from_dword
        ; Op = compound_eq
        ; Op = compound_lt
        ),
        unexpected($pred, "invalid binary operator")
    ).

:- pred output_int_binop_for_java(java_out_info::in, io.text_output_stream::in,
    binary_op::in(int_binary_op), mlds_rval::in, mlds_rval::in, io::di, io::uo)
    is det.
:- pragma no_inline(pred(output_int_binop_for_java/7)).

output_int_binop_for_java(Info, Stream, Op, X, Y, !IO) :-
    (
        ( Op = int_add(Type),   OpStr = "+"
        ; Op = int_sub(Type),   OpStr = "-"
        ; Op = int_mul(Type),   OpStr = "*"
        ),
        (
            ( Type = int_type_int
            ; Type = int_type_int32
            ; Type = int_type_int64
            ; Type = int_type_uint
            ; Type = int_type_uint32
            ; Type = int_type_uint64
            ),
            output_basic_binop_maybe_with_enum_for_java(Info, Stream,
                OpStr, X, Y, !IO)
        ;
            ( Type = int_type_int8,     Cast = "(byte) "
            ; Type = int_type_int16,    Cast = "(short) "
            ; Type = int_type_uint8,    Cast = "(byte) "
            ; Type = int_type_uint16,   Cast = "(short) "
            ),
            io.write_string(Stream, Cast, !IO),
            % XXX Document why we aren't calling
            % output_basic_binop_maybe_with_enum_for_java here.
            output_basic_binop_for_java(Info, Stream,
                OpStr, X, Y, !IO)
        )
    ;
        ( Op = int_div(Type),   OpStr = "/"
        ; Op = int_mod(Type),   OpStr = "%"
        ),
        (
            ( Type = int_type_int
            ; Type = int_type_int32
            ; Type = int_type_int64
            ),
            output_basic_binop_maybe_with_enum_for_java(Info, Stream,
                OpStr, X, Y, !IO)
        ;
            ( Type = int_type_int8,     Cast = "(byte) "
            ; Type = int_type_int16,    Cast = "(short) "
            ),
            io.write_string(Stream, Cast, !IO),
            output_basic_binop_for_java(Info, Stream,
                OpStr, X, Y, !IO)
        ;
            ( Type = int_type_uint8,    Cast = "(byte)",   Mask = "0xff"
            ; Type = int_type_uint16,   Cast = "(short)",  Mask = "0xffff"
            ; Type = int_type_uint32,   Cast = "(int)",    Mask = "0xffffffffL"
            ; Type = int_type_uint,     Cast = "(int)",    Mask = "0xffffffffL"
            ),
            io.format(Stream, "(%s ", [s(Cast)], !IO),
            output_basic_binop_with_mask_for_java(Info, Stream,
                OpStr, Mask, X, Y, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            Type = int_type_uint64,
            % We could compute FuncName along with OpStr above,
            % but int64 operands are rare enough that it is better
            % not to burden the non-int64 code path with recording FuncName.
            ( Op = int_div(_), FuncName = "java.lang.Long.divideUnsigned"
            ; Op = int_mod(_), FuncName = "java.lang.Long.remainderUnsigned"
            ),
            output_binop_func_call_for_java(Info, Stream,
                FuncName, X, Y, !IO)
        )
    ;
        ( Op = int_lt(Type),    OpStr = "<"
        ; Op = int_gt(Type),    OpStr = ">"
        ; Op = int_le(Type),    OpStr = "<="
        ; Op = int_ge(Type),    OpStr = ">="
        ),
        (
            ( Type = int_type_int
            ; Type = int_type_int8
            ; Type = int_type_int16
            ; Type = int_type_int32
            ; Type = int_type_int64
            ),
            output_basic_binop_maybe_with_enum_for_java(Info, Stream,
                OpStr, X, Y, !IO)
        ;
            ( Type = int_type_uint8,    Mask = "0xff"
            ; Type = int_type_uint16,   Mask = "0xffff"
            ; Type = int_type_uint32,   Mask = "0xffffffffL"
            ; Type = int_type_uint,     Mask = "0xffffffffL"
            ),
            output_basic_binop_with_mask_for_java(Info, Stream,
                OpStr, Mask, X, Y, !IO)
        ;
            Type = int_type_uint64,
            io.write_string(Stream, "(", !IO),
            output_binop_func_call_for_java(Info, Stream,
                "java.lang.Long.compareUnsigned", X, Y, !IO),
            io.format(Stream, " %s 0)", [s(OpStr)], !IO)
        )
    ;
        ( Op = bitwise_and(Type),   OpStr = "&"
        ; Op = bitwise_or(Type),    OpStr = "|"
        ; Op = bitwise_xor(Type),   OpStr = "^"
        ),
        (
            ( Type = int_type_int
            ; Type = int_type_int32
            ; Type = int_type_int64
            ; Type = int_type_uint
            ; Type = int_type_uint32
            ; Type = int_type_uint64
            ),
            output_basic_binop_maybe_with_enum_for_java(Info, Stream,
                OpStr, X, Y, !IO)
        ;
            ( Type = int_type_int8,     Cast = "(byte) "
            ; Type = int_type_int16,    Cast = "(short) "
            ; Type = int_type_uint8,    Cast = "(byte) "
            ; Type = int_type_uint16,   Cast = "(short) "
            ),
            io.write_string(Stream, Cast, !IO),
            output_basic_binop_for_java(Info, Stream,
                OpStr, X, Y, !IO)
        )
    ;
        (
            Op = unchecked_left_shift(Type, _ShiftByType),
            OpStr = "<<"
        ;
            Op = unchecked_right_shift(Type, _ShiftByType),
            (
                ( Type = int_type_int
                ; Type = int_type_int8
                ; Type = int_type_int16
                ; Type = int_type_int32
                ; Type = int_type_int64
                ),
                OpStr = ">>"
            ;
                ( Type = int_type_uint
                ; Type = int_type_uint8
                ; Type = int_type_uint16
                ; Type = int_type_uint32
                ; Type = int_type_uint64
                ),
                OpStr = ">>>"
            )
        ),
        % We ignore the distinction between shift_by_int and shift_by_uint,
        % because when targeting Java, we represent Mercury uints as
        % Java ints anyway.
        (
            ( Type = int_type_int
            ; Type = int_type_int32
            ; Type = int_type_int64
            ; Type = int_type_uint
            ; Type = int_type_uint32
            ; Type = int_type_uint64
            ),
            output_basic_binop_maybe_with_enum_for_java(Info, Stream,
                OpStr, X, Y, !IO)
        ;
            ( Type = int_type_int8,     Cast = "(byte) ",   Mask = ""
            ; Type = int_type_int16,    Cast = "(short) ",  Mask = ""
            ; Type = int_type_uint8,    Cast = "(byte) ",   Mask = "0xff"
            ; Type = int_type_uint16,   Cast = "(short) ",  Mask = "0xffff"
            ),
            io.write_string(Stream, Cast, !IO),
            % This special case is needed because we represent
            % Mercury unsigned integers using signed Java integers.
            % When operating on a sub-word-sized integer we want to treat
            % as unsigned, we need to tell Java not to sign extend it
            % if the sign extension could interfere with the operation.
            % For left shifts, sign extension is irrelevant, since
            % the shifted-in bits come from the bottom of the word.
            % For right shifts, sign extension is relevant, since
            % the shifted-in bits come from the top of the word.
            ( if
                Op = unchecked_right_shift(_, _),
                Mask \= ""
            then
                % Unlike output_basic_binop_with_mask_for_java,
                % this code applies the mask *only* to X, not to Y.
                % (As the shift amount, Y should already be in the range
                % 0 .. 63.)
                io.write_string(Stream, "(((", !IO),
                output_rval_for_java(Info, X, Stream, !IO),
                io.format(Stream, ") & %s) %s ", [s(Mask), s(OpStr)], !IO),
                output_rval_for_java(Info, Y, Stream, !IO),
                io.write_string(Stream, ")", !IO)
            else
                output_basic_binop_for_java(Info, Stream,
                    OpStr, X, Y, !IO)
            )
        )
    ).

:- pred output_basic_binop_maybe_with_enum_for_java(java_out_info::in,
    io.text_output_stream::in, string::in, mlds_rval::in, mlds_rval::in,
    io::di, io::uo) is det.
:- pragma no_inline(pred(output_basic_binop_maybe_with_enum_for_java/7)).

output_basic_binop_maybe_with_enum_for_java(Info, Stream, OpStr, X, Y, !IO) :-
    ( if rval_is_enum_object(X) then
        io.write_string(Stream, "(", !IO),
        output_rval_for_java(Info, X, Stream, !IO),
        io.format(Stream, ".MR_value %s ", [s(OpStr)], !IO),
        output_rval_for_java(Info, Y, Stream, !IO),
        io.write_string(Stream, ".MR_value)", !IO)
    else
        output_basic_binop_for_java(Info, Stream, OpStr, X, Y, !IO)
    ).

:- pred output_basic_binop_for_java(java_out_info::in,
    io.text_output_stream::in, string::in, mlds_rval::in, mlds_rval::in,
    io::di, io::uo) is det.
:- pragma no_inline(pred(output_basic_binop_for_java/7)).

output_basic_binop_for_java(Info, Stream, OpStr, X, Y, !IO) :-
    io.write_string(Stream, "(", !IO),
    output_rval_for_java(Info, X, Stream, !IO),
    io.format(Stream, " %s ", [s(OpStr)], !IO),
    output_rval_for_java(Info, Y, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred output_basic_binop_with_mask_for_java(java_out_info::in,
    io.text_output_stream::in, string::in, string::in,
    mlds_rval::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma no_inline(pred(output_basic_binop_with_mask_for_java/8)).

output_basic_binop_with_mask_for_java(Info, Stream, OpStr, Mask, X, Y, !IO) :-
    io.write_string(Stream, "(((", !IO),
    output_rval_for_java(Info, X, Stream, !IO),
    io.format(Stream, ") & %s) %s ((", [s(Mask), s(OpStr)], !IO),
    output_rval_for_java(Info, Y, Stream, !IO),
    io.format(Stream, ") & %s))", [s(Mask)], !IO).

:- pred output_binop_func_call_for_java(java_out_info::in,
    io.text_output_stream::in, string::in, mlds_rval::in, mlds_rval::in,
    io::di, io::uo) is det.
:- pragma no_inline(pred(output_binop_func_call_for_java/7)).

output_binop_func_call_for_java(Info, Stream, FuncName, X, Y, !IO) :-
    io.format(Stream, "%s(", [s(FuncName)], !IO),
    output_rval_for_java(Info, X, Stream, !IO),
    io.write_string(Stream, ", ", !IO),
    output_rval_for_java(Info, Y, Stream, !IO),
    io.write_string(Stream, ")", !IO).

output_rval_maybe_with_enum_for_java(Info, Rval, Stream, !IO) :-
    output_rval_for_java(Info, Rval, Stream, !IO),
    ( if rval_is_enum_object(Rval) then
        io.write_string(Stream, ".MR_value", !IO)
    else
        true
    ).

:- pred output_rval_const_for_java(java_out_info::in,
    io.text_output_stream::in, mlds_rval_const::in, io::di, io::uo) is det.

output_rval_const_for_java(Info, Stream, Const, !IO) :-
    (
        Const = mlconst_true,
        io.write_string(Stream, "true", !IO)
    ;
        Const = mlconst_false,
        io.write_string(Stream, "false", !IO)
    ;
        Const = mlconst_int(N),
        output_int_const_for_java(Stream, N, !IO)
    ;
        Const = mlconst_uint(U),
        % Java does not have unsigned integer literals.
        % XXX perhaps we should output this in hexadecimal?
        output_int_const_for_java(Stream, uint.cast_to_int(U), !IO)
    ;
        Const = mlconst_int8(I8),
        io.format(Stream, "(byte) %d", [i8(I8)], !IO)
    ;
        Const = mlconst_uint8(U8),
        io.format(Stream, "(byte) %d", [i8(int8.cast_from_uint8(U8))], !IO)
    ;
        Const = mlconst_int16(I16),
        io.format(Stream, "(short) %d", [i16(I16)], !IO)
    ;
        Const = mlconst_uint16(U16),
        io.format(Stream, "(short) %d", [i16(int16.cast_from_uint16(U16))],
            !IO)
    ;
        Const = mlconst_int32(I32),
        io.write_int32(Stream, I32, !IO)
    ;
        Const = mlconst_uint32(U32),
        io.write_int32(Stream, int32.cast_from_uint32(U32), !IO)
    ;
        Const = mlconst_int64(I64),
        io.format(Stream, "%dL", [i64(I64)], !IO)
    ;
        Const = mlconst_uint64(U64),
        io.format(Stream, "%dL", [i64(int64.cast_from_uint64(U64))], !IO)
    ;
        Const = mlconst_char(N),
        io.write_string(Stream, "(", !IO),
        output_int_const_for_java(Stream, N, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Const = mlconst_enum(N, EnumType),
        io.format(Stream, "%s.K",
            [s(type_to_string_for_java(Info, EnumType))], !IO),
        output_int_const_for_java(Stream, N, !IO)
    ;
        Const = mlconst_foreign(Lang, Value, _Type),
        expect(unify(Lang, lang_java), $pred, "language other than Java."),
        % XXX Should we parenthesize this?
        io.write_string(Stream, Value, !IO)
    ;
        Const = mlconst_float(FloatVal),
        c_util.output_float_literal(Stream, FloatVal, !IO)
    ;
        Const = mlconst_string(String),
        output_quoted_string_java(Stream, String, !IO)
    ;
        Const = mlconst_multi_string(String),
        output_quoted_multi_string_java(Stream, String, !IO)
    ;
        Const = mlconst_named_const(TargetPrefixes, NamedConst),
        io.write_string(Stream, TargetPrefixes ^ java_prefix, !IO),
        io.write_string(Stream, NamedConst, !IO)
    ;
        Const = mlconst_code_addr(CodeAddr),
        mlds_output_wrapper_code_addr_for_java(Info, Stream, CodeAddr, !IO)
    ;
        Const = mlconst_data_addr_local_var(LocalVarName),
        LocalVarNameStr = local_var_name_to_string_for_java(LocalVarName),
        io.write_string(Stream, LocalVarNameStr, !IO)
    ;
        Const = mlconst_data_addr_global_var(ModuleName, GlobalVarName),
        SymName = mlds_module_name_to_sym_name(ModuleName),
        mangle_sym_name_for_java(SymName, module_qual, "__", ModuleNameStr),
        GlobalVarNameStr = global_var_name_to_string_for_java(GlobalVarName),
        io.format(Stream, "%s.%s",
            [s(ModuleNameStr), s(GlobalVarNameStr)], !IO)
    ;
        Const = mlconst_data_addr_rtti(ModuleName, RttiId),
        SymName = mlds_module_name_to_sym_name(ModuleName),
        mangle_sym_name_for_java(SymName, module_qual, "__", ModuleNameStr),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        io.format(Stream, "%s.%s", [s(ModuleNameStr), s(RttiAddrName)], !IO)
    ;
        Const = mlconst_data_addr_tabling(QualProcLabel, TablingId),
        QualProcLabel = qual_proc_label(ModuleName, ProcLabel),
        SymName = mlds_module_name_to_sym_name(ModuleName),
        mangle_sym_name_for_java(SymName, module_qual, "__", ModuleNameStr),
        TablingIdStr = tabling_info_id_str(TablingId),
        TablingProcLabel = mlds_std_tabling_proc_label(ProcLabel),
        TablingProcLabelStr = proc_label_to_string_for_java(TablingProcLabel),
        io.format(Stream, "%s.%s_%s",
            [s(ModuleNameStr), s(TablingIdStr), s(TablingProcLabelStr)], !IO)
    ;
        Const = mlconst_null(Type),
        Initializer = get_default_initializer_for_java(Type),
        io.write_string(Stream, Initializer, !IO)
    ).

:- pred output_int_const_for_java(io.text_output_stream::in, int::in,
    io::di, io::uo) is det.

output_int_const_for_java(Stream, N, !IO) :-
    % The Mercury compiler could be using 64-bit integers but Java has 32-bit
    % ints. A literal 0xffffffff in a source file would be interpreted by a
    % 64-bit Mercury compiler as 4294967295. If it is written out in decimal,
    % a Java compiler would rightly complain because the integer is too large
    % to fit in a 32-bit int. However, it won't complain if the literal is
    % expressed in hexadecimal (nor as the negative decimal -1).
    ( if
        N > 0,
        not int32.from_int(N, _I32),
        uint32.from_int(N, U32)
    then
        % The bit pattern fits in 32 bits, but is too large to write as a
        % positive decimal. This branch is unreachable on a 32-bit compiler.
        N32 = uint32.cast_to_int(U32),
        io.format(Stream, "0x%x", [i(N32)], !IO)
    else
        io.write_int(Stream, N, !IO)
    ).

    % Take the address of the wrapper for the given function (method).
    %
:- pred mlds_output_wrapper_code_addr_for_java(java_out_info::in,
    io.text_output_stream::in, mlds_code_addr::in, io::di, io::uo) is det.

mlds_output_wrapper_code_addr_for_java(Info, Stream, CodeAddr, !IO) :-
    AddrOfMap = Info ^ joi_addrof_map,
    map.lookup(AddrOfMap, CodeAddr, CodeAddrWrapper),
    CodeAddrWrapper = code_addr_wrapper(ClassName, MaybePtrNum),
    (
        MaybePtrNum = yes(PtrNum),
        io.format(Stream, "new %s_0(%d)", [s(ClassName), i(PtrNum)], !IO)
    ;
        MaybePtrNum = no,
        io.format(Stream, "new %s_0()", [s(ClassName)], !IO)
    ).

:- pred mlds_output_call_code_addr_for_java(io.text_output_stream::in,
    mlds_code_addr::in, io::di, io::uo) is det.

mlds_output_call_code_addr_for_java(Stream, CodeAddr, !IO) :-
    CodeAddr = mlds_code_addr(QualFuncLabel, _Signature),
    QualFuncLabel = qual_func_label(ModuleName, FuncLabel),
    FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
    Qualifier = qualifier_to_string_for_java(ModuleName, module_qual),
    ProcLabelStr = proc_label_to_string_for_java(ProcLabel),
    MaybeAuxSuffix = mlds_maybe_aux_func_id_to_suffix(MaybeAux),
    io.format(Stream, "%s.%s%s",
        [s(Qualifier), s(ProcLabelStr), s(MaybeAuxSuffix)], !IO).

%---------------------------------------------------------------------------%

    % Succeeds iff the Rval represents an enumeration object in the Java
    % backend. We need to check both Rvals that are variables and Rvals
    % that are casts. We need to know this in order to append the field name
    % to the object so we can access the value of the enumeration object.
    % XXX The code below does NOT check for casts.
    %
:- pred rval_is_enum_object(mlds_rval::in) is semidet.

rval_is_enum_object(Rval) :-
    Rval = ml_lval(Lval),
    (
        Lval = ml_local_var(_, Type)
    ;
        Lval = ml_global_var(_, Type)
    ;
        Lval = ml_field(_, _, _, _, Type)
    ),
    type_is_enum(Type).

    % Succeeds iff this type is a enumeration.
    %
:- pred type_is_enum(mlds_type::in) is semidet.

type_is_enum(Type) :-
    Type = mercury_nb_type(_, CtorCat),
    CtorCat = ctor_cat_enum(_).

%---------------------------------------------------------------------------%

output_initializer_for_java(Info, Stream, OutputAux, Indent, Type,
        Initializer, Suffix, !IO) :-
    (
        ( Initializer = init_obj(_)
        ; Initializer = init_struct(_, _)
        ; Initializer = init_array(_)
        ),
        io.write_string(Stream, " = ", !IO),
        % Due to cyclic references, we need to separate the allocation and
        % initialisation steps of RTTI structures. If OutputAux is alloc_only,
        % then we output an initializer to allocate a structure *without*
        % filling in the fields.
        (
            ( OutputAux = oa_none
            ; OutputAux = oa_cname(_, _)
            ; OutputAux = oa_force_init
            ),
            output_initializer_body_for_java(Info, Stream,
                not_at_start_of_line, Indent + 1, Initializer, yes(Type),
                Suffix, !IO)
        ;
            OutputAux = oa_alloc_only,
            output_initializer_alloc_only_for_java(Info, Stream, Initializer,
                yes(Type), Suffix, !IO)
        )
    ;
        Initializer = no_initializer,
        (
            OutputAux = oa_force_init,
            % Local variables need to be initialised to avoid warnings.
            InitForType = get_default_initializer_for_java(Type),
            io.format(Stream, " = %s", [s(InitForType)], !IO)
        ;
            ( OutputAux = oa_none
            ; OutputAux = oa_cname(_, _)
            ; OutputAux = oa_alloc_only
            )
        ),
        io.format(Stream, "%s\n", [s(Suffix)], !IO)
    ).

%---------------------%

output_initializer_alloc_only_for_java(Info, Stream, Initializer, MaybeType,
        Suffix, !IO) :-
    (
        Initializer = no_initializer,
        unexpected($pred, "no_initializer")
    ;
        Initializer = init_obj(_),
        unexpected($pred, "init_obj")
    ;
        Initializer = init_struct(StructType, FieldInits),
        io.write_string(Stream, "new ", !IO),
        ( if
            StructType = mercury_nb_type(_, CtorCat),
            type_category_is_array(CtorCat) = is_array
        then
            Size = list.length(FieldInits),
            io.format(Stream, "java.lang.Object[%d]%s\n",
                [i(Size), s(Suffix)], !IO)
        else
            io.format(Stream, "%s()%s\n",
                [s(type_to_string_for_java(Info, StructType)), s(Suffix)],
                !IO)
        )
    ;
        Initializer = init_array(ElementInits),
        Size = list.length(ElementInits),
        io.write_string(Stream, "new ", !IO),
        (
            MaybeType = yes(Type),
            type_to_string_and_dims_for_java(Info, Type,
                BaseTypeName, ArrayDims0),
            make_last_dimension_known_size(ArrayDims0, Size, ArrayDims),
            DimsStr = array_dimensions_to_string(ArrayDims),
            io.format(Stream, "%s%s%s\n",
                [s(BaseTypeName), s(DimsStr), s(Suffix)], !IO)
        ;
            MaybeType = no,
            % XXX We need to know the type here.
            io.format(Stream, "/* XXX init_array */ Object[%d]%s\n",
                [i(Size), s(Suffix)], !IO)
        )
    ).

%---------------------%

output_initializer_body_for_java(Info, Stream, InitStart, Indent, Initializer,
        MaybeType, Suffix, !IO) :-
    (
        Initializer = no_initializer,
        unexpected($pred, "no_initializer")
    ;
        Initializer = init_obj(Rval),
        (
            InitStart = not_at_start_of_line
        ;
            InitStart = at_start_of_line,
            write_indent2(Stream, Indent, !IO)
        ),
        output_rval_for_java(Info, Rval, Stream, !IO),
        io.format(Stream, "%s\n", [s(Suffix)], !IO)
    ;
        Initializer = init_struct(StructType, FieldInits),
        IndentStr = indent2_string(Indent),
        (
            InitStart = not_at_start_of_line,
            io.nl(Stream, !IO)
        ;
            InitStart = at_start_of_line
        ),
        io.format(Stream, "%snew ", [s(IndentStr)], !IO),
        output_type_for_java(Info, Stream, StructType, ArrayDims, !IO),
        init_arg_wrappers_cs_java(ArrayDims, Start, End),
        (
            FieldInits = [],
            io.format(Stream, "%s%s%s\n", [s(Start), s(End), s(Suffix)], !IO)
        ;
            FieldInits = [HeadFieldInit | TailFieldInits],
            io.format(Stream, "%s\n", [s(Start)], !IO),
            output_initializer_body_list_for_java(Info, Stream, Indent + 1,
                HeadFieldInit, TailFieldInits, "", !IO),
            io.format(Stream, "%s%s%s\n",
                [s(IndentStr), s(End), s(Suffix)], !IO)
        )
    ;
        Initializer = init_array(ElementInits),
        (
            InitStart = not_at_start_of_line,
            io.nl(Stream, !IO)
        ;
            InitStart = at_start_of_line
        ),
        IndentStr = indent2_string(Indent),
        io.format(Stream, "%snew ", [s(IndentStr)], !IO),
        (
            MaybeType = yes(Type),
            output_type_for_java(Info, Stream, Type, !IO)
        ;
            MaybeType = no,
            % XXX We need to know the type here.
            io.write_string(Stream, "/* XXX init_array */ Object[]", !IO)
        ),
        (
            ElementInits = [],
            io.format(Stream, " {}%s\n", [s(Suffix)], !IO)
        ;
            ElementInits = [HeadElementInit | TailElementInits],
            io.write_string(Stream, " {\n", !IO),
            output_initializer_body_list_for_java(Info, Stream, Indent + 1,
                HeadElementInit, TailElementInits, "", !IO),
            io.format(Stream, "%s}%s\n", [s(IndentStr), s(Suffix)], !IO)
        )
    ).

%---------------------%

output_nonempty_initializer_body_list_for_java(Info, Stream, Indent, Inits,
        Suffix, !IO) :-
    list.det_head_tail(Inits, HeadInit, TailInits),
    output_initializer_body_list_for_java(Info, Stream, Indent,
        HeadInit, TailInits, Suffix, !IO).

:- pred output_initializer_body_list_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, mlds_initializer::in,
    list(mlds_initializer)::in, string::in, io::di, io::uo) is det.

output_initializer_body_list_for_java(Info, Stream,
        Indent, HeadInit, TailInits, Suffix, !IO) :-
    (
        TailInits = [],
        output_initializer_body_for_java(Info, Stream, at_start_of_line,
            Indent, HeadInit, no, Suffix, !IO)
    ;
        TailInits = [HeadTailInit | TailTailInits],
        output_initializer_body_for_java(Info, Stream, at_start_of_line,
            Indent, HeadInit, no, ",", !IO),
        output_initializer_body_list_for_java(Info, Stream,
            Indent, HeadTailInit, TailTailInits, Suffix, !IO)
    ).

%---------------------------------------------------------------------------%

get_default_initializer_for_java(Type) = Initializer :-
    (
        Type = mercury_nb_type(_, CtorCat),
        (
            CtorCat = ctor_cat_builtin(_),
            unexpected($pred, "mercury_nb_type but ctor_cat_builtin")
        ;
            ( CtorCat = ctor_cat_system(_)
            ; CtorCat = ctor_cat_higher_order
            ; CtorCat = ctor_cat_tuple
            ; CtorCat = ctor_cat_enum(_)
            ; CtorCat = ctor_cat_builtin_dummy
            ; CtorCat = ctor_cat_variable
            ; CtorCat = ctor_cat_void
            ; CtorCat = ctor_cat_user(_)
            ),
            Initializer = "null"
        )
    ;
        ( Type = mlds_builtin_type_int(_)
        ; Type = mlds_builtin_type_float
        ),
        Initializer = "0"
    ;
        Type = mlds_builtin_type_string,
        Initializer = "null"
    ;
        Type = mlds_builtin_type_char,
        Initializer = "'\\u0000'"
    ;
        Type = mlds_native_bool_type,
        Initializer = "false"
    ;
        Type = mlds_foreign_type(ForeignLangType),
        ( if
            java_primitive_foreign_language_type(ForeignLangType, _, _,
                _, Initializer0)
        then
            Initializer = Initializer0
        else
            Initializer = "null"
        )
    ;
        ( Type = mlds_mercury_array_type(_)
        ; Type = mlds_cont_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_class_type(_)
        ; Type = mlds_enum_class_type(_)
        ; Type = mlds_env_type(_)
        ; Type = mlds_struct_type(_)
        ; Type = mlds_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
        ; Type = mlds_ptr_type(_)
        ; Type = mlds_func_type(_)
        ; Type = mlds_generic_type
        ; Type = mlds_generic_env_ptr_type
        ; Type = mlds_type_info_type
        ; Type = mlds_pseudo_type_info_type
        ; Type = mlds_rtti_type(_)
        ; Type = mlds_tabling_type(_)
        ),
        Initializer = "null"
    ;
        Type = mlds_unknown_type,
        unexpected($pred, "variable has unknown_type")
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_data.
%---------------------------------------------------------------------------%
