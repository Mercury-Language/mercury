%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2012 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_rep_tables.m.
% Author: zs.
%
% This module contains predicates to build the tables included in program
% representations for debugging and/or deep profiling (mostly the latter).
%
%---------------------------------------------------------------------------%

:- module ll_backend.prog_rep_tables.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- type string_table_info.

:- func init_string_table_info = string_table_info.

:- pred lookup_string_in_table(string::in, int::out,
    string_table_info::in, string_table_info::out) is det.

:- pred get_string_table_contents(string_table_info::in,
    list(string)::out, int::out) is det.

%---------------------------------------------------------------------------%

:- type type_table_info.

:- func init_type_table_info = type_table_info.

:- pred lookup_type_in_table(mer_type::in, int::out,
    string_table_info::in, string_table_info::out,
    type_table_info::in, type_table_info::out) is det.

:- pred get_type_table_contents(type_table_info::in, int::out,
    list(int)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.rtti_access.
:- import_module mdbcomp.sym_name.

:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

:- type string_table_info
    --->    string_table_info(
                % Maps strings to their offsets.
                map(string, int),

                % The list of strings so far, in reverse order.
                list(string),

                % The next available offset.
                int
            ).

init_string_table_info = !:StringTable :-
    map.init(StringMap0),
    !:StringTable = string_table_info(StringMap0, [], 0),
    lookup_string_in_table("", _, !StringTable),
    lookup_string_in_table("<too many variables>", _, !StringTable).

lookup_string_in_table(String, StringCode, !StringTable) :-
    % The encoding used here is decoded by MR_name_in_string_table
    % in runtime/mercury_stack_layout.c. The code here and there
    % must be kept in sync.
    ( if
        is_var_name_in_special_form(String, KindCode, MaybeBaseName, N),
        N < int.unchecked_left_shift(1, 10),
        (
            MaybeBaseName = yes(BaseName),
            lookup_raw_string_in_table(BaseName, MaybeOffset, !StringTable),
            MaybeOffset = yes(Offset),
            Offset < int.unchecked_left_shift(1, 16)
        ;
            MaybeBaseName = no,
            Offset = 0
        )
    then
        % | ... offset ... | ... N ... | Kind | 1 |
        % special form indication: 1 bit:   bit 0
        % kind indication:         5 bits:  bits 1-5
        % N:                       10 bits: bits 6-15
        % Offset:                  16 bits: bits 16-31
        StringCode = 1 \/
            int.unchecked_left_shift(KindCode, 1) \/
            int.unchecked_left_shift(N, 6) \/
            int.unchecked_left_shift(Offset, 16)
    else
        lookup_raw_string_in_table(String, MaybeOffset, !StringTable),
        (
            MaybeOffset = yes(Offset)
        ;
            MaybeOffset = no,
            % Says that the name of the variable is "TOO_MANY_VARIABLES".
            Offset = 1
        ),
        StringCode = int.unchecked_left_shift(Offset, 1)
    ).

:- pred is_var_name_in_special_form(string::in,
    int::out, maybe(string)::out, int::out) is semidet.

is_var_name_in_special_form(String, KindCode, MaybeBaseName, N) :-
    % state_var.m constructs variable names that always contain
    % the state var name, and usually but not always a numeric suffix.
    % The numeric suffix may be zero or positive. We could represent
    % the lack of a suffix using a negative number, but mixing unsigned
    % and signed fields in a single word is tricky, especially since
    % the size of the variable name descriptor word we generate (32 bits)
    % may or may not be the same as the word size of the compiler.
    % Instead, we simply add one to any actual suffix values, and use
    % zero to represent the absence of a numeric suffix.

    % polymorphism.m adds a numeric suffix but no type name
    % to type_ctor_infos and type_infos.

    % polymorphism.m adds a class id but no numeric suffix to
    % base_typeclass_infos and typeclass_infos. Since the usual string table
    % already does a good enough job for these, the code for handling them
    % specially is commented out.

    ( if string.remove_prefix("STATE_VARIABLE_", String, NoPrefix) then
        KindCode = 0,
        string.to_char_list(NoPrefix, NoPrefixChars),
        ( if find_number_suffix(NoPrefixChars, BaseNameChars, Num) then
            string.from_char_list(BaseNameChars, BaseName),
            MaybeBaseName = yes(BaseName),
            N = Num + 1
        else
            MaybeBaseName = yes(NoPrefix),
            N = 0
        )
    else if string.remove_prefix("TypeCtorInfo_", String, NoPrefix) then
        ( if string.to_int(NoPrefix, Num) then
            KindCode = 1,
            MaybeBaseName = no,
            N = Num
        else
            fail
        )
    else if string.remove_prefix("TypeInfo_", String, NoPrefix) then
        ( if string.to_int(NoPrefix, Num) then
            KindCode = 2,
            MaybeBaseName = no,
            N = Num
        else
            fail
        )
%   else if
%       string.remove_prefix("BaseTypeClassInfo_for_", String, NoPrefix)
%   then
%       KindCode = 3,
%       MaybeBaseName = yes(NoPrefix),
%       N = 0
%   else if string.remove_prefix("TypeClassInfo_for_", String, NoPrefix) then
%       KindCode = 4,
%       MaybeBaseName = yes(NoPrefix),
%       N = 0
    else if string.remove_prefix("PolyConst", String, NoPrefix) then
        ( if string.to_int(NoPrefix, Num) then
            KindCode = 5,
            MaybeBaseName = no,
            N = Num
        else
            fail
        )
    else
        fail
    ).

    % Given e.g. "Info_15" as input, we return "Info" as BeforeNum
    % and 15 as Num.
    %
:- pred find_number_suffix(list(char)::in, list(char)::out, int::out)
    is semidet.

find_number_suffix(String, BeforeNum, Num) :-
    list.reverse(String, RevString),
    rev_find_number_suffix(RevString, 0, Num, 1, Scale, RevRest),
    Scale > 1,
    list.reverse(RevRest, BeforeNum).

:- pred rev_find_number_suffix(list(char)::in, int::in, int::out,
    int::in, int::out, list(char)::out) is semidet.

rev_find_number_suffix([RevHead | RevTail], !Num, !Scale, RevRest) :-
    ( if char.decimal_digit_to_int(RevHead, Digit) then
        !:Num = !.Num + (!.Scale * Digit),
        !:Scale = !.Scale * 10,
        rev_find_number_suffix(RevTail, !Num, !Scale, RevRest)
    else if RevHead = '_' then
        RevRest = RevTail
    else
        fail
    ).

:- pred lookup_raw_string_in_table(string::in, maybe(int)::out,
    string_table_info::in, string_table_info::out) is det.

lookup_raw_string_in_table(String, MaybeOffset, !StringTable) :-
    !.StringTable = string_table_info(TableMap0, TableList0, TableOffset0),
    ( if
        map.search(TableMap0, String, OldOffset)
    then
        MaybeOffset = yes(OldOffset)
    else if
        Length = string.count_utf8_code_units(String),
        TableOffset = TableOffset0 + Length + 1,
        % We use a 32 bit unsigned integer to represent the offset. Computing
        % that limit exactly without getting an overflow or using unportable
        % code isn't trivial. The code below is overly conservative, requiring
        % the offset to be representable in only 30 bits. The over-conservatism
        % should not be an issue; the machine will run out of virtual memory
        % before the test below fails, for the next several years anyway.
        % (Compiling a module that has a 1 Gb string table will require
        % several tens of Gb of other compiler structures.)
        TableOffset < (1 << 30)
    then
        MaybeOffset = yes(TableOffset0),
        map.det_insert(String, TableOffset0, TableMap0, TableMap),
        TableList = [String | TableList0],
        !:StringTable = string_table_info(TableMap, TableList, TableOffset)
    else
        MaybeOffset = no
    ).

get_string_table_contents(StringTable, Strings, StringTableSize) :-
    StringTable = string_table_info(_, RevStrings, StringTableSize),
    list.reverse(RevStrings, Strings).

%---------------------------------------------------------------------------%
%
% The type table data structure.
%

:- type type_table_info
    --->    type_table_info(
                % Maps the types in the type table to their representations.
                map(mer_type, int),

                % The bytecode representing the types in the first field.
                cord(int),

                % The next available type number.
                int
            ).

init_type_table_info = type_table_info(map.init, cord.init, 0).

lookup_type_in_table(Type, TypeCode, !StringTable, !TypeTable) :-
    !.TypeTable = type_table_info(TypeMap0, _, _),
    ( if map.search(TypeMap0, Type, TypeCodePrime) then
        TypeCode = TypeCodePrime
    else
        add_type_to_table(Type, TypeCode, !StringTable, !TypeTable)
    ).

:- pred add_type_to_table(mer_type::in, int::out,
    string_table_info::in, string_table_info::out,
    type_table_info::in, type_table_info::out) is det.

add_type_to_table(Type, TypeCode, !StringTable, !TypeTable) :-
    % The encoding used here is decoded by read_encoded_type in mdbcomp/
    % program_representation.m. The code here and there must be kept in sync.
    (
        Type = defined_type(TypeCtorSymName, ArgTypes, _Kind),
        list.map_foldl2(lookup_type_in_table, ArgTypes, ArgTypeCodes,
            !StringTable, !TypeTable),
        TypeCtorSymNameStr = sym_name_to_string(TypeCtorSymName),
        lookup_string_in_table(TypeCtorSymNameStr, TypeCtorSymNameCode,
            !StringTable),
        encode_int32_det(TypeCtorSymNameCode, TypeCtorSymNameBytes),
        (
            ArgTypeCodes = [],
            Selector = 0,
            ArgTypeBytesCord = cord.init
        ;
            ArgTypeCodes = [ArgTypeCode1],
            Selector = 1,
            ArgTypeBytesCord = cord.from_list(encode_num_func(ArgTypeCode1))
        ;
            ArgTypeCodes = [ArgTypeCode1, ArgTypeCode2],
            Selector = 2,
            ArgTypeBytesCord = cord.from_list(encode_num_func(ArgTypeCode1))
                ++ cord.from_list(encode_num_func(ArgTypeCode2))
        ;
            ArgTypeCodes = [ArgTypeCode1, ArgTypeCode2, ArgTypeCode3],
            Selector = 3,
            ArgTypeBytesCord = cord.from_list(encode_num_func(ArgTypeCode1))
                ++ cord.from_list(encode_num_func(ArgTypeCode2))
                ++ cord.from_list(encode_num_func(ArgTypeCode3))
        ;
            ArgTypeCodes = [_, _, _, _ | _],
            Selector = 4,
            encode_arg_type_codes(ArgTypeCodes, ArgTypeBytesCord)
        ),
        TypeBytesCord =
            cord.from_list([Selector | TypeCtorSymNameBytes])
            ++ ArgTypeBytesCord
    ;
        Type = builtin_type(BuiltinType),
        (
            BuiltinType = builtin_type_int(int_type_int),
            Selector = 5
        ;
            BuiltinType = builtin_type_int(int_type_uint),
            Selector = 6
        ;
            BuiltinType = builtin_type_float,
            Selector = 7
        ;
            BuiltinType = builtin_type_string,
            Selector = 8
        ;
            BuiltinType = builtin_type_char,
            Selector = 9
        ;
            % XXX in order to avoid bumping the deep profiler's binary
            % compatibility version number when the fixed size integers were
            % added, the newly added types were assigned unused Selector
            % values.  The next time the format of the program representation
            % file is changed for some unavoidable reason this should be tidied
            % up.
            BuiltinType = builtin_type_int(int_type_int8),
            Selector = 14
        ;
            BuiltinType = builtin_type_int(int_type_uint8),
            Selector = 15
        ;
            BuiltinType = builtin_type_int(int_type_int16),
            Selector = 16
        ;
            BuiltinType = builtin_type_int(int_type_uint16),
            Selector = 17
        ;
            BuiltinType = builtin_type_int(int_type_int32),
            Selector = 18
        ;
            BuiltinType = builtin_type_int(int_type_uint32),
            Selector = 19
        ),
        TypeBytesCord = cord.singleton(Selector)
    ;
        Type = tuple_type(ArgTypes, _Kind),
        Selector = 10,
        list.map_foldl2(lookup_type_in_table, ArgTypes, ArgTypeCodes,
            !StringTable, !TypeTable),
        encode_arg_type_codes(ArgTypeCodes, ArgTypeBytesCord),
        TypeBytesCord = cord.singleton(Selector) ++ ArgTypeBytesCord
    ;
        Type = higher_order_type(PorF, ArgTypes, _HOInstInfo, _Purity,
            _EvalMethod),
        list.map_foldl2(lookup_type_in_table, ArgTypes, ArgTypeCodes,
            !StringTable, !TypeTable),
        encode_arg_type_codes(ArgTypeCodes, ArgTypeBytesCord),
        (
            PorF = pf_predicate,
            Selector = 11
        ;
            PorF = pf_function,
            Selector = 12
        ),
        TypeBytesCord = cord.singleton(Selector) ++ ArgTypeBytesCord
    ;
        Type = apply_n_type(_TVar, _ArgTypes, _Kind),
        unexpected($module, $pred, "apply_n_type")
    ;
        Type = kinded_type(_Kind, _SubType),
        unexpected($module, $pred, "kinded_type")
    ;
        Type = type_variable(TVar, _Kind),
        Selector = 13,
        var_to_int(TVar, TVarNum),
        encode_num_det(TVarNum, TVarNumBytes),
        TypeBytesCord = cord.singleton(Selector) ++
            cord.from_list(TVarNumBytes)
    ),
    !.TypeTable = type_table_info(TypeMap0, TypeTableCord0, NextTypeNum0),
    TypeCode = NextTypeNum0,
    NextTypeNum = NextTypeNum0 + 1,
    map.det_insert(Type, TypeCode, TypeMap0, TypeMap),
    TypeTableCord = TypeTableCord0 ++ TypeBytesCord,
    !:TypeTable = type_table_info(TypeMap, TypeTableCord, NextTypeNum).

:- pred encode_arg_type_codes(list(int)::in, cord(int)::out) is det.

encode_arg_type_codes(ArgTypeCodes, ArgTypeBytesCord) :-
    list.map(encode_num_det, ArgTypeCodes, ArgTypeByteLists),
    ArgTypeByteCords = list.map(cord.from_list, ArgTypeByteLists),
    ArgTypeBytesCord0 = cord.cord_list_to_cord(ArgTypeByteCords),
    list.length(ArgTypeCodes, NumArgTypeCodes),
    ArgTypeBytesCord = cord.from_list(encode_num_func(NumArgTypeCodes))
        ++ ArgTypeBytesCord0.

get_type_table_contents(TypeTable, NumTypes, TypeBytes) :-
    TypeTable = type_table_info(_, TypeBytesCord, NumTypes),
    TypeBytes = cord.list(TypeBytesCord).

%---------------------------------------------------------------------------%
:- end_module ll_backend.prog_rep_tables.
%---------------------------------------------------------------------------%
