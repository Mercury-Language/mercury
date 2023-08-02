%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: term_conversion.m.
% Stability: medium.
%
% This file provides predicates to convert values of arbitrary types to terms,
% and vice versa.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module term_conversion.
:- interface.

:- import_module list.
:- import_module term.
:- import_module type_desc.
:- import_module univ.

%---------------------------------------------------------------------------%
%
% Types that record the results of term to type conversions.
%

:- type term_to_type_result(T, U)
    --->    ok(T)
    ;       error(term_to_type_error(U)).

:- type term_to_type_result(T) == term_to_type_result(T, generic).

:- type term_to_type_error(T)
    --->    type_error(
                term(T),
                type_desc.type_desc,
                context,
                term_to_type_context
            )
    ;       mode_error(
                var(T),
                term_to_type_context
            ).

:- type term_to_type_context == list(term_to_type_arg_context).

:- type term_to_type_arg_context
    --->    arg_context(
                const,      % functor
                int,        % argument number (starting from 1)
                context     % filename & line number
            ).

%---------------------------------------------------------------------------%
%
% The following predicates can convert values of (almost) any type
% to the type term and back again.
%

    % try_term_to_type(Term, Result):
    %
    % Try to convert the given term to a ground value of type T.
    % If successful, return `ok(X)' where X is the converted value.
    % If Term is not ground, return `mode_error(Var, Context)',
    % where Var is a variable occurring in Term.
    % If Term is not a valid term of the specified type, return
    % `type_error(SubTerm, ExpectedType, Context, ArgContexts)',
    % where SubTerm is a sub-term of Term and ExpectedType is the type
    % expected for that part of Term.
    % Context specifies the file and line number where the
    % offending part of the term was read in from, if available.
    % ArgContexts specifies the path from the root of the term
    % to the offending subterm.
    %
:- func try_term_to_type(term(U)) = term_to_type_result(T, U).
:- pred try_term_to_type(term(U)::in, term_to_type_result(T, U)::out) is det.

    % term_to_type(Term, Type) :- try_term_to_type(Term, ok(Type)).
    %
:- pred term_to_type(term(U)::in, T::out) is semidet.

    % Like term_to_type, but calls error/1 rather than failing.
    %
:- func det_term_to_type(term(_)) = T.
:- pred det_term_to_type(term(_)::in, T::out) is det.

    % Converts a value to a term representation of that value.
    %
:- func type_to_term(T) = term(_).
:- pred type_to_term(T::in, term(_)::out) is det.

    % Convert the value stored in the univ (as distinct from the univ itself)
    % to a term.
    %
:- func univ_to_term(univ) = term(_).
:- pred univ_to_term(univ::in, term(_)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- import_module array.
:- import_module bitmap.
:- import_module construct.
:- import_module deconstruct.
:- import_module int.
:- import_module integer.
:- import_module require.
:- import_module string.
:- import_module term_context.
:- import_module term_int.
:- import_module term_subst.
:- import_module version_array.

%---------------------------------------------------------------------------%

try_term_to_type(Term) = Result :-
    try_term_to_type(Term, Result).

try_term_to_type(Term, Result) :-
    try_term_to_univ(Term, type_desc.type_of(ValTypedVar), UnivResult),
    (
        UnivResult = ok(Univ),
        det_univ_to_type(Univ, Val),
        same_type(Val, ValTypedVar),
        Result = ok(Val)
    ;
        UnivResult = error(Error),
        Result = error(Error)
    ).

term_to_type(Term, Val) :-
    try_term_to_type(Term, ok(Val)).

:- pred try_term_to_univ(term(T)::in, type_desc.type_desc::in,
    term_to_type_result(univ, T)::out) is det.

try_term_to_univ(Term, Type, Result) :-
    try_term_to_univ_2(Term, Type, [], Result).

:- pred try_term_to_univ_2(term(T)::in,
    type_desc::in, term_to_type_context::in,
    term_to_type_result(univ, T)::out) is det.

try_term_to_univ_2(Term, Type, Context, Result) :-
    (
        Term = variable(Var, _),
        Result = error(mode_error(Var, Context))
    ;
        Term = functor(Functor, ArgTerms, TermContext),
        ( if
            type_ctor_and_args(Type, TypeCtor, TypeArgs),
            ModuleName = type_ctor_module_name(TypeCtor),
            TypeCtorName = type_ctor_name(TypeCtor),
            term_to_univ_special_case(ModuleName, TypeCtorName,
                TypeArgs, Term, Context, SpecialCaseResult)
        then
            Result = SpecialCaseResult
        else if
            Functor = atom(FunctorName),
            list.length(ArgTerms, Arity),
            find_functor(Type, FunctorName, Arity, FunctorNumber, ArgTypes),
            term_list_to_univ_list(ArgTerms, ArgTypes, Functor, 1, Context,
                TermContext, ArgsResult)
        then
            (
                ArgsResult = ok(ArgValues),
                ( if
                    Value = construct.construct(Type, FunctorNumber, ArgValues)
                then
                    Result = ok(Value)
                else
                    unexpected($pred, "construct/3 failed")
                )
            ;
                ArgsResult = error(Error),
                Result = error(Error)
            )
        else
            % The arg contexts are built up in reverse order,
            % so we need to reverse them here.
            list.reverse(Context, RevContext),
            Result = error(type_error(Term, Type, TermContext, RevContext))
        )
    ).

:- pred term_to_univ_special_case(string::in, string::in, list(type_desc)::in,
    term(T)::in(bound(functor(ground, ground, ground))),
    term_to_type_context::in, term_to_type_result(univ, T)::out) is semidet.

term_to_univ_special_case(ModuleName, TypeCtorName, TypeArgs, Term,
        PrevContext, Result) :-
    (
        ModuleName = "builtin",
        TypeArgs = [],
        Term = functor(Functor, [], _),
        (
            TypeCtorName = "character",
            Functor = atom(FunctorName),
            string.first_char(FunctorName, Char, ""),
            type_to_univ(Char, Univ)
        ;
            TypeCtorName = "string",
            Functor = string(String),
            type_to_univ(String, Univ)
        ;
            TypeCtorName = "int",
            Functor = integer(_Base, Integer, signed, size_word),
            integer.to_int(Integer, Int),
            type_to_univ(Int, Univ)
        ;
            TypeCtorName = "uint",
            Functor = integer(_Base, Integer, unsigned, size_word),
            integer.to_uint(Integer, UInt),
            type_to_univ(UInt, Univ)
        ;
            TypeCtorName = "int8",
            Functor = integer(_Base, Integer, signed, size_8_bit),
            integer.to_int8(Integer, Int8),
            type_to_univ(Int8, Univ)
        ;
            TypeCtorName = "uint8",
            Functor = integer(_Base, Integer, unsigned, size_8_bit),
            integer.to_uint8(Integer, UInt8),
            type_to_univ(UInt8, Univ)
        ;
            TypeCtorName = "int16",
            Functor = integer(_Base, Integer, signed, size_16_bit),
            integer.to_int16(Integer, Int16),
            type_to_univ(Int16, Univ)
        ;
            TypeCtorName = "uint16",
            Functor = integer(_Base, Integer, unsigned, size_16_bit),
            integer.to_uint16(Integer, UInt16),
            type_to_univ(UInt16, Univ)
        ;
            TypeCtorName = "int32",
            Functor = integer(_Base, Integer, signed, size_32_bit),
            integer.to_int32(Integer, Int32),
            type_to_univ(Int32, Univ)
        ;
            TypeCtorName = "uint32",
            Functor = integer(_Base, Integer, unsigned, size_32_bit),
            integer.to_uint32(Integer, UInt32),
            type_to_univ(UInt32, Univ)
        ;
            TypeCtorName = "int64",
            Functor = integer(_Base, Integer, signed, size_64_bit),
            integer.to_int64(Integer, Int64),
            type_to_univ(Int64, Univ)
        ;
            TypeCtorName = "uint64",
            Functor = integer(_Base, Integer, unsigned, size_64_bit),
            integer.to_uint64(Integer, UInt64),
            type_to_univ(UInt64, Univ)
        ;
            TypeCtorName = "float",
            Functor = float(Float),
            type_to_univ(Float, Univ)
        ),
        Result = ok(Univ)
    ;
        ModuleName = "bitmap",
        TypeCtorName = "bitmap",
        TypeArgs = [],
        % Bitmaps are represented as hex strings.
        Term = functor(string(String), [], _),
        type_to_univ(bitmap.from_string(String), Univ),
        Result = ok(Univ)
    ;
        ModuleName = "array",
        TypeCtorName = "array",
        TypeArgs = [ElemType],
        % arrays are represented as terms of the form
        %   array([elem1, elem2, ...])
        Term = functor(atom("array"), [ArgList], TermContext),

        % To convert such terms back to arrays, we first
        % convert the term representing the list of elements back to a list,
        % and then (if successful) we just call the array/1 function.
        has_type(Elem, ElemType),
        ListType = type_of([Elem] : list(_)),
        ArgContext = arg_context(atom("array"), 1, TermContext),
        NewContext = [ArgContext | PrevContext],
        try_term_to_univ_2(ArgList, ListType, NewContext, ArgResult),
        (
            ArgResult = ok(ListUniv),
            has_type(Elem2, ElemType),
            same_type(List, [Elem2]),
            det_univ_to_type(ListUniv, List),
            Array = array(List),
            Result = ok(univ(Array))
        ;
            ArgResult = error(Error),
            Result = error(Error)
        )
    ;
        ModuleName = "version_array",
        TypeCtorName = "version_array",
        TypeArgs = [ElemType],
        % We handle version arrays in pretty much the same way
        % as normal arrays.
        Term = functor(atom("version_array"), [ArgList], TermContext),
        has_type(Elem, ElemType),
        ListType = type_of([Elem] : list(_)),
        ArgContext = arg_context(atom("version_array"), 1, TermContext),
        NewContext = [ArgContext | PrevContext],
        try_term_to_univ_2(ArgList, ListType, NewContext, ArgResult),
        (
            ArgResult = ok(ListUniv),
            has_type(Elem2, ElemType),
            same_type(List, [Elem2]),
            det_univ_to_type(ListUniv, List),
            Array = version_array(List),
            Result = ok(univ(Array))
        ;
            ArgResult = error(Error),
            Result = error(Error)
        )
    ;
        ModuleName = "univ",
        TypeCtorName = "univ",
        TypeArgs = [],
        % Implementing this properly would require keeping a global table
        % mapping from type names to type_infos for all of the types in the
        % program... so for the moment, we only allow it for basic types.
        Term = functor(atom("univ"), [ArgTerm], _),
        ArgTerm = functor(atom(":"), [ValueTerm, TypeTerm], _),
        (
            TypeTerm = functor(atom("int"), [], _),
            term_int.term_to_int(ValueTerm, Int),
            Univ = univ(Int)
        ;
            TypeTerm = functor(atom("uint"), [], _),
            term_int.term_to_uint(ValueTerm, UInt),
            Univ = univ(UInt)
        ;
            TypeTerm = functor(atom("int8"), [], _),
            term_int.term_to_int8(ValueTerm, Int8),
            Univ = univ(Int8)
        ;
            TypeTerm = functor(atom("uint8"), [], _),
            term_int.term_to_uint8(ValueTerm, UInt8),
            Univ = univ(UInt8)
        ;
            TypeTerm = functor(atom("int16"), [], _),
            term_int.term_to_int16(ValueTerm, Int16),
            Univ = univ(Int16)
        ;
            TypeTerm = functor(atom("uint16"), [], _),
            term_int.term_to_uint16(ValueTerm, UInt16),
            Univ = univ(UInt16)
        ;
            TypeTerm = functor(atom("int32"), [], _),
            term_int.term_to_int32(ValueTerm, Int32),
            Univ = univ(Int32)
        ;
            TypeTerm = functor(atom("uint32"), [], _),
            term_int.term_to_uint32(ValueTerm, UInt32),
            Univ = univ(UInt32)
        ;
            TypeTerm = functor(atom("int64"), [], _),
            term_int.term_to_int64(ValueTerm, Int64),
            Univ = univ(Int64)
        ;
            TypeTerm = functor(atom("uint64"), [], _),
            term_int.term_to_uint64(ValueTerm, UInt64),
            Univ = univ(UInt64)
        ;
            TypeTerm = functor(atom("string"), [], _),
            ValueTerm = functor(string(String), [], _),
            Univ = univ(String)
        ;
            TypeTerm = functor(atom("float"), [], _),
            ValueTerm = functor(float(Float), [], _),
            Univ = univ(Float)
        ),
        % The result is a univ, but it is also wrapped in a univ
        % like all the other results returned from this procedure.
        Result = ok(univ(Univ))
    ).

:- pred term_list_to_univ_list(list(term(T))::in,
    list(type_desc)::in, const::in, int::in,
    term_to_type_context::in, context::in,
    term_to_type_result(list(univ), T)::out) is semidet.

term_list_to_univ_list([], [], _, _, _, _, ok([])).
term_list_to_univ_list([ArgTerm | ArgTerms], [Type | Types], Functor, ArgNum,
        PrevContext, TermContext, Result) :-
    ArgContext = arg_context(Functor, ArgNum, TermContext),
    NewContext = [ArgContext | PrevContext],
    try_term_to_univ_2(ArgTerm, Type, NewContext, ArgResult),
    (
        ArgResult = ok(Arg),
        term_list_to_univ_list(ArgTerms, Types, Functor, ArgNum + 1,
            PrevContext, TermContext, RestResult),
        (
            RestResult = ok(Rest),
            Result = ok([Arg | Rest])
        ;
            RestResult = error(Error),
            Result = error(Error)
        )
    ;
        ArgResult = error(Error),
        Result = error(Error)
    ).

det_term_to_type(Term) = X :-
    det_term_to_type(Term, X).

det_term_to_type(Term, X) :-
    ( if term_to_type(Term, XPrime) then
        X = XPrime
    else if \+ term_subst.term_is_ground(Term) then
        unexpected($pred, "the term is not ground")
    else
        Message = "type error:\nthe term is not a valid term" ++
            " for type `" ++ type_name(type_of(X)) ++ "'",
        unexpected($pred, Message)
    ).

%---------------------------------------------------------------------------%

type_to_term(Var) = Term :-
    type_to_term(Var, Term).

type_to_term(Val, Term) :-
    type_to_univ(Val, Univ),
    univ_to_term(Univ, Term).

univ_to_term(Univ) = Term :-
    univ_to_term(Univ, Term).

univ_to_term(Univ, Term) :-
    Context = term_context.dummy_context,
    Type = univ_type(Univ),
    ( if construct.num_functors(Type) = _ then
        deconstruct(univ_value(Univ), canonicalize, FunctorString,
            _FunctorArity, FunctorArgs),
        univ_list_to_term_list(FunctorArgs, TermArgs),
        Term = functor(atom(FunctorString), TermArgs, Context)
    else
        ( if
            type_ctor_and_args(Type, TypeCtor, TypeArgs),
            ModuleName = type_ctor_module_name(TypeCtor),
            TypeCtorName = type_ctor_name(TypeCtor),
            univ_to_term_special_case(ModuleName, TypeCtorName, TypeArgs,
                Univ, Context, SpecialCaseTerm)
        then
            Term = SpecialCaseTerm
        else
            Message = "unknown type `" ++ type_name(univ_type(Univ)) ++ "'",
            unexpected($pred, Message)
        )
    ).

:- pred univ_to_term_special_case(string::in, string::in, list(type_desc)::in,
    univ::in, context::in, term(T)::out) is semidet.

univ_to_term_special_case(ModuleName, TypeCtorName, TypeArgs, Univ, Context,
        Term) :-
    (
        ModuleName = "builtin",
        TypeArgs = [],
        (
            TypeCtorName = "character",
            det_univ_to_type(Univ, Char),
            string.char_to_string(Char, CharName),
            Functor = atom(CharName)
        ;
            TypeCtorName = "string",
            det_univ_to_type(Univ, String),
            Functor = string(String)
        ;
            TypeCtorName = "int",
            det_univ_to_type(Univ, Int),
            Functor = integer(base_10, integer(Int), signed, size_word)
        ;
            TypeCtorName = "uint",
            det_univ_to_type(Univ, UInt),
            Functor = integer(base_10, integer.from_uint(UInt), unsigned,
                size_word)
        ;
            TypeCtorName = "int8",
            det_univ_to_type(Univ, Int8),
            Functor = integer(base_10, integer.from_int8(Int8), signed,
                size_8_bit)
        ;
            TypeCtorName = "uint8",
            det_univ_to_type(Univ, UInt8),
            Functor = integer(base_10, integer.from_uint8(UInt8), unsigned,
                size_8_bit)
        ;
            TypeCtorName = "int16",
            det_univ_to_type(Univ, Int16),
            Functor = integer(base_10, integer.from_int16(Int16), signed,
                size_16_bit)
        ;
            TypeCtorName = "uint16",
            det_univ_to_type(Univ, UInt16),
            Functor = integer(base_10, integer.from_uint16(UInt16), unsigned,
                size_16_bit)
        ;
            TypeCtorName = "int32",
            det_univ_to_type(Univ, Int32),
            Functor = integer(base_10, integer.from_int32(Int32), signed,
                size_32_bit)
        ;
            TypeCtorName = "uint32",
            det_univ_to_type(Univ, UInt32),
            Functor = integer(base_10, integer.from_uint32(UInt32), unsigned,
                size_32_bit)
        ;
            TypeCtorName = "int64",
            det_univ_to_type(Univ, Int64),
            Functor = integer(base_10, integer.from_int64(Int64), signed,
                size_64_bit)
        ;
            TypeCtorName = "uint64",
            det_univ_to_type(Univ, UInt64),
            Functor = integer(base_10, integer.from_uint64(UInt64), unsigned,
                size_64_bit)
        ;
            TypeCtorName = "float",
            det_univ_to_type(Univ, Float),
            Functor = float(Float)
        ),
        Term = functor(Functor, [], Context)
    ;
        ModuleName = "type_desc",
        TypeCtorName = "type_desc",
        TypeArgs = [],
        det_univ_to_type(Univ, TypeInfo),
        type_info_to_term(Context, TypeInfo, SubTerm),
        Term = functor(atom("type_info"), [SubTerm], Context)
    ;
        ModuleName = "univ",
        TypeCtorName = "univ",
        TypeArgs = [],
        det_univ_to_type(Univ, NestedUniv),
        type_to_term(univ_value(NestedUniv), ValueTerm),
        type_info_to_term(Context, univ_type(NestedUniv), TypeTerm),
        Term = functor(atom("univ"),
            [functor(atom(":"), [ValueTerm, TypeTerm], Context)], Context)
    ;
        ModuleName = "bitmap",
        TypeCtorName = "bitmap",
        TypeArgs = [],
        det_univ_to_type(Univ, Bitmap),
        BitmapStr = bitmap.to_string(Bitmap),
        Term = functor(string(BitmapStr), [], Context)
    ;
        ModuleName = "array",
        TypeCtorName = "array",
        TypeArgs = [ElemType],
        has_type(Elem, ElemType),
        same_type(List, [Elem]),
        det_univ_to_type(Univ, Array),
        array.to_list(Array, List),
        type_to_term(List, ArgsTerm),
        Term = functor(atom("array"), [ArgsTerm], Context)
    ;
        ModuleName = "version_array",
        TypeCtorName = "version_array",
        TypeArgs = [ElemType],
        has_type(Elem, ElemType),
        same_type(List, [Elem]),
        det_univ_to_type(Univ, Array),
        List = version_array.to_list(Array),
        type_to_term(List, ArgsTerm),
        Term = functor(atom("version_array"), [ArgsTerm], Context)
    ).

:- pred univ_list_to_term_list(list(univ)::in, list(term(T))::out) is det.

univ_list_to_term_list([], []).
univ_list_to_term_list([Value|Values], [Term|Terms]) :-
    univ_to_term(Value, Term),
    univ_list_to_term_list(Values, Terms).

    % Given a type_info, return a term that represents the name of that type.
    %
:- pred type_info_to_term(context::in, type_desc::in, term(T)::out) is det.

type_info_to_term(Context, TypeInfo, Term) :-
    type_ctor_and_args(TypeInfo, TypeCtor, ArgTypes),
    TypeName = type_ctor_name(TypeCtor),
    ModuleName = type_ctor_name(TypeCtor),
    list.map(type_info_to_term(Context), ArgTypes, ArgTerms),

    ( if ModuleName = "builtin" then
        Term = functor(atom(TypeName), ArgTerms, Context)
    else
        Arg1 = functor(atom(ModuleName), [], Context),
        Arg2 = functor(atom(TypeName), ArgTerms, Context),
        Term = functor(atom(":"), [Arg1, Arg2], Context)
    ).

%---------------------------------------------------------------------------%
:- end_module term_conversion.
%---------------------------------------------------------------------------%
