%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2015, 2018-2020, 2025 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: string.parse_runtime.m.
% Stability: high.
%
% This module parses format strings for the runtime system;
% the module compiler/parse_string_format.m does the same job for the compiler.
% Any changes here will probably also require a corresponding change there.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module string.parse_runtime.
:- interface.

:- import_module list.
:- import_module string.parse_util.

%---------------------------------------------------------------------------%

:- type string_format_spec
    --->    spec_constant_string(
                string
            )
    ;       spec_char(
                string_format_flags,
                string_format_maybe_width,
                char
            )
    ;       spec_string(
                string_format_flags,
                string_format_maybe_width,
                string_format_maybe_prec,
                string
            )
    ;       spec_signed_int(
                string_format_flags,
                string_format_maybe_width,
                string_format_maybe_prec,
                sized_int
            )
    ;       spec_unsigned_int(
                string_format_flags,
                string_format_maybe_width,
                string_format_maybe_prec,
                string_format_int_base,
                sized_int
            )
    ;       spec_uint(
                string_format_flags,
                string_format_maybe_width,
                string_format_maybe_prec,
                string_format_int_base,
                sized_uint
            )
    ;       spec_float(
                string_format_flags,
                string_format_maybe_width,
                string_format_maybe_prec,
                string_format_float_kind,
                float
            ).

:- type sized_int
    --->    sized_int(int)
    ;       sized_int8(int8)
    ;       sized_int16(int16)
    ;       sized_int32(int32)
    ;       sized_int64(int64).

:- type sized_uint
    --->    sized_uint(uint)
    ;       sized_uint8(uint8)
    ;       sized_uint16(uint16)
    ;       sized_uint32(uint32)
    ;       sized_uint64(uint64).

    % This predicate parses the entire format string. When it encounters
    % something that looks like a conversion specification (i.e. it starts
    % with a '%' character), but which cannot be parsed as one, it records
    % an error message, and keeps going.
    %
    % Note that making this predicate use an accumulator for the lists
    % of specs and errors seen so far would yield cleaner code,
    % but would probably be slower since our caller would have to unreverse
    % the list of specs we return.
    %
    % The lack of tail recursion here should not be a problem, since no
    % format string will be long enough to make us consume too much stack.
    %
:- pred parse_format_string(list(char)::in,
    list(poly_type)::in, int::in,
    list(string_format_spec)::out, list(string_format_error)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

parse_format_string(!.Chars, !.PolyTypes, SpecNum, Specs, Errors) :-
    gather_non_percent_chars(!.Chars, NonConversionSpecChars, GatherEndedBy),
    (
        GatherEndedBy = found_end_of_string,
        Specs0 = [],
        (
            !.PolyTypes = [],
            Errors = []
        ;
            !.PolyTypes = [_ | _],
            Error = error_extra_polytypes(SpecNum, list.length(!.PolyTypes)),
            Errors = [Error]
        )
    ;
        GatherEndedBy = found_percent(!:Chars),
        parse_conversion_specification(!Chars, !PolyTypes, SpecNum,
            HeadSpec, HeadErrors),
        parse_format_string(!.Chars, !.PolyTypes, SpecNum + 1,
            TailSpecs, TailErrors),
        (
            HeadErrors = [],
            Specs0 = [HeadSpec | TailSpecs],
            Errors = TailErrors
        ;
            HeadErrors = [_ | _],
            Specs0 = TailSpecs,
            Errors = HeadErrors ++ TailErrors
        )
    ),
    (
        NonConversionSpecChars = [],
        Specs = Specs0
    ;
        NonConversionSpecChars = [_ | _],
        NonConversionSpecString =
            string.from_char_list(NonConversionSpecChars),
        StringSpec = spec_constant_string(NonConversionSpecString),
        Specs = [StringSpec | Specs0]
    ).

    % Each conversion specification starts with the character '%' (which
    % our caller has already read) and ends with a conversion specifier.
    % In between there may be (in this order) zero or more flags, an optional
    % minimum field width, and an optional precision.
    %
:- pred parse_conversion_specification(list(char)::in, list(char)::out,
    list(poly_type)::in, list(poly_type)::out, int::in,
    string_format_spec::out, list(string_format_error)::out) is det.

parse_conversion_specification(!Chars, !PolyTypes, SpecNum, Spec, Errors) :-
    Flags0 = string_format_flags(flag_hash_clear, flag_space_clear,
        flag_zero_clear, flag_minus_clear, flag_plus_clear),
    gather_flag_chars(!Chars, Flags0, Flags),
    get_optional_width(!Chars, !PolyTypes, SpecNum, MaybeWidth, WidthErrors),
    get_optional_prec(!Chars, !PolyTypes, SpecNum, MaybePrec, PrecErrors),
    get_first_spec(!Chars, !PolyTypes, Flags, MaybeWidth, MaybePrec, SpecNum,
        Spec, SpecErrors),
    Errors = WidthErrors ++ PrecErrors ++ SpecErrors.

    % Do we have a minimum field width? If yes, get it.
    %
:- pred get_optional_width(list(char)::in, list(char)::out,
    list(poly_type)::in, list(poly_type)::out, int::in,
    string_format_maybe_width::out, list(string_format_error)::out) is det.

get_optional_width(!Chars, !PolyTypes, SpecNum, MaybeWidth, Errors) :-
    ( if !.Chars = ['*' | !:Chars] then
        (
            !.PolyTypes = [PolyType | !:PolyTypes],
            ( if PolyType = i(PolyWidth) then
                MaybeWidth = specified_width(PolyWidth),
                Errors = []
            else
                MaybeWidth = no_specified_width,
                Errors = [error_nonint_star_width(SpecNum,
                    poly_type_to_kind(PolyType))]
            )
        ;
            !.PolyTypes = [],
            MaybeWidth = no_specified_width,
            Errors = [error_missing_star_width(SpecNum)]
        )
    else if get_nonzero_number_prefix(!Chars, Width) then
        MaybeWidth = specified_width(Width),
        Errors = []
    else
        MaybeWidth = no_specified_width,
        Errors = []
    ).

    % Do we have a precision? If yes, get it.
    %
:- pred get_optional_prec(list(char)::in, list(char)::out,
    list(poly_type)::in, list(poly_type)::out, int::in,
    string_format_maybe_prec::out, list(string_format_error)::out) is det.

get_optional_prec(!Chars, !PolyTypes, SpecNum, MaybePrec, Errors) :-
    ( if !.Chars = ['.' | !:Chars] then
        ( if !.Chars = ['*' | !:Chars] then
            (
                !.PolyTypes = [PolyType | !:PolyTypes],
                ( if PolyType = i(PolyPrec) then
                    MaybePrec = specified_prec(PolyPrec),
                    Errors = []
                else
                    MaybePrec = no_specified_prec,
                    Errors = [error_nonint_star_prec(SpecNum,
                        poly_type_to_kind(PolyType))]
                )
            ;
                !.PolyTypes = [],
                MaybePrec = no_specified_prec,
                Errors = [error_missing_star_prec(SpecNum)]
            )
        else
            % This treats an empty string as an EXPLICIT zero.
            get_number_prefix(!Chars, Prec),
            MaybePrec = specified_prec(Prec),
            Errors = []
        )
    else
        MaybePrec = no_specified_prec,
        Errors = []
    ).

%---------------------------------------------------------------------------%

    % get_first_spec(!Chars, !PolyTypes, Flags, MaybeWidth, MaybePrec,
    %   SpecNum, Spec, Errors):
    %
    % Try to read one conversion specifier, whose percent sign, flags,
    % width and precision have already been read, from !Chars.
    %
    % If successful, consume the corresponding poly_type from !PolyTypes,
    % we return the specifier as Spec and return an empty error list.
    %
    % If there is a problem, we return a garbage Spec and a nonempty
    % errors list. We also consume the poly_type that corresponds
    % (or at least, looks like it corresponds) to the specifier,
    % if there is one.
    %
:- pred get_first_spec(list(char)::in, list(char)::out,
    list(poly_type)::in, list(poly_type)::out,
    string_format_flags::in, string_format_maybe_width::in,
    string_format_maybe_prec::in, int::in,
    string_format_spec::out, list(string_format_error)::out) is det.

get_first_spec(!Chars, !PolyTypes, _Flags, _MaybeWidth, _MaybePrec, SpecNum,
        Spec, Errors) :-
    !.Chars = [],
    Spec = spec_constant_string(""),
    Errors = [error_no_specifier(SpecNum, list.length(!.PolyTypes))].
get_first_spec(!Chars, !PolyTypes, Flags0, MaybeWidth, MaybePrec, SpecNum,
        Spec, Errors) :-
    !.Chars = [SpecChar | !:Chars],
    ( if
        parse_spec(SpecChar, !PolyTypes, Flags0, MaybeWidth, MaybePrec,
            SpecNum, SpecPrime, ErrorsPrime)
    then
        Spec = SpecPrime,
        Errors = ErrorsPrime
    else
        Error = error_unknown_specifier(SpecNum, SpecChar),
        Spec = spec_constant_string(""),
        Errors = [Error]
    ).

:- pred parse_spec(char::in, list(poly_type)::in, list(poly_type)::out,
    string_format_flags::in, string_format_maybe_width::in,
    string_format_maybe_prec::in, int::in,
    string_format_spec::out, list(string_format_error)::out) is semidet.

parse_spec(SpecChar, !PolyTypes, !.Flags, MaybeWidth, MaybePrec,
        SpecNum, Spec, Errors) :-
    require_switch_arms_det [SpecChar]
    (
        SpecChar = '%',
        Spec = spec_constant_string("%"),
        Errors = []
    ;
        ( SpecChar = 'd'
        ; SpecChar = 'i'
        ),
        (
            !.PolyTypes = [SpecPolyType | !:PolyTypes],
            (
                ( SpecPolyType = i(Int),        SizedInt = sized_int(Int)
                ; SpecPolyType = i8(Int8),      SizedInt = sized_int8(Int8)
                ; SpecPolyType = i16(Int16),    SizedInt = sized_int16(Int16)
                ; SpecPolyType = i32(Int32),    SizedInt = sized_int32(Int32)
                ; SpecPolyType = i64(Int64),    SizedInt = sized_int64(Int64)
                ),
                % Base is always decimal.
                Spec = spec_signed_int(!.Flags, MaybeWidth, MaybePrec,
                    SizedInt),
                Errors = []
            ;
                ( SpecPolyType = f(_)
                ; SpecPolyType = u(_)
                ; SpecPolyType = u8(_)
                ; SpecPolyType = u16(_)
                ; SpecPolyType = u32(_)
                ; SpecPolyType = u64(_)
                ; SpecPolyType = s(_)
                ; SpecPolyType = c(_)
                ),
                Error = error_wrong_polytype(SpecNum, SpecChar,
                    poly_type_to_kind(SpecPolyType)),
                Spec = spec_constant_string(""),
                Errors = [Error]
            )
        ;
            !.PolyTypes = [],
            Error = error_no_polytype(SpecNum, SpecChar),
            Spec = spec_constant_string(""),
            Errors = [Error]
        )
    ;
        (
            SpecChar = 'o',
            Base = base_octal
        ;
            SpecChar = 'u',
            Base = base_decimal
        ;
            SpecChar = 'x',
            Base = base_hex_lc
        ;
            SpecChar = 'X',
            Base = base_hex_uc
        ;
            SpecChar = 'p',
            Base = base_hex_p,
            % XXX This should not be necessary.
            !Flags ^ flag_hash := flag_hash_set
        ),
        (
            !.PolyTypes = [SpecPolyType | !:PolyTypes],
            (
                ( SpecPolyType = i(Int),      SizedInt = sized_int(Int)
                ; SpecPolyType = i8(Int8),    SizedInt = sized_int8(Int8)
                ; SpecPolyType = i16(Int16),  SizedInt = sized_int16(Int16)
                ; SpecPolyType = i32(Int32),  SizedInt = sized_int32(Int32)
                ; SpecPolyType = i64(Int64),  SizedInt = sized_int64(Int64)
                ),
                Spec = spec_unsigned_int(!.Flags, MaybeWidth, MaybePrec, Base,
                    SizedInt),
                Errors = []
            ;
                ( SpecPolyType = u(UInt),     SizedUInt = sized_uint(UInt)
                ; SpecPolyType = u8(UInt8),   SizedUInt = sized_uint8(UInt8)
                ; SpecPolyType = u16(UInt16), SizedUInt = sized_uint16(UInt16)
                ; SpecPolyType = u32(UInt32), SizedUInt = sized_uint32(UInt32)
                ; SpecPolyType = u64(UInt64), SizedUInt = sized_uint64(UInt64)
                ),
                Spec = spec_uint(!.Flags, MaybeWidth, MaybePrec, Base,
                    SizedUInt),
                Errors = []
            ;
                ( SpecPolyType = f(_)
                ; SpecPolyType = s(_)
                ; SpecPolyType = c(_)
                ),
                Error = error_wrong_polytype(SpecNum, SpecChar,
                    poly_type_to_kind(SpecPolyType)),
                Spec = spec_constant_string(""),
                Errors = [Error]
            )
        ;
            !.PolyTypes = [],
            Error = error_no_polytype(SpecNum, SpecChar),
            Spec = spec_constant_string(""),
            Errors = [Error]
        )
    ;
        (
            SpecChar = 'e',
            FloatKind = kind_e_scientific_lc
        ;
            SpecChar = 'E',
            FloatKind = kind_e_scientific_uc
        ;
            SpecChar = 'f',
            FloatKind = kind_f_plain_lc
        ;
            SpecChar = 'F',
            FloatKind = kind_f_plain_uc
        ;
            SpecChar = 'g',
            FloatKind = kind_g_flexible_lc
        ;
            SpecChar = 'G',
            FloatKind = kind_g_flexible_uc
        ),
        (
            !.PolyTypes = [SpecPolyType | !:PolyTypes],
            ( if SpecPolyType = f(Float) then
                Spec = spec_float(!.Flags, MaybeWidth, MaybePrec,
                    FloatKind, Float),
                Errors = []
            else
                Error = error_wrong_polytype(SpecNum, SpecChar,
                    poly_type_to_kind(SpecPolyType)),
                Spec = spec_constant_string(""),
                Errors = [Error]
            )
        ;
            !.PolyTypes = [],
            Error = error_no_polytype(SpecNum, SpecChar),
            Spec = spec_constant_string(""),
            Errors = [Error]
        )
    ;
        SpecChar = 'c',
        (
            !.PolyTypes = [SpecPolyType | !:PolyTypes],
            ( if SpecPolyType = c(Char) then
                % XXX Should we generate an error if MaybePrec = yes(...)?
                Spec = spec_char(!.Flags, MaybeWidth, Char),
                Errors = []
            else
                Error = error_wrong_polytype(SpecNum, SpecChar,
                    poly_type_to_kind(SpecPolyType)),
                Spec = spec_constant_string(""),
                Errors = [Error]
            )
        ;
            !.PolyTypes = [],
            Error = error_no_polytype(SpecNum, SpecChar),
            Spec = spec_constant_string(""),
            Errors = [Error]
        )
    ;
        SpecChar = 's',
        (
            !.PolyTypes = [SpecPolyType | !:PolyTypes],
            ( if SpecPolyType = s(Str) then
                Spec = spec_string(!.Flags, MaybeWidth, MaybePrec, Str),
                Errors = []
            else
                Error = error_wrong_polytype(SpecNum, SpecChar,
                    poly_type_to_kind(SpecPolyType)),
                Spec = spec_constant_string(""),
                Errors = [Error]
            )
        ;
            !.PolyTypes = [],
            Error = error_no_polytype(SpecNum, SpecChar),
            Spec = spec_constant_string(""),
            Errors = [Error]
        )
    ).

:- func poly_type_to_kind(poly_type) = poly_kind.

poly_type_to_kind(c(_)) = poly_kind_char.
poly_type_to_kind(s(_)) = poly_kind_str.
poly_type_to_kind(i(_)) = poly_kind_int.
poly_type_to_kind(i8(_)) = poly_kind_int8.
poly_type_to_kind(i16(_)) = poly_kind_int16.
poly_type_to_kind(i32(_)) = poly_kind_int32.
poly_type_to_kind(i64(_)) = poly_kind_int64.
poly_type_to_kind(u(_)) = poly_kind_uint.
poly_type_to_kind(u8(_)) = poly_kind_uint8.
poly_type_to_kind(u16(_)) = poly_kind_uint16.
poly_type_to_kind(u32(_)) = poly_kind_uint32.
poly_type_to_kind(u64(_)) = poly_kind_uint64.
poly_type_to_kind(f(_)) = poly_kind_float.

%---------------------------------------------------------------------------%
:- end_module string.parse_runtime.
%---------------------------------------------------------------------------%
