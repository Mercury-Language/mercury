%----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: parse_string_format.m.
%
% This module parses format strings for the compiler; the module
% library/string.parse_runtime.m does the same job for the runtime system.
% Any changes here, in the parts of this module below the code of
% flatten_components, will probably also require a corresponding change there.
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- module check_hlds.simplify.format_call.parse_string_format.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module string.parse_util.
:- import_module list.
:- import_module pair.

%----------------------------------------------------------------------------%

    % An abtract representation of a polytype, with the actual value
    % to be printed replaced by the variable that will hold that value
    % at runtime.
:- type abstract_poly_type
    --->    apt_f(prog_var)
    ;       apt_i(prog_var)
    ;       apt_s(prog_var)
    ;       apt_c(prog_var).

:- type compiler_format_maybe_width
    --->    compiler_no_specified_width
    ;       compiler_manifest_width(int)
    ;       compiler_var_width(prog_var).

:- type compiler_format_maybe_prec
    --->    compiler_no_specified_prec
    ;       compiler_manifest_prec(int)
    ;       compiler_var_prec(prog_var).

:- type flat_component
    --->    flat_string_const(string)
    ;       flat_format_char(
                string_format_flags,
                compiler_format_maybe_width,
                prog_var
            )
    ;       flat_format_string(
                string_format_flags,
                compiler_format_maybe_width,
                compiler_format_maybe_prec,
                prog_var
            )
    ;       flat_format_signed_int(
                string_format_flags,
                compiler_format_maybe_width,
                compiler_format_maybe_prec,
                prog_var
            )
    ;       flat_format_unsigned_int(
                string_format_flags,
                compiler_format_maybe_width,
                compiler_format_maybe_prec,
                string_format_int_base,
                prog_var
            )
    ;       flat_format_float(
                string_format_flags,
                compiler_format_maybe_width,
                compiler_format_maybe_prec,
                string_format_float_kind,
                prog_var
            ).

    % Parse the entire format string. Return either a list of things to be
    % formated and printed, or a list of error messages.
    %
:- pred parse_and_flatten_format_string(list(char)::in,
    list(abstract_poly_type)::in,
    maybe_error(list(flat_component),
        pair(string_format_error, list(string_format_error)))::out) is det.
% XXX Should we introduce a new type, nonempty_list? We already have the inst.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module integer.
:- import_module maybe.
:- import_module require.

%----------------------------------------------------------------------------%

parse_and_flatten_format_string(Chars, PolyTypes, MaybeFlatComponents) :-
    compiler_parse_format_string(Chars, PolyTypes, 1, Components, Errors),
    (
        Errors = [HeadError | TailErrors],
        MaybeFlatComponents = error(HeadError - TailErrors)
    ;
        Errors = [],
        flatten_components(Components, FlatComponents),
        MaybeFlatComponents = ok(FlatComponents)
    ).

    % Replace both
    %
    %   compiler_comp_str and
    %   compiler_comp_conv_spec(compiler_spec_percent)
    %
    % with flat_string_const, and concatenate together any adjacent
    % flat_string_consts. At runtime, we don't do this since the time
    % the time taken by flattening is probably greater than the time
    % saved by flattening, but at compile time, we spread the cost
    % over many executions.
    %
:- pred flatten_components(list(compiler_format_component)::in,
    list(flat_component)::out) is det.

flatten_components([], []).
flatten_components([HeadComponent | TailComponents], FlatComponents) :-
    flatten_components(TailComponents, TailFlatComponents),
    (
        HeadComponent = compiler_comp_str(StrConst),
        (
            TailFlatComponents =
                [flat_string_const(TailStrConst) | LaterFlatComponents]
        ->
            FlatComponents = 
                [flat_string_const(StrConst ++ TailStrConst) |
                    LaterFlatComponents]
        ;
            FlatComponents = 
                [flat_string_const(StrConst) | TailFlatComponents]
        )
    ;
        HeadComponent = compiler_comp_conv_spec(HeadSpec),
        (
            HeadSpec = compiler_spec_percent,
            (
                TailFlatComponents =
                    [flat_string_const(TailStrConst) | LaterFlatComponents]
            ->
                FlatComponents = 
                    [flat_string_const("%" ++ TailStrConst) |
                        LaterFlatComponents]
            ;
                FlatComponents = 
                    [flat_string_const("%") | TailFlatComponents]
            )
        ;
            HeadSpec = compiler_spec_char(Flags, MaybeWidth, Var),
            FlatComponents = 
                [flat_format_char(Flags, MaybeWidth, Var) | TailFlatComponents]
        ;
            HeadSpec = compiler_spec_string(Flags, MaybeWidth, MaybePrec, Var),
            FlatComponents = 
                [flat_format_string(Flags, MaybeWidth, MaybePrec, Var) |
                    TailFlatComponents]
        ;
            HeadSpec = compiler_spec_signed_int(Flags, MaybeWidth, MaybePrec,
                Var),
            FlatComponents = 
                [flat_format_signed_int(Flags, MaybeWidth, MaybePrec, Var) |
                    TailFlatComponents]
        ;
            HeadSpec = compiler_spec_unsigned_int(Flags, MaybeWidth, MaybePrec,
                Base, Var),
            FlatComponents = 
                [flat_format_unsigned_int(Flags, MaybeWidth, MaybePrec, Base,
                    Var) | TailFlatComponents]
        ;
            HeadSpec = compiler_spec_float(Flags, MaybeWidth, MaybePrec,
                Kind, Var),
            FlatComponents = 
                [flat_format_float(Flags, MaybeWidth, MaybePrec, Kind, Var) |
                    TailFlatComponents]
        )
    ).

:- type compiler_format_spec
    --->    compiler_spec_percent
    ;       compiler_spec_char(
                string_format_flags,
                compiler_format_maybe_width,
                prog_var
            )
    ;       compiler_spec_string(
                string_format_flags,
                compiler_format_maybe_width,
                compiler_format_maybe_prec,
                prog_var
            )
    ;       compiler_spec_signed_int(
                string_format_flags,
                compiler_format_maybe_width,
                compiler_format_maybe_prec,
                prog_var
            )
    ;       compiler_spec_unsigned_int(
                string_format_flags,
                compiler_format_maybe_width,
                compiler_format_maybe_prec,
                string_format_int_base,
                prog_var
            )
    ;       compiler_spec_float(
                string_format_flags,
                compiler_format_maybe_width,
                compiler_format_maybe_prec,
                string_format_float_kind,
                prog_var
            ).

:- type compiler_format_component
    --->    compiler_comp_str(string)
    ;       compiler_comp_conv_spec(compiler_format_spec).

    % This predicate parses the entire format string. When it encounters
    % something that looks like a conversion specification (i.e. it starts
    % with a '%' character), but which cannot be parsed as one, it records
    % an error message, and keeps going.
    %
    % Note that making this predicate use an accumulator for the lists
    % of components and errors seen so far would yield cleaner code,
    % but would probably be slower since our caller would have to unreverse
    % the list of components we return.
    %
    % The lack of tail recursion here should not be a problem, since no
    % format string will be long enough to make us consume too much stack.
    %
:- pred compiler_parse_format_string(list(char)::in,
    list(abstract_poly_type)::in, int::in,
    list(compiler_format_component)::out, list(string_format_error)::out)
    is det.

compiler_parse_format_string(!.Chars, !.PolyTypes, SpecNum,
        Components, Errors) :-
    gather_non_percent_chars(!.Chars, NonConversionSpecChars, GatherEndedBy),
    (
        GatherEndedBy = found_end_of_string,
        Components0 = [],
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
            Spec, SpecErrors),
        compiler_parse_format_string(!.Chars, !.PolyTypes, SpecNum + 1,
            ComponentsTail, ErrorsTail),
        (
            SpecErrors = [],
            ConvComponent = compiler_comp_conv_spec(Spec),
            Components0 = [ConvComponent | ComponentsTail],
            Errors = ErrorsTail
        ;
            SpecErrors = [_ | _],
            Components0 = ComponentsTail,
            Errors = SpecErrors ++ ErrorsTail
        )
    ),
    (
        NonConversionSpecChars = [],
        Components = Components0
    ;
        NonConversionSpecChars = [_ | _],
        NonConversionSpecString =
            string.from_char_list(NonConversionSpecChars),
        StringComponent = compiler_comp_str(NonConversionSpecString),
        Components = [StringComponent | Components0]
    ).

    % Each conversion specification starts with the character '%' (which
    % our caller has already read) and ends with a conversion specifier.
    % In between there may be (in this order) zero or more flags, an optional
    % minimum field width, and an optional precision.
    %
:- pred parse_conversion_specification(list(char)::in, list(char)::out,
    list(abstract_poly_type)::in, list(abstract_poly_type)::out, int::in,
    compiler_format_spec::out, list(string_format_error)::out) is det.

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
    list(abstract_poly_type)::in, list(abstract_poly_type)::out, int::in,
    compiler_format_maybe_width::out, list(string_format_error)::out) is det.

get_optional_width(!Chars, !PolyTypes, SpecNum, MaybeWidth, Errors) :-
    ( if !.Chars = ['*' | !:Chars] then
        (
            !.PolyTypes = [PolyType | !:PolyTypes],
            ( if PolyType = apt_i(PolyWidthVar) then
                MaybeWidth = compiler_var_width(PolyWidthVar),
                Errors = []
            else
                MaybeWidth = compiler_no_specified_width,
                Errors = [error_nonint_star_width(SpecNum,
                    abstract_poly_type_to_kind(PolyType))]
            )
        ;
            !.PolyTypes = [],
            MaybeWidth = compiler_no_specified_width,
            Errors = [error_missing_star_width(SpecNum)]
        )
    else if get_nonzero_number_prefix(!Chars, Width) then
        MaybeWidth = compiler_manifest_width(Width),
        Errors = []
    else
        MaybeWidth = compiler_no_specified_width,
        Errors = []
    ).

    % Do we have a precision? If yes, get it.
    %
:- pred get_optional_prec(list(char)::in, list(char)::out,
    list(abstract_poly_type)::in, list(abstract_poly_type)::out, int::in,
    compiler_format_maybe_prec::out, list(string_format_error)::out) is det.

get_optional_prec(!Chars, !PolyTypes, SpecNum, MaybePrec, Errors) :-
    ( if !.Chars = ['.' | !:Chars] then
        ( if !.Chars = ['*' | !:Chars] then
            (
                !.PolyTypes = [PolyType | !:PolyTypes],
                ( if PolyType = apt_i(PolyPrecVar) then
                    MaybePrec = compiler_var_prec(PolyPrecVar),
                    Errors = []
                else
                    MaybePrec = compiler_no_specified_prec,
                    Errors = [error_nonint_star_prec(SpecNum,
                        abstract_poly_type_to_kind(PolyType))]
                )
            ;
                !.PolyTypes = [],
                MaybePrec = compiler_no_specified_prec,
                Errors = [error_missing_star_prec(SpecNum)]
            )
        else
            % This treats an empty string as an EXPLICIT zero.
            get_number_prefix(!Chars, Prec),
            MaybePrec = compiler_manifest_prec(Prec),
            Errors = []
        )
    else
        MaybePrec = compiler_no_specified_prec,
        Errors = []
    ).

%--------------------------------------------------------------------------%

    % get_first_spec(!Chars, !PolyTypes, Flags, MaybeWidth, MaybePrec,
    %   SpecNum, Spec, Errors):
    %
    % Try to read one conversion specifier, whose percent sign, flags,
    % width and precision have already been read, from !Chars.
    %
    % If successful, consume the corresponding abstract_poly_type
    % from !PolyTypes, we return the specifier as Spec and return
    % an empty error list.
    %
    % If there is a problem, we return a garbage Spec and a nonempty
    % errors list. We also consume the abstract_poly_type that corresponds
    % (or at least, looks like it corresponds) to the specifier,
    % if there is one.
    %
:- pred get_first_spec(list(char)::in, list(char)::out,
    list(abstract_poly_type)::in, list(abstract_poly_type)::out,
    string_format_flags::in, compiler_format_maybe_width::in,
    compiler_format_maybe_prec::in, int::in,
    compiler_format_spec::out, list(string_format_error)::out) is det.

get_first_spec(!Chars, !PolyTypes, _Flags, _MaybeWidth, _MaybePrec, SpecNum,
        Spec, Errors) :-
    !.Chars = [],
    Spec = compiler_spec_percent,
    Errors = [error_no_specifier(SpecNum, list.length(!.PolyTypes))].
get_first_spec(!Chars, !PolyTypes, !.Flags, MaybeWidth, MaybePrec, SpecNum,
        Spec, Errors) :-
    !.Chars = [SpecChar | !:Chars],
    ( if
        (
            SpecChar = '%',
            SpecPrime = compiler_spec_percent,
            ErrorsPrime = []
        ;
            ( SpecChar = 'd'
            ; SpecChar = 'i'
            ),
            require_det
            (
                !.PolyTypes = [SpecPolyType | !:PolyTypes],
                ( if SpecPolyType = apt_i(IntVar) then
                    % Base is always decimal
                    SpecPrime = compiler_spec_signed_int(!.Flags,
                        MaybeWidth, MaybePrec, IntVar),
                    ErrorsPrime = []
                else
                    Error = error_wrong_polytype(SpecNum, SpecChar,
                        abstract_poly_type_to_kind(SpecPolyType)),
                    SpecPrime = compiler_spec_percent,
                    ErrorsPrime = [Error]
                )
            ;
                !.PolyTypes = [],
                Error = error_no_polytype(SpecNum, SpecChar),
                SpecPrime = compiler_spec_percent,
                ErrorsPrime = [Error]
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
            require_det
            (
                !.PolyTypes = [SpecPolyType | !:PolyTypes],
                ( if SpecPolyType = apt_i(IntVar) then
                    SpecPrime = compiler_spec_unsigned_int(!.Flags,
                        MaybeWidth, MaybePrec, Base, IntVar),
                    ErrorsPrime = []
                else
                    Error = error_wrong_polytype(SpecNum, SpecChar,
                        abstract_poly_type_to_kind(SpecPolyType)),
                    SpecPrime = compiler_spec_percent,
                    ErrorsPrime = [Error]
                )
            ;
                !.PolyTypes = [],
                Error = error_no_polytype(SpecNum, SpecChar),
                SpecPrime = compiler_spec_percent,
                ErrorsPrime = [Error]
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
            require_det
            (
                !.PolyTypes = [SpecPolyType | !:PolyTypes],
                ( if SpecPolyType = apt_f(FloatVar) then
                    SpecPrime = compiler_spec_float(!.Flags,
                        MaybeWidth, MaybePrec, FloatKind, FloatVar),
                    ErrorsPrime = []
                else
                    Error = error_wrong_polytype(SpecNum, SpecChar,
                        abstract_poly_type_to_kind(SpecPolyType)),
                    SpecPrime = compiler_spec_percent,
                    ErrorsPrime = [Error]
                )
            ;
                !.PolyTypes = [],
                Error = error_no_polytype(SpecNum, SpecChar),
                SpecPrime = compiler_spec_percent,
                ErrorsPrime = [Error]
            )
        ;
            SpecChar = 'c',
            require_det
            (
                !.PolyTypes = [SpecPolyType | !:PolyTypes],
                ( if SpecPolyType = apt_c(CharVar) then
                    % XXX Should we generate an error if MaybePrec = yes(...)?
                    SpecPrime = compiler_spec_char(!.Flags,
                        MaybeWidth, CharVar),
                    ErrorsPrime = []
                else
                    Error = error_wrong_polytype(SpecNum, SpecChar,
                        abstract_poly_type_to_kind(SpecPolyType)),
                    SpecPrime = compiler_spec_percent,
                    ErrorsPrime = [Error]
                )
            ;
                !.PolyTypes = [],
                Error = error_no_polytype(SpecNum, SpecChar),
                SpecPrime = compiler_spec_percent,
                ErrorsPrime = [Error]
            )
        ;
            SpecChar = 's',
            require_det
            (
                !.PolyTypes = [SpecPolyType | !:PolyTypes],
                ( if SpecPolyType = apt_s(StrVar) then
                    SpecPrime = compiler_spec_string(!.Flags,
                        MaybeWidth, MaybePrec, StrVar),
                    ErrorsPrime = []
                else
                    Error = error_wrong_polytype(SpecNum, SpecChar,
                        abstract_poly_type_to_kind(SpecPolyType)),
                    SpecPrime = compiler_spec_percent,
                    ErrorsPrime = [Error]
                )
            ;
                !.PolyTypes = [],
                Error = error_no_polytype(SpecNum, SpecChar),
                SpecPrime = compiler_spec_percent,
                ErrorsPrime = [Error]
            )
        )
    then
        Spec = SpecPrime,
        Errors = ErrorsPrime
    else
        Error = error_unknown_specifier(SpecNum, SpecChar),
        Spec = compiler_spec_percent,
        Errors = [Error]
    ).

%----------------------------------------------------------------------------%

:- func abstract_poly_type_to_kind(abstract_poly_type) = poly_kind.

abstract_poly_type_to_kind(apt_c(_)) = poly_kind_char.
abstract_poly_type_to_kind(apt_s(_)) = poly_kind_str.
abstract_poly_type_to_kind(apt_i(_)) = poly_kind_int.
abstract_poly_type_to_kind(apt_f(_)) = poly_kind_float.

%----------------------------------------------------------------------------%
