%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A module similar to string, with only a format exported function.
% Prints everything out in uppercase.

:- module qual_adv_test_helper_1.

:- interface.
:- import_module list.
:- import_module string.

:- pred format(string, list(poly_type), string).
:- mode format(in, in, out) is det.

:- func format_func(string, list(poly_type)) = string.
:- mode format_func(in, in) = out is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module require.
:- import_module std_util.

format_func(FString, PolyList) = String :-
    qual_adv_test_helper_1.format(FString, PolyList, String).

format(Fstring, Poly_list, Ostring) :-
    to_char_list(Fstring, Clist),
    format_2q(Clist, Poly_list, Ostring) .

    % format_2q(stream, char_f, vars, IO, IO).
    % The main function, with different input types.
    %
    % Accumulator recursion is not used, as it would involve adding a
    % short string to the end of a long string many times, which I understand
    % is not efficient. Instead, many short strings are added to the front
    % of a (long) and growing string.
    %
:- pred format_2q(list(char), list(poly_type), string).
:- mode format_2q(in, in, out) is det.

format_2q([], _, "").
format_2q([Achar | As], Vars_in, Ostring) :-
    ( if Achar = '%' then
        ( if As = ['%' | Ass] then
            format_2q(Ass, Vars_in, Temp_out),
            first_char(Ostring, '%', Temp_out)
        else
            ( if
                format_top_convert_variable(As, As_out,
                    Vars_in, Vars_out, String_1)
            then
                format_2q(As_out, Vars_out, String_2),
                append(String_1, String_2, Ostring)
            else
                error("format: Too few variables.")
            )
        )
    else
        format_2q(As, Vars_in, Temp_out),
        first_char(Ostring, Bchar, Temp_out),
        char.to_upper(Achar, Bchar)
    ).

    % format_top_convert_variable(formated string in, out, var in, out,
    %    Out string)
    % Return a string of the formatted variable.
    %
:- pred format_top_convert_variable(list(char), list(char),
    list(poly_type), list(poly_type), string).
:- mode format_top_convert_variable(in, out, in, out, out) is semidet.

format_top_convert_variable(['%' | Bs], Bs, [], [], "%").
        % Above rule should not be executed... defensive rule.
format_top_convert_variable(F_chars_in, F_chars_out,
            [Var_in | Vars_l_in], Vars_out, Out_string) :-
    format_takewhile1(F_chars_in, [Conv_char_0 | F_chars_out], Fmt_info),
    % Separate formatting info from formatting string.
    % in, out, out
    format_get_optional_args(Fmt_info, Flags, Int_width,
        Int_precis, Conv_modify),
    % Parse formatting info.
    % in, out, out, out, out.
    format_mod_conv_char(Precision, Var_in, Conv_char_1, Conv_char_0),
    % Modify(?) conversion character.
    % in, in, out, in
    format_do_mod_char(Conv_modify, Conv_char_2, Conv_char_1),
    % Interperate input conversion modifiers.
    % in, out, in
    format_read_star(Vars_out, Vars_l_in, Width, Int_width,
        Precision, Int_precis),
    % Do something if a precision or width was '*'
    % out, in, out, in, out, in
    format_do_conversion(Conv_char_2, Var_in, Ostring, Precision,
        Flags, Move_i0),
    %  Actually convert a Variable to a string
    %  in, out, in, in, out, in, in, out
    format_add_sign(Ostring2, Ostring, Flags, Var_in, Move_i0, Move_i1),
    %  Adds an optional '+' or ' ' to string.
    %  out, in, in, in, in, out
    format_pad_width(Ostring2, Width, Flags, Out_string, Move_i1).
    %  Ensures that the string is at least width.
    %  in, in, in, out, in

    % Change conversion character.
    %
    % Ideally the outer "->" symbols could be removed, the last case given
    % a guard, and the compiler accept this as det, rather than non-det.
    %
:- pred format_mod_conv_char(int, poly_type, char, char).
:- mode format_mod_conv_char(in, in, out, in) is det.

format_mod_conv_char(Precision, Poly_var, Conv_c_out, Conv_c_in) :-
    ( if Conv_c_in = 'i' then
        Conv_c_out = 'd'            % %d = %i
    else if Conv_c_in = 'g' then            % %g is either %e of %f
        ( if Poly_var = f(F) then
            float_abs(F, Ft),
            int.pow(10, Precision, P),
            Pe = float.float(P),
            ( if
                Ft > 0.0001,
                Pe > Ft
            then
                Conv_c_out = 'f'
            else
                Conv_c_out = 'e'
            )
        else
            error("format:  %g without a f(Float).")
        )
     else if Conv_c_in = 'G' then           % %G is either %E of %f
        ( if Poly_var = f(F) then
            float_abs(F, Ft),
            int.pow(10, Precision, P),
            Pe = float.float(P),
            ( if
                Ft > 0.0001,
                Pe > Ft
            then
                Conv_c_out = 'f'
            else
                Conv_c_out = 'E'
            )
        else
            error("format:  %G without a f(float).")
        )
    else
        Conv_c_out = Conv_c_in
    ).

    % This function glances at the input-modification flags, only applicable
    % with a more complicated type system
    %
    % Another function that would be better off as a switch.
    %
:- pred format_do_mod_char(char, char, char).
:- mode format_do_mod_char(in, out, in) is det.

format_do_mod_char(Char_mods, C_out, C_in) :-
    ( if Char_mods = 'h' then
        C_out = C_in
    else if Char_mods = 'l' then
        C_out = C_in
    else if Char_mods = 'L' then
        C_out = C_in
    else
        C_out = C_in
    ).

    % Change Width or Precision value, if '*' was spcified.
    %
:- pred format_read_star(list(poly_type), list(poly_type), int, int, int, int).
:- mode format_read_star(out, in, out, in, out, in) is semidet.

format_read_star(Polys_out, Polys_in, Width, Int_width,
        Precision, Int_precis) :-
    ( if special_precision_and_width(Int_width) then
        Polys_in = [ i(Width) |  Poly_temp]
    else
        Polys_in = Poly_temp,
        Int_width = Width
    ),
    ( if special_precision_and_width(Int_precis) then
        Poly_temp = [ i(Precision) | Polys_out]
    else
        Polys_out = Poly_temp,
        Int_precis = Precision
    ).

    % This function did the variable conversion to string.
    % Now done by do_conversion_0/6.
    %
    %
    % Mv_width records the length of the prefix in front of the number,
    % so that it is more easy to insert width and precision padding and
    % optional signs, in the correct place.
    %
:- pred format_do_conversion(char, poly_type, string, int, list(char), int).
:- mode format_do_conversion(in, in, out, in, in, out) is det.

format_do_conversion(Conv_c, Poly_t, Ostring, Precision, Flags, Mv_width) :-
    ( if
        do_conversion_0(Conv_c, Poly_t, Tstring, Precision,
            Flags, TMv_width)
    then
        TMv_width = Mv_width,
        Ostring = Tstring
    else
        do_conversion_fail(Conv_c)
    ).

:- pred do_conversion_0(char, poly_type, string, int, list(char), int).
:- mode do_conversion_0(in, in, out, in, in, out) is semidet.

do_conversion_0(Conv_c, Poly_t, Ostring, Precision, Flags, Mv_width) :-
    (
        Conv_c = 'd',
        Poly_t = i(I),
        int_to_string(I, S),
        format_int_precision(S, Ostring, Precision, _),
        ( if I < 0 then
            Mv_width = 1
        else
            Mv_width = 0
        )
    ;
        Conv_c = 'o',
        Poly_t = i(I),
        ( if I = 0 then
            S = "0",
            format_int_precision(S, Ostring, Precision, _),
            Pfix_len = 0
        else
            int_to_base_string(I, 8, S),
            format_int_precision(S, SS, Precision, _),
            ( if list.member('#', Flags) then
                first_char(Ostring, '0', SS),
                Pfix_len = 1
            else
                Ostring = SS,
                Pfix_len = 0
            )
        ),
        ( if I < 0 then Mv_width = Pfix_len + 1 else Mv_width = Pfix_len )
    ;
        Conv_c = 'x' ,
        Poly_t = i(I),
        ( if I = 0 then
            SS = "0",
            Pfix_len = 0,
            format_int_precision(SS, Ostring, Precision, _)
        else
            int_to_base_string(I, 16, S),
            format_int_precision(S, SS, Precision, _),
            ( if list.member('#', Flags) then
                append("0x", SS, Ostring),
                Pfix_len = 2
            else
                Ostring = SS,
                Pfix_len = 0
            )
        ),
        ( if I < 0 then Mv_width = Pfix_len + 1 else Mv_width = Pfix_len )
    ;
        Conv_c = 'X',
        Poly_t = i(I),
        ( if I = 0 then
            SS = "0",
            Pfix_len = 0,
            format_int_precision(SS, Ostring, Precision, _)
        else
            int_to_base_string(I, 16, Otemp),
            to_upper(Otemp, S),
            format_int_precision(S, SS, Precision, _),
            ( if list.member('#', Flags) then
                append("0X", SS, Ostring),
                Pfix_len = 2
            else
                SS = Ostring,
                Pfix_len = 0
            )
        ),
        ( if I < 0 then Mv_width = Pfix_len + 1 else Mv_width = Pfix_len )
    ;
        Conv_c = 'u' ,
        Poly_t = i(I),
        int.abs(I, J),
        int_to_string(J, S),
        format_int_precision(S, Ostring, Precision, Mvt),
        Mv_width = Mvt
    ;
        Conv_c = 'c' ,
        Poly_t = c(C),
        char_to_string(C, Ostring),
        Mv_width = 0
    ;
        Conv_c = 's' ,
        Poly_t = s(S),
        ( if default_precision_and_width(Precision) then
            to_upper(S, T),
            T = Ostring
        else
            split(S, Precision, Ostring, _)
        ),
        Mv_width = 0
    ;
        Conv_c = 'f' ,
        Poly_t = f(F),
        float_to_string(F, Fstring),
        format_calc_prec(Fstring, Ostring, Precision),
        ( if F < 0.0 then Mv_width = 1 else Mv_width = 0 )
    ;
        Conv_c = 'e',
        Poly_t = f(F),
        format_calc_exp(F, Ostring, Precision, 0),
        ( if F < 0.0 then Mv_width = 1 else Mv_width = 0 )
    ;
        Conv_c = 'E' ,
        Poly_t = f(F),
        format_calc_exp(F, Otemp, Precision, 0),
        to_upper(Otemp, Ostring),
        ( if F < 0.0 then Mv_width = 1 else Mv_width = 0 )
    ;
        Conv_c = 'p' ,
        Poly_t = i(I),
        int_to_string(I, Ostring),
        ( if (I < 0) then Mv_width = 1 else Mv_width = 0 )
    ).

:- pred do_conversion_fail(char).
:- mode do_conversion_fail(in) is erroneous.

do_conversion_fail(Conv_c) :-
    qual_adv_test_helper_1.format("%s `%%%c', without a correct poly-variable.",
        [s("format: statement has used type"), c(Conv_c)],
        Error_message),
    error(Error_message).

    % Use precision information to modify string.  - for integers
    %
:- pred format_int_precision(string, string, int, int).
:- mode format_int_precision(in, out, in, out) is semidet.

format_int_precision(S, Ostring, Precision, Added_width) :-
    ( if default_precision_and_width(Precision) then
        Prec = 0
    else
        Prec = Precision
    ),
    length(S, L),
    ( if first_char(S, '-', _) then
        Xzeros = Prec - L + 1
    else
        Xzeros = Prec - L
    ),
    Added_width = Xzeros,
    ( if Xzeros > 0 then
        duplicate_char('0', Xzeros, Pfix),
        first_char(S, C, Rest),
        ( if
            C \= ('-'),
            C \= ('+')
        then
            append(Pfix, S, Ostring)
        else
            append(Pfix, Rest, Temps),
            first_char(Ostring, C, Temps)
        )
    else
        Ostring = S
    ).

    % Function to calculate exponent for a %e conversion of a float.
    %
:- pred format_calc_exp(float, string, int, int).
:- mode format_calc_exp(in, out, in, in) is det.

format_calc_exp(F, Fstring, Precision, Exp) :-
    ( if F < 0.0 then
        Tf = 0.0 - F,
        format_calc_exp(Tf, Tst, Precision, Exp),
        first_char(Fstring, '-', Tst)
    else
        ( if F < 1.0 then
            Texp = Exp - 1,
            FF = F * 10.0,
            format_calc_exp(FF, Fstring, Precision, Texp)
        else if F >= 10.0 then
            Texp = Exp + 1,
            FF = F / 10.0,
            format_calc_exp(FF, Fstring, Precision, Texp)
        else
            float_to_string(F, Fs),
            format_calc_prec(Fs, Fs2, Precision),
            int_to_string(Exp, Exps),
            ( if Exp < 0 then
                append("e", Exps, TFstring),
                append(Fs2, TFstring, Fstring)
            else
                append("e+", Exps, TFstring),
                append(Fs2, TFstring, Fstring)
            )
        )
    ).

    % This precision output-modification predicate handles floats.
    %
:- pred format_calc_prec(string, string, int).
:- mode format_calc_prec(in, out, in) is det.

format_calc_prec(Istring, Ostring, Precision) :-
    ( if default_precision_and_width(Precision) then
        Prec = 6
    else
        Prec = Precision
    ),
    ( if find_index(Istring, '.', Index) then
        Spa = Prec + Index
    else
        length(Istring, Spa_0),
        Spa = Spa_0 + 1
        %  This branch should never be called if mercury is implemented
        %  in ansi-C, according to Kernighan and Ritchie p244, as a
        %  float converted to a string using sprintf should always have
        %  a decimal point.  (where specified precision != 0.
        %  float_to_string doesn't specify a precision to be
        %  used.)  If a future implementation changes the
        %  way float_to_string is implemented, and a float can
        %  be converted to a string without a decimal point, then this
        %  rule would be useful.  It is not expected that
        %  float_to_string will ever produce %e style output.
    ),
    ( if
        length(Istring, L1),
        L1 < Spa
    then
        duplicate_char('0', Precision, P0s),
        append(Istring, P0s, Mstring)
    else
        Mstring = Istring
    ),
    ( if Precision = 0 then
        Space = Spa - 1
    else
        Space = Spa
    ),
    split(Mstring, Space, Ostring, _).

    % find_index is a funky little predicate to find the first
    % occurrence of a particular character in a string.
    %
:- pred find_index(string, char, int).
:- mode find_index(in, in, out) is semidet.

find_index(Str, C, Index) :-
    to_char_list(Str, List),
    find_index_2(List, C, Index).

:- pred find_index_2(list(char), char, int).
:- mode find_index_2(in, in, out) is semidet.

find_index_2([], _C, _Index) :- fail.
find_index_2([X | Xs], C, Index) :-
    ( if X = C then
        Index = 1
    else
        find_index_2(Xs, C, Index0),
        Index = Index0 + 1
    ).

    % Add a '+' or ' ' sign, if it is needed in this output.
    %
:- pred format_add_sign(string, string, list(char), poly_type, int, int).
:- mode format_add_sign(out, in, in, in, in, out) is det.

format_add_sign(Ostring, Istring, Flags, _V, Mvw1, Mvw2) :-
    T1 = Mvw1 - 1,
    ( if index(Istring, T1, '-') then
        Ostring = Istring,
        Mvw2 = Mvw1
    else
        split(Istring, Mvw1, Lstring, Rstring),
        ( if list.member(('+'), Flags) then
            append("+", Rstring, Astring),
            append(Lstring, Astring, Ostring),
            Mvw2 = Mvw1 + 1
        else
            ( if list.member(' ', Flags) then
                append(" ", Rstring, Astring),
                append(Lstring, Astring, Ostring),
                Mvw2 = Mvw1 + 1
            else
                Ostring = Istring,
                Mvw2 = Mvw1
            )
        )
    ).

    % This function pads some characters to the left or right of a string
    % that is shorter than its width.
    %
:- pred format_pad_width(string, int, list(char), string, int).
:- mode format_pad_width(in, in, in, out, in) is det.

format_pad_width(Istring, Width, Flags, Out_string, Mv_cs) :-
    length(Istring, Len),
    ( if Len < Width then
        % time for some FLAG tests
        Xspace = Width - Len,
        ( if list.member('0', Flags) then
            Pad_char = '0'
        else
            Pad_char = ' '
        ),
        duplicate_char(Pad_char, Xspace, Pad_string),
        ( if list.member('-', Flags) then
            append(Istring, Pad_string, Out_string)
        else
            ( if list.member('0', Flags) then
                split(Istring, Mv_cs, B4, After),
                append(Pad_string, After, Astring),
                append(B4, Astring, Out_string)
            else
                append(Pad_string, Istring, Out_string)
            )
        )
    else
        Out_string = Istring
    ).

    % format_get_optional_args(format info, flags, width, precision, modifier)
    % format is assumed to be in ANSI C format.
    % p243-4 of Kernighan & Ritchie 2nd Ed. 1988
    %
    % A function to do some basic parsing on the optional printf arguments.
    %
    % The ites make this det. It would be nicer to see a det switch on A,
    % but the determinism checker does not `see' the equity tests
    % that are hidden one layer further down.
    %
:- pred format_get_optional_args(list(char), list(char), int, int, char).
:- mode format_get_optional_args(in, out, out, out, out) is det.

format_get_optional_args([], Flags, Width, Precision, Mods) :-
    Flags = [],
    Width = 0,
    default_precision_and_width(Precision),
    Mods = ' '.
format_get_optional_args([A | As], Flags, Width, Precision, Mods) :-
    ( if
        ( A = (-) ; A = (+) ; A = ' ' ; A = '0' ; A = '#' )
    then
        format_get_optional_args(As, Oflags, Width, Precision, Mods),
        UFlags = [A | Oflags],
        list.sort_and_remove_dups(UFlags, Flags)
    else if
        ( A = (.) ; A = '1' ; A = '2' ; A = '3' ; A = '4' ;
          A = '5' ; A = '6' ; A = '7' ; A = '8' ; A = '9' )
    then
        format_string_to_ints([A | As], Bs, Numl1, Numl2, yes),
        format_int_from_char_list(Numl1, Width),
        format_int_from_char_list(Numl2, Prec),
        format_get_optional_args(Bs, Flags, _, Ptemp, Mods),
        ( if Numl2 = [] then
            Precision = Ptemp
        else
            Precision = Prec
        )
    else if
        ( A = 'h' ; A = 'l' ; A = 'L' )
    then
        Mods = A,
        format_get_optional_args(As, Flags, Width, Precision, _)
    else if
        A = ('*')
    then
        format_get_optional_args(As, Flags, W, P, Mods),
        ( if As = [(.) | _] then
            Precision = P,
            special_precision_and_width(Width)
        else
            Width = W,
            special_precision_and_width(Precision)
        )
%       ( if
%           default_precision_and_width(P)
%       then
%           special_precision_and_width(Precision)
%       else
%           Precision = P
%       ),
%       special_precision_and_width(Width)
    else
        error("format:  Unrecognised formatting information\n")
    ).

:- pred format_takewhile1(list(char), list(char), list(char)).
:- mode format_takewhile1(in, out, out) is det.

format_takewhile1([], [], []).
format_takewhile1([A | As], Rem, Finf) :-
    ( if
        ( A = 'd' ; A = 'i' ; A = 'o' ; A = 'x' ; A = 'X' ; A = 'u' ;
          A = 's' ; A = '%' ; A = 'c' ; A = 'f' ; A = 'e' ; A = 'E' ;
          A = 'g' ; A = 'G' ; A = 'p' )
    then
        Rem = [A | As],
        Finf = []
    else
        format_takewhile1(As, Rem, F),
        Finf = [A | F]
    ).

    % (String in, out, Number1, Number2, seen '.' yet?)
    % Takes in a char list and splits off the rational number at the
    % start of the list. This is split into 2 parts - an int and a fraction.
    %
:- pred format_string_to_ints(list(char), list(char), list(char), list(char),
    bool).
:- mode format_string_to_ints(in, out, out, out, in) is det.

format_string_to_ints([], [], [], [], _).
format_string_to_ints([A | As], Bs, Int1, Int2, Bool) :-
    ( if char.is_digit(A) then
        ( if Bool = yes then
            format_string_to_ints(As, Bs, I1, Int2, yes),
            Int1 = [A | I1]
        else
            format_string_to_ints(As, Bs, Int1, I2, no),
            Int2 = [A | I2]
        )
    else
        ( if A = ('.') then
            format_string_to_ints(As, Bs, Int1, Int2, no)
        else
            Bs = [A | As],
            Int1 = [],
            Int2 = []
        )
    ).

    % Convert a char_list to an int.
    %
:- pred format_int_from_char_list(list(char), int).
:- mode format_int_from_char_list(in, out) is det.

format_int_from_char_list([], 0).
format_int_from_char_list([L | Ls], I) :-
    ( if
        from_char_list([L | Ls], S),
        to_int(S, I_0)
    then
        I = I_0
    else
        I = 0
    ).

:- pred float_abs(float, float).
:- mode float_abs(in, out) is det.

float_abs(Fin, Fout) :-
    ( if Fin < 0.0 then
        Fout = 0.0 - Fin
    else
        Fout = Fin
    ).

:- pred default_precision_and_width(int).
:- mode default_precision_and_width(out) is det.

default_precision_and_width(-6).

:- pred special_precision_and_width(int).
:- mode special_precision_and_width(out) is det.

special_precision_and_width(-1).

%---------------------------------------------------------------------------%
