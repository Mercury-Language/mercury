%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

% This module defines the GML parser, lexer, and the parse tree
% data structure.

:- module gml.
:- interface.

:- import_module vector.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%
%
% Lexer
%

:- type basic_token
    --->    '['
    ;       ']'
    ;       '{'
    ;       '}'
    ;       token(token).

:- type token
    --->    operator(operator)
    ;       identifier(string)
    ;       binder(string)
    ;       boolean(bool)
    ;       number(number)
    ;       string(string)

            % Not part of the spec; these are extra operators
            % which make interpretation more efficient.
    ;       extra(extra_operator).

:- type number
    --->    integer(int)
    ;       real(float).

:- type operator
    --->    acos
    ;       addi
    ;       addf
    ;       apply
    ;       asin
    ;       clampf
    ;       cone            % Tier-2
    ;       cos
    ;       cube            % Tier-2
    ;       cylinder        % Tier-2
    ;       difference      % Tier-3
    ;       divi
    ;       divf
    ;       eqi
    ;       eqf
    ;       floor
    ;       frac
    ;       get
    ;       getx
    ;       gety
    ;       getz
    ;       (if)
    ;       intersect       % Tier-3
    ;       length
    ;       lessi
    ;       lessf
    ;       light
    ;       modi
    ;       muli
    ;       mulf
    ;       negi
    ;       negf
    ;       plane
    ;       point
    ;       pointlight      % Tier-2
    ;       real
    ;       render
    ;       rotatex
    ;       rotatey
    ;       rotatez
    ;       scale
    ;       sin
    ;       sphere
    ;       spotlight       % Tier-3
    ;       sqrt
    ;       subi
    ;       subf
    ;       translate
    ;       union
    ;       uscale.

    % New operators which are not defined as part of the spec.
    % They can only be introduced by an optimization phase, and
    % exist to make interpretation more efficient.
:- import_module eval.

:- type extra_operator
    --->    popn(int)       % discard top n elements of stack
    ;       dup             % duplicate the topmost element
    ;       constant_sphere(
                surface_properties
            )
    ;       constant_plane(
                surface_properties
            )
    ;       constant_cone(
                surface_properties
            )
    ;       constant_cube(
                surface_properties
            )
    ;       constant_cylinder(
                surface_properties
            )
    ;       constant_point(
                point
            )
                % an `if' whose arms are just constants
    ;       constant_if(
                value,
                value
            ).
% XXX this is not used anywhere and gets in the way of parallelisation --pw
%   ;       mercury_closure(pred(env, env, stack, stack, io, io)).

:- inst extra_operator_inst for extra_operator/0 ==
    bound(  popn(ground)
    ;       dup
    ;       constant_sphere(ground)
    ;       constant_plane(ground)
    ;       constant_cone(ground)
    ;       constant_cube(ground)
    ;       constant_cylinder(ground)
    ;       constant_point(ground)
    ;       constant_if(ground, ground)
%   ;       mercury_closure(pred(in, out, in, out, di, uo) is det)
    ).

    % throws an exception if it gets an invalid token or I/O error
:- pred tokenize(list(basic_token)::out, io::di, io::uo) is det.

:- type lexer_error
    --->    lexer_error(int, string).

:- type parse_error
    --->    parse_error(string).

%-----------------------------------------------------------------------------%
%
% Parser
%

:- type gml_program == token_list.

:- type token_list == list(token_group).

:- type token_group
    --->    single_token(token)
    ;       function(token_list)
    ;       array(token_list).

    % Throws an exception if it gets a parse error.
    %
:- pred parse(list(basic_token)::in, gml_program::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module exception.
:- import_module require.
:- import_module string.

tokenize(Tokens, !IO) :-
    tokenize_2([], RevTokens, !IO),
    list.reverse(RevTokens, Tokens).

:- pred tokenize_2(list(basic_token)::in, list(basic_token)::out,
    io::di, io::uo) is det.

tokenize_2(Tokens0, Tokens, !IO) :-
    skip_whitespace(FirstCharResult, !IO),
    (
        FirstCharResult = ok(FirstChar),
        get_token(FirstChar, Token, !IO),
        tokenize_2([Token | Tokens0], Tokens, !IO)
    ;
        FirstCharResult = eof,
        Tokens = Tokens0
    ;
        FirstCharResult = error(Error),
        lexer_io_error(Error, !IO)
    ).

:- pred skip_whitespace(io.result(char)::out, io::di, io::uo) is det.

skip_whitespace(FirstCharResult, !IO) :-
    lexer_read_char(CharResult0, !IO),
    (
        CharResult0 = ok(FirstChar),
        ( FirstChar = '%' ->
            skip_to_end_of_line(!IO),
            skip_whitespace(FirstCharResult, !IO)
        ; lexer_is_whitespace(FirstChar) ->
            skip_whitespace(FirstCharResult, !IO)
        ;
            FirstCharResult = CharResult0
        )
    ;
        CharResult0 = eof,
        FirstCharResult = eof
    ).

:- pred lexer_is_whitespace(char::in) is semidet.

lexer_is_whitespace(' ').
lexer_is_whitespace('\t').
lexer_is_whitespace('\n').
lexer_is_whitespace('\r').
lexer_is_whitespace('\f').
lexer_is_whitespace('\v').

:- pred skip_to_end_of_line(io::di, io::uo) is det.

skip_to_end_of_line(!IO) :-
    lexer_read_char(CharResult, !IO),
    (
        CharResult = ok(Char),
        ( Char = '\n' ->
            true
        ; Char = '\v' ->
            true
        ; Char = '\f' ->
            true
        ; Char = '\r' ->
            true
        ;
            skip_to_end_of_line(!IO)
        )
    ;
        CharResult = eof
    ).

:- pred get_token(char::in, basic_token::out, io::di, io::uo) is det.

get_token(Char, Token, !IO) :-
    ( special_token(Char, Token0) ->
        Token = Token0
    ; Char = '"' ->
        get_string([], String, !IO),
        Token = token(string(String))
    ; char.is_alpha(Char) ->
        get_identifier([Char], Identifier, !IO),
        ( Identifier = "true" ->
            Token = token(boolean(yes))
        ; Identifier = "false" ->
            Token = token(boolean(no))
        ; is_operator(Identifier, Operator) ->
            Token = token(operator(Operator))
        ;
            Token = token(identifier(Identifier))
        )
    ; Char = ('/') ->
        get_identifier([], Identifier, !IO),
        (
            ( is_operator(Identifier, _)
            ; Identifier = "true"
            ; Identifier = "false"
            )
        ->
            error_rebind_operator(Identifier, !IO)
        ;
            true
        ),
        Token = token(binder(Identifier))
    ; Char = ('-') ->
        get_number([Char], Num, !IO),
        Token = token(number(Num))
    ; char.is_digit(Char) ->
        get_number([Char], Num, !IO),
        Token = token(number(Num))
    ;
        lexer_unexpected_char(Char, "start of token", !IO)
    ).

:- pred special_token(char::in, basic_token::out) is semidet.

special_token('[', '[').
special_token(']', ']').
special_token('{', '{').
special_token('}', '}').

:- pred is_printable(char::in) is semidet.

is_printable(Char) :-
    ( char.is_alnum_or_underscore(Char)
    ; is_printable_2(Char)
    ).

:- pred is_printable_2(char::in) is semidet.

is_printable_2(('!')).
is_printable_2('"').
is_printable_2('#').
is_printable_2('$').
is_printable_2('%').
is_printable_2('&').
is_printable_2(('''')).
is_printable_2('(').
is_printable_2(')').
is_printable_2(('*')).
is_printable_2(('+')).
is_printable_2(',').
is_printable_2(('-')).
is_printable_2('.').
is_printable_2(('/')).

is_printable_2((':')).
is_printable_2((';')).
is_printable_2(('<')).
is_printable_2(('=')).
is_printable_2(('>')).
is_printable_2('?').
is_printable_2('@').

is_printable_2(('[')).
is_printable_2(('\\')).
is_printable_2((']')).
is_printable_2('^').
    % _ should be picked up already, but it's still printable
is_printable_2('_').
is_printable_2('`').
is_printable_2(('{')).
is_printable_2(('|')).
is_printable_2(('}')).
is_printable_2('~').
is_printable_2(' ').

:- pred is_operator(string, operator).
:- mode is_operator(in, out) is semidet.
:- mode is_operator(out, in) is det.

is_operator("acos", acos).
is_operator("addi", addi).
is_operator("addf", addf).
is_operator("apply", apply).
is_operator("asin", asin).
is_operator("clampf", clampf).
is_operator("cone", cone).
is_operator("cos", cos).
is_operator("cube", cube).
is_operator("cylinder", cylinder).
is_operator("difference", difference).
is_operator("divi", divi).
is_operator("divf", divf).
is_operator("eqi", eqi).
is_operator("eqf", eqf).
is_operator("floor", floor).
is_operator("frac", frac).
is_operator("get", get).
is_operator("getx", getx).
is_operator("gety", gety).
is_operator("getz", getz).
is_operator("if", (if)).
is_operator("intersect", intersect).
is_operator("length", length).
is_operator("lessi", lessi).
is_operator("lessf", lessf).
is_operator("light", light).
is_operator("modi", modi).
is_operator("muli", muli).
is_operator("mulf", mulf).
is_operator("negi", negi).
is_operator("negf", negf).
is_operator("plane", plane).
is_operator("point", point).
is_operator("pointlight", pointlight).
is_operator("real", real).
is_operator("render", render).
is_operator("rotatex", rotatex).
is_operator("rotatey", rotatey).
is_operator("rotatez", rotatez).
is_operator("scale", scale).
is_operator("sin", sin).
is_operator("sphere", sphere).
is_operator("spotlight", spotlight).
is_operator("sqrt", sqrt).
is_operator("subi", subi).
is_operator("subf", subf).
is_operator("translate", translate).
is_operator("union", union).
is_operator("uscale", uscale).

:- pred get_identifier(list(char)::in, string::out, io::di, io::uo) is det.

get_identifier(Chars, String, !IO) :-
    lexer_read_char(CharResult, !IO),
    (
        CharResult = ok(Char),
        (
            ( char.is_alnum_or_underscore(Char)
            ; Char = ('-')
            )
        ->
            get_identifier([Char | Chars], String, !IO)
        ;
            io.putback_char(Char, !IO),
            string.from_rev_char_list(Chars, String)
        )
    ;
        CharResult = eof,
        string.from_rev_char_list(Chars, String)
    ).

:- pred get_string(list(char)::in, string::out, io::di, io::uo) is det.

get_string(Chars, String, !IO) :-
    lexer_read_char(CharResult, !IO),
    (
        CharResult = ok(Char),
        ( Char = '"' ->
            string.from_rev_char_list(Chars, String)
        ; is_printable(Char) ->
            get_string([Char | Chars], String, !IO)
        ;
            lexer_unexpected_char(Char, "string", !IO)
        )
    ;
        CharResult = eof,
        lexer_unexpected_eof("string constant", !IO)
    ).

:- pred get_number(list(char)::in, number::out, io::di, io::uo) is det.

get_number(Chars, Token, !IO) :-
    lexer_read_char(Result, !IO),
    (
        Result = eof,
        rev_char_list_to_int(Chars, 10, Token, !IO)
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_number([Char | Chars], Token, !IO)
        ; Char = ('.') ->
            get_int_dot(Chars, Token, !IO)
        ; ( Char = 'e' ; Char = 'E' ) ->
            get_float_exponent([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_int(Chars, 10, Token, !IO)
        )
    ).

:- pred get_int_dot(list(char)::in, number::out, io::di, io::uo) is det.

get_int_dot(Chars, Token, !IO) :-
    lexer_read_char(Result, !IO),
    (
        Result = eof,
        io.putback_char('.', !IO),
        rev_char_list_to_int(Chars, 10, Token, !IO)
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_float_decimals([Char, '.' | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            io.putback_char('.', !IO),
            rev_char_list_to_int(Chars, 10, Token, !IO)
        )
    ).

    % we've read past the decimal point, so now get the decimals
    %
:- pred get_float_decimals(list(char)::in, number::out, io::di, io::uo) is det.

get_float_decimals(Chars, Token, !IO) :-
    lexer_read_char(Result, !IO),
    (
        Result = eof,
        rev_char_list_to_float(Chars, Token, !IO)
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_float_decimals([Char | Chars], Token, !IO)
        ; ( Char = 'e' ; Char = 'E' ) ->
            get_float_exponent([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_float(Chars, Token, !IO)
        )
    ).

:- pred get_float_exponent(list(char)::in, number::out, io::di, io::uo) is det.

get_float_exponent(Chars, Token, !IO) :-
    lexer_read_char(Result, !IO),
    (
        Result = eof,
        rev_char_list_to_float(Chars, Token, !IO)
    ;
        Result = ok(Char),
        ( Char = ('-') ->
            get_float_exponent_2([Char | Chars], Token, !IO)
        ; char.is_digit(Char) ->
            get_float_exponent_3([Char | Chars], Token, !IO)
        ;
            lexer_unexpected_char(Char, "float exponent", !IO)
        )
    ).

    % we've read past the E signalling the start of the exponent -
    % make sure that there's at least one digit following,
    % and then get the remaining digits
    %
:- pred get_float_exponent_2(list(char)::in, number::out, io::di, io::uo)
    is det.

get_float_exponent_2(Chars, Token, !IO) :-
    lexer_read_char(Result, !IO),
    (
        Result = eof,
        lexer_unexpected_eof("float exponent", !IO)
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_float_exponent_3([Char | Chars], Token, !IO)
        ;
            lexer_unexpected_char(Char, "float exponent", !IO)
        )
    ).

    % we've read past the first digit of the exponent -
    % now get the remaining digits
    %
:- pred get_float_exponent_3(list(char)::in, number::out, io::di, io::uo)
    is det.

get_float_exponent_3(Chars, Token, !IO) :-
    lexer_read_char(Result, !IO),
    (
        Result = eof,
        rev_char_list_to_float(Chars, Token, !IO)
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_float_exponent_3([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_float(Chars, Token, !IO)
        )
    ).

:- pred rev_char_list_to_int(list(char)::in, int::in, number::out,
    io::di, io::uo) is det.

rev_char_list_to_int(RevChars, Base, Token, !IO) :-
    string.from_rev_char_list(RevChars, String),
    conv_string_to_int(String, Base, Token, !IO).

:- pred conv_string_to_int(string::in, int::in, number::out,
    io::di, io::uo) is det.

conv_string_to_int(String, Base, Token, !IO) :-
    ( string.base_string_to_int(Base, String, Int) ->
        Token = integer(Int)
    ;
        io.get_line_number(Line, !IO),
        Msg = "invalid int token `" ++ String ++ "'",
        throw(lexer_error(Line, Msg))
    ).

:- pred rev_char_list_to_float(list(char)::in, number::out,
    io::di, io::uo) is det.

rev_char_list_to_float(RevChars, Token, !IO) :-
    string.from_rev_char_list(RevChars, String),
    conv_to_float(String, Token, !IO).

:- pred conv_to_float(string::in, number::out, io::di, io::uo) is det.

conv_to_float(String, Token, !IO) :-
    ( string.to_float(String, Float) ->
        Token = real(Float)
    ;
        io.get_line_number(Line, !IO),
        Msg = "invalid float token `" ++ String ++ "'",
        throw(lexer_error(Line, Msg))
    ).

%-----------------------------------------------------------------------------%

:- pred lexer_read_char(io.result(char)::out(bound(ok(ground);eof)),
    io::di, io::uo) is det.

lexer_read_char(CharResult, !IO) :-
    io.read_char(CharResult0, !IO),
    (
        CharResult0 = ok(_),
        CharResult = CharResult0
    ;
        CharResult0 = eof,
        CharResult = CharResult0
    ;
        CharResult0 = error(Error),
        lexer_io_error(Error, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred lexer_unexpected_eof(string::in, io::di, io::uo) is erroneous.

lexer_unexpected_eof(Where, !IO) :-
    io.get_line_number(Line, !IO),
    Msg = string.append("unexpected end-of-file in ", Where),
    throw(lexer_error(Line, Msg)).

:- pred lexer_unexpected_char(char::in, string::in, io::di, io::uo)
    is erroneous.

lexer_unexpected_char(Char, Where, !IO) :-
    io.get_line_number(Line, !IO),
    Msg = "unexpected character `" ++ string.from_char_list([Char]) ++ "'" ++
        " in " ++ Where,
    throw(lexer_error(Line, Msg)).

:- pred lexer_io_error(io.error::in, io::di, io::uo) is erroneous.

lexer_io_error(Error, !IO) :-
    io.get_line_number(Line, !IO),
    io.error_message(Error, Msg),
    throw(lexer_error(Line, Msg)).

:- pred error_rebind_operator(string::in, io::di, io::uo) is erroneous.

error_rebind_operator(Identifier, !IO) :-
    io.get_line_number(Line, !IO),
    Msg = "attempt to rebind operator `" ++ Identifier ++ "'",
    throw(lexer_error(Line, Msg)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

parse(Tokens, Program) :-
    StopAt = eof,
    parse_2(StopAt, Tokens, RemainingTokens, [], Program0),
    (
        RemainingTokens =  []
    ;
        RemainingTokens =  [_ | _],
        throw(parse_error("tokens left over at end of parse"))
    ),
    list.reverse(Program0, Program).

:- type stop_at
    --->    eof
    ;       end_array
    ;       end_function.

:- pred parse_2(stop_at::in, list(basic_token)::in, list(basic_token)::out,
    gml_program::in, gml_program::out) is det.

parse_2(StopAt, [], RemainingTokens, Prog0, Prog) :-
    (
        StopAt = eof,
        RemainingTokens = [],
        Prog = Prog0
    ;
        StopAt = end_array,
        throw(parse_error("unterminated array"))
    ;
        StopAt = end_function,
        throw(parse_error("unterminated function"))
    ).
parse_2(StopAt, [Token | Tokens], RemainingTokens, Prog0, Prog) :-
    (
        Token = token(TheToken),
        parse_2(StopAt, Tokens, RemainingTokens,
            [single_token(TheToken) | Prog0], Prog)
    ;
        Token = '[',
        parse_2(end_array, Tokens, Tokens1, [], Array0),
        list.reverse(Array0, Array),
        parse_2(StopAt, Tokens1, RemainingTokens,
            [array(Array) | Prog0], Prog)
    ;
        Token = ']',
        ( StopAt = end_array ->
            RemainingTokens = Tokens,
            Prog = Prog0
        ;
            throw(parse_error("']' without preceding '['"))
        )
    ;
        Token = '{',
        parse_2(end_function, Tokens, Tokens1, [], Func0),
        list.reverse(Func0, Func),
        parse_2(StopAt, Tokens1, RemainingTokens,
            [function(Func) | Prog0], Prog)
    ;
        Token = '}',
        ( StopAt = end_function ->
            RemainingTokens = Tokens,
            Prog = Prog0
        ;
            throw(parse_error("'}' without preceding '{'"))
        )
    ).
