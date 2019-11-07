%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This is a regression test. In versions of the compiler before 7 Aug 2007,
% it used to cause this compiler abort:
%
% Software Error: code_gen.m: Unexpected: nondet model in det/semidet context
%
% when generating code for the java_identifier predicate.
%
% The bug was that the follow_code pass would push a goal binding a variable
% into a semidet disjunction. This would cause the disjunction to have an
% output, which would cause the simplify pass to change its determinism,
% and from there the determinism of the entire procedure body, to nondet.
% Since java_identifier is supposed to be semidet, this causes the abort above.
%
%---------------------------------------------------------------------------%
% Ralph Becket <rafe@cs.mu.oz.au>
% Mon Feb 21 09:42:40 EST 2005
%
% Java grammar taken from
% http://java.sun.com/docs/books/jls/second_edition/html/syntax.doc.html
% and converted to Mercury DCGs.  And then corrected because the grammar
% at the above page contains bugs...
%
% Usage: parse_java <filename> ...
%
% This implementation only recognises Java programs; it does not construct an
% ADT or perform any kind of analysis.  However, it has been written in a
% style that should make it fairly easy to add construction of an ADT.
%
% To compile this as a packrat parser, uncomment all of the pragma memo
% lines.
%
% To compile this as a partial packrat parser, uncomment all of the
% pragma memo lines except those for literal/2, qualified_identifier/2, and
% punct/2.
%
%---------------------------------------------------------------------------%

:- module mercury_java_parser_follow_code_bug.

:- interface.

:- import_module io.

:- pred main(io :: di, io :: uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module std_util.
:- import_module string.

    % The parser "state".  This is just the offset into the input string,
    % which is stored in a C global.  We do this to (a) avoid memoizing the
    % input string if we are memoing any rules and (b) to obtain a fair
    % timing comparison with the Rats! Java parser
    % (www.cs.nyu.edu/rgrimm/xtc/rats.html).
    %
:- type ps == int.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    list.foldl(parse_file, Args, !IO).

:- pred parse_file(string::in, io::di, io::uo) is det.
:- pragma promise_pure(parse_file/3).

parse_file(Filename, !IO) :-
    io.write_string(Filename, !IO),
    io.see(Filename, _, !IO),
    io.read_file_as_string(Result, !IO),
    ( if Result = ok(Str) then
        impure set_input_string(Str),
        ( if   compilation_unit(0, _)
          then io.print(" parsed successfully\n", !IO)
          else io.print(" failed to parse\n", !IO)
        )
    else
        throw(Result)
    ).

%---------------------------------------------------------------------------%
% Low-level predicates.
%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

    MR_String input_string = NULL;
    MR_Word input_length = (MR_Word)0;

").

:- impure pred set_input_string(string::in) is det.

:- pragma foreign_proc("C",
    set_input_string(Str::in),
    [will_not_call_mercury], "

    input_string = Str;
    input_length = strlen(Str);

").

:- semipure pred input_string_and_length(string::out, int::out) is det.

:- pragma foreign_proc("C",
    input_string_and_length(Str::out, Length::out),
    [will_not_call_mercury, promise_semipure], "

    Str = input_string;
    Length = input_length;

").

:- pred current_offset(int::out, int::in, int::out) is det.

current_offset(Offset, Offset, Offset).

:- pred eof(int::in, int::out) is semidet.
:- pragma promise_pure(eof/2).

eof(Offset, Offset) :-
    semipure input_string_and_length(_Str, Length),
    Offset = Length.

    % XXX These are really semipure, but I'm being naughty and promising them
    % to be pure because I don't want to pollute my code with impurity
    % annotations.
    %
    % XXX Also, I do not check for negative offsets.  I probably should.
    %
:- pred char(char::out, int::in, int::out) is semidet.
:- pragma promise_pure(char/3).

char(Char, Offset, Offset + 1) :-
    semipure input_string_and_length(Str, Length),
    Offset < Length,
    Char = Str ^ unsafe_elem(Offset).

:- pred input_substring(int::in, int::in, string::out,
        int::in, int::out) is semidet.
:- pragma promise_pure(input_substring/5).

input_substring(Start, End, Substring, Offset, Offset) :-
    semipure input_string_and_length(Str, Length),
    End =< Length,
    Substring = unsafe_between(Str, Start, End).

:- pred match_string(string::in, int::in, int::out) is semidet.
:- pragma promise_pure(match_string/3).

match_string(MatchStr, Offset, Offset + N) :-
    semipure input_string_and_length(Str, Length),
    N = length(MatchStr),
    Offset + N =< Length,
    match_string_2(0, N, MatchStr, Offset, Str).

:- pred match_string_2(int::in, int::in, string::in, int::in, string::in)
        is semidet.

match_string_2(I, N, MatchStr, Offset, Str) :-
    ( if I < N then
        MatchStr ^ unsafe_elem(I) = Str ^ unsafe_elem(Offset + I),
        match_string_2(I + 1, N, MatchStr, Offset, Str)
    else
        true
    ).

%---------------------------------------------------------------------------%
% Utility predicates.
%---------------------------------------------------------------------------%

:- pred optional(pred(ps, ps)::in(pred(in, out) is semidet),
    ps::in, ps::out) is semidet.

optional(P) -->
    ( if P then [] else { semidet_succeed } ).

:- pred zero_or_more(pred(ps, ps)::in(pred(in, out) is semidet),
            ps::in, ps::out) is semidet.

zero_or_more(P) -->
    ( if P then zero_or_more(P) else { semidet_succeed } ).

:- pred one_or_more(pred(ps, ps)::in(pred(in, out) is semidet),
    ps::in, ps::out) is semidet.

one_or_more(P) -->
    P,
    zero_or_more(P).

:- pred brackets(string::in, pred(ps, ps)::in(pred(in, out) is semidet),
    string::in, ps::in, ps::out) is semidet.

brackets(L, P, R) -->
    punct(L),
    P,
    punct(R).

%---------------------------------------------------------------------------%

:- pred ++(pred(ps, ps)::in(pred(in, out) is semidet),
           pred(ps, ps)::in(pred(in, out) is semidet),
           ps::in, ps::out) is semidet.

:- pragma inline((++)/4).

P ++ Q -->
    P,
    Q.

%---------------------------------------------------------------------------%

:- pred comma_separated_list(pred(ps, ps)::in(pred(in, out) is semidet),
    ps::in, ps::out) is semidet.

comma_separated_list(P) -->
    P, zero_or_more(punct(", ") ++ P).

%---------------------------------------------------------------------------%

:- pred whitespace(ps::in, ps::out) is semidet.

whitespace -->
    ( if char(C1), { char.is_whitespace(C1) } then
        whitespace
    else if char('/'), char('/') then
        skip_to_eol,
        whitespace
    else if char('/'), char('*') then
        skip_to_end_of_trad_comment,
        whitespace
    else
        []
    ).

:- pred skip_to_eol(ps::in, ps::out) is semidet.

skip_to_eol -->
    char(C),
    ( if { C = ('\n') } then [] else skip_to_eol ).

:- pred skip_to_end_of_trad_comment(ps::in, ps::out) is semidet.

skip_to_end_of_trad_comment -->
    ( if char('*'), char('/') then
        []
    else
        char(_),
        skip_to_end_of_trad_comment
    ).

%---------------------------------------------------------------------------%

:- pred punct(string::in, ps::in, ps::out) is semidet.

% :- pragma memo(punct/3).
punct(Punct) -->
    match_string(Punct),
    whitespace.

%---------------------------------------------------------------------------%

:- pred keyword(string::in, ps::in, ps::out) is semidet.

% :- pragma memo(keyword/3).
keyword(Keyword) -->
    match_string(Keyword),
    not(java_identifier_part),
    whitespace.

%---------------------------------------------------------------------------%

:- pred keyword(string::in, pred(ps, ps)::in(pred(in, out) is semidet),
    ps::in, ps::out) is semidet.

keyword(Keyword, P) -->
    match_string(Keyword),
    not(java_identifier_part),
    whitespace,
    P.

%---------------------------------------------------------------------------%

:- pragma memo(java_identifier/2, [allow_reset, fast_loose]).

:- pred java_identifier(/*string::out, */ ps::in, ps::out) is semidet.

% :- pragma memo(java_identifier/2).
java_identifier/*(Identifier)*/ -->
%     current_offset(Start),
    java_identifier_start,
    zero_or_more(java_identifier_part),
%     current_offset(End),
%     input_substring(Start, End, Identifier),
    whitespace.

:- pred java_identifier_start(ps::in, ps::out) is semidet.

java_identifier_start -->
    char(C),
    { char.is_alpha_or_underscore(C) ; C = ('$') }.

:- pred java_identifier_part(ps::in, ps::out) is semidet.

java_identifier_part -->
    char(C),
    { char.is_alnum_or_underscore(C) ; C = ('$') }.

%---------------------------------------------------------------------------%

:- pred floating_point_literal(ps::in, ps::out) is semidet.

floating_point_literal -->
    ( if optional(digits(10)), char('.'), digits(10) then
        optional(exponent_part),
        optional(float_type_suffix)
    else
        digits(10),
        ( if exponent_part then optional(float_type_suffix)
                           else float_type_suffix
        )
    ).

:- pred exponent_part(ps::in, ps::out) is semidet.

exponent_part -->
    char(C), { C = ('E') ; C = ('e') },
    optional(sign),
    one_or_more(digit(10)).

:- pred sign(ps::in, ps::out) is semidet.

sign -->
    char(C), { C = ('+') ; C = ('-') }.

:- pred float_type_suffix(ps::in, ps::out) is semidet.

float_type_suffix -->
    char(C), { C = ('F') ; C = ('f') ; C = ('D') ; C = ('d') }.

%---------------------------------------------------------------------------%

:- pred integer_literal(ps::in, ps::out) is semidet.

integer_literal -->
    ( if hex_literal then
        []
    else if oct_literal then
        []
    else
        dec_literal
    ).

:- pred hex_literal(ps::in, ps::out) is semidet.

hex_literal -->
    char('0'),
    char('x'),
    digits(16),
    optional(integer_type_suffix).

:- pred oct_literal(ps::in, ps::out) is semidet.

oct_literal -->
    char('0'),
    digits(8),
    optional(integer_type_suffix).

:- pred dec_literal(ps::in, ps::out) is semidet.

dec_literal -->
    ( if char('0') then not digit(16) else digits(10) ),
    optional(integer_type_suffix).

:- pred integer_type_suffix(ps::in, ps::out) is semidet.

integer_type_suffix -->
    char(C), { C = ('L') ; C = ('l') }.

:- pred digits(int::in, ps::in, ps::out) is semidet.

digits(Base) -->
    one_or_more(digit(Base)).

:- pred digit(int::in, ps::in, ps::out) is semidet.

digit(Base) -->
    char(C), { char.digit_to_int(C, D), D < Base }.

%---------------------------------------------------------------------------%

:- pred character_literal(ps::in, ps::out) is semidet.

character_literal -->
    char('\''),
    not char('\''),
    possibly_escaped_char,
    char('\'').

:- pred possibly_escaped_char(ps::in, ps::out) is semidet.

possibly_escaped_char -->
    char(C1),
    ( if { C1 = ('\\') } then
        ( if digits(8) then
            []
          else if char('u'), one_or_more(digits(16)) then
            []
          else
            char(C2),
            { member(C2, [('b'), ('t'), ('n'), ('f'), ('r'), ('\"'), ('\''),
                    ('\\')]) }
        )
    else
        []
    ).

%---------------------------------------------------------------------------%

:- pred string_literal(ps::in, ps::out) is semidet.

string_literal -->
    char('\"'),
    zero_or_more(string_char),
    char('\"').

:- pred string_char(ps::in, ps::out) is semidet.

string_char -->
    not(char('"')),
    possibly_escaped_char.

%---------------------------------------------------------------------------%

:- pred boolean_literal(ps::in, ps::out) is semidet.

boolean_literal -->
    ( if keyword("true") then
        []
    else
        keyword("false")
    ).

%---------------------------------------------------------------------------%

:- pred null_literal(ps::in, ps::out) is semidet.

null_literal -->
    keyword("null").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% Taken from
% http://java.sun.com/docs/books/jls/second_edition/html/syntax.doc.html

%      _________________________________________________________________
%
%    [5]Contents | [6]Prev | [7]Next | [8]Index Java Language Specification
%    Second Edition
%
%    [9]Copyright © 2000 Sun Microsystems, Inc. All rights reserved
%    Please send any comments or corrections to the [10]JLS team

%                                     Syntax
%
%    This chapter presents a grammar for the Java programming language.
%
%    The grammar presented piecemeal in the preceding chapters is much
%    better for exposition, but it is not ideally suited as a basis for a
%    parser. The grammar presented in this chapter is the basis for the
%    reference implementation.
%
%    The grammar below uses the following BNF-style conventions:
%
%      * [x] denotes zero or one occurrences of x.
%      * {x} denotes zero or more occurrences of x.
%      * x | y means one of either x or y.

% Identifier:
%         IDENTIFIER
%
% QualifiedIdentifier:
%         Identifier { . Identifier }

:- pred qualified_identifier(ps::in, ps::out) is semidet.

% :- pragma memo(qualified_identifier/2).
qualified_identifier -->
    java_identifier, zero_or_more(dot_java_identifier).

:- pred dot_java_identifier(ps::in, ps::out) is semidet.

% :- pragma memo(dot_java_identifier/2).
dot_java_identifier -->
    punct("."),
    java_identifier.

% Literal:
%         IntegerLiteral
%         FloatingPointLiteral
%         CharacterLiteral
%         StringLiteral
%         BooleanLiteral
%         NullLiteral

:- pred literal(ps::in, ps::out) is semidet.

% :- pragma memo(literal/2).
literal -->
    ( if
        floating_point_literal
    then
        []
    else if
        integer_literal
    then
        []
    else if
        character_literal
    then
        []
    else if
        string_literal
    then
        []
    else if
        boolean_literal
    then
        []
    else
        null_literal
    ),
    whitespace.

% Expression:
%         Expression1 [AssignmentOperator Expression1]]
%           XXX I think that should be
%           Expression1 {AssignmentOperator Expression1}

:- pred expression(ps::in, ps::out) is semidet.

% :- pragma memo(expression/2).
expression -->
    expression1, zero_or_more(assignment_operator_expression1).

:- pred assignment_operator_expression1(ps::in, ps::out) is semidet.

assignment_operator_expression1 -->
    assignment_operator, expression1.

% AssignmentOperator:
%         =
%         +=
%         -=
%         *=
%         /=
%         &=
%         | =
%         ^=
%         %=
%         <<=
%         >>=
%         >>>=

:- pred assignment_operator(ps::in, ps::out) is semidet.

% :- pragma memo(assignment_operator/2).
assignment_operator -->
    ( if
        punct("=")
    then
        []
    else if
        punct("+=")
    then
        []
    else if
        punct("-=")
    then
        []
    else if
        punct("*=")
    then
        []
    else if
        punct("/=")
    then
        []
    else if
        punct("&=")
    then
        []
    else if
        punct(" | =")
    then
        []
    else if
        punct("^=")
    then
        []
    else if
        punct("%=")
    then
        []
    else if
        punct("<<=")
    then
        []
    else if
        punct(">>=")
    then
        []
    else
        punct(">>>=")
    ).

% Type:
%         Identifier {   .   Identifier } BracketsOpt
%         BasicType

:- pred java_type(ps::in, ps::out) is semidet.

% :- pragma memo(java_type/2).
java_type -->
    ( if
        qualified_identifier, brackets_opt
    then
        []
    else
        basic_type
    ).

% StatementExpression:
%         Expression

:- pred statement_expression(ps::in, ps::out) is semidet.

% :- pragma memo(statement_expression/2).
statement_expression -->
    expression.

% ConstantExpression:
%         Expression

:- pred constant_expression(ps::in, ps::out) is semidet.

% :- pragma memo(constant_expression/2).
constant_expression -->
    expression.

% Expression1:
%         Expression2 [Expression1Rest]

:- pred expression1(ps::in, ps::out) is semidet.

expression1 -->
    expression2, optional(expression1_rest).

% Expression1Rest:
%         [  ?   Expression   :   Expression1]

:- pred expression1_rest(ps::in, ps::out) is semidet.

% :- pragma memo(expression1_rest/2).
expression1_rest -->
    punct("?"), expression, punct(":"), expression1.

% Expression2 :
%         Expression3 [Expression2Rest]

:- pred expression2(ps::in, ps::out) is semidet.

expression2 -->
    expression3, optional(expression2_rest).

% Expression2Rest:
%         {Infixop Expression3}
%         Expression3 instanceof Type
%         XXX The Expression3 here must be wrong...
%         XXX And {Infixop Expression3} should be allowed after Type.

:- pred expression2_rest(ps::in, ps::out) is semidet.

% :- pragma memo(expression2_rest/2).
expression2_rest -->
    ( if
        keyword("instanceof"), java_type, optional(expression2_rest)
    then
        []
    else
        zero_or_more(infix_op_expression3)
    ).

:- pred infix_op_expression3(ps::in, ps::out) is semidet.

infix_op_expression3 -->
    infix_op, expression3.

% Infixop:
%         | |
%         &&
%         |
%         ^
%         &
%         ==
%         !=
%         <
%         >
%         <=
%         >=
%         <<
%         >>
%         >>>
%         +
%         -
%         *
%         /
%         %

:- pred infix_op(ps::in, ps::out) is semidet.

% :- pragma memo(infix_op/2).
infix_op -->
    ( if
        punct(" | |")
    then
        []
    else if
        punct("&&")
    then
        []
    else if
        punct(" | ")
    then
        []
    else if
        punct("^")
    then
        []
    else if
        punct("&")
    then
        []
    else if
        punct("==")
    then
        []
    else if
        punct("!=")
    then
        []
    else if
        punct("<=")
    then
        []
    else if
        punct(">=")
    then
        []
    else if
        punct("<<")
    then
        []
    else if
        punct(">>>")
    then
        []
    else if
        punct(">>")
    then
        []
    else if
        punct("<")
    then
        []
    else if
        punct(">")
    then
        []
    else if
        punct("+")
    then
        []
    else if
        punct("-")
    then
        []
    else if
        punct("*")
    then
        []
    else if
        punct("/")
    then
        []
    else
        punct("%")
    ).

% Expression3:
%         PrefixOp Expression3
%         (   Expr | Type   )   Expression3
%         Primary {Selector} {PostfixOp}

:- pred expression3(ps::in, ps::out) is semidet.

expression3 -->
    ( if
        prefix_op, expression3
    then
        []
    else if
        brackets("(", expression_or_java_type, ")"), expression3
    then
        []
    else
        primary, zero_or_more(selector), zero_or_more(postfix_op)
    ).

:- pred expression_or_java_type(ps::in, ps::out) is semidet.

% :- pragma memo(expression_or_java_type/2).
expression_or_java_type -->
    ( if
        java_type
    then
        []
    else
        expression
    ).

% Primary:
%         ( Expression )
%         this [Arguments]
%         super SuperSuffix
%         Literal
%         new Creator
%         Identifier { . Identifier }[ IdentifierSuffix]
%         BasicType BracketsOpt .class
%         void.class

:- pred primary(ps::in, ps::out) is semidet.

% :- pragma memo(primary/2).
primary -->
    ( if
        brackets("(", expression, ")")
    then
        []
    else if
        keyword("this"), optional(arguments)
    then
        []
    else if
        keyword("super"), super_suffix
    then
        []
    else if
        keyword("new"), creator
    then
        []
    else if
        keyword("void"), punct("."), keyword("class")
    then
        []
    else if
        basic_type, brackets_opt, punct("."), keyword("class")
    then
        []
    else if
        literal
    then
        []
    else
        qualified_identifier, optional(identifier_suffix)
    ).

% XXX I don't understand how to read this rule:
%
% IdentifierSuffix:
%         [ ( ] BracketsOpt   .   class | Expression ])
%         Arguments
%         .   ( class | this | super Arguments | new InnerCreator )

:- pred identifier_suffix(ps::in, ps::out) is semidet.

% :- pragma memo(identifier_suffix/2).
identifier_suffix -->
    ( if
        brackets_opt, punct("."),
        ( if
            keyword("class")
          then
            []
          else
            expression
        )
    then
        []
    else if
        arguments
    then
        []
    else
        punct("."),
        ( if
            keyword("class")
          then
            []
          else if
            keyword("this")
          then
            []
          else if
            keyword("super"), arguments
          then
            []
          else
            keyword("new"), inner_creator
        )
    ).

% PrefixOp:
%        ,
%         --
%         !
%         ~
%         +
%         -

:- pred prefix_op(ps::in, ps::out) is semidet.

% :- pragma memo(prefix_op/2).
prefix_op -->
    ( if
        punct("++")
    then
        []
    else if
        punct("--")
    then
        []
    else if
        punct("!")
    then
        []
    else if
        punct("~")
    then
        []
    else if
        punct("+")
    then
        []
    else
        punct("-")
    ).

% PostfixOp:
%        ,
%         --

:- pred postfix_op(ps::in, ps::out) is semidet.

% :- pragma memo(postfix_op/2).
postfix_op -->
    ( if
        punct("++")
    then
        []
    else
        punct("--")
    ).

% Selector:
%         . Identifier [Arguments]
%         . this
%         . super SuperSuffix
%         . new InnerCreator
%         [ Expression ]

:- pred selector(ps::in, ps::out) is semidet.

% :- pragma memo(selector/2).
selector -->
    ( if
        brackets("[", expression, "]")
    then
        []
    else
        punct("."),
        ( if
            keyword("this")
          then
            []
          else if
            keyword("super"), super_suffix
          then
            []
          else if
            keyword("new"), inner_creator
          then
            []
          else
            java_identifier, optional(arguments)
        )
    ).

% SuperSuffix:
%         Arguments
%         . Identifier [Arguments]

:- pred super_suffix(ps::in, ps::out) is semidet.

% :- pragma memo(super_suffix/2).
super_suffix -->
    ( if
        arguments
    then
        []
    else
        punct("."), java_identifier, optional(arguments)
    ).

% BasicType:
%         byte
%         short
%         char
%         int
%         long
%         float
%         double
%         boolean

:- pred basic_type(ps::in, ps::out) is semidet.

% :- pragma memo(basic_type/2).
basic_type -->
    ( if
        punct("byte")
    then
        []
    else if
        punct("short")
    then
        []
    else if
        punct("char")
    then
        []
    else if
        punct("int")
    then
        []
    else if
        punct("long")
    then
        []
    else if
        punct("float")
    then
        []
    else if
        punct("double")
    then
        []
    else
        punct("boolean")
    ).

% Arguments:
%         ( [Expression { , Expression }] )

:- pred arguments(ps::in, ps::out) is semidet.

% :- pragma memo(arguments/2).
arguments -->
    brackets("(", optional(comma_separated_list(expression)), ")").

% BracketsOpt:
%         {[]}

:- pred brackets_opt(ps::in, ps::out) is semidet.

% :- pragma memo(brackets_opt/2).
brackets_opt -->
    zero_or_more(empty_brackets).

:- pred empty_brackets(ps::in, ps::out) is semidet.

% :- pragma memo(empty_brackets/2).
empty_brackets -->
    punct("["), punct("]").

% Creator:
%         QualifiedIdentifier ( ArrayCreatorRest  | ClassCreatorRest )

:- pred creator(ps::in, ps::out) is semidet.

% :- pragma memo(creator/2).
creator -->
    qualified_identifier, creator_rest.

:- pred creator_rest(ps::in, ps::out) is semidet.

% :- pragma memo(creator_rest/2).
creator_rest -->
    ( if
        array_creator_rest
    then
        []
    else
        class_creator_rest
    ).

% InnerCreator:
%         Identifier ClassCreatorRest

:- pred inner_creator(ps::in, ps::out) is semidet.

% :- pragma memo(inner_creator/2).
inner_creator -->
    java_identifier, class_creator_rest.

% XXX I don't understand how to read this rule:
%
% ArrayCreatorRest:
%         [ ( ] BracketsOpt ArrayInitializer | Expression ] {[ Expression ]} BracketsOpt )

:- pred array_creator_rest(ps::in, ps::out) is semidet.

% :- pragma memo(array_creator_rest/2).
array_creator_rest -->
    one_or_more(brackets("[", optional(expression), "]")),
    optional(array_initializer).

% ClassCreatorRest:
%         Arguments [ClassBody]

:- pred class_creator_rest(ps::in, ps::out) is semidet.

% :- pragma memo(class_creator_rest/2).
class_creator_rest -->
    arguments, optional(class_body).

% ArrayInitializer:
%         { [VariableInitializer {, VariableInitializer} [, ]] }

:- pred array_initializer(ps::in, ps::out) is semidet.

% :- pragma memo(array_initializer/2).
array_initializer -->
    brackets("{", optional(array_initializer_body), "}").

:- pred array_initializer_body(ps::in, ps::out) is semidet.

% :- pragma memo(array_initializer_body/2).
array_initializer_body -->
    comma_separated_list(variable_initializer), optional(punct(", ")).

% VariableInitializer:
%         ArrayInitializer
%         Expression

:- pred variable_initializer(ps::in, ps::out) is semidet.

% :- pragma memo(variable_initializer/2).
variable_initializer -->
    ( if
        array_initializer
    then
        []
    else
        expression
    ).

% ParExpression:
%         ( Expression )

:- pred par_expression(ps::in, ps::out) is semidet.

% :- pragma memo(par_expression/2).
par_expression -->
    brackets("(", expression, ")").

% Block:
%         { BlockStatements }

:- pred block(ps::in, ps::out) is semidet.

% :- pragma memo(block/2).
block -->
    brackets("{", block_statements, "}").

% BlockStatements:
%         { BlockStatement }

:- pred block_statements(ps::in, ps::out) is semidet.

% :- pragma memo(block_statements/2).
block_statements -->
    zero_or_more(block_statement).

% BlockStatement :
%         LocalVariableDeclarationStatement
%         ClassOrInterfaceDeclaration
%         [Identifier :] Statement

:- pred block_statement(ps::in, ps::out) is semidet.

% :- pragma memo(block_statement/2).
block_statement -->
    ( if
        local_variable_declaration_statement
    then
        []
    else if
        class_or_interface_declaration
    then
        []
    else
        optional(label), statement
    ).

:- pred label(ps::in, ps::out) is semidet.

% :- pragma memo(label/2).
label -->
    java_identifier, punct(":").

% LocalVariableDeclarationStatement:
%         [final] Type VariableDeclarators   ;
%         XXX I think this is wrong: [final] should be ModifiersOpt, surely?

:- pred local_variable_declaration_statement(ps::in, ps::out) is semidet.

% :- pragma memo(local_variable_declaration_statement/2).
local_variable_declaration_statement -->
    modifiers_opt,
    java_type,
    variable_declarators,
    punct(";").

% Statement:
%         Block
%         if ParExpression Statement [else if Statement]
%         for ( ForInitOpt   ;   [Expression]   ;   ForUpdateOpt ) Statement
%         while ParExpression Statement
%         do Statement while ParExpression   ;
%         try Block ( Catches | [Catches] finally Block )
%         switch ParExpression { SwitchBlockStatementGroups }
%         synchronized ParExpression Block
%         return [Expression] ;
%         throw Expression   ;
%         break [Identifier]
%         continue [Identifier]
%         ;
%         ExpressionStatement
%         Identifier   :   Statement

:- pred statement(ps::in, ps::out) is semidet.

% :- pragma memo(statement/2).
statement -->
    ( if
        block
    then
        []
    else if
        keyword("if"), par_expression, statement,
            optional(else_statement)
    then
        []
    else if
        keyword("for"),
        punct("("),
        optional(for_init),
        punct(";"),
        optional(expression),
        punct(";"),
        optional(for_update),
        punct(")"),
        statement
    then
        []
    else if
        keyword("while"), par_expression, statement
    then
        []
    else if
        keyword("do"), statement,
            keyword("while"), par_expression, punct(";")
    then
        []
    else if
        keyword("try"), block, catches_finally
    then
        []
    else if
        keyword("switch"), par_expression,
            brackets("{", switch_block_statement_groups, "}")
    then
        []
    else if
        keyword("synchronized"), par_expression, block
    then
        []
    else if
        keyword("return"), optional(expression), punct(";")
    then
        []
    else if
        keyword("throw"), expression, punct(";")
    then
        []
    else if
        keyword("break"), optional(java_identifier), punct(";")
    then
        []
    else if
        keyword("continue"), optional(java_identifier), punct(";")
    then
        []
    else if
        punct(";")
    then
        []
    else if
        expression, punct(";")
    then
        []
    else
        java_identifier, punct(":"), statement
    ).

:- pred else_statement(ps::in, ps::out) is semidet.

% :- pragma memo(else_statement/2).
else_statement -->
    keyword("else"), statement.

:- pred catches_finally(ps::in, ps::out) is semidet.

% :- pragma memo(catches_finally/2).
catches_finally -->
    ( if
        catches, optional(finally_block)
    then
        []
    else
        finally_block
    ).

:- pred finally_block(ps::in, ps::out) is semidet.

% :- pragma memo(finally_block/2).
finally_block -->
    keyword("finally"), block.

% Catches:
%         CatchClause {CatchClause}

:- pred catches(ps::in, ps::out) is semidet.

% :- pragma memo(catches/2).
catches -->
    one_or_more(catch_clause).

% CatchClause:
%         catch ( FormalParameter ) Block

:- pred catch_clause(ps::in, ps::out) is semidet.

% :- pragma memo(catch_clause/2).
catch_clause -->
    keyword("catch"), brackets("(", formal_parameter, ")"), block.

% SwitchBlockStatementGroups:
%         { SwitchBlockStatementGroup }

:- pred switch_block_statement_groups(ps::in, ps::out) is semidet.

% :- pragma memo(switch_block_statement_groups/2).
switch_block_statement_groups -->
    zero_or_more(switch_block_statement_group).

% SwitchBlockStatementGroup:
%         SwitchLabel BlockStatements

:- pred switch_block_statement_group(ps::in, ps::out) is semidet.

% :- pragma memo(switch_block_statement_group/2).
switch_block_statement_group -->
    switch_label, block_statements.

% SwitchLabel:
%         case ConstantExpression   :
%         default:

:- pred switch_label(ps::in, ps::out) is semidet.

% :- pragma memo(switch_label/2).
switch_label -->
    ( if
        keyword("case"), constant_expression, punct(":")
    then
        []
    else
        keyword("default"), punct(":")
    ).

% MoreStatementExpressions:
%         { , StatementExpression }
%
% ForInit:
%         StatementExpression MoreStatementExpressions
%         [final] Type VariableDeclarators

:- pred for_init(ps::in, ps::out) is semidet.

% :- pragma memo(for_init/2).
for_init -->
    ( if
        comma_separated_list(for_init_statement_expression)
    then
        []
    else
        optional(keyword("final")), java_type, variable_declarators
    ).

:- pred for_init_statement_expression(ps::in, ps::out) is semidet.

% :- pragma memo(for_init_statement_expression/2).
for_init_statement_expression -->
    ( if
        java_type, java_identifier, equals_variable_initializer
    then
        []
    else
        statement_expression
    ).

% ForUpdate:
%         StatementExpression MoreStatementExpressions

:- pred for_update(ps::in, ps::out) is semidet.

% :- pragma memo(for_update/2).
for_update -->
    comma_separated_list(statement_expression).

% ModifiersOpt:
%         { Modifier }

:- pred modifiers_opt(ps::in, ps::out) is semidet.

% :- pragma memo(modifiers_opt/2).
modifiers_opt -->
    zero_or_more(modifier).

% Modifier:
%         public
%         protected
%         private
%         static
%         abstract
%         final
%         native
%         synchronized
%         transient
%         volatile
%         strictfp

:- pred modifier(ps::in, ps::out) is semidet.

% :- pragma memo(modifier/2).
modifier -->
    ( if
        keyword("public")
    then
        []
    else if
        keyword("protected")
    then
        []
    else if
        keyword("private")
    then
        []
    else if
        keyword("static")
    then
        []
    else if
        keyword("abstract")
    then
        []
    else if
        keyword("final")
    then
        []
    else if
        keyword("native")
    then
        []
    else if
        keyword("synchronized")
    then
        []
    else if
        keyword("transient")
    then
        []
    else if
        keyword("volatile")
    then
        []
    else
        keyword("strictfp")
    ).

% VariableDeclarators:
%         VariableDeclarator { ,   VariableDeclarator }

:- pred variable_declarators(ps::in, ps::out) is semidet.

% :- pragma memo(variable_declarators/2).
variable_declarators -->
    comma_separated_list(variable_declarator).

% ConstantDeclaratorsRest:
%         ConstantDeclaratorRest { ,   ConstantDeclarator }

:- pred constant_declarators_rest(ps::in, ps::out) is semidet.

% :- pragma memo(constant_declarators_rest/2).
constant_declarators_rest -->
    comma_separated_list(constant_declarator_rest).

% VariableDeclarator:
%         Identifier VariableDeclaratorRest

:- pred variable_declarator(ps::in, ps::out) is semidet.

% :- pragma memo(variable_declarator/2).
variable_declarator -->
    java_identifier, variable_declarator_rest.

% ConstantDeclarator:
%         Identifier ConstantDeclaratorRest

:- pred constant_declarator(ps::in, ps::out) is semidet.

% :- pragma memo(constant_declarator/2).
constant_declarator -->
    java_identifier, constant_declarator_rest.

% VariableDeclaratorRest:
%         BracketsOpt [  =   VariableInitializer]

:- pred variable_declarator_rest(ps::in, ps::out) is semidet.

% :- pragma memo(variable_declarator_rest/2).
variable_declarator_rest -->
    brackets_opt, optional(equals_variable_initializer).

:- pred equals_variable_initializer(ps::in, ps::out) is semidet.

% :- pragma memo(equals_variable_initializer/2).
equals_variable_initializer -->
    punct("="),
    variable_initializer.

% ConstantDeclaratorRest:
%         BracketsOpt   =   VariableInitializer

:- pred constant_declarator_rest(ps::in, ps::out) is semidet.

% :- pragma memo(constant_declarator_rest/2).
constant_declarator_rest -->
    brackets_opt, punct("="), variable_initializer.

% VariableDeclaratorId:
%         Identifier BracketsOpt

:- pred variable_declarator_id(ps::in, ps::out) is semidet.

% :- pragma memo(variable_declarator_id/2).
variable_declarator_id -->
    java_identifier, brackets_opt.

% CompilationUnit:
%         [package QualifiedIdentifier   ;  ] {ImportDeclaration} {TypeDeclaration}

:- pred compilation_unit(ps::in, ps::out) is semidet.

% :- pragma memo(compilation_unit/2).
compilation_unit -->
    whitespace,
    optional(package_declaration),
    zero_or_more(import_declaration),
    zero_or_more(type_declaration),
    eof.

:- pred package_declaration(ps::in, ps::out) is semidet.

% :- pragma memo(package_declaration/2).
package_declaration -->
    keyword("package"), qualified_identifier, punct(";").

% ImportDeclaration:
%         import Identifier {   .   Identifier } [   .     *   ] ;

:- pred import_declaration(ps::in, ps::out) is semidet.

% :- pragma memo(import_declaration/2).
import_declaration -->
    keyword("import"),
    qualified_identifier,
    optional(dot_star),
    punct(";").

:- pred dot_star(ps::in, ps::out) is semidet.

% :- pragma memo(dot_star/2).
dot_star -->
    punct("."), punct("*").

% TypeDeclaration:
%         ClassOrInterfaceDeclaration

:- pred type_declaration(ps::in, ps::out) is semidet.

% :- pragma memo(type_declaration/2).
type_declaration -->
    class_or_interface_declaration.

% ClassOrInterfaceDeclaration:
%         ModifiersOpt (ClassDeclaration | InterfaceDeclaration)

:- pred class_or_interface_declaration(ps::in, ps::out) is semidet.

% :- pragma memo(class_or_interface_declaration/2).
class_or_interface_declaration -->
    modifiers_opt,
    ( if
        class_declaration
    then
        []
    else
        interface_declaration
    ).

% ClassDeclaration:
%         class Identifier [extends Type] [implements TypeList] ClassBody

:- pred class_declaration(ps::in, ps::out) is semidet.

% :- pragma memo(class_declaration/2).
class_declaration -->
    keyword("class"),
    java_identifier,
    optional(keyword("extends", java_type)),
    optional(keyword("implements", java_type_list)),
    class_body.

% InterfaceDeclaration:
%         interface Identifier [extends TypeList] InterfaceBody

:- pred interface_declaration(ps::in, ps::out) is semidet.

% :- pragma memo(interface_declaration/2).
interface_declaration -->
    keyword("interface"),
    java_identifier,
    optional(keyword("extends", java_type_list)),
    interface_body.

% TypeList:
%         Type {  ,   Type}

:- pred java_type_list(ps::in, ps::out) is semidet.

% :- pragma memo(java_type_list/2).
java_type_list -->
    comma_separated_list(java_type).

% ClassBody:
%         { {ClassBodyDeclaration} }

:- pred class_body(ps::in, ps::out) is semidet.

% :- pragma memo(class_body/2).
class_body -->
    brackets("{", zero_or_more(class_body_declaration), "}").

% InterfaceBody:
%         { {InterfaceBodyDeclaration} }

:- pred interface_body(ps::in, ps::out) is semidet.

% :- pragma memo(interface_body/2).
interface_body -->
    brackets("{", zero_or_more(interface_body_declaration), "}").

% ClassBodyDeclaration:
%         ;
%         [static] Block
%         ModifiersOpt MemberDecl

:- pred class_body_declaration(ps::in, ps::out) is semidet.

% :- pragma memo(class_body_declaration/2).
class_body_declaration -->
    ( if
        punct(";")
    then
        []
    else if
        optional(keyword("static")), block
    then
        []
    else
        modifiers_opt, member_decl
    ).

% MemberDecl:
%         MethodOrFieldDecl
%         void Identifier MethodDeclaratorRest
%         Identifier ConstructorDeclaratorRest
%         ClassOrInterfaceDeclaration

:- pred member_decl(ps::in, ps::out) is semidet.

% :- pragma memo(member_decl/2).
member_decl -->
    ( if
        class_or_interface_declaration
    then
        []
    else if
        method_or_field_decl
    then
        []
    else if
        keyword("void"), java_identifier, method_declarator_rest
    then
        []
    else
        java_identifier, constructor_declarator_rest
    ).

% MethodOrFieldDecl:
%         Type Identifier MethodOrFieldRest

:- pred method_or_field_decl(ps::in, ps::out) is semidet.

% :- pragma memo(method_or_field_decl/2).
method_or_field_decl -->
    java_type, java_identifier, method_or_field_rest.

% MethodOrFieldRest:
%         VariableDeclaratorRest
%         MethodDeclaratorRest
%         XXX First should be
%           VariableDeclaratorRest [', ' VariableDeclarators]

:- pred method_or_field_rest(ps::in, ps::out) is semidet.

% :- pragma memo(method_or_field_rest/2).
method_or_field_rest -->
    ( if
        method_declarator_rest
    then
        []
    else
        variable_declarator_rest,
        ( if punct(", ") then
            variable_declarators
          else
            []
        )
    ).

% InterfaceBodyDeclaration:
%         ;
%         ModifiersOpt InterfaceMemberDecl

:- pred interface_body_declaration(ps::in, ps::out) is semidet.

% :- pragma memo(interface_body_declaration/2).
interface_body_declaration -->
    ( if
        punct(";")
    then
        []
    else
        modifiers_opt, interface_member_decl
    ).

% InterfaceMemberDecl:
%         InterfaceMethodOrFieldDecl
%         void Identifier VoidInterfaceMethodDeclaratorRest
%         ClassOrInterfaceDeclaration

:- pred interface_member_decl(ps::in, ps::out) is semidet.

% :- pragma memo(interface_member_decl/2).
interface_member_decl -->
    ( if
        interface_method_or_field_decl
    then
        []
    else if
        keyword("void"), java_identifier, void_interface_method_declarator_rest
    then
        []
    else
        class_or_interface_declaration
    ).

% InterfaceMethodOrFieldDecl:
%         Type Identifier InterfaceMethodOrFieldRest

:- pred interface_method_or_field_decl(ps::in, ps::out) is semidet.

% :- pragma memo(interface_method_or_field_decl/2).
interface_method_or_field_decl -->
    java_type, java_identifier, interface_method_or_field_rest.

% InterfaceMethodOrFieldRest:
%         ConstantDeclaratorsRest ;
%         InterfaceMethodDeclaratorRest

:- pred interface_method_or_field_rest(ps::in, ps::out) is semidet.

% :- pragma memo(interface_method_or_field_rest/2).
interface_method_or_field_rest -->
    ( if
        constant_declarator_rest
    then
        []
    else
        interface_method_declarator_rest
    ).

% MethodDeclaratorRest:
%                 FormalParameters BracketsOpt [throws QualifiedIdentifierList] ( MethodBody |   ;  )

:- pred method_declarator_rest(ps::in, ps::out) is semidet.

% :- pragma memo(method_declarator_rest/2).
method_declarator_rest -->
    formal_parameters,
    brackets_opt,
    optional(throws_qualified_identifier_list),
    method_body_or_semicolon.

:- pred method_body_or_semicolon(ps::in, ps::out) is semidet.

% :- pragma memo(method_body_or_semicolon/2).
method_body_or_semicolon -->
    ( if
        method_body
    then
        []
    else
        punct(";")
    ).

% VoidMethodDeclaratorRest:
%                 FormalParameters [throws QualifiedIdentifierList] ( MethodBody |   ;  )

:- pred void_method_declarator_rest(ps::in, ps::out) is semidet.

% :- pragma memo(void_method_declarator_rest/2).
void_method_declarator_rest -->
    formal_parameters,
    optional(throws_qualified_identifier_list),
    method_body_or_semicolon.

% InterfaceMethodDeclaratorRest:
%         FormalParameters BracketsOpt [throws QualifiedIdentifierList]   ;

:- pred interface_method_declarator_rest(ps::in, ps::out) is semidet.

% :- pragma memo(interface_method_declarator_rest/2).
interface_method_declarator_rest -->
    formal_parameters,
    brackets_opt,
    optional(throws_qualified_identifier_list),
    punct(";").

% VoidInterfaceMethodDeclaratorRest:
%         FormalParameters [throws QualifiedIdentifierList]   ;

:- pred void_interface_method_declarator_rest(ps::in, ps::out) is semidet.

% :- pragma memo(void_interface_method_declarator_rest/2).
void_interface_method_declarator_rest -->
    formal_parameters, optional(throws_qualified_identifier_list), punct(";").

% ConstructorDeclaratorRest:
%         FormalParameters [throws QualifiedIdentifierList] MethodBody

:- pred constructor_declarator_rest(ps::in, ps::out) is semidet.

% :- pragma memo(constructor_declarator_rest/2).
constructor_declarator_rest -->
    formal_parameters, optional(throws_qualified_identifier_list), method_body.

:- pred throws_qualified_identifier_list(ps::in, ps::out) is semidet.

% :- pragma memo(throws_qualified_identifier_list/2).
throws_qualified_identifier_list -->
    keyword("throws"), qualified_identifier_list.

% QualifiedIdentifierList:
%         QualifiedIdentifier {  ,   QualifiedIdentifier}

:- pred qualified_identifier_list(ps::in, ps::out) is semidet.

% :- pragma memo(qualified_identifier_list/2).
qualified_identifier_list -->
    comma_separated_list(qualified_identifier).

% FormalParameters:
%         ( [FormalParameter { , FormalParameter}] )

:- pred formal_parameters(ps::in, ps::out) is semidet.

% :- pragma memo(formal_parameters/2).
formal_parameters -->
    brackets("(", optional(comma_separated_list(formal_parameter)), ")").

% FormalParameter:
%         [final] Type VariableDeclaratorId

:- pred formal_parameter(ps::in, ps::out) is semidet.

% :- pragma memo(formal_parameter/2).
formal_parameter -->
    optional(keyword("final")), java_type, variable_declarator_id.

% MethodBody:
%         Block

:- pred method_body(ps::in, ps::out) is semidet.

% :- pragma memo(method_body/2).
method_body -->
    block.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
