:- module cgram.

:- interface.

:- parse(file/0, token, ('$'), xx, in, out).

:- type token --->
('!'); ('!='); ('$'); ('%');
('%='); ('&&'); ('&'); ('&=');
('('); (')'); ('*'); ('*=');
('+'); ('++'); ('+='); (',');
('-'); ('--'); ('-='); ('->');
('.'); ('...'); ('/'); ('/=');
(':'); (';'); ('<'); ('<<');
('<<='); ('<='); ('='); ('==');
('>'); ('>='); ('>>'); ('>>=');
('?'); ('['); (']'); ('^');
('^='); ('auto'); ('break'); ('case');
('char'); ('const'); ('continue'); ('default');
('do'); ('double'); ('else'); ('enum');
('extern'); ('float'); ('for'); ('goto');
('id'); ('if'); ('int'); ('literal'); ('long');
('register'); ('return'); ('short'); ('signed');
('sizeof'); ('static'); ('string'); ('struct');
('switch'); ('type_name'); ('typedef'); ('union');
('unsigned'); ('void'); ('volatile'); ('while');
('{'); ('|'); ('|='); ('||'); ('}'); ('~').

:- implementation.

:- rule primary_expr.
primary_expr --->
	  identifier
	; [literal]
	; [string]
	; ['('], expr, [')']
	.

:- rule postfix_expr.
postfix_expr --->
	  primary_expr
	; postfix_expr, [('[')], expr, [(']')]
	; postfix_expr, ['('], [')']
	; postfix_expr, ['('], argument_expr_list, [')']
	; postfix_expr, ['.'], identifier
	; postfix_expr, ['->'], identifier
	; postfix_expr, ['++']
	; postfix_expr, ['--']
	.

:- rule argument_expr_list.
argument_expr_list --->
	  assignment_expr
	; argument_expr_list, [','], assignment_expr
	.

:- rule unary_expr.
unary_expr --->
	  postfix_expr
	; ['++'], unary_expr
	; ['--'], unary_expr
	; unary_operator, cast_expr
	; [sizeof], unary_expr
	; [sizeof], ['('], type_name, [')']
	.

:- rule unary_operator.
unary_operator --->
	  ['&']
	; ['*']
	; ['+']
	; ['-']
	; ['~']
	; ['!']
	.

:- rule cast_expr.
cast_expr --->
	  unary_expr
	; ['('], type_name, [')'], cast_expr
	.

:- rule multiplicative_expr.
multiplicative_expr --->
	  cast_expr
	; multiplicative_expr, ['*'], cast_expr
	; multiplicative_expr, ['/'], cast_expr
	; multiplicative_expr, ['%'], cast_expr
	.

:- rule additive_expr.
additive_expr --->
	  multiplicative_expr
	; additive_expr, ['+'], multiplicative_expr
	; additive_expr, ['-'], multiplicative_expr
	.

:- rule shift_expr.
shift_expr --->
	  additive_expr
	; shift_expr, ['<<'], additive_expr
	; shift_expr, ['>>'], additive_expr
	.

:- rule relational_expr.
relational_expr --->
	  shift_expr
	; relational_expr, ['<'], shift_expr
	; relational_expr, ['>'], shift_expr
	; relational_expr, ['<='], shift_expr
	; relational_expr, ['>='], shift_expr
	.

:- rule equality_expr.
equality_expr --->
	  relational_expr
	; equality_expr, ['=='], relational_expr
	; equality_expr, ['!='], relational_expr
	.

:- rule and_expr.
and_expr --->
	  equality_expr
	; and_expr, ['&'], equality_expr
	.

:- rule exclusive_or_expr.
exclusive_or_expr --->
	  and_expr
	; exclusive_or_expr, ['^'], and_expr
	.

:- rule inclusive_or_expr.
inclusive_or_expr --->
	  exclusive_or_expr
	; inclusive_or_expr, ['|'], exclusive_or_expr
	.

:- rule logical_and_expr.
logical_and_expr --->
	  inclusive_or_expr
	; logical_and_expr, ['&&'], inclusive_or_expr
	.

:- rule logical_or_expr.
logical_or_expr --->
	  logical_and_expr
	; logical_or_expr, ['||'], logical_and_expr
	.

:- rule conditional_expr.
conditional_expr --->
	  logical_or_expr
	; logical_or_expr, ['?'], logical_or_expr, [':'], conditional_expr
	.

:- rule assignment_expr.
assignment_expr --->
	  conditional_expr
	; unary_expr, assignment_operator, assignment_expr
	.

:- rule assignment_operator.
assignment_operator --->
	  ['=']
	; ['*=']
	; ['/=']
	; ['%=']
	; ['+=']
	; ['-=']
	; ['<<=']
	; ['>>=']
	; ['&=']
	; ['^=']
	; ['|=']
	.

:- rule expr.
expr --->
	  assignment_expr
	; expr, [','], assignment_expr
	.

:- rule constant_expr.
constant_expr --->
	  conditional_expr
	.

:- rule declaration.
declaration --->
	  declaration_specifiers, [';']
	; declaration_specifiers, init_declarator_list, [';']
	.

:- action(declaration/0, handle_typedefs).

:- rule declaration_specifiers.
declaration_specifiers --->
	  storage_class_specifier
	; storage_class_specifier, declaration_specifiers
	; type_specifier
	; type_specifier, declaration_specifiers
	.

:- rule init_declarator_list.
init_declarator_list --->
	  init_declarator
	; init_declarator_list, [','], init_declarator
	.

:- rule init_declarator.
init_declarator --->
	  declarator
	; declarator, ['='], initializer
	.

:- rule storage_class_specifier.
storage_class_specifier --->
	  [typedef]
	; [extern]
	; [static]
	; [auto]
	; [register]
	.

:- rule type_specifier.
type_specifier --->
	  [char]
	; [short]
	; [int]
	; [long]
	; [signed]
	; [unsigned]
	; [float]
	; [double]
	; [const]
	; [volatile]
	; [void]
	; struct_or_union_specifier
	; enum_specifier
	; [type_name]
	.

:- rule struct_or_union_specifier.
struct_or_union_specifier --->
	  struct_or_union, identifier, ['{'], struct_declaration_list, ['}']
	; struct_or_union, ['{'], struct_declaration_list, ['}']
	; struct_or_union, identifier
	.

:- rule struct_or_union.
struct_or_union --->
	  [struct]
	; [union]
	.

:- rule struct_declaration_list.
struct_declaration_list --->
	  struct_declaration
	; struct_declaration_list, struct_declaration
	.

:- rule struct_declaration.
struct_declaration --->
	  type_specifier_list, struct_declarator_list, [';']
	.

:- rule struct_declarator_list.
struct_declarator_list --->
	  struct_declarator
	; struct_declarator_list, [','], struct_declarator
	.

:- rule struct_declarator.
struct_declarator --->
	  declarator
	; [':'], constant_expr
	; declarator, [':'], constant_expr
	.

:- rule enum_specifier.
enum_specifier --->
	  [enum], ['{'], enumerator_list, ['}']
	; [enum], identifier, ['{'], enumerator_list, ['}']
	; [enum], identifier
	.

:- rule enumerator_list.
enumerator_list --->
	  enumerator
	; enumerator_list, [','], enumerator
	.

:- rule enumerator.
enumerator --->
	  identifier
	; identifier, ['='], constant_expr
	.

:- rule declarator.
declarator --->
	  declarator2
	; pointer, declarator2
	.

:- rule declarator2.
declarator2 --->
	  identifier
	; ['('], declarator, [')']
	; declarator2, ['['], [']']
	; declarator2, ['['], constant_expr, [']']
	; declarator2, ['('], [')']
	; declarator2, ['('], parameter_type_list, [')']
	; declarator2, ['('], parameter_identifier_list, [')']
	.

:- rule pointer.
pointer --->
	  ['*']
	; ['*'], type_specifier_list
	; ['*'], pointer
	; ['*'], type_specifier_list, pointer
	.

:- rule type_specifier_list.
type_specifier_list --->
	  type_specifier
	; type_specifier_list, type_specifier
	.

:- rule parameter_identifier_list.
parameter_identifier_list --->
	  identifier_list
	; identifier_list, [','], ['...']
	.

:- rule identifier_list.
identifier_list --->
	  identifier
	; identifier_list, [','], identifier
	.

:- rule parameter_type_list.
parameter_type_list --->
	  parameter_list
	; parameter_list, [','], ['...']
	.

:- rule parameter_list.
parameter_list --->
	  parameter_declaration
	; parameter_list, [','], parameter_declaration
	.

:- rule parameter_declaration.
parameter_declaration --->
	  type_specifier_list, declarator
	; type_name
	. 

:- rule type_name.
type_name --->
	  type_specifier_list
	; type_specifier_list, abstract_declarator
	.

:- rule abstract_declarator.
abstract_declarator --->
	  pointer
	; abstract_declarator2
	; pointer, abstract_declarator2
	.

:- rule abstract_declarator2.
abstract_declarator2 --->
	  ['('], abstract_declarator, [')']
	; ['['], [']']
	; ['['], constant_expr, [']']
	; abstract_declarator2, ['['], [']']
	; abstract_declarator2, ['['], constant_expr, [']']
	; ['('], [')']
	; ['('], parameter_type_list, [')']
	; abstract_declarator2, ['('], [')']
	; abstract_declarator2, ['('], parameter_type_list, [')']
	.

:- rule initializer.
initializer --->
	  assignment_expr
	; ['{'], initializer_list, ['}']
	; ['{'], initializer_list, [','], ['}']
	.

:- rule initializer_list.
initializer_list --->
	  initializer
	; initializer_list, [','], initializer
	.

:- rule statement.
statement --->
	  labeled_statement
	; compound_statement
	; expression_statement
	; selection_statement
	; iteration_statement
	; jump_statement
	.

:- rule labeled_statement.
labeled_statement --->
	  identifier, [':'], statement
	; [case], constant_expr, [':'], statement
	; [default], [':'], statement
	.

:- rule compound_statement.
compound_statement --->
	  ['{'], ['}']
	; ['{'], statement_list, ['}']
	; ['{'], declaration_list, ['}']
	; ['{'], declaration_list, statement_list, ['}']
	.

:- rule declaration_list.
declaration_list --->
	  declaration
	; declaration_list, declaration
	.

:- rule statement_list.
statement_list --->
	  statement
	; statement_list, statement
	.

:- rule expression_statement.
expression_statement --->
	  [';']
	; expr, [';']
	.

:- rule selection_statement.
selection_statement --->
	  [if], ['('], expr, [')'], statement
	; [if], ['('], expr, [')'], statement, [else], statement
	; [switch], ['('], expr, [')'], statement
	.

:- rule iteration_statement.
iteration_statement --->
	  [while], ['('], expr, [')'], statement
	; [do], statement, [while], ['('], expr, [')'], [';']
	; [for], ['('], [';'], [';'], [')'], statement
	; [for], ['('], [';'], [';'], expr, [')'], statement
	; [for], ['('], [';'], expr, [';'], [')'], statement
	; [for], ['('], [';'], expr, [';'], expr, [')'], statement
	; [for], ['('], expr, [';'], [';'], [')'], statement
	; [for], ['('], expr, [';'], [';'], expr, [')'], statement % XXX
	; [for], ['('], expr, [';'], expr, [';'], [')'], statement
	; [for], ['('], expr, [';'], expr, [';'], expr, [')'], statement
	.

:- rule jump_statement.
jump_statement --->
	  [goto], identifier, [';']
	; [continue], [';']
	; [break], [';']
	; [return], [';']
	; [return], expr, [';']
	.

:- rule file.
file --->
	  external_definition
	; file, external_definition
	.

:- rule external_definition.
external_definition --->
	  function_definition
	; declaration
	.

:- rule function_definition.
function_definition --->
	  declarator, function_body
	; declaration_specifiers, declarator, function_body
	.

:- rule function_body.
function_body --->
	  compound_statement
	; declaration_list, compound_statement
	.

:- rule identifier.
identifier --->
	  [id]
	.

