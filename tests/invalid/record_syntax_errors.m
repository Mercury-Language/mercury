:- module record_syntax_errors.

:- interface.

:- type exist_cons
	---> some [T] exist_cons(field1::int, field2::T, field3::T).

:- type cons
	--->	cons(field4::int, field5::int, field6::cons2).

:- type cons2
	--->	cons2(field7::int, field8::int).

:- pred dcg_syntax(cons::in, cons::out) is det.

:- pred dcg_syntax_2(cons::in, cons::out) is det.

:- pred dcg_type_error(cons::in, cons::out) is det.

:- pred construct_exist_cons(exist_cons::out) is det.

:- pred arg_type_error(cons::out) is det.
:- mode field8(in) = out is det.
:- implementation.

dcg_syntax -->
	{ Field = field4 },
	^ Field := 1.

dcg_syntax_2 -->
	X := Y.

dcg_type_error -->
	^ field4 := 2.

construct_exist_cons(ExistCons) :-
	ExistCons0 = 'new exist_cons'(1, 'b', 'c'),
	% This field cannot be updated because it shares
	% an existentially quantified type variable
	% with another field - updating the field changes
	% the type of the field.
	ExistCons = ExistCons0 ^ field2 := 1.

arg_type_error(Cons) :-
	Cons0 = cons(1, 2, cons2(3, 4)),
	Cons = Cons0 ^ field6 ^ field7 := "invalid value".

term_type_error(Cons) :-
	Cons0 = cons(1, 2, cons2(3, 4)),
	Cons = Cons0 ^ field6 ^ field4 := 1.

% Check error message for local declarations for access functions
% for exported fields.
:- func field4(cons) = int.

% Check error message for clauses for automatically generated access functions.
field4(_) = 1.

