:- module simple_code.
:- interface.
:- pred p(int::in, int::out) is erroneous.
:- implementation.

:- import_module require.
p --> 
	(
		[]
	;
		{ error("foo") }
	),
	( { true } ->
		{ Z = 2 }	
	;
		{ Z = 3 }
	),
	( { X = 3, X = 2, Z = 2 } ->
		[]
	;
		[]
	),
	( { \+ true } ->
		[]
	;
		[]
	),
	( { \+ det_pred } ->
		[]
	;	
		[]
	),
	( { \+ fail_pred } ->
		[]
	;
		[]
	),
	{ \+ fail },
	{ obsolete },
	( { error("blah") } ->
		[]
	;
		[]
	).

:- pred det_pred is det.

det_pred.

:- pred fail_pred is failure.

fail_pred :- fail.

:- pred obsolete is det.
:- pragma obsolete(obsolete/0).

obsolete.
