:- module fail_ite.
:- interface.
:- import_module io.
:- pred p(io__state::di, io__state::uo) is erroneous.
:- implementation.
:- import_module require.
p --> 
	( { \+ fail_pred } ->
		[]
	;
		[]
	),
	( { \+ det_pred } ->
		[]
	;	
		[]
	),	
	( { error("blah") } ->
		[]
	;
		[]
	).


:- pred det_pred is det.

det_pred.

:- pred fail_pred is failure.

fail_pred :- fail.
