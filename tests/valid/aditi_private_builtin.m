:- module aditi_private_builtin.

:- interface.

:- import_module io.
:- import_module aditi.

:- type relation_ticket == c_pointer.

	% do_call_returning_relation(ProcName, InputSchema, InputTuple,
	%		OutputRel).
	%
	% Call an Aditi procedure, returning a reference to the output
	% relation. InputTuple is a tuple containing the
	% input arguments. InputSchema is an Aditi schema string
	% describing the tuple of input arguments.
:- impure pred do_call_returning_relation(string, string,
			T, relation_ticket).
:- mode do_call_returning_relation(in, in, in, out) is det.

	% Find the single solution for a deterministic database call.
	% Abort the transaction if the call does not succeed at
	% least once.
	% InputTuple and OutputTuple must have type '{}/N' (the arity
	% depends on the relation being called).
:- impure pred do_det_call(string, string, InputTuple, OutputTuple).
:- mode do_det_call(in, in, in, out) is det.

:- impure pred do_semidet_call(string, string, InputTuple, OutputTuple).
:- mode do_semidet_call(in, in, in, out) is semidet.

:- impure pred do_nondet_call(string, string, InputTuple, OutputTuple).
:- mode do_nondet_call(in, in, in, out) is nondet.

:- impure pred do_multi_call(string, string, InputTuple, OutputTuple).
:- mode do_multi_call(in, in, in, out) is multi.

	% XXX I'm not sure whether it makes sense to have
	% committed choice Aditi predicates.
:- impure pred do_cc_nondet_call(string, string, InputTuple, OutputTuple).
:- mode do_cc_nondet_call(in, in, in, out) is cc_nondet.

:- impure pred do_cc_multi_call(string, string, InputTuple, OutputTuple).
:- mode do_cc_multi_call(in, in, in, out) is cc_multi.

:- impure pred do_erroneous_call(string, string, InputTuple, OutputTuple).
:- mode do_erroneous_call(in, in, in, out) is erroneous.

:- impure pred do_failure_call(string, string, InputTuple, OutputTuple).
:- mode do_failure_call(in, in, in, out) is failure.

	% do_insert_tuple(BaseRelationName, Tuple).
	%
	% TypeInfos is an array containing the type-infos for
	% the tuple to insert. TupleArgs contains the attribute
	% values of the tuple to insert.
:- pred do_insert_tuple(string, InputTuple, aditi__state, aditi__state).
:- mode do_insert_tuple(in, in, aditi_di, aditi_uo) is det.

	% do_delete_tuple(BaseRelationName, DeleteProcName,
	%	DeleteProcInputSchema, Tuple).
:- pred do_delete_tuple(string, string, string, Tuple,
		aditi__state, aditi__state).
:- mode do_delete_tuple(in, in, in, in, aditi_di, aditi_uo) is det.

:- type update_closure == pred(relation_ticket).
:- inst update_closure == (pred(out) is det).

	% do_bulk_insert(BaseRelationName, UpdateProcName, Closure).
:- pred do_bulk_insert(string, string, update_closure,
		aditi__state, aditi__state).
:- mode do_bulk_insert(in, in, in(update_closure), aditi_di, aditi_uo) is det.

	% do_bulk_delete(BaseRelationName, UpdateProcName, Closure).
:- pred do_bulk_delete(string, string, update_closure,
		aditi__state, aditi__state).
:- mode do_bulk_delete(in, in, in(update_closure), aditi_di, aditi_uo) is det.

	% do_bulk_modify(BaseRelationName, UpdateProcName, Closure).
:- pred do_bulk_modify(string, string, update_closure,
		aditi__state, aditi__state).
:- mode do_bulk_modify(in, in, in(update_closure), aditi_di, aditi_uo) is det.

