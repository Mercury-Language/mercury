% There is one tricky aspect with exception__try_store.  If we're not
% careful, the user could use `store__extract_ref_value', which destroys
% the store and extracts the referenced value without making a copy.
% The user could then throw the extracted value, and if the handler gets
% both the extracted value and a unique version of the store, then it
% can update the reference, which would modify the extracted value,
% breaking referential transparency.
%
% In other words, with a naive implementation of `try_store',
% the following program could print out 
% 
% 	Result = exception(univ_cons("initial"))
% 	Result = exception(univ_cons("updated"))
%	...
% 
% To avoid this, the implementation of try_store must make a copy of the
% thrown object before returning it from try_store.
%
% This test case checks that the implementation of try_store does the
% right thing in this tricky case.

:- module tricky_try_store.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module exception, store, list.

:- pred tricky(store_ref(T, S), int, store(S), store(S)).
:- mode tricky(in, out, di, uo) is det.

tricky(Key, _Output, Store0, _Store) :-
	store__extract_ref_value(Store0, Key, Value),
	throw(Value).

main -->
	{ store__new(Store0) },
	{ store__new_ref(mklist("initial"), Key, Store0, Store1) },
	{ store__arg_ref(Key, 0, SubKey, Store1, Store2) },
	{ exception__try_store(tricky(Key), Result, Store2, Store3) },
	print("Result = "), print(Result), nl,
	{ store__set_ref_value(SubKey, "updated", Store3, Store) },
	print("Result = "), print(Result), nl,
	{ store__extract_ref_value(Store, Key, Val) },
	print("Val = "), print(Val), nl.

% XXX the current compiler has a bug whereby it generates static ground terms
%     even for things that are used in `di' modes.  To avoid that bug,
%     we use the following hack -- a `pragma no_inline' function --
%     to ensure that `["initial"]' doesn't get stored as a static constant.
:- func mklist(T) = list(T).
:- mode mklist(di) = uo is det.
:- pragma no_inline(mklist/1).
mklist(X) = [X].
