%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: mercury_builtin.nl.
% Main author: fjh.

% This file is automatically imported into every module.
% It is intended for things that are part of the language,
% but which are implemented just as normal user-level code
% rather than with special coding in the compiler.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mercury_builtin.
:- interface.

%-----------------------------------------------------------------------------%

% TYPES.

% The types `character', `int', `float', and `string',
% and the types `pred', `pred(T)', `pred(T1, T2)', `pred(T1, T2, T3)', ...
% are builtin and are implemented using special code in the
% type-checker.  (XXX TODO: report an error for attempts to redefine
% these types.)

%-----------------------------------------------------------------------------%

% INSTS.

% The standard insts `free', `ground', and `bound(...)' are builtin
% and are implemented using special code in the parser and mode-checker.

% Unique insts.  Currently aliased to standard insts, since unique insts
% aren't implemented yet.  Note that `bound_unique' is aliased to `bound'
% in the parser (prog_io.nl), because it couldn't be done here.

:- inst free_unique = free.
:- inst ground_unique = ground.
:- inst dead = ground.

%-----------------------------------------------------------------------------%

% MODES.

% The standard modes.

:- mode unused :: (free -> free).
:- mode output :: (free -> ground).
:- mode input :: (ground -> ground).

:- mode in :: (ground -> ground).
:- mode out :: (free -> ground).

:- mode in(Inst) :: (Inst -> Inst).
:- mode out(Inst) :: (free -> Inst).

% Unique modes.  Currently aliased to standard modes, since unique modes
% aren't implemented yet.

:- mode di :: input.
:- mode uo :: output.
:- mode ui :: input.

% Higher-order predicate modes.
% This needs to be builtin - the following is just a temporary hack.

:- mode det_pred :: output.
:- mode det_pred(_, _) :: output.
:- mode det_pred(_, _, _, _) :: output.
:- mode det_pred(_, _, _, _, _, _) :: output.

:- mode semidet_pred :: output.
:- mode semidet_pred(_, _) :: output.
:- mode semidet_pred(_, _, _, _) :: output.
:- mode semidet_pred(_, _, _, _, _, _) :: output.

:- mode nondet_pred :: output.
:- mode nondet_pred(_, _) :: output.
:- mode nondet_pred(_, _, _, _) :: output.
:- mode nondet_pred(_, _, _, _, _, _) :: output.

%-----------------------------------------------------------------------------%

% PREDICATES.

% We define !/0 (and !/2 for dcgs) to be equivalent to `true'.  This is for
% backwards compatibility with Prolog systems.  But of course it only works
% if all your cuts are green cuts.

:- pred ! is det.

:- pred !(T, T).
:- mode !(in, out) is det.

% The call/N family.  Note that the compiler (make_hlds.nl) will transform
% goals which are not atoms (e.g. goals which are free variables) into
% calls to call/1.

:- pred call(pred).
:- mode call(in) is semidet.

:- pred call(pred(T), T).
:- mode call(in, in) is semidet.

:- pred call(pred(T1, T2), T1, T2).
:- mode call(in, in, in) is semidet.

% In addition, the following predicate-like constructs are builtin:
%
%	:- pred (T = T).
%	:- pred (T \= T).
%	:- pred (pred , pred).
%	:- pred (pred ; pred).
%	:- pred (\+ pred).
%	:- pred (not pred).
%	:- pred (pred -> pred).
%	:- pred (if pred then pred).
%	:- pred (if pred then pred else pred).
%	:- pred (pred => pred).
%	:- pred (pred <= pred).
%	:- pred (pred <=> pred).
%
%	(pred -> pred ; pred).
%	some Vars pred
%	all Vars pred

%-----------------------------------------------------------------------------%

:- type comparison_result ---> (=) ; (<) ; (>).

:- pred unify(T::in, T::in) is semidet.

:- pred compare(comparison_result::out, T::in, T::in) is det.

% The following are used by the compiler, to implement polymorphism.
% They should not be used in programs.

:- pred index(T::in, int::out) is det.

:- pred builtin_unify_int(int::in, int::in) is semidet.
:- pred builtin_index_int(int::in, int::out) is det.
:- pred builtin_compare_int(comparison_result::out, int::in, int::in) is det.

:- pred builtin_unify_string(string::in, string::in) is semidet.
:- pred builtin_index_string(string::in, int::out) is det.
:- pred builtin_compare_string(comparison_result::out, string::in, string::in)
	is det.

/***** float not yet implemented
:- pred builtin_unify_float(float::in, float::in) is semidet.
:- pred builtin_index_float(float::in, int::out) is det.
:- pred builtin_compare_float(comparison_result::out, float::in, float::in)
	is det.
*****/

:- pred builtin_unify_pred((pred)::in, (pred)::in) is semidet.
:- pred builtin_index_pred((pred)::in, int::out) is det.
:- pred builtin_compare_pred(comparison_result::out, (pred)::in, (pred)::in)
	is det.

:- pred compare_error is erroneous.

:- type type_info(T) ---> type_info.

% These should be defined in int.nl, but we define them here since
% they're need for the implementation of compare/3.

:- pred <(int, int).
:- mode <(in, in) is semidet.

:- pred >(int, int).
:- mode >(in, in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, std_util.

% Many of the predicates defined in this module are builtin -
% the compiler generates code for them inline.

%-----------------------------------------------------------------------------%

!.
!(X, X).

%-----------------------------------------------------------------------------%

	% A temporary hack until we implement call/N (N>1) properly
	% The way this works is magic ;-)

call(Pred, T) :-
	call(call(Pred, T)).

call(Pred, T1, T2) :-
	call(call(Pred, T1, T2)).

%-----------------------------------------------------------------------------%

:- external(unify/2).
:- external(index/2).
:- external(compare/3).

builtin_unify_int(X, X).

builtin_index_int(X, X).

builtin_compare_int(R, X, Y) :-
	( X < Y ->
		R = (<)
	; X = Y ->
		R = (=)
	;
		R = (>)
	).

builtin_unify_string(S, S).

builtin_index_string(_, -1).

builtin_compare_string(R, S1, S2) :-
	builtin_strcmp(Res, S1, S2),
	( Res < 0 ->
		R = (<)
	; Res = 0 ->
		R = (=)
	;
		R = (>)
	).

:- pred builtin_strcmp(int, string, string).
:- mode builtin_strcmp(out, in, in) is det.

:- external(builtin_strcmp/3).

builtin_unify_pred(_Pred1, _Pred2) :-
	semidet_succeed,	% suppress determinism warning
	error("attempted unification of higher-order predicate terms").

builtin_compare_pred(_Res, _Pred1, _Pred2) :-
	error("attempted comparison of higher-order predicate terms").

builtin_index_pred(_, -1).

	% This is used by the code that the compiler generates for compare/3.
compare_error :-
	error("internal error in compare/3").

:- end_module mercury_builtin.

%-----------------------------------------------------------------------------%
