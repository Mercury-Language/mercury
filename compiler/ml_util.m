%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_util.m
% Main author: fjh, trd

% This module contains utility predicates for manipulating the MLDS.

%-----------------------------------------------------------------------------%

:- module ml_util.
:- interface.

:- import_module mlds.

%-----------------------------------------------------------------------------%
%
% Various utility routines used for MLDS manipulation.
%

	% return `true' if the statement is a tail call which
	% can be optimized into a jump back to the start of the
	% function
:- pred can_optimize_tailcall(mlds__qualified_entity_name, mlds__stmt).
:- mode can_optimize_tailcall(in, in) is semidet.


	% nondeterministically generates sub-statements from statements.
:- pred statements_contains_statement(mlds__statements, mlds__statement).
:- mode statements_contains_statement(in, out) is nondet.

:- pred statement_contains_statement(mlds__statement, mlds__statement).
:- mode statement_contains_statement(in, out) is multi.

:- pred stmt_contains_statement(mlds__stmt, mlds__statement).
:- mode stmt_contains_statement(in, out) is nondet.

	% defn_contains_foreign_code(NativeTargetLang, Defn):
	%	Succeeds iff this definition contains target_code
	%	statements in a target language other than the
	%	specified native target language.
:- pred defn_contains_foreign_code(target_lang, mlds__defn).
:- mode defn_contains_foreign_code(in, in) is semidet.

	% Succeeds iff this definition is a type definition.
:- pred defn_is_type(mlds__defn).
:- mode defn_is_type(in) is semidet.

	% Succeeds iff this definition is a function definition.
:- pred defn_is_function(mlds__defn).
:- mode defn_is_function(in) is semidet.

	% Succeeds iff this definition is a data definition which
	% defines a type_ctor_info constant.
:- pred defn_is_type_ctor_info(mlds__defn).
:- mode defn_is_type_ctor_info(in) is semidet.

	% Succeeds iff this definition is a data definition which
	% defines a variable whose type is mlds__commit_type.
:- pred defn_is_commit_type_var(mlds__defn).
:- mode defn_is_commit_type_var(in) is semidet.

	% Succeeds iff this definition has `public' in the access
	% field in its decl_flags.
:- pred defn_is_public(mlds__defn).
:- mode defn_is_public(in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module rtti.
:- import_module bool, list, std_util.

can_optimize_tailcall(Name, Call) :-
	Call = call(_Signature, FuncRval, MaybeObject, _CallArgs,
		_Results, IsTailCall),
	%
	% check if this call can be optimized as a tail call
	%
	IsTailCall = tail_call,

	%
	% check if the callee adddress is the same as
	% the caller
	%
	FuncRval = const(code_addr_const(CodeAddr)),
	(	
		CodeAddr = proc(QualifiedProcLabel, _Sig),
		MaybeSeqNum = no
	;
		CodeAddr = internal(QualifiedProcLabel, SeqNum, _Sig),
		MaybeSeqNum = yes(SeqNum)
	),
	QualifiedProcLabel = qual(ModuleName, PredLabel - ProcId),
	% check that the module name matches
	Name = qual(ModuleName, FuncName),
	% check that the PredLabel, ProcId, and MaybeSeqNum match
	FuncName = function(PredLabel, ProcId, MaybeSeqNum, _),

	%
	% In C++, `this' is a constant, so our usual technique
	% of assigning the arguments won't work if it is a
	% member function.  Thus we don't do this optimization
	% if we're optimizing a member function call
	%
	MaybeObject = no.

%-----------------------------------------------------------------------------%

statements_contains_statement(Statements, SubStatement) :-
	list__member(Statement, Statements),
	statement_contains_statement(Statement, SubStatement).

:- pred maybe_statement_contains_statement(maybe(mlds__statement),
		mlds__statement).
:- mode maybe_statement_contains_statement(in, out) is nondet.

maybe_statement_contains_statement(no, _Statement) :- fail.
maybe_statement_contains_statement(yes(Statement), SubStatement) :-
	statement_contains_statement(Statement, SubStatement).


statement_contains_statement(Statement, Statement).
statement_contains_statement(Statement, SubStatement) :-
	Statement = mlds__statement(Stmt, _Context),
	stmt_contains_statement(Stmt, SubStatement).


stmt_contains_statement(Stmt, SubStatement) :-
	(
		Stmt = block(_Defns, Statements),
		statements_contains_statement(Statements, SubStatement)
	;
		Stmt = while(_Rval, Statement, _Once),
		statement_contains_statement(Statement, SubStatement)
	;
		Stmt = if_then_else(_Cond, Then, MaybeElse),
		( statement_contains_statement(Then, SubStatement)
		; maybe_statement_contains_statement(MaybeElse, SubStatement)
		)
	;
		Stmt = switch(_Type, _Val, _Range, Cases, Default),
		( cases_contains_statement(Cases, SubStatement)
		; default_contains_statement(Default, SubStatement)
		)
	;
		Stmt = label(_Label),
		fail
	;
		Stmt = goto(_),
		fail
	;
		Stmt = computed_goto(_Rval, _Labels),
		fail
	;
		Stmt = call(_Sig, _Func, _Obj, _Args, _RetLvals, _TailCall),
		fail
	;
		Stmt = return(_Rvals),
		fail
	;
		Stmt = do_commit(_Ref),
		fail
	;
		Stmt = try_commit(_Ref, Statement, Handler),
		( statement_contains_statement(Statement, SubStatement)
		; statement_contains_statement(Handler, SubStatement)
		)
	;
		Stmt = atomic(_AtomicStmt),
		fail
	).

:- pred cases_contains_statement(list(mlds__switch_case), mlds__statement).
:- mode cases_contains_statement(in, out) is nondet.

cases_contains_statement(Cases, SubStatement) :-
	list__member(Case, Cases),
	Case = _MatchCond - Statement,
	statement_contains_statement(Statement, SubStatement).

:- pred default_contains_statement(mlds__switch_default, mlds__statement).
:- mode default_contains_statement(in, out) is nondet.

default_contains_statement(default_do_nothing, _) :- fail.
default_contains_statement(default_is_unreachable, _) :- fail.
default_contains_statement(default_case(Statement), SubStatement) :-
	statement_contains_statement(Statement, SubStatement).

%-----------------------------------------------------------------------------%

defn_contains_foreign_code(NativeTargetLang, Defn) :-
	Defn = mlds__defn(_Name, _Context, _Flags, Body),
	Body = function(_, _, yes(FunctionBody)),
	statement_contains_statement(FunctionBody, Statement),
	Statement = mlds__statement(Stmt, _),
	Stmt = atomic(target_code(TargetLang, _)),
	TargetLang \= NativeTargetLang.

defn_is_type(Defn) :-
	Defn = mlds__defn(Name, _Context, _Flags, _Body),
	Name = type(_, _).

defn_is_function(Defn) :-
	Defn = mlds__defn(Name, _Context, _Flags, _Body),
	Name = function(_, _, _, _).

defn_is_type_ctor_info(Defn) :-
	Defn = mlds__defn(_Name, _Context, _Flags, Body),
	Body = mlds__data(Type, _),
	Type = mlds__rtti_type(RttiName),
	RttiName = type_ctor_info.

defn_is_commit_type_var(Defn) :-
	Defn = mlds__defn(_Name, _Context, _Flags, Body),
	Body = mlds__data(Type, _),
	Type = mlds__commit_type.

defn_is_public(Defn) :-
	Defn = mlds__defn(_Name, _Context, Flags, _Body),
	access(Flags) \= private.

%-----------------------------------------------------------------------------%
