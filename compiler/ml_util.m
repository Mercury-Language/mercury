%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000 The University of Melbourne.
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

%-----------------------------------------------------------------------------%

:- implementation.

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



statements_contains_statement(Statements, SubStatement) :-
	list__member(Statement, Statements),
	statement_contains_statement(Statement, SubStatement).

:- pred maybe_statement_contains_statement(maybe(mlds__statement), mlds__statement).
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

%-----------------------------------------------------------------------------%
