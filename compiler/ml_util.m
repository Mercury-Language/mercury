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
:- import_module list, std_util.
:- import_module prog_data.  % for foreign_language

%-----------------------------------------------------------------------------%
	% succeeds iff the definitions contain the entry point to
	% the a main predicate.
	%
:- pred defns_contain_main(mlds__defns).
:- mode defns_contain_main(in) is semidet.

%-----------------------------------------------------------------------------%
	% return `true' if the statement is a tail call which
	% can be optimized into a jump back to the start of the
	% function
:- pred can_optimize_tailcall(mlds__qualified_entity_name, mlds__stmt).
:- mode can_optimize_tailcall(in, in) is semidet.

%-----------------------------------------------------------------------------%
%
% routines that deal with statements
%

	% nondeterministically generates sub-statements from statements.
:- pred statements_contains_statement(mlds__statements, mlds__statement).
:- mode statements_contains_statement(in, out) is nondet.

:- pred statement_contains_statement(mlds__statement, mlds__statement).
:- mode statement_contains_statement(in, out) is multi.

:- pred stmt_contains_statement(mlds__stmt, mlds__statement).
:- mode stmt_contains_statement(in, out) is nondet.

:- pred has_foreign_languages(mlds__statement, list(foreign_language)).
:- mode has_foreign_languages(in, out) is det.

%-----------------------------------------------------------------------------%
%
% routines that deal with definitions
%

	% defn_contains_foreign_code(NativeTargetLang, Defn):
	%	Succeeds iff this definition contains outline_foreign_proc
	%	statements, or inline_target_code statements in a target
	%	language other than the specified native target language.
	%	XXX perhaps we should eliminate the need to check for
	%	inline_target_code, because it shouldn't be generated
	%	with target language different to the native target
	%	language in the long run.
:- pred defn_contains_foreign_code(target_lang, mlds__defn).
:- mode defn_contains_foreign_code(in, in) is semidet.

	% defn_contains_foreign_code(ForeignLang, Defn):
	%	Succeeds iff this definition contains outline_foreign_proc
	%	statements for the given foreign language.
:- pred defn_contains_outline_foreign_proc(foreign_language, mlds__defn).
:- mode defn_contains_outline_foreign_proc(in, in) is semidet.

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
%
% routines that deal with lvals/rvals
%

	%
	% initializer_contains_var:
	% rvals_contains_var:
	% maybe_rval_contains_var:
	% rval_contains_var:
	% lvals_contains_var:
	% lval_contains_var:
	%	Succeeds iff the specified construct contains a reference to
	%	the specified variable.
	%

:- pred initializer_contains_var(mlds__initializer, mlds__var).
:- mode initializer_contains_var(in, in) is semidet.

:- pred rvals_contains_var(list(mlds__rval), mlds__var).
:- mode rvals_contains_var(in, in) is semidet.

:- pred maybe_rval_contains_var(maybe(mlds__rval), mlds__var).
:- mode maybe_rval_contains_var(in, in) is semidet.

:- pred rval_contains_var(mlds__rval, mlds__var).
:- mode rval_contains_var(in, in) is semidet.

:- pred lvals_contains_var(list(mlds__lval), mlds__var).
:- mode lvals_contains_var(in, in) is semidet.

:- pred lval_contains_var(mlds__lval, mlds__var).
:- mode lval_contains_var(in, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module rtti.
:- import_module bool, list, std_util, prog_data.

%-----------------------------------------------------------------------------%

defns_contain_main(Defns) :-
	list__member(Defn, Defns),
	Defn = mlds__defn(Name, _, _, _),
	Name = function(FuncName, _, _, _), 
	FuncName = pred(predicate, _, "main", 2, _, _).

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
%
% routines that deal with statements
%

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

has_foreign_languages(Statement, Langs) :-
	GetTargetCode = (pred(Lang::out) is nondet :-
		statement_contains_statement(Statement, SubStatement),
		SubStatement = statement(atomic(
		  	outline_foreign_proc(Lang, _, _)), _) 
		),
	solutions(GetTargetCode, Langs).

%-----------------------------------------------------------------------------%
%
% routines that deal with definitions
%

defn_contains_foreign_code(NativeTargetLang, Defn) :-
	Defn = mlds__defn(_Name, _Context, _Flags, Body),
	Body = function(_, _, defined_here(FunctionBody), _),
	statement_contains_statement(FunctionBody, Statement),
	Statement = mlds__statement(Stmt, _),
	( 
		Stmt = atomic(inline_target_code(TargetLang, _)),
		TargetLang \= NativeTargetLang
	; 
		Stmt = atomic(outline_foreign_proc(_, _, _))
	).

defn_contains_outline_foreign_proc(ForeignLang, Defn) :-
	Defn = mlds__defn(_Name, _Context, _Flags, Body),
	Body = function(_, _, defined_here(FunctionBody), _),
	statement_contains_statement(FunctionBody, Statement),
	Statement = mlds__statement(Stmt, _),
	Stmt = atomic(outline_foreign_proc(ForeignLang, _, _)).

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
	access(Flags) = public.

%-----------------------------------------------------------------------------%
%
% routines that deal with lvals/rvals
%

%
% initializer_contains_var:
% rvals_contains_var:
% maybe_rval_contains_var:
% rval_contains_var:
% lvals_contains_var:
% lval_contains_var:
%	Succeeds iff the specified construct contains a reference to
%	the specified variable.
%

% initializer_contains_var(no_initializer, _) :- fail.
initializer_contains_var(init_obj(Rval), Name) :-
	rval_contains_var(Rval, Name).
initializer_contains_var(init_struct(Inits), Name) :-
	list__member(Init, Inits),
	initializer_contains_var(Init, Name).
initializer_contains_var(init_array(Inits), Name) :-
	list__member(Init, Inits),
	initializer_contains_var(Init, Name).

rvals_contains_var(Rvals, Name) :-
	list__member(Rval, Rvals),
	rval_contains_var(Rval, Name).

% maybe_rval_contains_var(no, _Name) :- fail.
maybe_rval_contains_var(yes(Rval), Name) :-
	rval_contains_var(Rval, Name).

rval_contains_var(lval(Lval), Name) :-
	lval_contains_var(Lval, Name).
rval_contains_var(mkword(_Tag, Rval), Name) :-
	rval_contains_var(Rval, Name).
% rval_contains_var(const(_Const), _Name) :- fail.
rval_contains_var(unop(_Op, Rval), Name) :-
	rval_contains_var(Rval, Name).
rval_contains_var(binop(_Op, X, Y), Name) :-
	( rval_contains_var(X, Name)
	; rval_contains_var(Y, Name)
	).
rval_contains_var(mem_addr(Lval), Name) :-
	lval_contains_var(Lval, Name).

lvals_contains_var(Lvals, Name) :-
	list__member(Lval, Lvals),
	lval_contains_var(Lval, Name).

lval_contains_var(field(_MaybeTag, Rval, _FieldId, _, _), Name) :-
	rval_contains_var(Rval, Name).
lval_contains_var(mem_ref(Rval, _Type), Name) :-
	rval_contains_var(Rval, Name).
lval_contains_var(var(Name, _Type), Name).  /* this is where we can succeed! */

%-----------------------------------------------------------------------------%
