%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% File: aditi.m
% Main author: stayl
%
% This module provides an interface to the Aditi deductive database
% system developed at the University of Melbourne. See the 
% "Aditi deductive database interface" section of the Mercury
% Language Reference Manual (listed under "Implementation defined pragmas"
% in the "Pragmas" chapter) for details on how to compile database queries.
%
% For information on how to build programs which use this interface,
% see the example Mmakefile in $ADITI_HOME/demos/transactions. 
%
%
% Compilation grade notes (see the section "Compilation model options"
% in the Mercury User's Guide for more information):
%
%	This module requires a compilation grade with conservative garbage 
%	collection. Any grade containing `.gc' in its name, such as
%	`asm_fast.gc' or `asm_fast.gc.tr', will do.
%
% 	When trailing is not being used (the compilation grade does not
% 	contain `.tr'), resources will sometimes not be cleaned up until
%	the end of a transaction.
%	If there is a commit across a nondet database call, or an exception
%	is thrown, or a database call is retried in the debugger, the output
%	relation from the call and its cursor will not be cleaned up until the
%	transaction ends.
%	It is up to the programmer to decide whether imposing the overhead
%	of trailing on the rest of the program is worthwhile.
%
%	Compilation of this module in a high level C code grade (e.g. `hlc.gc')
%	is not yet supported.
%
%
% The transaction interface used here is described in
%	Kemp, Conway, Harris, Henderson, Ramamohanarao and Somogyi,
% 	"Database transactions in a purely declarative 
%		logic programming language", 
%	In Proceedings of the Fifth International Conference on Database
%	Systems for Advanced Applications, pp. 283-292.
%	Melbourne, Australia, 1-4 April, 1997.
%
%	This paper is also available as
%	Technical Report 96/45, Department of Computer Science, 
%	University of Melbourne, December 1996,
%	<http://www.cs.mu.OZ.AU/publications/tr_db/mu_96_45_cover.ps.gz>
%	and <http://www.cs.mu.OZ.AU/publications/tr_db/mu_96_45.ps.gz>.
%
%---------------------------------------------------------------------------%
:- module aditi.

:- interface.

:- import_module io.

:- type aditi__state.

:- inst aditi_unique = ground.
:- mode aditi_di :: in(aditi_unique).
:- mode aditi_uo :: out(aditi_unique).
:- mode aditi_mui :: in(aditi_unique).

:- type aditi__result(T)
	--->	ok(T)
	;	error(aditi__error, string).

:- type aditi__result
	--->	ok
	;	error(aditi__error, string).

:- type aditi__error
	--->	error_creating_client
	;	invalid_passwd
	;	too_many_connections
	;	invalid_ticket
	;	general_failure
	;	already_logged_in
	;	not_logged_in
	;	not_connected
	;	not_implemented
	;	abort
	;	bad_value
	;	bad_rl_code
	;	error_opening_relation
	;	security_violation
	;	unique_key_violation
	;	relation_or_cursor_not_open
	.

:- type aditi__error.

:- type aditi__connection.

	% aditi__connect(Host, User, Passwd, Result).
	%
	% Only one connection is allowed per process.
:- pred aditi__connect(string, string, string,
		aditi__result(aditi__connection), io__state, io__state).
:- mode aditi__connect(in, in, in, out, di, uo) is det.

:- pred aditi__disconnect(aditi__connection, aditi__result,
		io__state, io__state).
:- mode aditi__disconnect(in, out, di, uo) is det.

:- type aditi__transaction(T) == pred(T, aditi__state, aditi__state).
:- inst aditi__transaction = (pred(out, aditi_di, aditi_uo) is det).

	% aditi__transaction(Connection, Transaction, Result).
	%
	% Start a transaction with the Aditi database referred to by
	% Connection, call Transaction, returning ok(Result) if the
	% transaction is not aborted, or error(Error, Msg) if
	% the transaction fails.
	%
	% If Transaction throws an exception, the transaction will
	% be aborted and the exception will be rethrown to the caller.
	%
	% Predicates with `:- pragma aditi' or `:- pragma base_relation'
	% markers can only be called from within a transaction -- there
	% is no other way to get an `aditi__state' to pass to them.
:- pred aditi__transaction(aditi__connection, aditi__transaction(T),
		aditi__result(T), io__state, io__state).
:- mode aditi__transaction(in, in(aditi__transaction), out, di, uo) is det.

	% aditi__aggregate_compute_initial(Closure, UpdateAcc, 
	% 		ComputeInitial, Results)
	%
	% When called, the query Closure returns the relation to be 
	% aggregated over. This relation must have two attributes,
	% the first being the attribute to group by. The closure 
	% ComputeInitial computes an initial accumulator for each 
	% group given the first tuple in the group. The closure
	% UpdateAcc is called for each tuple in each group to 
	% update the accumulator. The outputs are the group-by element
	% and final accumulator for each group.
	%
	% For example, to compute a sum over relation `p/3' where
	% the first non-aditi__state attribute of `p' is the group-by
	% attribute:
	% 	aditi__aggregate_compute_initial(p(DB), 
	%		(pred(_::in, Attr::in, Acc0::in, Acc::out) is det :-
	%			Acc = Acc0 + Attr),
	%		(pred(_::in, _::in, 0::out) is det :- true),
	%		GrpBy, Sum).
:- pred aditi__aggregate_compute_initial(pred(GrpBy, NonGrpBy), 
		pred(GrpBy, NonGrpBy, Acc, Acc),
		pred(GrpBy, NonGrpBy, Acc), GrpBy, Acc).
:- mode aditi__aggregate_compute_initial(pred(out, out) is nondet, 
		pred(in, in, in, out) is det,
		pred(in, in, out) is det, out, out) is nondet.
:- mode aditi__aggregate_compute_initial(pred(out, out) is multi, 
		pred(in, in, in, out) is det,
		pred(in, in, out) is det, out, out) is multi.

/*
	This should be translated into the equivalent
	aggregate_compute_initial , but that hasn't been
	done yet. The main problem is collecting the initial
	value - it may not be constant.

	Also, it would be nice to provide versions of aditi__aggregate
	which work over one attribute relations, as in std_util__aggregate. 

	% aditi_aggregate_given_initial(Closure, UpdateAcc, 
	% 		InitialAcc, Results)
	% 
	% Same as aditi__aggregate_compute_initial except the initial
	% accumulator is supplied, rather than computed from the first
	% element.
:- pred aditi__aggregate_given_initial(pred(GrpBy, NonGrpBy), 
		pred(GrpBy, NonGrpBy, Acc, Acc), Acc, GrpBy, Acc).
:- mode aditi__aggregate_given_initial(pred(out, out) is nondet, 
		pred(in, in, in, out) is det,
		in, out, out) is nondet.
*/

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool, char, exception, list, require, std_util, string.

:- type aditi__connection == int.
:- type aditi__state ---> aditi__state.

	% These are handled by the RL code generator.
:- external(aditi__aggregate_compute_initial/5).
%:- external(aditi__aggregate_given_initial/5).

:- pragma c_header_code("

#include ""mercury_wrapper.h""
#include ""mercury_ho_call.h""

/* aditi_api_config.h must be included before aditi_clnt.h */
#include ""aditi_api_config.h""
#include ""aditi_clnt.h""

#define MADITI_check(status) MADITI_do_check(status, __LINE__);
#define MADITI_throw MR_longjmp(&MADITI_jmp_buf);

/* The timeout (in seconds) to use for a call to an Aditi procedure. */
#define MADITI_CALL_TIMEOUT 600

typedef enum { MADITI_INSERT_TUPLE, MADITI_DELETE_TUPLE } MADITI_insert_delete;
typedef enum { MADITI_INSERT, MADITI_DELETE, MADITI_MODIFY } MADITI_bulk_op;

/*
** Information used to clean up a call result if there is a commit
** or an exception across a database call.
*/
typedef struct {
	ticket *output_rel;
	ticket *output_cursor;
	int num_output_args;
	bool cleaned_up;
} MADITI_output_info;

static ticket MADITI_ticket;		/* Current connection ticket. */
static MR_jmp_buf MADITI_jmp_buf;	/* jmp_buf to longjmp() to when
					** aborting the transaction.
					*/
static int MADITI_status;		/* Return code of the last
					** Aditi API call.
					*/

static void MADITI_do_nondet_call(void);
static ticket * MADITI_run_procedure(String proc_name,
		String input_schema, String input_tuple);
static void MADITI_do_insert_delete_tuple(MADITI_insert_delete);
static void MADITI_do_bulk_operation(MADITI_bulk_op operation);
static void MADITI_do_bulk_insert_or_delete(MADITI_bulk_op operation,
		String rel_name, ticket *call_result_ticket);
static bool MADITI_get_output_tuple(int);
static void MADITI_post_call_cleanup(void);
static void MADITI_cleanup_call_output(MADITI_output_info *);

#ifdef MR_USE_TRAIL
static void MADITI_trail_cleanup_call_output(void *cleanup_data,
		MR_untrail_reason reason);
#endif

static String MADITI_construct_tuple(int num_input_args,
		Word *input_typeinfos, Word *input_args);
static void MADITI_do_check(int, int);
static void MADITI_list_rel(ticket* rel);
").

%-----------------------------------------------------------------------------%

aditi__connect(Host, User, Passwd, Result) -->
	aditi__connect_2(Host, User, Passwd, Status, Connection),
	{ Status = 0 ->
		Result = ok(Connection)
	;
		aditi__error_code(Status, Error, String),
		Result = error(Error, String)
	}.

:- pred aditi__connect_2(string::in, string::in, string::in, int::out,
		aditi__connection::out, io__state::di, io__state::uo) is det.
:- pragma c_code(
		aditi__connect_2(Host::in, User::in, Passwd::in, Stat::out,
			Connection::out, IO0::di, IO::uo),
			will_not_call_mercury,
"
{
	/* connect */
	if ((Stat = init_aditi_clnt()) == ADITI_OK
		&& (Stat = ADITI_NAME(recon)(Host)) == ADITI_OK) {

		DEBUG(printf(""connected\\n""));

		/*
		** Login and upload all the RL code for the program to 
		** the database.
		*/
		if ((Stat = ADITI_NAME(login)(User, Passwd)) == ADITI_OK) {

			DEBUG(ADITI_NAME(set_debug)());

			DEBUG(printf(""logged in\\n""));
			if ((Stat = MR_load_aditi_rl_code())
					== ADITI_OK) {
				DEBUG(printf(""code loaded\\n""));
			} else {
				ADITI_NAME(discon)(FORCE_LOGOUT);
			}
		} else {
			ADITI_NAME(discon)(FORCE_LOGOUT);
		}
	}
	Connection = 1;
	IO = IO0;
}
").

%-----------------------------------------------------------------------------%

aditi__disconnect(Connection, Result) -->
	aditi__disconnect_2(Connection, Status),
	{ Status = 0 ->
		Result = ok
	;
		aditi__error_code(Status, Error, Msg),
		Result = error(Error, Msg)
	}.

:- pred aditi__disconnect_2(aditi__connection::in, int::out,
		io__state::di, io__state::uo) is det.
:- pragma c_code(
		aditi__disconnect_2(_Connection::in, Stat::out,
			IO0::di, IO::uo), will_not_call_mercury,
"
{
	if ((Stat = ADITI_NAME(discon)(FORCE_LOGOUT)) == ADITI_OK) {
		Stat = fin_aditi_clnt();
	}
	IO = IO0;
}
").

%-----------------------------------------------------------------------------%

aditi__transaction(_Connection, Transaction, TransResult, IO0, IO) :-
	aditi__do_transaction(Transaction, Results,
		Status, GotException, Exception, IO0, IO),
	( GotException = 0 ->
		( Status = 0 ->
			TransResult = ok(Results)
		;
			aditi__error_code(Status, Error, String),
			TransResult = error(Error, String) 
		)
	;
		rethrow(exception(Exception))
	).

%-----------------------------------------------------------------------------%

:- pred aditi__do_transaction(aditi__transaction(T),
		T, int, int, univ, io__state, io__state).
:- mode aditi__do_transaction(in(aditi__transaction),
		out, out, out, out, di, uo) is det.
:- pragma no_inline(aditi__do_transaction/7).	% contains labels

:- pragma c_code(aditi__do_transaction(Transaction::in(aditi__transaction), 
		Results::out, Stat::out, GotException::out, Exception::out,
		IO0::di, IO::uo),
		may_call_mercury,
"
{
	Word DB;

	IO = IO0;

	DEBUG(printf(""starting transaction...""));
	if ((Stat = ADITI_NAME(trans_begin)(&MADITI_ticket))
			!= ADITI_OK) {
		goto transaction_done;
	}
	DEBUG(printf(""done\\n""));

	MR_setjmp(&MADITI_jmp_buf, transaction_error);

	MADITI__call_transaction(TypeInfo_for_T,
		Transaction, &Results, &GotException,
		&Exception, (Word)0, &DB);

	if (GotException == 0) {
		DEBUG(printf(""committing transaction...""));
		Stat = ADITI_NAME(trans_commit)(&MADITI_ticket);
		DEBUG(printf(""done\\n""));
	} else {
		DEBUG(printf(""got exception - aborting transaction...""));
		Stat = ADITI_NAME(trans_abort)(&MADITI_ticket);
		DEBUG(printf(""done\\n""));
	}
	
	goto transaction_done;

transaction_error:

	DEBUG(printf(""aborting transaction...""));
	ADITI_NAME(trans_abort)(&MADITI_ticket);
	DEBUG(printf(""done\\n""));
	GotException = 0;
	Stat = MADITI_status;

transaction_done:
}
").

:- pred aditi__call_transaction(aditi__transaction(T), T, int, univ,
		aditi__state, aditi__state).
:- mode aditi__call_transaction(in(aditi__transaction), out, out, out,
		aditi_di, aditi_uo) is cc_multi.
:- pragma export(
		aditi__call_transaction(in(aditi__transaction), out,
			out, out, aditi_di, aditi_uo),
		"MADITI__call_transaction"
	).

aditi__call_transaction(Trans, Results, GotException, Exception,
		State0, State) :-
	TryTransaction = 
		(pred(Result - AditiState::out) is det :-
			Trans(Result, State0, AditiState)
		),
	try(TryTransaction, ExceptionResult),
	(
		ExceptionResult = succeeded(Results - State),
		GotException = 0,
		make_dummy_value(Exception)
	;
		ExceptionResult = exception(Exception),
		State = State0,
		GotException = 1,
		make_dummy_value(Results)
	).	

	% Pretend to bind a variable. The result must never be looked at.
:- pred make_dummy_value(T::out) is det.
:- pragma c_code(make_dummy_value(T::out),
		[will_not_call_mercury, thread_safe],
		"T = 0;").

%-----------------------------------------------------------------------------%

:- pragma c_code("

	/*
	** We might allocate memory from may_call_mercury C code.
	*/
#ifndef MR_CONSERVATIVE_GC
#error ""The Aditi interface requires conservative garbage collection. \\
                Use a compilation grade containing .gc.""
#endif

#ifdef MR_HIGHLEVEL_CODE
#error ""The Aditi interface does not yet work in `hlc' grades""
#endif

").

	% Hand coded C to call an Aditi procedure. This is needed
	% because an Aditi call can have any number of arguments.
	% The alternative is to define this using nondet pragma C
	% and duplicate it for each Aditi procedure.
	% Data is sent to and received from Aditi as strings in the
	% normal Mercury term format, with the exception that
	% everything is written in prefix form.
:- pragma c_code("

/*
INIT mercury_sys_init_aditi_call
*/
/*
** This file handles calls to procedures compiled to Aditi-RL. 
** compiler/magic.m creates a goal which constructs type_infos for
** all the arguments, which are passed to a call to a predicate with
** an aditi_interface marker, which is generated as a call to do_call_aditi.
** do_call_aditi is hand-coded because the number of input and
** output arguments is not fixed.
**
** NOTE: changes here may require changes in compiler/magic.m and
** compiler/call_gen.m
**
** Input arguments to do_call_aditi:
** r1 -> string: procedure name
** r2 -> number of input arguments
** r3 -> input relation schema
** r4 -> number of output arguments
** r5... -> type_infos for input arguments
**       -> input arguments
**       -> type_infos for output arguments
*/

#define MADITI_proc_name			r1
#define MADITI_num_input_args			r2
#define MADITI_input_schema			r3
#define MADITI_num_output_args			r4

	/* Register containing the typeinfo for the first input argument */
#define MADITI_CALL_FIRST_INPUT 5

/*
** Output arguments from do_call_aditi are the same as for
** any predicate using the compact argument convention.
*/

/*
** Layout of nondet stack frame:
**
** MR_framevar(1) -> number of output arguments
** MR_framevar(2) -> output relation ticket
** MR_framevar(3) -> cursor for output relation
** MR_framevar(4...) -> type_infos for output arguments
*/

	/* Slots in frame apart from the typeinfos for the outputs */
#define MADITI_NUM_FRAME_VARS 1

#define MADITI_saved_output_info		MR_framevar(1)
#define MADITI_ADITI_FIRST_TYPEINFO		MADITI_NUM_FRAME_VARS + 1
#define MADITI_OUTPUT_TYPEINFO(i) \
			MR_framevar(MADITI_ADITI_FIRST_TYPEINFO + (i))

/*---------------------------------------------------------------------------*/
Define_extern_entry(do_nondet_aditi_call);
Declare_label(do_nondet_aditi_call_i1);
Define_extern_entry(do_semidet_aditi_call);
Define_extern_entry(do_det_aditi_call);
Define_extern_entry(do_aditi_insert);
Define_extern_entry(do_aditi_delete);
Define_extern_entry(do_aditi_bulk_insert);
Define_extern_entry(do_aditi_bulk_delete);
Define_extern_entry(do_aditi_bulk_modify);

MR_MAKE_PROC_LAYOUT(do_nondet_aditi_call, MR_DETISM_NON,
	MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN, MR_PREDICATE,
	""aditi"", ""do_nondet_aditi_call"", 0, 0);
MR_MAKE_INTERNAL_LAYOUT(do_nondet_aditi_call, 1);
MR_MAKE_PROC_LAYOUT(do_semidet_aditi_call, MR_DETISM_NON,
	MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN, MR_PREDICATE,
	""aditi"", ""do_semidet_aditi_call"", 0, 0);
MR_MAKE_PROC_LAYOUT(do_det_aditi_call, MR_DETISM_NON,
	MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN, MR_PREDICATE,
	""aditi"", ""do_det_aditi_call"", 0, 0);
MR_MAKE_PROC_LAYOUT(do_aditi_insert, MR_DETISM_DET,
	MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN, MR_PREDICATE,
	""aditi"", ""do_aditi_insert"", 0, 0);
MR_MAKE_PROC_LAYOUT(do_aditi_delete, MR_DETISM_DET,
	MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN, MR_PREDICATE,
	""aditi"", ""do_aditi_delete"", 0, 0);
MR_MAKE_PROC_LAYOUT(do_aditi_bulk_insert, MR_DETISM_DET,
	MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN, MR_PREDICATE,
	""aditi"", ""do_aditi_bulk_insert"", 0, 0);
MR_MAKE_PROC_LAYOUT(do_aditi_bulk_delete, MR_DETISM_DET,
	MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN, MR_PREDICATE,
	""aditi"", ""do_aditi_bulk_delete"", 0, 0);
MR_MAKE_PROC_LAYOUT(do_aditi_bulk_modify, MR_DETISM_DET,
	MR_ENTRY_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN, MR_PREDICATE,
	""aditi"", ""do_aditi_bulk_modify"", 0, 0);

BEGIN_MODULE(do_aditi_call_module)
	init_entry_sl(do_nondet_aditi_call);
	MR_INIT_PROC_LAYOUT_ADDR(do_nondet_aditi_call);
	init_label_sl(do_nondet_aditi_call_i1);
	init_entry_sl(do_semidet_aditi_call);
	MR_INIT_PROC_LAYOUT_ADDR(do_semidet_aditi_call);
	init_entry_sl(do_det_aditi_call);
	MR_INIT_PROC_LAYOUT_ADDR(do_det_aditi_call);
	init_entry_sl(do_aditi_insert);
	MR_INIT_PROC_LAYOUT_ADDR(do_aditi_insert);
	init_entry_sl(do_aditi_delete);
	MR_INIT_PROC_LAYOUT_ADDR(do_aditi_delete);
	init_entry_sl(do_aditi_bulk_insert);
	MR_INIT_PROC_LAYOUT_ADDR(do_aditi_bulk_insert);
	init_entry_sl(do_aditi_bulk_delete);
	MR_INIT_PROC_LAYOUT_ADDR(do_aditi_bulk_delete);
	init_entry_sl(do_aditi_bulk_modify);
	MR_INIT_PROC_LAYOUT_ADDR(do_aditi_bulk_modify);
BEGIN_CODE

Define_entry(do_nondet_aditi_call);
{
	MR_mkframe(""do_nondet_aditi_call"",
		(MADITI_NUM_FRAME_VARS + MADITI_num_output_args),
		LABEL(do_nondet_aditi_call_i1));

	save_transient_registers();
	DEBUG(printf(""do_nondet_aditi_call\\n""));
	MADITI_do_nondet_call();
	restore_transient_registers();
	GOTO(LABEL(do_nondet_aditi_call_i1));
}
Define_label(do_nondet_aditi_call_i1);
{
	save_transient_registers();

	/* Unpack the output tuple into r1 and upwards. */
	if (MADITI_get_output_tuple(1)) {
		restore_transient_registers();
		MR_succeed();
	} else {
		MADITI_post_call_cleanup();
		restore_transient_registers();
		MR_fail();
	}
}

Define_entry(do_semidet_aditi_call);
{
	MR_mkframe(""do_semidet_aditi_call"",
		(MADITI_NUM_FRAME_VARS + MADITI_num_output_args),
		ENTRY(MR_do_not_reached));

	save_transient_registers();

	DEBUG(printf(""do_semidet_aditi_call\\n""));
	MADITI_do_nondet_call();

	/* Unpack the output tuple into r2 and upwards for semidet code. */
	if (MADITI_get_output_tuple(2)) {
		/*
		** We don't check that there is only one solution because
		** duplicates may not have been removed.
		** We assume that any other solutions are duplicates
		** of the one we just collected.
		*/
		MADITI_post_call_cleanup();
		restore_transient_registers();
		r1 = 1;
	} else {
		MADITI_post_call_cleanup();
		restore_transient_registers();
		r1 = 0;
	}
	MR_succeed_discard();
}

Define_entry(do_det_aditi_call);
{
	MR_mkframe(""do_det_aditi_call"",
		(MADITI_NUM_FRAME_VARS + MADITI_num_output_args),
		ENTRY(MR_do_not_reached));

	save_transient_registers();

	DEBUG(printf(""do_det_aditi_call\\n""));
	MADITI_do_nondet_call();

	/* Unpack the output tuple into r1 and upwards. */
	if (!MADITI_get_output_tuple(1)) {
		MR_fatal_error(""no solution for det Aditi call"");
	}

	/*
	** We don't check that there is only one solution because
	** duplicates may not have been removed.
	** We assume that any other solutions are duplicates
	** of the one we just collected.
	*/

	MADITI_post_call_cleanup();
	restore_transient_registers();

	MR_succeed_discard();
}

/*---------------------------------------------------------------------------*/
/*
** Insert or delete a single tuple into/from a relation.
**
** Input arguments:
** r1 -> name of relation
** r2 -> arity of relation
** r3 -> name of the deletion procedure for the relation (aditi_delete only)
** r4 -> schema of the relation (aditi_delete only)
** r5 -> type_infos for arguments of tuple to insert
**    -> arguments of tuple to insert
**
** There are no output arguments.
*/
Define_entry(do_aditi_insert);
{
	DEBUG(printf(""do_aditi_insert\\n""));
	save_transient_registers();
	MADITI_do_insert_delete_tuple(MADITI_INSERT_TUPLE);
	restore_transient_registers();
	proceed();	
}
Define_entry(do_aditi_delete);
{
	DEBUG(printf(""do_aditi_delete\\n""));
	save_transient_registers();
	MADITI_do_insert_delete_tuple(MADITI_DELETE_TUPLE);
	restore_transient_registers();
	proceed();	
}


/*
** Insert/delete the tuples returned by the query argument
** 
** Input arguments:
** r1 -> name of relation
** r2 -> name of deletion or modification procedure
** r3 -> closure
*/
Define_entry(do_aditi_bulk_insert);
{
	DEBUG(printf(""do_aditi_bulk_insert\\n""));
	save_transient_registers();
	MADITI_do_bulk_operation(MADITI_INSERT);
	restore_transient_registers();
	proceed();	
}
Define_entry(do_aditi_bulk_delete);
{
	DEBUG(printf(""do_aditi_bulk_delete\\n""));
	save_transient_registers();
	MADITI_do_bulk_operation(MADITI_DELETE);
	restore_transient_registers();
	proceed();	
}
Define_entry(do_aditi_bulk_modify);
{
	DEBUG(printf(""do_aditi_bulk_modify\\n""));
	save_transient_registers();
	MADITI_do_bulk_operation(MADITI_MODIFY);
	restore_transient_registers();
	proceed();	
}

END_MODULE
void mercury_sys_init_aditi_call(void); /* suppress gcc warning */
void mercury_sys_init_aditi_call(void) {
	do_aditi_call_module();
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

/*
** The functions below fiddle with the Mercury registers, so be sure
** to save/restore_transient_registers around calls to them.
*/

/*
** Send the input tuple to the database then run the procedure.
** Afterwards, setup for nondeterministic return of the output tuples.
*/
static void
MADITI_do_nondet_call(void)
{
	String proc_name;
	int num_input_args;
	Word *input_typeinfos;
	Word *input_args;	

	String input_schema;
	String input_tuple = NULL;

	int num_output_args;

	ticket *output_ticket = NULL;	/* ticket for the output relation */
	ticket *cursor = NULL;		/* ticket for the output cursor */

	int first_output_typeinfo;	/* register containing the first of the
					** type-infos for the output arguments
					*/
	int i;
	MADITI_output_info *output_info;

	restore_transient_registers();

	proc_name = (String) MADITI_proc_name;
	num_input_args = (int) MADITI_num_input_args;
	input_schema = (String) MADITI_input_schema;

	num_output_args = (int) MADITI_num_output_args;

	DEBUG(printf(""Handling call to %s\\n"", proc_name));
	DEBUG(printf(""%d input args; %d output args\\n"",
		num_input_args, num_output_args));
	DEBUG(printf(""input schema %s\\n"", input_schema));

	/* extract the input arguments and their type-infos from registers */
	input_args = MR_GC_NEW_ARRAY(Word, num_input_args);
	input_typeinfos = MR_GC_NEW_ARRAY(Word, num_input_args);
	save_registers();	
	for (i = 0; i < num_input_args; i++) {
		input_typeinfos[i] = virtual_reg(MADITI_CALL_FIRST_INPUT + i);
		input_args[i] =
		    virtual_reg(MADITI_CALL_FIRST_INPUT + num_input_args + i);
	}

	/* store the output typeinfos in the nondet stack frame */
	first_output_typeinfo = MADITI_CALL_FIRST_INPUT + num_input_args * 2;
	for (i = 0; i < num_output_args; i++) {
		MADITI_OUTPUT_TYPEINFO(i) =
			virtual_reg(first_output_typeinfo + i);
	}

	input_tuple = MADITI_construct_tuple(num_input_args,
				input_typeinfos, input_args);
	MR_GC_free(input_args);
	MR_GC_free(input_typeinfos);

	output_ticket = MADITI_run_procedure(proc_name,
				input_schema, input_tuple);

	/* create cursor on the output relation */
	DEBUG(printf(""opening output cursor...""));
	/* XXX MR_GC_NEW_ATOMIC */
	cursor = (ticket *) MR_GC_NEW(ticket);
	MADITI_check(ADITI_NAME(rel_cursor_create)(output_ticket, cursor));
	MADITI_check(ADITI_NAME(cursor_open)(cursor, CUR_FORWARD));
	DEBUG(printf(""done\\n""));

	output_info = MR_GC_NEW(MADITI_output_info);
	output_info->output_rel = output_ticket;
	output_info->output_cursor = cursor;
	output_info->num_output_args = num_output_args;
	output_info->cleaned_up = MR_FALSE;
	MADITI_saved_output_info = (Word) output_info;
#ifdef MR_USE_TRAIL
	MR_trail_function(MADITI_trail_cleanup_call_output,
		(void *) output_info);
#endif

	save_transient_registers();
}

/* 
** Given an RL procedure name, the schema of the input relation and a tuple
** to insert into the input relation, run the procedure, returning a ticket
** for the output relation.
** MADITI_run_procedure does not look at the Mercury registers.
*/
static ticket *
MADITI_run_procedure(String proc_name, String input_schema, String input_tuple)
{
		/* Ticket identifying the input relation. */
	ticket input_ticket;
		/* Ticket identifying the output relation. */
	ticket *output_ticket = NULL;

	/*
	** Create a temporary relation to hold the input tuple.
	*/
	DEBUG(printf(""creating input temporary (schema %s)..."",
		input_schema));
	MADITI_check(ADITI_NAME(tmp_create)(&MADITI_ticket,
		input_schema, &input_ticket));
	DEBUG(printf(""done\\n""));

	/*
	** Insert the input tuple into the relation.
	*/
	DEBUG(printf(""adding input tuple...%s"", input_tuple));
	MADITI_check(ADITI_NAME(tmp_addtup)(&input_ticket, input_tuple));
	DEBUG(printf(""done\\n""));

	/*
	** Run the procedure.
	*/
	DEBUG(printf(""running procedure... ""));
	/* XXX MR_GC_NEW_ATOMIC */
	output_ticket = (ticket *) MR_GC_NEW(ticket);
	MADITI_check(ADITI_NAME(run2_s)(proc_name, MADITI_CALL_TIMEOUT,
		&MADITI_ticket, &input_ticket, output_ticket));
	DEBUG(printf(""done\\n""));

	/*
	** Drop the input relation.
	*/
	DEBUG(printf(""dropping input temporary...""));
	MADITI_check(ADITI_NAME(tmp_destroy)(&input_ticket));
	DEBUG(printf(""done\\n""));

	DEBUG(printf(""output tuples\n""));
	DEBUG(MADITI_list_rel(output_ticket));
	DEBUG(printf(""\\n\\n""));

	return output_ticket;
}

static void 
MADITI_list_rel(ticket* rel)
{
	size_t len;
	char* ptr;
	ticket cur;

	MADITI_check(ADITI_NAME(tmp_cursor_create)(rel,&cur));
	MADITI_check(ADITI_NAME(cursor_open)(&cur,CUR_FORWARD));
	len = 0;
	ptr = NULL;
	fflush(stdout);
	while (ADITI_NAME(cursor_next)(&cur,&len,&ptr) == ADITI_OK) {
		fprintf(stdout,""tuple: [%.*s]\n"",(int)len,ptr);
		free(ptr);
		len = 0;
		ptr = NULL;
	}
	MADITI_check(ADITI_NAME(cursor_close)(&cur));
	MADITI_check(ADITI_NAME(cursor_destroy)(&cur));
}

/*---------------------------------------------------------------------------*/

static void
MADITI_do_insert_delete_tuple(MADITI_insert_delete op)
{
	String rel_name;
	int rel_arity;
	String tuple;
	String delete_proc_name;
	String delete_input_schema;
	Word *input_typeinfos;
	Word *input_args;
	int first_input_reg = 5;
	int i;
	ticket *delete_output_rel = NULL;

	restore_transient_registers();
	rel_name = (String) r1;
	rel_arity = r2;
	delete_proc_name = (String) r3;
	delete_input_schema = (String) r4;

	DEBUG(
		switch (op) {
			case MADITI_INSERT_TUPLE:
				printf(""aditi_insert(%s)\\n"",
					rel_name);	
				break;
			case MADITI_DELETE_TUPLE:
				printf(""aditi_delete(%s)\\n"",
					rel_name);	
				break;
		}
	)

	/* extract the input arguments and their type-infos from registers */
	input_args = MR_GC_NEW_ARRAY(Word, rel_arity);
	input_typeinfos = MR_GC_NEW_ARRAY(Word, rel_arity);
	save_registers();
	for (i = 0; i < rel_arity; i++) {
		input_typeinfos[i] = virtual_reg(first_input_reg + i);
		input_args[i] = virtual_reg(first_input_reg + rel_arity + i);
	}

	save_transient_registers();
	tuple = MADITI_construct_tuple(rel_arity, input_typeinfos, input_args);
	restore_transient_registers();

	MR_GC_free(input_args);
	MR_GC_free(input_typeinfos);

	switch (op) {
		case MADITI_INSERT_TUPLE:
			DEBUG(printf(""inserting tuple %s\\n"", tuple));
			MADITI_check(ADITI_NAME(addtup)(rel_name, tuple));
			DEBUG(printf(""finished insertion\\n""));
			break;
		case MADITI_DELETE_TUPLE:
			DEBUG(printf(""deleting tuple %s\\n"", tuple));
			delete_output_rel =
				MADITI_run_procedure(delete_proc_name,
					delete_input_schema, tuple);
			MADITI_check(ADITI_NAME(rel_close)(
				delete_output_rel));
			MR_GC_free(delete_output_rel);
			DEBUG(printf(""finished deletion\\n""));
			break;
	}

	save_transient_registers();
}

/*---------------------------------------------------------------------------*/

static void
MADITI_do_bulk_operation(MADITI_bulk_op operation)
{
	String rel_name;
	int num_input_args;
	Word *input_args;
	Word *input_typeinfos;
	MR_Closure *closure;
	String called_proc_name;
	String update_proc_name = NULL;
	String input_schema;
	String input_tuple;
	ticket modified_rel_ticket;
	ticket dummy_output_ticket;
	ticket *call_result_ticket;

	restore_transient_registers();

	rel_name = (String) r1;
	update_proc_name = (String) r2;
	closure = (MR_Closure *) r3;

	DEBUG(
		switch (operation) {
			case MADITI_INSERT:
				printf(""aditi_bulk_insert(%s)\\n"",
					rel_name);	
				break;
			case MADITI_DELETE:
				printf(""aditi_bulk_delete(%s)\\n"",
					rel_name);	
				break;
			case MADITI_MODIFY:
				printf(""aditi_bulk_delete(%s)\\n"",
					rel_name);	
				break;
		}
	)

	/*
	** The 'code' passed to an aditi_bottom_up closure is
	** actually a tuple containing the procedure name and
	** the schema of the input relation.
	*/
	called_proc_name =
		(String) MR_field(MR_mktag(0), closure->MR_closure_code, 0);
	input_schema =
		(String) MR_field(MR_mktag(0), closure->MR_closure_code, 1);

	DEBUG(printf(""closure proc name %s\n"", called_proc_name));

	num_input_args = closure->MR_closure_num_hidden_args;
	input_args = (Word *) closure->MR_closure_hidden_args_0;
	input_typeinfos =
		(Word *) closure->MR_closure_layout->arg_pseudo_type_info;

	/*
	** Call the query to compute the tuples to insert/delete/modify.
	*/
	save_transient_registers();
	input_tuple = MADITI_construct_tuple(num_input_args,
				input_typeinfos, input_args);
	restore_transient_registers();

	call_result_ticket = MADITI_run_procedure(called_proc_name,
				input_schema, input_tuple);

	/*
	** Call the procedure generated by the compiler to apply the update.
	*/
	DEBUG(printf(""Query finished -- calling update procedure %s\\n"",
		update_proc_name));
	MADITI_check(ADITI_NAME(run2_s)(update_proc_name,
		MADITI_CALL_TIMEOUT, &MADITI_ticket, call_result_ticket,
		&dummy_output_ticket)
	);

	/*
	** Clean up.
	*/
	MADITI_check(ADITI_NAME(rel_close)(&dummy_output_ticket));
	MADITI_check(ADITI_NAME(rel_close)(call_result_ticket));
	MR_GC_free(call_result_ticket);

	save_transient_registers();
}

/*---------------------------------------------------------------------------*/

/*
** Convert a list of arguments in registers into a string representation
** suitable for sending to Aditi.
** Starting at register `first_argument', there must be `num_input_args'
** type_infos, followed by `num_input_args' data values.
** save_transient_registers()/restore_transient_registers() must be done
** around calls to this function.
*/
String MADITI_construct_tuple(int num_input_args,
		Word *typeinfos, Word *input_args)
{
		/* Used to hold the list of attributes of the input tuple
		** as they are built up.
		*/
	Word tuple_list; 
	Word new_tuple_list;
	Code *saved_succip;

	String tuple;
	int i;

	restore_transient_registers();

	/*
	** This part calls back into Mercury to construct the tuple.
	** The wrapper functions expect the registers to be saved,
	** so do that here.
	** MR_succip may be clobbered by the called code, so
	** it must be saved and restored here.
	*/
	saved_succip = MR_succip;
	save_registers();

	tuple_list = MR_list_empty();
	DEBUG(printf(""building input tuple...""));
	for (i = 0; i < num_input_args; i++) {
		/* convert the argument to a string, adding it to the tuple */
		MADITI__attr_to_string(typeinfos[i], input_args[i],
			tuple_list, &new_tuple_list);
		tuple_list = new_tuple_list;
	}
	MADITI__reverse_append_string_list(tuple_list, &tuple);
	DEBUG(printf(""done: tuple = %s\\n"", tuple));

	/*
	** Get back the updated Mercury registers after the calls into Mercury.
	*/
	restore_registers();
	MR_succip = saved_succip;
	save_transient_registers();
	
	return tuple;
}

/*---------------------------------------------------------------------------*/

/*
** Get an output tuple from the database, return an indication of success.
** Attributes from the tuple should be placed in consecutive registers
** starting with r(first_reg).
** If no tuple is found, the registers are unchanged.
*/
static bool
MADITI_get_output_tuple(int first_reg)
{
	int i;
	Word pos, newpos;
		/* Length of the tuple string retrieved from Aditi. */
	size_t tuple_str_len;
	char *tuple_str; /* The tuple string from Aditi. */
		/* The tuple string copied onto the Mercury heap */
	String tuple_str_copy;
	Word arg;
	int found_result;
	MADITI_output_info *output_info;
	int rc;
		/* Somewhere to put the output arguments before copying them
		** into the registers.
		*/
	Word *output_save_area;
	Code *saved_succip;

	restore_transient_registers();
	
	output_info = (MADITI_output_info *) MADITI_saved_output_info;

	/* advance cursor, get tuple string */
	DEBUG(printf(""getting output tuple\\n""));
	rc = ADITI_NAME(cursor_next)(output_info->output_cursor,
		&tuple_str_len, &tuple_str);
	if (rc != ADITI_OK) {
		DEBUG(printf(""no more output tuples\\n""));
		found_result = MR_FALSE;
	} else {
		DEBUG(printf(""handling tuple %s %ld %d\\n"",
			tuple_str, tuple_str_len,
			output_info->num_output_args));

		/*
		** Found another output tuple.
		*/
	
		/* Copy out the tuple string. */
		make_aligned_string_copy(tuple_str_copy, tuple_str);
		/* The tuple is on the C heap. */
		free(tuple_str);

		/*
		** Allocate some space to hold the output arguments
		** before we put them in registers. 
		*/
		output_save_area = MR_GC_NEW_ARRAY(Word,
					output_info->num_output_args);
	
		/*
		** This part calls back into Mercury to parse the
		** tuple terms from the string returned from Aditi.
		** The wrapper functions generated expect the registers
		** to be saved, so do that here.
		** MR_succip may be clobbered by the called code, so
		** it must be saved and restored here.
		*/
		saved_succip = MR_succip;
		save_registers();
		/* convert tuple, put output args in stack slots */
		MADITI__init_posn(&pos);
		for (i = 0; i < output_info->num_output_args; i++) {
			Word status;

			MADITI__read_attr_from_string(
				MADITI_OUTPUT_TYPEINFO(i),
				tuple_str_copy, tuple_str_len - 1, &arg,
				pos, &newpos, &status);
			MADITI_check(status);

			pos = newpos;
			output_save_area[i] = arg;
		}

		/* Move the output arguments to their registers. */
		for (i = 0; i < output_info->num_output_args; i++) {
			virtual_reg(first_reg + i) = output_save_area[i];
		}
		restore_registers();
		MR_succip = saved_succip;
		MR_GC_free(output_save_area);

		found_result = MR_TRUE;
	}
	save_transient_registers();
	return found_result;
}

/*---------------------------------------------------------------------------*/

/*
** Free all resources used by a database call.
*/
static void
MADITI_post_call_cleanup(void)
{ 
	restore_transient_registers();
	MADITI_cleanup_call_output(
		(MADITI_output_info *) MADITI_saved_output_info);
	save_transient_registers();
}

#ifdef MR_USE_TRAIL
static void
MADITI_trail_cleanup_call_output(void *data, MR_untrail_reason reason)
{
	switch (reason) {
	    case MR_commit:
	    case MR_exception:
	    case MR_retry:
		/*
		** Clean up the output relation.
		*/
		DEBUG(printf(
		    ""MADITI_trail_cleanup_call_output: cleaning up %d\\n"",
		    reason));
		MADITI_cleanup_call_output((MADITI_output_info *)data);
		break;

	    case MR_solve:
	    case MR_undo:
		/*
		** Undo on backtracking will be handled by
		** MADITI_post_call_cleanup, so that the 
		** cleanup will happen even if trailing
		** is not being used.
		*/
		break;

	    case MR_gc:
	    default:
		MR_fatal_error(""MADITI_trail_cleanup_call_output"");
	}
}
#endif /* MR_USE_TRAIL */

static void
MADITI_cleanup_call_output(MADITI_output_info *output_info)
{
	if (output_info->cleaned_up) {

		/*
		** This can happen if there is a commit followed
		** by an exception -- the commit will not reset
		** the trail.
		*/
		DEBUG(printf(
			""MADITI_cleanup_call_output: already cleaned up\n""
		));

	} else {

		DEBUG(printf(
			""MADITI_cleanup_call_output: cleaning up\n""
		));

		/* close cursor */
		DEBUG(printf(""closing cursor\\n""));
		MADITI_check(
			ADITI_NAME(cursor_close)(output_info->output_cursor)
		);

		/* destroy cursor */
		DEBUG(printf(""destroying cursor\\n""));
		MADITI_check(
			ADITI_NAME(cursor_destroy)(output_info->output_cursor)
		);
		MR_GC_free(output_info->output_cursor);

		/* close output temporary */
		DEBUG(printf(""closing output temporary relation\\n""));
		MADITI_check(ADITI_NAME(rel_close)(output_info->output_rel));
		MR_GC_free(output_info->output_rel);

		/* Make sure we don't do this again. */
		output_info->cleaned_up = MR_TRUE;
	}
}

/*---------------------------------------------------------------------------*/

/*
** If the status is not OK, abort the transaction.
*/
static void
MADITI_do_check(int status, int line)
{
	if (status != ADITI_OK) {
		DEBUG(printf(""aditi.m:%d MADITI_check_failed, status %d\\n"",
				line, status));
		MADITI_status = status;
		MR_longjmp(&MADITI_jmp_buf);
	}
}
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
	% Data conversion.

	% This is very similar to io__write except
	% a) it writes to a string
	% b) everything is written in prefix form.
	% c) arrays, c_pointers, type_infos and univs result in an abort.
:- pred aditi__attr_to_string(T, list(string), list(string)).
:- mode aditi__attr_to_string(in, in, out) is det.
:- pragma export(aditi__attr_to_string(in, in, out),
		"MADITI__attr_to_string").

aditi__attr_to_string(Attr, Strings0, [String | Strings0]) :-
	type_to_univ(Attr, Univ),
	aditi__univ_to_string(Univ, String).

:- pred aditi__univ_to_string(univ, string).
:- mode aditi__univ_to_string(in, out) is det.

aditi__univ_to_string(Univ, String) :-
	%
	% we need to special-case the builtin types:
	%	int, char, float, string
	%	type_info, univ, c_pointer, array
	%
	( univ_to_type(Univ, String1) ->
		string__append_list(["\"", String1, "\""], String)
	; univ_to_type(Univ, Char) ->
		char__to_int(Char, CharInt),
		string__int_to_string(CharInt, String)
	; univ_to_type(Univ, Int) ->
		string__int_to_string(Int, String)
	; univ_to_type(Univ, Float) ->
		string__float_to_string(Float, String)
	;
		aditi__ordinary_term_to_string(Univ, String)
	).

:- pred aditi__ordinary_term_to_string(univ, string).
:- mode aditi__ordinary_term_to_string(in, out) is det.

aditi__ordinary_term_to_string(Term, String) :-
	deconstruct(Term, Functor, _Arity, Args),
	aditi__quote_atom(Functor, FunctorStr),
	aditi__term_args_to_strings(yes, Args, ["(" | FunctorStr],
		Strings0),
	list__reverse([")" | Strings0], Strings),
	string__append_list(Strings, String).

:- pred aditi__term_args_to_strings(bool, list(univ),
		list(string), list(string)).
:- mode aditi__term_args_to_strings(in, in, in, out) is det.

aditi__term_args_to_strings(_, [], Strings, Strings).
aditi__term_args_to_strings(IsFirst, [X | Xs], Strings0, Strings) :-
	aditi__univ_to_string(X, XStr),
	( IsFirst = yes ->
		Comma = ""
	;
		Comma = ", "
	),
	aditi__term_args_to_strings(no, Xs, [XStr, Comma | Strings0], Strings).

:- pred aditi__quote_atom(string::in, list(string)::out) is det.

aditi__quote_atom(String0, Quoted) :-
	( string__is_alnum_or_underscore(String0) ->
		Quoted = [String0]
	;
		Quoted = ["'", String0, "'"]
	).

%-----------------------------------------------------------------------------%

:- pred aditi__reverse_append_string_list(list(string), string).
:- mode aditi__reverse_append_string_list(in, out) is det.
:- pragma export(aditi__reverse_append_string_list(in, out),
		"MADITI__reverse_append_string_list").

aditi__reverse_append_string_list(Strings0, String) :-
	( Strings0 = [] ->
		String = "()\n"
	;
		aditi__construct_attr_list(Strings0, yes, [")\n"], Strings1),
		string__append_list(Strings1, String)
	).

:- pred aditi__construct_attr_list(list(string), bool,
		list(string), list(string)).
:- mode aditi__construct_attr_list(in, in, in, out) is det.

aditi__construct_attr_list([], _, Strings, ["(" | Strings]).
aditi__construct_attr_list([Attr | Attrs], IsLast, Strings0, Strings) :-
	( IsLast = yes ->
		Strings1 = [Attr | Strings0]
	;
		Strings1 = [Attr, ", " | Strings0]
	),
	aditi__construct_attr_list(Attrs, no, Strings1, Strings).	

%-----------------------------------------------------------------------------%

:- pred aditi__init_posn(posn::out) is det.
:- pragma export(aditi__init_posn(out), "MADITI__init_posn").

aditi__init_posn(posn(1, 0, 0)).

	% Get the next attribute from the string.
:- pred aditi__read_attr_from_string(string, int, T, posn, posn, int).
:- mode aditi__read_attr_from_string(in, in, out, in, out, out) is det.
:- pragma export(aditi__read_attr_from_string(in, in, out, in, out, out),
		"MADITI__read_attr_from_string").

aditi__read_attr_from_string(String, MaxPos, Thing, Posn0, Posn, Status) :-
	io__read_from_string_with_int_instead_of_char("Aditi tuple", String,
		MaxPos, Result, Posn0, Posn),
	( Result = ok(Thing0) ->
		Thing = Thing0,
		Status = 0
	;
		% Nothing looks at the output of this.
		private_builtin:unsafe_type_cast(0, Thing),	
		aditi__fail_status(Status)
	).

:- pred aditi__fail_status(int::out) is det.
:- pragma c_code(aditi__fail_status(Stat::out), "Stat = ADITI_FAIL").

%-----------------------------------------------------------------------------%

	% Try to classify an error code returned by Aditi.
:- pred aditi__error_code(int::in, aditi__error::out, string::out) is det.

aditi__error_code(Status, Error, String) :-
	( aditi__error_code_2(Status, Error0) ->
		Error = Error0,
		aditi__error_message(Status, String)
	;
		Error = general_failure,
		string__format("invalid Aditi error code %i",
			[i(Status)], String)
	).

:- pred aditi__error_code_2(int::in, aditi__error::out) is semidet.
	
aditi__error_code_2(-1, invalid_passwd).
aditi__error_code_2(-2, invalid_passwd).
aditi__error_code_2(-3, general_failure).
aditi__error_code_2(-4, general_failure).
aditi__error_code_2(-5, too_many_connections).
aditi__error_code_2(-6, general_failure).
aditi__error_code_2(-7, general_failure).
aditi__error_code_2(-8, already_logged_in).
aditi__error_code_2(-9, not_logged_in).
aditi__error_code_2(-10, general_failure).
aditi__error_code_2(-11, general_failure).
aditi__error_code_2(-12, general_failure).
aditi__error_code_2(-13, general_failure).
aditi__error_code_2(-14, error_creating_client).
aditi__error_code_2(-15, general_failure).
aditi__error_code_2(-16, not_implemented).
aditi__error_code_2(-17, abort).
aditi__error_code_2(-18, general_failure).
aditi__error_code_2(-19, general_failure).
aditi__error_code_2(-20, general_failure).
aditi__error_code_2(-21, bad_value).
aditi__error_code_2(-22, not_connected).
aditi__error_code_2(-23, bad_rl_code).
aditi__error_code_2(-24, bad_rl_code).
aditi__error_code_2(-25, bad_rl_code).
aditi__error_code_2(-26, error_opening_relation).
aditi__error_code_2(-27, bad_rl_code).
aditi__error_code_2(-28, bad_rl_code).
aditi__error_code_2(-29, security_violation).
aditi__error_code_2(-30, bad_rl_code).
aditi__error_code_2(-31, bad_rl_code).
aditi__error_code_2(-32, bad_rl_code).
aditi__error_code_2(-33, unique_key_violation).
aditi__error_code_2(-34, relation_or_cursor_not_open).
aditi__error_code_2(-35, general_failure).

:- pred aditi__error_message(int::in, string::out) is det.

:- pragma c_code(aditi__error_message(Stat::in, Msg::out),
		will_not_call_mercury,
"
	Msg = aditi_strerror((int) Stat);
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
