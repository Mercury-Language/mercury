%---------------------------------------------------------------------------%
% Copyright (C) 1998-1999 University of Melbourne.
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
% (XXX this section in the manual is commented out, since Aditi is not yet
% publicly available.)
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
:- mode aditi_ui :: in(aditi_unique).

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
	This should be translated into the equivalent aggregate_compute_initial
   	by magic.m, but that hasn't been done yet. The main problem is
	collecting the initial value - it may not be constant.

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

:- type aditi__transaction(T) == pred(T, aditi__state, aditi__state).
:- inst aditi__transaction = (pred(out, aditi_di, aditi_uo) is det).

:- pred aditi__transaction(aditi__connection, aditi__transaction(T),
		aditi__result(T), io__state, io__state).
:- mode aditi__transaction(in, in(aditi__transaction), out, di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool, char, list, require, std_util, string, std_util.

:- type aditi__connection == int.
:- type aditi__state == unit.

	% These are handled by the RL code generator.
:- external(aditi__aggregate_compute_initial/5).
%:- external(aditi__aggregate_given_initial/5).

:- pragma c_header_code("

#include ""mercury_init.h""
#include ""aditi_clnt.h""

#define MADITI_throw MR_longjmp(&MADITI_jmp_buf);

static ticket MADITI_ticket;		/* Current connection ticket. */
static MR_jmp_buf MADITI_jmp_buf;	/* jmp_buf to longjmp() to when
					** aborting the transaction.
					*/
static int MADITI_status;		/* Return code of the last
					** Aditi API call.
					*/

static void MADITI_do_call(void);
static void MADITI_check(int);
static bool MADITI_get_output_tuple(int);
static void MADITI_cleanup(void);
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

			DEBUG(struct param_value pvalue);
			DEBUG(pvalue.type = int_type);
			DEBUG(pvalue.param_value_u.int_val = 1);
			DEBUG(ADITI_NAME(setparam)(""debug"", &pvalue));
			DEBUG(ADITI_NAME(disable_ping)());

			DEBUG(printf(""logged in\\n""));
			if ((Stat = mercury__load_aditi_rl_code())
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
		Status, IO0, IO),
	( Status = 0 ->
		TransResult = ok(Results)
	;
		aditi__error_code(Status, Error, String),
		TransResult = error(Error, String) 
	).

%-----------------------------------------------------------------------------%

:- pred aditi__do_transaction(aditi__transaction(T),
		T, int, io__state, io__state).
:- mode aditi__do_transaction(in(aditi__transaction), out, out, di, uo) is det.
:- pragma no_inline(aditi__do_transaction/5).	% contains labels

:- pragma c_code(aditi__do_transaction(Transaction::in(aditi__transaction), 
		Results::out, Stat::out, IO0::di, IO::uo),
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
		Transaction, &Results, 0, &DB);

	DEBUG(printf(""committing transaction...""));
	Stat = ADITI_NAME(trans_commit)(&MADITI_ticket);
	DEBUG(printf(""done\\n""));
	
	goto transaction_done;

transaction_error:

	DEBUG(printf(""aborting transaction...""));
	ADITI_NAME(trans_abort)(&MADITI_ticket);
	DEBUG(printf(""done\\n""));
	Stat = MADITI_status;

transaction_done:
}
").

:- pred aditi__call_transaction(aditi__transaction(T), T,
		aditi__state, aditi__state).
:- mode aditi__call_transaction(in(aditi__transaction), out,
		aditi_di, aditi_uo) is det.
:- pragma export(
		aditi__call_transaction(in(aditi__transaction), out,
			aditi_di, aditi_uo),
		"MADITI__call_transaction"
	).

aditi__call_transaction(Trans, Results) -->
	call(Trans, Results).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma c_code("

	/*
	** We might allocate memory from may_call_mercury C code. XXX?
	*/
#ifndef CONSERVATIVE_GC
#error The Aditi interface requires conservative garbage collection. \\
                Use a compilation grade containing .gc.
#endif /* ! CONSERVATIVE_GC */

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

	/* Register containing the first input argument */
#define MADITI_FIRST_INPUT 5

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
#define MADITI_NUM_FRAME_VARS 3

#define MADITI_saved_num_output_args		MR_framevar(1)
#define MADITI_saved_output_rel			MR_framevar(2)
#define MADITI_saved_cursor			MR_framevar(3)
#define MADITI_ADITI_FIRST_TYPEINFO		MADITI_NUM_FRAME_VARS + 1
#define MADITI_OUTPUT_TYPEINFO(i) \
			MR_framevar(MADITI_ADITI_FIRST_TYPEINFO + (i))

/*---------------------------------------------------------------------------*/
Define_extern_entry(do_nondet_aditi_call);
Declare_label(do_nondet_aditi_call_i1);
Define_extern_entry(do_semidet_aditi_call);
Define_extern_entry(do_det_aditi_call);

MR_MAKE_PROC_LAYOUT(do_nondet_aditi_call, MR_DETISM_NON,
	MR_ENTRY_NO_SLOT_COUNT, MR_LVAL_TYPE_UNKNOWN, MR_PREDICATE,
	""private_builtin"", ""do_nondet_aditi_call"", 0, 0);
MR_MAKE_INTERNAL_LAYOUT(do_nondet_aditi_call, 1);
MR_MAKE_PROC_LAYOUT(do_semidet_aditi_call, MR_DETISM_NON,
	MR_ENTRY_NO_SLOT_COUNT, MR_LVAL_TYPE_UNKNOWN, MR_PREDICATE,
	""private_builtin"", ""do_semidet_aditi_call"", 0, 0);
MR_MAKE_PROC_LAYOUT(do_det_aditi_call, MR_DETISM_NON,
	MR_ENTRY_NO_SLOT_COUNT, MR_LVAL_TYPE_UNKNOWN, MR_PREDICATE,
	""private_builtin"", ""do_semidet_aditi_call"", 0, 0);

BEGIN_MODULE(do_aditi_call_module)
	init_entry_sl(do_nondet_aditi_call);
	MR_INIT_PROC_LAYOUT_ADDR(do_nondet_aditi_call);
	init_label_sl(do_nondet_aditi_call_i1);
	init_entry_sl(do_semidet_aditi_call);
	MR_INIT_PROC_LAYOUT_ADDR(do_semidet_aditi_call);
	init_entry_sl(do_det_aditi_call);
	MR_INIT_PROC_LAYOUT_ADDR(do_det_aditi_call);
BEGIN_CODE

Define_entry(do_nondet_aditi_call);
{
	mkframe(""do_nondet_aditi_call"",
		(MADITI_NUM_FRAME_VARS + MADITI_num_output_args),
		LABEL(do_nondet_aditi_call_i1));

	save_transient_registers();
	MADITI_do_call();
	restore_transient_registers();
	GOTO(LABEL(do_nondet_aditi_call_i1));
}
Define_label(do_nondet_aditi_call_i1);
{
	save_transient_registers();

	/* Unpack the output tuple into r1 and upwards. */
	if (MADITI_get_output_tuple(1)) {
		restore_transient_registers();
		succeed();
	} else {
		MADITI_cleanup();
		restore_transient_registers();
		fail();
	}
}

Define_entry(do_semidet_aditi_call);
{
	mkframe(""do_semidet_aditi_call"",
		(MADITI_NUM_FRAME_VARS + MADITI_num_output_args),
		ENTRY(do_not_reached));

	save_transient_registers();
	MADITI_do_call();

	/* Unpack the output tuple into r2 and upwards for semidet code. */
	if (MADITI_get_output_tuple(2)) {
		/*
		** We don't check that there is only one solution because
		** duplicates may not have been removed.
		** We assume that any other solutions are duplicates
		** of the one we just collected.
		*/
		MADITI_cleanup();
		restore_transient_registers();
		r1 = 1;
	} else {
		MADITI_cleanup();
		restore_transient_registers();
		r1 = 0;
	}
	succeed_discard();
}

Define_entry(do_det_aditi_call);
{
	mkframe(""do_det_aditi_call"",
		(MADITI_NUM_FRAME_VARS + MADITI_num_output_args),
		ENTRY(do_not_reached));

	save_transient_registers();
	MADITI_do_call();

	/* Unpack the output tuple into r1 and upwards. */
	if (!MADITI_get_output_tuple(1)) {
		fatal_error(""no solution for det Aditi call"");
	}

	/*
	** We don't check that there is only one solution because
	** duplicates may not have been removed.
	** We assume that any other solutions are duplicates
	** of the one we just collected.
	*/

	MADITI_cleanup();
	restore_transient_registers();

	succeed_discard();
}

END_MODULE
void mercury_sys_init_aditi_call(void); /* suppress gcc warning */
void mercury_sys_init_aditi_call(void) {
	do_aditi_call_module();
}

/*---------------------------------------------------------------------------*/

/*
** The functions below fiddle with the Mercury registers, so be sure
** to save/restore_transient_registers around calls to them.
*/

/*
** Send the input tuple to the database then run the procedure.
*/
static void
MADITI_do_call(void)
{ 
	String proc_name;
	int num_input_args;
	String input_schema;
	int num_output_args;

		/* Ticket identifying the input relation. */
	ticket *input_ticket = NULL;
		/* Ticket identifying the output relation. */
	ticket *output_ticket = NULL;
		/* Ticket identifying the cursor on the output relation. */
	ticket *cursor = NULL;
	
		/* Used to hold the list of attributes of the input tuple
		** as they are built up.
		*/
	Word tuple_list; 
	Word new_tuple_list;

	char *tuple = NULL; /* The input tuple. */
	int i;
		/* Register containing the first of the output type_infos. */
	int first_output_typeinfo;

		/* Memory used to hold copies of the input arguments. */
	Word *input_save_area;
	
	restore_transient_registers();

	proc_name = (String) MADITI_proc_name;
	num_input_args = (int) MADITI_num_input_args;
	input_schema = (String) MADITI_input_schema;
	num_output_args = MADITI_num_output_args;

	/* save the number of output arguments */
	MADITI_saved_num_output_args = num_output_args;

	/* create temporary relation to hold the input tuple */
	DEBUG(printf(""creating input temporary...""));
	input_ticket = (ticket *) checked_malloc(sizeof(ticket));
	output_ticket = (ticket *) checked_malloc(sizeof(ticket));
	MADITI_check(ADITI_NAME(tmp_create)(&MADITI_ticket,
		input_schema, input_ticket));
	DEBUG(printf(""done\\n""));

	/*
	** Copy the input arguments and their type_infos out of the registers.
	** This is done to avoid having to save/restore the
	** registers around each call to MADITI__attr_to_string,
	** which calls back into Mercury. 
	*/
	input_save_area = make_many(Word, num_input_args * 2);
	save_registers();	
	for (i = 0; i < num_input_args * 2; i++) {
		input_save_area[i] = virtual_reg(MADITI_FIRST_INPUT + i);
	}

	/* store the output typeinfos in the nondet stack frame */
	first_output_typeinfo = MADITI_FIRST_INPUT + num_input_args * 2;
	for (i = 0; i < num_output_args; i++) {
		MADITI_OUTPUT_TYPEINFO(i) =
			virtual_reg(first_output_typeinfo + i);
	}

	/*
	** We're finished with the virtual_reg array.
	** There's no need to restore the registers here since 
	** we haven't altered any of the virtual_reg array entries.
	*/

	tuple_list = MR_list_empty();
	DEBUG(printf(""building input tuple...""));
	for (i = 0; i < num_input_args; i++) {
		/* convert the argument to a string, adding it to the tuple */
		MADITI__attr_to_string(input_save_area[i],
			input_save_area[num_input_args + i],
			tuple_list, &new_tuple_list);
		tuple_list = new_tuple_list;
	}
	MADITI__reverse_append_string_list(tuple_list, &tuple);
	DEBUG(printf(""done\\n""));

	/*
	** We're done with the saved input arguments.
	*/
	oldmem(input_save_area);

	/* insert the input tuple into the relation */
	DEBUG(printf(""adding input tuple...%s"", tuple));
	MADITI_check(ADITI_NAME(tmp_addtup)(input_ticket, tuple));
	DEBUG(printf(""done\\n""));

	/* run the procedure */
	DEBUG(printf(""running procedure... ""));
	MADITI_check(ADITI_NAME(run2)(proc_name, 100000, &MADITI_ticket,
		input_ticket, output_ticket));
	DEBUG(printf(""done\\n""));

	/* drop the input relation */
	DEBUG(printf(""dropping input temporary...""));
	MADITI_check(ADITI_NAME(tmp_destroy)(input_ticket));
	free(input_ticket);
	DEBUG(printf(""done\\n""));

	/* create cursor on the output relation */
	DEBUG(printf(""opening output cursor...""));
	cursor = (ticket *) checked_malloc(sizeof(ticket));
	MADITI_check(ADITI_NAME(tmp_cursor_create)(output_ticket, cursor));
	MADITI_check(ADITI_NAME(cursor_open)(cursor, CUR_FORWARD));
	DEBUG(printf(""done\\n""));

	MADITI_saved_output_rel = (Word) output_ticket;
	MADITI_saved_cursor = (Word) cursor;

	save_transient_registers();
}

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
	int num_output_args;
	int rc;
		/* Somewhere to put the output arguments before copying them
		** into the registers.
		*/
	Word *output_save_area;

	restore_transient_registers();
	num_output_args = MADITI_saved_num_output_args;

	/* advance cursor, get tuple string */
	rc = ADITI_NAME(cursor_next)((ticket *)MADITI_saved_cursor,
		&tuple_str_len, &tuple_str);
	DEBUG(printf(""handling tuple %s %ld %d\\n"",
		tuple_str, tuple_str_len, num_output_args));

	if (rc != ADITI_OK) {
		found_result = FALSE;
	} else {

		/*
		** Found another output tuple.
		*/
	
		/* Copy out the tuple string. */
		make_aligned_string_copy(tuple_str_copy, tuple_str);
		free(tuple_str);

		/*
		** Allocate some space to hold the output arguments
		** before we put them in registers. 
		*/
		output_save_area = make_many(Word, num_output_args);
	
		/* convert tuple, put output args in stack slots */
		MADITI__init_posn(&pos);
		for (i = 0; i < num_output_args; i++) {
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
		save_registers();
		for (i = 0; i < num_output_args; i++) {
			virtual_reg(first_reg + i) = output_save_area[i];
		}
		restore_registers();
		oldmem(output_save_area);

		found_result = TRUE;
	}
	save_transient_registers();
	return found_result;
}

/*
** Free all resources used by a database call.
*/
static void
MADITI_cleanup()
{ 
	restore_transient_registers();

	/* close cursor */
	MADITI_check(ADITI_NAME(cursor_close)(
		(ticket *)MADITI_saved_cursor));

	/* destroy cursor */
	MADITI_check(ADITI_NAME(cursor_destroy)(
		(ticket *)MADITI_saved_cursor));
	free((ticket *)MADITI_saved_cursor);

	/* destroy output temporary */
	MADITI_check(ADITI_NAME(tmp_destroy)(
		(ticket *)MADITI_saved_output_rel));
	free((ticket *)MADITI_saved_output_rel);

	save_transient_registers();
}

/*
** If the status is not OK, abort the transaction.
*/
static void
MADITI_check(int status)
{
	if (status != ADITI_OK) {
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

	% Yes, the output of this is meant to have unbalanced parentheses.
aditi__reverse_append_string_list(Strings0, String) :-
	( Strings0 = [] ->
		String = "()\n"
	;
		aditi__construct_attr_list(Strings0, yes, ["\n"], Strings1),
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
	io__read_from_string("Aditi tuple", String,
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
	Msg = aditi_strerror(Stat);
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
