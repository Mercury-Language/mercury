/*
** Copyright (C) 1997-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trail.h - code for handling the trail.
**
** The trail is used to record values that need to be
** restored on backtracking.
*/

#ifndef MERCURY_TRAIL_H
#define MERCURY_TRAIL_H

#include "mercury_memory.h"

#ifdef MR_USE_TRAIL
  #define MR_IF_USE_TRAIL(x) x
#else
  #define MR_IF_USE_TRAIL(x)
#endif

/*---------------------------------------------------------------------------*/
/*
** The following macros define how to store and retrieve a 'ticket' -
** the information that we need to be able to backtrack. 
** This is the interface with the code generator;
** the macros here are used by the generated code.
**
** MR_store_ticket()
**	called when creating a choice point, or before a commit
** MR_reset_ticket()
**	called under the following circumstances, with different parameters:
**	- when resuming forward execution after failing (MR_undo);
**	- after a commit (MR_commit);
**	- after a "soft commit" [one that doesn't prune away all the
**	  alternative solutions, but which does require us to commit to
**	  this goal being solvable] in an if-then-else with a nondet condition,
**	  or in solutions/2 (MR_solve);
**	- when executing a `retry' command in the debugger (MR_retry).
** MR_prune_ticket()
**	called when cutting away the topmost choice point
** MR_discard_ticket()
**	called when failing over the topmost choice point
** MR_mark_ticket_stack()
**	called before a commit,
**	when establishing an exception handler,
**	or when entering an execution traced procedure
** MR_prune_tickets_to()
**	called after a commit
** MR_discard_tickets_to()
**	called when an exception is thrown,
**	or when doing a retry in the debugger
*/
/*---------------------------------------------------------------------------*/

/* void MR_mark_ticket_stack(MR_Word &); */
#define MR_mark_ticket_stack(save_ticket_counter)		\
	do {							\
		(save_ticket_counter) = MR_ticket_counter;	\
	} while(0)

/* void MR_prune_ticket(void); */
#define MR_prune_ticket()					\
	do {							\
		--MR_ticket_counter;				\
	} while(0)

/* void MR_discard_ticket(void); */
#define MR_discard_ticket()					\
	do {							\
		MR_ticket_high_water = --MR_ticket_counter;	\
	} while(0)

/* void MR_prune_tickets_to(MR_Word); */
#define MR_prune_tickets_to(save_ticket_counter)		\
	do {							\
		MR_ticket_counter = (save_ticket_counter);	\
	} while(0)

/* void MR_discard_tickets_to(MR_Word); */
#define MR_discard_tickets_to(save_ticket_counter)		\
	do {							\
		MR_ticket_high_water = MR_ticket_counter =	\
			(save_ticket_counter);			\
	} while(0)

	/* 
	** Called when we create a choice point
	** (including semidet choice points).
	*/
/* void MR_store_ticket(MR_Word &); */
#define MR_store_ticket(save_trail_ptr)				\
	do {							\
		(save_trail_ptr) = (MR_Word) MR_trail_ptr; 	\
		MR_ticket_counter = ++MR_ticket_high_water;	\
	} while(0)

	/*
	** Unwind restoration info back to `old'.  `kind' indicates
	** whether we are restoring or just discarding the info.
	** 
	** Note that the commented out calls to save/restore
	** transient registers are not needed because
	** MR_trail_ptr is never a real register.
	*/
/* void MR_reset_ticket(MR_Word, MR_untrail_reason); */
#define MR_reset_ticket(old, kind)				\
	do {							\
		MR_TrailEntry *old_trail_ptr =  		\
			(MR_TrailEntry *) (old);		\
		if (MR_trail_ptr != old_trail_ptr) {		\
			/* MR_save_transient_registers(); */	\
			MR_untrail_to(old_trail_ptr, (kind));	\
			/* MR_restore_transient_registers(); */	\
		}						\
	} while(0)

/*---------------------------------------------------------------------------*/
/*
** The following stuff defines the Mercury trail.
** All of the stuff in the section below is implementation details.
** Do not use it.  Instead, use the interface functions/macros
** defined in the next section.
*/
/*---------------------------------------------------------------------------*/

/*
** MR_untrail_reason defines the possible reasons why the trail is to be
** traversed.
*/
typedef enum {
	/*
	** MR_undo:
	** Ordinary backtracking on failure.
	** Function trail entries are invoked and value
	** trail entries are used to restore memory.
	** Then these trail entries are discarded.
	*/
	MR_undo,       

	/*
	** MR_commit:
	** A hard (pruning) commit.
	** Occurs when nondet/multi goal is called in a cc_nondet/cc_multi
	** context, or when a nondet/multi goal has no output variables.
	** Function trail entries are invoked.
	*/
	MR_commit,     

	/*
	** MR_solve:
	** A soft (non-pruning) commit.
	** Used for the check for floundering in solutions/2
	** and in nondet if-the-elses.
	** Function trail entries are invoked.
	*/
	MR_solve,      

	/*
	** MR_exception:
	** An exception was thrown.
	** Behaves as MR_undo, except that function trail entries may
	** choose to behave differently for exceptions than for failure.
	*/
	MR_exception,  

	/*
	** MR_retry:
	** A `retry' command was executed in the debugger.
	** Behaves as MR_undo, except that function trail entries may
	** choose to behave differently for retries than for failure.
	*/
	MR_retry,  

	/*
	** MR_gc:
	** Reserved for future use.
	** The interface between the trail and accurate
	** garbage collection is not yet designed.
	*/
	MR_gc

} MR_untrail_reason;

typedef enum {
	MR_val_entry,
	MR_func_entry
} MR_trail_entry_kind;
#define MR_LAST_TRAIL_ENTRY_KIND MR_func_entry

/*
** MR_USE_TAGGED_TRAIL is true iff we have enough tag bits to store
** an MR_trail_entry_kind as a pointer tag.
*/
#define MR_USE_TAGGED_TRAIL ((1<<MR_TAGBITS) > MR_LAST_TRAIL_ENTRY_KIND)

typedef void MR_untrail_func_type(void *datum, MR_untrail_reason);

typedef struct {
#if !(MR_USE_TAGGED_TRAIL)
	MR_trail_entry_kind MR_entry_kind;
#endif
	union {
		struct {
			MR_Word *MR_address;
			MR_Word MR_value;
		} MR_val;
		struct {
			MR_untrail_func_type *MR_untrail_func;
			void *MR_datum;
		} MR_func;
	} MR_union;
} MR_TrailEntry;

/*
** Macros for accessing these fields, taking tagging into account.
** DO NOT ACCESS THE FIELDS DIRECTLY.
*/

#if MR_USE_TAGGED_TRAIL
  #define MR_func_trail_tag	MR_mktag(MR_func_entry)
  #define MR_value_trail_tag	MR_mktag(MR_val_entry)

  /*
  ** MR_trail_entry_kind MR_get_trail_entry_kind(const MR_trail_entry *);
  */
  #define MR_get_trail_entry_kind(entry)				\
	((MR_trail_entry_kind)						\
	  (MR_tag((MR_Word) (entry)->MR_union.MR_val.MR_address)))

  /*
  ** MR_Word * MR_get_trail_entry_address(const MR_trail_entry *);
  */
  #define MR_get_trail_entry_address(entry) \
	((MR_Word *)							\
	  MR_body((entry)->MR_union.MR_val.MR_address, MR_value_trail_tag))

  /*
  ** MR_untrail_func_type *
  ** MR_get_trail_entry_untrail_func(const MR_trail_entry *);
  */
  #define MR_get_trail_entry_untrail_func(entry)			\
	((MR_untrail_func_type *)					\
	    MR_body((MR_Word) (entry)->MR_union.MR_func.MR_untrail_func,\
		     MR_func_trail_tag))

  /*
  ** void MR_store_value_trail_entry(
  **		MR_trail_entry *entry, MR_Word *address, MR_Word value);
  */
  #define MR_store_value_trail_entry(entry, address, value)		\
	  do {								\
		(entry)->MR_union.MR_val.MR_address =			\
			(MR_Word *) (MR_Word)				\
			  MR_mkword(MR_value_trail_tag, (address));	\
		(entry)->MR_union.MR_val.MR_value = (value);		\
	  } while (0)

  /*
  ** void MR_store_function_trail_entry(
  **		MR_trail_entry * func, MR_untrail_func entry, void *datum);
  */
  #define MR_store_function_trail_entry(entry, func, datum)		\
	  do {								\
		(entry)->MR_union.MR_func.MR_untrail_func =		\
			(MR_untrail_func_type *) (MR_Word)		\
			  MR_mkword(MR_func_trail_tag, (func));		\
		(entry)->MR_union.MR_func.MR_datum = (datum);		\
	  } while (0)
#else /* !MR_USE_TAGGED_TRAIL */
  #define MR_get_trail_entry_kind(entry) ((entry)->MR_kind)

  #define MR_get_trail_entry_address(entry) \
	((entry)->MR_union.MR_val.MR_address)

  #define MR_get_trail_entry_untrail_func(entry) \
	((entry)->MR_union.MR_func.MR_untrail_func)

  /*
  ** void MR_store_value_trail_entry(
  **		MR_trail_entry *entry, MR_Word *address, MR_Word value);
  */
  #define MR_store_value_trail_entry(entry, address, value)		\
	  do {								\
		(entry)->MR_kind = MR_val_entry;			\
		(entry)->MR_union.MR_val.MR_address = (address);	\
		(entry)->MR_union.MR_val.MR_value = (value);		\
	  } while (0)

  /*
  ** void MR_store_function_trail_entry_kind(
  **		MR_trail_entry *entry, MR_untrail_func *func, void *datum);
  */
  #define MR_store_function_trail_entry(entry, func, datum)		\
	  do {								\
		(entry)->MR_kind = MR_func_entry;			\
		(entry)->MR_union.MR_func.MR_untrail_func = (func);	\
		(entry)->MR_union.MR_func.MR_datum = (datum);		\
	  } while (0)
#endif

/*
** MR_Word MR_get_trail_entry_value(const MR_trail_entry *);
*/
#define MR_get_trail_entry_value(entry) \
	((entry)->MR_union.MR_val.MR_value)

/*
** void * MR_get_trail_entry_datum(const MR_trail_entry *);
*/
#define MR_get_trail_entry_datum(entry) \
	((entry)->MR_union.MR_func.MR_datum)

/*---------------------------------------------------------------------------*/

/* The Mercury trail */
extern MR_MemoryZone *MR_trail_zone;

/* Pointer to the current top of the Mercury trail */
/*
** N.B. Use `MR_trail_ptr', defined in mercury_regorder.h,
** not `MR_trail_ptr_var'.
*/
extern MR_TrailEntry *MR_trail_ptr_var;

/*
** An integer variable that holds the current choice point identifier;
** it is allocated a new value whenever we create a choice
** point (including semidet choice points, e.g. in an if-then-else)
** and it is reset whenever a choice point is backtracked over
** or pruned away.
**
** N.B.  Use `MR_ticket_counter', defined in mercury_regorder.h,
** not `MR_ticket_counter_var'.
*/
extern MR_Unsigned MR_ticket_counter_var;

/*
** An integer variable that is incremented whenever we create a choice
** point (including semidet choice points, e.g. in an if-then-else)
** and decremented or reset whenever we backtrack over one,
** but which is _not_ decremented or reset when a choice point is
** pruned away with a commit.
**
** N.B.  Use `MR_ticket_high_water', defined in mercury_regorder.h,
** not `MR_ticket_high_water_var'.
*/
extern MR_Unsigned MR_ticket_high_water_var;

/*---------------------------------------------------------------------------*/
/*
** This is the interface that should be used by C code that wants to
** do trailing.
**
** This stuff is documented in the "Trailing" section of the
** Mercury language reference manual.
*/
/*---------------------------------------------------------------------------*/

/*
** void  MR_trail_value(MR_Word *address, MR_Word value);
**
** Make sure that when the current execution is
** backtracked over, `value' is placed in `address'.
*/
#define MR_trail_value(address, value)		\
	do {							\
		MR_store_value_trail_entry(MR_trail_ptr,	\
			(address), (value));			\
		MR_trail_ptr++;					\
	} while(0);

/*
** void  MR_trail_current_value(MR_Word *address);
**
** Make sure that when the current execution is
** backtracked over, the value currently in `address'
** is restored.
*/
#define MR_trail_current_value(address) \
	MR_trail_value((address), *(address))

/*
** void MR_trail_function(void (*untrail_func)(void *, MR_untrail_reason),
**		void *datum);
**
** Make sure that when the current execution is
** backtracked over, (*untrail_func)(value, MR_undo) is called.
** Also make sure that if the current choicepoint is
** trimmed without being backtracked over (ie, the
** current choice is committed to), then
** (*untrail_func)(value, MR_commit) is called.
*/
#define MR_trail_function(untrail_func, datum)				\
	do {								\
		MR_store_function_trail_entry((MR_trail_ptr),		\
			(untrail_func), (datum));			\
		MR_trail_ptr++;						\
	} while(0);

/*
** Apply all the trail entries between MR_trail_ptr and old_trail_ptr.
*/
void MR_untrail_to(MR_TrailEntry *old_trail_ptr, MR_untrail_reason reason);

/* abstract type */
typedef MR_Unsigned MR_ChoicepointId;

/*
** MR_ChoicepointId MR_current_choicepoint_id(void);
**
** Returns a value indicative of the current choicepoint. 
** The value remains meaningful if the choicepoint is pruned
** away by a commit, but is not meaningful after backtracking
** past the point where the choicepoint was created (since
** choicepoint ids may be reused after backtracking).
**
** If we execute
** 
** 	oldcp = MR_current_choicepoint_id();
** 	... and a long time later ...
** 	if (oldcp == MR_current_choicepoint_id()) {A}
** 
** then we can be assured that if the choicepoint current
** at the time of the first call to MR_current_choicepoint()
** has not been backtracked over before the second call,
** then code A will be executed if and only if the
** current choicepoint is the same in both calls.
*/
#define MR_current_choicepoint_id() ((const MR_ChoicepointId) MR_ticket_counter)

/*
** MR_ChoicepointId MR_null_choicepoint_id(void);
**
** A macro defining a "null" ChoicepointId.
** This is suitable for use in static initializers.
*/
#define MR_null_choicepoint_id() ((const MR_ChoicepointId)0)

/*
** MR_bool MR_choicepoint_newer(MR_ChoicepointId x, MR_ChoicepointId y);
**
** Returns true iff the choicepoint indicated by `x'
** is newer than (i.e. was created more recently than)
** the choicepoint indicated by `y'.
** The null ChoicepointId is considered older than
** any non-null ChoicepoindId.
** If either of the choice points have been
** backtracked over, the behaviour is undefined.
*/
#define MR_choicepoint_newer(x, y) ((x) > (y))

#endif /* not MERCURY_TRAIL_H */
