/*
** vim: ts=4 sw=4 expandtab
*/
/*
INIT mercury_sys_init_threadscope
ENDINIT
*/
/*
** Copyright (C) 2009-2010 The University of Melbourne.
** Copyright (C) 2008-2009 The GHC Team.
**
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** Event log format
** 
** The log format is designed to be extensible: old tools should be
** able to parse (but not necessarily understand all of) new versions
** of the format, and new tools will be able to understand old log
** files.
** 
** Each event has a specific format.  If you add new events, give them
** new numbers: we never re-use old event numbers.
**
** - The format is endian-independent: all values are represented in 
**    bigendian order.
**
** - The format is extensible:
**
**    - The header describes each event type and its length.  Tools
**      that don't recognise a particular event type can skip those events.
**
**    - There is room for extra information in the event type
**      specification, which can be ignored by older tools.
**
**    - Events can have extra information added, but existing fields
**      cannot be changed.  Tools should ignore extra fields at the
**      end of the event record.
**
**    - Old event type ids are never re-used; just take a new identifier.
**
**
** The format
** ----------
**
** log : EVENT_HEADER_BEGIN
**       EventType*
**       EVENT_HEADER_END
**       EVENT_DATA_BEGIN
**       Event*
**       EVENT_DATA_END
**
** EventType :
**       EVENT_ET_BEGIN
**       Word16         -- unique identifier for this event
**       Int16          -- >=0  size of the event in bytes (minus the header)
**                      -- -1   variable size
**       Word32         -- length of the next field in bytes
**       Word8*         -- string describing the event
**       Word32         -- length of the next field in bytes
**       Word8*         -- extra info (for future extensions)
**       EVENT_ET_END
**
** Event : 
**       Word16         -- event_type
**       Word64         -- time (nanosecs)
**       [Word16]       -- length of the rest (for variable-sized events only)
**       ... extra event-specific info ...
**
** All values a packed, no attempt is made to align them.
**
** New events must be registered with GHC.  These are kept in the GHC-events
** package.
**
*/

#include "mercury_imp.h"

#include "mercury_threadscope.h"

#include "mercury_atomic_ops.h"

#include <stdio.h>
#include <string.h>

#ifdef MR_THREADSCOPE

/***************************************************************************/

/*
** Markers for begin/end of the Header.
*/
#define MR_TS_EVENT_HEADER_BEGIN    0x68647262 /* 'h' 'd' 'r' 'b' */
#define MR_TS_EVENT_HEADER_END      0x68647265 /* 'h' 'd' 'r' 'e' */

#define MR_TS_EVENT_DATA_BEGIN      0x64617462 /* 'd' 'a' 't' 'b' */
#define MR_TS_EVENT_DATA_END        0xffff

/*
** Markers for begin/end of the list of Event Types in the Header.
** Header, Event Type, Begin = hetb
** Header, Event Type, End = hete
*/
#define MR_TS_EVENT_HET_BEGIN       0x68657462 /* 'h' 'e' 't' 'b' */
#define MR_TS_EVENT_HET_END         0x68657465 /* 'h' 'e' 't' 'e' */

/*
** Markers for the beginning and end of individual event types.
*/
#define MR_TS_EVENT_ET_BEGIN        0x65746200 /* 'e' 't' 'b' 0 */
#define MR_TS_EVENT_ET_END          0x65746500 /* 'e' 't' 'e' 0 */

/*
** The threadscope events:
*/
#define MR_TS_EVENT_CREATE_THREAD        0 /* (thread)               */
#define MR_TS_EVENT_RUN_THREAD           1 /* (thread)               */
#define MR_TS_EVENT_STOP_THREAD          2 /* (thread, status)       */
#define MR_TS_EVENT_THREAD_RUNNABLE      3 /* (thread)               */
#define MR_TS_EVENT_MIGRATE_THREAD       4 /* (thread, new_cap)      */
#define MR_TS_EVENT_RUN_SPARK            5 /* (thread)               */
#define MR_TS_EVENT_STEAL_SPARK          6 /* (thread, victim_cap)   */
#define MR_TS_EVENT_SHUTDOWN             7 /* ()                     */
#define MR_TS_EVENT_THREAD_WAKEUP        8 /* (thread, other_cap)    */
#define MR_TS_EVENT_GC_START             9 /* ()                     */
#define MR_TS_EVENT_GC_END              10 /* ()                     */
#define MR_TS_EVENT_REQUEST_SEQ_GC      11 /* ()                     */
#define MR_TS_EVENT_REQUEST_PAR_GC      12 /* ()                     */
#define MR_TS_EVENT_CREATE_SPARK_THREAD 15 /* (spark_thread)         */
#define MR_TS_EVENT_LOG_MSG             16 /* (message ...)          */
#define MR_TS_EVENT_STARTUP             17 /* (num_capabilities)     */
#define MR_TS_EVENT_BLOCK_MARKER        18 /* (size, end_time, capability) */
#define MR_TS_EVENT_USER_MSG            19 /* (message ...)          */
#define MR_TS_EVENT_GC_IDLE             20 /* () */
#define MR_TS_EVENT_GC_WORK             21 /* () */
#define MR_TS_EVENT_GC_DONE             22 /* () */
#define MR_TS_EVENT_CALL_MAIN           23 /* () */
#define MR_TS_EVENT_LOOKING_FOR_GLOBAL_WORK \
                                        24 /* () */

#define MR_TS_NUM_EVENT_TAGS            25

#if 0  /* DEPRECATED EVENTS: */
#define EVENT_CREATE_SPARK        13 /* (cap, thread) */
#define EVENT_SPARK_TO_THREAD     14 /* (cap, thread, spark_thread) */
#endif

/*
** GHC uses 2MB per buffer.  Note that the minimum buffer size is the size of
** the largest message plus the size of the block marker message, however it is
** _sensible_ for the buffer to be much larger so that we make system calls
** less often.
*/
#define MR_TS_BUFFERSIZE (2*1024*1024)
#define MR_TS_FILENAME_FORMAT ("%s.eventlog")
#define MR_TSC_SYNC_NUM_ROUNDS (10)
#define MR_TSC_SYNC_NUM_BEST_ROUNDS (3)

/* Uncomment this to enable some debugging code */
/* #define MR_DEBUG_THREADSCOPE 1 */

#if MR_DEBUG_THREADSCOPE
#define MR_DO_THREADSCOPE_DEBUG(x) do { x; } while(0)
#else
#define MR_DO_THREADSCOPE_DEBUG(x)
#endif

/***************************************************************************/

struct MR_threadscope_event_buffer {
    MR_UnsignedChar     MR_tsbuffer_data[MR_TS_BUFFERSIZE];

    /* The current writing position in the buffer. */
    MR_Unsigned         MR_tsbuffer_pos;

    /* The position of the start of the most recent block. */
    MR_Integer          MR_tsbuffer_block_open_pos;

    /* 
    ** True if the engine's current context is stopped and therefore stop and
    ** start events should not be posted from the GC callback procedures.
    */
    MR_bool             MR_tsbuffer_ctxt_is_stopped;

    /* A cheap userspace lock to make buffers reentrant. */
    volatile MR_Us_Lock MR_tsbuffer_lock;
};

/*
** We define some types and functions to write them.  These types are set
** carefully to match the ones that GHC uses.
*/
typedef MR_uint_least16_t   EventType;
typedef MR_uint_least64_t   Time;
typedef MR_int_least64_t    Timedelta;

/*
** The difference between two positions in the eventlog file measured in bytes.
*/
typedef MR_uint_least32_t   EventlogOffset;

typedef struct {
    EventType   etd_event_type;
    const char  *etd_description;
} EventTypeDesc;

/***************************************************************************/

static EventTypeDesc event_type_descs[] = {
    {
        /*
        ** The startup event informs threadscope of the number of engines we're
        ** using.  It should be given outside of a block.
        */
        MR_TS_EVENT_STARTUP,
        "Startup (num_engines)"
    },
    { 
        /*
        ** The last event in the log.  It should be given outside of a block.
        */
        MR_TS_EVENT_SHUTDOWN, "Shutdown"
    },
    {
        /*
        ** A block of events belonging to the named engine follows,
        ** The length of this block is given including the block message
        ** itself, the time that this block finishes is also given.
        ** Blocks _must not_ exist within other blocks.
        */
        MR_TS_EVENT_BLOCK_MARKER, 
        "A block of events generated by a specific engine follows" 
    },
    {
        /*
        ** Called when a context is created or re-used.
        */
        MR_TS_EVENT_CREATE_THREAD,
        "A context is created or re-used"
    },
    {
        /*
        ** Called from MR_schedule_context()
        */
        MR_TS_EVENT_THREAD_RUNNABLE,
        "The context is being placed on the run queue"
    },
    {
        /*
        ** The named context begun executing on the engine named by the current
        ** block.
        */
        MR_TS_EVENT_RUN_THREAD, "Run context"
    },
    {
        /*
        ** The named context finished executing on the engine named by the
        ** current block.  The reason why the context stopped is given.
        */
        MR_TS_EVENT_STOP_THREAD, 
        "Context stopped"
    },
    {
        /*
        ** This event is posted when a context is created for a spark.
        */
        MR_TS_EVENT_CREATE_SPARK_THREAD,
        "Create a context for executing a spark"
    },
    {
        MR_TS_EVENT_LOG_MSG,
        "A user-provided log message"
    },
    {
        /*
        ** Start a garbage collection run
        */
        MR_TS_EVENT_GC_START,
        "Start GC"
    },
    {
        /*
        ** Stop a garbage collection run
        */
        MR_TS_EVENT_GC_END,
        "Stop GC",
    },
    {
        /*
        ** The runtime system is about to call main/2.  This message has no
        ** parameters.
        */
        MR_TS_EVENT_CALL_MAIN,
        "About to call main/2"
    },
    {
        MR_TS_EVENT_LOOKING_FOR_GLOBAL_WORK,
        "Engine begins looking for global work"
    },
    {
        /* Mark the end of this array. */
        MR_TS_NUM_EVENT_TAGS, NULL
    }
};

static MR_uint_least16_t event_type_sizes[] = {
    [MR_TS_EVENT_STARTUP]           = 2, /* MR_EngineId */
    [MR_TS_EVENT_SHUTDOWN]          = 0,
    [MR_TS_EVENT_BLOCK_MARKER]      = 4 + 8 + 2, 
                                      /* EnginelogOffset, Time, MR_EngineId */
    [MR_TS_EVENT_CREATE_THREAD]     = 4, /* MR_ContextId */
    [MR_TS_EVENT_THREAD_RUNNABLE]   = 4, /* MR_ContextId */
    [MR_TS_EVENT_RUN_THREAD]        = 4, /* MR_ContextId */
    [MR_TS_EVENT_STOP_THREAD]       = 4 + 2,
                                      /* MR_ContextId, MR_ContextStopReason */
    [MR_TS_EVENT_CREATE_SPARK_THREAD] = 4, /* MR_ContextId */
    [MR_TS_EVENT_LOG_MSG]           = -1, /* Variable size event */
    [MR_TS_EVENT_GC_START]          = 0,
    [MR_TS_EVENT_GC_END]            = 0,
    [MR_TS_EVENT_CALL_MAIN]         = 0,
    [MR_TS_EVENT_LOOKING_FOR_GLOBAL_WORK] = 0,
};

static FILE* MR_threadscope_output_file = NULL;
static char* MR_threadscope_output_filename;

/*
** The TSC value recorded when the primordial thread called
** MR_setup_threadscope(), this is used retroactivly to initialise the
** MR_eng_cpu_clock_ticks_offset field in the engine structure once it is
** created.
*/
static MR_uint_least64_t MR_primordial_first_tsc; 

static MercuryLock      MR_next_engine_id_lock;
static MR_EngineId      MR_next_engine_id = 0;

static Timedelta        MR_global_offset;

static struct MR_threadscope_event_buffer global_buffer;

/***************************************************************************/

/*
** Is there enough room for this statically sized event in the current engine's
** buffer _and_ enough room for the block marker event.
*/
static __inline__ MR_bool
enough_room_for_event(
        struct MR_threadscope_event_buffer *buffer,
        EventType event_type) 
{
    return (buffer->MR_tsbuffer_pos + event_type_sizes[event_type] +
                event_type_sizes[MR_TS_EVENT_BLOCK_MARKER] +
                ((2 + 8) * 2)) /* (EventType, Time) * 2 */
            < MR_TS_BUFFERSIZE; 
}

static __inline__ MR_bool
enough_room_for_variable_size_event(
        struct MR_threadscope_event_buffer *buffer,
        MR_Unsigned length)
{
    return (buffer->MR_tsbuffer_pos + length + 
                event_type_sizes[MR_TS_EVENT_BLOCK_MARKER] +
                ((2 + 8) * 2) + 2) /* (EventType, Time) * 2 + StringLength */
        - MR_TS_BUFFERSIZE;
}

/*
** Is a block currently open?
*/
static __inline__ MR_bool block_is_open(
        struct MR_threadscope_event_buffer *buffer)
{
    return !(buffer->MR_tsbuffer_block_open_pos == -1);
}

/*
** Put words into the current engine's buffer in big endian order.
*/
static __inline__ void put_byte(
        struct MR_threadscope_event_buffer *buffer, 
        int byte) 
{
    buffer->MR_tsbuffer_data[buffer->MR_tsbuffer_pos++] = byte;
}

static __inline__ void put_be_int16(
        struct MR_threadscope_event_buffer *buffer,
        MR_int_least16_t word) 
{
    put_byte(buffer, (word >> 8) & 0xFF);
    put_byte(buffer, word & 0xFF);
}

static __inline__ void put_be_uint16(
        struct MR_threadscope_event_buffer *buffer,
        MR_uint_least16_t word) 
{
    put_byte(buffer, (word >> 8) & 0xFF);
    put_byte(buffer, word & 0xFF);
}

static __inline__ void put_be_uint32(
        struct MR_threadscope_event_buffer *buffer,
        MR_uint_least32_t word) 
{
    put_be_uint16(buffer, (word >> 16) & 0xFFFF);
    put_be_uint16(buffer, word & 0xFFFF);
}

static __inline__ void put_be_uint64(
        struct MR_threadscope_event_buffer *buffer,
        MR_uint_least64_t word)
{
    put_be_uint32(buffer, (word >> 32) & 0xFFFFFFFF);
    put_be_uint32(buffer, word & 0xFFFFFFFF);
}

static __inline__ void put_raw_string(
        struct MR_threadscope_event_buffer *buffer,
        const char *string,
        unsigned len)
{
    unsigned i;
    for (i = 0; i < len; i++) {
        put_byte(buffer, string[i]);
    }
}

/*
** Put a string in the given buffer,  The string will be preceeded by a 16 bit
** integer giving the string's length.
*/
static __inline__ void put_string_size16(
        struct MR_threadscope_event_buffer *buffer,
        const char *string)
{
    unsigned i, len;

    len = strlen(string);
    put_be_uint16(buffer, len);
    put_raw_string(buffer, string, len);
}

/*
** Put a string in the given buffer,  The string will be preceeded by a 32 bit
** integer giving the string's length.
*/
static __inline__ void put_string_size32(
        struct MR_threadscope_event_buffer *buffer,
        const char *string)
{
    unsigned i, len;

    len = strlen(string);
    put_be_uint32(buffer, len);
    put_raw_string(buffer, string, len);
}

static __inline__ void put_timestamp(
        struct MR_threadscope_event_buffer *buffer,
        Time timestamp) 
{
    put_be_uint64(buffer, timestamp);
}

static __inline__ void put_eventlog_offset(
        struct MR_threadscope_event_buffer *buffer,
        EventlogOffset offset) 
{
    put_be_uint32(buffer, offset);
}

static __inline__ void put_event_header(
        struct MR_threadscope_event_buffer *buffer,
        EventType event_type, Time timestamp) 
{
    put_be_uint16(buffer, event_type);
    put_timestamp(buffer, timestamp);
}

static __inline__ void put_engine_id(
        struct MR_threadscope_event_buffer *buffer,
        MR_EngineId engine_num) 
{
    put_be_uint16(buffer, engine_num);
}

static __inline__ void put_context_id(
        struct MR_threadscope_event_buffer *buffer,
        MR_ContextId context_id) 
{
    put_be_uint32(buffer, context_id);
}

static __inline__ void put_stop_reason(
        struct MR_threadscope_event_buffer *buffer,
        MR_ContextStopReason reason) 
{
    put_be_uint16(buffer, reason);
}

/***************************************************************************/

static struct MR_threadscope_event_buffer* 
MR_create_event_buffer(void);

/*
** The prelude is everything up to and including the 'DATA_BEGIN' marker
*/
static void 
MR_open_output_file_and_write_prelude(void);

static void
MR_close_output_file(void);

static void
put_event_type(struct MR_threadscope_event_buffer *buffer, 
    EventTypeDesc *event_type);

static MR_bool 
flush_event_buffer(struct MR_threadscope_event_buffer *buffer); 

static void
maybe_close_block(struct MR_threadscope_event_buffer *buffer);

static void
open_block(struct MR_threadscope_event_buffer *buffer, MR_Unsigned eng_id);

static void
start_gc_callback(void);
static void
stop_gc_callback(void);
static void
pause_thread_gc_callback(void);
static void
resume_thread_gc_callback(void);

/***************************************************************************/

static MR_uint_least64_t 
get_current_time_nanosecs(void);

/***************************************************************************/

void
MR_setup_threadscope(void) 
{
    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In setup threadscope thread: 0x%lx\n", pthread_self())
    );
    /* This value is used later when setting up the primordial engine */
    MR_primordial_first_tsc = MR_read_cpu_tsc();
   
    /* Setup locks. */
    pthread_mutex_init(&MR_next_engine_id_lock, MR_MUTEX_ATTR);
    
    /*
    ** These variables are used for TSC synchronization which is not used.  See
    ** below.
    **
    pthread_mutex_init(&MR_tsc_sync_slave_lock, MR_MUTEX_ATTR);
    MR_US_COND_CLEAR(&MR_tsc_sync_slave_entry_cond);
    MR_US_COND_CLEAR(&MR_tsc_sync_master_entry_cond);
    MR_US_COND_CLEAR(&MR_tsc_sync_t0);
    MR_US_COND_CLEAR(&MR_tsc_sync_t1);
    */
    
    /* Configure Boehm */
    GC_mercury_callback_start_collect = start_gc_callback;
    GC_mercury_callback_stop_collect = stop_gc_callback;
    GC_mercury_callback_pause_thread = pause_thread_gc_callback;
    GC_mercury_callback_resume_thread = resume_thread_gc_callback;

    /* Clear the global buffer and setup the file */
    global_buffer.MR_tsbuffer_pos = 0;
    global_buffer.MR_tsbuffer_block_open_pos = -1;
    global_buffer.MR_tsbuffer_lock = MR_US_LOCK_INITIAL_VALUE;
    MR_open_output_file_and_write_prelude();
    
    /*
    ** Put the startup event in the buffer.
    */
    put_event_header(&global_buffer, MR_TS_EVENT_STARTUP, 0);
    put_engine_id(&global_buffer, (MR_EngineId)MR_num_threads);
    flush_event_buffer(&global_buffer);
}

void
MR_finalize_threadscope(void) 
{
    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In finalize threadscope thread: 0x%lx\n", pthread_self())
    );
    flush_event_buffer(&global_buffer);
    MR_close_output_file();
}

void
MR_threadscope_setup_engine(MercuryEngine *eng) 
{
    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In threadscope setup engine thread: 0x%lx\n", pthread_self())
    );
    MR_LOCK(&MR_next_engine_id_lock, "MR_get_next_engine_id");
    eng->MR_eng_id = MR_next_engine_id++;
    MR_UNLOCK(&MR_next_engine_id_lock, "MR_get_next_engine_id");

    if (eng->MR_eng_id == 0) {
        MR_global_offset = -MR_primordial_first_tsc;
    }
    eng->MR_eng_cpu_clock_ticks_offset = MR_global_offset;

    eng->MR_eng_ts_buffer = MR_create_event_buffer();
}

void
MR_threadscope_finalize_engine(MercuryEngine *eng)
{
    struct MR_threadscope_event_buffer *buffer = eng->MR_eng_ts_buffer;
    
    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In threadscope finalize engine thread: 0x%lx\n", pthread_self())
    );
    
    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    
    if (!enough_room_for_event(buffer, MR_TS_EVENT_SHUTDOWN)) {
        flush_event_buffer(buffer);
        open_block(buffer, eng->MR_eng_id);
    } else if (!block_is_open(buffer)) {
        open_block(buffer, eng->MR_eng_id);
    }
    put_event_header(buffer, MR_TS_EVENT_SHUTDOWN, get_current_time_nanosecs());

    flush_event_buffer(buffer);
    eng->MR_eng_ts_buffer = NULL;
    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

#if 0
/*
** It looks like we don't need this on modern CPUs including multi-socket
** systems (goliath).  If we find systems where this is needed we can enable it
** via a runtime check.
*/
/*
** The synchronization of TSCs operates as follows:
** The master and slave enter their functions.  Both threads spin until the
** other is ready (signaling the other before they begin spinning).  Then for
** MR_TSC_SYNC_NUM_ROUNDS: The master spins waiting for the slave.  The slave
** records it's current TSC, signals the master and spins waiting for a reply.
** The master upon hearing from the slave records it's TSC and then signals
** the slave.  The slave can then compute the delay in this round.  The slave
** takes the NR_TSC_SYNC_NUM_BEST_ROUNDS best delays (smallest) and computes
** the offset as the average between between the difference of the clocks based
** on Cristan's algorithm (1989).
*/

typedef struct {
    Timedelta   delay;
    Timedelta   offset;
} TimeDelayOffset;

static MercuryLock          MR_tsc_sync_slave_lock;
volatile static MR_Us_Cond  MR_tsc_sync_slave_entry_cond; 
volatile static MR_Us_Cond  MR_tsc_sync_master_entry_cond; 
volatile static MR_Us_Cond  MR_tsc_sync_t0; 
volatile static MR_Us_Cond  MR_tsc_sync_t1; 
static Time                 MR_tsc_sync_master_time;

static int
compare_time_delay_offset_by_delay(const void *a, const void *b); 

void
MR_threadscope_sync_tsc_master(void)
{
    unsigned i;

    /*
    ** Wait for a slave to enter.
    */
    MR_US_COND_SET(&MR_tsc_sync_master_entry_cond);
    MR_US_SPIN_COND(&MR_tsc_sync_slave_entry_cond);
    MR_US_COND_CLEAR(&MR_tsc_sync_slave_entry_cond);
   
    for (i = 0; i < MR_TSC_SYNC_NUM_ROUNDS; i++) {
        /*
        ** Wait to receive the message from the slave at T0
        */
        MR_US_SPIN_COND(&MR_tsc_sync_t0);
        MR_US_COND_CLEAR(&MR_tsc_sync_t0);

        /*
        ** Read our TSC and send the slave a message.
        */
        MR_tsc_sync_master_time = MR_read_cpu_tsc();
        MR_US_COND_SET(&MR_tsc_sync_t1);
    }

}

void
MR_threadscope_sync_tsc_slave(void)
{
    unsigned        i, j;
    TimeDelayOffset delay_offset[MR_TSC_SYNC_NUM_ROUNDS];
    Timedelta       total_offset;
    MercuryEngine   *eng = MR_thread_engine_base;

    /*
    ** Only one slave may enter at a time.
    */
    MR_LOCK(&MR_tsc_sync_slave_lock, "MR_threadscope_sync_tsc_slave");

    /*
    ** Tell the master we're ready to begin and wait for it to tell us it's ready.
    */
    MR_US_COND_SET(&MR_tsc_sync_slave_entry_cond);
    MR_US_SPIN_COND(&MR_tsc_sync_master_entry_cond);
    MR_US_COND_CLEAR(&MR_tsc_sync_master_entry_cond);
    
    for (i = 0; i < MR_TSC_SYNC_NUM_ROUNDS; i++) {
        Time    slave_tsc_at_t0;
        Time    slave_tsc_at_t2;

        /*
        ** Get the current time and signal that we've done so (T=0).
        */
        slave_tsc_at_t0 = MR_read_cpu_tsc();
        MR_US_COND_SET(&MR_tsc_sync_t0);

        /*
        ** Wait for the master to reply, the master handles T=1, here we
        ** proceed to T=2.
        */
        MR_US_SPIN_COND(&MR_tsc_sync_t1);
        slave_tsc_at_t2 = MR_read_cpu_tsc();
        MR_US_COND_CLEAR(&MR_tsc_sync_t1);

        /*
        ** Here are Cristian's formulas.  Delay is the round trip time,
        ** slave_tsc_at_t0 + delay/2 is the time on the slave's clock that the
        ** master processed the slaves message and sent it's own.  This is
        ** accurate if the communication delays in either direction are
        ** uniform, that is communication latency is synchronous.
        */
        delay_offset[i].delay = slave_tsc_at_t2 - slave_tsc_at_t0;
        delay_offset[i].offset = 
            MR_tsc_sync_master_time - (slave_tsc_at_t0 + delay_offset[i].delay/2);
    }
    /* By now the master thread will return and continue with it's normal work. */

    /*
    ** We do this debugging output while holding the lock, so that the output
    ** is reasonable.
    */
    MR_DO_THREADSCOPE_DEBUG({
        fprintf(stderr, "TSC Synchronization for thread 0x%x\n", pthread_self()); 
        for (i = 0; i < MR_TSC_SYNC_NUM_ROUNDS; i++) {
            fprintf(stderr, "delay: %ld offset (local + global = total) %ld + %ld = %ld\n",
                delay_offset[i].delay, delay_offset[i].offset, MR_global_offset, 
                delay_offset[i].offset + MR_global_offset);
        }
    });
    MR_UNLOCK(&MR_tsc_sync_slave_lock, "MR_threadscope_sync_tsc_slave");
    
    /*
    ** Now to average the best offsets.
    */
    qsort(&delay_offset, MR_TSC_SYNC_NUM_ROUNDS, sizeof(TimeDelayOffset), 
        compare_time_delay_offset_by_delay);
    total_offset = 0;
    for (i = 0; i < MR_TSC_SYNC_NUM_BEST_ROUNDS; i++) {
        total_offset = delay_offset[i].offset;
    }
    eng->MR_eng_cpu_clock_ticks_offset = total_offset + MR_global_offset;
    
    MR_DO_THREADSCOPE_DEBUG({
        fprintf(stderr, "TSC Synchronization offset for thread 0x%x: %ld\n", 
            pthread_self(), eng->MR_eng_cpu_clock_ticks_offset);
    });
}

static int
compare_time_delay_offset_by_delay(const void *a, const void *b) {
    TimeDelayOffset *tdo_a = (TimeDelayOffset*)a;
    TimeDelayOffset *tdo_b = (TimeDelayOffset*)b;

    if (tdo_a->delay > tdo_b->delay) {
        return 1;
    } else if (tdo_a->delay < tdo_b->delay) {
        return -1;
    } else {
        return 0;
    }
}

#endif

/***************************************************************************/

void
MR_threadscope_post_create_context(MR_Context *context)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);
  
    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    
    if (!enough_room_for_event(buffer, MR_TS_EVENT_CREATE_THREAD)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_EVENT_CREATE_THREAD, get_current_time_nanosecs());
    put_context_id(buffer, context->MR_ctxt_num_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_create_context_for_spark(MR_Context *context)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);
  
    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    
    if (!enough_room_for_event(buffer, MR_TS_EVENT_CREATE_SPARK_THREAD)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_EVENT_CREATE_SPARK_THREAD,
        get_current_time_nanosecs());
    put_context_id(buffer, context->MR_ctxt_num_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_context_runnable(MR_Context *context)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));

    if (!enough_room_for_event(buffer, MR_TS_EVENT_THREAD_RUNNABLE)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_EVENT_THREAD_RUNNABLE, get_current_time_nanosecs());
    put_context_id(buffer, context->MR_ctxt_num_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

static void
MR_threadscope_post_run_context_locked(
    struct MR_threadscope_event_buffer *buffer,
    MR_Context *context)
{
    if (!enough_room_for_event(buffer, MR_TS_EVENT_RUN_THREAD)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_thread_engine_base->MR_eng_id);
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_thread_engine_base->MR_eng_id);
    }
    
    put_event_header(buffer, MR_TS_EVENT_RUN_THREAD, 
        get_current_time_nanosecs());
    put_context_id(buffer, 
        MR_thread_engine_base->MR_eng_this_context->MR_ctxt_num_id);
}

void
MR_threadscope_post_run_context(void)
{
    struct MR_threadscope_event_buffer  *buffer;
    MR_Context                          *context;

    buffer = MR_thread_engine_base->MR_eng_ts_buffer;
   
    context = MR_thread_engine_base->MR_eng_this_context;

    if (context) {
        MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
        MR_threadscope_post_run_context_locked(buffer, context);
        buffer->MR_tsbuffer_ctxt_is_stopped = 0;
        MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
    }
}

static void
MR_threadscope_post_stop_context_locked(
    struct MR_threadscope_event_buffer *buffer,
    MR_Context *context,
    MR_ContextStopReason reason)
{
    if (!enough_room_for_event(buffer, MR_TS_EVENT_STOP_THREAD)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_thread_engine_base->MR_eng_id);
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_thread_engine_base->MR_eng_id);
    }
    
    put_event_header(buffer, MR_TS_EVENT_STOP_THREAD, get_current_time_nanosecs());
    put_context_id(buffer, context->MR_ctxt_num_id);
    put_stop_reason(buffer, reason); 
}

void
MR_threadscope_post_stop_context(MR_ContextStopReason reason) 
{
    struct MR_threadscope_event_buffer  *buffer;
    MR_Context                          *context;

    buffer = MR_thread_engine_base->MR_eng_ts_buffer;
    context = MR_thread_engine_base->MR_eng_this_context;

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    MR_threadscope_post_stop_context_locked(buffer, context, reason);

    buffer->MR_tsbuffer_ctxt_is_stopped = 1;
    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_calling_main(void) {
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);
    
    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_EVENT_CALL_MAIN)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }
    
    put_event_header(buffer, MR_TS_EVENT_CALL_MAIN,
        get_current_time_nanosecs());
    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_looking_for_global_work(void) {
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_EVENT_LOOKING_FOR_GLOBAL_WORK)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_EVENT_LOOKING_FOR_GLOBAL_WORK,
        get_current_time_nanosecs());
    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_log_msg(const char *message) {
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_variable_size_event(buffer, strlen(message))) {
        flush_event_buffer(buffer),
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_EVENT_LOG_MSG,
        get_current_time_nanosecs());
    put_string_size16(buffer, message);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

/***************************************************************************/

static struct MR_threadscope_event_buffer* 
MR_create_event_buffer(void)
{
    struct MR_threadscope_event_buffer* buffer;

    buffer = MR_GC_NEW(MR_threadscope_event_buffer_t);
    buffer->MR_tsbuffer_pos = 0;
    buffer->MR_tsbuffer_block_open_pos = -1;
    buffer->MR_tsbuffer_ctxt_is_stopped = 1;
    buffer->MR_tsbuffer_lock = MR_US_LOCK_INITIAL_VALUE;

    return buffer;
}

/***************************************************************************/
    
static void 
MR_open_output_file_and_write_prelude(void)
{
    MR_Unsigned     filename_len;
    char            *progname_copy;
    char            *progname_base;
    MR_Unsigned     i;

    progname_copy = strdup(MR_progname);
    progname_base = basename(progname_copy);

    /*
    ** This is an over-approximation on the amount of space needed for this
    ** filename.
    */
    filename_len = strlen(progname_base) + strlen(MR_TS_FILENAME_FORMAT) + 1;
    MR_threadscope_output_filename = MR_GC_NEW_ARRAY(char, filename_len);
    snprintf(MR_threadscope_output_filename, filename_len, 
        MR_TS_FILENAME_FORMAT, progname_base);
    free(progname_copy);
    progname_copy = NULL;
    progname_base = NULL;

    MR_threadscope_output_file = fopen(MR_threadscope_output_filename, "w");
    if (!MR_threadscope_output_file) {
        perror(MR_threadscope_output_filename);
        return;
    }

    put_be_uint32(&global_buffer, MR_TS_EVENT_HEADER_BEGIN);
    put_be_uint32(&global_buffer, MR_TS_EVENT_HET_BEGIN);
    for ( i = 0; 
          event_type_descs[i].etd_event_type != MR_TS_NUM_EVENT_TAGS;
          i++) {
        put_event_type(&global_buffer, &event_type_descs[i]);
    }
    put_be_uint32(&global_buffer, MR_TS_EVENT_HET_END);
    put_be_uint32(&global_buffer, MR_TS_EVENT_HEADER_END);
    put_be_uint32(&global_buffer, MR_TS_EVENT_DATA_BEGIN);

    flush_event_buffer(&global_buffer);
}

static void 
MR_close_output_file(void)
{
    if (MR_threadscope_output_file) {
        put_be_uint16(&global_buffer, MR_TS_EVENT_DATA_END);
        if (flush_event_buffer(&global_buffer)) {
            if (EOF == fclose(MR_threadscope_output_file)) {
                perror(MR_threadscope_output_filename);
            }
            MR_threadscope_output_file = NULL;
            MR_threadscope_output_filename = NULL;
        }
    }
}

static void
put_event_type(struct MR_threadscope_event_buffer *buffer, EventTypeDesc *event_type)
{
    put_be_uint32(buffer, MR_TS_EVENT_ET_BEGIN);

    put_be_uint16(buffer, event_type->etd_event_type);
    put_be_int16(buffer, event_type_sizes[event_type->etd_event_type]);

    put_string_size32(buffer, event_type->etd_description);

    /* There is no extended data in any of our events */
    put_be_uint32(buffer, 0);
    
    put_be_uint32(buffer, MR_TS_EVENT_ET_END);
}

static MR_bool 
flush_event_buffer(struct MR_threadscope_event_buffer *buffer) 
{
    maybe_close_block(buffer);

    /*
    ** fwrite handles locking for us, so we have no concurrent access problems.
    */
    if (MR_threadscope_output_file && buffer->MR_tsbuffer_pos) {
        if (0 == fwrite(buffer->MR_tsbuffer_data, buffer->MR_tsbuffer_pos, 1, 
                MR_threadscope_output_file)) {
            perror(MR_threadscope_output_filename);
            MR_threadscope_output_file = NULL;
            MR_threadscope_output_filename = NULL;
        }
    }
    buffer->MR_tsbuffer_pos = 0;

    return (MR_threadscope_output_filename ? MR_TRUE : MR_FALSE);
}

static void
maybe_close_block(struct MR_threadscope_event_buffer *buffer)
{
    MR_Unsigned                         saved_pos;

    if (buffer->MR_tsbuffer_block_open_pos != -1)
    {
        saved_pos = buffer->MR_tsbuffer_pos;
        buffer->MR_tsbuffer_pos = buffer->MR_tsbuffer_block_open_pos +
            sizeof(EventType) + sizeof(Time);
        put_eventlog_offset(buffer, saved_pos - buffer->MR_tsbuffer_block_open_pos);
        put_timestamp(buffer, get_current_time_nanosecs());

        buffer->MR_tsbuffer_block_open_pos = -1;
        buffer->MR_tsbuffer_pos = saved_pos;
    }
}

static void
open_block(struct MR_threadscope_event_buffer *buffer, MR_Unsigned eng_id)
{
    maybe_close_block(buffer);

    /*
    ** Save the old position, close block uses this so that it knows where the
    ** block maker is that it should write into.
    */
    buffer->MR_tsbuffer_block_open_pos = buffer->MR_tsbuffer_pos;

    put_event_header(buffer, MR_TS_EVENT_BLOCK_MARKER, get_current_time_nanosecs());

    /* Skip over the next two fields, they are filled in by close_block */
    buffer->MR_tsbuffer_pos += sizeof(EventlogOffset) + sizeof(Time); 

    put_engine_id(buffer, eng_id);
}

static void 
start_gc_callback(void) 
{
    struct MR_threadscope_event_buffer  *buffer;
    MR_Context                          *context;

    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In gc start callback thread: 0x%lx\n", pthread_self())
    );
    if (MR_thread_engine_base == NULL) {
        return;
    }
    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "\tEngine: 0x%.16lx\n", MR_thread_engine_base)
    );
    buffer = MR_thread_engine_base->MR_eng_ts_buffer;
    if (buffer == NULL) {
        /* GC might be running before we're done setting up */
        return; 
    }
    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "\tBuffer: 0x%.16lx\n", buffer)
    );

    if (MR_US_TRY_LOCK(&(buffer->MR_tsbuffer_lock))) {
        context = MR_thread_engine_base->MR_eng_this_context;
        if (!buffer->MR_tsbuffer_ctxt_is_stopped && context) {
            MR_threadscope_post_stop_context_locked(buffer,
                context, MR_TS_STOP_REASON_HEAP_OVERFLOW);
        }

        if (!enough_room_for_event(buffer, MR_TS_EVENT_GC_START)) {
            flush_event_buffer(buffer);
            open_block(buffer, MR_thread_engine_base->MR_eng_id);
        } else if (!block_is_open(buffer)) {
            open_block(buffer, MR_thread_engine_base->MR_eng_id);
        }
        
        put_event_header(buffer, MR_TS_EVENT_GC_START,
            get_current_time_nanosecs());
        MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
    }
}

static void 
stop_gc_callback(void) 
{
    struct MR_threadscope_event_buffer  *buffer;
    MR_Context                          *context;

    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In gc stop callback thread: 0x%lx\n", pthread_self());
    );
    if (MR_thread_engine_base == NULL) return; 
    buffer = MR_thread_engine_base->MR_eng_ts_buffer;
    if (buffer == NULL) {
        /* GC might be running before we're done setting up */
        return; 
    }
    
    if (MR_US_TRY_LOCK(&(buffer->MR_tsbuffer_lock))) {
        if (!enough_room_for_event(buffer, MR_TS_EVENT_GC_END)) {
            flush_event_buffer(buffer);
            open_block(buffer, MR_thread_engine_base->MR_eng_id);
        } else if (!block_is_open(buffer)) {
            open_block(buffer, MR_thread_engine_base->MR_eng_id);
        }
        
        put_event_header(buffer, MR_TS_EVENT_GC_END, get_current_time_nanosecs());
        
        context = MR_thread_engine_base->MR_eng_this_context;
        if (!buffer->MR_tsbuffer_ctxt_is_stopped && context) {
            MR_threadscope_post_run_context_locked(buffer, context);
        }
        MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
    }
}

static void 
pause_thread_gc_callback(void) 
{
    struct MR_threadscope_event_buffer  *buffer;
    MR_Context                          *context;

    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In gc pause thread callback thread: 0x%lx\n", pthread_self())
    );
    if (MR_thread_engine_base == NULL) return; 
    buffer = MR_thread_engine_base->MR_eng_ts_buffer;
    if (buffer == NULL) {
        /* GC might be running before we're done setting up */
        return; 
    }

    if (MR_US_TRY_LOCK(&(buffer->MR_tsbuffer_lock))) {
        context = MR_thread_engine_base->MR_eng_this_context;
        if (!buffer->MR_tsbuffer_ctxt_is_stopped && context) {
            MR_threadscope_post_stop_context_locked(buffer, context, 
                MR_TS_STOP_REASON_YIELDING);
        }
        MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
    }
}

static void 
resume_thread_gc_callback(void) 
{
    struct MR_threadscope_event_buffer  *buffer;
    MR_Context                          *context;

    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In gc resume thread callback thread: 0x%lx\n", pthread_self());
    );
    if (MR_thread_engine_base == NULL) return; 
    buffer = MR_thread_engine_base->MR_eng_ts_buffer;
    if (buffer == NULL) {
        /* GC might be running before we're done setting up */
        return; 
    }
   
    if (MR_US_TRY_LOCK(&(buffer->MR_tsbuffer_lock))) {
        context = MR_thread_engine_base->MR_eng_this_context;
        if (!buffer->MR_tsbuffer_ctxt_is_stopped && context) {
            MR_threadscope_post_run_context_locked(buffer, context);
        }
        MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
    }
}

/***************************************************************************/

static MR_uint_least64_t
get_current_time_nanosecs(void)
{
    MR_uint_least64_t   current_tsc;
    MercuryEngine       *eng = MR_thread_engine_base;

    current_tsc = MR_read_cpu_tsc();
    return (current_tsc + eng->MR_eng_cpu_clock_ticks_offset) / 
        (MR_cpu_cycles_per_sec / 1000000000);
}

/***************************************************************************/

#endif /* MR_THREADSCOPE */

/* forward decls to suppress gcc warnings */
void mercury_sys_init_threadscope_init(void);
void mercury_sys_init_threadscope_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_threadscope_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_threadscope_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
/* XXX: What does this do?  Why do other modules have a call like this.
    threadscope_module();
*/
#endif
}

void mercury_sys_init_threadscope_init_type_tables(void)
{
    /* no types to register */
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_threadscope_write_out_proc_statics(FILE *fp)
{
    /* no proc_statics to write out */
}
#endif
