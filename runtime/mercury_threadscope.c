// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2008-2009 The GHC Team.
// Copyright (C) 2009-2011 The University of Melbourne.
// Copyright (C) 2012, 2014-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// Event log format
//
// The log format is designed to be extensible: old tools should be able
// to parse (but not necessarily understand) new versions of the format,
// and new tools will be able to understand old log files.
//
// Each event has a specific format. If you add new events, give them
// new numbers: we never re-use old event numbers.
//
// - The format is endian-independent: all values are represented in
//    bigendian order.
//
// - The format is extensible:
//
//    - The header describes each event type and its length. Tools that
//      don't recognise a particular event type can skip those events.
//
//    - There is room for extra information in the event type
//      specification, which can be ignored by older tools.
//
//    - Events can have extra information added, but existing fields
//      cannot be changed. Tools should ignore extra fields at the
//      end of the event record.
//
//    - Old event type ids are never re-used; just take a new identifier.
//
//
// The format
// ----------
//
// log : EVENT_HEADER_BEGIN
//       EventType*
//       EVENT_HEADER_END
//       EVENT_DATA_BEGIN
//       Event*
//       EVENT_DATA_END
//
// EventType :
//       EVENT_ET_BEGIN
//       Word16         -- unique identifier for this event
//       Int16          -- >=0  size of the event in bytes (minus the header)
//                      -- -1   variable size
//       Word32         -- length of the next field in bytes
//       Word8*         -- string describing the event
//       Word32         -- length of the next field in bytes
//       EventTypeExt*  -- extensions
//       EVENT_ET_END
//
// Event :
//       Word16         -- event_type
//       Word64         -- time (nanosecs)
//       [Word16]       -- length of the rest (for variable-sized events only)
//       ... extra event-specific info ...
//
// EventTypeExt :
//       Word16         -- unique identifier for this extension type.
//       Word16         -- size of the payload in bytes.
//       Word8          -- payload bytes, their meaning depends upon the type.
//
// EVENT_EXT_TYPE_EXTENSION
//  This event extends another event also defined in this file, the payload of
//  this extension is:
//       Word16         -- unique identifier of the event being extended
//
// All values are packed, no attempt is made to align them.
//
// New events must be registered with GHC. These are kept in the GHC-events
// package.
//

#include "mercury_imp.h"
#include "mercury_threadscope.h"
#include "mercury_atomic_ops.h"

#include <stdio.h>
#include <string.h>

#ifdef MR_THREADSCOPE

////////////////////////////////////////////////////////////////////////////

// Markers for begin/end of the Header.

#define MR_TS_EVENT_HEADER_BEGIN    0x68647262 // 'h' 'd' 'r' 'b'
#define MR_TS_EVENT_HEADER_END      0x68647265 // 'h' 'd' 'r' 'e'

#define MR_TS_EVENT_DATA_BEGIN      0x64617462 // 'd' 'a' 't' 'b'
#define MR_TS_EVENT_DATA_END        0xffff

// Markers for begin/end of the list of Event Types in the Header.
// Header, Event Type, Begin = hetb
// Header, Event Type, End = hete

#define MR_TS_EVENT_HET_BEGIN       0x68657462 // 'h' 'e' 't' 'b'
#define MR_TS_EVENT_HET_END         0x68657465 // 'h' 'e' 't' 'e'

// Markers for the beginning and end of individual event types.

#define MR_TS_EVENT_ET_BEGIN        0x65746200 // 'e' 't' 'b' 0
#define MR_TS_EVENT_ET_END          0x65746500 // 'e' 't' 'e' 0

// The threadscope events.

#define MR_TS_EVENT_CREATE_THREAD        0 // (thread)
#define MR_TS_EVENT_RUN_THREAD           1 // (thread)
#define MR_TS_EVENT_STOP_THREAD          2 // (thread, status)
#define MR_TS_EVENT_THREAD_RUNNABLE      3 // (thread)
#define MR_TS_EVENT_MIGRATE_THREAD       4 // (thread, new_cap)
#define MR_TS_EVENT_SHUTDOWN             7 // ()
#define MR_TS_EVENT_THREAD_WAKEUP        8 // (thread, other_cap)
#define MR_TS_EVENT_GC_START             9 // ()
#define MR_TS_EVENT_GC_END              10 // ()
#define MR_TS_EVENT_REQUEST_SEQ_GC      11 // ()
#define MR_TS_EVENT_REQUEST_PAR_GC      12 // ()
#define MR_TS_EVENT_CREATE_SPARK_THREAD 15 // (spark_thread)
#define MR_TS_EVENT_LOG_MSG             16 // (message ...)
#define MR_TS_EVENT_STARTUP             17 // (num_capabilities)
#define MR_TS_EVENT_BLOCK_MARKER        18 // (size, end_time, capability)
#define MR_TS_EVENT_USER_MSG            19 // (message ...)
#define MR_TS_EVENT_GC_IDLE             20 // ()
#define MR_TS_EVENT_GC_WORK             21 // ()
#define MR_TS_EVENT_GC_DONE             22 // ()

// 23, 24 used by eden

// Capsets or capability sets are groups of engines with some association,
// for instance a group of threads in a process.

#define MR_TS_EVENT_CAPSET_CREATE       25 // (capset, capset_type)
#define MR_TS_EVENT_CAPSET_DELETE       26 // (capset)
#define MR_TS_EVENT_CAPSET_ASSIGN_CAP   27 // (capset, cap)
#define MR_TS_EVENT_CAPSET_REMOVE_CAP   28 // (capset, cap)
// the RTS identifier is in the form of "GHC-version rts_way"
#define MR_TS_EVENT_RTS_IDENTIFIER      29 // (capset, name_version_string)
// the vectors in two these events are null separated strings
#define MR_TS_EVENT_PROGRAM_ARGS        30 // (capset, commandline_vector)
#define MR_TS_EVENT_PROGRAM_ENV         31 // (capset, environment_vector)

#define MR_TS_EVENT_OSPROCESS_PID       32 // (capset, pid)
#define MR_TS_EVENT_OSPROCESS_PPID      33 // (capset, parent_pid)
#define MR_TS_EVENT_SPARK_COUNTERS      34 // (crt,dud,ovf,cnv,fiz,gcd,rem)
#define MR_TS_EVENT_SPARK_CREATE        35 // ()
#define MR_TS_EVENT_SPARK_DUD           36 // ()
#define MR_TS_EVENT_SPARK_OVERFLOW      37 // ()
#define MR_TS_EVENT_SPARK_RUN           38 // ()
#define MR_TS_EVENT_SPARK_STEAL         39 // (victim_cap)
#define MR_TS_EVENT_SPARK_FIZZLE        40 // ()
#define MR_TS_EVENT_SPARK_GC            41 // ()
#define MR_TS_EVENT_INTERN_STRING       42 // (string, id)
#define MR_TS_EVENT_WALL_CLOCK_TIME     43 // (capset, unix_epoch_seconds,
                                           // nanoseconds)
#define MR_TS_EVENT_THREAD_LABEL        44 // (thread, name_string)
#define MR_TS_EVENT_CAP_CREATE          45 // (cap)
#define MR_TS_EVENT_CAP_DELETE          46 // (cap)
#define MR_TS_EVENT_CAP_DISABLE         47 // (cap)
#define MR_TS_EVENT_CAP_ENABLE          48 // (cap)
#define MR_TS_EVENT_HEAP_ALLOCATED      49 // (heap_capset, alloc_bytes)
#define MR_TS_EVENT_HEAP_SIZE           50 // (heap_capset, size_bytes)
#define MR_TS_EVENT_HEAP_LIVE           51 // (heap_capset, live_bytes)
#define MR_TS_EVENT_HEAP_INFO_GHC       52 // (heap_capset, n_generations,
                                           // max_heap_size, alloc_area_size,
                                           // mblock_size, block_size)
#define MR_TS_EVENT_GC_STATS_GHC        53 // (heap_capset, generation,
                                           // copied_bytes, slop_bytes,
                                           // frag_bytes, par_n_threads,
                                           // par_max_copied, par_tot_copied)
#define MR_TS_EVENT_GC_GLOBAL_SYNC      54 // ()

#define MR_TS_NUM_EVENT_TAGS            55

#define MR_TS_MER_EVENT_START           100

#define MR_TS_MER_EVENT_START_PAR_CONJ      100 // (int id, memo'd string id)
#define MR_TS_MER_EVENT_END_PAR_CONJ        101 // (int id)
#define MR_TS_MER_EVENT_END_PAR_CONJUNCT    102 // (int id)

// Creating sparks is not specifically mercury, but conjunct IDs are.
// If other systems wish to use this event, they can move it
// to the main events section.

#define MR_TS_MER_EVENT_SPARK_CREATE        103 // (int id, spark id)

#define MR_TS_MER_EVENT_FUT_CREATE          104 // (fut id, memo'd name id)
#define MR_TS_MER_EVENT_FUT_WAIT_NOSUSPEND  105 // (fut id)
#define MR_TS_MER_EVENT_FUT_WAIT_SUSPENDED  106 // (fut id)
#define MR_TS_MER_EVENT_FUT_SIGNAL          107 // (fut id)
#define MR_TS_MER_EVENT_LOOKING_FOR_GLOBAL_CONTEXT \
                                            108 // ()
#define MR_TS_MER_EVENT_WORK_STEALING       109 // ()
#define MR_TS_MER_EVENT_RELEASE_CONTEXT     110 // (context id)
#define MR_TS_MER_EVENT_ENGINE_SLEEPING     111 // ()
#define MR_TS_MER_EVENT_LOOKING_FOR_LOCAL_SPARK \
                                            112 // ()
#define MR_TS_MER_EVENT_CALLING_MAIN        113 // ()
#define MR_TS_MER_EVENT_SPARK_RUN           114 // (spark id)
#define MR_TS_MER_EVENT_SPARK_STEAL         115 // (victim cap, spark id)
#define MR_TS_MER_EVENT_REUSE_THREAD        116 // (context id, old context id)
#define MR_TS_NUM_MER_EVENTS                 17

#if 0  // DEPRECATED EVENTS:
#define EVENT_CREATE_SPARK        13 // (cap, thread)
#define EVENT_SPARK_TO_THREAD     14 // (cap, thread, spark_thread)
#define MR_TS_EVENT_RUN_SPARK      5 // (thread, spark_id)
#define MR_TS_EVENT_STEAL_SPARK    6 // (thread, victim_cap, spark_id)
#endif

// Engine set type values for EVENT_CAPSET_CREATE.

#define MR_TS_ENGSET_TYPE_CUSTOM      1 // reserved for end-user applications
#define MR_TS_ENGSET_TYPE_OSPROCESS   2 // engines belong to same OS process
#define MR_TS_ENGSET_TYPE_CLOCKDOMAIN 3 // engines share a local clock/time

// Event extension types.

#define MR_EXT_TYPE_EXTENSION         1 // This event extends another event

// GHC uses 2MB per buffer. Note that the minimum buffer size is the size of
// the largest message plus the size of the block marker message, however it is
// _sensible_ for the buffer to be much larger so that we make system calls
// less often.

#define MR_TS_BUFFERSIZE (2*1024*1024)
#define MR_TS_FILENAME_FORMAT ("%s.eventlog")
#define MR_TSC_SYNC_NUM_ROUNDS (10)
#define MR_TSC_SYNC_NUM_BEST_ROUNDS (3)

// Uncomment this to enable some debugging code.
// #define MR_DEBUG_THREADSCOPE 1

#if MR_DEBUG_THREADSCOPE
#define MR_DO_THREADSCOPE_DEBUG(x) do { x; } while (0)
#else
#define MR_DO_THREADSCOPE_DEBUG(x)
#endif

////////////////////////////////////////////////////////////////////////////

struct MR_threadscope_event_buffer {
    unsigned char       MR_tsbuffer_data[MR_TS_BUFFERSIZE];

    // The current writing position in the buffer.
    MR_Unsigned         MR_tsbuffer_pos;

    // The position of the start of the most recent block.
    MR_Integer          MR_tsbuffer_block_open_pos;

    // True if the engine's current context is stopped, and therefore
    // stop and start events should not be posted from the GC callback
    // procedures.

    MR_bool             MR_tsbuffer_ctxt_is_stopped;

    // A cheap userspace lock to make buffers reentrant.
    volatile MR_Us_Lock MR_tsbuffer_lock;
};

// We define some types and functions to write them.
// These types are set carefully to match the ones that GHC uses.

typedef MR_uint_least16_t   EventType;
typedef MR_uint_least64_t   Time;
typedef MR_int_least64_t    Timedelta;

// The difference between two positions in the eventlog file measured in bytes.

typedef MR_uint_least32_t   EventlogOffset;

// A descriptor used when writing the header of threadscope files.
// The fields are:
//
// edt_event_type       The type of this event.
//
// edt_description      A string description of this event.
//
// edt_size             The event's size or -1 for variable length.
//
// edt_extends_event    The event that this event extends, or
//                      0xFFFF if this is a base event.

typedef struct {
    EventType           etd_event_type;
    const char          *etd_description;
    MR_int_least16_t    etd_size;
    EventType           edt_extends_event;
} EventTypeDesc;

////////////////////////////////////////////////////////////////////////////

#define SZ_EVENT_TYPE           2
#define SZ_CAPSET_ID            4
#define SZ_CAPSET_TYPE          2
#define SZ_CONTEXT_ID           4
#define SZ_CONTEXT_STOP_REASON  2
#define SZ_DYN_CONJ_ID          8
#define SZ_ENGINELOG_OFFSET     4
#define SZ_ENGINE_ID            2
#define SZ_PID                  4
#define SZ_SPARK_ID             4
#define SZ_STRING_ID            4
#define SZ_STATIC_CONJ_ID       (SZ_STRING_ID)
#define SZ_VAR_NAME_ID          (SZ_STRING_ID)
#define SZ_TIME                 8
#define SZ_FUTURE_ID            8

static EventTypeDesc event_type_descs[] = {
    {
        // The startup event informs threadscope of the number of engines
        // we are using. It should be given outside of a block.

        MR_TS_EVENT_STARTUP,
        "Startup (num_engines)",
        SZ_ENGINE_ID,
        0xFFFF
    },
    {
        // The last event in the log. It should be given outside of a block.

        MR_TS_EVENT_SHUTDOWN,
        "Shutdown",
        0,
        0xFFFF
    },
    {
        // A block of events belonging to the named engine follows.
        // The length of this block is given including the block message
        // itself, the time that this block finishes is also given.
        // Blocks _must not_ exist within other blocks.

        MR_TS_EVENT_BLOCK_MARKER,
        "A block of events generated by a specific engine follows",
        SZ_ENGINELOG_OFFSET + SZ_TIME + SZ_ENGINE_ID,
        0xFFFF
    },
    {
        // Called when a context is created or re-used.

        MR_TS_EVENT_CREATE_THREAD,
        "A context is created or re-used",
        SZ_CONTEXT_ID,
        0xFFFF
    },
    {
        // Called from MR_schedule_context().

        MR_TS_EVENT_THREAD_RUNNABLE,
        "The context is being placed on the run queue",
        SZ_CONTEXT_ID,
        0xFFFF
    },
    {
        // The engine has taken a spark from it's local stack and will run it.

        MR_TS_EVENT_SPARK_RUN,
        "Run a spark from the local stack",
        0,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_SPARK_RUN,
        "Run a spark from the local stack, the spark is identified by an id",
        SZ_SPARK_ID,
        MR_TS_EVENT_SPARK_RUN
    },
    {
        // The named context has begun executing a spark from another
        // engine's stack.

        MR_TS_EVENT_SPARK_STEAL,
        "Run a spark stolen from another engine",
        SZ_ENGINE_ID,
        0XFFFF
    },
    {
        MR_TS_MER_EVENT_SPARK_STEAL,
        "Run a spark stolen from another engine, "
            "the spark is identified by an id",
        SZ_ENGINE_ID + SZ_SPARK_ID,
        MR_TS_EVENT_SPARK_STEAL
    },
    {
        // The named context has begun executing on the engine
        // named by the current block.

        MR_TS_EVENT_RUN_THREAD,
        "Run context",
        SZ_CONTEXT_ID,
        0xFFFF
    },
    {
        // The named context has stopped executing on the engine named by
        // the current block. The reason why the context stopped is given.

        MR_TS_EVENT_STOP_THREAD,
        "Context stopped",
        SZ_CONTEXT_ID + SZ_CONTEXT_STOP_REASON,
        0xFFFF
    },
    {
        // This event is posted when a context is created for a spark.

        MR_TS_EVENT_CREATE_SPARK_THREAD,
        "Create a context for executing a spark",
        SZ_CONTEXT_ID,
        0xFFFF
    },
    {
        MR_TS_EVENT_LOG_MSG,
        "A user-provided log message",
        -1, // Variable length
        0xFFFF
    },
    {
        // Start a garbage collection run.

        MR_TS_EVENT_GC_START,
        "Start GC",
        0,
        0xFFFF
    },
    {
        // Stop a garbage collection run.

        MR_TS_EVENT_GC_END,
        "Stop GC",
        0,
        0xFFFF
    },
    {
        // The runtime system registers a string and an ID for it
        // so that the ID represents the string in future messages.

        MR_TS_EVENT_INTERN_STRING,
        "Register an id->string mapping",
        -1,
        0xFFFF
    },
    {
        MR_TS_EVENT_CAPSET_CREATE,
        "Create an engine set",
        SZ_CAPSET_ID + SZ_CAPSET_TYPE,
        0xFFFF
    },
    {
        MR_TS_EVENT_CAPSET_DELETE,
        "Detete an engine set",
        SZ_CAPSET_ID,
        0xFFFF
    },
    {
        MR_TS_EVENT_CAPSET_ASSIGN_CAP,
        "Add an engine to an engine set",
        SZ_CAPSET_ID + SZ_ENGINE_ID,
        0xFFFF
    },
    {
        MR_TS_EVENT_CAPSET_REMOVE_CAP,
        "Add an engine to an engine set",
        SZ_CAPSET_ID + SZ_ENGINE_ID,
        0xFFFF
    },
    {
        MR_TS_EVENT_RTS_IDENTIFIER,
        "The type of the runtime system for this capset",
        -1,
        0xFFFF
    },
    {
        MR_TS_EVENT_PROGRAM_ARGS,
        "The command line arguments of this process",
        -1,
        0xFFFF
    },
    {
        MR_TS_EVENT_PROGRAM_ENV,
        "The environment variables this process inherited",
        -1,
        0xFFFF
    },
    {
        MR_TS_EVENT_OSPROCESS_PID,
        "The pid of this process",
        SZ_PID,
        0xFFFF
    },
    {
        MR_TS_EVENT_OSPROCESS_PPID,
        "The parent pid of this process",
        SZ_PID,
        0xFFFF
    },
    {
        MR_TS_EVENT_SPARK_CREATE,
        "A spark is being created",
        0,
        0xFFFF
    },
    // We don't use events 43--53.

    {
        MR_TS_EVENT_GC_GLOBAL_SYNC,
        // If using parallel marking this also means that marker threads are
        // ready. This doesn't apply to Mercury as Boehm uses separate
        // threads

        "The world has stopped and GC may begin",
        0,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_SPARK_CREATE,
        "A spark is being created with attributes for Mercury",
        SZ_DYN_CONJ_ID + SZ_SPARK_ID,
        MR_TS_EVENT_SPARK_CREATE
    },
    {
        MR_TS_MER_EVENT_START_PAR_CONJ,
        "Start a parallel conjunction (dyn id, static id)",
        SZ_DYN_CONJ_ID + SZ_STATIC_CONJ_ID,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_END_PAR_CONJ,
        "End a parallel conjunction (dyn id)",
        SZ_DYN_CONJ_ID,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_END_PAR_CONJUNCT,
        "End a parallel conjunct (dyn id)",
        SZ_DYN_CONJ_ID,
        0xFFFF
    },
    {
        // The dynamic conjunction id can be inferred from context;
        // it is the next conjunction started after this event.

        MR_TS_MER_EVENT_FUT_CREATE,
        "Create a future (future id)",
        SZ_FUTURE_ID + SZ_VAR_NAME_ID,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_FUT_WAIT_NOSUSPEND,
        "Wait on a future without suspending (future id)",
        SZ_FUTURE_ID,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_FUT_WAIT_SUSPENDED,
        "Wait on a future by suspending this thread (future id)",
        SZ_FUTURE_ID,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_FUT_SIGNAL,
        "Signal a future (future id)",
        SZ_FUTURE_ID,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_LOOKING_FOR_GLOBAL_CONTEXT,
        "Engine begins looking for a context to execute",
        0,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_LOOKING_FOR_LOCAL_SPARK,
        "Engine begins looking for a local spark to execute",
        0,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_WORK_STEALING,
        "Engine begins attempt to steal work",
        0,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_RELEASE_CONTEXT,
        "Release this context to the free context pool",
        SZ_CONTEXT_ID,
        0xFFFF
    },
    {
        MR_TS_MER_EVENT_ENGINE_SLEEPING,
        "This engine is going to sleep",
        0,
        0xFFFF
    },
    {
        // The runtime system is about to call main/2.
        // This message has no parameters.

        MR_TS_MER_EVENT_CALLING_MAIN,
        "About to call main/2",
        0,
        0xFFFF
    },
    {
        // The runtime system is re-using a previous context and
        // re-assigning its ID.

        MR_TS_MER_EVENT_REUSE_THREAD,
        "Reusing a previously allocated thread",
        SZ_CONTEXT_ID + SZ_CONTEXT_ID,
        MR_TS_EVENT_CREATE_THREAD
    },
    {
        // Mark the end of this array.
        MR_TS_NUM_EVENT_TAGS,
        NULL,
        0,
        0xFFFF
    }
};

// These tables are filled in when the header of the log file is written.
// While they can be inferred from the event_type_desc structure,
// they allow for constant time lookup.

static MR_int_least16_t event_type_sizes[MR_TS_NUM_EVENT_TAGS];
static MR_int_least16_t event_type_sizes_mercury[MR_TS_NUM_MER_EVENTS];

static FILE* MR_threadscope_output_file = NULL;
static char* MR_threadscope_output_filename;

// The TSC value recorded when the primordial thread called
// MR_setup_threadscope(), this is used retroactively to initialise the
// MR_eng_cpu_clock_ticks_offset field in the engine structure once it is
// created.

static MR_uint_least64_t MR_primordial_first_tsc;

static Timedelta        MR_global_offset;

static struct MR_threadscope_event_buffer global_buffer;

// Alternatively, we use gettimeofday for measuring time.

MR_bool                 MR_threadscope_use_tsc = MR_FALSE;
static Timedelta        MR_gettimeofday_offset;

// An ID that may be allocated to the next string to be registered.

static MR_TS_StringId   MR_next_string_id = 0;
static MR_EngSetId      next_engset_id = 0;

static MR_EngSetId      process_engset_id;

////////////////////////////////////////////////////////////////////////////

static MR_EngSetId
get_next_engset_id(void)
{
    // This is a separate function as I may have to add locking or atomic ops
    // later.

    return next_engset_id++;
}

////////////////////////////////////////////////////////////////////////////

MR_STATIC_INLINE MR_int_least16_t
event_type_size(EventType event_type)
{
    MR_int_least16_t size;

    if (event_type < MR_TS_NUM_EVENT_TAGS) {
        size = event_type_sizes[event_type];
    } else if ((event_type < (MR_TS_MER_EVENT_START + MR_TS_NUM_MER_EVENTS))
            && (event_type >= MR_TS_MER_EVENT_START)) {
        size = event_type_sizes_mercury[event_type - MR_TS_MER_EVENT_START];
    } else {
        fprintf(stderr, "Unknown event type %d\n", event_type);
        abort();
    }

    return size;
}

// Is there enough room in the current engine's buffer
// for this statically sized event _and_ for the block marker event.

MR_STATIC_INLINE MR_bool
enough_room_for_event(struct MR_threadscope_event_buffer *buffer,
    EventType event_type)
{
    int needed =
        buffer->MR_tsbuffer_pos +
        event_type_size(event_type) +
        event_type_size(MR_TS_EVENT_BLOCK_MARKER) +
        ((2 + 8) * 2); // (EventType, Time) * 2
    return needed < MR_TS_BUFFERSIZE;
}

MR_STATIC_INLINE MR_bool
enough_room_for_variable_size_event(struct MR_threadscope_event_buffer *buffer,
    MR_Unsigned length)
{
    int needed =
        buffer->MR_tsbuffer_pos +
        length +
        event_type_size(MR_TS_EVENT_BLOCK_MARKER) +
        ((2 + 8) * 2); // (EventType, Time) * 2
    return needed < MR_TS_BUFFERSIZE;
}

// Is a block currently open?

MR_STATIC_INLINE MR_bool
block_is_open(struct MR_threadscope_event_buffer *buffer)
{
    return !(buffer->MR_tsbuffer_block_open_pos == -1);
}

// Put words into the current engine's buffer in big endian order.

MR_STATIC_INLINE void
put_byte(struct MR_threadscope_event_buffer *buffer, int byte)
{
    buffer->MR_tsbuffer_data[buffer->MR_tsbuffer_pos++] = byte;
}

MR_STATIC_INLINE void
put_be_int16(struct MR_threadscope_event_buffer *buffer, MR_int_least16_t word)
{
    put_byte(buffer, (word >> 8) & 0xFF);
    put_byte(buffer, word & 0xFF);
}

MR_STATIC_INLINE void
put_be_uint16(struct MR_threadscope_event_buffer *buffer,
    MR_uint_least16_t word)
{
    put_byte(buffer, (word >> 8) & 0xFF);
    put_byte(buffer, word & 0xFF);
}

MR_STATIC_INLINE void
put_be_uint32(struct MR_threadscope_event_buffer *buffer,
    MR_uint_least32_t word)
{
    put_be_uint16(buffer, (word >> 16) & 0xFFFF);
    put_be_uint16(buffer, word & 0xFFFF);
}

MR_STATIC_INLINE void
put_be_uint64(struct MR_threadscope_event_buffer *buffer,
    MR_uint_least64_t word)
{
    put_be_uint32(buffer, (word >> 32) & 0xFFFFFFFF);
    put_be_uint32(buffer, word & 0xFFFFFFFF);
}

MR_STATIC_INLINE void
put_raw_string(struct MR_threadscope_event_buffer *buffer,
    const char *string, unsigned len)
{
    unsigned i;
    for (i = 0; i < len; i++) {
        put_byte(buffer, string[i]);
    }
}

// Put a string in the given buffer. The string will be preceded
// by a 16 bit integer giving the string's length.

MR_STATIC_INLINE void
put_string_size16(struct MR_threadscope_event_buffer *buffer,
    const char *string)
{
    unsigned i, len;

    len = strlen(string);
    put_be_uint16(buffer, len);
    put_raw_string(buffer, string, len);
}

// Put a string in the given buffer. The string will be preceded
// by a 32 bit integer giving the string's length.

MR_STATIC_INLINE void
put_string_size32(struct MR_threadscope_event_buffer *buffer,
    const char *string)
{
    unsigned i, len;

    len = strlen(string);
    put_be_uint32(buffer, len);
    put_raw_string(buffer, string, len);
}

MR_STATIC_INLINE void
put_timestamp(struct MR_threadscope_event_buffer *buffer, Time timestamp)
{
    put_be_uint64(buffer, timestamp);
}

MR_STATIC_INLINE void
put_eventlog_offset(struct MR_threadscope_event_buffer *buffer,
    EventlogOffset offset)
{
    put_be_uint32(buffer, offset);
}

MR_STATIC_INLINE void
put_event_header(struct MR_threadscope_event_buffer *buffer,
    EventType event_type, Time timestamp)
{
    put_be_uint16(buffer, event_type);
    put_timestamp(buffer, timestamp);
}

MR_STATIC_INLINE void
put_engine_id(struct MR_threadscope_event_buffer *buffer,
    MR_EngineId engine_num)
{
    put_be_uint16(buffer, engine_num);
}

MR_STATIC_INLINE void
put_context_id(struct MR_threadscope_event_buffer *buffer,
    MR_ContextId context_id)
{
    put_be_uint32(buffer, context_id);
}

MR_STATIC_INLINE void
put_stop_reason(struct MR_threadscope_event_buffer *buffer,
    MR_ContextStopReason reason)
{
    put_be_uint16(buffer, reason);
}

MR_STATIC_INLINE void
put_string_id(struct MR_threadscope_event_buffer *buffer, MR_TS_StringId id)
{
    put_be_uint32(buffer, id);
}

MR_STATIC_INLINE void
put_par_conj_dynamic_id(struct MR_threadscope_event_buffer *buffer,
    MR_Word* id)
{
    put_be_uint64(buffer, (MR_Word)id);
}

MR_STATIC_INLINE void
put_spark_id(struct MR_threadscope_event_buffer *buffer, MR_SparkId spark_id)
{
    put_be_uint32(buffer, spark_id);
}

MR_STATIC_INLINE void
put_engset_id(struct MR_threadscope_event_buffer *buffer,
    MR_EngSetId engset_id)
{
    put_be_uint32(buffer, engset_id);
}

MR_STATIC_INLINE void
put_engset_type(struct MR_threadscope_event_buffer *buffer, MR_EngSetType type)
{
    put_be_uint16(buffer, type);
}

MR_STATIC_INLINE void
put_future_id(struct MR_threadscope_event_buffer *buffer, MR_Future* id)
{
    put_be_uint64(buffer, (MR_Word)id);
}

////////////////////////////////////////////////////////////////////////////

static struct MR_threadscope_event_buffer* MR_create_event_buffer(void);

// The prelude is everything up to and including the 'DATA_BEGIN' marker.

static void MR_open_output_file_and_write_prelude(void);

static void MR_close_output_file(void);

static void put_event_type(struct MR_threadscope_event_buffer *buffer,
                EventTypeDesc *event_type);

static MR_bool flush_event_buffer(struct MR_threadscope_event_buffer *buffer);

static void maybe_close_block(struct MR_threadscope_event_buffer *buffer);

static void open_block(struct MR_threadscope_event_buffer *buffer,
                MR_Unsigned eng_id);

////////////////////////////////////////////////////////////////////////////

static MR_TS_StringId MR_threadscope_register_string(const char *string);

// These four events are used to create and manage engine sets.
// Haskell calls these cap sets.
//
// The first two work on the global event buffer and are not thread safe.

static void MR_threadscope_post_create_engset(MR_EngSetId id,
                MR_EngSetType type);

static void MR_threadscope_post_destroy_engset(MR_EngSetId id);

static void MR_threadscope_post_engset_add(
                struct MR_threadscope_event_buffer *buffer,
                MR_EngSetId id, MR_EngineId eng);

static void MR_threadscope_post_engset_remove(MR_EngSetId id, MR_EngineId eng);

// Post the name and version of the runtime system to the log file.
//
// Note that this is the name of the implementation (mmc),
// not the name of the language (mercury).
//
// The name and version are separated by a '-'.

static void MR_threadscope_post_runtime_identifier(MR_EngSetId id,
                const char *ident);

////////////////////////////////////////////////////////////////////////////

static void start_gc_callback(void);
static void stop_gc_callback(void);
static void pause_thread_gc_callback(void);
static void resume_thread_gc_callback(void);

////////////////////////////////////////////////////////////////////////////

static Time get_current_time_nanosecs(void);
static Time gettimeofday_nsecs(void);

////////////////////////////////////////////////////////////////////////////

void
MR_setup_threadscope(void)
{
    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In setup threadscope thread: 0x%lx\n", pthread_self())
    );

    if (!MR_tsc_is_sensible()) {
        MR_threadscope_use_tsc = MR_FALSE;
    }

    if (MR_threadscope_use_tsc) {
        // This value is used later when setting up the primordial engine.
        MR_primordial_first_tsc = MR_read_cpu_tsc();

        // These variables are used for TSC synchronization which is not used.
        // See below.
        pthread_mutex_init(&MR_tsc_sync_slave_lock, MR_MUTEX_ATTR);
        MR_US_COND_CLEAR(&MR_tsc_sync_slave_entry_cond);
        MR_US_COND_CLEAR(&MR_tsc_sync_master_entry_cond);
        MR_US_COND_CLEAR(&MR_tsc_sync_t0);
        MR_US_COND_CLEAR(&MR_tsc_sync_t1);

    } else {
        MR_gettimeofday_offset = -1 * gettimeofday_nsecs();
    }

    // Configure Boehm.
#ifdef MR_BOEHM_GC
    GC_mercury_callback_start_collect = start_gc_callback;
    GC_mercury_callback_stop_collect = stop_gc_callback;
    GC_mercury_callback_pause_thread = pause_thread_gc_callback;
    GC_mercury_callback_resume_thread = resume_thread_gc_callback;
#endif

    // Clear the global buffer and setup the file.
    global_buffer.MR_tsbuffer_pos = 0;
    global_buffer.MR_tsbuffer_block_open_pos = -1;
    global_buffer.MR_tsbuffer_lock = MR_US_LOCK_INITIAL_VALUE;
    MR_open_output_file_and_write_prelude();

    // Post the initial events to the buffer.
    process_engset_id = get_next_engset_id();
    MR_threadscope_post_create_engset(process_engset_id,
        MR_TS_ENGSET_TYPE_OSPROCESS);
    MR_threadscope_post_runtime_identifier(process_engset_id,
        "mmc-" MR_VERSION);

    // Put the startup event in the buffer.
    put_event_header(&global_buffer, MR_TS_EVENT_STARTUP, 0);
    put_engine_id(&global_buffer, (MR_EngineId)MR_num_ws_engines);

    flush_event_buffer(&global_buffer);
}

void
MR_finalize_threadscope(void)
{
    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In finalize threadscope thread: 0x%lx\n",
            pthread_self())
    );

    MR_threadscope_post_destroy_engset(process_engset_id);

    flush_event_buffer(&global_buffer);
    MR_close_output_file();
}

void
MR_threadscope_setup_engine(MercuryEngine *eng)
{
    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In threadscope setup engine thread: 0x%lx\n",
            pthread_self())
    );
    eng->MR_eng_next_spark_id = 0;

    if (MR_threadscope_use_tsc) {
        if (eng->MR_eng_id == 0) {
            MR_global_offset = -MR_primordial_first_tsc;
        }
        eng->MR_eng_cpu_clock_ticks_offset = MR_global_offset;
    }

    eng->MR_eng_ts_buffer = MR_create_event_buffer();

    MR_threadscope_post_engset_add(eng->MR_eng_ts_buffer, process_engset_id,
        eng->MR_eng_id);
    // Flush the buffer to ensure the message above (which lacks a timestamp)
    // appears in a sensible place in the buffer.
    flush_event_buffer(eng->MR_eng_ts_buffer);
}

void
MR_threadscope_finalize_engine(MercuryEngine *eng)
{
    struct MR_threadscope_event_buffer *buffer = eng->MR_eng_ts_buffer;

    MR_DO_THREADSCOPE_DEBUG(
        fprintf(stderr, "In threadscope finalize engine thread: 0x%lx\n",
            pthread_self())
    );

    MR_threadscope_post_engset_remove(process_engset_id, eng->MR_eng_id);

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
// It looks like we don't need this on modern CPUs including multi-socket
// systems (goliath). If we find systems where this is needed, we can
// enable it via a runtime check.

// The synchronization of TSCs operates as follows:
// The master and slave enter their functions. Both threads spin until the
// other is ready (signaling the other before they begin spinning). Then for
// MR_TSC_SYNC_NUM_ROUNDS: The master spins waiting for the slave. The slave
// records it's current TSC, signals the master and spins waiting for a reply.
// The master upon hearing from the slave records it's TSC and then signals
// the slave. The slave can then compute the delay in this round. The slave
// takes the NR_TSC_SYNC_NUM_BEST_ROUNDS best delays (smallest) and computes
// the offset as the average between between the difference of the clocks based
// on Cristan's algorithm (1989).

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

static int compare_time_delay_offset_by_delay(const void *a, const void *b);

void
MR_threadscope_sync_tsc_master(void)
{
    unsigned i;

    // Wait for a slave to enter.
    MR_US_COND_SET(&MR_tsc_sync_master_entry_cond);
    MR_US_SPIN_COND(&MR_tsc_sync_slave_entry_cond);
    MR_US_COND_CLEAR(&MR_tsc_sync_slave_entry_cond);

    for (i = 0; i < MR_TSC_SYNC_NUM_ROUNDS; i++) {
        // Wait to receive the message from the slave at T0.
        MR_US_SPIN_COND(&MR_tsc_sync_t0);
        MR_US_COND_CLEAR(&MR_tsc_sync_t0);

        // Read our TSC and send the slave a message.
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

    // Only one slave may enter at a time.
    MR_LOCK(&MR_tsc_sync_slave_lock, "MR_threadscope_sync_tsc_slave");

    // Tell the master we are ready to begin, and wait for it to tell us
    // it is ready.

    MR_US_COND_SET(&MR_tsc_sync_slave_entry_cond);
    MR_US_SPIN_COND(&MR_tsc_sync_master_entry_cond);
    MR_US_COND_CLEAR(&MR_tsc_sync_master_entry_cond);

    for (i = 0; i < MR_TSC_SYNC_NUM_ROUNDS; i++) {
        Time    slave_tsc_at_t0;
        Time    slave_tsc_at_t2;

        // Get the current time and signal that we've done so (T=0).

        slave_tsc_at_t0 = MR_read_cpu_tsc();
        MR_US_COND_SET(&MR_tsc_sync_t0);

        // Wait for the master to reply, the master handles T=1,
        // here we proceed to T=2.

        MR_US_SPIN_COND(&MR_tsc_sync_t1);
        slave_tsc_at_t2 = MR_read_cpu_tsc();
        MR_US_COND_CLEAR(&MR_tsc_sync_t1);

        // Here are Cristian's formulas. Delay is the round trip time,
        // slave_tsc_at_t0 + delay/2 is the time on the slave's clock that the
        // master processed the slaves message and sent its own. This is
        // accurate if the communication delays in either direction are
        // uniform, that is communication latency is synchronous.

        delay_offset[i].delay = slave_tsc_at_t2 - slave_tsc_at_t0;
        delay_offset[i].offset = MR_tsc_sync_master_time -
            (slave_tsc_at_t0 + delay_offset[i].delay/2);
    }

    // By now the master thread will return,
    // and continue with its normal work.

    // We do this debugging output while holding the lock, so that the output
    // is reasonable.

    MR_DO_THREADSCOPE_DEBUG({
        fprintf(stderr, "TSC Synchronization for thread 0x%x\n",
            pthread_self());
        for (i = 0; i < MR_TSC_SYNC_NUM_ROUNDS; i++) {
            fprintf(stderr,
                "delay: %ld offset (local + global = total) %ld + %ld = %ld\n",
                delay_offset[i].delay, delay_offset[i].offset, MR_global_offset,
                delay_offset[i].offset + MR_global_offset);
        }
    });
    MR_UNLOCK(&MR_tsc_sync_slave_lock, "MR_threadscope_sync_tsc_slave");

    // Now to average the best offsets.
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

////////////////////////////////////////////////////////////////////////////

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

    put_event_header(buffer, MR_TS_EVENT_CREATE_THREAD,
        get_current_time_nanosecs());
    put_context_id(buffer, context->MR_ctxt_num_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_reuse_context(MR_Context *context, MR_Unsigned old_id)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));

    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_REUSE_THREAD)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_REUSE_THREAD,
        get_current_time_nanosecs());
    put_context_id(buffer, context->MR_ctxt_num_id);
    put_context_id(buffer, old_id);

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
MR_threadscope_post_release_context(MR_Context *context)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));

    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_RELEASE_CONTEXT)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_RELEASE_CONTEXT,
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

    put_event_header(buffer, MR_TS_EVENT_THREAD_RUNNABLE,
        get_current_time_nanosecs());
    put_context_id(buffer, context->MR_ctxt_num_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

static void
MR_threadscope_post_run_context_locked(
    struct MR_threadscope_event_buffer *buffer, MR_Context *context)
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
        if (buffer->MR_tsbuffer_ctxt_is_stopped) {
            MR_threadscope_post_run_context_locked(buffer, context);
            buffer->MR_tsbuffer_ctxt_is_stopped = MR_FALSE;
        }
        MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
    }
}

static void
MR_threadscope_post_stop_context_locked(
    struct MR_threadscope_event_buffer *buffer,
    MR_Context *context, MR_ContextStopReason reason)
{
    if (!enough_room_for_event(buffer, MR_TS_EVENT_STOP_THREAD)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_thread_engine_base->MR_eng_id);
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_thread_engine_base->MR_eng_id);
    }

    put_event_header(buffer, MR_TS_EVENT_STOP_THREAD,
        get_current_time_nanosecs());
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
    if (!buffer->MR_tsbuffer_ctxt_is_stopped) {
        MR_threadscope_post_stop_context_locked(buffer, context, reason);
        buffer->MR_tsbuffer_ctxt_is_stopped = MR_TRUE;
    }
    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_run_spark(MR_SparkId spark_id)
{
    struct MR_threadscope_event_buffer  *buffer;

    buffer = MR_thread_engine_base->MR_eng_ts_buffer;

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_SPARK_RUN)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_SPARK_RUN,
        get_current_time_nanosecs());
    put_spark_id(buffer, spark_id);
    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_steal_spark(MR_SparkId spark_id)
{
    struct MR_threadscope_event_buffer  *buffer;
    unsigned                            engine_id;

    buffer = MR_thread_engine_base->MR_eng_ts_buffer;

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_SPARK_STEAL)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_SPARK_STEAL,
        get_current_time_nanosecs());

    // The engine that created the spark (which may not be whom it was stolen
    // from if different work-stealing algorithms are implemented) can be
    // derived from the spark id.

    engine_id = (spark_id & 0xFF000000) >> 24;
    put_be_uint16(buffer, engine_id);
    put_spark_id(buffer, spark_id);
    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_sparking(MR_Word* dynamic_conj_id, MR_SparkId spark_id)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_SPARK_CREATE)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_SPARK_CREATE,
        get_current_time_nanosecs());
    put_par_conj_dynamic_id(buffer, dynamic_conj_id);
    put_spark_id(buffer, spark_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_calling_main(void)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_CALLING_MAIN)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_CALLING_MAIN,
        get_current_time_nanosecs());
    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_looking_for_global_context(void)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer,
            MR_TS_MER_EVENT_LOOKING_FOR_GLOBAL_CONTEXT))
    {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_LOOKING_FOR_GLOBAL_CONTEXT,
        get_current_time_nanosecs());
    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_looking_for_local_spark(void)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_LOOKING_FOR_LOCAL_SPARK)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_LOOKING_FOR_LOCAL_SPARK,
        get_current_time_nanosecs());
    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_work_stealing(void)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_WORK_STEALING)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_WORK_STEALING,
        get_current_time_nanosecs());
    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_start_par_conj(MR_Word* dynamic_id,
    MR_TS_StringId static_id)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_START_PAR_CONJ)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_START_PAR_CONJ,
        get_current_time_nanosecs());
    put_par_conj_dynamic_id(buffer, dynamic_id);
    put_string_id(buffer, static_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_end_par_conj(MR_Word *dynamic_id)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_END_PAR_CONJ)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_END_PAR_CONJ,
        get_current_time_nanosecs());
    put_par_conj_dynamic_id(buffer, dynamic_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_end_par_conjunct(MR_Word *dynamic_id)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_END_PAR_CONJUNCT)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_END_PAR_CONJUNCT,
        get_current_time_nanosecs());
    put_par_conj_dynamic_id(buffer, dynamic_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

// Register a string for use in future messages.

static MR_TS_StringId
MR_threadscope_register_string(const char *string)
{
    MR_TS_StringId id;
    unsigned length;

    length = strlen(string);

    // +2 for the event length.
    // +4 for the string id.
    if (!enough_room_for_variable_size_event(&global_buffer, strlen(string)
        + 2 + 4))
    {
        flush_event_buffer(&global_buffer);
    }

    put_event_header(&global_buffer, MR_TS_EVENT_INTERN_STRING, 0);
    id = MR_next_string_id++;
    put_be_uint16(&global_buffer, length + 4);
    put_raw_string(&global_buffer, string, length);
    put_string_id(&global_buffer, id);

    return id;
}

void
MR_threadscope_register_strings_array(MR_Threadscope_String *array,
    unsigned size)
{
    unsigned i;

    for (i = 0; i < size; i++) {
        array[i].MR_tsstring_id =
            MR_threadscope_register_string(array[i].MR_tsstring_string);
    }

    flush_event_buffer(&global_buffer);
}

void
MR_threadscope_post_log_msg(const char *message)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_variable_size_event(buffer, strlen(message) + 2)) {
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

void
MR_threadscope_post_create_engset(MR_EngSetId id, MR_EngSetType type)
{
    struct MR_threadscope_event_buffer *buffer = &global_buffer;

    if (!enough_room_for_event(buffer, MR_TS_EVENT_CAPSET_CREATE)) {
        flush_event_buffer(buffer);
    }

    put_event_header(buffer, MR_TS_EVENT_CAPSET_CREATE, 0);
    put_engset_id(buffer, id);
    put_engset_type(buffer, type);
}

void
MR_threadscope_post_destroy_engset(MR_EngSetId id)
{
    struct MR_threadscope_event_buffer *buffer = &global_buffer;

    if (!enough_room_for_event(buffer, MR_TS_EVENT_CAPSET_DELETE)) {
        flush_event_buffer(buffer);
    }

    put_event_header(buffer, MR_TS_EVENT_CAPSET_DELETE,
        get_current_time_nanosecs());
    put_engset_id(buffer, id);
}

void
MR_threadscope_post_engset_add(struct MR_threadscope_event_buffer *buffer,
    MR_EngSetId id, MR_EngineId eng)
{
    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    maybe_close_block(buffer);
    if (!enough_room_for_event(buffer, MR_TS_EVENT_CAPSET_ASSIGN_CAP)) {
        flush_event_buffer(buffer);
    }

    // When this event occurs the engine hasn't been setup yet. Even though
    // we use the engine's buffer, we cannot use get_current_time_nanosecs().

    put_event_header(buffer, MR_TS_EVENT_CAPSET_ASSIGN_CAP, 0);
    put_engset_id(buffer, id);
    put_engine_id(buffer, eng);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_engset_remove(MR_EngSetId id, MR_EngineId eng)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    maybe_close_block(buffer);
    if (!enough_room_for_event(buffer, MR_TS_EVENT_CAPSET_REMOVE_CAP)) {
        flush_event_buffer(buffer);
    }

    put_event_header(buffer, MR_TS_EVENT_CAPSET_REMOVE_CAP,
        get_current_time_nanosecs());
    put_engset_id(buffer, id);
    put_engine_id(buffer, eng);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_runtime_identifier(MR_EngSetId engset_id,
    const char *identifier)
{
    struct MR_threadscope_event_buffer *buffer = &global_buffer;
    unsigned len;

    len = strlen(identifier);

    if (!enough_room_for_variable_size_event(buffer, len + SZ_CAPSET_ID)) {
        flush_event_buffer(buffer);
    }

    put_event_header(buffer, MR_TS_EVENT_RTS_IDENTIFIER, 0);
    put_be_int16(buffer, len + SZ_CAPSET_ID);
    put_engset_id(buffer, engset_id);
    put_raw_string(buffer, identifier, len);
}

void
MR_threadscope_post_new_future(MR_Future *future_id, MR_TS_StringId name)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_FUT_CREATE)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_FUT_CREATE,
        get_current_time_nanosecs());
    put_future_id(buffer, future_id);
    put_string_id(buffer, name);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_wait_future_nosuspend(MR_Future* future_id)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_FUT_WAIT_NOSUSPEND)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_FUT_WAIT_NOSUSPEND,
        get_current_time_nanosecs());
    put_future_id(buffer, future_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_wait_future_suspended(MR_Future* future_id)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_FUT_WAIT_SUSPENDED)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_FUT_WAIT_SUSPENDED,
        get_current_time_nanosecs());
    put_future_id(buffer, future_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void
MR_threadscope_post_signal_future(MR_Future* future_id)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_FUT_SIGNAL)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_FUT_SIGNAL,
        get_current_time_nanosecs());
    put_future_id(buffer, future_id);

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

void MR_threadscope_post_engine_sleeping(void)
{
    struct MR_threadscope_event_buffer *buffer = MR_ENGINE(MR_eng_ts_buffer);

    MR_US_SPIN_LOCK(&(buffer->MR_tsbuffer_lock));
    if (!enough_room_for_event(buffer, MR_TS_MER_EVENT_ENGINE_SLEEPING)) {
        flush_event_buffer(buffer);
        open_block(buffer, MR_ENGINE(MR_eng_id));
    } else if (!block_is_open(buffer)) {
        open_block(buffer, MR_ENGINE(MR_eng_id));
    }

    put_event_header(buffer, MR_TS_MER_EVENT_ENGINE_SLEEPING,
        get_current_time_nanosecs());

    MR_US_UNLOCK(&(buffer->MR_tsbuffer_lock));
}

////////////////////////////////////////////////////////////////////////////

static struct MR_threadscope_event_buffer*
MR_create_event_buffer(void)
{
    struct MR_threadscope_event_buffer* buffer;

    buffer = MR_GC_NEW(MR_threadscope_event_buffer_t);
    buffer->MR_tsbuffer_pos = 0;
    buffer->MR_tsbuffer_block_open_pos = -1;
    buffer->MR_tsbuffer_ctxt_is_stopped = MR_TRUE;
    buffer->MR_tsbuffer_lock = MR_US_LOCK_INITIAL_VALUE;

    return buffer;
}

////////////////////////////////////////////////////////////////////////////

static void
MR_open_output_file_and_write_prelude(void)
{
    MR_Unsigned     filename_len;
    char            *progname_copy;
    char            *progname_base;
    MR_Unsigned     i;

    progname_copy = strdup(MR_progname);
    progname_base = basename(progname_copy);

    // This is an over-approximation on the amount of space needed
    // for this filename.

    filename_len = strlen(progname_base) + strlen(MR_TS_FILENAME_FORMAT) + 1;
    MR_threadscope_output_filename = MR_GC_NEW_ARRAY(char, filename_len);
    snprintf(MR_threadscope_output_filename, filename_len,
        MR_TS_FILENAME_FORMAT, progname_base);
    free(progname_copy);
    progname_copy = NULL;
    progname_base = NULL;

    MR_threadscope_output_file = fopen(MR_threadscope_output_filename, "w");
    if (!MR_threadscope_output_file) {
        MR_perror(MR_threadscope_output_filename);
        return;
    }

    put_be_uint32(&global_buffer, MR_TS_EVENT_HEADER_BEGIN);
    put_be_uint32(&global_buffer, MR_TS_EVENT_HET_BEGIN);
    for (i = 0;
        event_type_descs[i].etd_event_type != MR_TS_NUM_EVENT_TAGS;
        i++)
    {
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
                MR_perror(MR_threadscope_output_filename);
            }
            MR_threadscope_output_file = NULL;
            MR_threadscope_output_filename = NULL;
        }
    }
}

static void
put_event_type(struct MR_threadscope_event_buffer *buffer,
    EventTypeDesc *event_type_desc)
{
    MR_int_least16_t    size;
    EventType           event_type;

    // This also fills in our tables of event sizes.
    event_type = event_type_desc->etd_event_type;
    size = event_type_desc->etd_size;
    if (event_type < MR_TS_NUM_EVENT_TAGS) {
        event_type_sizes[event_type] = size;
    } else if ((event_type < (MR_TS_MER_EVENT_START + MR_TS_NUM_MER_EVENTS))
        && (event_type >= MR_TS_MER_EVENT_START))
    {
        event_type_sizes_mercury[event_type - MR_TS_MER_EVENT_START] = size;
    } else {
        fprintf(stderr, "Unknown event type %d\n", event_type);
        abort();
    }

    put_be_uint32(buffer, MR_TS_EVENT_ET_BEGIN);

    put_be_uint16(buffer, event_type);
    put_be_int16(buffer, size);

    put_string_size32(buffer, event_type_desc->etd_description);

    if (event_type_desc->edt_extends_event != 0xFFFF) {
        put_be_uint32(buffer, 2 + 2 + SZ_EVENT_TYPE);
        put_be_uint16(buffer, MR_EXT_TYPE_EXTENSION);
        put_be_uint16(buffer, SZ_EVENT_TYPE);
        put_be_uint16(buffer, event_type_desc->edt_extends_event);
    } else {
        // There is no extended data in this event.
        put_be_uint32(buffer, 0);
    }

    put_be_uint32(buffer, MR_TS_EVENT_ET_END);
}

static MR_bool
flush_event_buffer(struct MR_threadscope_event_buffer *buffer)
{
    maybe_close_block(buffer);

    // fwrite handles locking for us, so we have no concurrent access problems.

    if (MR_threadscope_output_file && buffer->MR_tsbuffer_pos) {
        if (0 == fwrite(buffer->MR_tsbuffer_data, buffer->MR_tsbuffer_pos, 1,
            MR_threadscope_output_file))
        {
            MR_perror(MR_threadscope_output_filename);
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
    MR_Unsigned saved_pos;

    if (buffer->MR_tsbuffer_block_open_pos != -1) {
        saved_pos = buffer->MR_tsbuffer_pos;
        buffer->MR_tsbuffer_pos = buffer->MR_tsbuffer_block_open_pos +
            sizeof(EventType) + sizeof(Time);
        put_eventlog_offset(buffer,
            saved_pos - buffer->MR_tsbuffer_block_open_pos);
        put_timestamp(buffer, get_current_time_nanosecs());

        buffer->MR_tsbuffer_block_open_pos = -1;
        buffer->MR_tsbuffer_pos = saved_pos;
    }
}

static void
open_block(struct MR_threadscope_event_buffer *buffer, MR_Unsigned eng_id)
{
    maybe_close_block(buffer);

    // Save the old position. Close block uses this so that it knows
    // where the block marker is that it should write into.

    buffer->MR_tsbuffer_block_open_pos = buffer->MR_tsbuffer_pos;

    put_event_header(buffer, MR_TS_EVENT_BLOCK_MARKER,
        get_current_time_nanosecs());

    // Skip over the next two fields, they are filled in by close_block.
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
        // GC might be running before we are done setting up.
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

        if (!enough_room_for_event(buffer, MR_TS_EVENT_GC_GLOBAL_SYNC)) {
            flush_event_buffer(buffer);
            open_block(buffer, MR_thread_engine_base->MR_eng_id);
        } else if (!block_is_open(buffer)) {
            open_block(buffer, MR_thread_engine_base->MR_eng_id);
        }

        // Ideally this event should be posted after the world has stopped.
        // Doing so means adding more instrumentation into Boehm.

        put_event_header(buffer, MR_TS_EVENT_GC_GLOBAL_SYNC,
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
        // GC might be running before we are done setting up.
        return;
    }

    if (MR_US_TRY_LOCK(&(buffer->MR_tsbuffer_lock))) {
        if (!enough_room_for_event(buffer, MR_TS_EVENT_GC_END)) {
            flush_event_buffer(buffer);
            open_block(buffer, MR_thread_engine_base->MR_eng_id);
        } else if (!block_is_open(buffer)) {
            open_block(buffer, MR_thread_engine_base->MR_eng_id);
        }

        put_event_header(buffer, MR_TS_EVENT_GC_END,
            get_current_time_nanosecs());

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
        fprintf(stderr, "In gc pause thread callback thread: 0x%lx\n",
            pthread_self())
    );
    if (MR_thread_engine_base == NULL) return;
    buffer = MR_thread_engine_base->MR_eng_ts_buffer;
    if (buffer == NULL) {
        // GC might be running before we are done setting up.
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
        fprintf(stderr, "In gc resume thread callback thread: 0x%lx\n",
            pthread_self());
    );
    if (MR_thread_engine_base == NULL) return;
    buffer = MR_thread_engine_base->MR_eng_ts_buffer;
    if (buffer == NULL) {
        // GC might be running before we are done setting up.
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

////////////////////////////////////////////////////////////////////////////

static Time
get_current_time_nanosecs(void)
{
    if (MR_threadscope_use_tsc) {
        MR_uint_least64_t   current_tsc;
        MercuryEngine       *eng = MR_thread_engine_base;

        current_tsc = MR_read_cpu_tsc();

        // The large constant (10^9) here converts seconds into nanoseconds.
        return (current_tsc + eng->MR_eng_cpu_clock_ticks_offset) /
            (MR_cpu_cycles_per_sec / 1000000000);
    } else {
        return gettimeofday_nsecs() + MR_gettimeofday_offset;
    }
}

static Time
gettimeofday_nsecs(void)
{
    struct timeval      tv;

    if (0 != gettimeofday(&tv, NULL)) {
        MR_perror("gettimeofday()");
        // Return a stupid value generating an obviously bad logfile
        // rather than crashing a program that may otherwise work.
        return 0;
    }
    return (Time) tv.tv_sec  * 1000000000 +
           (Time) tv.tv_usec * 1000;
}

////////////////////////////////////////////////////////////////////////////

#endif // MR_THREADSCOPE
