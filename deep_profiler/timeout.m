%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2006, 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: timeout.m.
% Author: zs.
%
% This module implements timeouts and cleanup for the deep profiler.
%
% The timeout design we use and its rationale are given in the file DESIGN.
%
% The cleanup system consists of an array of filenames. When the profiler
% creates a temporary file, it adds its name to the array; when it deletes
% the temporary file, it deletes its name from the array. When we get an
% unexpected signal, we clean up by deleting all the temporary files named
% in the array.
%
% We establish the exit action to clean up the files as soon as they are
% created, but we don't want the parent process after the fork to delete them
% while they are still in use by the child process. This is prevented by the
% boolean flag process_is_detached_server.
%
%---------------------------------------------------------------------------%

:- module timeout.
:- interface.

:- import_module bool.
:- import_module io.

%---------------------------------------------------------------------------%

    % Add the given file name to the list of files to be cleaned up.
    %
:- pred register_file_for_cleanup(string::in, io::di, io::uo) is det.

    % Remove the given file name from the list of files to be cleaned up.
    %
:- pred unregister_file_for_cleanup(string::in, io::di, io::uo) is det.

    % Remove all file names from the list of files to be cleaned up.
    %
:- pred unregister_all_files_for_cleanup(io::di, io::uo) is det.

    % Delete all the files on the cleanup list.
    %
:- pred delete_cleanup_files(io::di, io::uo) is det.

    % Set up signal handlers for all the signals we can catch.
    % The three strings specify the name of the mutex file, the name of the
    % directory containing the `want' files, and the prefix of the names of
    % the `want' files.
    %
:- pred setup_signals(string::in, string::in, string::in,
    io::di, io::uo) is det.

    % Set up a timeout for the given number of minutes in the future.
    %
:- pred setup_timeout(int::in, io::di, io::uo) is det.

    % Get the lock on the named mutex file if the bool is `no'.
    % (The mutex file exists iff some process holds the lock.)
    % If the bool is `yes', meaning debugging is enabled, do nothing.
    %
:- pred get_lock(bool::in, string::in, io::di, io::uo) is det.

    % Release the lock on the named mutex file if the bool is `no'.
    % (The mutex file exists iff some process holds the lock.)
    % If the bool is `yes', meaning debugging is enabled, do nothing.
    %
:- pred release_lock(bool::in, string::in,
    io::di, io::uo) is det.

    % Create the `want' file with the given name.
    %
:- pred make_want_file(string::in, io::di, io::uo) is det.

    % Delete the `want' file with the given name.
    %
:- pred remove_want_file(string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
#ifdef  MR_DEEP_PROFILER_ENABLED

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>  /* for O_CREAT, O_EXCL */
#include <signal.h> /* for signal numbers */
#include <unistd.h> /* for alarm() */
#include <stdio.h>
#include <errno.h>  /* for EEXIST etc */
#include    <dirent.h>
#include ""mercury_signal.h""

#define MP_MAX_CLEANUP_FILES    20  /* this should be plenty */

extern  const char  *MP_cleanup_files[MP_MAX_CLEANUP_FILES];
extern  int     MP_cleanup_file_next;

extern  void        MP_maybe_print_cleanup_files(const char *msg);
extern  void        MP_register_cleanup_file(const char *filename);
extern  void        MP_unregister_cleanup_file(const char *filename);
extern  void        MP_handle_fatal_exception(void *data);
extern  void        MP_delete_cleanup_files(void);
MR_NO_RETURN(extern  void        MP_delete_cleanup_files_and_exit_failure(
                        const char *signal_name));

extern  int         MP_timeout_seconds;
extern  const char  *MP_timeout_mutex_file;
extern  const char  *MP_timeout_want_dir;
extern  const char  *MP_timeout_want_prefix;

typedef struct
{
    int             MP_signum;
    void            (*MP_handler)(void);
} MP_sig_handler;

extern  const MP_sig_handler    MP_signal_structs[];

extern  void    MP_handle_timeout(void);

MR_NO_RETURN(extern  void    MP_handle_sig_term(void));
MR_NO_RETURN(extern  void    MP_handle_sig_hup(void));
MR_NO_RETURN(extern  void    MP_handle_sig_int(void));
MR_NO_RETURN(extern  void    MP_handle_sig_quit(void));
MR_NO_RETURN(extern  void    MP_handle_sig_ill(void));
MR_NO_RETURN(extern  void    MP_handle_sig_abrt(void));
MR_NO_RETURN(extern  void    MP_handle_sig_bus(void));
MR_NO_RETURN(extern  void    MP_handle_sig_fpe(void));
MR_NO_RETURN(extern  void    MP_handle_sig_segv(void));
MR_NO_RETURN(extern  void    MP_handle_sig_pipe(void));

extern  MR_bool MP_do_try_get_lock(const char *mutex_file);
extern  void    MP_do_get_lock(const char *mutex_file);
extern  void    MP_do_release_lock(const char *mutex_file);

#endif
").

:- pragma foreign_code("C",
"
#ifdef  MR_DEEP_PROFILER_ENABLED

#include    <sys/types.h>

const char  *MP_cleanup_files[MP_MAX_CLEANUP_FILES];
int         MP_cleanup_file_next = 0;

int         MP_timeout_seconds = 30 * 60;
const char  *MP_timeout_mutex_file = NULL;
const char  *MP_timeout_want_dir = NULL;
const char  *MP_timeout_want_prefix = NULL;

/* set this variable to MR_TRUE to debug the code cleanup array */
MR_bool     MP_print_cleanup_files = MR_FALSE;

void
MP_maybe_print_cleanup_files(const char *msg)
{
    int i;

    if (MP_print_cleanup_files) {
        fprintf(stderr, ""\\n%s cleanup files:\\n"", msg);
        for (i = 0; i < MP_cleanup_file_next; i++) {
            fprintf(stderr, ""%i %s\\n"", i, MP_cleanup_files[i]);
        }
    }
}

void
MP_register_cleanup_file(const char *filename)
{
    int i;

    if (MP_cleanup_file_next >= MP_MAX_CLEANUP_FILES - 1) {
        MR_fatal_error(""MP_register_cleanup_file: too many entries"");
    }

    for (i = 0; i < MP_cleanup_file_next; i++) {
        if (MR_streq(filename, MP_cleanup_files[i])) {
            MR_fatal_error(""MP_register_cleanup_file: duplicate"");
        }
    }

    MP_cleanup_files[MP_cleanup_file_next] = filename;
    MP_cleanup_file_next++;
    MP_maybe_print_cleanup_files(""register"");
}

void
MP_unregister_cleanup_file(const char *filename)
{
    int i;
    int j;

    for (i = 0; i < MP_cleanup_file_next; i++) {
        if (MR_streq(filename, MP_cleanup_files[i])) {
            /* shift the array entries above index i down one */
            for (j = i + 1; j < MP_cleanup_file_next; j++) {
                MP_cleanup_files[j - 1] = MP_cleanup_files[j];
            }

            MP_cleanup_file_next--;
            MP_maybe_print_cleanup_files(""unregister"");
            return;
        }
    }

    MR_fatal_error(""MP_unregister_cleanup_file: not found"");
}

void
MP_handle_fatal_exception(void *data)
{
    /* we ignore data */
    MP_delete_cleanup_files();
}

void
MP_delete_cleanup_files(void)
{
    int     i;
    MR_bool delayed_mutex_file;

    /*
    ** We want to remove the mutex file only after we have removed the
    ** files manipulated by the critical section it was protecting.
    */

    MP_maybe_print_cleanup_files(""delete"");

    delayed_mutex_file = MR_FALSE;
    for (i = 0; i < MP_cleanup_file_next; i++) {
        if (MR_streq(MP_timeout_mutex_file, MP_cleanup_files[i])) {
            delayed_mutex_file = MR_TRUE;
        } else {
            if (remove(MP_cleanup_files[i]) != 0) {
                perror(MP_cleanup_files[i]);
            }
        }
    }

    if (delayed_mutex_file) {
        if (remove(MP_timeout_mutex_file) != 0) {
            perror(MP_timeout_mutex_file);
        }
    }

    MP_cleanup_file_next = 0;
}

void
MP_delete_cleanup_files_and_exit_failure(const char *signal_name)
{

#ifdef  MP_DEBUG_MDPROF_SIGNAL
    FILE    *fp;
    char    buf[1024];  /* that should be big enough */

    fp = fopen(""/tmp/mdprof_signal"", ""w"");
    if (fp != NULL) {
        fprintf(fp, ""%s\\n"", signal_name);
        (void) fclose(fp);
    }
#endif

    MP_delete_cleanup_files();

#ifdef  MP_DEBUG_MDPROF_SIGNAL
    sprintf(buf, ""Mercury deep profiler: received unexpected signal %s"",
        signal_name);
    MR_fatal_error(buf);
#else
    exit(EXIT_FAILURE);
#endif
}

/*
** SIGALRM alarm signal indicates a timeout. SIGTERM usually indicates the
** machine is being shut down. The others are there to catch forceful shutdowns
** during development, both intentional ones where the programmer sends the
** signal and those caused by bugs in the server code. We would like to include
** all catchable, fatal signals in this list, but that set is somewhat OS
** dependent. The set whose existence we test for here includes all the
** signals that are at all likely to be sent to server process.
**
** We don't test for the existence of SIGALRM, because we want compilation to
** fail if it does not exist. Without alarm signals, server processes will
** never be timed out, and thus constitute a resource leak (mostly of virtual
** memory/swap space).
**
** We could avoid this problem if we had a version of atexit that executed
** its actions even when the program exits after a signal.
*/

const MP_sig_handler MP_signal_structs[] =
{
    { SIGALRM,  MP_handle_timeout },
#ifdef SIGTERM
    { SIGTERM,  MP_handle_sig_term },
#endif
#ifdef SIGHUP
    { SIGHUP,   MP_handle_sig_hup },
#endif
#ifdef SIGINT
    { SIGINT,   MP_handle_sig_int },
#endif
#ifdef SIGQUIT
    { SIGQUIT,  MP_handle_sig_quit },
#endif
#ifdef SIGILL
    { SIGILL,   MP_handle_sig_ill },
#endif
#ifdef SIGABRT
    { SIGABRT,  MP_handle_sig_abrt },
#endif
#ifdef SIGBUS
    { SIGBUS,   MP_handle_sig_bus },
#endif
#ifdef SIGFPE
    { SIGFPE,   MP_handle_sig_fpe },
#endif
#ifdef SIGSEGV
    { SIGSEGV,  MP_handle_sig_segv },
#endif
#ifdef SIGPIPE
    { SIGPIPE,  MP_handle_sig_pipe },
#endif
    { -1,       NULL }
};

void
MP_handle_timeout(void)
{
    DIR             *dir;
    struct  dirent  *dirent;
    size_t          matchlen;
    MR_bool         success;

#ifdef  MP_DEBUG_LOCKS
    {
        FILE    *debug_fp;

        debug_fp = fopen(""/tmp/deep_locks"", ""a"");
        if (debug_fp != NULL) {
            fprintf(debug_fp, ""pid %d MP_handle_timeout at %d\\n"",
                getpid(), (int) time(NULL));
            fclose(debug_fp);
        }
    }
#endif

    if (MP_timeout_want_dir == NULL || MP_timeout_want_prefix == NULL) {
        MR_fatal_error(""MP_handle_timeout: null dir or prefix"");
    }

    matchlen = strlen(MP_timeout_want_prefix);

    success = MP_do_try_get_lock(MP_timeout_mutex_file);
    if (! success) {
        /*
        ** We could not get the lock, so some other process holds it.
        ** We therefore abort the timeout, but schedule the next one.
        */

#ifdef  MP_DEBUG_LOCKS
        {
            FILE    *debug_fp;

            debug_fp = fopen(""/tmp/deep_locks"", ""a"");
            if (debug_fp != NULL) {
                fprintf(debug_fp, ""pid %d aborting timeout: lock\\n"",
                    getpid());
                fclose(debug_fp);
            }
        }
#endif

        (void) alarm(MP_timeout_seconds);
        return;
    }

    dir = opendir(MP_timeout_want_dir);
    if (dir == NULL) {
        char    errbuf[MR_STRERROR_BUF_SIZE];

        MR_fatal_error(""MP_handle_timeout: opendir failed: %s"",
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }

    while ((dirent = readdir(dir)) != NULL) {
        if (MR_strneq(dirent->d_name, MP_timeout_want_prefix, matchlen)) {

#ifdef  MP_DEBUG_LOCKS
            {
                FILE    *debug_fp;

                debug_fp = fopen(""/tmp/deep_locks"", ""a"");
                if (debug_fp != NULL) {
                    fprintf(debug_fp,
                        ""pid %d aborting timeout: want file\\n"", getpid());
                    fclose(debug_fp);
                }
            }
#endif
            /* abort the timeout */
            (void) closedir(dir);
            (void) alarm(MP_timeout_seconds);
            return;
        }
    }

    (void) closedir(dir);

    /*
    ** This call will delete the mutex file last, releasing the mutex.
    */

    MP_delete_cleanup_files();

#ifdef  MP_DEBUG_LOCKS
    {
        FILE    *debug_fp;

        debug_fp = fopen(""/tmp/deep_locks"", ""a"");
        if (debug_fp != NULL) {
            fprintf(debug_fp, ""pid %d timeout exit\\n"", getpid());
            fclose(debug_fp);
        }
    }
#endif

    exit(EXIT_SUCCESS);
}

void
MP_handle_sig_term(void)
{
    MP_delete_cleanup_files_and_exit_failure(""SIGTERM"");
}

void
MP_handle_sig_hup(void)
{
    MP_delete_cleanup_files_and_exit_failure(""SIGHUP"");
}

void
MP_handle_sig_int(void)
{
    MP_delete_cleanup_files_and_exit_failure(""SIGINT"");
}

void
MP_handle_sig_quit(void)
{
    MP_delete_cleanup_files_and_exit_failure(""SIGQUIT"");
}

void
MP_handle_sig_ill(void)
{
    MP_delete_cleanup_files_and_exit_failure(""SIGILL"");
}

void
MP_handle_sig_abrt(void)
{
    MP_delete_cleanup_files_and_exit_failure(""SIGABRT"");
}

void
MP_handle_sig_bus(void)
{
    MP_delete_cleanup_files_and_exit_failure(""SIGBUS"");
}

void
MP_handle_sig_fpe(void)
{
    MP_delete_cleanup_files_and_exit_failure(""SIGFPE"");
}

void
MP_handle_sig_segv(void)
{
    MP_delete_cleanup_files_and_exit_failure(""SIGSEGV"");
}

void
MP_handle_sig_pipe(void)
{
    MP_delete_cleanup_files_and_exit_failure(""SIGPIPE"");
}

MR_bool
MP_do_try_get_lock(const char *mutex_file)
{
    int     res;
    MR_bool success;

    res = open(mutex_file, O_CREAT | O_EXCL, 0);
    if (res >= 0) {
#ifdef  MP_DEBUG_LOCKS
        FILE    *debug_fp;

        debug_fp = fopen(""/tmp/deep_locks"", ""a"");
        if (debug_fp != NULL) {
            fprintf(debug_fp, ""pid %d try: lock %s\\n"",
                getpid(), mutex_file);
            fclose(debug_fp);
        }
#endif

        (void) close(res);
        MP_register_cleanup_file(mutex_file);
        success = MR_TRUE;
    } else if (res < 0 && errno == EEXIST) {
#ifdef  MP_DEBUG_LOCKS
        FILE    *debug_fp;

        debug_fp = fopen(""/tmp/deep_locks"", ""a"");
        if (debug_fp != NULL) {
            fprintf(debug_fp, ""pid %d try: no lock %s\\n"",
                getpid(), mutex_file);
            fclose(debug_fp);
        }
#endif

        success = MR_FALSE;
    } else {
        char    errbuf[MR_STRERROR_BUF_SIZE];

        MR_fatal_error(""MP_do_try_get_lock failed: %s"",
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }

    return success;
}

void
MP_do_get_lock(const char *mutex_file)
{
    int res;

    for (;;) {
        res = open(mutex_file, O_CREAT | O_EXCL, 0);
        if (res >= 0) {
#ifdef  MP_DEBUG_LOCKS
            FILE    *debug_fp;

            debug_fp = fopen(""/tmp/deep_locks"", ""a"");
            if (debug_fp != NULL) {
                fprintf(debug_fp, ""pid %d got lock %s\\n"",
                    getpid(), mutex_file);
                fclose(debug_fp);
            }
#endif

            (void) close(res);
            MP_register_cleanup_file(mutex_file);
            return;
        } else if (res < 0 && errno == EEXIST) {
#ifdef  MP_DEBUG_LOCKS
            FILE    *debug_fp;

            debug_fp = fopen(""/tmp/deep_locks"", ""a"");
            if (debug_fp != NULL) {
                fprintf(debug_fp, ""pid %d trying to lock %s ...\\n"",
                    getpid(), mutex_file);
                fclose(debug_fp);
            }
#endif

            sleep(5);
            continue;
        } else {
            char    errbuf[MR_STRERROR_BUF_SIZE];

            MR_fatal_error(""MP_do_get_lock failed: %s"",
                MR_strerror(errno, errbuf, sizeof(errbuf)));
        }
    }
}

void
MP_do_release_lock(const char *mutex_file)
{
#ifdef  MP_DEBUG_LOCKS
    FILE    *debug_fp;

    debug_fp = fopen(""/tmp/deep_locks"", ""a"");
    if (debug_fp != NULL) {
        fprintf(debug_fp, ""pid %d releasing lock %s\\n"",
            getpid(), mutex_file);
        fclose(debug_fp);
    }
#endif

    MP_unregister_cleanup_file(mutex_file);
    (void) unlink(mutex_file);
}

#endif  /* MR_DEEP_PROFILER_ENABLED */
").

:- pragma foreign_proc("C",
    register_file_for_cleanup(File::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
#ifdef  MR_DEEP_PROFILER_ENABLED
    MP_register_cleanup_file(File);
#else
    MR_fatal_error(""deep profiler not enabled"");
#endif
").

:- pragma foreign_proc("C",
    unregister_file_for_cleanup(File::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
#ifdef  MR_DEEP_PROFILER_ENABLED
    MP_unregister_cleanup_file(File);
#else
    MR_fatal_error(""deep profiler not enabled"");
#endif
").

:- pragma foreign_proc("C",
    unregister_all_files_for_cleanup(_S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
#ifdef  MR_DEEP_PROFILER_ENABLED
    MP_cleanup_file_next = 0;
#else
    MR_fatal_error(""deep profiler not enabled"");
#endif
").

:- pragma foreign_proc("C",
    delete_cleanup_files(_S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
#ifdef  MR_DEEP_PROFILER_ENABLED
    MP_delete_cleanup_files();
#else
    MR_fatal_error(""deep profiler not enabled"");
#endif
").

:- pragma foreign_proc("C",
    setup_signals(MutexFile::in, WantDir::in, WantPrefix::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
#ifdef  MR_DEEP_PROFILER_ENABLED
    int i;

#ifdef  MP_DEBUG_LOCKS
    {
        FILE    *debug_fp;

        debug_fp = fopen(""/tmp/deep_locks"", ""a"");
        if (debug_fp != NULL) {
            fprintf(debug_fp, ""pid %d setup signals ...\\n"", getpid());
            fprintf(debug_fp, ""mutexfile %s, wantdir %s, wantprefix %s\\n"",
                MutexFile, WantDir, WantPrefix);
            fclose(debug_fp);
        }
    }
#endif

    MP_timeout_mutex_file = MutexFile;
    MP_timeout_want_dir = WantDir;
    MP_timeout_want_prefix = WantPrefix;

    for (i = 0; MP_signal_structs[i].MP_signum >= 0; i++) {
        MR_setup_signal(MP_signal_structs[i].MP_signum,
            MP_signal_structs[i].MP_handler, MR_FALSE,
            ""Mercury deep profiler: cannot setup signal exit"");
    }

    /*
    ** Mercury exceptions do not cause signals. The default exception
    ** handler prints and error message and exits. To ensure that
    ** we delete the files we need to clean up, we get the exit
    ** library function to invoke MP_delete_cleanup_files through
    ** MP_handle_fatal_exception.
    */

    MR_register_exception_cleanup(MP_handle_fatal_exception, NULL);

#else
    MR_fatal_error(""deep profiler not enabled"");
#endif
").

:- pragma foreign_proc("C",
    setup_timeout(Minutes::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
#ifdef  MR_DEEP_PROFILER_ENABLED
    MP_timeout_seconds = (int) Minutes * 60;
    (void) alarm(MP_timeout_seconds);

#ifdef  MP_DEBUG_LOCKS
    {
        FILE    *debug_fp;

        debug_fp = fopen(""/tmp/deep_locks"", ""a"");
        if (debug_fp != NULL) {
            fprintf(debug_fp, ""pid %d setup timeout at %d for %d\\n"",
                getpid(), (int) time(NULL),
                (int) time(NULL) + MP_timeout_seconds);
            fclose(debug_fp);
        }
    }
#endif
#else
    MR_fatal_error(""deep profiler not enabled"");
#endif
").

%---------------------------------------------------------------------------%

get_lock(Debug, MutexFile, !IO) :-
    (
        Debug = yes
    ;
        Debug = no,
        do_get_lock(MutexFile, !IO)
    ).

release_lock(Debug, MutexFile, !IO) :-
    (
        Debug = yes
    ;
        Debug = no,
        do_release_lock(MutexFile, !IO)
    ).

:- pred do_get_lock(string::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_get_lock(MutexFile::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
#ifdef  MR_DEEP_PROFILER_ENABLED
    MP_do_get_lock(MutexFile);
#else
    MR_fatal_error(""deep profiler not enabled"");
#endif
").

:- pred do_release_lock(string::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_release_lock(MutexFile::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
#ifdef  MR_DEEP_PROFILER_ENABLED
    MP_do_release_lock(MutexFile);
#else
    MR_fatal_error(""deep profiler not enabled"");
#endif
").

:- pragma foreign_proc("C",
    make_want_file(WantFileName::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
#ifdef  MR_DEEP_PROFILER_ENABLED
    int     fd;
    char    errbuf[MR_STRERROR_BUF_SIZE];

    fd = open(WantFileName, O_CREAT, 0);
    if (fd < 0) {
        MR_fatal_error(""make_want_file: open failed: %s"",
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }
    (void) close(fd);
    MP_register_cleanup_file(WantFileName);
#else
    MR_fatal_error(""deep profiler not enabled"");
#endif
").

:- pragma foreign_proc("C",
    remove_want_file(WantFileName::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
#ifdef  MR_DEEP_PROFILER_ENABLED
    MP_unregister_cleanup_file(WantFileName);
    (void) unlink(WantFileName);
#else
    MR_fatal_error(""deep profiler not enabled"");
#endif
").

%---------------------------------------------------------------------------%
:- end_module timeout.
%---------------------------------------------------------------------------%
