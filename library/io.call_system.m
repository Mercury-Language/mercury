%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: io.call_system.m.
%
% This module provides predicates to invoke commands via the shell
% of the underlying operation system.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module io.call_system.
:- interface.

:- import_module string.

    % Invokes the operating system shell with the specified Command.
    % Result is either `ok(ExitStatus)', if it was possible to invoke
    % the command, or `error(ErrorCode)' if not. The ExitStatus will be 0
    % if the command completed successfully or the return value of the system
    % call. If a signal kills the system call, then Result will be an error
    % indicating which signal occurred.
    %
:- pred call_system(string::in, io.res(int)::out, io::di, io::uo) is det.

    % call_system_return_signal(Command, Result, !IO):
    %
    % Invokes the operating system shell with the specified Command.
    % Result is either `ok(ExitStatus)' if it was possible to invoke
    % the command or `error(Error)' if the command could not be executed.
    % If the command could be executed then ExitStatus is either
    % `exited(ExitCode)' if the command ran to completion or
    % `signalled(SignalNum)' if the command was killed by a signal.
    % If the command ran to completion then ExitCode will be 0 if the command
    % ran successfully and the return value of the command otherwise.
    %
:- pred call_system_return_signal(string::in, io.res(system_result)::out,
    io::di, io::uo) is det.

    % Interpret the child process exit status returned by system() or wait().
    %
:- func decode_system_command_exit_code(int) = io.res(io.system_result).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bitmap.
:- import_module bool.
:- import_module char.
:- import_module deconstruct.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module stream.
:- import_module time.
:- import_module univ.

%---------------------------------------------------------------------------%

call_system(Command, Result, !IO) :-
    io.call_system.call_system_return_signal(Command, Result0, !IO),
    (
        Result0 = ok(exited(Code)),
        Result = ok(Code)
    ;
        Result0 = ok(signalled(Signal)),
        string.int_to_string(Signal, SignalStr),
        ErrMsg = "system command killed by signal number " ++ SignalStr,
        Result = error(io_error(ErrMsg))
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

%---------------------------------------------------------------------------%

call_system_return_signal(Command, Result, !IO) :-
    call_system_code(Command, Status, Error, !IO),
    is_error(Error, "error invoking system command: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = decode_system_command_exit_code(Status)
    ).

    % call_system_code(Command, Status, Error, !IO):
    %
    % Invokes the operating system shell with the specified Command.
    % Status is valid when Error indicates success.
    %
:- pred call_system_code(string::in, int::out, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    call_system_code(Command::in, Status::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    // In multithreaded grades, try to use posix_spawn() instead of system().
    // There were problems with threads and system() on Linux/glibc, probably
    // because system() uses fork().
#if defined(MR_THREAD_SAFE) && defined(MR_HAVE_POSIX_SPAWN) && \
        defined(MR_HAVE_ENVIRON)

    char    *argv[4];
    pid_t   pid;
    int     err;
    int     st;

    argv[0] = (char *) ""sh"";
    argv[1] = (char *) ""-c"";
    argv[2] = Command;
    argv[3] = NULL;

    // Protect `environ' from concurrent modifications.
    MR_OBTAIN_GLOBAL_LOCK(""io.call_system_code/5"");

    // See the comment at the head of the body of preceding foreign_decl
    // for details of why Mac OS X is different here.
    #if defined(MR_MAC_OSX)
        err = posix_spawn(&pid, ""/bin/sh"", NULL, NULL, argv,
            *_NSGetEnviron());
    #else
        err = posix_spawn(&pid, ""/bin/sh"", NULL, NULL, argv, environ);
    #endif

    MR_RELEASE_GLOBAL_LOCK(""io.call_system_code/5"");

    if (err != 0) {
        // Spawn failed.
        Error = errno;
    } else {
        // Wait for the spawned process to exit.
        do {
            err = waitpid(pid, &st, 0);
        } while (err == -1 && MR_is_eintr(errno));
        if (err == -1) {
            Error = errno;
        } else {
            Status = st;
            Error = 0;
        }
    }

#else   // !MR_THREAD_SAFE || !MR_HAVE_POSIX_SPAWN || !MR_HAVE_ENVIRON

  #ifdef MR_WIN32
    Status = _wsystem(ML_utf8_to_wide(Command));
  #else
    Status = system(Command);
  #endif

    if (Status == -1) {
        Error = errno;
    } else {
        Error = 0;
    }

#endif  // !MR_THREAD_SAFE || !MR_HAVE_POSIX_SPAWN || !MR_HAVE_ENVIRON
").

:- pragma foreign_proc("C#",
    call_system_code(Command::in, Status::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    try {
        // XXX This could be better... need to handle embedded spaces
        // in the command name ...
        // XXX ... and redirection, and everything else done by sh.
        int index = Command.IndexOf("" "");
        string command, arguments;
        if (index > 0) {
            command = Command.Substring(0, index);
            arguments = Command.Remove(0, index + 1);
        } else {
            command = Command;
            arguments = """";
        }

        System.Diagnostics.Process process = new System.Diagnostics.Process();
        // Never interpret the command as a document to open with whatever
        // application is registered for that document type. This also
        // prevents creating a new window for console programs on Windows.
        process.StartInfo.UseShellExecute = false;
        process.StartInfo.FileName = command;
        process.StartInfo.Arguments = arguments;
        process.Start();
        process.WaitForExit();
        Status = process.ExitCode;
        Error = null;
    }
    catch (System.Exception e) {
        Status = 1;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    call_system_code(Command::in, Status::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    boolean has_sh;
    try {
        SecurityManager sm = System.getSecurityManager();
        if (sm != null) {
            sm.checkExec(""/bin/sh"");
            has_sh = true;
        } else {
            // If there is no security manager installed, we just check
            // if the file exists.
            has_sh = new java.io.File(""/bin/sh"").exists();
        }
    } catch (java.lang.Exception e) {
        has_sh = false;
    }

    try {
        // Emulate system() if /bin/sh is available.
        java.lang.Process process;
        if (has_sh) {
            final String[] args = {""/bin/sh"", ""-c"", Command};
            process = java.lang.Runtime.getRuntime().exec(args);
        } else {
            process = java.lang.Runtime.getRuntime().exec(Command);
        }

        StreamPipe stdin = new StreamPipe(jmercury.io.mercury_stdin,
            process.getOutputStream());
        StreamPipe stdout = new StreamPipe(process.getInputStream(),
            jmercury.io.mercury_stdout);
        StreamPipe stderr = new StreamPipe(process.getErrorStream(),
            jmercury.io.mercury_stderr);
        stdin.start();
        stdout.start();
        stderr.start();

        Status = process.waitFor();
        Error = null;

        // The stdin StreamPipe is killed off after the Process is finished
        // so as not to waste CPU cycles with a pointless thread.
        stdin.interrupt();

        // Wait for all the outputs to be written.
        stdout.join();
        stderr.join();

        if (stdin.exception != null) {
            throw stdin.exception;
        }
        if (stdout.exception != null) {
            throw stdout.exception;
        }
        if (stderr.exception != null) {
            throw stderr.exception;
        }
    } catch (java.lang.Exception e) {
        Status  = 1;
        Error = e;
    }
").

:- pragma foreign_code("Java", "

    // StreamPipe is a mechanism for connecting streams to those of a
    // Runtime.exec() Process.

    private static class StreamPipe extends java.lang.Thread {
        jmercury.io.MR_TextInputFile        in;
        jmercury.io.MR_TextOutputFile       out;
        boolean                 closeOutput = false;
        java.lang.Exception     exception = null;

        StreamPipe(java.io.InputStream in, jmercury.io.MR_TextOutputFile out) {
            this.in  = new jmercury.io.MR_TextInputFile(in);
            this.out = out;
        }

        StreamPipe(jmercury.io.MR_TextInputFile in, java.io.OutputStream out) {
            this.in  = in;
            this.out = new jmercury.io.MR_TextOutputFile(out);
            closeOutput = true;
        }

        public void run() {
            try {
                while (true) {
                    int c = in.read_char();
                    if (c == -1 || interrupted()) {
                        break;
                    }
                    out.put((char) c);
                }
                out.flush();
                if (closeOutput) {
                    out.close();
                }
            }
            catch (java.lang.Exception e) {
                exception = e;
            }
        }
    } // class StreamPipe
").

%---------------------------------------------------------------------------%

decode_system_command_exit_code(Code0) = Status :-
    do_decode_system_command_exit_code(Code0, Exited, ExitCode, Signalled,
        Signal),
    (
        Exited = yes,
        Status = ok(exited(ExitCode))
    ;
        Exited = no,
        (
            Signalled = yes,
            Status = ok(signalled(Signal))
        ;
            Signalled = no,
            Status = error(io_error("unknown result code from system command"))
        )
    ).

    % Interpret the child process exit status returned by system() or wait():
    %
:- pred do_decode_system_command_exit_code(int::in, bool::out, int::out,
    bool::out, int::out) is det.

:- pragma foreign_proc("C",
    do_decode_system_command_exit_code(Status0::in, Exited::out, Status::out,
        Signalled::out, Signal::out),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, no_sharing],
"
    Exited = MR_NO;
    Status = 0;
    Signalled = MR_NO;
    Signal = 0;

    #if defined (WIFEXITED) && defined (WEXITSTATUS) && \
            defined (WIFSIGNALED) && defined (WTERMSIG)
        if (WIFEXITED(Status0)) {
            Exited = MR_YES;
            Status = WEXITSTATUS(Status0);
        } else if (WIFSIGNALED(Status0)) {
            Signalled = MR_YES;
            Signal = WTERMSIG(Status0);
        }
    #else
        Exited = MR_YES;
        Status = Status0;
    #endif
").

% This is a fall-back for back-ends that don't support the C interface.
do_decode_system_command_exit_code(Status, yes, Status, no, 0).

%---------------------------------------------------------------------------%
:- end_module io.call_system.
%---------------------------------------------------------------------------%
