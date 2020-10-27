%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2010-2011 The University of Melbourne.
% Copyright (C) 2015, 2017-2018, 2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: listing.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>
%
% Support for providing file listing functionality in the debugger.
%
% Unfortunately, scanning large files such as library/io.m byte-by-byte
% in a debugging grade is likely to exhaust the stack, because debugging
% grades do not support tail recursion. Instead we have to handle this
% aspect using a bit of C code.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.listing.
:- interface.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- type search_path.
:- type line_no    == int.
:- type path_name  == string.
:- type file_name  == string.
:- type c_file_ptr.                     % For passing `FILE *' arguments.

    % Construct an empty search_path structure.
    %
:- func new_list_path = search_path.

    % Get/set/clear the stack of directories searched for FileName matches by
    %   list_file/7.
    %
:- func get_list_path(search_path::in) = (list(path_name)::out) is det.
:- pred set_list_path(list(path_name)::in,
    search_path::in, search_path::out) is det.
:- pred clear_list_path(search_path::in, search_path::out) is det.

    % push_list_path(Dir, !Path):
    %
    % Push Dir on to the stack of directories searched for FileName
    % matches by list_file/7.
    %
:- pred push_list_path(path_name::in, search_path::in, search_path::out)
    is det.

    % pop_list_path(!Path):
    %
    % Pop the last Dir pushed on to the stack of directories.
    % Does nothing if the search path stack is empty.
    %
:- pred pop_list_path(search_path::in, search_path::out) is det.

    % list_file(OutStrm, ErrStrm, FileName, FirstLine, LastLine, MarkLine,
    %   Path, !IO):
    %
    % Print, on OutStrm, the lines from FileName with numbers in the range
    % FirstLine.. LastLine (the first line is numbered 1). We mark the line
    % numbered MarkLine with a chevron; we indent all other lines
    % appropriately.
    %
    % We search for the file matching FileName by first looking in the current
    % working directory or, failing that, by prepending each Dir on the
    % search path stack in turn until a match is found. If no match is found,
    % we print an error message.
    %
    % We report any errors on ErrStrm.
    %
:- pred list_file(c_file_ptr::in, c_file_ptr::in, file_name::in, line_no::in,
    line_no::in, line_no::in, search_path::in, io::di, io::uo) is det.

    % As above, but implemented without foreign code. This is used by the
    % source-to-source debugger which does not enable debugging in standard
    % library so does not suffer the problem of excessive stack usage.
    %
:- pred list_file_portable(io.output_stream::in, io.output_stream::in,
    file_name::in, line_no::in, line_no::in, line_no::in, search_path::in,
    io::di, io::uo) is det.

    % list_file_with_command(OutStrm, ErrStrm, FileName, FirstLine, LastLine,
    %   MarkLine, Path, !IO):
    %
    % Like list_file, but invokes an external command to print the source
    % listing. The command is passed the four arguments:
    %
    %   FileName, FirstLine, LastLine, MarkLine
    %
    % The command should print all the lines from the first to the last,
    % both inclusive, with the current line marked (or highlighted) in
    % some fashion to standard output, and report any errors to standard error.
    %
:- pred list_file_with_command(c_file_ptr::in, c_file_ptr::in, string::in,
    file_name::in, line_no::in, line_no::in, line_no::in, search_path::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module maybe.
:- import_module string.
:- import_module type_desc.

%---------------------------------------------------------------------------%

:- type search_path  == list(path_name).

:- pragma foreign_type("C", c_file_ptr, "FILE *", [can_pass_as_mercury_type]).
    % stub.
:- pragma foreign_type("C#", c_file_ptr, "object").
:- pragma foreign_type("Java", c_file_ptr, "java.lang.Object").

    % These predicates are called from trace/mercury_trace_internal.c.
    %
:- pragma foreign_export("C", new_list_path = out,
    "ML_LISTING_new_list_path").
:- pragma foreign_export("C", get_list_path(in) = out,
    "ML_LISTING_get_list_path").
:- pragma foreign_export("C", set_list_path(in, in, out),
    "ML_LISTING_set_list_path").
:- pragma foreign_export("C", clear_list_path(in, out),
    "ML_LISTING_clear_list_path").
:- pragma foreign_export("C", push_list_path(in, in, out),
    "ML_LISTING_push_list_path").
:- pragma foreign_export("C", pop_list_path(in, out),
    "ML_LISTING_pop_list_path").
:- pragma foreign_export("C", list_file(in, in, in, in, in, in, in, di, uo),
    "ML_LISTING_list_file").
:- pragma foreign_export("C", list_file_with_command(in, in, in, in, in, in,
    in, in, di, uo), "ML_LISTING_list_file_with_command").

:- func listing_type = type_desc.
:- pragma foreign_export("C", listing_type = out, "ML_LISTING_listing_type").

listing_type = type_of(Path) :-
    clear_list_path(Path @ [], _).

%---------------------------------------------------------------------------%

new_list_path = [].

%---------------------------------------------------------------------------%

get_list_path(Path) = Path.

set_list_path(Dirs, _, Dirs).

clear_list_path(_, []).

%---------------------------------------------------------------------------%

push_list_path(Dir, Path, [Dir | Path]).

%---------------------------------------------------------------------------%

pop_list_path([],         []).
pop_list_path([_ | Path], Path).

%---------------------------------------------------------------------------%

list_file(OutStrm, ErrStrm, FileName, FirstLine, LastLine, MarkLine, Path,
        !IO) :-
    ( if dir.path_name_is_absolute(FileName) then
        io.open_input(FileName, Result0, !IO),
        (
            Result0 = ok(InStream),
            InStrm = mercury_stream_to_c_file_ptr(InStream),
            print_lines_in_range_c(InStrm, OutStrm, 1, FirstLine, LastLine,
                MarkLine, !IO),
            io.close_input(InStream, !IO)
        ;
            Result0 = error(Error),
            ErrorMsg = io.error_message(Error),
            write_to_c_file(ErrStrm,
                string.format("mdb: cannot open file %s: %s\n",
                    [s(FileName), s(ErrorMsg)]), !IO)
        )
    else
        find_and_open_file([dir.this_directory | Path], FileName, Result, !IO),
        (
            Result = yes(InStream),
            InStrm = mercury_stream_to_c_file_ptr(InStream),
            print_lines_in_range_c(InStrm, OutStrm, 1, FirstLine, LastLine,
                MarkLine, !IO),
            io.close_input(InStream, !IO)
        ;
            Result = no,
            write_to_c_file(ErrStrm,
                string.format("mdb: cannot find file %s\n",
                    [s(FileName)]), !IO)
        )
    ).

:- func mercury_stream_to_c_file_ptr(io.input_stream) = c_file_ptr.

:- pragma foreign_proc("C",
    mercury_stream_to_c_file_ptr(InStream::in) = (InStrm::out),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    InStrm = MR_file(*(MR_unwrap_input_stream(InStream)));
").

:- pred write_to_c_file(c_file_ptr::in, string::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    write_to_c_file(ErrStrm::in, Str::in, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    fputs(Str, (FILE *)ErrStrm);
").

%---------------------------------------------------------------------------%

list_file_portable(OutStrm, ErrStrm, FileName, FirstLine, LastLine,
        MarkLine, Path, !IO) :-
    ( if dir.path_name_is_absolute(FileName) then
        io.open_input(FileName, Result0, !IO),
        (
            Result0 = ok(InStrm),
            print_lines_in_range_m(InStrm, OutStrm, 1, FirstLine, LastLine,
                MarkLine, !IO),
            io.close_input(InStrm, !IO)
        ;
            Result0 = error(Error),
            ErrorMsg = io.error_message(Error),
            io.write_string(ErrStrm, "mdb: cannot open file ", !IO),
            io.write_string(ErrStrm, FileName, !IO),
            io.write_string(ErrStrm, ": ", !IO),
            io.write_string(ErrStrm, ErrorMsg, !IO),
            io.write_string(ErrStrm, "\n", !IO)
        )
    else
        find_and_open_file([dir.this_directory | Path], FileName, Result, !IO),
        (
            Result = yes(InStrm),
            print_lines_in_range_m(InStrm, OutStrm, 1, FirstLine, LastLine,
                MarkLine, !IO),
            io.close_input(InStrm, !IO)
        ;
            Result = no,
            io.write_string(ErrStrm, "mdb: cannot find file ", !IO),
            io.write_string(ErrStrm, FileName, !IO),
            io.write_string(ErrStrm, "\n", !IO)
        )
    ).

%---------------------------------------------------------------------------%

list_file_with_command(OutStrm, ErrStrm, Command, FileName, FirstLine,
        LastLine, MarkLine, Path, !IO) :-
    LineArgs = [string.from_int(FirstLine), string.from_int(LastLine),
        string.from_int(MarkLine)],
    ( if dir.path_name_is_absolute(FileName) then
        FindResult = yes(FileName)
    else
        find_file([dir.this_directory | Path], FileName, FindResult, !IO)
    ),
    (
        FindResult = yes(FoundFileName),
        execute_command_with_redirects(Command, [FoundFileName | LineArgs],
            OutStrm, ErrStrm, CallResult, !IO),
        (
            CallResult = ok
        ;
            CallResult = error(Error),
            write_to_c_file(ErrStrm,
                string.format("mdb: %s: %s\n", [s(Command), s(Error)]), !IO)
        )
    ;
        FindResult = no,
        write_to_c_file(ErrStrm,
            string.format("mdb: cannot find file %s\n", [s(FileName)]), !IO)
    ).

%---------------------------------------------------------------------------%

    % Search for the first file with the given name on the search path
    % that we can open for reading and return the complete file name
    % (including the path component) and input stream handle.
    %
:- pred find_and_open_file(search_path::in, file_name::in,
    maybe(io.input_stream)::out, io::di, io::uo) is det.

find_and_open_file([], _, no, !IO).
find_and_open_file([Dir | Path], FileName, Result, !IO) :-
    io.open_input(Dir / FileName, Result0, !IO),
    (
        Result0 = ok(InStream),
        Result  = yes(InStream)
    ;
        Result0 = error(_),
        find_and_open_file(Path, FileName, Result, !IO)
    ).

:- pred find_file(search_path::in, file_name::in, maybe(file_name)::out,
    io::di, io::uo) is det.

find_file([], _, no, !IO).
find_file([Dir | Path], FileName0, Result, !IO) :-
    FileName = Dir / FileName0,
    FollowSymLinks = yes,
    io.file_type(FollowSymLinks, FileName, FileTypeRes, !IO),
    (
        FileTypeRes = ok(FileType),
        (
            ( FileType = regular_file
            ; FileType = symbolic_link
            ; FileType = named_pipe
            ; FileType = socket
            ; FileType = character_device
            ; FileType = block_device
            ; FileType = message_queue
            ; FileType = semaphore
            ; FileType = shared_memory
            ; FileType = unknown
            ),
            Result = yes(FileName)
        ;
            FileType = directory,
            % It is debatable whether we should continue searching.
            find_file(Path, FileName0, Result, !IO)
        )
    ;
        FileTypeRes = error(_),
        find_file(Path, FileName0, Result, !IO)
    ).

%---------------------------------------------------------------------------%

    % print_lines_in_range(InStrm, OutStrm, ThisLine, FirstLine, LastLine,
    %   MarkLine, !IO):
    %
    % Print the lines numbered FirstLine to LastLine from InStrm
    % on OutStrm (the current line number is taken as ThisLine).
    % Each line is printed indented with "  ", except for the line
    % numbered MarkLine, if it occurs in the range FirstLine .. LastLine,
    % which is indented with "> ".
    %
:- pred print_lines_in_range_c(c_file_ptr::in, c_file_ptr::in,
    line_no::in, line_no::in, line_no::in, line_no::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    print_lines_in_range_c(InStrm::in, OutStrm::in, ThisLine::in,
        FirstLine::in, LastLine::in, MarkLine::in, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    if (FirstLine <= ThisLine && ThisLine <= LastLine) {
        const char *s = (ThisLine == MarkLine) ? \"> \" : \"  \";
        fputs(s, (FILE *)OutStrm);
    }
    while(ThisLine <= LastLine) {
        int c = fgetc((FILE *)InStrm);
        if (c == EOF) {
            fputc('\\n', (FILE *)OutStrm);
            break;
        }
        if (FirstLine <= ThisLine) {
            fputc(c, (FILE *)OutStrm);
        }
        if (c == '\\n') {
            ThisLine++;
            if (FirstLine <= ThisLine && ThisLine <= LastLine)  {
                const char *s = (ThisLine == MarkLine) ? \"> \" : \"  \";
                fputs(s, (FILE *)OutStrm);
            }
        }
    }
").

%---------------------------------------------------------------------------%

:- pred print_lines_in_range_m(io.input_stream::in, io.output_stream::in,
    line_no::in, line_no::in, line_no::in, line_no::in, io::di, io::uo) is det.

print_lines_in_range_m(InStrm, OutStrm, ThisLine, FirstLine, LastLine,
        MarkLine, !IO) :-
    io.read_line_as_string(InStrm, Res, !IO),
    (
        Res = ok(Line),
        ( if FirstLine =< ThisLine, ThisLine =< LastLine then
            ( if ThisLine = MarkLine then
                io.write_string(OutStrm, "> ", !IO)
            else
                io.write_string(OutStrm, "  ", !IO)
            ),
            io.write_string(OutStrm, Line, !IO)
        else
            true
        ),
        print_lines_in_range_m(InStrm, OutStrm, ThisLine + 1, FirstLine,
            LastLine, MarkLine, !IO)
    ;
        Res = eof
    ;
        Res = error(Error),
        io.write_string(OutStrm, "Error: ", !IO),
        io.write_string(OutStrm, io.error_message(Error), !IO),
        io.write_string(OutStrm, "\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred execute_command_with_redirects(string::in, list(string)::in,
    c_file_ptr::in, c_file_ptr::in, maybe_error::out, io::di, io::uo) is det.

execute_command_with_redirects(Prog, Args, OutStrm, ErrStrm, Result, !IO) :-
    do_posix_spawnp(Prog, length(Args), Args, OutStrm, ErrStrm, Status, Error0,
        !IO),
    ( if Status = -1 then
        io.make_err_msg(Error0, "error invoking system command: ", Message,
            !IO),
        Result = error(Message)
    else if Status = -2 then
        Result = error("posix_spawn not supported on this platform")
    else
        Result0 = io.decode_system_command_exit_code(Status),
        (
            Result0 = ok(exited(ExitStatus)),
            ( if ExitStatus = 0 then
                Result = ok
            else
                Result = error("exit status " ++ string.from_int(ExitStatus))
            )
        ;
            Result0 = ok(signalled(Signal)),
            Result = error("received signal " ++ string.from_int(Signal))
        ;
            Result0 = error(Error),
            Result = error(io.error_message(Error))
        )
    ).

:- pred do_posix_spawnp(string::in, int::in, list(string)::in,
    c_file_ptr::in, c_file_ptr::in, int::out, io.system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_posix_spawnp(Prog::in, NumArgs::in, Args::in, OutStrm::in, ErrStrm::in,
        Status::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    int error;

    Status = do_posix_spawnp(Prog, NumArgs, Args,
        fileno(OutStrm), fileno(ErrStrm), &error);
    if (Status == -1) {
        Error = error;
    } else {
        Error = 0;
    }
").

:- pragma foreign_decl("C", local, "
// See library/io.m regarding declaration of the environ global variable.
#if defined(MR_HAVE_SPAWN_H) && defined(MR_HAVE_ENVIRON)
    #include <spawn.h>

    #if defined(MR_MAC_OSX)
        #include <crt_externs.h>
    #else
        extern char **environ;
    #endif
#endif

static int do_posix_spawnp(MR_String prog, int num_args, MR_Word args,
    const int outfd, const int errfd, int *ret_errno);
").

:- pragma foreign_code("C", "
static int
do_posix_spawnp(MR_String prog, int num_args, MR_Word args,
    const int outfd, const int errfd, int *ret_errno)
{
#if defined(MR_HAVE_POSIX_SPAWN) && defined(MR_HAVE_ENVIRON)

    pid_t   pid;
    char    **argv;
    posix_spawn_file_actions_t file_actions;
    posix_spawnattr_t attr;
    int     rc;
    int     status;
    int     i;

    argv = MR_GC_NEW_ARRAY(char *, 1 + num_args + 1);
    argv[0] = prog;
    for (i = 1; i <= num_args; i++) {
        argv[i] = (MR_String) MR_list_head(args);
        args = MR_list_tail(args);
    }
    argv[i] = NULL;

    rc = posix_spawnattr_init(&attr);
    if (rc == -1) {
        *ret_errno = errno;
        return -1;
    }

    rc = posix_spawn_file_actions_init(&file_actions);
    if (rc == -1) {
        *ret_errno = errno;
        goto error_cleanup_attr;
    }

    if (outfd != STDOUT_FILENO) {
        // Redirect standard output in child process.
        rc = posix_spawn_file_actions_adddup2(&file_actions,
            outfd, STDOUT_FILENO);
        if (rc == -1) {
            *ret_errno = errno;
            goto error_cleanup_fa_attr;
        }
        // Close outfd in child process.
        rc = posix_spawn_file_actions_addclose(&file_actions, outfd);
        if (rc == -1) {
            *ret_errno = errno;
            goto error_cleanup_fa_attr;
        }
    }

    if (errfd != STDERR_FILENO) {
        // Redirect standard error in child process.
        rc = posix_spawn_file_actions_adddup2(&file_actions,
            errfd, STDERR_FILENO);
        if (rc == -1) {
            *ret_errno = errno;
            goto error_cleanup_fa_attr;
        }
        // Close errfd in child process.
        rc = posix_spawn_file_actions_addclose(&file_actions, errfd);
        if (rc == -1) {
            *ret_errno = errno;
            goto error_cleanup_fa_attr;
        }
    }

    #ifdef MR_MAC_OSX
        rc = posix_spawnp(&pid, prog, &file_actions, &attr, argv,
            *_NSGetEnviron());
    #else
        rc = posix_spawnp(&pid, prog, &file_actions, &attr, argv,
            environ);
    #endif

    if (rc == -1) {
        // Spawn failed.
        *ret_errno = errno;
        goto error_cleanup_fa_attr;
    }

    posix_spawnattr_destroy(&attr);
    posix_spawn_file_actions_destroy(&file_actions);

    // Wait for the spawned process to exit.
    do {
        rc = waitpid(pid, &status, 0);
    } while (rc == -1 && MR_is_eintr(errno));
    if (rc == -1) {
        *ret_errno = errno;
        return -1;
    }
    *ret_errno = 0;
    return status;

error_cleanup_fa_attr:
    posix_spawn_file_actions_destroy(&file_actions);
error_cleanup_attr:
    posix_spawnattr_destroy(&attr);
    return -1;

#else   // not (defined(MR_HAVE_POSIX_SPAWN) && defined(MR_HAVE_ENVIRON))

    *ret_errno = ENOEXEC;
    return -2;

#endif  // not (defined(MR_HAVE_POSIX_SPAWN) && defined(MR_HAVE_ENVIRON))
}
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
