%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2004, 2007 The University of Melbourne.
% Copyright (C) 2018-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.stat.
% Main author: Michael Day <miked@lendtech.com.au>
%
%-----------------------------------------------------------------------------%

:- module posix.stat.
:- interface.

:- import_module string.
:- import_module time.

%-----------------------------------------------------------------------------%

:- type file_type
    --->    file
    ;       directory
    ;       symbolic_link
    ;       character_device
    ;       block_device
    ;       fifo
    ;       unknown.

:- type stat.

:- func dev(stat) = dev_t.
:- func ino(stat) = ino_t.
:- func mode(stat) = mode_t.
:- func file_type(stat) = posix.stat.file_type.
:- func nlink(stat) = nlink_t.
:- func uid(stat) = uid_t.
:- func gid(stat) = gid_t.
:- func rdev(stat) = dev_t.
:- func size(stat) = off_t.
:- func blksize(stat) = blksize_t.
:- func blocks(stat) = blkcnt_t.
:- func atime(stat) = time_t.
:- func mtime(stat) = time_t.
:- func ctime(stat) = time_t.

:- pred stat(string::in, posix.result(stat)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

:- interface.

:- pragma foreign_type("C", stat, "struct stat *", [can_pass_as_mercury_type]).

%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

stat(Path, Result, !IO) :-
    stat0(Path, Res, Stat, !IO),
    ( if Res = 0 then
        Result = ok(Stat)
    else
        errno(Err, !IO),
        Result = error(Err)
    ).

:- pred stat0(string::in, int::out, stat::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    stat0(Path::in, Res::out, Stat::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    Stat = MR_GC_NEW(struct stat);
    do {
        Res = stat(Path, Stat);
    } while (Res == -1 && MR_is_eintr(errno));
").

%-----------------------------------------------------------------------------%

file_type(Stat) =
    ( if is_slnk(Mode) then symbolic_link
    else if is_reg(Mode) then file
    else if is_dir(Mode) then directory
    else if is_chr(Mode) then character_device
    else if is_blk(Mode) then block_device
    else if is_fifo(Mode) then fifo
    else unknown ) :- Mode = Stat ^ (mode).

:- pred is_slnk(mode_t::in) is semidet.
:- pragma foreign_proc("C",
    is_slnk(Mode::in),
    [promise_pure, will_not_call_mercury, thread_safe],
 "
    SUCCESS_INDICATOR = S_ISLNK(Mode);
 ").

:- pred is_reg(mode_t::in) is semidet.
:- pragma foreign_proc("C",
    is_reg(Mode::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = S_ISREG(Mode);
").

:- pred is_dir(mode_t::in) is semidet.
:- pragma foreign_proc("C",
    is_dir(Mode::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = S_ISDIR(Mode);
").

:- pred is_chr(mode_t::in) is semidet.
:- pragma foreign_proc("C",
    is_chr(Mode::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = S_ISCHR(Mode);
").

:- pred is_blk(mode_t::in) is semidet.
:- pragma foreign_proc("C",
    is_blk(Mode::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = S_ISBLK(Mode);
").

:- pred is_fifo(mode_t::in) is semidet.
:- pragma foreign_proc("C",
    is_fifo(Mode::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = S_ISFIFO(Mode);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    dev(S::in) = (Dev::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Dev = S->st_dev;
").

:- pragma foreign_proc("C",
    ino(S::in) = (Ino::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Ino = S->st_ino;
").

:- pragma foreign_proc("C",
    mode(S::in) = (Mode::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Mode = S->st_mode;
").

:- pragma foreign_proc("C",
    nlink(S::in) = (Nlink::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Nlink = S->st_nlink;
").

:- pragma foreign_proc("C",
    uid(S::in) = (Uid::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Uid = S->st_uid;
").

:- pragma foreign_proc("C",
    gid(S::in) = (Gid::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Gid = S->st_gid;
").

:- pragma foreign_proc("C",
    rdev(S::in) = (Rdev::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Rdev = S->st_rdev;
").

size(S) = off(integer(size0(S))).

:- func size0(stat) = int.
:- pragma foreign_proc("C",
    size0(S::in) = (Size::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Size = S->st_size;
").

:- pragma foreign_proc("C",
    blksize(S::in) = (Blksize::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Blksize = S->st_blksize;
").

blocks(S) = blkcnt(integer(blocks0(S))).

:- func blocks0(stat) = int.
:- pragma foreign_proc("C",
    blocks0(S::in) = (Blocks::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Blocks = S->st_blocks;
").

:- pragma foreign_proc("C",
    atime(S::in) = (Atime::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Atime = S->st_atime;
").

:- pragma foreign_proc("C",
    mtime(S::in) = (Mtime::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Mtime = S->st_mtime;
").

:- pragma foreign_proc("C",
    ctime(S::in) = (Ctime::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Ctime = S->st_ctime;
").

%------------------------------------------------------------------------------%
:- end_module posix.stat.
%------------------------------------------------------------------------------%
