%------------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__stat.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__stat.

:- interface.

:- import_module string, time.

:- type file_type
    --->    file
    ;	    directory
    ;	    symbolic_link
    ;	    character_device
    ;	    block_device
    ;	    fifo
    ;	    unknown
    .

:- type stat.

:- func dev(stat) = dev_t.
:- func ino(stat) = ino_t.
:- func mode(stat) = mode_t.
:- func file_type(stat) = file_type.
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

:- pred stat(string, posix__result(stat), io__state, io__state).
:- mode stat(in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- pragma c_header_code("
	#include <sys/types.h>
	#include <sys/stat.h>
	#include <unistd.h>
").

%------------------------------------------------------------------------------%

:- type stat ---> stat(c_pointer).

stat(Path, Result) -->
	stat0(Path, Res, Stat),
	( if { Res = 0 } then
		{ Result = ok(Stat) }
	else
		errno(Err),
		{ Result = error(Err) }
	).				    

:- pred stat0(string, int, stat, io__state, io__state).
:- mode stat0(in, out, out, di, uo) is det.

:- pragma c_code(stat0(Path::in, Res::out, Stat::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	Stat = (MR_Word) MR_GC_NEW(struct stat);
	Res = stat(Path, (struct stat *)Stat);
	IO = IO0;
}").

file_type(Stat) =
	( if is_slnk(Mode) then symbolic_link
	else if is_reg(Mode) then file
	else if is_dir(Mode) then directory
	else if is_chr(Mode) then character_device
	else if is_blk(Mode) then block_device
	else if is_fifo(Mode) then fifo
	else unknown ) :- Mode = Stat ^ (mode).

:- pred is_slnk(mode_t).
:- mode is_slnk(in) is semidet.

:- pragma c_code(is_slnk(Mode::in), [will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = S_ISLNK(Mode); ").

:- pred is_reg(mode_t).
:- mode is_reg(in) is semidet.

:- pragma c_code(is_reg(Mode::in), [will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = S_ISREG(Mode); ").

:- pred is_dir(mode_t).
:- mode is_dir(in) is semidet.

:- pragma c_code(is_dir(Mode::in), [will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = S_ISDIR(Mode); ").

:- pred is_chr(mode_t).
:- mode is_chr(in) is semidet.

:- pragma c_code(is_chr(Mode::in), [will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = S_ISCHR(Mode); ").

:- pred is_blk(mode_t).
:- mode is_blk(in) is semidet.

:- pragma c_code(is_blk(Mode::in), [will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = S_ISBLK(Mode); ").

:- pred is_fifo(mode_t).
:- mode is_fifo(in) is semidet.

:- pragma c_code(is_fifo(Mode::in), [will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = S_ISFIFO(Mode); ").

:- pragma c_code(dev(S::in) = (Dev::out),
	[will_not_call_mercury, thread_safe],
	"Dev = ((struct stat *)S)->st_dev; ").

:- pragma c_code(ino(S::in) = (Ino::out),
	[will_not_call_mercury, thread_safe],
	"Ino = ((struct stat *)S)->st_ino; ").

:- pragma c_code(mode(S::in) = (Mode::out),
	[will_not_call_mercury, thread_safe],
	"Mode = ((struct stat *)S)->st_mode; ").

:- pragma c_code(nlink(S::in) = (Nlink::out),
	[will_not_call_mercury, thread_safe],
	"Nlink = ((struct stat *)S)->st_nlink; ").

:- pragma c_code(uid(S::in) = (Uid::out),
	[will_not_call_mercury, thread_safe],
	"Uid = ((struct stat *)S)->st_uid; ").

:- pragma c_code(gid(S::in) = (Gid::out),
	[will_not_call_mercury, thread_safe],
	"Gid = ((struct stat *)S)->st_gid; ").

:- pragma c_code(rdev(S::in) = (Rdev::out),
	[will_not_call_mercury, thread_safe],
	"Rdev = ((struct stat *)S)->st_rdev; ").

size(S) = off(integer(size0(S))).

:- func size0(stat) = int.

:- pragma c_code(size0(S::in) = (Size::out),
	[will_not_call_mercury, thread_safe],
	"Size = ((struct stat *)S)->st_size; ").

:- pragma c_code(blksize(S::in) = (Blksize::out),
	[will_not_call_mercury, thread_safe],
	"Blksize = ((struct stat *)S)->st_blksize; ").

blocks(S) = blkcnt(integer(blocks0(S))).

:- func blocks0(stat) = int.

:- pragma c_code(blocks0(S::in) = (Blocks::out),
	[will_not_call_mercury, thread_safe],
	"Blocks = ((struct stat *)S)->st_blocks; ").

:- pragma c_code(atime(S::in) = (Atime::out),
	[will_not_call_mercury, thread_safe],
	"Atime = ((struct stat *)S)->st_atime; ").

:- pragma c_code(mtime(S::in) = (Mtime::out),
	[will_not_call_mercury, thread_safe],
	"Mtime = ((struct stat*)S)->st_mtime; ").

:- pragma c_code(ctime(S::in) = (Ctime::out),
	[will_not_call_mercury, thread_safe],
	"Ctime = ((struct stat *)S)->st_ctime; ").

%------------------------------------------------------------------------------%

