%------------------------------------------------------------------------------%
% Copyright (C) 1999, 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix.m
% main author: conway@cs.mu.oz.au
%
% This module (with its submodules) provides a bare interface to the POSIX.3
% operating system interface. It is intended for providing basic functionality
% to other library functionality (such as a user oriented IO facility).
%
% Conventions
% -----------
%
% File descriptors and other `descriptor' like entities are represented by
% distinct types using a single-constructor, single-argument wrapper, which
% is mandated by Mercury to have the same representation as the argument
% (these are sometimes refered to as `notag' types). In most cases these
% are concrete rather than abstract, because the intent for this POSIX
% binding is to provide flexible glue to the C functionality, rather than
% to provide abstractions of the functionality.
%
% Flags are represented by enumerations. Since Mercury doesn't [yet] allow 
% for user assignable values for enumerations, these are mapped onto ints,
% and the ints are used to index static C arrays of the flag constants.
%
%------------------------------------------------------------------------------%
:- module posix.

:- interface.

:- import_module io, int, integer.

:- include_module posix__closedir.
:- include_module posix__dup.
:- include_module posix__exec.
:- include_module posix__fork.
:- include_module posix__getpid.
:- include_module posix__kill.
:- include_module posix__lseek.
:- include_module posix__mkdir.
:- include_module posix__open.
:- include_module posix__opendir.
:- include_module posix__pipe.
:- include_module posix__read.
:- include_module posix__readdir.
:- include_module posix__rmdir.
:- include_module posix__select.
:- include_module posix__socket.
:- include_module posix__stat.
:- include_module posix__wait.
:- include_module posix__write.

	% Generic file descriptors.
:- type fd --->	fd(int).

	% Directory streams.
:- type dir ---> dir(c_pointer).

	% Devices.
:- type dev_t ---> dev(int).

	% File modes.
:- type mode_t ---> mode(int).

	% Inodes.
:- type ino_t ---> ino(int).

	% Link counts.
:- type nlink_t ---> nlink(int).

	% File offsets.
:- type off_t ---> off(integer).

	% Block counts.
:- type blkcnt_t ---> blkcnt(integer).

	% Block size.
:- type blksize_t ---> blksize(int).

	% Process identifiers.
:- type pid_t ---> pid(int).

	% User identifiers.
:- type uid_t ---> uid(int).

	% Group identifiers.
:- type gid_t ---> gid(int).

:- type error
	--->	e2BIG			/* Arg list too long */
	;	eACCES			/* Permission denied */
	;	eAGAIN			/* Try again */
	;	eBADF			/* Bad file number */
	;	eBADMSG			/* Not a data message */
	;	eBUSY			/* Device or resource busy */
	%;	eCANCELED		/* Operation canceled */
	;	eCHILD			/* No child processes */
	;	eDEADLK			/* Resource deadlock would occur */
	;	eDOM			/* Math argument out of domain */
	;	eEXIST			/* File exists */
	;	eFAULT			/* Bad address */
	;	eFBIG			/* File too large */
	;	eINPROGRESS		/* Operation now in progress */
	;	eINTR			/* Interrupted system call */
	;	eINVAL			/* Invalid argument */
	;	eIO			/* I/O error */
	;	eISDIR			/* Is a directory */
	;	eMFILE			/* Too many open files */
	;	eMLINK			/* Too many links */
	;	eMSGSIZE		/* Message too long */
	;	eNAMETOOLONG		/* File name too long */
	;	eNFILE			/* File table overflow */
	;	eNODEV			/* No such device */
	;	eNOENT			/* No such file or directory */
	;	eNOEXEC			/* Exec format error */
	%;	eNOLOCK			/* No locks available */
	;	eNOMEM			/* Out of memory */
	;	eNOSPC			/* No space left on device */
	;	eNOSYS			/* Function not implemented */
	;	eNOTDIR			/* Not a directory */
	;	eNOTEMPTY		/* Directory not empty */
	%;	eNOTSUP			/* Not supported */
	;	eNOTTY			/* Not a typewriter */
	;	eNXIO			/* No such device or address */
	;	ePERM			/* Operation not permitted */
	;	ePIPE			/* Broken pipe */
	;	eRANGE			/* Math result not representable */
	;	eROFS			/* Read-only file system */
	;	eSPIPE			/* Illegal seek */
	;	eSRCH			/* No such process */
	;	eTIMEDOUT		/* Connection timed out */
	;	eXDEV			/* Cross-device link */
	;	unknown(int, string)	% unknown(Errno, Msg)
	.

:- type posix__result
	--->	ok
	;	error(posix__error)
	.

:- type posix__result(T)
	--->	ok(T)
	;	error(posix__error)
	.

:- type (mode) == mode_t. % XXX This is deprecated; please use mode_t.

:- type timeval
	--->	timeval(int, int). % time(Sec, uSec)

:- pred errno(posix__error, io__state, io__state).
:- mode errno(out, di, uo) is det.

:- implementation.

:- import_module require.

:- pragma c_header_code("
	#include <unistd.h>
	#include <errno.h>
").

errno(Error) -->
	errno0(ErrNo),
	{ Error = error(errnumber(ErrNo)) }.

:- pred errno0(int, io__state, io__state).
:- mode errno0(out, di, uo) is det.

:- pragma c_code(errno0(E::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	E = errno;
	IO = IO0;
}").

:- func error(int) = posix__error.

error(Num) = Res :-
	((	Num = 0,	Err = e2BIG
	;	Num = 1,	Err = eACCES
	;	Num = 2,	Err = eAGAIN
	;	Num = 3,	Err = eBADF
	;	Num = 4,	Err = eBADMSG
	;	Num = 5,	Err = eBUSY
	%;	Num = 6,	Err = eCANCELED
	;	Num = 7,	Err = eCHILD
	;	Num = 8,	Err = eDEADLK
	;	Num = 9,	Err = eDOM
	;	Num = 10,	Err = eEXIST
	;	Num = 11,	Err = eFAULT
	;	Num = 12,	Err = eFBIG
	;	Num = 13,	Err = eINPROGRESS
	;	Num = 14,	Err = eINTR
	;	Num = 15,	Err = eINVAL
	;	Num = 16,	Err = eIO
	;	Num = 17,	Err = eISDIR
	;	Num = 18,	Err = eMFILE
	;	Num = 19,	Err = eMLINK
	;	Num = 20,	Err = eMSGSIZE
	;	Num = 21,	Err = eNAMETOOLONG
	;	Num = 22,	Err = eNFILE
	;	Num = 23,	Err = eNODEV
	;	Num = 24,	Err = eNOENT
	;	Num = 25,	Err = eNOEXEC
	%;	Num = 26,	Err = eNOLOCK
	;	Num = 27,	Err = eNOMEM
	;	Num = 28,	Err = eNOSPC
	;	Num = 29,	Err = eNOSYS
	;	Num = 30,	Err = eNOTDIR
	;	Num = 31,	Err = eNOTEMPTY
	%;	Num = 32,	Err = eNOTSUP
	;	Num = 33,	Err = eNOTTY
	;	Num = 34,	Err = eNXIO
	;	Num = 35,	Err = ePERM
	;	Num = 36,	Err = ePIPE
	;	Num = 37,	Err = eRANGE
	;	Num = 38,	Err = eROFS
	;	Num = 39,	Err = eSPIPE
	;	Num = 40,	Err = eSRCH
	;	Num = 41,	Err = eTIMEDOUT
	;	Num = 42,	Err = eXDEV
	) ->
		Res = Err
	;
		Res = unknown(Num, "unknown errno")
	).

:- func errnumber(int) = int.
:- mode (errnumber(in) = out) is det.

:- pragma c_code(errnumber(Er::in) = (En::out),
		[will_not_call_mercury, thread_safe], "{
	switch (Er) {
		case E2BIG:	En = 0;		break;
		case EACCES:	En = 1;		break;
		case EAGAIN:	En = 2;		break;
		case EBADF:	En = 3;		break;
		case EBADMSG:	En = 4;		break;
		case EBUSY:	En = 5;		break;
		/* case ECANCELED:	En = 6;		break; */
		case ECHILD:	En = 7;		break;
		case EDEADLK:	En = 8;		break;
		case EDOM:	En = 9;		break;
		case EEXIST:	En = 10;	break;
		case EFAULT:	En = 11;	break;
		case EFBIG:	En = 12;	break;
		case EINPROGRESS: En = 13;	break;
		case EINTR:	En = 14;	break;
		case EINVAL:	En = 15;	break;
		case EIO:	En = 16;	break;
		case EISDIR:	En = 17;	break;
		case EMFILE:	En = 18;	break;
		case EMLINK:	En = 19;	break;
		case EMSGSIZE:	En = 20;	break;
		case ENAMETOOLONG: En = 21;	break;
		case ENFILE:	En = 22;	break;
		case ENODEV:	En = 23;	break;
		case ENOENT:	En = 24;	break;
		case ENOEXEC:	En = 25;	break;
		/* case ENOLOCK:	En = 26;	break; */
		case ENOMEM:	En = 27;	break;
		case ENOSPC:	En = 28;	break;
		case ENOSYS:	En = 29;	break;
		case ENOTDIR:	En = 30;	break;
		case ENOTEMPTY:	En = 31;	break;
		/* case ENOTSUP:	En = 32;	break; */
		case ENOTTY:	En = 33;	break;
		case ENXIO:	En = 34;	break;
		case EPERM:	En = 35;	break;
		case EPIPE:	En = 36;	break;
		case ERANGE:	En = 37;	break;
		case EROFS:	En = 38;	break;
		case ESPIPE:	En = 39;	break;
		case ESRCH:	En = 40;	break;
		case ETIMEDOUT:	En = 41;	break;
		case EXDEV:	En = 42;	break;
		default:
			En = -1;
	}
}").

