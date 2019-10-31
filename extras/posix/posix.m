%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999, 2001 The University of Melbourne.
% Copyright (C) 2018-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.m
% Main author: conway@cs.mu.oz.au
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
% (these are sometimes referred to as `notag' types). In most cases these
% are concrete rather than abstract, because the intent for this POSIX
% binding is to provide flexible glue to the C functionality, rather than
% to provide abstractions of the functionality.
%
% Flags are represented by enumerations. Since Mercury doesn't [yet] allow
% for user assignable values for enumerations, these are mapped onto ints,
% and the ints are used to index static C arrays of the flag constants.
%
%-----------------------------------------------------------------------------%

:- module posix.
:- interface.

:- import_module int.
:- import_module integer.
:- import_module io.

:- include_module posix.closedir.
:- include_module posix.dup.
:- include_module posix.exec.
:- include_module posix.fork.
:- include_module posix.getpid.
:- include_module posix.kill.
:- include_module posix.lseek.
:- include_module posix.mkdir.
:- include_module posix.open.
:- include_module posix.opendir.
:- include_module posix.pipe.
:- include_module posix.read.
:- include_module posix.readdir.
:- include_module posix.realpath.
:- include_module posix.rmdir.
:- include_module posix.select.
:- include_module posix.socket.
:- include_module posix.stat.
:- include_module posix.wait.
:- include_module posix.write.
:- include_module posix.strerror.

%-----------------------------------------------------------------------------%

    % Generic file descriptors.
    %
:- type fd ---> fd(int).

    % Directory streams.
    %
:- type dir.

    % Devices.
    %
:- type dev_t ---> dev(int).

    % File modes.
    %
:- type mode_t ---> mode(int).

    % Inodes.
    %
:- type ino_t ---> ino(int).

    % Link counts.
    %
:- type nlink_t ---> nlink(int).

    % File offsets.
    %
:- type off_t ---> off(integer).

    % Block counts.
    %
:- type blkcnt_t ---> blkcnt(integer).

    % Block size.
    %
:- type blksize_t ---> blksize(int).

    % Process identifiers.
    %
:- type pid_t ---> pid(int).

    % User identifiers.
    %
:- type uid_t ---> uid(int).

    % Group identifiers.
    %
:- type gid_t ---> gid(int).

:- type error
    --->    e2BIG           /* Arg list too long */
    ;       eACCES          /* Permission denied */
    ;       eAGAIN          /* Try again */
    ;       eBADF           /* Bad file number */
    ;       eBADMSG         /* Not a data message */
    ;       eBUSY           /* Device or resource busy */
    %;      eCANCELED       /* Operation canceled */
    ;       eCHILD          /* No child processes */
    ;       eDEADLK         /* Resource deadlock would occur */
    ;       eDOM            /* Math argument out of domain */
    ;       eEXIST          /* File exists */
    ;       eFAULT          /* Bad address */
    ;       eFBIG           /* File too large */
    ;       eINPROGRESS     /* Operation now in progress */
    ;       eINTR           /* Interrupted system call */
    ;       eINVAL          /* Invalid argument */
    ;       eIO             /* I/O error */
    ;       eISDIR          /* Is a directory */
    ;       eMFILE          /* Too many open files */
    ;       eMLINK          /* Too many links */
    ;       eMSGSIZE        /* Message too long */
    ;       eNAMETOOLONG    /* File name too long */
    ;       eNFILE          /* File table overflow */
    ;       eNODEV          /* No such device */
    ;       eNOENT          /* No such file or directory */
    ;       eNOEXEC         /* Exec format error */
    %;      eNOLOCK         /* No locks available */
    ;       eNOMEM          /* Out of memory */
    ;       eNOSPC          /* No space left on device */
    ;       eNOSYS          /* Function not implemented */
    ;       eNOTDIR         /* Not a directory */
    ;       eNOTEMPTY       /* Directory not empty */
    %;      eNOTSUP         /* Not supported */
    ;       eNOTTY          /* Not a typewriter */
    ;       eNXIO           /* No such device or address */
    ;       ePERM           /* Operation not permitted */
    ;       ePIPE           /* Broken pipe */
    ;       eRANGE          /* Math result not representable */
    ;       eROFS           /* Read-only file system */
    ;       eSPIPE          /* Illegal seek */
    ;       eSRCH           /* No such process */
    ;       eTIMEDOUT       /* Connection timed out */
    ;       eXDEV           /* Cross-device link */
    ;       unknown(int, string)    % unknown(Errno, Msg)
    .

:- type posix.result
    --->    ok
    ;       error(posix.error).

:- type posix.result(T)
    --->    ok(T)
    ;       error(posix.error).

:- type timeval
    --->    timeval(int, int). % time(Sec, uSec)

%-----------------------------------------------------------------------------%

:- pred errno(posix.error::out, io::di, io::uo) is det.

    % error_to_cerrno(Error, CError):
    % CError is the error number corresponding to Error, or -1 if Error is
    % unknown_error/2.
    %
:- pred error_to_cerrno(posix.error::in, int::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- pragma foreign_decl("C", "
    #include <unistd.h>
    #include <errno.h>
").

%-----------------------------------------------------------------------------%

:- interface.

:- pragma foreign_type("C", dir, "DIR *", [can_pass_as_mercury_type]).

%-----------------------------------------------------------------------------%

:- implementation.

errno(Error, !IO) :-
    errno0(ErrNo, !IO),
    Error = num_to_error(cerrno_to_num(ErrNo)).

:- pred errno0(int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    errno0(E::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    E = errno;
").

:- func num_to_error(int) = posix.error.

num_to_error(Num) = Err :-
    ( if num_error(Num, Err0) then
        Err = Err0
    else
        Err = unknown(Num, "unknown errno")
    ).

:- func error_to_num(posix.error) = int.

error_to_num(Err) = Num :-
    ( if num_error(Num0, Err) then
        Num = Num0
    else
        Num = (-1)
    ).

:- pred num_error(int, posix.error).
:- mode num_error(in, out) is semidet.
:- mode num_error(out, in) is semidet.

num_error(Num, Err) :-
    (   Num = 0,    Err = e2BIG
    ;   Num = 1,    Err = eACCES
    ;   Num = 2,    Err = eAGAIN
    ;   Num = 3,    Err = eBADF
    ;   Num = 4,    Err = eBADMSG
    ;   Num = 5,    Err = eBUSY
    %;  Num = 6,    Err = eCANCELED
    ;   Num = 7,    Err = eCHILD
    ;   Num = 8,    Err = eDEADLK
    ;   Num = 9,    Err = eDOM
    ;   Num = 10,   Err = eEXIST
    ;   Num = 11,   Err = eFAULT
    ;   Num = 12,   Err = eFBIG
    ;   Num = 13,   Err = eINPROGRESS
    ;   Num = 14,   Err = eINTR
    ;   Num = 15,   Err = eINVAL
    ;   Num = 16,   Err = eIO
    ;   Num = 17,   Err = eISDIR
    ;   Num = 18,   Err = eMFILE
    ;   Num = 19,   Err = eMLINK
    ;   Num = 20,   Err = eMSGSIZE
    ;   Num = 21,   Err = eNAMETOOLONG
    ;   Num = 22,   Err = eNFILE
    ;   Num = 23,   Err = eNODEV
    ;   Num = 24,   Err = eNOENT
    ;   Num = 25,   Err = eNOEXEC
    %;  Num = 26,   Err = eNOLOCK
    ;   Num = 27,   Err = eNOMEM
    ;   Num = 28,   Err = eNOSPC
    ;   Num = 29,   Err = eNOSYS
    ;   Num = 30,   Err = eNOTDIR
    ;   Num = 31,   Err = eNOTEMPTY
    %;  Num = 32,   Err = eNOTSUP
    ;   Num = 33,   Err = eNOTTY
    ;   Num = 34,   Err = eNXIO
    ;   Num = 35,   Err = ePERM
    ;   Num = 36,   Err = ePIPE
    ;   Num = 37,   Err = eRANGE
    ;   Num = 38,   Err = eROFS
    ;   Num = 39,   Err = eSPIPE
    ;   Num = 40,   Err = eSRCH
    ;   Num = 41,   Err = eTIMEDOUT
    ;   Num = 42,   Err = eXDEV
    ).

:- func cerrno_to_num(int) = int.

:- pragma foreign_proc("C",
    cerrno_to_num(Er::in) = (En::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    switch (Er) {
        case E2BIG:     En = 0;     break;
        case EACCES:    En = 1;     break;
        case EAGAIN:    En = 2;     break;
        case EBADF:     En = 3;     break;
        case EBADMSG:   En = 4;     break;
        case EBUSY:     En = 5;     break;
        /* case ECANCELED:  En = 6;     break; */
        case ECHILD:    En = 7;     break;
        case EDEADLK:   En = 8;     break;
        case EDOM:      En = 9;     break;
        case EEXIST:    En = 10;    break;
        case EFAULT:    En = 11;    break;
        case EFBIG:     En = 12;    break;
        case EINPROGRESS: En = 13;  break;
        case EINTR:     En = 14;    break;
        case EINVAL:    En = 15;    break;
        case EIO:       En = 16;    break;
        case EISDIR:    En = 17;    break;
        case EMFILE:    En = 18;    break;
        case EMLINK:    En = 19;    break;
        case EMSGSIZE:  En = 20;    break;
        case ENAMETOOLONG: En = 21; break;
        case ENFILE:    En = 22;    break;
        case ENODEV:    En = 23;    break;
        case ENOENT:    En = 24;    break;
        case ENOEXEC:   En = 25;    break;
        /* case ENOLOCK:    En = 26;    break; */
        case ENOMEM:    En = 27;    break;
        case ENOSPC:    En = 28;    break;
        case ENOSYS:    En = 29;    break;
        case ENOTDIR:   En = 30;    break;
        case ENOTEMPTY: En = 31;    break;
        /* case ENOTSUP:    En = 32;    break; */
        case ENOTTY:    En = 33;    break;
        case ENXIO:     En = 34;    break;
        case EPERM:     En = 35;    break;
        case EPIPE:     En = 36;    break;
        case ERANGE:    En = 37;    break;
        case EROFS:     En = 38;    break;
        case ESPIPE:    En = 39;    break;
        case ESRCH:     En = 40;    break;
        case ETIMEDOUT: En = 41;    break;
        case EXDEV:     En = 42;    break;
        default:
            En = -1;
    }
").

:- func num_to_cerrno(int) = int.

:- pragma foreign_proc("C",
    num_to_cerrno(En::in) = (Er::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    switch (En) {
        case  0: Er = E2BIG;        break;
        case  1: Er = EACCES;       break;
        case  2: Er = EAGAIN;       break;
        case  3: Er = EBADF;        break;
        case  4: Er = EBADMSG;      break;
        case  5: Er = EBUSY;        break;
     /* case  6: Er = ECANCELED;    break; */
        case  7: Er = ECHILD;       break;
        case  8: Er = EDEADLK;      break;
        case  9: Er = EDOM;         break;
        case 10: Er = EEXIST;       break;
        case 11: Er = EFAULT;       break;
        case 12: Er = EFBIG;        break;
        case 13: Er = EINPROGRESS;  break;
        case 14: Er = EINTR;        break;
        case 15: Er = EINVAL;       break;
        case 16: Er = EIO;          break;
        case 17: Er = EISDIR;       break;
        case 18: Er = EMFILE;       break;
        case 19: Er = EMLINK;       break;
        case 20: Er = EMSGSIZE;     break;
        case 21: Er = ENAMETOOLONG; break;
        case 22: Er = ENFILE;       break;
        case 23: Er = ENODEV;       break;
        case 24: Er = ENOENT;       break;
        case 25: Er = ENOEXEC;      break;
     /* case 26: Er = ENOLOCK;      break; */
        case 27: Er = ENOMEM;       break;
        case 28: Er = ENOSPC;       break;
        case 29: Er = ENOSYS;       break;
        case 30: Er = ENOTDIR;      break;
        case 31: Er = ENOTEMPTY;    break;
     /* case 32: Er = ENOTSUP;      break; */
        case 33: Er = ENOTTY;       break;
        case 34: Er = ENXIO;        break;
        case 35: Er = EPERM;        break;
        case 36: Er = EPIPE;        break;
        case 37: Er = ERANGE;       break;
        case 38: Er = EROFS;        break;
        case 39: Er = ESPIPE;       break;
        case 40: Er = ESRCH;        break;
        case 41: Er = ETIMEDOUT;    break;
        case 42: Er = EXDEV;        break;
        default:
            Er = -1;
    }
").

error_to_cerrno(Errno, CErrno) :-
    CErrno = num_to_cerrno(error_to_num(Errno)).

%-----------------------------------------------------------------------------%
:- end_module posix.
%-----------------------------------------------------------------------------%
