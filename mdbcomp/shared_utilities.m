%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2012 The University of Melbourne.
% Copyright (C) 2014, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This module contains utility predicates that may be useful in several
% directories of the Mercury system.

:- module mdbcomp.shared_utilities.
:- interface.

:- import_module io.

    % When the deep profiler is compiled in hlc grades, on many systems
    % large profiling data files cannot be processed using only the default
    % size of the C stack. Similarly, when the compiler is compiled in hlc
    % grades, it often runs out of stack when compiling large input files.
    %
    % This predicate tells the OS to allow the system stack to grow
    % as large as it needs to, subject only to the hard limits enforced
    % by the system.
    %
    % In llc grades, this predicate has no useful effect. The stack size
    % limit can be lifted in such grades by using their .stseg versions.
    %
:- pred unlimit_stack(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "

// On some systems (e.g. Mac OS X 10.3) RLIMIT_STACK is defined in the
// header sys/resource.h.
#if defined(MR_HAVE_SYS_RESOURCE_H)
    #include <sys/resource.h>
#endif

").

:- pragma foreign_proc("C",
    unlimit_stack(_S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
#if defined(MR_HAVE_SETRLIMIT)
    struct rlimit   limit_struct;
    rlim_t          max_value;
    char            errbuf[MR_STRERROR_BUF_SIZE];

    if (getrlimit(RLIMIT_STACK, &limit_struct) != 0) {
        MR_fatal_error(""could not get current stack limit: %s"",
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }

    max_value = limit_struct.rlim_max;
    limit_struct.rlim_cur = limit_struct.rlim_max;
    /* If this fails, we have no recourse, so ignore any failure. */
    (void) setrlimit(RLIMIT_STACK, &limit_struct);
#endif
").

    % Clause for non-C backends.
    %
unlimit_stack(!IO).

%---------------------------------------------------------------------------%
:- end_module mdbcomp.shared_utilities.
%---------------------------------------------------------------------------%
