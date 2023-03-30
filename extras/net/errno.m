%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014, 2018 The Mercury Team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Module: errno.
% Main Author:  Paul Bone <paul@bone.id.au>
% Stability:    low
%
% Provide an interface to the C errno type and functions.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- module net.errno.

:- interface.

:- import_module int.
:- import_module string.

:- type errno == int.

:- func strerror(errno) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "#include \"mercury_runtime_util.h\"").

%---------------------------------------------------------------------------%

    % Errno handling.
    %
strerror(Errno) = String :-
    strerror(Errno, String).

:- pred strerror(int::in, string::uo) is det.

:- pragma foreign_proc("C",
    strerror(Errno::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    char errbuf[MR_STRERROR_BUF_SIZE];

    MR_make_aligned_string_copy(Str,
        MR_strerror(Errno, errbuf, sizeof(errbuf)));
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
