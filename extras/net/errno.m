%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury Team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%-----------------------------------------------------------------------------%
%
% Module: errno.
% Main Author:  Paul Bone <paul@bone.id.au>
% Stability:    low
%
% Provide an interface to the C errno type and functions.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- module net.errno.

:- interface.

:- import_module int.
:- import_module string.

:- type errno == int.

:- func strerror(errno) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
