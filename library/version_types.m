%-----------------------------------------------------------------------------%
% Copyright (C) 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% version_types.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Jul  9 15:31:16 EST 2004
%
% (This module only provides documentation describing general properties
% of version types.)
%
% Version types are efficient pure implementations of typically imperative
% structures, subject to the following caveat: efficient access is only
% guaranteed for the "latest" version of a given structure.  An older version
% incurrs an access cost proportional to the number of its descendants.
%
% For example, if A0 is a version array, and A1 is created by updating A0,
% and A2 is created by updating A1, ..., and An is created by updating An-1,
% then accesses to An cost O(1) (assuming no further versions of the array
% have been created from An), but accesses to A0 cost O(n).
%
% Most of these data structures come with impure, unsafe means to "rewind"
% to an earlier version, restoring that version's O(1) access times, but
% leaving later versions undefined (i.e. only do this if you are discarding
% all later versions of the structure.)
%
% The motivation for using version types is that they are ordinary ground
% structures and do not depend upon uniqueness, while in many circumstances
% offering similar levels of performance.
%
%-----------------------------------------------------------------------------%

:- module version_types.

:- interface.

    % This is just to stop the compiler complaining about this module
    % not defining anything.
    %
:- type prevent_warning_about_empty_interface ---> dummy.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
