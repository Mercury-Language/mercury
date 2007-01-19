%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001,2003-2004, 2006-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: spawn.m.
% Main author: conway.
% Stability: medium.
%
% This module provides `spawn/3' which is the primitive for starting the
% concurrent execution of a goal. The term `concurrent' here is referring
% to threads, not parallel execution, though the latter is possible by
% compiling in one of the *.par grades (e.g. asm_fast.gc.par or hlc.gc.par).
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module spawn.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

    % spawn(Closure, IO0, IO) is true iff IO0 denotes a list of I/O
    % transactions that is an interleaving of those performed by `Closure'
    % and those contained in IO - the list of transactions performed by
    % the continuation of spawn.
    %
    % NOTE: this predicate is obsolete.  New code should use the 
    % standard library's version: thread.spawn/3.
    %
:- pragma obsolete(spawn.spawn/3).
:- pred spawn(pred(io, io), io, io).
:- mode spawn(pred(di, uo) is cc_multi, di, uo) is cc_multi.

    % yield(IO0, IO) is logically equivalent to (IO = IO0) but
    % operationally, yields the mercury engine to some other thread
    % if one exists.
    %
    % NOTE: this is not yet implemented in the hlc.par.gc grade.
    % 
    % NOTE: this predicate is obsolete.  New code should use the 
    % standard library's version: thread.yield/2.
    % 
:- pragma obsolete(spawn.yield/2).
:- pred yield(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module thread.

:- pragma foreign_decl("C", "
#if defined(MR_HIGHLEVEL_CODE) && !defined(MR_THREAD_SAFE)
  #error The spawn module requires either hlc.par.gc grade or a non-hlc grade.
#endif
").

spawn.spawn(Goal, !IO) :-
    thread.spawn(Goal, !IO).

spawn.yield(!IO) :-
    thread.yield(!IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
