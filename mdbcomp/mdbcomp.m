%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2006, 2010-2012 The University of Melbourne.
% Copyright (C) 2014, 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This is the top module of a library that contains information shared
% between the debugger and the compiler. This means mostly the definitions
% of data structures that the compiler generates for the debugger, and
% the predicates that operate on them.
%
% The compiler links in this library in all grades, but links in the browser
% library (module mdb and its submodules) only when debugging is enabled.
% Therefore the modules of the mdbcomp library should avoid importing any
% part of the mdb module.

:- module mdbcomp.
:- interface.

:- pred mdbcomp.version(string::out) is det.

% If you add any modules here, you should update the lists in the following
% places:
%
%   deep_profiler/Mmakefile
%   deep_profiler/.gitignore
%   slice/Mmakefile
%   slice/.gitignore
%
% Also, the definition of mercury_mdbcomp_module/1 below should be updated.

:- include_module builtin_modules.
:- include_module feedback.
:- include_module goal_path.
:- include_module prim_data.
:- include_module program_representation.
:- include_module rtti_access.
:- include_module shared_utilities.
:- include_module slice_and_dice.
:- include_module sym_name.
:- include_module trace_counts.

:- implementation.

% See library/library.m for why we implement this predicate this way.

:- pragma foreign_proc("C",
    mdbcomp.version(Version::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_ConstString version_string;

    version_string = MR_VERSION "", configured for "" MR_FULLARCH;
    /*
    ** Cast away const needed here, because Mercury declares Version
    ** with type MR_String rather than MR_ConstString.
    */
    Version = (MR_String) (MR_Word) version_string;
").

mdbcomp.version("unknown version").

:- pred mercury_mdbcomp_module(string::in) is semidet.

mercury_mdbcomp_module("mdbcomp.builtin_modules").
mercury_mdbcomp_module("mdbcomp").
mercury_mdbcomp_module("mdbcomp.feedback.automatic_parallelism").
mercury_mdbcomp_module("mdbcomp.feedback").
mercury_mdbcomp_module("mdbcomp.goal_path").
mercury_mdbcomp_module("mdbcomp.prim_data").
mercury_mdbcomp_module("mdbcomp.program_representation").
mercury_mdbcomp_module("mdbcomp.rtti_access").
mercury_mdbcomp_module("mdbcomp.shared_utilities").
mercury_mdbcomp_module("mdbcomp.slice_and_dice").
mercury_mdbcomp_module("mdbcomp.sym_name").
mercury_mdbcomp_module("mdbcomp.trace_counts").

%---------------------------------------------------------------------------%
:- end_module mdbcomp.
%---------------------------------------------------------------------------%
