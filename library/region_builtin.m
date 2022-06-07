%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2009-2010 The University of Melbourne.
% Copyright (C) 2014, 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: region_builtin.m.
% Main authors: qph, juliensf.
% Stability: low.
%
% This file is automatically imported into every module when region-based
% memory management is enabled.  It contains support predicates used for
% RBMM.
%
% This module is a private part of the Mercury implementation; user modules
% should never explicitly import this module. The interface for this module
% does not get included in the Mercury library reference manual.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module region_builtin.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

    % A pointer to a memory region.
    %
:- type region.

    % Create a new region.
    %
:- impure pred create_region(region::out) is det.

    % Remove a region.
    %
:- impure pred remove_region(region::in) is det.

:- pred print_rbmm_profiling_info(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "#include \"mercury_region.h\"").

:- pragma foreign_type("C", region, "MR_RegionHeader *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C#", region, "object"). % dummy

:- pragma foreign_type("Java", region, "java.lang.Object"). % dummy

:- pragma foreign_proc("C",
    create_region(Region::out),
    [will_not_call_mercury],
"
#ifdef MR_USE_REGIONS
    Region = MR_region_create_region();
#else
    MR_fatal_error(\"create_region: non-rbmm grade\");
#endif
").

:- pragma foreign_proc("C",
    remove_region(Region::in),
    [will_not_call_mercury],
"
#ifdef MR_USE_REGIONS
    MR_region_remove_region(Region);
#else
    MR_fatal_error(\"remove_region: non-rbmm grade\");
#endif
").

:- pragma foreign_proc("C",
    print_rbmm_profiling_info(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
#ifdef MR_USE_REGIONS
    MR_region_print_profiling_info();
#else
    printf(
        \"There is no rbmm profiling info to print in a non-rbmm grade.\\n\");
#endif
").

%---------------------------------------------------------------------------%
:- end_module region_builtin.
%---------------------------------------------------------------------------%
