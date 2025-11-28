%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: module_qual.m.
% Main authors: stayl, fjh.
%
% The code in this package performs two tasks.
%
% - It checks for undefined types, typeclasses, insts and modes.
%
% - It module qualifies types, typeclasses, insts and modes within declaration
%   items in the source code of the compilation unit. The heads of all
%   declarations should be module qualified as they are read in by the parser;
%   this module qualifies the bodies of those declarations.
%
%   Note that we don't qualify the parts of the augmented compilation unit
%   that derive from other modules' interface or optimization files, since
%   those parts should be read in fully module qualified already.
%
% The algorithm we use does two passes over all the items in the compilation
% unit. The first pass records the set of modules, types, typeclasses, insts
% and modes that are visible in the compilation unit. The second uses this
% information to actually do this package's job.
%
% If any type, typeclass, inst or mode used in the module is not uniquely
% module qualifiable, i.e. if we either find zero matches for it, or we find
% two or more matches for it, we can generate an error message for it.
% We do so when we are module qualifying a compilation unit; we don't when
% we are qualifying the contents of an interface file.
%
% Note that this package is NOT the only place in the compiler that does module
% qualification. The modes of lambda expressions are qualified in modes.m,
% and predicate and function names are qualified during typecheck, with
% the results recorded during the post_typecheck phase of the purity pass.
% This is because figuring out whether e.g. a call to predicate `p' calls
% module m1's predicate p or module m2's predicate p may require knowing
% the types of the arguments in the call.
%
% Since this package does not and cannot know about the module qualification
% of predicate names, function names and function symbols, it cannot figure out
% which modules are referred to in goals. The only goals that may appear
% in the interface section of a module are in promise declarations.
% If a promise goal contains any unqualified symbols, the second pass
% leaves the symbol unchanged, but since the eventual actual qualification
% of the symbol could refer to any of the modules imported in the interface,
% we consider them *all* of them to be "used".
%
% For the same reason (we don't know what modules predicate names,
% function names and function symbols in goals may refer to), this package
% cannot implement any equivalent of --warn-unused-interface-imports that would
% report unnecessary imports in the *implementation* section of a module.
%
% If the --warn-unused-imports option is set, then unused_imports.m
% can generate all the warnings we would, but it can generate *more* messages,
% since unlike the code here, it can report that an imported module is unused
% *anywhere* in the module. However, even if --warn-unused-imports *is* set,
% the code in unused_imports.m won't be invoked if we stop compilation
% before its normal invocation time, due to e.g. the op_mode being
% the creation of an interface file. We therefore print warnings about
% unneeded imports in the interface section only if the option controlling
% its operation dictates that the code in unused_imports.m won't be invoked.
%
%---------------------------------------------------------------------------%

:- module parse_tree.module_qual.
:- interface.

:- include_module parse_tree.module_qual.id_set.
:- include_module parse_tree.module_qual.mq_info.
:- include_module parse_tree.module_qual.qualify_items.

:- implementation.

:- include_module parse_tree.module_qual.collect_mq_info.
:- include_module parse_tree.module_qual.qual_errors.

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_qual.
%---------------------------------------------------------------------------%
