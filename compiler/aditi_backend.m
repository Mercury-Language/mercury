%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% The Aditi back-end
%
:- module aditi_backend.
:- interface.
:- import_module transform_hlds, check_hlds. % are these needed?
:- import_module hlds, parse_tree, libs.

%:- import_module aditi_hlds, aditi_codegen, aditi_rl_out.

%
% Phase 4-rl: Aditi-related HLDS transformations
%
%:- module aditi_hlds.
%   :- interface.
   :- include_module dnf.
   :- include_module magic, magic_util.
   :- include_module context.
%:- end_module aditi_hlds.

%
% The Aditi-RL type itself.
%
:- include_module rl.
:- include_module rl_dump.

%
% Phase 5-rl: The Aditi RL code generator
%
%:- module aditi_codegen.
%   :- interface.
   :- include_module rl_gen.
   :- include_module rl_info.
   :- include_module rl_relops.
%:- end_module aditi_codegen.

%
% Phase 6-rl: Low-level (RL -> RL) optimizations
%
:- include_module rl_opt.
   :- include_module rl_block, rl_analyse, rl_liveness, rl_loop, rl_block_opt.
   :- include_module rl_key, rl_sort, rl_stream.

%
% Phase 7-rl: Emit RL bytecodes.
%
%:- module aditi_rl_out.
   :- include_module rl_out, rl_exprn, rl_code, rl_file.
%:- end_module aditi_rl_out.
       
%-----------------------------------------------------------------------------%

:- implementation.
	% aditi_backend__rl_exprn uses ll_backend__llds
	% and backend_libs__builtin_ops.
:- import_module ll_backend.
:- import_module backend_libs.

:- end_module aditi_backend.

%-----------------------------------------------------------------------------%
