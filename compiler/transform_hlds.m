%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%          
% transform_hlds: High-level transformations
%	that are independent of the choice of back-end
%	(the "middle" HLDS pass).
%  

:- module transform_hlds.
:- interface.
:- import_module check_hlds. % is this needed?
:- import_module hlds, parse_tree, libs.

%-----------------------------------------------------------------------------%

:- include_module intermod, trans_opt.

:- include_module dependency_graph.	% XXX imports llds (for profiling labels)

:- include_module table_gen.

:- include_module (lambda).

:- include_module termination.
   :- include_module term_pass1, term_pass2, term_traversal, term_errors.
   :- include_module term_util.
   :- include_module lp. % this could alternatively go in the `libs' module

% Optimizations (HLDS -> HLDS)
:- include_module higher_order.
:- include_module inlining.
:- include_module deforest.
   :- include_module pd_cost, pd_debug, pd_info, pd_term.
   :- include_module pd_util.
:- include_module delay_construct.
:- include_module unused_args.
:- include_module unneeded_code.
:- include_module accumulator.
   :- include_module goal_store.
:- include_module dead_proc_elim.
:- include_module const_prop.

% XXX The following modules are all currently unused.
:- include_module constraint, transform.
:- include_module lco.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module ll_backend. % XXX for code_util, code_aux
:- import_module backend_libs. % XXX for rtti

:- end_module transform_hlds.

%-----------------------------------------------------------------------------%
