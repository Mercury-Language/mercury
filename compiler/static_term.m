%---------------------------------------------------------------------------%
% Copyright (C) 2000,2002-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: static_term.m.
% Author: zs.
%
% This module handles the conversion of Mercury terms in the compiler
% into rvals we can give to llds_out.m in order to make those terms available
% at runtime in the program being compiled.
%
% XXX At the moment, the constructed term never has term_size slots.
%
%---------------------------------------------------------------------------%

:- module ll_backend__static_term.

:- interface.

:- import_module ll_backend__global_data.
:- import_module ll_backend__llds.

:- import_module std_util.

:- pred static_term__term_to_rval(univ::in, rval::out,
	static_cell_info::in, static_cell_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops.

:- import_module deconstruct, list, require.

static_term__term_to_rval(Univ, Rval, !StaticCellInfo) :-
	( deconstruct__get_functor_info(Univ, FunctorInfo) ->
		static_term__functor_info_to_rval(FunctorInfo, Rval,
			!StaticCellInfo)
	;
		error("static_term__term_to_rval: unexpected kind of term")
	).

:- pred static_term__functor_info_to_rval(functor_tag_info::in,
	rval::out, static_cell_info::in, static_cell_info::out) is det.

static_term__functor_info_to_rval(FunctorInfo, Rval, !StaticCellInfo) :-
	(
		FunctorInfo = functor_integer(Int),
		Rval = const(int_const(Int))
	;
		FunctorInfo = functor_float(Float),
		Rval = const(float_const(Float))
	;
		FunctorInfo = functor_string(String),
		Rval = const(string_const(String))
	;
		FunctorInfo = functor_enum(Enum),
		Rval = const(int_const(Enum))
	;
		FunctorInfo = functor_local(Ptag, Sectag),
		Rval = mkword(Ptag,
			unop(mkbody, const(int_const(Sectag))))
	;
		FunctorInfo = functor_remote(Ptag, Sectag, Args),
		SectagRval = const(int_const(Sectag)),
		list__map_foldl(static_term__term_to_rval, Args, ArgRvals,
			!StaticCellInfo),
		add_static_cell_natural_types([SectagRval | ArgRvals],
			DataAddr, !StaticCellInfo),
		Rval = mkword(Ptag, const(data_addr_const(DataAddr, no)))
	;
		FunctorInfo = functor_unshared(Ptag, Args),
		list__map_foldl(static_term__term_to_rval, Args, ArgRvals,
			!StaticCellInfo),
		add_static_cell_natural_types(ArgRvals, DataAddr,
			!StaticCellInfo),
		Rval = mkword(Ptag, const(data_addr_const(DataAddr, no)))
	;
		FunctorInfo = functor_notag(Univ),
		static_term__term_to_rval(Univ, Rval, !StaticCellInfo)
	;
		FunctorInfo = functor_equiv(Univ),
		static_term__term_to_rval(Univ, Rval, !StaticCellInfo)
	).
