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
%---------------------------------------------------------------------------%

:- module ll_backend__static_term.

:- interface.

:- import_module ll_backend__llds.

:- import_module std_util.

:- pred static_term__term_to_rval(univ::in, maybe(rval)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops.

:- import_module deconstruct, list, require.

static_term__term_to_rval(Univ, Rval) :-
	( deconstruct__get_functor_info(Univ, FunctorInfo) ->
		static_term__functor_info_to_rval(FunctorInfo, Rval)
	;
		error("static_term__term_to_rval: unexpected kind of term")
	).

:- pred static_term__functor_info_to_rval(functor_tag_info::in,
	maybe(rval)::out) is det.

static_term__functor_info_to_rval(FunctorInfo, MaybeRval) :-
	(
		FunctorInfo = functor_integer(Int),
		MaybeRval = yes(const(int_const(Int)))
	;
		FunctorInfo = functor_float(Float),
		MaybeRval = yes(const(float_const(Float)))
	;
		FunctorInfo = functor_string(String),
		MaybeRval = yes(const(string_const(String)))
	;
		FunctorInfo = functor_enum(Enum),
		MaybeRval = yes(const(int_const(Enum)))
	;
		FunctorInfo = functor_local(Ptag, Sectag),
		MaybeRval = yes(mkword(Ptag,
			unop(mkbody, const(int_const(Sectag)))))
	;
		FunctorInfo = functor_remote(Ptag, Sectag, Args),
		MaybeSectagRval = yes(const(int_const(Sectag))),
		list__map(static_term__term_to_rval, Args, MaybeArgRvals),
		Reuse = no,
		MaybeRval = yes(create(Ptag, [MaybeSectagRval | MaybeArgRvals],
			uniform(no), must_be_static, "static_term", Reuse))
	;
		FunctorInfo = functor_unshared(Ptag, Args),
		list__map(static_term__term_to_rval, Args, MaybeArgRvals),
		Reuse = no,
		MaybeRval = yes(create(Ptag, MaybeArgRvals,
			uniform(no), must_be_static, "static_term", Reuse))
	;
		FunctorInfo = functor_notag(Univ),
		static_term__term_to_rval(Univ, MaybeRval)
	;
		FunctorInfo = functor_equiv(Univ),
		static_term__term_to_rval(Univ, MaybeRval)
	).
