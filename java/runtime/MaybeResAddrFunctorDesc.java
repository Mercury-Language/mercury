//
// Copyright (C) 2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class MaybeResAddrFunctorDesc {
	public java.lang.String		maybe_res_name;
	public int			maybe_res_arity;
	public boolean			maybe_res_is_res;
	public MaybeResFunctorDesc	maybe_res_ptr;
	public MaybeResAddrFunctorDesc(java.lang.String name, int arity,
		boolean is_res, MaybeResFunctorDesc ptr)
	{
		maybe_res_name = name;
		maybe_res_arity = arity;
		maybe_res_is_res = is_res;
		maybe_res_ptr = ptr;
	}
}
