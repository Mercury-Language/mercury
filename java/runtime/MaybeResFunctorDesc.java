//
// Copyright (C) 2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class MaybeResFunctorDesc {
	public DuFunctorDesc		maybe_res_du;
	public ReservedAddrFunctorDesc	maybe_res_res;
	public MaybeResFunctorDesc(DuFunctorDesc du, 
		ReservedAddrFunctorDesc res)
	{
		maybe_res_du = du;
		maybe_res_res = res;
	}
}
