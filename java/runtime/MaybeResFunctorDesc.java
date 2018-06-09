//
// Copyright (C) 2003 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

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
