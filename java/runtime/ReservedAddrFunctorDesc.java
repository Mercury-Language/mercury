//
// Copyright (C) 2003-2004 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

// This corresponds to the C type MR_ReservedAddrFunctorDesc
// in runtime/mercury_type_info.h.

package mercury.runtime;

public class ReservedAddrFunctorDesc {
	public java.lang.String		ra_functor_name;
	public int			ra_ordinal;
	public java.lang.Object		ra_reserved_addr;
	public ReservedAddrFunctorDesc(java.lang.String functor_name,
		int ordinary, java.lang.Object reserved_addr)
	{
		ra_functor_name = functor_name;
		ra_ordinal = ordinary;
		ra_reserved_addr = reserved_addr;
	}
}
