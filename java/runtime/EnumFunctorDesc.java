//
// Copyright (C) 2001-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package jmercury.runtime;

public class EnumFunctorDesc implements java.io.Serializable {
	
	public java.lang.String enum_functor_name;
	public int              enum_functor_ordinal;

	public EnumFunctorDesc() {
	}

	public void init(String name, int ordinal) {
		enum_functor_name = name;
		enum_functor_ordinal = ordinal;
	}

}
