//
// Copyright (C) 2007 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class ForeignEnumFunctorDesc {

	public java.lang.String		foreign_enum_functor_name;
	public int			foreign_enum_functor_ordinal;
	public int			foreign_enum_functor_value;

	public ForeignEnumFunctorDesc() {
	}

	public void init(String name, int ordinal, int value) {
		foreign_enum_functor_name = name;
		foreign_enum_functor_ordinal = ordinal;
		foreign_enum_functor_value = value;
	}
}
