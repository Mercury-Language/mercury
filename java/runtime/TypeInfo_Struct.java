//
// Copyright (C) 2001-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class TypeInfo_Struct extends PseudoTypeInfo {

	public TypeCtorInfo_Struct type_ctor;
	public PseudoTypeInfo args[];
    
	public TypeInfo_Struct(TypeCtorInfo_Struct tc, PseudoTypeInfo[] as)
	{
		type_ctor = tc;
		args = as;
	}

	public TypeInfo_Struct(TypeCtorInfo_Struct tc)
	{
		type_ctor = tc;
		args = null;
	}

	public TypeInfo_Struct(TypeCtorInfo_Struct tc, PseudoTypeInfo a1)
	{
		type_ctor = tc;
		args = new PseudoTypeInfo[] { a1 };
	}

	public TypeInfo_Struct(TypeCtorInfo_Struct tc, PseudoTypeInfo a1,
				PseudoTypeInfo a2)
	{
		type_ctor = tc;
		args = new PseudoTypeInfo[] { a1, a2 };
	}
}
