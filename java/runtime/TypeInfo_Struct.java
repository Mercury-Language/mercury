//
// Copyright (C) 2001-2004 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class TypeInfo_Struct extends PseudoTypeInfo {

	public TypeCtorInfo_Struct type_ctor;
	public PseudoTypeInfo args[];
    
    	// raw constructor
	public TypeInfo_Struct(TypeCtorInfo_Struct tc, PseudoTypeInfo[] as)
	{
		type_ctor = tc;
		args = as;
	}

	// copy constructor
	// XXX Rather than invoking this constructor, and allocating a new
	//     type_info object on the heap, we should generate code which
	//     just copies the pointer,
	public TypeInfo_Struct(TypeInfo_Struct ti)
	{
		type_ctor = ti.type_ctor;
		args = ti.args;
	}

	//
	// constructors for fixed-arity type_infos
	//

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

	public TypeInfo_Struct(TypeCtorInfo_Struct tc,
			// XXX "as" should have type PseudoTypeInfo[],
			//     but mlds_to_java.m uses Object[]
			//     because init_array/1 does not store the type.
			Object[] as)
	{
		type_ctor = tc;
		args = new PseudoTypeInfo[as.length];
		for (int i = 0; i < as.length; i++) {
			args[i] = (PseudoTypeInfo) as[i];
		}
	}

	//
	// constructors for variable-arity type_infos (tuple, pred, func)
	//

	public TypeInfo_Struct(TypeCtorInfo_Struct tc, int arity)
	{
		// assert arity == 0;
		type_ctor = tc;
		args = new PseudoTypeInfo[] { };
	}

	public TypeInfo_Struct(TypeCtorInfo_Struct tc, int arity,
			PseudoTypeInfo a1)
	{
		// assert arity == 1;
		type_ctor = tc;
		args = new PseudoTypeInfo[] { a1 };
	}

	public TypeInfo_Struct(TypeCtorInfo_Struct tc, int arity,
			PseudoTypeInfo a1, PseudoTypeInfo a2)
	{
		// assert arity == 2;
		type_ctor = tc;
		args = new PseudoTypeInfo[] { a1, a2 };
	}

	public TypeInfo_Struct(TypeCtorInfo_Struct tc, int arity,
			// XXX "as" should have type PseudoTypeInfo[],
			//     but mlds_to_java.m uses Object[]
			//     because init_array/1 does not store the type.
			Object[] as)
	{
		// assert arity == as.length;
		type_ctor = tc;
		args = new PseudoTypeInfo[as.length];
		for (int i = 0; i < as.length; i++) {
			args[i] = (PseudoTypeInfo) as[i];
		}
	}


	// XXX a temp hack just to get things to run
	public TypeInfo_Struct(java.lang.Object obj)
	{
		try {
			TypeInfo_Struct ti = (TypeInfo_Struct) obj;
			type_ctor = ti.type_ctor;
			args = ti.args;
		} catch (java.lang.Exception e) {
			try {
				TypeCtorInfo_Struct tci =
					(TypeCtorInfo_Struct) obj;
				type_ctor = tci;
				args = null;
			} catch (java.lang.Exception e2) {
				throw new java.lang.Error(
					"TypeInfo_Struct(Object)");
			}
		}
	}
}
