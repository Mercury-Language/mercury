//
// Copyright (C) 2001-2004 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package jmercury.runtime;

public class TypeInfo_Struct extends PseudoTypeInfo {

	public TypeCtorInfo_Struct type_ctor;
	public PseudoTypeInfo args[];

	public TypeInfo_Struct()
	{
	}

	public TypeInfo_Struct(TypeCtorInfo_Struct tc)
	{
		type_ctor = tc;
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

	public void init(TypeCtorInfo_Struct tc, PseudoTypeInfo[] as)
	{
	    type_ctor = tc;
	    args = as;
	}

	public TypeInfo_Struct copy()
	{
		TypeInfo_Struct ti = new TypeInfo_Struct();
		ti.type_ctor = type_ctor;
		if (args != null) {
			ti.args = args.clone();
		}
		return ti;
	}

	// XXX "as" should have type PseudoTypeInfo[],
	//     but mlds_to_java.m uses Object[]
	//     because init_array/1 does not store the type.
	public void init(TypeCtorInfo_Struct tc, int arity, Object[] as)
	{
		assert arity == as.length;

		init(tc, as);
	}

	// XXX "as" should have type PseudoTypeInfo[],
	//     but mlds_to_java.m uses Object[]
	//     because init_array/1 does not store the type.
	public void init(TypeCtorInfo_Struct tc, Object[] as)
	{
		PseudoTypeInfo[] ptis = new PseudoTypeInfo[as.length];
		for (int i = 0; i < as.length; i++) {
			ptis[i] = (PseudoTypeInfo) as[i];
		}
                init(tc, ptis);
	}

	// XXX untested guess
	public TypeInfo_Struct(TypeInfo_Struct ti, int arity, Object... as)
	{
		init(ti.type_ctor, arity, as);
	}

	// XXX untested guess
	public TypeInfo_Struct(TypeInfo_Struct ti, Object... as)
	{
		init(ti.type_ctor, as);
	}

	// XXX a temp hack just to get things to run
	public TypeInfo_Struct(java.lang.Object obj)
	{
		if (obj instanceof TypeInfo_Struct) {
			TypeInfo_Struct ti = (TypeInfo_Struct) obj;
			type_ctor = ti.type_ctor;
			args = ti.args;
		} else {
			throw new java.lang.Error("TypeInfo_Struct(Object)");
		}
	}

		// XXX this should be renamed `equals'
	public boolean unify(TypeInfo_Struct ti) {
		if (this == ti) {
			return true;
		}

		if (type_ctor.unify(ti.type_ctor) == false) {
			return false;
		}

		if (args == null || ti.args == null) {
			if (args == null && ti.args == null) {
				return true;
			}
			return false;
		}

		for (int i = 0; i < args.length || i < ti.args.length; i++) {
			if (i == args.length || i == ti.args.length) {
				return false;
			}
			if (args[i].unify(ti.args[i]) == false) {
				return false;
			}
		}
		return true;
	}
}
