//
// Copyright (C) 2001-2005, 2010 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package jmercury.runtime;

public class TypeCtorRep implements java.io.Serializable {
	
	// Constants are in private_builtin.m, named MR_TYPECTOR_REP_*.
	//
	// XXX We have to duplicate these here for use by TypeInfo_Struct.java
	// In fact, all the values should be moved here, and the compiler
	// should generate references to these constants instead of those in
	// private_builtin.
	public static final int MR_TYPECTOR_REP_EQUIV		= 6;
	public static final int MR_TYPECTOR_REP_EQUIV_GROUND	= 29;

	// Instance variable for TypeCtorRep objects.
	
	public int value;

	// Constructor

	public TypeCtorRep(int arg) {
		this.value = arg;
	}
}
	
