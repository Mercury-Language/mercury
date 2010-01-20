//
// Copyright (C) 2001-2005 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package jmercury.runtime;

public class TypeCtorRep implements java.io.Serializable {
	
	// Constants are in private_builtin.m, named MR_TYPECTOR_REP_*.

	// Instance variable for TypeCtorRep objects.
	
	public int value;

	// Constructor

	public TypeCtorRep(int arg) {
		this.value = arg;
	}
}
	
