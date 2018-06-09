//
// Copyright (C) 2001-2003, 2007 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

//   XXX In the C backend this was a union.  
//   It might (eventually) be better to have derived classes
//   for each of the unions constructors and make them all extend
//   this class (rather like we do with the generated code from the 
//   mercury compiler.  That way we can just use the `instanceof' operator
//   to work out what each instance is.

public class TypeFunctors implements java.io.Serializable {
	public java.lang.Object functors_init;
	// the above field should contain one of the following types:
	public DuFunctorDesc[] functors_du() {
		return (DuFunctorDesc[]) functors_init;
	}
	public EnumFunctorDesc[] functors_enum() {
		return (EnumFunctorDesc[]) functors_init;
	}
	public ForeignEnumFunctorDesc[] functors_foreign_enum() {
		return (ForeignEnumFunctorDesc[]) functors_init;
	}
	public NotagFunctorDesc functors_notag() {
		return (NotagFunctorDesc) functors_init;
	}
	public TypeFunctors(java.lang.Object init) {
		functors_init = init;
	}
}
