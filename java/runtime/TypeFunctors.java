//
// Copyright (C) 2001-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

//   XXX In the C backend this was a union.  
//   It might (eventually) be better to have derived classes
//   for each of the unions constructors and make them all extend
//   this class (rather like we do with the generated code from the 
//   mercury compiler.  That way we can just use the `instanceof' operator
//   to work out what each instance is.

public class TypeFunctors {
	public java.lang.Object functors_init;
	// the above field should contain one of the following types:
	public mercury.runtime.DuFunctorDesc[] functors_du() {
		return (mercury.runtime.DuFunctorDesc[]) functors_init;
	}
	public mercury.runtime.EnumFunctorDesc[] functors_enum() {
		return (mercury.runtime.EnumFunctorDesc[]) functors_init;
	}
	public mercury.runtime.NotagFunctorDesc functors_notag() {
		return (mercury.runtime.NotagFunctorDesc) functors_init;
	}
	public TypeFunctors(java.lang.Object init) {
		functors_init = init;
	}
}
