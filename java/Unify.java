//
// Copyright (C) 2001 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
// This interface is implemented by classes that are wrapped around the
// `unify' special predicate.  Instantitions of those classes are then
// used as entries in the TypeCtorInfo_Struct's.
//

package mercury.runtime;

public interface Unify {
	public abstract boolean call(java.lang.Object[] args);
}
