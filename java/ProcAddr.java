//
// Copyright (C) 2001 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
// This interface is implemented by classes that have been wrapped around
// any general predicate. We then use instantiations of those classes as
// pseudo function pointers (which we don't have in Java).  The original
// predicate can be called via the `call' method.  This interface should
// not be used for the `compare' and `unify' special predicates as the
// `Unify' and `Compare' interfaces perform as similar function for them.
//

package mercury.runtime;

public interface ProcAddr {
	public abstract java.lang.Object[] call(java.lang.Object[] Args);
}
