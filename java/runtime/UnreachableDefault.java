//
// Copyright (C) 2001-2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
// This exception signals when an unreachable default case of of a switch
// statement is reached.
//

package mercury.runtime;

public class UnreachableDefault extends java.lang.RuntimeException {
	
	public UnreachableDefault() {
		super();
	}

	public UnreachableDefault(String s) {
		super(s);
	}
}
