//
// Copyright (C) 2001-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class PseudoTypeInfo {
	// This class is intentionally empty. 
	// XXX PsuedoTypeInfo's have not been implemented yet.
	//     They should all extend this class.
	public int variable_number;
	public PseudoTypeInfo()      { variable_number = -1; }
	public PseudoTypeInfo(int n) { variable_number = n; }
}
