//
// Copyright (C) 2001-2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class DuPtagLayout {
	
	public int sectag_sharers;
	public mercury.runtime.Sectag_Locn sectag_locn;
	public /* final */ DuFunctorDesc[] sectag_alternatives;

}
