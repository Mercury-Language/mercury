//
// Copyright (C) 2001-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class DuPtagLayout {
	
	public int sectag_sharers;
	public mercury.runtime.Sectag_Locn sectag_locn;
	public /* final */ mercury.runtime.DuFunctorDesc[] sectag_alternatives;

	public DuPtagLayout(int sharers, mercury.runtime.Sectag_Locn locn,
			mercury.runtime.DuFunctorDesc[] alts)
	{
		sectag_sharers = sharers;
		sectag_locn = locn;
		sectag_alternatives = alts;
	}

	public DuPtagLayout(int sharers, int locn, DuFunctorDesc[] alts) {
		sectag_sharers = sharers;
		sectag_locn = new mercury.runtime.Sectag_Locn(locn);
		sectag_alternatives = alts;
	}
}
