//
// Copyright (C) 2001-2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class Sectag_Locn {
	
	public static final int MR_SECTAG_NONE = 0;
	public static final int MR_SECTAG_LOCAL = 1;
	public static final int MR_SECTAG_REMOTE = 2;

	public int value;

	public Sectag_Locn(int arg) {
		this.value = arg;
	}
}



