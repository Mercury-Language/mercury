//
// Copyright (C) 2004 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class PredFunc {
	
	public static final int MR_PREDICATE = 0;
	public static final int MR_FUNCTION = 1;

	public int value;

	public PredFunc(int arg) {
		this.value = arg;
	}
}
