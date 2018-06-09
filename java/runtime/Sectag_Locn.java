//
// Copyright (C) 2001-2002 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class Sectag_Locn implements java.io.Serializable {
	
	public static final int MR_SECTAG_NONE = 0;
	public static final int MR_SECTAG_NONE_DIRECT_ARG = 1;
	public static final int MR_SECTAG_LOCAL = 2;
	public static final int MR_SECTAG_REMOTE = 3;

	public int value;

	public Sectag_Locn(int arg) {
		this.value = arg;
	}
}



