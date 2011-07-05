//
// Copyright (C) 2011 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package jmercury.runtime;

public class DuArgLocn implements java.io.Serializable {
	
	public int arg_offset;
	public int arg_shift;
	public int arg_bits;

	public DuArgLocn(int arg_offset, int arg_shift, int arg_bits)
	{
		this.arg_offset = arg_offset;
		this.arg_shift = arg_shift;
		this.arg_bits = arg_bits;
	}
}
