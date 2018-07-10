// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2001-2002, 2004-2007, 2009-2011 The University of Melbourne
// Copyright (C) 2014, 2018 The Mercury Team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

/**
 * Simple bitmap implementation.
 * This bitmap class is mainly used by bitmap.m in the standard library.
 * It is also used by version_array.m which is why it is part of the
 * jmercury.runtime package rather than being internal to bitmap.m.
 */
public class MercuryBitmap implements java.io.Serializable {
    public static final int BITS_PER_BYTE = 8;
    public int num_bits;
    public byte[] elements;

    public MercuryBitmap(int numBits) {
        int num_bytes = (numBits + BITS_PER_BYTE - 1) / BITS_PER_BYTE;

        this.num_bits = numBits;
        this.elements = new byte[num_bytes];
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        }
        if (that instanceof MercuryBitmap) {
            MercuryBitmap other = (MercuryBitmap)that;
            return this.num_bits == other.num_bits
                && java.util.Arrays.equals(this.elements, other.elements);
        }
        return false;
    }

    public boolean getBit(int bit)
    {
        return (elements[byteIndexForBit(bit)]
            & (1 << bitIndexWithinByte(bit))) != 0;
    }

    public void setBit(int bit)
    {
        byte b;

        b = elements[byteIndexForBit(bit)];
        b |= 1 << bitIndexWithinByte(bit);
        elements[byteIndexForBit(bit)] = b;
    }

    public void clearBit(int bit)
    {
        byte b;

        b = elements[byteIndexForBit(bit)];
        b &= ~(1 << bitIndexWithinByte(bit));
        elements[byteIndexForBit(bit)] = b;
    }

    public static int byteIndexForBit(int bit) {
        return bit / BITS_PER_BYTE;
    }

    public static int bitIndexWithinByte(int bit) {
        return bit % BITS_PER_BYTE;
    }
}
