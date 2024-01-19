// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2014, 2016, 2018, 2024 The Mercury Team.
// This file is distributed under the terms specified in COPYING.LIB.
//
package jmercury.runtime;

import java.util.List;

/**
 * Process the MERCURY_OPTIONS environment variable.
 */
public class MercuryOptions {

    private int num_processors;

    /**
     * Create a new MercuryOptions object.
     * This constructor is package-private (no access declaration).
     */
    MercuryOptions()
    {
        // Zero means autodetect.
        num_processors = 0;
    }

    public void process()
    {
        String options = System.getenv("MERCURY_OPTIONS");
        if (options == null) {
            return;
        }

        Getopt getopt = new Getopt(options);
        for (Getopt.Option option : getopt) {
            if (option.optionIs("P")) {
                try {
                    num_processors = option.getValueInt();
                } catch (java.lang.NumberFormatException e) {
                    throw new MercuryFatalError(
                        "the value of the -P option must be " +
                        "a non-negative integer", e);
                }
            } else {
                throw new MercuryFatalError("Unrecognized option: " + option);
            }
        }

        List<String> args = getopt.getArguments();
        if (args.size() > 0) {
            throw new MercuryFatalError(
                "Error parsing MERCURY_OPTIONS environment variable,"
                 + " unexpected: " + args.get(0));
        }
    }

    /**
     * Get the number of processors in the machine.
     */
    public int getNumProcessors() {
        return num_processors;
    }
}
