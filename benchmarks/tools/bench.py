#!/usr/bin/env python

#
# Use /usr/bin/time and a number of repetitions to collect data for test
# parallelism programs.
#

import os
import sys

REPS=100

DATA = ' '.join([
"data/apply_exclusion.m",
"data/apply_exclusion.m.1",
"data/array_util.m",
"data/array_util.m.1",
"data/callgraph.m",
"data/callgraph.m.1",
"data/canonical.m",
"data/canonical.m.1",
"data/cliques.m",
"data/cliques.m.1",
"data/conf.m",
"data/conf.m.1",
"data/coverage.m",
"data/coverage.m.1",
"data/create_report.m",
"data/create_report.m.1",
"data/dense_bitset.m",
"data/dense_bitset.m.1",
"data/display.m",
"data/display.m.1",
"data/display_report.m",
"data/display_report.m.1",
"data/dump.m",
"data/dump.m.1",
"data/exclude.m",
"data/exclude.m.1",
"data/feedback.m",
"data/feedback.m.1",
"data/html_format.m",
"data/html_format.m.1",
"data/interface.m",
"data/interface.m.1",
"data/io_combinator.m",
"data/io_combinator.m.1",
"data/mdbcomp.m",
"data/mdbcomp.m.1",
"data/mdprof_cgi.m",
"data/mdprof_cgi.m.1",
"data/mdprof_dump.m",
"data/mdprof_dump.m.1",
"data/mdprof_fb.automatic_parallelism.m",
"data/mdprof_fb.automatic_parallelism.m.1",
"data/mdprof_fb.m",
"data/mdprof_fb.m.1",
"data/mdprof_feedback.m",
"data/mdprof_feedback.m.1",
"data/mdprof_procrep.m",
"data/mdprof_procrep.m.1",
"data/mdprof_test.m",
"data/mdprof_test.m.1",
"data/measurements.m",
"data/measurements.m.1",
"data/measurement_units.m",
"data/measurement_units.m.1",
"data/message.m",
"data/message.m.1",
"data/old_html_format.m",
"data/old_html_format.m.1",
"data/old_query.m",
"data/old_query.m.1",
"data/prim_data.m",
"data/prim_data.m.1",
"data/profile.m",
"data/profile.m.1",
"data/program_representation.m",
"data/program_representation.m.1",
"data/program_representation_utils.m",
"data/program_representation_utils.m.1",
"data/query.m",
"data/query.m.1",
"data/read_profile.m",
"data/read_profile.m.1",
"data/report.m",
"data/report.m.1",
"data/rtti_access.m",
"data/rtti_access.m.1",
"data/slice_and_dice.m",
"data/slice_and_dice.m.1",
"data/startup.m",
"data/startup.m.1",
"data/timeout.m",
"data/timeout.m.1",
"data/top_procs.m",
"data/top_procs.m.1",
"data/trace_counts.m",
"data/trace_counts.m.1",
"data/util.m",
"data/util.m.1",
"data/var_use_analysis.m",
"data/var_use_analysis.m.1" ])

#TESTS = [
#    ("./quicksort_plain %s" % DATA, "", "plain.log"),
#    ("./quicksort_plain_nopar %s" % DATA, "-P 4", "plain_nopar_4.log"),
#    ("./quicksort_plain_par %s" % DATA, "-P 1", "plain_par_1.log"),
#    ("./quicksort_plain_par %s" % DATA, "-P 2", "plain_par_2.log"),
#    ("./quicksort_plain_par %s" % DATA, "-P 4", "plain_par_4.log"),
#    ("./quicksort_maybe_par %s" % DATA, "-P 1", "maybe_par_1.log"),
#    ("./quicksort_maybe_par %s" % DATA, "-P 2", "maybe_par_2.log"),
#    ("./quicksort_maybe_par %s" % DATA, "-P 4", "maybe_par_4.log"),
#    ("./quicksort_maybe_par2 %s" % DATA, "-P 1", "maybe_par2_1.log"),
#    ("./quicksort_maybe_par2 %s" % DATA, "-P 2", "maybe_par2_2.log"),
#    ("./quicksort_maybe_par2 %s" % DATA, "-P 4", "maybe_par2_4.log"),
#    ("./quicksort_top_par %s" % DATA, "-P 1", "top_par_1.log"),
#    ("./quicksort_top_par %s" % DATA, "-P 2", "top_par_2.log"),
#    ("./quicksort_top_par %s" % DATA, "-P 4", "top_par_4.log"),
#]

def do_test(test):
    log = test[2]
    cmd = test[0]
    env = test[1]
    time_cmd = ("time -a -v -o %s %s" % (log, cmd)).split(" ")

    for rep in range(REPS):
        sys.stderr.write("Rep %d of %d\n" % (rep + 1, REPS))
        write_log(log, "Rep: %d\n" % rep)
        os.putenv("MERCURY_OPTIONS", env)
        os.spawnlp(os.P_WAIT, "time", *time_cmd)

def write_log(log, message):
    log = open(log, "a")
    log.write(message)
    log.close()

def do_tests(programs):
    import operator

    tests = reduce(operator.concat, 
        [[("%s %s" % (program, DATA), "-P %d" % ncores, "%s_p%d.log" % (program, ncores))
          for program in programs
              if (program.startswith("asm_fast.gc.par") or (ncores == 1))]
         for ncores in range(1, 5)])

    num_tests = len(tests)
    for num, test in enumerate(tests):
        sys.stderr.write("Running test %d of %d\n" % (num+1, num_tests))
        do_test(test)

if __name__ == "__main__":
    do_tests(sys.argv[1:])


