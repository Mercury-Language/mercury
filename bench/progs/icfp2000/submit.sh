#!/bin/sh
icfpaddress=icfp-sub@cs.cornell.edu
icfpsubject="ICFPsubmission"

infofile=$1
submissionfile=$2

## data must be valid rfc822 headers 
## do not add white space to the beginning of lines
echo sending submission
cat ${infofile}
metasend -b -S 2200000 \
	    -t ${icfpaddress} \
            -s ${icfpsubject} \
            -f ${infofile} \
            -e 7bit \
            -m text/plain \
            -n \
            -f ${submissionfile} \
            -e base64 \
            -m application/octet-stream

