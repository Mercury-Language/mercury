#!/bin/sh
# vim: ft=sh ts=8 sts=4 sw=4 et
#-----------------------------------------------------------------------------#
# Test handling of :- external procedures with intermodule analysis.

. ../common.sh

set -e
set -x

rm -rf Mercury

$MMCMAKE ext.analyse --analysis-repeat 0 || failed

check_result ext 'exception.*foo'
check_result ext 'structure_reuse.*foo'
check_result ext 'structure_sharing.*foo'
check_result ext 'trail.*foo'
check_result ext 'unused_args.*foo'
check_statuses 'optimal.'
check_no_requests

: Succeeded

exit 0

#-----------------------------------------------------------------------------#
