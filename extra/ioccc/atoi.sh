#!/bin/sh
# Portability concerns:
# - Not tested a machine where sizeof(int)==16
# - Stack usage is determined by number of arguments passed in
# - Loading a program is done via the number of arguments passed
# via the command line, large programs may cause problems
set -eu
IMAGE=${IMAGE:-../../subleq.dec}
PROG=${PROG:-$(cat ${IMAGE} | tr '\n' ' ')}
echo BUILDING
make atoi
echo "PROG SZ:" $(tr -d '\n' < atoi.c | wc | awk '{print $3}')
echo RUNNING
./atoi ${PROG}
echo BYE
