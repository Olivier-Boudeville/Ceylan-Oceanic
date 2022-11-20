#!/bin/sh

# Main launcher of the Ceylan-Oceanic telegram sending tool (knowing that the
# escript-based version is a pain to debug in an escript form).

# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:

# To debug:
#make -s send_telegram_exec CMD_LINE_OPT="-extra $*"
#make send_telegram_exec CMD_LINE_OPT="-extra $*"

cd ${CEYLAN_OCEANIC} && make -s all 1>/dev/null && cd src/scripts/ && send-telegram.escript $*

# Now not triggering a prior recompilation check:
#cd ${CEYLAN_OCEANIC} && cd src/scripts/ && send-telegram.escript $*
