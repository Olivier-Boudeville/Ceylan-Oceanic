OCEANIC_TOP = ../..

# No 'send_telegram_exec' must be listed here, otherwise rule inhibited:
.PHONY: all run-direct run-param run-example help

SEND_SCRIPT := ./send-telegram.escript

EXAMPLE_TELEGRAM := "55000707017ad50801959f620001ffffffff5b0069"

all:


run-direct: send_telegram_exec


run-param:
	@$(MAKE) -s send_telegram_exec CMD_LINE_OPT="-h"


run-example:
	@$(MAKE) -s send_telegram_exec CMD_LINE_OPT="$(EXAMPLE_TELEGRAM)"


send_telegram_exec: telegram_sending.beam


help:
	@$(SEND_SCRIPT) --help


include $(OCEANIC_TOP)/GNUmakesettings.inc
