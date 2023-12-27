OCEANIC_TOP = ..

.PHONY: all real-clean

MODULES_DIRS = scripts

# As this module must be generated to share Oceanic constants:
#
# (see the oceanic_constants module and the %_generated.beam target in Myriad's
# GNUmakerules-automatic.inc)
#
EXTRA_BEAM_FILES = oceanic_generated.beam


# As shall be the default rule:
all:


real-clean:
	-@/bin/rm -f oceanic.app.src


include $(OCEANIC_TOP)/GNUmakesettings.inc
