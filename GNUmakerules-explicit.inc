# Here are gathered explicit (immediate, static) rules for Ceylan-Oceanic.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: OCEANIC_TOP must be set.


.PHONY: default-oceanic-rule clean-ast-debug clean-ast-debug-oceanic   \
		create-oceanic-checkout                                        \
		rebar3-local-update rebar3-local-update-for-oceanic            \
		rebar3-local-checkout-update-for-oceanic                       \
		rebar3-local-sibling-update-for-oceanic                        \
		info-compile-oceanic


# The first non-generic, therefore default, rule:
default-oceanic-rule: all


# Create a _checkouts directory in which all Ceylan dependencies for the current
# layer are referenced through a symlink.
#
create-oceanic-checkout:
	@echo "  Creating a checkout directory for Oceanic"
	@$(MAKE) -s create-myriad-checkout
	@cd _checkouts && ln -sf ../$(MYRIAD_TOP)


# Allows to update in the current OTP _build tree Myriad files from a supposedly
# local built tree (hence not packaged; obtained thanks to a 'make
# rebar3-compile' there) alongside the current one (through a checkout directory
# or a sibling one).
#
# Allows to rely on a local, updated Myriad without having to publish yet an Hex
# package for it.
#
rebar3-local-update: rebar3-local-update-for-oceanic


# As called by upper layers:
#
# (used to depend beforehand on the rebar3-local-sibling-update-for-oceanic
#  target):
#
rebar3-local-update-for-oceanic: rebar3-local-checkout-update-for-oceanic


# Supposing typically that a _checkouts/myriad symlinks exists, pointing to an
# actual Myriad clone:
#
rebar3-local-checkout-update-for-oceanic:
	@echo "  Updating Myriad from local checkout build tree $(MYRIAD_CHECKOUT_BUILD)"
	@cd $(MYRIAD_CHECKOUT_BUILD) && $(MAKE) -s rebar3-compile
	@mkdir -p $(MYRIAD_LOCAL_EBIN)
	@/bin/cp -f $(MYRIAD_CHECKOUT_BUILD)/_build/default/lib/myriad/ebin/* $(MYRIAD_LOCAL_EBIN)


# Supposing a myriad symlink exists at the same level (sibling) of the oceanic
# root one.
#
# Mostly superseded by a more convenient solution based on a _checkouts
# directory (see rebar3-local-checkout-update-for-oceanic above).
#
rebar3-local-sibling-update-for-oceanic:
	@echo "  Updating Myriad from local sibling build tree $(MYRIAD_SIBLING_BUILD)"
	@cd $(MYRIAD_SIBLING_BUILD) && $(MAKE) -s rebar3-compile
	@mkdir -p $(MYRIAD_LOCAL_EBIN)
	@/bin/cp -f $(MYRIAD_SIBLING_BUILD)/_build/default/lib/myriad/ebin/* $(MYRIAD_LOCAL_EBIN)


info-compile-oceanic:
	@echo "COMPILER_OPT_FOR_OCEANIC_CLASSES = $(COMPILER_OPT_FOR_OCEANIC_CLASSES)"
	@echo "ENABLE_OCEANIC_DEBUG_OPT = $(ENABLE_OCEANIC_DEBUG_OPT)"
