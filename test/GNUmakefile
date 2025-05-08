OCEANIC_TOP = ..

.PHONY: all test test-local sync-for-testing


# So that it remains the default target:
all:

test: test-local

test-local:

# Targeting a test install, not he CEYLAN_SYNC_TARGET_ROOT for deployments:
sync-for-testing:
	@echo "  Synchronising the Oceanic test sources to testbed on gateway"
	@$(SYNC_TOOL) $(SYNC_OPT) *.erl $(GATEWAY):$(OCEANIC_SYNC_TARGET_ROOT)/test


include $(OCEANIC_TOP)/GNUmakesettings.inc
