OCEANIC_TOP = .


.PHONY: help help-intro help-oceanic                                       \
		all register-version-in-header register-oceanic list-beam-dirs     \
		sync                                                               \
		add-prerequisite-plts link-plt                                     \
		release release-zip release-bz2 release-xz                         \
		prepare-release clean-release clean-archive stats                  \
		info-context info-versions info-paths                              \
		info-compile info-conditionals info-deps info-serial               \
		info-sync-sources


MODULES_DIRS = src doc conf test priv


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


OCEANIC_RELEASES = $(OCEANIC_RELEASE_ARCHIVE_BZ2) \
				   $(OCEANIC_RELEASE_ARCHIVE_ZIP) \
				   $(OCEANIC_RELEASE_ARCHIVE_XZ)



# First target for default:
help: help-intro help-oceanic


help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-oceanic:
	@cd $(MYRIAD_TOP) && $(MAKE) -s help-myriad


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ]; then \
	echo "Error, no version file defined." 1>&2; exit 51; else \
	$(MAKE) -s register-oceanic; fi


register-oceanic:
	@echo "-define( oceanic_version, \"$(OCEANIC_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(OCEANIC_BEAM_DIRS); do echo $$(readlink -f $$d); done


# To synchronise from the local tree the code base of a remote server having a
# USB gateway, with no Git commit needed.
#
# If the source tree is built and up to date, no need to (re)build on the server
# (if homogeneous in terms of versions).
#
# (note that files removed from the local sources will remain in the target
# server)
#
sync-sources-to-server:
	@$(MAKE) -s all
	@echo " Synchronising the $$(basename $$(pwd)) layer to $(OCEANIC_SRV):$(OCEANIC_SYNC_TARGET_ROOT)"
	@if [ -n "$(OCEANIC_SRV)" ]; then if [ -n "$(OCEANIC_SYNC_TARGET_ROOT)" ]; then $(SYNC_TOOL) $(SYNC_OPT) $(OCEANIC_TOP)/../oceanic $(OCEANIC_SRV):$(OCEANIC_SYNC_TARGET_ROOT); else echo "Error, no OCEANIC_SYNC_TARGET_ROOT variable set." 1>&2; exit 4; fi; else echo "Error, no OCEANIC_SRV variable set." 1>&2; exit 5; fi


add-prerequisite-plts: link-plt


# As upper layers may rely on the 'oceanic' naming:
link-plt:
	@if [ ! "$(PLT_FILE)" = "$(OCEANIC_PLT_FILE)" ]; then /bin/ln -s --force $(PLT_FILE) $(OCEANIC_PLT_FILE); fi


# Note: the source archives are not produced in this directory, but in its
# parent, so that everything related to OCEANIC (including these rules) remains
# self-contained.


release: release-zip release-bz2 release-xz
	@$(MAKE) clean-release


release-zip: prepare-release
	@echo "     Creating Oceanic release archive $(OCEANIC_RELEASE_ARCHIVE_ZIP)"
	@cd .. && zip -r $(OCEANIC_RELEASE_ARCHIVE_ZIP) $(OCEANIC_RELEASE_BASE) \
	&& echo "     Archive $(OCEANIC_RELEASE_ARCHIVE_ZIP) ready in $$(pwd)"


release-bz2: prepare-release
	@echo "     Creating Oceanic release archive $(OCEANIC_RELEASE_ARCHIVE_BZ2)"
	@cd .. && tar chvjf $(OCEANIC_RELEASE_ARCHIVE_BZ2) $(OCEANIC_RELEASE_BASE) \
	&& echo "     Archive $(OCEANIC_RELEASE_ARCHIVE_BZ2) ready in $$(pwd)"


release-xz: prepare-release
	@echo "     Creating Oceanic release archive $(OCEANIC_RELEASE_ARCHIVE_XZ)"
	@cd .. && tar chvjf $(OCEANIC_RELEASE_ARCHIVE_XZ) $(OCEANIC_RELEASE_BASE) \
	&& echo "     Archive $(OCEANIC_RELEASE_ARCHIVE_XZ) ready in $$(pwd)"


# The '-L' option with cp is used so that symbolic links are replaced by their
# actual target file, otherwise tar would include dead links in releases.
#
prepare-release: clean clean-release
	@echo "     Preparing release archive for Oceanic $(OCEANIC_VERSION)"
	@cd .. && mkdir -p $(OCEANIC_RELEASE_BASE) && /bin/cp -L -r myriad oceanic $(OCEANIC_RELEASE_BASE)
	@cd ../$(OCEANIC_RELEASE_BASE) && mv oceanic/top-GNUmakefile-for-releases GNUmakefile
	-@cd .. && find $(OCEANIC_RELEASE_BASE) -type d -a -name '.git' -exec /bin/rm -rf '{}' ';' 2>/dev/null
	-@cd .. && find $(OCEANIC_RELEASE_BASE) -type f -a -name '*.beam' -exec /bin/rm -f '{}' ';' 2>/dev/null


clean: clean-release clean-archive


clean-release:
	@echo "   Cleaning release archive for Oceanic"
	-@cd .. && /bin/rm -rf $(OCEANIC_RELEASE_BASE)


clean-archive:
	-@cd .. && /bin/rm -f $(OCEANIC_RELEASES)


stats:
	@$(MAKE_CODE_STATS) $(OCEANIC_TOP)


# Typically useful to know the software context for continuous integration:
info-context: info-platform info-versions info-source-layout


info-versions:
	@echo "MYRIAD_VERSION = $(MYRIAD_VERSION)"
	@echo "OCEANIC_VERSION = $(OCEANIC_VERSION)"


info-paths:
	@echo "BEAM_PATH_OPT = $(BEAM_PATH_OPT)"


info-compile: info-compile-oceanic


info-conditionals:
	@echo "OCEANIC_DEBUG_FLAGS = $(OCEANIC_DEBUG_FLAGS)"
	@echo "OCEANIC_CHECK_FLAGS = $(OCEANIC_CHECK_FLAGS)"


info-deps: info-serial
	@echo "MYRIAD_TOP = $(MYRIAD_TOP) (i.e. $$(realpath $(MYRIAD_TOP)))"


info-serial:
	@echo "ERLANG_SERIAL_BASE = $(ERLANG_SERIAL_BASE)"


info-sync-sources: info-sync
	@echo "OCEANIC_SRV = $(OCEANIC_SRV)"
	@echo "OCEANIC_SYNC_TARGET_ROOT = $(OCEANIC_SYNC_TARGET_ROOT)"


include $(OCEANIC_TOP)/GNUmakesettings.inc
