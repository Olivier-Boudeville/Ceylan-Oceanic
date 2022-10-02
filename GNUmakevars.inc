# Prerequisite: OCEANIC_TOP must be set.


# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)

	ENABLE_DEBUG = true

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)

	ENABLE_DEBUG = false

else

	$(error Invalid Oceanic execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif



# Tells whether the debug mode will be activated for the next Oceanic classes to
# be built (maximum performance versus extended checkings).
#
ifeq ($(ENABLE_DEBUG),true)
	ENABLE_OCEANIC_DEBUG_OPT = -Doceanic_debug_mode
else
	ENABLE_OCEANIC_DEBUG_OPT =
endif



# Defining (optional) layer-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management):

# Topics covered by flags:
#  - tty: for the management of the TTY allocated to the USB Enocean gateway
#  - decoding: for the decoding of Enocean telegrams
#  - encoding: for the encoding of Enocean telegrams
# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	OCEANIC_DEBUG_FLAGS += -Doceanic_debug_tty
	OCEANIC_DEBUG_FLAGS += -Doceanic_debug_decoding
	OCEANIC_DEBUG_FLAGS += -Doceanic_debug_encoding
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	OCEANIC_CHECK_FLAGS += -Doceanic_check_tty
	OCEANIC_CHECK_FLAGS += -Doceanic_check_decoding
	OCEANIC_CHECK_FLAGS += -Doceanic_check_encoding
endif



# As, with Oceanic, traces still rely on console output, the default here is to
# ellipse emitted traces of lesser severity; uncomment next line to disable any
# ellipsing of them (in order to have all traces in full):
#
# (note that, for such a make variable to be assigned by an upper layer and to
# be taken into account here, Oceanic must be built from an overall, integrated
# Make context - not as a series of separate builds)
#
#OCEANIC_UNELLIPSED_TRACES_OPT = -Doceanic_unellipsed_traces



# One may specify oceanic_unellipsed_traces if not wanting the emitted traces to
# be ellipsed (typically if relying on an advanced logger able to cope with
# longer traces, such as Ceylan-Traces).

ERLANG_COMPILER_OPT_BASE += $(OCEANIC_DEBUG_FLAGS)               \
							$(OCEANIC_CHECK_FLAGS)               \
							$(OCEANIC_UNELLIPSED_TRACES_OPT)



# Useful to target for example the root of the current layer (ex: to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(OCEANIC_TOP)
endif


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := Ceylan-Oceanic
endif


# PROJECT_SHORT_NAME should be defined on a per-project basis.
ifndef PROJECT_SHORT_NAME
	PROJECT_SHORT_NAME := oceanic
endif


# PROJECT_NAME_FOR_REBAR should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_REBAR
	PROJECT_NAME_FOR_REBAR := $(PROJECT_SHORT_NAME)
endif


# The uniquely-defined version of this layer, for all uses (including rebar and
# hex):
#
OCEANIC_VERSION := 0.0.1


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(OCEANIC_VERSION)
endif



ifndef PACKAGE_TOP
	PACKAGE_TOP := $(OCEANIC_TOP)
endif


# For any quick, local, non release-packaged update thereof:
MYRIAD_CHECKOUT_BUILD = $(LAYER_TOP)/_checkouts/myriad
MYRIAD_SIBLING_BUILD = $(LAYER_TOP)/../myriad


# The OTP tree of the Myriad application within a local OTP _build tree:
MYRIAD_LOCAL_APP := $(LAYER_TOP)/_build/default/lib/myriad

# Where Myriad BEAMs are to lie when used as an OTP application:
MYRIAD_LOCAL_EBIN := $(MYRIAD_LOCAL_APP)/ebin


# Erlang-serial section
#
# (refer to http://oceanic.esperide.org/#erlang-serial for installation
# guidelines)
#
ERLANG_SERIAL_ROOT := $$HOME/Software/erlang-serial
ERLANG_SERIAL_VERSION := 1.1
ERLANG_SERIAL_BASE := $(ERLANG_SERIAL_ROOT)/erlang/lib/serial-$(ERLANG_SERIAL_VERSION)
ERLANG_SERIAL_BEAM_DIR := $(ERLANG_SERIAL_BASE)/ebin


OCEANIC_RELEASE_BASE := oceanic-$(PROJECT_VERSION)

OCEANIC_RELEASE_ARCHIVE_ZIP := $(OCEANIC_RELEASE_BASE).zip
OCEANIC_RELEASE_ARCHIVE_BZ2 := $(OCEANIC_RELEASE_BASE).tar.bz2
OCEANIC_RELEASE_ARCHIVE_XZ  := $(OCEANIC_RELEASE_BASE).tar.xz


# Source section.
OCEANIC_SRC := $(OCEANIC_TOP)/src

# BEAM path section.
OCEANIC_BEAM_DIR := $(OCEANIC_SRC)


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
OCEANIC_BEAM_DIRS := $(OCEANIC_BEAM_DIR) $(OCEANIC_TOP)/..



# For OTP releases and all:

OCEANIC_REBAR_BUILD_BASE = $(MYRIAD_REBAR_BUILD_DIR)/lib/oceanic


# When building Oceanic in an OTP context from its usual (GIT) root (ex: with
# 'make rebar3-application'), the BEAMs of Myriad are to be found in the OTP
# build tree - rather than in a supposedly fully-built usual root for Myriad:
#
# (these information are useful only at compilation-time, in order to locate the
# needed parse transforms and their dependencies; at runtime, the OTP rules
# ensure that the relevant ebin directories are in the code path)
#
# Note that this implies that the BEAM files in all ebin directories should be
# up to date with the ones in the build trees, otherwise hard-to-debug
# discrepancies may happen (these OTP/rebar-related directories act as default
# catch-alls, should no prior directory correspond in the BEAM paths).
#
MYRIAD_OTP_BEAM_DIR_FROM_USUAL = $(LAYER_TOP)/$(MYRIAD_REBAR_BUILD_BASE)/ebin


# In an OTP/rebar3-style application layout (not speaking of sibling base
# directories here), at compilation time as well, modules compiled (directly or
# not) by the Myriad parse transform will expect its modules to be located in:
#
# (a third place could be considered, in the _build tree of Myriad itself, yet
# it would not provide any additional chance)
#
MYRIAD_OTP_BEAM_DIR_FROM_OTP = $(LAYER_TOP)/../myriad/ebin/


# To locate Myriad modules from all OTP contexts:
MYRIAD_PARSE_TRANSFORMS_PZ_OPT = -pz $(MYRIAD_OTP_BEAM_DIR_FROM_USUAL) \
								 -pz $(MYRIAD_OTP_BEAM_DIR_FROM_OTP)


# We rely on Myriad as well:
#
# (erlang-serial not rebar-compliant and anyway meaningless for continuous
# integration, hence no $(ERLANG_SERIAL_BEAM_DIR) in BEAM_DIRS):
#
BEAM_DIRS += $(OCEANIC_BEAM_DIRS)

ABS_BEAM_DIRS += $(ERLANG_SERIAL_BEAM_DIR)

ifndef VM_TEST_NAME
	VM_NAME := oceanic_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := oceanic_test
endif




# Compiler section.



ifndef COMPILER_OPT_FOR_OCEANIC_CLASSES

	# ENABLE_OCEANIC_DEBUG_OPT managed transversely:
	COMPILER_OPT_FOR_OCEANIC_CLASSES =                     \
		$(ERLANG_COMPILER_OPT_BASE)                        \
		$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_OCEANIC) \
		$(ERLANG_COMPILER_EXTRA_OPTS)

endif


ERLANG_COMPILER_EXTRA_OPTS += $(ENABLE_OCEANIC_DEBUG_OPT)


# For later reuse in the next layer of the software stack:
OCEANIC_PLT_FILE := $(OCEANIC_TOP)/Ceylan-Oceanic.plt


# This is the merged PLT of the level just below in the software stack.
# For 'Oceanic', it is the PLT of 'Myriad':
#
ifndef PREDECESSOR_PLT
	PREDECESSOR_PLT = $(MYRIAD_PLT_FILE)
endif



# Documentation section.


ifndef EDOC_OPTS
	EDOC_OPTS = '[{dir,"$(API_DOC_ROOT)"}, {def,{vsn,"$(PROJECT_VERSION)"}}, {preprocess,true}, {includes,["$(LAYER_TOP)/include", "$(LAYER_TOP)/include/interfaces", "$(MYRIAD_TOP)/.."]}, {overview, "$(LAYER_TOP)/overview.edoc"}]'
endif



# We define the path to the Myriad layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, Ceylan dependencies will be
# searched (in that order, the one enforced in otp_utils) in a local _checkouts
# child directory (if checkout directories are used and if the top-level package
# of interest is the current one), otherwise in sibling directories (i.e. whose
# build root is at the same level as this one).
#
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (ex: myriad) as opposed to
# project/repository names (ex: Ceylan-Myriad):
#
# First priority is an existing checkout directory:
ifneq ($(wildcard $(OCEANIC_TOP)/_checkouts/myriad/.),)
	MYRIAD_TOP = $(OCEANIC_TOP)/_checkouts/myriad
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(OCEANIC_TOP)/../myriad/.),)
		MYRIAD_TOP = $(OCEANIC_TOP)/../myriad
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(OCEANIC_TOP)/_build/default/lib/myriad/.),)
			MYRIAD_TOP = $(OCEANIC_TOP)/_build/default/lib/myriad
		else
			FORCE_EVAL := $(error Unable to find the build root of Ceylan-Myriad from '$(realpath .)'.)
		endif
	endif
endif

ifndef PREDECESSOR_LAYER_TOPS
	PREDECESSOR_LAYER_TOPS := $(MYRIAD_TOP)
endif