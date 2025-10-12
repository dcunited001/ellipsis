##
# Ellipsis
#
# @file
# @version 0.1
MKPATH       := $(abspath $(lastword $(MAKEFILE_LIST)))
MKPATHREAL   := $(realpath $(lastword $(MAKEFILE_LIST)))
MKDIR        := $(abspath $(dir $(MKPATH)))
MKPARENT     := $(abspath $(dir $(MKDIR)))
SHELL=/bin/sh

# maybe rebuild cache: --rebuild-cache

CHANNELS_FILE=./env/dc-configs/guix/channels.scm
GUIXTM=guix time-machine -L ./env -C ${CHANNELS_FILE}
GUIX=$(GUIXTM) --

# don't use these vars for GUIXTM
ELCHANNEL=$(abspath $(MKDIR)/ellipsis)
DCCHANNEL=$(abspath $(MKDIR)/dc)
GUIXPACKAGE=guix package -L ${ELCHANNEL} -L ${DCCHANNEL}

# channel outputs
ELLIPSIS_SRC_LOAD_PATH=-L ./env -L ./ellipsis
DC_SRC_LOAD_PATH=-L ./env -L ./ellipsis -L ./dc

# inputs, testing and repl
DEV_ENV_LOAD_PATH=-L ./env -L ./ellipsis


PULL_EXTRA_OPTIONS=
# --allow-downgrades

# to install directly onto a system booted with iso (cow-store)
#
# ROOT_MOUNT_MOUNT=/mnt

# =============================================
# `make ares`
#
# (1) sets /gnu/store to the specified CHANNELS_FILE (for fast GUIXTM)
#
# then (2) loads a consistent `guile` with the second set of -L flags
#
# and (3) starts the repl.
#
# for rde dev worklows, `make env/sync` syncs channel specs between rde's
# upstream and the example's channel spec

# ares: env/sync
repl: ares
ares:
	${GUIX} shell -L ./env \
	guile-next guile-ares-rs \
	-e '(@ (dc-configs dev packages) guix-package)' \
	-e '(@ (dc-configs dev packages) channels-package)' \
	-- guile -L ./env -L ./ellipsis -L ./dc -c \
"(begin (use-modules (guix gexp)) #;(load gexp reader macro globally) \
((@ (ares server) run-nrepl-server)))"

# TODO: write an env/sync task to emit channel spec to (dc-configs guix channels)
# env/sync: env/guix/rde/env/guix/channels.scm

# guix: env/sync
guix:
guix-pull:
	guix pull -L ./env -C ${CHANNELS_FILE}

# TODO: figure out how to get a separate GC link, so the GUIXTM store items
# aren't prematurely purged (like a basic emacs profile or something)

# .guix-profile: manifest.scm channels-lock.scm
# 	${GUIXPACKAGE} -m ${MANIFEST} -p ${GUIX_PROFILE}

# =============================================
# Guix

# ---------------------------------------------
# GuixHome
GUIX_HOST=$(shell hostname)
GUIX_HOST_SYSTEM=./dc/dc/system/$(GUIX_HOST).scm
GUIX_HOST_HOME=./dc/dc/home/$(GUIX_HOST).scm

GUIX_HOST_HE="(@ (dc home kharis) kharis-home-environment)"

.PHONY: ghbuild
ghbuild:
	${GUIX} home \
	${DC_SRC_LOAD_PATH} \
	build ${GUIX_HOST_HOME}

.PHONY: ghcontainer
ghcontainer:
	${GUIX} home \
	${DC_SRC_LOAD_PATH} \
	container ${GUIX_HOST_HOME}

#-----------------------
# Guix Home
#
# Just quickly extract a few files from there

# TODO: cat this content from the store after guix home build

.PHONY: guixHomeContainer
guixHomeContainer:
	guix home container -L ../dc -L ../ellipsis --share "$(MKDIR)" \
	-e "$(GH_KHARIS_HE)"

# end

# ** Writing Makefiles
#
# + [[Makefile Basics][https://www.gnu.org/prep/standards/html_node/Makefile-Basics.html]]
#
# Don't assume PATH
#
# | ./         | program is built as part of the make          |
# | $(srcdir)/ | file is an unchanging part of the source code |

# *** Portable Scripts
# + [[Portable Shell Programming][https://www.gnu.org/savannah-checkouts/gnu/autoconf/manual/autoconf-2.72/html_node/Portable-Shell.html#Portable-Shell]]
#
# only these commands should be used directly
#
# awk cat cmp cp diff echo expr false grep install-info ln ls
# mkdir mv printf pwd rm rmdir sed sleep sort tar test touch tr true

# use these tools after locating them:
#
# $(AR) $(BISON) $(CC) $(FLEX) $(INSTALL) $(LD) $(LDCONFIG) $(LEX)
# $(MAKE) $(MAKEINFO) $(RANLIB) $(TEXI2DVI) $(YACC)
#
# $(CHGRP) $(CHMOD) $(CHOWN) $(MKNOD)

# only use compression programs like gzip in the dist rule
