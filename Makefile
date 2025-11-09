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

GUIXGUIXCHAN=$(HOME)/.config/guix/channels.scm
CHANNELS_FILE=./env/dc-configs/guix/channels.scm
GUIXTM=guix time-machine -L ./env -C ${CHANNELS_FILE}
GUIX=$(GUIXTM) --

# don't use these vars for GUIXTM
ELCHANNEL=$(abspath $(MKDIR)/ellipsis)
DCCHANNEL=$(abspath $(MKDIR)/dc)
GUIXPACKAGE=guix package -L ${ELCHANNEL} -L ${DCCHANNEL}

# TODO: fix these load paths

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
# Guix Channel

guix:

# TODO: this be crazy. either use git-restore-mtime from MestreLion/git-tools
#   - or fsmonitor-watchman (receives command+args and list of rewrites on stdin)

# multiple bad patterns here....

# GUIX_GIT_FILES := .config/guix/base-channels.scm \
# .config/guix/channels.scm \
# env/dc-configs/guix/channels.scm
# GUIX_GIT_TIMESTAMPS := $(foreach f, $(GUIX_GIT_FILES), $(shell git log -1 --format="%ad" --date=iso-strict -- $(f)))

# guixGitTouch:
# 	echo $(GUIX_GIT_FILES)
# 	echo $(GUIX_GIT_TIMESTAMPS)

# # for f in $(GUIX_GIT_TOUCH); do \
# #   echo commit_date=$$(git log -1 --format=\"%ad\" --date=iso-strict -- \"$$f\"); \
# #   echo "touched mtime: $$f $$commit_date"; \
# #   echo touch -m -d "$$commit_date" "$$f"; \
# # done # wtf

# -----------------------
# For Dotfiles
#
# - for Doom Emacs, and misc profiles

$(HOME)/.config/guix/current:
.config/guix/base-channels.scm: # guix-git-touch
.config/guix/channels.scm:  .config/guix/current # guix-git-touch .config/guix/current
	guix describe --format=channels > .config/guix/channels.scm

.PHONY: guix-pull
guix-pull: # guix-git-touch
	guix pull -L ./ellipsis -L ./dc -C ${GUIXGUIXCHAN}

# -----------------------
# For hacking on Scheme
#
# - for packages, services and guix-home in ./ellipsis and ./dc
# - this stays locked via ${CHANNELS_FILE}

# guix: env/sync

env/dc-configs/guix/channels.scm: # .config/guix/channels.scm # (run manually)

guix-pull-dev: .config/guix/channels.scm env/dc-configs/guix/channels.scm
	guix pull -L ./env -C ${CHANNELS_FILE}

# TODO: automatically sync the commit shas in env/dc-configs/guix/channels.scm
# with those in .config/guix/channels (run as a PHONY task). it should be as
# simple as:
#
# - create a timestamped backup (so it doesn't get overwritten, though it's in git)
# - then eliminate everything after (define core-channels ...)
# - run a guile -e script that sources .config/guix/channels (or guix describe)
# - and write the object it to a scheme port. append a reference to `core-channels`
#
# done... or it would be as simple as that... but idk scheme tooling well
# enough. you can write plain scheme objects to a file. that's all that's
# happening here. the channels format is like a nested plist.

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


# TODO: figure out how to get a separate GC link, so the GUIXTM store items
# aren't prematurely purged (like a basic emacs profile or something)

# .guix-profile: manifest.scm channels-lock.scm
# 	${GUIXPACKAGE} -m ${MANIFEST} -p ${GUIX_PROFILE}

# =============================================
# Guix

#-----------------------
# ISOs
.PHONY: guixIso
GPGISO_SCM="(@ (ellipsis system usb-gpg-tools) usb-gpg-tools)"

gpgiso:
	${GUIX} system -L ./ellipsis \
	image --image-type=iso9660 \
	-e ${GPGISO_SCM}

# ---------------------------------------------
# Guix Home
GUIX_HOST=$(shell hostname)
GUIX_HOST_SYSTEM=./dc/dc/system/$(GUIX_HOST).scm
GUIX_HOST_HOME=./dc/dc/home/$(GUIX_HOST).scm

.PHONY: ghbuild
ghbuild:
	${GUIX} home -L ./ellipsis -L ./dc \
	build ${GUIX_HOST_HOME}

.PHONY: ghcontainer
ghcontainer:
	${GUIX} home -L ./ellipsis -L ./dc \
	container ${GUIX_HOST_HOME}

#-----------------------
# Guix Home Container
#
# Just quickly extract a few files from there

# TODO: cat this content from the store after guix home build

GUIX_HOST_HE="(@ (dc home kharis) kharis-home-environment)"

.PHONY: guixHomeContainer
guixHomeContainer:
	${GUIX} home -L ./ellipsis -L ./dc \
	container --share="$(MKDIR)" -e ${GUIX_HOST_HE}

#-----------------------
# Screen
.PHONY: screen
screen: ${HOME}/.screenrc ${HOME}/.screen

# probably a bad pattern.... may switch to using stow
${HOME}/.screenrc:
	ln -s $(MKDIR)/.screenrc $(MKDIR)/../.screenrc

# Screen creates sockets and hjem creates *.screenrc links
# ${HOME}/.screen:
# 	ln -s $(MKDIR)/.screen $(MKDIR)/../.screen

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
