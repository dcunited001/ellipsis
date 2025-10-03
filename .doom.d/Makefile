MKPATH       := $(abspath $(lastword $(MAKEFILE_LIST)))
MKPATHREAL   := $(realpath $(lastword $(MAKEFILE_LIST)))
MKDIR        := $(abspath $(dir $(MKPATH)))
MKPARENT     := $(abspath $(dir $(MKDIR)))
SHELL=/bin/sh

# - MAKEFILE_LIST gets appended on `include`
# - .VARIABLES includes all variable names: e.g. echo $(.VARIABLES)
# - `make` searches .INCLUDE_DIRS for Makefiles
# - .DEFAULT_GOAL sets default goal.
# - realpath: resolve symlinks in paths
# - value: access value without symbolic expansion
# - `make -n`: dry run
# - use `@echo echo` to avoid "echo echo echo \n echo echo"
# - `make -L`: consider the newest timestamp in a chain of symlinks (instead of the
#   final referent's timestamp)

# .PHONY will override w/o the following
#
# .DEFAULT_GOAL := all
# all: ; @echo $@

GUIX_PROFILE=$(MKDIR)/.guix-profile
MANIFEST=$(MKDIR)/manifest.scm
CHANLOCK=$(MKDIR)/channels-lock.scm
GUIXGUIXCHAN=$(HOME)/.config/guix/channels.scm

GUIXTM=guix time-machine -C channels-lock.scm

# TODO: shouldn't be coupled to another project install path
ELCHANNEL=$(abspath $(MKPARENT)/ellipsis)
DCCHANNEL=$(abspath $(MKPARENT)/dc)
GUIXPACKAGE=guix package -L ${ELCHANNEL} -L ${DCCHANNEL}

# quoting issues:
# GUIXLOADARGS="-L ${ELCHANNEL} -L ${DCCHANNEL}"
# GUIXPACKAGE="guix package ${GUIXLOADARGS}"

# .guix-profile requires pulling $ELCHANNEL,$DCCHANNEL first

.DEFAULT_GOAL := all
.PHONY: all
all: mkenv
	@echo all: $@
	@echo make all

.PHONY: mkenv
mkenv:
	@echo $(.DEFAULT_GOAL)
	@echo mkenv: $@
	@echo "MKPATH=$(MKPATH)  MKPATHREAL=$(MKPATHREAL)  MKDIR=$(MKDIR)  MKPARENT=$(MKPARENT)"

# This stops before the first message displays
.PHONY: testErrorStatus
testErrorStatus:
	@echo Update testError! Run make manualTask
	@echo $(error Update testError! Run make manualTask)

# - this is more verbose than necessary, but only copies when it's necessary.
#   - should it be the real file? or is it better avoid file target in $HOME?
# - also, timestamps won't hold on clone.
# - how to reference the name of a .PHONY target?

.PHONY: updateChanLock
updateChanLock:
	@echo "Running updateChanLock"
	test ${CHANLOCK} -nt ${GUIXGUIXCHAN} \
	&& echo ${GUIXGUIXCHAN} is not newer than ${CHANLOCK} \
	|| (cp -p ${GUIXGUIXCHAN} ${CHANLOCK} \
	    && echo Copied ${GUIXGUIXCHAN} to ${CHANLOCK})

# this seems like a bad pattern
manifest.scm:
	touch manifest.scm

channels-lock.scm: $(GUIXGUIXCHAN)
	@echo $(error Update channels-lock.scm! Run make updateChanLock)

# this does NOT consistently run its dependents, regardless of whether `make
# -L` is used... which is annoyingly required.
.guix-profile: manifest.scm channels-lock.scm
	${GUIXPACKAGE} -m ${MANIFEST} -p ${GUIX_PROFILE}

# channels-lock.scm:
# 	mktemp
# 	echo Update channels-lock.scm! Run make updateChanLock
# 	echo $(error thats a silent error)

# .PHONY: $(GUIXGUIXCHAN)
# $(GUIXGUIXCHAN):
# 	cp -p $(GUIXGUIXCHAN) $(MKDIR)/channels-lock.scm

# ---------------------------------------------
# # to test config changes requiring a fresh emacs server
#
# uwsm app -- doomemacs -s fdsa # (`uswm app` avoids UWSM crash)
#
# # from ./doom-set-env (if reconstructing a minimal env is necessary)
#
# export EMAIL=aionfork@gmail.com
# export SSH_AUTH_SOCK=/run/user/1000/gnupg/S.gpg-agent.ssh
# export DOOMDIR=${HOME}.doom.d
# export EMACS=$(pwd)/.guix-profile/bin/emacs
# export EMACSDIR=${HOME}/.emacs.doom
# export DOOMLSP=lsp
#
# emacs_socket_name=fdsa
# export EMACSSOCKET=/run/users/1000/emacs/$emacs_socket_name
#
# # then... (add -q to avoid using doom)
# uwsm app -- emacs -nw
#
# # or set GUIX_PROFILE and
#
# guix shell -p $GUIX_PROFILE -- \
#      emacs --init-directory=$EMACSDIR
#      --fg-daemon="$emacs_socket_name"
