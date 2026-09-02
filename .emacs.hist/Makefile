##
# Emacs profile to browse history
#
# @file
# @version 0.1


MKDIR      ?= $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

MKPATH       := $(abspath $(lastword $(MAKEFILE_LIST)))
MKPATHREAL   := $(realpath $(lastword $(MAKEFILE_LIST)))
MKDIR        := $(abspath $(dir $(MKPATH)))
MKPARENT     := $(abspath $(dir $(MKDIR)))

SHELL=/bin/sh
HOST=$(shell hostname)
GUIXHOST=$(shell hostname)
GUIXUSER=$(shell whoami)

SRC = $(MKDIR)
GSRC = $(MKDIR)/../guix

GUIX_PROFILE=$(MKDIR)/.guix-profile
MANIFEST=$(MKDIR)/manifest.scm
GUIXPACKAGE=guix package -L $(GSRC)/dc

manifest.scm:
	touch manifest.scm
.guix-profile: manifest.scm
	$(GUIXPACKAGE) -m $(MANIFEST) -p $(GUIX_PROFILE)

EMACSDIR=$(HOME)/.emacs.hist
EMACS=$(MKDIR)/.guix-profile/bin/emacs
emacs_socket_name=emacshist

EMACS_SHELL= guix shell -p $(GUIX_PROFILE) -E EMACSDIR -E EMACS

.PHONY: run
run: .guix-profile
	uwsm app -- $(EMACS_SHELL) -- emacs --init-directory=$(EMACSDIR)

# end
