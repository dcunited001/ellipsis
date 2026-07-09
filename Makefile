##
# dc
#
# @file
# @version 0.1

# [[file:README.org::*=Makefile=][=Makefile=:2]]
# relative
TOP := $(dir $(lastword $(MAKEFILE_LIST)))

# absolute
MKPATH       := $(abspath $(lastword $(MAKEFILE_LIST)))
MKPATHREAL   := $(realpath $(lastword $(MAKEFILE_LIST)))
MKDIR        := $(abspath $(dir $(MKPATH)))
MKPARENT     := $(abspath $(dir $(MKDIR)))

SHELL=/bin/sh
HOST=$(shell hostname)

GSRC ?= ./guix
include guix/defaults.mk
include guix/targets.mk

NSRC ?= ./nixos


DSRC ?= ./.doom.d
# =Makefile=:2 ends here

# [[file:README.org::*=Makefile=][=Makefile=:3]]
.PHONY: screen
screen: $(HOME)/.screenrc $(HOME)/.screen

$(HOME)/.screenrc:
	ln -s $(MKDIR)/.screenrc $(MKDIR)/../.screenrc

# Screen creates sockets and hjem creates *.screenrc links
# $(HOME)/.screen:
# 	ln -s $(MKDIR)/.screen $(MKDIR)/../.screen
# =Makefile=:3 ends here

# end
