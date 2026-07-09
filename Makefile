##
# dc
#
# @file
# @version 0.1

MKDIR      := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
MKPARENT   := $(abspath $(dir $(MKDIR)))
MKPATH     := $(abspath $(lastword $(MAKEFILE_LIST)))
MKPATHREAL := $(realpath $(lastword $(MAKEFILE_LIST)))

SHELL=/bin/sh
HOST=$(shell hostname)

MODULES := guix nixos oom

guix-%:
	$(MAKE) -C guix $(word 2,$(subst -, ,$*))

nixos-%:
	$(MAKE) -C nixos $(word 2,$(subst -, ,$*))

doom-%:
	$(MAKE) -C .doom.d $(word 2,$(subst -, ,$*))

.PHONY: screen
screen: $(HOME)/.screenrc $(HOME)/.screen

$(HOME)/.screenrc:
	ln -s $(MKDIR)/.screenrc $(MKDIR)/../.screenrc

# Screen creates sockets and hjem creates *.screenrc links
# $(HOME)/.screen:
# 	ln -s $(MKDIR)/.screen $(MKDIR)/../.screen

# end
