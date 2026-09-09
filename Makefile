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
	$(MAKE) -C guix "$*"

nixos-%:
	$(MAKE) -C nixos "$*"

doom-%:
	$(MAKE) -C .doom.d "$*"

hypr-%:
	$(MAKE) -C .config/hypr "$*"

.PHONY: screen
screen: $(HOME)/.screenrc $(HOME)/.screen

$(HOME)/.screenrc:
	ln -s $(MKDIR)/.screenrc $(MKDIR)/../.screenrc

.PHONY: omarchy
omarchy: $(HOME)/.config/omarchy

$(HOME)/.config/omarchy:
	ln -s $(MKDIR)/.config/omarchy.$(HOST) $(HOME)/.config/omarchy

# cut -f1 -d'     ' tmp/mimesdb.tsv | sort | uniq | wc -l
.PHONY: xdg-trace-mime xdg-trace-mime-types xdg-trace-mime-which
xdg-trace-mime: xdg-trace-mime-types xdg-trace-mime-which
xdg-trace-mime-types:
	xdg-trace-mime-types > tmp/mime-type.$(HOST)_$(shell date +%s).tsv
xdg-trace-mime-which:
	xdg-trace-mime-which > tmp/mime-apps.$(HOST)_$(shell date +%s).tsv

# Screen creates sockets and hjem creates *.screenrc links
# $(HOME)/.screen:
# 	ln -s $(MKDIR)/.screen $(MKDIR)/../.screen

# end
