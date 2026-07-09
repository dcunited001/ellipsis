# [[file:../README.org::*=./guix/targets.mk=][=./guix/targets.mk=:1]]
# `ares <- ares-profile` would add some additional eval time
ares-profile:
	@echo $(ARES_SHELL) --root=$(ARES_GC_ROOT)

ares-impure:
	guix shell -L ./env -p $(ARES_PROFILE) \
	-- guile -L ./env -L ./dc -c \
	"(begin (use-modules (guix gexp)) #;(load gexp reader macro globally) \
	((@ (ares server) run-nrepl-server)))"

ares:
	$(ARES_SHELL) -- guile -L ./env -L ./dc -c \
	"(begin (use-modules (guix gexp)) #;(load gexp reader macro globally) \
	((@ (ares server) run-nrepl-server)))"

repl: ares

# TODO: write an env/sync task to emit channel spec to (dc-configs guix channels)
# env/sync: env/guix/rde/env/guix/channels.scm

# TODO: figure out how to get a separate GC link, so the GUIXTM store items
# aren't prematurely purged (like a basic emacs profile or something)
# =./guix/targets.mk=:1 ends here

# [[file:../README.org::*=./guix/targets.mk=][=./guix/targets.mk=:2]]
guix:

$(GUIXGUIXCHAN): # guix-git-touch .config/guix/current

.PHONY: guix-pull guix-pull-sync guix-pull-lock
guix-pull: guix-pull-sync guix-pull-lock
guix-pull-sync:
	guix pull -L ./dc -C $(GUIXGUIXBASE)
guix-pull-lock:
	guix describe --format=channels > $(GUIXGUIXCHAN)

.PHONY: guix-upgrade guix-upgrade-doom
guix-upgrade:
	make guix-pull
	make repl

guix-upgrade-doom:
	make -C .doom.d updateChanLock
	make -C .doom.d .guix-profile
	make -C .doom.d doomup
# =./guix/targets.mk=:2 ends here

# [[file:../README.org::*=./guix/targets.mk=][=./guix/targets.mk=:3]]
# ---------------------------------------------
# Copy installed Guix profiles

# Run with `GUIXHOST=ahost GUIXPROFILE=$HOME/.dotfiles/.doom.d/.guixprofile make -e guix-copy`
GUIXHOST=$(shell hostname)
GUIXUSER=$(shell whoami)
GUIXGUIXPROFILE=$(HOME)/.config/guix/current
# =./guix/targets.mk=:3 ends here

# [[file:../README.org::*=./guix/targets.mk=][=./guix/targets.mk=:4]]
.PHONY: guix-copy
guix-copy:
	@echo guix copy --to=$(GUIXUSER)@$(GUIXHOST) $(shell readlink -f $(GUIXGUIXPROFILE))

# then connect and install the top-level summary of the channel as a link in
# most cases, the dotfiles will need to be current

.PHONY: guix-copy-pull-sync guix-copy-pull-lock
guix-copy-pull-sync:
	@ssh $(GUIXUSER)@$(GUIXHOST) -- guix pull -L ./dc -C $(GUIXGUIXCHAN)
guix-copy-pull-lock:
	ssh $(GUIXUSER)@$(GUIXHOST) -- guix describe --format=channels > $(GUIXGUIXCHAN)
# =./guix/targets.mk=:4 ends here

# [[file:../README.org::*=./guix/targets.mk=][=./guix/targets.mk=:5]]
.PHONY: guix-dev-sync
guix-sync-dev:
	cp $(CHANNELS_FILE) $(CHANNELS_FILE).bak
	nmatch=$(grep -ne '^(define core-channels' | cut -f1 -d':')
	echo $nmatch

# $(CHANNELS_FILE): $(GUIXGUIXCHAN)

# .PHONY: guix-dev-pull
# guix-pull-dev: .config/guix/channels.scm env/dc-configs/guix/channels.scm
# 	guix pull -L ./env -C $(CHANNELS_FILE)
# =./guix/targets.mk=:5 ends here

# [[file:../README.org::*System Images][System Images:1]]
GPGISO_SCM="(@ (dc system images usb-gpg-tools) usb-gpg-tools)"

.PHONY: gpgiso
gpgiso:
	guix system -L ./dc image --image-type=iso9660 -e $(GPGISO_SCM)
# System Images:1 ends here

# [[file:../README.org::*Guix Home][Guix Home:1]]
GUIX_HOST=$(shell hostname)
GUIX_HOST_SYSTEM=./dc/dc/system/$(GUIX_HOST).scm
GUIX_HOST_HOME=./dc/dc/home/$(GUIX_HOST).scm

PHONY: ghbuild
ghbuild:
	guix home -L ./dc build $(GUIX_HOST_HOME)

.PHONY: ghcontainer
ghcontainer:
	guix home -L ./dc container $(GUIX_HOST_HOME)

# To just quickly extract a few files from the container
GUIX_HOST_HE="(@ (dc home kharis) kharis-home-environment)"

.PHONY: guixHomeContainer
guixHomeContainer:
	guix home -L ./dc container --share="$(MKDIR)" -e $(GUIX_HOST_HE)
# Guix Home:1 ends here

# [[file:../README.org::*=./guix/defaults.mk=][=./guix/defaults.mk=:2]]
ARES_PROFILE=.guix-profile-dev
ARES_GC_ROOT=$(MKDIR)/$(ARES_PROFILE)
ARES_SHELL=$(ARES_GUIX) shell -L ./env \
	guile-next guile-ares-rs \
	-e '(@ (dc-configs dev packages) guix-package)' \
	-e '(@ (dc-configs dev packages) channels-package)'
# =./guix/defaults.mk=:2 ends here
