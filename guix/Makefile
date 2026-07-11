##
# dcunited001/ellipsis guix
#
# @file
# @version 0.1

MKDIR      ?= $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

SHELL=/bin/sh
HOST=$(shell hostname)
GUIXHOST=$(shell hostname)
GUIXUSER=$(shell whoami)

SRC = $(MKDIR)/dc
TEST = $(MKDIR)/test
ENV = $(MKDIR)/env

CHANNELS_GUIX = $(HOME)/.config/guix/channels.scm
CHANNELS_LATEST = $(MKDIR)/channels-latest.scm
CHANNELS_LOCK = $(MKDIR)/channels-lock.scm
CHANNELS_DEV = $(ENV)/dc-configs/guix/channels.scm

CHANNELS ?= $(CHANNELS_LOCK)

GUIXTM_DEV=guix time-machine --unsafe-channel-evaluation -C $(CHANNELS_DEV)
GUILE_DEV=$(GUIXTM_DEV) -- shell guile guile-fibers --pure --preserve=.*LANG.* -- guile
GUIX_DEV=$(GUIXTM_DEV) --

GUIXTM=guix time-machine -C $(CHANNELS)
GUILE=$(GUIXTM) -- shell guile guile-fibers --pure --preserve=.*LANG.* -- guile
GUIX=$(GUIXTM) --

guix_load_path=-L $(MKDIR)/dc

# Temporary: I need a proper repl to refactor for restricted channels API
# channels_unsafe ?= 0
# channels_unsafe_eval =
# ifeq ($(channels_unsafe),1)
#     channels_unsafe_eval = --unsafe-channel-evaluation
# endif

# Note: may need to split these env/env so guix package is internalized first
ARES_ENV_PATH=-L $(MKDIR)/env -L $(MKDIR)/dc
ARES_LOAD_PATH=-L $(MKDIR)/env -L $(MKDIR)/dc -L $(MKDIR)/test

ARES_PROFILE=.guix-profile-dev
ARES_GC_ROOT=$(MKDIR)/$(ARES_PROFILE)
ARES_SHELL=$(GUIX_DEV) shell $(ARES_ENV_PATH) \
	guile-next guile-ares-rs \
	-e '(@ (dc-configs dev packages) guix-package)' \
	-e '(@ (dc-configs dev packages) channels-package)'


PULL_EXTRA_OPTIONS=
# --allow-downgrades

ROOT_MOUNT_MOUNT=/mnt

.PHONY: repl
repl:
	$(GUIX) repl $(guix_load_path) --listen=tcp:54321

.PHONY: ares ares-profile ares-impure
ares:
	$(ARES_SHELL) -- guile $(ARES_LOAD_PATH) -c \
	"(begin (use-modules (guix gexp)) #;(load gexp reader macro globally) \
	((@ (ares server) run-nrepl-server)))"

ares-impure:
	guix shell -p $(ARES_PROFILE) -- guile $(ARES_LOAD_PATH) -c \
	"(begin (use-modules (guix gexp)) #;(load gexp reader macro globally) \
	((@ (ares server) run-nrepl-server)))"

# $(ARES_GC_ROOT):
ares-profile:
	@echo $(ARES_SHELL) --root=$(ARES_GC_ROOT)

# TODO: write an env/sync task to emit channel spec to (dc-configs guix channels)
# env/sync: env/guix/rde/env/guix/channels.scm

# TODO: figure out how to get a separate GC link, so the GUIXTM store items
# aren't prematurely purged (like a basic emacs profile or something)

# guix-git-touch .config/guix/current

# $(CHANNELS_LATEST):

# TODO: merge this with below `guix pull`
# 
# - updating on multiple system leans on git timestamps
# - $(ARES_PROFILE) isn't moved/recreated like $(CHANNELS_GUIX) link
$(CHANNELS_LOCK).lock $(CHANNELS_DEV).lock: $(CHANNELS_GUIX)
	$(error update $@.scm.lock)
	touch $@.scm.lock 

.PHONY: pull pull-sync pull-lock
pull: pull-sync pull-lock
pull-sync:
	guix pull -L $(MKDIR)/dc -C $(CHANNELS_LATEST)
pull-lock:
	guix describe --format=channels > $(CHANNELS_LOCK)
	guix describe --format=channels > $(CHANNELS_GUIX)

.PHONY: up up-sync up-lock
up: up-sync up-lock
up-sync:
	guix pull -L $(MKDIR)/dc -C $(CHANNELS_LOCK)
up-lock:
	guix describe --format=channels > $(CHANNELS_GUIX)

.PHONY: upgrade
upgrade:
	make up
	make ares-profile

# Run with `GUIXHOST=ahost GUIXPROFILE=$HOME/.dotfiles/.doom.d/.guixprofile make -e copy`

.PHONY: copy
copy:
	@echo guix copy --to=$(GUIXUSER)@$(GUIXHOST) $(shell readlink -f $(CHANNELS_GUIX))

# then connect and install the top-level summary of the channel as a link in
# most cases, the dotfiles will need to be current

.PHONY: copy-pull-sync copy-pull-lock
copy-pull-sync:
	@ssh $(GUIXUSER)@$(GUIXHOST) -- guix pull -L $(MKDIR)/dc -C $(CHANNELS_LOCK)
copy-pull-lock:
	ssh $(GUIXUSER)@$(GUIXHOST) -- guix describe --format=channels > $(CHANNELS_LOCK)

.PHONY: dev-sync
sync-dev:
	cp $(CHANNELS_DEV) $(CHANNELS_DEV).bak
	nmatch=$(grep -ne '^(define core-channels' | cut -f1 -d':')
	echo $nmatch

guixexpr="(@ (dc system images usb-gpg-tools) usb-gpg-tools)"
guixsystem=$(SRC)/dc/system/images/usb-gpg-tools.scm

GUIX_ISO_CMD=guix system -L $(MKDIR)/dc image

.PHONY: iso iso-gpg pxe
iso:
	$(GUIX_ISO_CMD) --image-type=iso9660 -f $(guixsystem)

iso-gpg:
	guixexpr="(@ (dc system images usb-gpg-tools) usb-gpg-tools)"
	$(GUIX_ISO_CMD) --image-type=iso9660 -e $(guixexpr)

pxe:
	guixexpr="(@ (dc system images pxe-boot-tools) pxe-boot-tools)"
	$(GUIX_ISO_CMD) --image-type=iso9660 -f $(guixexpr)

GUIX_HOST=$(shell hostname)

.PHONY: host
host:
	sudo guix system reconfigure -L $(MKDIR)/dc image --image-type=iso9660 -f $(guixexpr)

GUIX_HOST_SYSTEM=$(MKDIR)/dc/dc/system/$(GUIX_HOST).scm
GUIX_HOST_HOME=$(MKDIR)/dc/dc/home/$(GUIX_HOST).scm

PHONY: ghbuild
ghbuild:
	guix home -L $(MKDIR)/dc build $(GUIX_HOST_HOME)

.PHONY: ghcontainer
ghcontainer:
	guix home -L $(MKDIR)/dc container $(GUIX_HOST_HOME)

# To just quickly extract a few files from the container
GUIX_HOST_HE="(@ (dc home kharis) kharis-home-environment)"

.PHONY: guixHomeContainer
guixHomeContainer:
	guix home -L $(MKDIR)/dc container --share="$(MKDIR)" -e $(GUIX_HOST_HE)


# end
