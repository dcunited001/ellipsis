# [[file:../README.org::*=./guix/defaults.mk=][=./guix/defaults.mk=:1]]
GUIXGUIXBASE=$(HOME)/.config/guix/base-channels.scm
GUIXGUIXCHAN=$(HOME)/.config/guix/channels.scm

CHANNELS_FILE=$(DCPATH)/env/dc-configs/guix/channels.scm

# Temporary: I need a proper repl to refactor for restricted channels API
channels_unsafe ?= 0
channels_unsafe_eval =
ifeq ($(channels_unsafe),1)
    channels_unsafe_eval = --unsafe-channel-evaluation
endif

ARES_GUIXTM=guix time-machine -L ./env -C $(CHANNELS_FILE) $(channels_unsafe_eval)
ARES_GUIX=$(ARES_GUIXTM) --

# don't use these vars for GUIXTM
DCCHANNEL=$(abspath $(MKDIR)/dc)
GUIXPACKAGE=guix package -L $(DCCHANNEL)

# channel outputs
DC_SRC_LOAD_PATH=-L guix/env -L guix/dc

# inputs, testing and repl
DEV_ENV_LOAD_PATH=-L guix/env -L guix/dc -L guix/test

PULL_EXTRA_OPTIONS=
# --allow-downgrades

ROOT_MOUNT_MOUNT=/mnt
# =./guix/defaults.mk=:1 ends here
