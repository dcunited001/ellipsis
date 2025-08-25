GUIX_PROFILE=.guix-profile
MANIFEST=manifest.scm
CHAN=channels.scm
CHANLOCK=channels-lock.scm

GUIXTM=guix time-machine -C channels-lock.scm

# TODO: shouldn't be coupled to another project install path
ELCHANNEL=../ellipsis
DCCHANNEL=../dc
GUIXPACKAGE=guix package -L ${ELCHANNEL} -L ${DCCHANNEL}

# quoting issues:
# GUIXLOADARGS="-L ${ELCHANNEL} -L ${DCCHANNEL}"
# GUIXPACKAGE="guix package ${GUIXLOADARGS}"

# .guix-profile requires pulling $ELCHANNEL,$DCCHANNEL first

.guix-profile: manifest.scm
	$(GUIXPACKAGE) -m ${MANIFEST} -p ${GUIX_PROFILE}

#	# not using the dotfiles packages for now
#	guix package ${GUIXLOADARGS} -m ${MANIFEST} -p ${GUIX_PROFILE}

# .... i thought this would dump the profile's spec (or the manifest in the profile)
#.guix-profile.lock:
#	guix describe -p ${GUIX_PROFILE} --format=channels > .guix-profile.lock

# - update profile/lock
# - clean artifacts; how to clean out ~/.emacs.d? not within dir.

# grep --include=*Makefile --include=*.mk -C5 -re target /data/ecto/x.files/akagi/dotfiles/
# grep --include=*Makefile --include=*.mk -C5 -re all: /data/ecto/x.files/akagi/dotfiles/


# ---------------------------------------------
# from ./doom-set-env (if reconstructing a minimal env is necessary)
#
# EMAIL=aionfork@gmail.com
# SSH_AUTH_SOCK=/run/user/1000/gnupg/S.gpg-agent.ssh
# DOOMDIR=${HOME}.doom.d
# EMACS=${GUIX_PROFILE}/bin/emacs
# EMACSDIR=${HOME}/.emacs.doom
# DOOMLSP=lsp
# EMACSSOCKET=/run/users/1000/emacs/doom
