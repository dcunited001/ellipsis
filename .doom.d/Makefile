GUIX_PROFILE=.guix-profile
MANIFEST=manifest.scm

# from ./doom-set-env (if reconstructing a minimal env is necessary)
#
# EMAIL=aionfork@gmail.com
# SSH_AUTH_SOCK=/run/user/1000/gnupg/S.gpg-agent.ssh
# DOOMDIR=${HOME}.doom.d
# EMACS=${GUIX_PROFILE}/bin/emacs
# EMACSDIR=${HOME}/.emacs.doom
# DOOMLSP=lsp
# EMACSSOCKET=/run/users/1000/emacs/doom

# TODO: shouldn't be coupled to another project install path
ELCHANNEL=../ellipsis/ellipsis
DCCHANNEL=../dc/dc
GUIXLOADARGS="-L ${ELCHANNEL} -L ${DCCHANNEL}"

# .guix-profile requires pulling $ELCHANNEL,$DCCHANNEL first

.guix-profile:
	echo guix package ${GUIXLOADARGS} -m ${MANIFEST} -p ${GUIX_PROFILE}
.guix-profile.lock:
	echo guix describe -p ${GUIX_PROFILE} --format=channels > .guix-profile.lock

# - update profile/lock
# - clean artifacts; how to clean out ~/.emacs.d? not within dir.

