##
# Ellipsis
#
# @file
# @version 0.1

# maybe rebuild cache: --rebuild-cache

ares:
	guix time-machine -C env/dc/channels.scm -- \
	shell -L ./env --pure guile-next guile-ares-rs \
	-e '(@ (dc packages) guix-from-my-channels)' \
	-e '(@ (dc packages) channels-package)' \
	-- \
	guile -L ./dc -L ./ellipsis -L ./env -c \
	"(begin (use-modules (guix gexp)) #;(load gexp reader macro globally) \
((@ (ares server) run-nrepl-server)))"

# end
