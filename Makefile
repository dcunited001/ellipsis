##
# Ellipsis
#
# @file
# @version 0.1

ares:
	guix time-machine -C env/dc/channels.scm -- \
	shell -L ./env --pure --rebuild-cache guile-next guile-ares-rs \
	-e '(@ (dc packages) guix-from-my-channels)' \
	-e '(@ (dc packages) channels-package)' \
	-- \
	guile -L ./dc -L ./ellipsis -L ./env -c \
	"(begin (use-modules (guix gexp)) #;(load gexp reader macro globally) \
((@ (ares server) run-nrepl-server)))"

# building /gnu/store/kzlvlml36483dxljd853j87a8ajxp2k6-inferior-script.scm.drv...
# building package cache...
# building profile with 2 packages...
# guix shell: error: failed to evaluate expression '(@ (dc packages) channels-package)':
# In procedure abi-check: #<record-type <channel>>: record ABI mismatch; recompilation needed
# make: *** [Makefile:8: ares] Error 1


# end
