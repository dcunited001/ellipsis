;;; rde --- Reproducible development environment.
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; SPDX-FileCopyrightText: 2024, 2025 Andrew Tropin <andrew@trop.in>

;; originally from rde

(define-module (dc-configs dev packages)
  #:use-module (rde lib file)
  #:use-module (dc-configs guix channels)
  #:use-module (rde packages guix)
  #:export (guix-package
            channels-package))

;; must make the original guix package separately first
(define guix-package
  (make-guix-package core-channels))

;; then build additional channel dependencies
(define channels-package
  (make-channels-package core-channels))

;; - (rde packages guix) exports procedures, doesn't refer directly to
;;   core-channels.
;;
;; - with -L, you must load the parent directory to get the intended name of
;;   the root module (in name-space), so the ./env separates this. the ./src
;;   in the rde project aids a similar purpose.
;;
;; - if not careful, channel consumers will end up polluting their
;;   GUILE_LOAD_PATH with modules in these file branches. the complexity here
;;   helps avoid that.
;;
;; - module namespace collisions are also possible (thus, rde-configs and the
;;   extra intermediate module paths ... i think?)
;;
;; - the extra "guix" and "dev" in the ./env/rde-configs/env/guix path allow
;;   for builds against other module namespaces (file trees) that could
;;   contain alternative environments.

;; I don't think I need the extra layers of abstraction
;; =============================================

;; See also ~akagi/guixrc for an example of maintaining GUIXTM profiles
;;
;; https://git.sr.ht/~akagi/guixrc/tree/master/item/configs/profiles.mk
;;
;; - uses guix.lock (on the profile links) to trigger further make tasks
;; - maintains two sets of profiles, each with a set of pinned channels-{fdsa}.scm

;; ./src/rde/packages/guix.scm does the same as the above with more granular
;; control to create (channels-union ...) with a single set of sources.
;;
;; https://git.sr.ht/~abcdw/rde/tree/master/src/rde/packages/guix.scm#L6
;;
;; - it's only used for development purposes: in Makefiles and referenced
;;   by other dev modules

;; =============================================
;; 
;; cat $(locate '/data/ecto/guix/channels/rde/*rde-configs*') 2>/dev/null \
;; | grep 'define-module' \
;; | sed -e 's/(define-module (\(.*\))/\1/g' \
;; | tr ' ' '/' | tree --fromfile .

;; .
;; └── rde-configs
;;     ├── ci
;;     ├── configs
;;     ├── env
;;     │   ├── channels
;;     │   ├── dev
;;     │   │   └── packages
;;     │   └── guix
;;     │       ├── channels
;;     │       └── channels-ci
;;     ├── hosts
;;     │   ├── cloud
;;     │   ├── ixy
;;     │   └── live
;;     ├── minimal
;;     ├── minimal-emacs
;;     └── users
;;         ├── abcdw
;;         └── guest

;; locate '/data/ecto/guix/channels/*rde-config*' | grep env | tree --fromfile .
;; 
;; rde
;; └── examples
;;     └── env
;;         ├── dev
;;         │   └── rde-configs
;;         │       └── env
;;         │           ├── channels.scm
;;         │           └── dev
;;         │               └── packages.scm
;;         └── guix
;;             └── rde-configs
;;                 └── env
;;                     └── guix
;;                         ├── channels-ci.scm
;;                         └── channels.scm

;; =============================================
;; 
;; cat $(locate '/data/ecto/guix/channels/rde/*env*') 2>/dev/null \
;; | grep 'define-module' \
;; | sed -e 's/(define-module (\(.*\))/\1/g' \
;; | tr ' ' '/' | tree --fromfile .

;; .
;; ├── rde
;; │   └── env
;; │       ├── dev
;; │       │   └── packages
;; │       └── guix
;; │           └── channels
;; └── rde-configs
;;     └── env
;;         ├── channels
;;         ├── dev
;;         │   └── packages
;;         └── guix
;;             ├── channels
;;             └── channels-ci

;; locate '/data/ecto/guix/channels/rde/*env*' | grep env | tree --fromfile .
;; 
;; rde
;; ├── env
;; │   ├── dev
;; │   │   └── rde
;; │   │       └── env
;; │   │           └── dev
;; │   │               └── packages.scm
;; │   └── guix
;; │       └── rde
;; │           └── env
;; │               └── guix
;; │                   └── channels.scm
;; └── examples
;;     └── env
;;         ├── dev
;;         │   └── rde-configs
;;         │       └── env
;;         │           ├── channels.scm
;;         │           └── dev
;;         │               └── packages.scm
;;         └── guix
;;             ├── rde
;;             │   └── env
;;             │       └── guix
;;             │           └── channels.scm
;;             └── rde-configs
;;                 └── env
;;                     └── guix
;;                         ├── channels-ci.scm
;;                         └── channels.scm
