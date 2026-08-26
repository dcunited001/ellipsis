;;; rde --- Reproducible development environment.
;;;
;;; SPDX-FileCopyrightText: 2024, 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;; originally from rde

(define-module (dc-configs guix channels)
  #:use-module (guix channels)
  #:export (core-channels))

(define core-channels
  (list (channel
          (name 'guix)
          (url "https://codeberg.org/guix/guix.git")
          (branch "master")
          (commit "18e73c792281e61c3813a99d662fbde108cf6ec8")
          (introduction
           (make-channel-introduction
            "e42227e1c7e7055e27cecada52ec801a75e44909"
            (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
          (name 'sops-guix)
          (url "https://github.com/fishinthecalculator/sops-guix")
          (branch "main")
          (commit "c53e27e533836ea8595626ba6796dee5362f8c4a")
          (introduction
           (make-channel-introduction
            "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
            (openpgp-fingerprint
             "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
        (channel
          (name 'gocix)
          (url "https://github.com/fishinthecalculator/gocix")
          (branch "main")
          (commit "5cbc7d0cb911dd27eb364d350ac5a1ef43308316")
          (introduction
           (make-channel-introduction
            "cdb78996334c4f63304ecce224e95bb96bfd4c7d"
            (openpgp-fingerprint
             "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
        (channel
          (name 'nonguix)
          (url "https://gitlab.com/nonguix/nonguix")
          (branch "master")
          (commit "73baab37361b3a81f326aa3fdec78840f5acc577")
          (introduction
           (make-channel-introduction
            "c15e19cdbdfdfddacdae865741809af4fa86a665"
            (openpgp-fingerprint
             "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
        (channel
          (name 'rde)
          (url "https://git.sr.ht/~abcdw/rde")
          (branch "master")
          (commit "70a1881f09c939792eb2ed932dded1f16291a59f")
          (introduction
           (make-channel-introduction
            "47fb718f2b1a3f68a03c51a2740c1ada4052bd3c"
            (openpgp-fingerprint
             "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))))

core-channels
