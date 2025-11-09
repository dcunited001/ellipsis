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
          (commit
           "54deb87a79e5c2705567f4c68b119b659af15119")
          (introduction
           (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
          (name 'sops-guix)
          (url "https://github.com/fishinthecalculator/sops-guix")
          (branch "main")
          (commit
           "d988b2b31b9a51f081c88758649a2ff4064762de")
          (introduction
           (make-channel-introduction
            "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
            (openpgp-fingerprint
             "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
        (channel
          (name 'gocix)
          (url "https://github.com/fishinthecalculator/gocix")
          (branch "main")
          (commit
           "d36624d0b09eacb5f56c3cdcead25bab8976d0ac")
          (introduction
           (make-channel-introduction
            "cdb78996334c4f63304ecce224e95bb96bfd4c7d"
            (openpgp-fingerprint
             "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
        (channel
          (name 'nonguix)
          (url "https://gitlab.com/nonguix/nonguix")
          (branch "master")
          (commit
           "74b20a74f9b37944f9532f5b649e962b50068faf")
          (introduction
           (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
             "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
        (channel
          (name 'rde)
          (url "https://git.sr.ht/~abcdw/rde")
          (branch "master")
          (commit
           "c5aea8d9c81db29f3e4d9ea5a6a152ece11c26cc")
          (introduction
           (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
             "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))))

core-channels
