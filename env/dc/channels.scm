(define-module (dc channels)
  #:use-module (guix channels)
  #:export (%dc-lock-channels))

(define %dc-lock-channels
  (list (channel
         (name 'guix)
         (url "https://codeberg.org/guix/guix.git")
         (branch "master")
         (commit
          "679618893dbe9e9f32085db41356bebef7ddc4df")
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
          "c2897f21a86077aeb50b5d179db6d0bd99683830")
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
          "211a55900940043d061fced1068efa7ccf016786")
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
          "841777ffcac40ad0a5a9e0451c05079a9cbcf139")
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
          "a0740b46c74210ea972560e07f33fb493eb65c78")
         (introduction
          (make-channel-introduction
           "257cebd587b66e4d865b3537a9a88cccd7107c95"
           (openpgp-fingerprint
            "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))))

%dc-lock-channels
