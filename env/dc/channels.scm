(define-module (dc channels)
  #:use-module (guix channels)
  #:export (my-channels))

(define my-channels
  (list (channel
         (name 'guix)
         (url "https://codeberg.org/guix/guix-mirror.git")
         (commit "ea874d0b3f8cd7539f03c0ea3e1b5184fe4d4132")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
         (name 'sops-guix)
         (url "https://github.com/fishinthecalculator/sops-guix")
         (branch "main")
         (commit "8c91903af984dad9998cfcf597e9aa873af6d4d1")
         (introduction
          (make-channel-introduction
           "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
           (openpgp-fingerprint
            "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
        (channel
         (name 'gocix)
         (url "https://github.com/fishinthecalculator/gocix")
         ;; (branch "main")
         (commit "4a58b1273e4970661c0edaff35ac80e4483ac847")
         (introduction
          (make-channel-introduction
           "cdb78996334c4f63304ecce224e95bb96bfd4c7d"
           (openpgp-fingerprint
            "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
        (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix")
         (commit "6c497a883d8d517987071f91a8423c4a59d6f6ff")
         (introduction
          (make-channel-introduction
           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
           (openpgp-fingerprint
            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
        (channel
         (name 'rde)
         (url "https://git.sr.ht/~abcdw/rde")
         (commit "bfe0605f72161d2648b194316a89de2902217ff7")
         (introduction
          (make-channel-introduction
           "257cebd587b66e4d865b3537a9a88cccd7107c95"
           (openpgp-fingerprint
            "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))))

my-channels
