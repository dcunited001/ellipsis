(define-module (dc channels)
  #:use-module (guix channels)
  #:export (my-channels))

(define my-channels
  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (branch "master")
         (commit
          "3355de608cb2267435c2592fc7dc76a1dcc5c02d")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
         (name 'rde)
         (url "https://git.sr.ht/~abcdw/rde")
         (branch "master")
         (commit
          "eaf0ec48cb06ee5dea1f86a502255e14c2b3973b")
         (introduction
          (make-channel-introduction
           "257cebd587b66e4d865b3537a9a88cccd7107c95"
           (openpgp-fingerprint
            "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))))

my-channels
