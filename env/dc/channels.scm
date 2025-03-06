(define-module (dc channels)
  #:use-module (guix channels)
  #:export (my-channels))

(define my-channels
  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (commit "f785ff154c08e842b89145d9d47b20203d797e33")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
         (name 'nonguix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (commit "d0b6099324cd47961064bd96ae830c4ca6d1bada")
         (introduction
          (make-channel-introduction
           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
         (name 'rde)
         (url "https://git.sr.ht/~abcdw/rde")
         (commit "8600655bbdc399b6fc682755d4d63f3709f562be")
         (introduction
          (make-channel-introduction
           "257cebd587b66e4d865b3537a9a88cccd7107c95"
           (openpgp-fingerprint
            "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))))

my-channels
