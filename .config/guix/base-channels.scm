(use-modules (guix ci)
             (guix channels))

(list
 ;; %default-guix-channel
 (channel
  (name 'guix)
  (url "https://codeberg.org/guix/guix-mirror.git")
  (branch "master")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
 (channel
  (name 'sops-guix)
  (url "https://github.com/fishinthecalculator/sops-guix")
  (branch "main")
  (introduction
   (make-channel-introduction
    "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
    (openpgp-fingerprint
     "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
 (channel
  (name 'gocix)
  (url "https://github.com/fishinthecalculator/gocix")
  (branch "main")
  ;; don't pin to commit (rapid dev cycle, channel may merge)
  (introduction
   (make-channel-introduction
    "cdb78996334c4f63304ecce224e95bb96bfd4c7d"
    (openpgp-fingerprint
     "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (commit "7648a495767990d625cd0ca242cd634aa39bcc58")
  (introduction
   (make-channel-introduction
    "257cebd587b66e4d865b3537a9a88cccd7107c95"
    (openpgp-fingerprint
     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))))
