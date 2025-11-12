(use-modules (guix ci)
             (guix channels))

(list
 ;; %default-guix-channel
 (channel
   (name 'guix)
   (url "https://codeberg.org/guix/guix.git")
   (branch "master")
   (introduction
    (make-channel-introduction
     "9edb3f66fd807b096b48283debdcddccfea34bad"
     (openpgp-fingerprint
      "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
 ;; For RDE: last commit before tdlib moved to (gnu packages telegram)
 ;; (commit "2f12efaf3994edc23be816bb49dd2a349ca99ac9")
 ;; RDE specs this commit, but the build fails (nonguix)
 ;; (commit "92c63391ee25205be3b8525d5d1fe5b9f345f37f")
 (channel
   (name 'sops-guix)
   (url "https://github.com/fishinthecalculator/sops-guix")
   (branch "main")
   ;; (commit "89f46bc4686504763f49e6b34c596720d347d8da")
   (introduction
    (make-channel-introduction
     "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
     (openpgp-fingerprint
      "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
 (channel
   (name 'gocix)
   (url "https://github.com/fishinthecalculator/gocix")
   ;; don't pin to commit (rapid dev cycle, channel may merge)
   (branch "main")
   ;; (commit "78dbc472dabe62c76200964277b94d8e57c4db50")
   (introduction
    (make-channel-introduction
     "cdb78996334c4f63304ecce224e95bb96bfd4c7d"
     (openpgp-fingerprint
      "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
 (channel
   (name 'nonguix)
   (url "https://gitlab.com/nonguix/nonguix")
   ;; (commit "02270b585e7d641afabd86ee6eaf7a6aad2f5df7")
   (introduction
    (make-channel-introduction
     "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
     (openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 (channel
   (name 'rde)
   (url "https://git.sr.ht/~abcdw/rde")
   ;; (commit "5669a5f2e729635746bd9015e50bd9e3abe14b3a")
   (introduction
    (make-channel-introduction
     "257cebd587b66e4d865b3537a9a88cccd7107c95"
     (openpgp-fingerprint
      "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))))
