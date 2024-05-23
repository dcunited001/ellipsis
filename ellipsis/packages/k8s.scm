(define-module (ellipsis packages k8s)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module ((guix build gnu-build-system) :prefix gnu:)

  #:use-module (gnu packages base)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)

  #:use-module (nonguix build-system chromium-binary)

  #:use-module (srfi srfi-1))

;; TODO: copy the bin to bin/, set executable (not intended to run as user)
;; TODO: write a service?

(define-public helm-chartmap-jar
  (let* ((jar-version "1.2.0")
         (jar-name (string-append "helm-chartmap-" jar-version ".jar"))
         (jar-namespace "com.melahn.helm-chartmap")
         (jar-source "https://oss.sonatype.org/service/local/repositories/releases/content")
         (jar-url (string-join
                   (list jar-source
                         (string-replace-substring jar-namespace "." "/")
                         jar-version
                         jar-name) "/")))
    (package
      (name "helm-chartmap-jar")
      (version jar-version)
      (source (origin
                (method url-fetch)
                (uri jar-url)
                (sha256
                 (base32 "1b6vz6d9a4wlhfvngz6zfsrrm3fn0h4nlf0piyn8l55hxgb3a3r8"))))
      (build-system copy-build-system)
      (inputs `(("jre" ,icedtea)))
      (propagated-inputs '())
      (arguments
       (list
        #:install-plan
        ''(("." "share/java" #:include-regexp ("helm-chartmap.*jar$")))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'make-symlink
              (lambda* (#:key outputs #:allow-other-keys)
                (with-directory-excursion
                    (string-append #$output "/share/java")
                  (symlink (car (find-files "." "helm-chartmap")) "helm-chartmap.jar"))))

            (add-after 'make-symlink 'make-wrapper
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((wrapper (string-append #$output "/bin/helm-chartmap")))
                  (mkdir-p (string-append #$output "/bin"))
                  (with-output-to-file wrapper
                    (lambda _
                      (display
                       (string-append
                        "#!/bin/sh\n\n"
                        (assoc-ref inputs "jre") "/bin/java -jar "
                        #$output "/share/java/helm-chartmap.jar \"$@\"\n"))))
                  (chmod wrapper #o555)))))))
      (home-page "")
      (synopsis "")
      (description "")
      (license license:asl2.0))))

(define-public k0s-bin
  (let* ((bin-platform "amd64")
         (bin-version "1.29.3")
         (bin-name (string-append "k0s-v" bin-version
                                  "+k0s.0-"
                                  bin-platform)))

    ;; bin-name: k0s-v1.28.4+k0s.0-amd64
    ;; url: v1.28.4+k0s.0/k0s-v1.28.4+k0s.0-amd64

    (package
      (name "k0s-bin")
      (version bin-version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/k0sproject/k0s/releases/download/"
                      "v" version "+k0s.0/" bin-name))
                (sha256
                 (base32
                  "1kpch019kjd939wlq92v3fayh8dzand76ac65i2cjpzi5ync880k"))))

      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan ''(("." "bin/"
                           #:include-regexp ("k0s-.*$")))
        #:modules '((guix build copy-build-system)
                    (guix build utils)  ; for find-file
                    (srfi srfi-26))     ; for cut, a swappier curry

        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'make-executable
              (lambda _
                (for-each (cut chmod <> #o555)
                          (find-files "." "k0s-.*$"))))

            ;; from tcl's phase install-private-headers
            (add-after 'install 'make-symlink
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((bin (string-append (assoc-ref outputs "out")
                                          "/bin")))
                  (with-directory-excursion bin
                    (symlink (car (find-files "." "k0s-"))
                             "k0s")))))

            (add-after 'make-symlink 'install-completions
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (share (string-append out "/share"))
                       (k0s (string-append out "/bin/k0s")))
                  (mkdir-p (string-append share "/bash-completion/completions"))
                  (with-output-to-file
                      (string-append share "/bash-completion/completions/k0s")
                    (lambda _ (invoke k0s "completion" "bash")))))))))

      (inputs '())
      (propagated-inputs '())
      (home-page "https://k0sproject.io/")
      (synopsis "A statically compiled, zero friction kubernetes.")
      (description "k0s is an all-inclusive Kubernetes distribution, which is
configured with all of the features needed to build a Kubernetes cluster and
packaged as a single binary for ease of use.  k0s fits well in any cloud
environment, but can also be used in IoT gateways, Edge and Bare metal
deployments due to its simple design, flexible deployment options and modest
system requirements.")
      (license license:asl2.0))))
