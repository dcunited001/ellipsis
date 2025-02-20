(define-module (ellipsis packages clojure)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  ;; #:use-module ((guix licenses #:prefix license:))
  
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages clojure)
  
  #:use-module (ice-9 match))

;; (src) core.specs.alpha: 2.62
;; (src) data.generators: 1.0.0
;; (src) java.classpath: 1.0.0
;; (src) spec.alpha: 0.3.218
;; (src) test.check: 1.1.1
;; (src) test.generative: 1.0.0
;; (src) tools.namespace: 1.0.0 => 1.4.5 
;; (src) tools.reader: 1.3.2 => 1.3.7

(define-public clojure-ellipsis
  (let* ((lib (lambda (prefix version hash)
                (origin (method url-fetch)
                        (uri (string-append "https://github.com/clojure/"
                                            prefix version ".tar.gz"))
                        (sha256 (base32 hash)))))
         ;; The libraries below are needed to run the tests.
         (libraries
          `(("core-specs-alpha-src"
             ,(lib "core.specs.alpha/archive/v"
                   "0.2.62"
                   "0v6nhghsigpzm8y7dykfm318q5dvk5l8sykmn1hr0qgs1jsjqh9j"))
            ("data-generators-src"
             ,(lib "data.generators/archive/data.generators-"
                   "1.0.0"
                   "0s3hf1njvs68b8igasikvzagzqxl0gbri7w2qhzsypkhfh60v2cp"))
            ("java-classpath-src"
             ,(lib "java.classpath/archive/java.classpath-"
                   "1.0.0"
                   "178zajjsc9phk5l61r8w9hcpk0wgc9a811pl7kjgvn7rg4l7fh7j"))
            ("spec-alpha-src"
             ,(lib "spec.alpha/archive/v"
                   "0.3.218"
                   "0h5nd9xlind1a2vmllr2yfhnirgj2pm5dndgqzrly78l5iwcc3wa"))
            ("test-check-src"
             ,(lib "test.check/archive/v"
                   "1.1.1"
                   "0kx8l79mhpnn94rpsgc7nac7gb222g7a47mzrycj8crfc54wf0c1"))
            ("test-generative-src"
             ,(lib "test.generative/archive/test.generative-"
                   "1.0.0"
                   "0yy2vc38s4j5n94jdcjx1v7l2gdq0lywam31id1jh07sx37lv5il"))
            ;; 1.0.0 => 1.4.5
            ("tools-namespace-src"
             ;; url changed "tools.namespace/archive/tools.namespace-"
             ;; https://github.com/clojure/tools.namespace/archive/refs/tags/v1.4.5.tar.gz
             ,(lib "tools.namespace/archive/v"
                   "1.4.5"
                   "1vnzvk6w7vv1yhmxlrxadjbyx2gvnsj6q9a7gcsw42k18vqsk8pp"))
            ;; 1.3.2 => 1.3.7
            ("tools-reader-src"
             ,(lib "tools.reader/archive/v"
                   "1.3.7"
                   "0g2ckj890ljc8337hyfq0wsjsh5j3j2w5xkw99vig2c5fb29iq6h"))))
         (library-names (match libraries
                          (((library-name _) ...)
                           library-name))))
    (package
      (inherit clojure)
      (name "clojure-ellipsis")
      (version "1.11.1")
      (native-inputs libraries))))
