(use-modules (guix packages)
             (guix transformations)
             (gnu packages clojure)
             (gnu packages readline)
             (ellipsis packages clojure)
             )

;; ;; the main difference for clojure-ellipsis is that it's built using
;; ;; [tools.namespace "1.4.5"] and [tools.reader "1.3.7"]
(define upgrade-clojure-deps
;;   ;; p-i-r/spec doesn't operate by identity
;;   ;; (package-input-rewriting/spec `(("clojure" . ,(const clojure-ellipsis))))
   (package-input-rewriting `((,clojure . ,clojure-ellipsis))))

(packages->manifest
 (map ;; identity
      upgrade-clojure-deps
      ;; deps for clojure-tools
      (list
       rlwrap
       clojure-tools-deps-alpha
       ;; clojure-tools
       clojure-tools-cli
       clojure-data-xml
       clojure-data-codec
       clojure-test-check
       clojure-tools-gitlibs

       ;; algo-monads
       clojure-algo-monads
       clojure-tools-macro

       ;; no deps
       clojure-algo-generic
       clojure-core-match
       clojure-instaparse
       clojure-data-csv)))
