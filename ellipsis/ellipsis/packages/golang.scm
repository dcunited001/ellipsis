(define-module (ellipsis packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix build-system copy)

  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages security-token)
  ;; #:use-module (gnu packages gcc)

  #:use-module (srfi srfi-1))


(define-public go-github-com-jfeliu007-goplantuml
  (package
    (name "go-github-com-jfeliu007-goplantuml")
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jfeliu007/goplantuml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qp3jnkb1zb0a2vfndcbxs9ixm0z4m3w79kjn7bfxv9n1fmq0w1s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jfeliu007/goplantuml/cmd/goplantuml"
      #:unpack-path "github.com/jfeliu007/goplantuml"))
    (propagated-inputs (list go-github-com-spf13-afero))
    (home-page "https://github.com/jfeliu007/goplantuml")
    (synopsis "GoPlantUML V2")
    (description
     "@code{PlantUML} Class Diagram Generator for golang projects.  Generates class
diagram text compatible with plantuml with the information of all structures and
interfaces as well as the relationship among them.")
    (license license:expat)))

(define-public d2-bin
  (let* ((bin-platform "linux-amd64")
         (bin-version "0.6.9")
         (bin-name (string-append "d2-v" bin-version "-" bin-platform ".tar.gz")))
    (package
      (name "d2-bin")
      (version "0.6.9")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/terrastruct/d2/releases/download/"
                      "v" version "/" bin-name))
                (sha256
                 (base32
                  "0hzlyyjzzdv8fna0z6c6jska81a27bn496namij0bnm2bc3afwjv"))))

      (build-system copy-build-system)
      (arguments
       (list
        #:substitutable? #f
        #:install-plan
        #~'(("bin/d2" "bin/")
            ("man/d2.1" "share/man/man1/"))))

      (home-page "https://oss.terrastruct.com/d2")
      (synopsis "Table of Contents")
      (description
       "The most convenient way to use D2 is to just run it as a CLI executable to
produce SVGs from @@code{.d2} files.")
      (license license:mpl2.0))))

;;** Codesearch

;;*** Setup

;; heard about this on a podcast and i was curious

;; -----------------------
;; cd /data/ecto/finance/openbb
;; export CSEARCHINDEX=$PWD/.csearchindex
;; cindex .
;; find . -name '*.ts*' -type f -print | xargs cgrep -n -i "atom"  | wc -l
;; -----------------------

;; it does seem fairly fast, but code changes, so you need a background
;; service (or a git hook), probably with multiple checkouts.

;; TODO: these are technically separate packages (and leaf-level CLI, almost
;; self-contained). Do they need to be bundled separately? Do they need to
;; follow the Guix go package naming conventions?

;; #:go go-1.17 ;; earlier, specifying go-1.14 used 1.17 anyways
;;
;; this changed after adding #:unpack-path, which probably means it wasn't
;; properly discovering the configuration/metadata for the main package

(define-public go-github-com-google-codesearch-cmd-cgrep
  (package
    (name "go-github-com-google-codesearch-cmd-cgrep")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/codesearch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jzgq0rf9qzng1mqlpx0ib7zwz5hk6lnj6fmp923xmkpyvqg0kcb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.14 ;; go.mod: 1.13
      #:install-source? #f
      #:import-path "github.com/google/codesearch/cmd/cgrep"
      #:unpack-path "github.com/google/codesearch"))
    (home-page "https://github.com/google/codesearch")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-google-codesearch-cmd-csearch
  (package
    (name "go-github-com-google-codesearch-cmd-csearch")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/codesearch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jzgq0rf9qzng1mqlpx0ib7zwz5hk6lnj6fmp923xmkpyvqg0kcb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.14 ;; go.mod: 1.13
      #:install-source? #f
      #:import-path "github.com/google/codesearch/cmd/csearch"
      #:unpack-path "github.com/google/codesearch"))
    (home-page "https://github.com/google/codesearch")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-google-codesearch-cmd-cindex
  (package
    (name "go-github-com-google-codesearch-cmd-cindex")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/codesearch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jzgq0rf9qzng1mqlpx0ib7zwz5hk6lnj6fmp923xmkpyvqg0kcb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.14 ;; go.mod: 1.13
      #:install-source? #f
      #:import-path "github.com/google/codesearch/cmd/cindex"
      #:unpack-path "github.com/google/codesearch"))
    (home-page "https://github.com/google/codesearch")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))


(define-public go-github-com-go-echarts-go-echarts-v2
  (package
    (name "go-github-com-go-echarts-go-echarts-v2")
    (version "2.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-echarts/go-echarts")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jfby88cngfckik0wva4wi18lh09b8ffgz74zxv9wh1l8kwnxzcw"))
       (modules '((guix build utils)))
       (snippet
        ;; It's a helper for go-build-system to compile import-path and
        ;; unpack-path when it struggles to find module.
        #~(begin
            (mkdir "v2")
            (for-each (lambda (f)
                        (rename-file f (string-append "v2/" (basename f))))
                      '("charts" "components" "datasets" "event"
                        "opts" "render" "templates" "types" "util"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-echarts/go-echarts/v2"
      #:unpack-path "github.com/go-echarts/go-echarts"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build) ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./...")))))
          (add-before 'check 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               ;; global_test.go calls util.GenerateUniqueID() which evokes from
               ;; the fourth dimension several identical moments of the first
               ;; kind
               (string-append "src/" import-path "/opts/global_test.go")))))))
    ;; unsure as to whether any of these need to be propagated
    (propagated-inputs (list go-github-com-kr-pretty
                             go-gopkg-in-yaml-v3))
    (native-inputs
     (list go-gopkg-in-check-v1
           go-github-com-davecgh-go-spew
           go-github-com-pmezard-go-difflib
           go-github-com-stretchr-testify-bootstrap))
    (home-page "https://github.com/go-echarts/go-echarts")
    (synopsis "The adorable charts library for Golang.")
    (description
     "In the Golang ecosystem, there are not many choices for data visualization
libraries.  The development of
@@url{https://github.com/go-echarts/go-echarts,go-echarts} aims to provide a
simple yet powerful data visualization library for Golang.
@@url{https://echarts.apache.org/,Apache ECharts} is an awesome charting and
visualization library, it supports adorable chart types and various interactive
features.  and there have many program languages interactive with Echarts, such
as @@url{https://github.com/pyecharts/pyecharts,pyecharts}, which
@@code{go-echarts} learns and has evolved a lot from, and the
@@url{https://github.com/Koooooo-7/echarts4j,echarts4j} either.")
    (license license:expat)))

(define-public go-gopkg-in-dnaeon-go-priorityqueue-v1
  (package
    (name "go-gopkg-in-dnaeon-go-priorityqueue-v1")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dnaeon/go-priorityqueue")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zv4y0v53s52qxqm3i0gm83zks4053hwn4ajhwd6cajvbzzkg971"))))
    (build-system go-build-system)
    (arguments
     (list ;; #:go go-1.21.3
      #:import-path "gopkg.in/dnaeon/go-priorityqueue.v1"))
    (home-page "https://github.com/dnaeon/go-priorityqueue")
    (synopsis "Simple and generic implementation of priority queues in Go")
    (description "@@code{go-priorityqueue} builds on top of container/heap, and
also adds various convenience methods for creating new priority queues,
predicates for testing whether the queue is empty, synchronization so it can
be safely used by multiple goroutines.")
    (license #f)))

(define-public go-gopkg-in-dnaeon-go-deque-v1 ;; go-github-com-dnaeon-go-deque-v1
  (package
    (name "go-gopkg-in-dnaeon-go-deque")
    (version "1.0.0-20250203064611-7d48f7299755")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dnaeon/go-deque")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15vcnl20fcv1cci7j0n9n4f2hgqv90sx88bnf4sg5miyd1nd3lpy"))))
    (build-system go-build-system)
    (arguments
     (list ;; #:go go-1.22
      #:import-path "gopkg.in/dnaeon/go-deque.v1"))
    (home-page "https://github.com/dnaeon/go-deque")
    (synopsis
     "A simple, generic, thread-safe implementation of double-ended queue in Go.")
    (description
     "This package provides a simple, generic, thread-safe implementation of in Go.")
    (license #f)))

(define-public go-gopkg-in-dnaeon-go-graph-v1
  (package
    (name "go-gopkg-in-dnaeon-go-graph-v1")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/dnaeon/go-graph.v1")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p99gq7d5by67lcs94a6a7z8s735ifnh22q02gihhfs1w40j52ci"))))
    (build-system go-build-system)
    (arguments
     (list ;; #:go go-1.23
      #:import-path "gopkg.in/dnaeon/go-graph.v1"
      ;; hmmmm
      #:embed-files #~(list "base\\.tpl" ;; .*/base.tpl
                            "base_(script|option|element).tpl"
                            "chart\\.tpl"
                            "header\\.tpl"
                            "page\\.tpl")))
    (propagated-inputs (list go-github-com-go-echarts-go-echarts-v2
                             go-gopkg-in-dnaeon-go-priorityqueue-v1
                             go-gopkg-in-dnaeon-go-deque-v1))
    (home-page "https://gopkg.in/dnaeon/go-graph.v1")
    (synopsis "Simple and generic graph library")
    (description
     "@@code{go-graph} provides utilities for specifying and analyzing graphs,
including topo-sort, shortest-path and other traversal methods. @@code{go-graph}
can export to @@{graphviz} and @@url{https://echarts.apache.org/,Apache ECharts}")
    (license license:bsd-2)))

(define-public makefile-graph
  (package
    (name "makefile-graph")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dnaeon/makefile-graph")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00ghy98960gxqggdg4mjfs88w1w5gvshgjhzh86vwivwi6wwqwm0"))))
    (build-system go-build-system)
    (arguments
     (list ;; #:go go-1.23
      #:import-path "github.com/dnaeon/makefile-graph/cmd/makefile-graph"
      #:unpack-path "github.com/dnaeon/makefile-graph"
      #:embed-files #~(list "base\\.tpl"
                            "base_(script|option|element).tpl"
                            "chart\\.tpl"
                            "header\\.tpl"
                            "page\\.tpl")))
    ;; i think propagated implicitly pulls these along for go, but echarts is
    ;; needed for embeds (see: pkgs that openapi-spec is propagated towards)
    (propagated-inputs (list go-github-com-go-echarts-go-echarts-v2
                             ;; go-gopkg-in-dnaeon-go-priorityqueue-v1
                             ;; go-gopkg-in-dnaeon-go-deque-v1
                             go-gopkg-in-dnaeon-go-graph-v1))
    (home-page "https://github.com/dnaeon/makefile-graph")
    (synopsis "makefile-graph")
    (description
     "@@code{makefile-graph} is a Go module and CLI application, which parses
@@url{https://www.gnu.org/software/make/,GNU Make}'s internal database and
generates a graph representing the relationships between the discovered Makefile
targets.")
    (license license:bsd-2)))

                                        ; go-gopkg-in-dnaeon-go-deque-v1
                                        ; go-gopkg-in-dnaeon-go-priorityqueue-v1
                                        ; go-github-com-go-echarts-go-echarts-v2
                                        ; go-gopkg-in-dnaeon-go-graph-v1
;; go-github-com-dnaeon-makefile-graph
