(define-module (ellipsis packages android)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (guix build-system copy)

  #:use-module (gnu packages base)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)

  ;; #:use-module (gnu packages gcc)

  #:use-module (srfi srfi-1))

;; git-repo on guix is 2.4.1, but should be 2.39
;; - the package needs a lot of work (no gexp's)

;; the repo bin loads from /data/ecto/git-repo which is checked out there
;; - as a hack, a symlink from /data/ecto/git-repo/python -> guix python3
;; - this allows sync, which is better than other hacks i was using
;; - 2.4.1 is expecting python2 and now python3 is in my path

;; building the package at 2.39

;; TODO: other packages set HOME to /tmp when running tests
;;   - env vars needs to be set in each phase
;;   - git-repo pulls it's own source and interacts with the system
;;     in weird ways, so this may not work.
;; the build fails two tests
;;   - test a git commit: no git user in git config
;;   - something with GnuPG (repo checks for a signed binary on auto-update)
;;   - gen. python errors on many others.

;; so, the ad-hoc patches here need to be updated
;;   - replace with gexp where possible

;; ----------------------------------

;; i really don't understand how so many developers don't need this tool (or
;; something like it). it's independent of any package manager. i'm not using
;; it as designed, but it helps a ton with referencing code and navigating
;; project subdirectories.
;;
;; i guess i just don't know whatever that "something like it" is; sourcegraph
;; is one option; copilot is another (bad) option for /reading/ code; and
;; "having better things to do outside work" is also a valid option.

;; ----------------------------------


(define-public git-repo-python3         ; temp pkg name
  (package
    (name "git-repo")
    (version "2.39")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gerrit.googlesource.com/git-repo")
             (commit (string-append "v" version))))
       (file-name (string-append "git-repo-" version "-checkout"))
       (sha256
        (base32 "0khg1731927gvin73dcbw1657kbfq4k7agla5rpzqcnwkk5agzg3"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-executable-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (git (assoc-ref inputs "git"))
                    (ssh (assoc-ref inputs "ssh")))
               (substitute* '("repo" "git_command.py")
                 (("^GIT = 'git'")
                  (string-append "GIT = '" git "/bin/git'")))
               (substitute* "git_config.py"
                 ((" command_base = \\['ssh',")
                  (string-append " command_base = ['" ssh "/bin/ssh',")))
               #t)))
         (add-before 'build 'do-not-self-update
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Setting the REPO_MAIN variable to an absolute file name is
             ;; enough to have _FindRepo return the store main.py file.  The
             ;; self update mechanism is activated with the call to _Init() in
             ;; main(), so we bypass it.

             ;; Ticket requesting upstream to provide a mean to disable the
             ;; self update mechanism:
             ;; https://bugs.chromium.org/p/gerrit/issues/detail?id=12407.
             (let* ((out (assoc-ref outputs "out"))
                    (repo-main (string-append out "/share/git-repo/main.py")))
               (substitute* "repo"
                 (("^REPO_MAIN = .*")
                  (format #f "REPO_MAIN = ~s~%" repo-main))
                 ((" _Init\\(args, gitc_init=\\(cmd ==.*" all)
                  (string-append "True #" all)))
               ;; Prevent repo from trying to git describe its version from
               ;; the (disabled) self updated copy.
               (substitute* "git_command.py"
                 (("ver = getattr\\(RepoSourceVersion.*")
                  (format #f "ver = ~s~%" ,version)))
               (substitute* "subcmds/version.py"
                 (("rp_ver = .*")
                  (format #f "rp_ver = ~s~%" ,version)))
               ;; Prevent repo from adding its (disabled) self update copy to
               ;; the list of projects to fetch when using 'repo sync'.
               (substitute* "subcmds/sync.py"
                 (("to_fetch\\.extend\\(all_projects\\).*" all)
                  (string-append "#" all))
                 (("self\\._Fetch\\(to_fetch")
                  "self._Fetch(all_projects")
                 (("_PostRepoFetch\\(rp, opt\\.repo_verify).*" all)
                  (string-append "#" all))))))
         (delete 'build)                ; nothing to build
         (add-before 'check 'configure-git
           (lambda _
             (setenv "HOME" (getcwd))
             (invoke "git" "config" "--global" "user.email" "you@example.com")
             (invoke "git" "config" "--global" "user.name" "Your Name")))
         (replace 'check
           (lambda _
             (invoke "./run_tests")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin-dir (string-append out "/bin"))
                    (repo-dir (string-append out "/share/" ,name)))
               (mkdir-p bin-dir)
               (mkdir-p repo-dir)
               (copy-recursively "." repo-dir)
               (delete-file-recursively (string-append repo-dir "/tests"))
               (symlink (string-append repo-dir "/repo")
                        (string-append bin-dir "/repo"))
               #t))))))
    (inputs
     ;; TODO: Add git-remote-persistent-https once it is available in guix
     `(("git" ,git)
       ("ssh" ,openssh)))
    (native-inputs
     `(("pytest" ,python-pytest)))
    (home-page "https://code.google.com/p/git-repo/")
    (synopsis "Helps to manage many Git repositories")
    (description "Repo is a tool built on top of Git.  Repo helps manage many
Git repositories, does the uploads to revision control systems, and automates
parts of the development workflow.  Repo is not meant to replace Git, only to
make it easier to work with Git.  The repo command is an executable Python
script that you can put anywhere in your path.")
    (license license:asl2.0)))
