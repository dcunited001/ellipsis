;;* Nyxt
(in-package #:nyxt-user)

;; to start an alternate configuration:
;; nyxt --profile nosave --socket /tmp/nyxt-nosave.socket

;;** Modules

;;** Editor

;;*** Theme

;; Alter the instance of the browser's theme slot
(defmethod customize-instance ((browser browser) &key)
           (setf (slot-value browser 'theme)
                 theme:+dark-theme+)
           (setf (slot-value browser 'external-editor-program)
                 ;; '("gmacsclient" "-c")
                 '("alacritty --command vim")))

;;** Keybindings

(define-configuration
 buffer
 ((default-modes
   (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))


(define-configuration
 buffer
 ((override-map
   (let ((map (make-keymap "override-map")))
     (define-key map "f1 f5" 'execute-command "C-space" 'nothing)))))

;;** Search

(defvar *dc/search-engines*
  (list
   '("g" "https://google.com/search?q=~a" "https://google.com")
   '("doi" "https://dx.doi.org/~a" "https://dx.doi.org/")
   '("py" "https://docs.python.org/3/search.html?q=~a"
     "https://docs.python.org/3")
   '("ansd" "https://docs.ansible.com/ansible/latest/index.html#stq=~a&stp=1"
     "https://docs.ansible.com/")
   '("ansg" "https://galaxy.ansible.com/search?deprecated=false&keywords=~a&order_by=-download_count&page=1"
     "https://galaxy.ansible.com/")
   '("gh" "https://github.com/~a" "Github (URL)")
   '("ghi" "https://github.com/search?q=~a&type=issues" "Github (Issues)")
   '("ghp" "https://github.com/search?q=~a&type=pullrequests" "Github (Pull Reqs)")
   '("ght" "https://github.com/search?q=~a&type=topics" "Github (Topics)")
   '("npm" "https://www.npmjs.com/search?q=~a"
     "https://www.npmjs.com/"))
  "List of search engines.")

;; TODO: consider using ©hárß for urls without search
;; '("á©" "https://galaxy.ansible.com/community") ; no URL for community

(define-configuration
 context-buffer
 "Go through the search engines above and make-search-engine out of them."
 ((search-engines
   (append
    (mapcar (lambda (engine) (apply 'make-search-engine engine))
            *dc/search-engines*)
    %slot-default%))))
