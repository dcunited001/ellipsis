;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Alexey Abramov <levenson@mmer.org>

(define-module (ellipsis services vpn)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (nongnu packages vpn)
  #:export (zerotier-one-service-type
            zerotier-one-configuration))
 
(define-record-type* <zerotier-one-configuration>
  zerotier-one-configuration make-zerotier-one-configuration
  zerotier-one-configuration?
  (auto-start? zerotier-one-configuration-auto-start? (default #f)))

(define %zerotier-action-join
  (shepherd-action
   (name 'join)
   (documentation "Join a network")
   (procedure #~(lambda (running network)
                  (let* ((zerotier-cli (string-append #$zerotier "/sbin/zerotier-cli"))
                         (cmd (string-join (list zerotier-cli "join" network)))
                         (port (open-input-pipe cmd))
                         (str (get-string-all port)))
                    (display str)
                    (status:exit-val (close-pipe port)))))))

(define %zerotier-action-leave
  (shepherd-action
   (name 'leave)
   (documentation "Leave a network")
   (procedure #~(lambda (running network)
                  (let* ((zerotier-cli (string-append #$zerotier "/sbin/zerotier-cli"))
                         (cmd (string-join (list zerotier-cli "leave" network)))
                         (port (open-input-pipe cmd))
                         (str (get-string-all port)))
                    (display str)
                    (status:exit-val (close-pipe port)))))))

(define zerotier-one-shepherd-service
  (lambda (config)
    (list (shepherd-service
           (documentation "ZeroTier One daemon.")
           (provision '(zerotier-one))
           (requirement '(networking))
           (actions (list %zerotier-action-join
                          %zerotier-action-leave))
           (start #~(make-forkexec-constructor
                     (list (string-append #$zerotier "/sbin/zerotier-one"))))
           (stop #~(make-kill-destructor))
           (auto-start? (zerotier-one-configuration-auto-start? config))))))

(define zerotier-one-service-type
  (service-type (name 'zerotier-one)
                (description "ZeroTier One daemon.")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          zerotier-one-shepherd-service)))
                (default-value (zerotier-one-configuration))))
