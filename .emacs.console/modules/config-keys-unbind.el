(defun dc/unbind-keys (key-names &optional keymap)
  (seq-do (lambda (key)
            (if keymap
                (unbind-key key keymap)
              (unbind-key key)))
          key-names))
(dc/unbind-keys
 '("<f2> 2" "<f2> b" "<f2> s" "<f2> <f2>"
   "<f10>" "M-<f10>" "<f11>"
   "<f3>" "<f4>"))

(provide 'config-keys-unbind)
