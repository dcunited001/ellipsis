(define-configuration tailor:tailor-mode
  ((tailor:auto-p :time)
   (tailor:light-theme-threshold (* 8 60 60))
   (tailor:dark-theme-threshold (* 21.5 60 60))
   (tailor:main '(modus-operandi . modus-vivendi))
   (tailor:themes
    (list
     (make-instance 'tailor:user-theme
                    :name 'modus-operandi
                    :background-color "white"
                    :on-background-color "black"
                    :primary-color "#093060"
                    :secondary-color "#dfdfdf"
                    :on-secondary-color "#f0f0f0"
                    :accent-color "#8f0075"
                    :on-accent-color "#005a5f"
                    :font-family "Iosevka")
     (make-instance 'tailor:user-theme
                    :name 'modus-vivendi
                    :dark-p t
                    :background-color "black"
                    :on-background-color "white"
                    :primary-color "#c6eaff"
                    :secondary-color "#323232"
                    :on-secondary-color "#a8a8a8"
                    :accent-color "#afafef"
                    :on-accent-color "#a8a8a8"
                    :font-family "Iosevka")))))
