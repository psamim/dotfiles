(setq psamim-defaults-packages
      '(ranger
        sublimity))

(defun psamim-defaults/post-init-ranger ()
  (use-package ranger
    :config (progn
              (ranger-override-dired-mode t)
              (setq ranger-footer-delay 0.7)
              (define-key ranger-mode-map (kbd "M-n") 'ranger-new-tab)
              (define-key ranger-mode-map (kbd "M-l") 'ranger-next-tab)
              (define-key ranger-mode-map (kbd "M-h") 'ranger-prev-tab))))

(defun psamim-defaults/init-sublimity ()
  (use-package sublimity
    :config (progn
              (require 'sublimity)
              (require 'sublimity-scroll)
              (setq sublimity-attractive-centering-width 110)
              (require 'sublimity-attractive)
              (sublimity-mode 1)
              )))
