(setq psamim-defaults-packages
      '(ranger
        centered-window-mode))

(defun psamim-defaults/init-ranger ()
  (use-package ranger
    :config (progn
              (define-key ranger-mode-map (kbd "M-n") 'ranger-new-tab)
              (define-key ranger-mode-map (kbd "M-l") 'ranger-next-tab)
              (define-key ranger-mode-map (kbd "M-h") 'ranger-prev-tab))))

(defun psamim-defaults/init-centered-window-mode ()
  (use-package centered-window-mode
    :config (progn (centered-window-mode t))))
