(setq psamim-org-packages
    '(
      (org :location built-in)
      (org-babel :location local)))

(defun psamim-org/post-init-org ()
  (progn
    (setq org-default-notes-file "~/Notes/notes.org"
          org-log-done t
          org-clock-into-drawer t
          org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar")
          org-export-babel-evaluate nil
          org-confirm-babel-evaluate nil
          org-agenda-files (quote ("~/Notes/todo.org"))
          org-directory "~/Notes"
          org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE"))
          org-archive-location "~/Notes/archive/todo.org::")
    (add-hook 'org-mode-hook
              (lambda ()
                ;; (setq buffer-face-mode-face '(:family "DejaVu Sans" :height 120))
                ;; (buffer-face-mode)
                ;; (spacemacs|diminish buffer-face-mode nil nil)
                (linum-relative-mode -1)
                ;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
                (visual-line-mode)
                (spacemacs|diminish visual-line-mode nil nil)
                (setq bidi-paragraph-direction 'nil)))

    (custom-set-variables
     ;; Open PDFs after Export with Zathura
     '(org-file-apps (quote ((auto-mode . emacs)
                             ("\\.mm\\'" . default)
                             ("\\.x?html?\\'" . "xdg-open %s")
                             ("\\.pdf\\'" . "xdg-open %s"))))

     '(org-agenda-files (quote ("~/Notes/todo.org")))
     ;; '(org-agenda-ndays 7)
     ;; '(org-deadline-warning-days 14)
     ;; '(org-agenda-show-all-dates t)
     ;; '(org-agenda-skip-deadline-if-done t)
     ;; '(org-agenda-skip-scheduled-if-done t)
     ;; '(org-agenda-start-on-weekday nil)
     ;; '(org-reverse-note-order t)
     )))

(defun psamim-org/init-org-babel ()
  (use-package org-babel
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (emacs-lisp . t)
       (python . t)
       (sh . t)
       (js . t)
       (latex . t)
       (gnuplot . t)
       (sql . t)
       (sh . t)))))
