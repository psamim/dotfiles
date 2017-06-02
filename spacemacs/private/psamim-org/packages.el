(setq psamim-org-packages
    '(
      (org :location built-in)
      (org-babel :location local)))

(defun psamim-org/post-init-org ()
  (progn
    (setq org-default-notes-file "~/Notes/notes.org"
          org-log-done t
          org-image-actual-width '(700)
          org-clock-into-drawer t
          org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar")
          org-export-babel-evaluate nil
          org-confirm-babel-evaluate nil
          org-agenda-files (quote ("~/Notes/todo.org"))
          org-directory "~/Notes"
          org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE"))
          org-archive-location "~/Notes/archive/todo.org::"
          bidi-paragraph-direction t)

    (add-hook 'org-mode-hook
              (lambda ()
                (visual-line-mode)
                (spacemacs/toggle-vi-tilde-fringe-off)
                (spacemacs/toggle-line-numbers-off)
                (setq bidi-paragraph-direction t)))

    (custom-set-variables
     ;; Open PDFs after Export with Zathura
     '(org-file-apps (quote ((auto-mode . emacs)
                             ("\\.mm\\'" . default)
                             ("\\.x?html?\\'" . "xdg-open %s")
                             ("\\.pdf\\'" . "xdg-open %s"))))
     '(org-refile-targets '((org-agenda-files :maxlevel . 1)))
     '(org-outline-path-complete-in-steps nil)         ; Refile in a single go
     '(org-refile-use-outline-path t)                  ; Show full paths for refiling
     '(org-agenda-files (quote ("~/Notes/todo.org")))
     '(org-agenda-custom-commands
       (quote (("u" alltodo "" ((org-agenda-overriding-header "Next Actions"))))))
     '(org-agenda-ndays 7)
     '(org-deadline-warning-days 5)
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
