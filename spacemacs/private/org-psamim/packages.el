(setq org-psamim-packages
    '(
      ;; package names go here
      (org :location built-in)
      (org-babel :location local)
      ))

(defun org-psamim/post-init-org ()
  (progn
    (setq org-default-notes-file "~/Dropbox/notes/notes.org"
          org-log-done t
          org-clock-into-drawer t
          org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar")
          org-export-babel-evaluate nil
          org-confirm-babel-evaluate nil
          org-agenda-files (quote ("~/Dropbox/notes/todo.org"))
          org-directory "~/Dropbox/notes"
          org-archive-location "~/Dropbox/notes/archive/todo.org::"
    )

    (custom-set-variables
     ;; Open PDFs after Export with Zathura
     '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "zathura %s"))))
     '(org-agenda-files (quote ("~/Dropbox/notes/todo.org")))
     ;; '(org-agenda-ndays 7)
     ;; '(org-deadline-warning-days 14)
     ;; '(org-agenda-show-all-dates t)
     ;; '(org-agenda-skip-deadline-if-done t)
     ;; '(org-agenda-skip-scheduled-if-done t)
     ;; '(org-agenda-start-on-weekday nil)
     ;; '(org-reverse-note-order t)
     )
    ))

(defun org-psamim/init-org-babel ()
  (use-package org-babel
    :init
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
       ))
    ))
