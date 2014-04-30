(require 'org)
(setq org-default-notes-file "~/.orgs/notes.org"
      org-log-done t)

(require 'epa-file)
(epa-file-enable)
(setq epa-file-select-keys nil)

(defun my-org-mode-hook()
  (setq bidi-paragraph-direction nil)
  (auto-complete-mode t)
  (flyspell-mode t))

(add-hook 'org-mode-hook 'my-org-mode-hook)

(setq org-todo-keywords '("TODO" "NEXT" "WAITING" "DONE"))

(defvar ash-org-current-task-loc nil
"A cons of the buffer & location of the current task")

(defadvice org-clock-in (after ash-org-mark-task activate)
"When the user clocks in, bind F9 to go back to the worked on task."

(setq ash-org-current-task-loc (cons (current-buffer)
                                     (point)))
(define-key global-map [f9] (lambda ()
                              (interactive)
                              (switch-to-buffer
                               (car ash-org-current-task-loc))
                              (goto-char
                               (cdr ash-org-current-task-loc)))))

(require 'ob)
(org-babel-do-load-languages
'org-babel-load-languages
'((plantuml . t)))

(setq org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar"))


; Custom agendas and trees
(setq org-agenda-custom-commands
      (quote (
        ("un" "@uni NEXT" tags-tree "@uni+TODO=\"NEXT\"")
        ("ut" "@uni TODO" tags-tree "@uni+TODO=\"TODO\"")
        ("ua" "@uni ALL" tags-tree "@uni+TODO=\"NEXT\"|@uni+TODO=\"TODO\"")
        ("wn" "@work NEXT" tags-tree "@work+TODO=\"NEXT\"")
        ("wt" "@work TODO" tags-tree "@work+TODO=\"TODO\"")
        ("wa" "@work ALL" tags-tree "@work+TODO=\"NEXT\"|@work+TODO=\"TODO\"")
        ("mn" "@me NEXT" tags-tree "@me+TODO=\"NEXT\"|@tasks+TODO=\"NEXT\"")
        ("mt" "@me TODO" tags-tree "@me+TODO=\"TODO\"|@tasks+TODO=\"TODO\"")
        ("ma" "@me ALL" tags-tree "@me+TODO=\"TODO\"|@tasks+TODO=\"TODO\"|@me+TODO=\"NEXT\"|@tasks+TODO=\"NEXT\"")

       ("wg" "Work Agenda"
         (
          (tags-todo "@work+TODO=\"NEXT\"")
          (tags-todo "@work+TODO=\"TODO\"")
          )
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block

        ("ug" "Uni Agenda"
         (
          (tags-todo "@uni+TODO=\"NEXT\"")
          (tags-todo "@uni+TODO=\"TODO\"")
          )
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block

        ("mg" "Me Agenda"
         (
          (tags-todo "@me+TODO=\"NEXT\"|@tasks+TODO=\"NEXT\"")
          (tags-todo "@me+TODO=\"TODO\"|@tasks+TODO=\"TODO\"")
          )
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block

        )))


(setq org-agenda-files (quote ("~/.orgs/my.org")))
(setq org-mobile-directory "~/Owncloud/orgs")
;; (setq org-mobile-directory "~/.orgs/mob")
(setq org-directory "~/.orgs")
(setq org-mobile-inbox-for-pull "~/.orgs/mob.org")

;; Syntax Highlighting
;; http://praveen.kumar.in/2012/03/10/org-mode-latex-and-minted-syntax-highlighting/
(require 'org-latex)
(setq org-export-latex-listings 'minted)
(add-to-list 'org-export-latex-packages-alist '("" "minted"))

(provide 'init-org)
