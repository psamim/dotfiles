(require 'org)
(setq org-default-notes-file "~/.orgs/notes.org"
      org-log-done t)

;(setq org-agenda-files (list "~/.orgs/my.org.gpg"))

(require 'epa-file)
(epa-file-enable)
(setq epa-file-select-keys nil)

(setq org-M-RET-may-split-line nil)

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

;(require 'ob)
;(org-babel-do-load-languages
; 'org-babel-load-languages
; '((plantuml . t)))
;
;(setq org-plantuml-jar-path (expand-file-name "~/plantuml.jar"))


; Custom agendas and trees
(setq org-agenda-custom-commands
      (quote (
        ("u" "@uni NEXT" tags-tree "@uni+TODO=\"NEXT\"")
        ("1" "@uni TODO" tags-tree "@uni+TODO=\"TODO\"")
        ("w" "@work NEXT" tags-tree "@work+TODO=\"NEXT\"")
        ("2" "@work TODO" tags-tree "@work+TODO=\"TODO\"")
        ("m" "@me NEXT" tags-tree "@me+TODO=\"NEXT\"|@tasks+TODO=\"NEXT\"")
        ("3" "@me TODO" tags-tree "@me+TODO=\"TODO\"|@tasks+TODO=\"TODO\"")

       ("z" "Work Agenda"
         (
          (tags-todo "@work+TODO=\"NEXT\"")
          (tags-todo "@work+TODO=\"TODO\"")
          )
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block

        ("x" "Uni Agenda"
         (
          (tags-todo "@uni+TODO=\"NEXT\"")
          (tags-todo "@uni+TODO=\"TODO\"")
          )
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block

        ("c" "Me Agenda"
         (
          (tags-todo "@me+TODO=\"NEXT\"|@tasks+TODO=\"NEXT\"")
          (tags-todo "@me+TODO=\"TODO\"|@tasks+TODO=\"TODO\"")
          )
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block

        )))


(setq org-agenda-files (quote ("~/.orgs/my.org.gpg")))
(provide 'init-org)
