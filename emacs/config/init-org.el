(require 'org)
(setq org-default-notes-file "~/.orgs/notes.org"
      org-log-done t)

(setq org-agenda-files (list "~/.orgs/my.org.gpg"))
;(setq org-agenda-files (quote ("~/.orgs/")))

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


; Custom agendas
(setq org-agenda-custom-commands
      (quote
       (("w" "At Work"
         (
          (tags-todo "@work+TODO=\"NEXT\"")
          (tags-todo "@work+TODO=\"TODO\"")
          )
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block

        ("u" "At Uni"
         (
          (tags-todo "@uni+TODO=\"NEXT\"")
          (tags-todo "@uni+TODO=\"TODO\"")
          )
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block
        )))


(provide 'init-org)
