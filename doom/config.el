;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq helm-ag-show-status-function nil)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Samim Pezeshki"
      user-mail-address "psamim@gmail.com")

(setq doom-localleader-key ",")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka" :size 22)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile")
      doom-unicode-font (font-spec :family "Iosevka")
      ;; doom-big-font (font-spec :family "Fira Mono" :size 19)
      )

(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(custom-set-variables '(emojify-display-style 'unicode))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)


;; https://github.com/d12frosted/d12frosted.io/issues/15#issuecomment-908260553
(require 'cl-lib)
(defvar org-agenda--todo-keyword-regex
  (cl-reduce (lambda (cur acc)
               (concat acc "\\|" cur))
             (mapcar (lambda (entry) (concat "\\* " entry))
                     '("TODO" "PROJ" "NEXT")))
  "Regex which filters all TODO keywords")

(defun org-agenda--calculate-files-for-regex (regex)
  "Yields a fresh array with all files containing todos which match REGEX.

  Uses grep to discover all files containing anything stored in
  org-agenda--todo-keyword-regex."
  (let ((files
         (cl-remove-if #'file-directory-p
                       (split-string
                        (shell-command-to-string
                         (concat "grep --include=\"*.org\" --exclude-dir=\"archive\" -rl -e '" regex "' ~/Notes/roam"))
                        "\n"))))
    (cl-concatenate
     'list
     files
     '("~/Notes/calendar-inbox.org"
       "~/Notes/study.org"
       "~/Notes/events.org"
       "~/Notes/projects"))))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;

(setq mixed-pitch-fixed-pitch-faces
      (quote (line-number-current-line line-number font-lock-comment-face org-done org-todo org-todo-keyword-outd org-todo-keyword-kill org-todo-keyword-wait org-todo-keyword-done org-todo-keyword-habt org-todo-keyword-todo org-tag org-ref-cite-face org-property-value org-special-keyword org-date diff-added org-drawer diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-table org-verbatim)))

(defun add-property-with-date-captured ()
  "Add DATE_CAPTURED property to the current item."
  (interactive)
  (org-set-property "CREATED" (format-time-string "%F")))


;; Compute agenda files more frequently
(setq-hook! org-agenda-mode
  org-agenda-files
  (org-agenda--calculate-files-for-regex org-agenda--todo-keyword-regex))

(setq-hook! org-mode
  org-log-done t
  org-log-reschedule 'time
  org-image-actual-width nil
  org-clock-into-drawer t
  org-clock-persist t
  org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                ("STYLE_ALL" . "habit")))
  ;; org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar")
  ;; org-export-babel-evaluate nil
  org-confirm-babel-evaluate nil
  org-archive-location "~/Notes/archive/todo.org.gpg::datetree/"
  bidi-paragraph-direction t
  org-hide-emphasis-markers t
  org-fontify-done-headline t
  org-fontify-whole-heading-line t
  org-fontify-quote-and-verse-blocks t)

(customize-set-value
 'org-agenda-category-icon-alist
 `(
   ("work" "~/.dotfiles/icons/work.svg" nil nil :ascent center :mask heuristic)
   ("music" "~/.dotfiles/icons/music.svg" nil nil :ascent center :mask heuristic)
   ("chore" "~/.dotfiles/icons/chore.svg" nil nil :ascent center :mask heuristic)
   ("events" "~/.dotfiles/icons/events.svg" nil nil :ascent center :mask heuristic)
   ("inbox" "~/.dotfiles/icons/inbox.svg" nil nil :ascent center :mask heuristic)
   ("walk" "~/.dotfiles/icons/walk.svg" nil nil :ascent center :mask heuristic)
   ("solution" "~/.dotfiles/icons/solution.svg" nil nil :ascent center :mask heuristic)
   ("community" "~/.dotfiles/icons/community.svg" nil nil :ascent center :mask heuristic)
   ("idea" "~/.dotfiles/icons/idea.svg" nil nil :ascent center :mask heuristic)
   ("man" "~/.dotfiles/icons/man.svg" nil nil :ascent center :mask heuristic)
   ("scheduled" "~/.dotfiles/icons/scheduled.svg" nil nil :ascent center :mask heuristic)
   ("class" "~/.dotfiles/icons/class.svg" nil nil :ascent center :mask heuristic)
   ("plant" "~/.dotfiles/icons/plant.svg" nil nil :ascent center :mask heuristic)
   ("check" "~/.dotfiles/icons/check.svg" nil nil :ascent center :mask heuristic)
   ("search" "~/.dotfiles/icons/search.svg" nil nil :ascent center :mask heuristic)
   ("home" "~/.dotfiles/icons/home.svg" nil nil :ascent center :mask heuristic)
   ("book" "~/.dotfiles/icons/book.svg" nil nil :ascent center :mask heuristic)
   ("cook" "~/.dotfiles/icons/cook.svg" nil nil :ascent center :mask heuristic)
   ("buy" "~/.dotfiles/icons/buy.svg" nil nil :ascent center :mask heuristic)
   ("shower" "~/.dotfiles/icons/shower.svg" nil nil :ascent center :mask heuristic)
   ("archive" "~/.dotfiles/icons/archive.svg" nil nil :ascent center :mask heuristic)
   ))
(defun agenda-color-char ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "⚡" nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face '(:height 220 :foreground "gold2" :bold t))))

  ;; (save-excursion
  ;;   (goto-char (point-min))
  ;;   (while (re-search-forward org-agenda-hidden-separator nil t)
  ;;     (put-text-property (match-beginning 0) (- (match-end 0) 20)
  ;;                        'face '(:foreground "red"))))
  )


(defun my-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
                     This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date 1 nil))
         (day (cadr date))
         (persian (substring (calendar-persian-date-string date) 0 -6))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month 1))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (weekyear (cond ((and (= month 1) (>= iso-week 52))
                          (1- year))
                         ((and (= month 12) (<= iso-week 1))
                          (1+ year))
                         (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " W%02d" iso-week)
                       "")))
    (format " %-2s. %2d %s, %s"
            dayname day monthname persian)))


;; https://github.com/koddo/.emacs.d/blob/427aa248fcaf3c4f7cc827015b188e25161b1b5c/init-habits.el
;; (after! org-habit
;;   (defun org-habit-build-graph (habit starting current ending)
;;     "Build a graph for the given HABIT, from STARTING to ENDING.
;; CURRENT gives the current time between STARTING and ENDING, for
;; the purpose of drawing the graph.  It need not be the actual
;; current time."
;;     (let* ((all-done-dates (sort (org-habit-done-dates habit) #'<))
;; 	   (done-dates all-done-dates)
;; 	   (scheduled (org-habit-scheduled habit))
;; 	   (s-repeat (org-habit-scheduled-repeat habit))
;; 	   (start (time-to-days starting))
;; 	   (now (time-to-days current))
;; 	   (end (time-to-days ending))
;; 	   (graph (make-string (1+ (- end start)) org-habit-missed-glyph))
;; 	   (index 0)
;; 	   last-done-date)
;;       (while (and done-dates (< (car done-dates) start))
;;         (setq last-done-date (car done-dates)
;; 	      done-dates (cdr done-dates)))
;;       (while (< start end)
;;         (let* ((in-the-past-p (< start now))
;; 	       (todayp (= start now))
;; 	       (donep (and done-dates (= start (car done-dates))))
;; 	       (faces
;; 	        (if (and in-the-past-p
;; 		         (not last-done-date)
;; 		         (not (< scheduled now)))
;; 		    (if (and all-done-dates (= (car all-done-dates) start))
;; 		        ;; This is the very first done of this habit.
;; 		        '(org-habit-ready-face . org-habit-ready-future-face)
;; 		      '(org-habit-clear-face . org-habit-clear-future-face))
;; 		  (org-habit-get-faces
;; 		   habit start
;; 		   (and in-the-past-p
;; 		        last-done-date
;; 		        ;; Compute scheduled time for habit at the time
;; 		        ;; START was current.
;; 		        (let ((type (org-habit-repeat-type habit)))
;; 			  (cond
;; 			   ;; At the last done date, use current
;; 			   ;; scheduling in all cases.
;; 			   ((null done-dates) scheduled)
;; 			   ((equal type ".+") (+ last-done-date s-repeat))
;; 			   ((equal type "+")
;; 			    ;; Since LAST-DONE-DATE, each done mark
;; 			    ;; shifted scheduled date by S-REPEAT.
;; 			    (- scheduled (* (length done-dates) s-repeat)))
;; 			   (t
;; 			    ;; Compute the scheduled time after the
;; 			    ;; first repeat.  This is the closest time
;; 			    ;; past FIRST-DONE which can reach SCHEDULED
;; 			    ;; by a number of S-REPEAT hops.
;; 			    ;;
;; 			    ;; Then, play TODO state change history from
;; 			    ;; the beginning in order to find current
;; 			    ;; scheduled time.
;; 			    (let* ((first-done (car all-done-dates))
;; 				   (s (let ((shift (mod (- scheduled first-done)
;; 						        s-repeat)))
;; 				        (+ (if (= shift 0) s-repeat shift)
;; 					   first-done))))
;; 			      (if (= first-done last-done-date) s
;; 			        (catch :exit
;; 				  (dolist (done (cdr all-done-dates) s)
;; 				    ;; Each repeat shifts S by any
;; 				    ;; number of S-REPEAT hops it takes
;; 				    ;; to get past DONE, with a minimum
;; 				    ;; of one hop.
;; 				    (cl-incf s (* (1+ (/ (max (- done s) 0)
;; 						         s-repeat))
;; 						  s-repeat))
;; 				    (when (= done last-done-date)
;; 				      (throw :exit s))))))))))
;; 		   donep)))
;; 	       markedp face)
;; 	  (cond
;; 	   (donep
;; 	    (aset graph index org-habit-completed-glyph)
;; 	    (setq markedp t)
;; 	    (while (and done-dates (= start (car done-dates)))
;; 	      (setq last-done-date (car done-dates))
;; 	      (setq done-dates (cdr done-dates))))
;; 	   (todayp
;; 	    (aset graph index org-habit-today-glyph)))
;; 	  (setq face (if (or in-the-past-p todayp)
;; 		         (car faces)
;; 		       (cdr faces)))
;; 	  (when (and in-the-past-p
;; 		     (not (eq face 'org-habit-overdue-face))
;; 		     (not markedp))
;; 	    (setq face (cdr faces)))
;; 	  (put-text-property index (1+ index) 'face face graph)
;; 	  (put-text-property index (1+ index)
;; 			     'help-echo
;; 			     (concat (format-time-string
;; 				      (org-time-stamp-format)
;; 				      (time-add starting (days-to-time (- start (time-to-days starting)))))
;; 				     (if donep " DONE" ""))
;; 			     graph))
;;         (setq start (1+ start)
;; 	      index (1+ index)))
;;       graph))
;;   )

;; (after! org
;;   (add-to-list 'org-modules 'org-habit))

;; (defun org-agenda-highlight-todo (x)
;;   (let ((org-done-keywords org-done-keywords-for-agenda)
;; 	(case-fold-search nil)
;; 	re)
;;     (if (eq x 'line)
;; 	(save-excursion
;; 	  (beginning-of-line 1)
;; 	  (setq re (org-get-at-bol 'org-todo-regexp))
;; 	  (goto-char (or (text-property-any (point-at-bol) (point-at-eol) 'org-heading t) (point)))
;; 	  (when (looking-at (concat "[ \t]*\\.*\\(" re "\\) +"))
;; 	    (add-text-properties (match-beginning 0) (match-end 1)
;; 				 (list 'face (org-get-todo-face 1)))
;; 	    (let ((s (buffer-substring (match-beginning 1) (match-end 1))))
;; 	      (delete-region (match-beginning 1) (1- (match-end 0)))
;; 	      (goto-char (match-beginning 1))
;;                    (unless (string= org-agenda-todo-keyword-format "")
;; 	      (insert (format org-agenda-todo-keyword-format s)))
;;               )))
;;       (let ((pl (text-property-any 0 (length x) 'org-heading t x)))
;; 	(setq re (get-text-property 0 'org-todo-regexp x))
;; 	(when (and re
;; 		   ;; Test `pl' because if there's no heading content,
;; 		   ;; there's no point matching to highlight.  Note
;; 		   ;; that if we didn't test `pl' first, and there
;; 		   ;; happened to be no keyword from `org-todo-regexp'
;; 		   ;; on this heading line, then the `equal' comparison
;; 		   ;; afterwards would spuriously succeed in the case
;; 		   ;; where `pl' is nil -- causing an args-out-of-range
;; 		   ;; error when we try to add text properties to text
;; 		   ;; that isn't there.
;; 		   pl
;; 		   (equal (string-match (concat "\\(\\.*\\)" re "\\( +\\)")
;; 					x pl)
;; 			  pl))
;; 	  (add-text-properties
;; 	   (or (match-end 1) (match-end 0)) (match-end 0)
;; 	   (list 'face (org-get-todo-face (match-string 2 x)))
;; 	   x)
;; 	  (when (match-end 1)
;; 	    (setq x
;; 		  (concat
;; 		   (substring x 0 (match-end 1))
;;                    (unless (string= org-agenda-todo-keyword-format "")
;; 		     (format org-agenda-todo-keyword-format
;; 			     (match-string 2 x)
;;                    ;; Remove `display' property as the icon could leak
;; 		   ;; on the white space.
;; 		   (org-add-props " " (org-plist-delete (text-properties-at 0 x)
;;                                                         'display)))
;;                              )
;;                    (substring x (match-end 3)))))))
;;       x)))


(defun psamim-journal-prefix (time)
  (let*
      (
       (decodedTime (decode-time time))
       (now (list (nth 4 decodedTime) (nth 3 decodedTime) (nth 5 decodedTime))))
    (concat
     ;; (format-time-string "%B %e, %Y" time) "\n"
     ;; (format-time-string "%A" time)
     "# " (calendar-persian-date-string now) "\n"
     "# " (calendar-bahai-date-string now) "\n\n"
     "#+CATEGORY: check"
     )
    ))

(defun psamim-insert-persian-time ()
  (interactive)
  (shell-command-to-string "~/.bin/persian-date")
  )

(setq org-journal-enable-agenda-integration t)
(customize-set-variable 'org-journal-file-type 'yearly)
(setq!
 org-journal-dir "~/Notes/journal/yearly"
 org-journal-start-on-weekday 6 ; Saturday
 ;; org-journal-enable-cache t
 org-journal-encrypt-journal t
 org-journal-file-header 'psamim-journal-prefix)

;; (set-email-account! "psamim@gmail.com"
;;   '((mu4e-sent-folder       . "/[Gmail].Sent Mail")
;;     (mu4e-drafts-folder     . "/[Gmail].Drafts")
;;     (mu4e-trash-folder      . "/[Gmail].Bin")
;;     (mu4e-refile-folder     . "/[Gmail].All Mail")
;;     (smtpmail-smtp-user     . "psamim@gmail.com")
;;     (smtpmail-default-smtp-server . "smtp.gmail.com")
;;     (smtpmail-smtp-server . "smtp.gmail.com")
;;     (smtpmail-smtp-service . 587)
;;     (mu4e-compose-signature . "---\nSamim Pezeshki"))
;;   t)


(add-hook! '(org-clock-out-hook org-clock-in-hook) #'org-save-all-org-buffers)
(advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)


(defun export-clock ()
  (interactive)
  (org-clock-csv-to-file "~/clock.csv" "~/Notes/clock.org"))

(defun my-org-agenda ()
  (interactive)
  (if (eq system-type 'darwin)  ;; Work laptop
      (org-agenda nil "w")
    (org-agenda nil "a")))

(defun my-org-index ()
  (interactive)
  (find-file "~/Notes/roam/20210625224818-index.org"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; (defun set-farsi-font ()
;;   (interactive)
;;   (progn
;;     (set-fontset-font
;;      "fontset-default"
;;      'arabic
;;      ;; (cons (decode-char 'ucs #x0600) (decode-char 'ucs #x06ff)) ; arabic
;;      ;; "Vazir Code-13")
;;      ;; "Tanha-16"))
;;      ;; "Mikhak-15"
;;       )

(+bidi-global-mode 1)
(setq +bidi-want-smart-fontify nil)
(setq +bidi-arabic-font (font-spec :family "Mikhak"))

;; (after! org-mode
;;   (set-company-backend! 'company-dabbrev)
;;   )
;;
;; (after! company
;;   (setq company-idle-delay 0.3
;;         company-minimum-prefix-length 1
;;   company-dabbrev-code-everywhere t
;;   company-dabbrev-code-other-buffers 'all))

(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

(add-hook 'text-mode-hook
          (lambda ()
            ;; (set-farsi-font)
            (setq olivetti-body-width 0.91)
            (olivetti-mode)
            (setq header-line-format " ")
            (mixed-pitch-mode 1)))

;; Transparency
(set-frame-parameter nil 'alpha-background 94)
(add-to-list 'default-frame-alist '(alpha-background . 94))
(set-frame-parameter (selected-frame) 'alpha '(94 94))

(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; macos
(if (eq system-type 'darwin)
    (progn
      (setq-hook! org-mode
        org-archive-location "~/Notes/archive/todo.org::datetree/")
      (add-to-list 'default-frame-alist '(undecorated-round . t))
      (setq ns-use-proxy-icon nil)
      (setq frame-title-format nil))

  (progn (add-to-list 'default-frame-alist '(undecorated . t))))

(defun my-org-mode-autosave-settings ()
  (add-hook 'auto-save-hook 'org-save-all-org-buffers nil nil))

(add-hook 'org-agenda-finalize-hook #'set-window-clean)
(add-hook! 'org-mode-hook #'my-org-mode-autosave-settings)

(add-hook 'org-mode-local-vars-hook #'(lambda () (eldoc-mode -1)))

;; (add-hook! 'mu4e-view-mode-hook
;;            #'olivetti-mode)

;; (use-package! org-pretty-table
;;   :commands (org-pretty-table-mode global-org-pretty-table-mode))

(defun set-window-clean ()
  (setq line-spacing 2)
  (agenda-color-char)
  (+bidi-mode -1)
  (setq mode-line-format nil)
  ;; (mixed-pitch-mode 1)
  ;; (set-frame-parameter nil 'font "Iosevka-18")
  (setq header-line-format " ")
  (set-face-attribute 'header-line nil :background "#00000000")
  (set-window-margins (frame-selected-window) 4))


;; (global-activity-watch-mode)

(defun string-to-int (s) (string-to-number s))

;; (add-hook
;;  'org-agenda-mode-hook
;;  (lambda ()
;;    (defun org-agenda-get-progress ()
;;      (interactive)
;;      (let
;;          ;; ((effort (and (not (string= txt "")) (get-text-property 1 'effort txt))))
;;          (
;;           (effort (cdar (org-entry-properties nil "EFFORT")))
;;           (clocksum
;;            (org-duration-from-minutes
;;             (org-clock-sum-current-item (car (org-clock-special-range 'thisweek nil nil 0))))))
;;        (if effort
;;            (concat "[" clocksum  "/" effort "] ")
;;          (concat "[" clocksum "] "))))))

;; (use-package! mu4e
;;   :config
;;   (setq mu4e-use-fancy-chars nil
;;         mu4e-update-interval 300
;;         mu4e-headers-draft-mark '("D" . "")
;;         mu4e-headers-flagged-mark '("F" . "")
;;         mu4e-headers-new-mark '("N" . "✱")
;;         mu4e-headers-passed-mark '("P" . "❯")
;;         mu4e-headers-replied-mark '("R" . "❮")
;;         mu4e-headers-seen-mark '("S" . "✔")
;;         mu4e-headers-trashed-mark '("T" . "")
;;         mu4e-headers-attach-mark '("a" . "")
;;         mu4e-headers-encrypted-mark '("x" . "")
;;         mu4e-headers-signed-mark '("s" . "☡")
;;         mu4e-headers-unread-mark '("u" . "⎕")))

(use-package! doom-modeline
  :init
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode org-mode org-journal-mode)))

;; Define your custom doom-modeline
(doom-modeline-def-modeline 'my-simple-line
  '(buffer-info selection-info)
  '(misc-info input-method process))

;; Add to `doom-modeline-mode-hook` or other hooks
(defun setup-custom-doom-modeline ()
  (doom-modeline-set-modeline 'my-simple-line 'default))
(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)


;; (mu4e-alert-set-default-style 'libnotify)
;; (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

(use-package unidecode
  :defer t
  :commands unidecode-region unidecode-sanitize-region
  :config
  (evil-define-operator evil-unidecode (beg end type register)
    "Applies unidecode to text as an Evil operator"
    :move-point nil
    :keep-visual nil
    (interactive "<R><x>")
    (when (evil-visual-state-p)
      (let ((range (evil-expand beg end
                                (if (and evil-respect-visual-line-mode
                                         visual-line-mode)
                                    'screen-line
                                  'line))))
        (setq beg (evil-range-beginning range)
              end (evil-range-end range)
              type (evil-type range))))
    (unidecode-region beg end))
  :general
  (general-define-key
   :states '(normal visual)
   "gt" 'evil-unidecode))

;; (require 'notmuch)
;; (add-to-list 'auto-mode-alist '("psamim@gmail.com" . notmuch-message-mode))

(setq sendmail-program "gmi")
(setq message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/.mail/account.gmail"))

;; (require 'org-msg)
;; (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
;;       org-msg-startup "hidestars indent inlineimages"
;;       ;; org-msg-greeting-fmt "<div dir='rtl'>\n\n</div>"
;;       org-msg-greeting-name-limit 3
;;       org-msg-text-plain-alternative t
;;       org-msg-signature "
;;
;;  Regards,
;;
;;  #+begin_signature
;;  -- *Samim Pezeshki* \\\\
;;  #+end_signature")
;; (org-msg-mode)

;; (after! evil-org (progn
;;                    (map! :map evil-org-mode-map :n "M-l" #'centaur-tabs-forward)
;;                    (map! :map evil-org-mode-map :n "M-h" #'centaur-tabs-backward)))
;; (map! "M-l" #'centaur-tabs-forward)
;; (map! "M-h" #'centaur-tabs-backward)
(map! "M-q" #'kill-current-buffer)
(map! "M-n" #'vterm)

;; (add-hook 'org-agenda-mode-hook 'centaur-tabs-local-mode)

(setq evil-normal-state-cursor '(box "light blue")
      evil-insert-state-cursor '(bar "medium sea green")
      evil-visual-state-cursor '(hollow "orange"))

;; (defun org-toggle-tag-visibility (state)
;;   "Run in `org-cycle-hook'."
;;   (message "%s" state)
;;   (cond
;;    ;; global cycling
;;    ((memq state '(overview contents showall))
;;     (org-map-entries
;;      (lambda ()
;;        (let ((tagstring (nth 5 (org-heading-components)))
;;          start end)
;;      (when tagstring
;;        (save-excursion
;;          (beginning-of-line)
;;          (re-search-forward tagstring)
;;          (setq start (match-beginning 0)
;;            end (match-end 0)))
;;        (cond
;;         ((memq state '(overview contents))
;;          (outline-flag-region start end t))
;;         (t
;;          (outline-flag-region start end nil))))))))
;;    ;; local cycling
;;    ((memq state '(folded children subtree))
;;     (save-restriction
;;       (org-narrow-to-subtree)
;;       (org-map-entries
;;        (lambda ()
;;      (let ((tagstring (nth 5 (org-heading-components)))
;;            start end)
;;        (when tagstring
;;          (save-excursion
;;            (beginning-of-line)
;;            (re-search-forward tagstring)
;;            (setq start (match-beginning 0)
;;              end (match-end 0)))
;;          (cond
;;           ((memq state '(folded children))
;;            (outline-flag-region start end t))
;;           (t
;;            (outline-flag-region start end nil)))))))))))

;; (add-hook 'org-cycle-hook 'org-toggle-tag-visibility)

(doom-modeline-mode 0)

(add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)

(after! org
  (custom-set-variables
   '(org-capture-templates
     (quote (
             ("t" "todo" entry
              (file "~/Notes/projects/misc.org") "* TODO %?\n%a\n" :clock-keep t)
             ("e" "event" entry
              (file+headline "~/Notes/events.org" "Inbox") "* %?\n" :clock-keep t)
             ("s" "schedule" entry
              (file+headline "~/Notes/events.org" "Inbox") "* %?\nSCHEDULED: %t" :clock-keep t)
             ))))


  (setq
   org-agenda-block-separator nil
   org-habit-today-glyph ?◌
   org-habit-graph-column 40
   org-habit-following-days 1
   org-habit-show-habits t
   org-habit-completed-glyph ?●
   org-habit-preceding-days 10
   org-habit-show-habits-only-for-today t
   org-habit-missed-glyph ?○
   org-agenda-block-separator (string-to-char " ")
   org-agenda-format-date 'my-org-agenda-format-date-aligned
   org-agenda-hidden-separator "‌‌ "
   org-columns-default-format "%ITEM(Task) %Effort(Effort){:} %CLOCKSUM(Clock Sum){:}"
   org-agenda-clock-report-header "Report\n"
   org-superstar-headline-bullets-list '("◯" "∙" "∘" "∘" "∘" "∘" "∘" "∘")
   org-startup-with-inline-images t
   org-agenda-clockreport-parameter-plist
   '(:stepskip0 t :link t :maxlevel 2 :fileskip0 t :hidefiles t :properties ("EFFORT"))
   org-crypt-key "psamim@gmail.com"
   org-clock-persist t
   org-tags-exclude-from-inheritance '("project" "crypt")
   ;; org-duration-format 'h:mm
   org-tag-alist '(("crypt" . ?c) ("project" . ?p))
   org-duration-format '((special . h:mm))
   org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"
   org-export-with-section-numbers nil
   org-export-with-broken-links 't
   org-agenda-diary-file "~/Notes/diary.org"
   plstore-cache-passphrase-for-symmetric-encryption t
   password-cache-expiry nil
   org-ellipsis "…"
   ;; ➡, ⚡, ▼, ↴, , ∞, ⬎, ⤷, ⤵
   ;; org-agenda-files (quote ("~/Notes/projects"
   ;;                          "~/Notes/roam"
   ;;                          "~/Notes/calendar-inbox.org"
   ;;                          "~/Notes/roam/20210625224916-areas.org"
   ;;                          "~/Notes/roam/20210507181408-people.gpg.org"
   ;;                          "~/Notes/study.org"
   ;;                          "~/Notes/events.org"))
   org-agenda-files (org-agenda--calculate-files-for-regex org-agenda--todo-keyword-regex)
   org-file-apps
   '((remote . emacs)
     (auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "xournalpp %s"))
   org-deadline-warning-days 7
   org-agenda-breadcrumbs-separator " ❱ "
   org-export-in-background nil
   org-fold-catch-invisible-edits 'smart
   org-directory "~/Notes")

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "NEXT(n)"  ; This task going to be done thiw iteration (week)
           "NOTE(o)"  ; This task going to be done thiw iteration (week)
           "IDEA(i)"  ; This task going to be done thiw iteration (week)
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("IDEA" . +org-todo-project)
          ("NOTE" . +org-todo-project)
          ("PROJ" . +org-todo-project)))

  (setq org-agenda-custom-commands
        '(
          ("a" "My Agenda"
           (
            (agenda "" (
                        (org-agenda-skip-scheduled-if-done nil)
                        (org-agenda-time-leading-zero t)
                        (org-agenda-timegrid-use-ampm nil)
                        (org-agenda-skip-timestamp-if-done t)
                        (org-agenda-skip-deadline-if-done t)
                        (org-agenda-start-day "+0d")
                        (org-agenda-span 2)
                        (org-agenda-overriding-header "⚡ Calendar")
                        (org-agenda-repeating-timestamp-show-all nil)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "   %i %?-2 t%s")
                        ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                        ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                        ;; (org-agenda-todo-keyword-format " ☐ ")
                        (org-agenda-todo-keyword-format "")
                        (org-agenda-time)
                        (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                        (org-agenda-scheduled-leaders '("" ""))
                        (org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
                        (org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))

            (tags "+TODO=\"TODO\"" (
                                    (org-agenda-overriding-header "\n⚡ Today")
                                    (org-agenda-sorting-strategy '(priority-down))
                                    (org-agenda-remove-tags t)
                                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'scheduled))
                                    ;; (org-agenda-todo-ignore-scheduled 'all)
                                    (org-agenda-prefix-format "   %-2i ")
                                    ;; (org-agenda-todo-keyword-format "")
                                    ))

            (tags "-CATEGORY=\"work\"+TODO=\"NEXT\"" (
                                                      (org-agenda-overriding-header "\n⚡ Next")
                                                      (org-agenda-sorting-strategy '(priority-down))
                                                      (org-agenda-remove-tags t)
                                                      ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                                                      (org-agenda-todo-ignore-scheduled 'all)
                                                      (org-agenda-prefix-format "   %-2i %?b")
                                                      (org-agenda-todo-keyword-format "")))


            (tags "-CATEGORY=\"work\"+TODO=\"PROJ\"" (
                                                      (org-agenda-overriding-header "\n⚡ Projects")
                                                      (org-agenda-remove-tags t)
                                                      (org-tags-match-list-sublevels nil)
                                                      (org-agenda-show-inherited-tags nil)
                                                      (org-agenda-prefix-format "   %-2i %?b")
                                                      (org-agenda-todo-keyword-format "")))
            ))

          ("w" "Work Agenda"
           (
            ;; (agenda "" (
            ;;             ;; https://emacs.stackexchange.com/questions/38742/implement-scheduling-as-suggested-in-deep-work-using-emacs-org-mode
            ;;             (org-agenda-sorting-strategy '((agenda habit-down time-up ts-up
            ;;                                             priority-down category-keep)
            ;;                                            (todo priority-down category-keep)
            ;;                                            (tags priority-down category-keep)
            ;;                                            (search category-keep)))

            ;;             (org-agenda-skip-scheduled-if-done nil)
            ;;             (org-agenda-time-leading-zero t)
            ;;             (org-agenda-timegrid-use-ampm nil)
            ;;             (org-agenda-skip-timestamp-if-done t)
            ;;             (org-agenda-skip-deadline-if-done t)
            ;;             (org-agenda-start-day "+0d")
            ;;             (org-agenda-span 2)
            ;;             (org-agenda-overriding-header "⚡ Calendar")
            ;;             (org-agenda-repeating-timestamp-show-all nil)
            ;;             (org-agenda-remove-tags t)
            ;;             (org-agenda-prefix-format "   %i %?-2 t%s")
            ;;             ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
            ;;             ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
            ;;             ;; (org-agenda-todo-keyword-format " ☐ ")
            ;;             (org-agenda-todo-keyword-format "")
            ;;             (org-agenda-time)
            ;;             (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
            ;;             (org-agenda-scheduled-leaders '("" ""))
            ;;             (org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
            ;;             (org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))

            (tags "+TODO=\"TODO\"" (
                                    (org-agenda-overriding-header "\n⚡ To Do")
                                    (org-agenda-sorting-strategy '(priority-down))
                                    (org-agenda-remove-tags t)
                                    ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                                    (org-agenda-todo-ignore-scheduled 'all)
                                    (org-agenda-prefix-format "   %-2i %?b")
                                    (org-agenda-todo-keyword-format "")))

            (tags "+TODO=\"NEXT\"" (
                                    (org-agenda-overriding-header "\n⚡ Next")
                                    (org-agenda-sorting-strategy '(priority-down))
                                    (org-agenda-remove-tags t)
                                    (org-agenda-todo-ignore-scheduled 'all)
                                    (org-agenda-prefix-format "   %-2i %?b")
                                    (org-agenda-todo-keyword-format "")))

            (tags "+TODO=\"PROJ\"" (
                                    (org-agenda-overriding-header "\n⚡ Projects")
                                    (org-agenda-remove-tags t)
                                    (org-tags-match-list-sublevels nil)
                                    (org-agenda-show-inherited-tags nil)
                                    (org-agenda-prefix-format "   %-2i %?b")
                                    (org-agenda-todo-keyword-format "")))
            ))


          ("mo" "My Agenda"
           (
            (agenda "" (
                        (org-agenda-skip-scheduled-if-done nil)
                        (org-agenda-time-leading-zero nil)
                        (org-agenda-timegrid-use-ampm nil)
                        (org-agenda-skip-timestamp-if-done t)
                        (org-agenda-skip-deadline-if-done t)
                        (org-agenda-start-day "+0d")
                        (org-agenda-span 3)
                        (org-agenda-overriding-header "⚡ Calendar")
                        (org-agenda-repeating-timestamp-show-all nil)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "   %i %?-2 t%s")
                        ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                        ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                        ;; (org-agenda-todo-keyword-format " ☐ ")
                        (org-agenda-todo-keyword-format "")
                        (org-agenda-time)
                        (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                        (org-agenda-scheduled-leaders '("" ""))
                        (org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
                        (org-agenda-time-grid nil)))

            (todo "TODO" (
                          (org-agenda-overriding-header "\n⚡ To Do")
                          (org-agenda-sorting-strategy '(priority-down))
                          (org-agenda-remove-tags t)
                          ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                          (org-agenda-todo-ignore-scheduled 'all)
                          (org-agenda-prefix-format "   %-2i %?b")
                          (org-agenda-todo-keyword-format "")))

            ))))

  ;; https://emacs.stackexchange.com/questions/38742/implement-scheduling-as-suggested-in-deep-work-using-emacs-org-mode
  (setq org-agenda-sorting-strategy '((agenda habit-down time-up ts-up
                                       priority-down category-keep)
                                      (todo priority-down category-keep)
                                      (tags priority-down category-keep)
                                      (search category-keep)))
  ;; ⧗             ―               ﮸          λ ◁ ▷ ✧ ✦
  (appendq! +ligatures-extra-symbols
            `(:clock      "⧗ "
              :circle "⚫"
              :shogi "⛊"
              :white_shogi "☖"
              :black_shogi "☗"
              :two_lines "⚏"
              :tags "    ‌"
              :empty ""
              ))
  (set-ligatures! 'org-mode
    :merge t
    :clock ":LOGBOOK:"
    :quote         "#+begin_quote"
    :name "#+CAPTION:"
    :quote_end     "#+end_quote"
    :src_block         "#+begin_src"
    :src_block         "#+BEGIN_SRC"
    :src_block         "#+BEGIN:"
    :src_block_end     "#+end_src"
    :src_block_end     "#+END_SRC"
    :src_block_end     ":END:"
    :src_block_end     "#+END"
    :two_lines   ":PROPERTIES:"
    :black_shogi   "#+CATEGORY:"
    :black_shogi   "#+category:"
    ;; :two_lines   "#+startup:"
    ;; :two_lines   "#+STARTUP:"
    :empty "#+title:"
    :empty "#+TITLE:"
    :shogi "#+NAME:"
    :shogi "#+name:"
    ;; :tags "keywords:"
    :black_shogi "#+roam_tags:"
    )
  )

(use-package! org-fancy-priorities ; priority icons
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚑" "⬆" "⬇")))


(custom-theme-set-faces!
  'doom-solarized-light
  ;; '((org-agenda-date org-agenda-date-weekend) :foreground "#586e75"
  ;;   :weight semibold :slant normal
  ;;   :box (:line-width 7 :color "#fffbea" :style nil))
  ;; '((org-agenda-date-today) :foreground "#073642" :weight semibold :slant normal
  ;;   :box (:line-width 7 :color "#fffbea" :style nil)
  ;;   )
  ;; '((org-agenda-calendar-event)  :weight light)

  ;; '((org-agenda-done)
  ;;   :foreground "#91a6ad"
  ;;   :weight light
  ;;   :strike-through "dark gray")

  ;; '((org-scheduled org-scheduled-today)  :foreground "#556b72" :weight light)
  '((org-agenda-structure) :family "pacifico" :height 220
    :box (:line-width 2 :color "#FDF6E3" :style nil))
  ;; '((org-ellipsis) :height 1.0)
  '((org-level-1) :foreground "#bf360c" :weight normal :height 1.3 :inherit outline-1)
  '((org-level-2) :weight normal :foreground "#211221" :inherit outline-2)
  '((org-level-3) :weight normal :inherit outline-3 :foreground "#424242")
  '((org-level-4) :weight normal :inherit outline-4 :foreground "#616161")
  '((org-level-5) :weight normal :inherit outline-5 :foreground "#616161")
  '((org-level-6) :weight normal :inherit outline-6 :foreground "#616161")
  '((org-link) :weight normal :inherit link)
  '((org-document-title) :height 1.6)
  ;; '(org-tag :foreground "#fbf5e3")
  ;; '((org-drawer org-meta-line org-headline-done) :foreground "dark gray")
  ;; '((org-block-begin-line org-block-end-line) :foreground "dark gray" :background "#f7edd0" :extend t)
  ;; '((org-table) :background "#f7edd0")
  )

(map! :map magit-status-mode-map :n "<tab>" 'magit-section-toggle)

(setq ispell-dictionary "en_US")

;; (use-package! nroam
;;   :after org-roam
;;   :config
;;   (add-hook 'org-mode-hook #'nroam-setup-maybe))

(setq +org-roam-open-buffer-on-find-file nil)

;; (use-package! org-mind-map
;;   :init
;;   (require 'ox-org)
;;   :ensure t
;;   ;; Uncomment the below if 'ensure-system-packages` is installed
;;   ;;:ensure-system-package (gvgen . graphviz)
;;   :config
;;   (setq org-mind-map-engine "dot")       ; Default. Directed Graph
;;   ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
;;   ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
;;   ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
;;   ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
;;   ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
;;   ;; (setq org-mind-map-engine "circo")  ; Circular Layout
;;   )

(defun load-secrets () (interactive) (load "~/.doom.d/secrets.el.gpg"))

(defun psamim-sync-calendars ()
  (interactive)
  (progn
    (load-secrets)
    (org-gcal-sync-tokens-clear)
    (org-gcal-fetch)))

;; https://orgmode.org/manual/Filtering_002flimiting-agenda-items.html
(defun my-auto-exclude-fn (tag)
  (when (member tag '("work" "global"))
    (concat "-" tag)))

(setq org-agenda-auto-exclude-function #'my-auto-exclude-fn)

(defun do-not-display-work ()
  (interactive)
  (org-agenda-filter-apply '("-work") 'category))

(defun only-display-work ()
  (interactive)
  (org-agenda-filter-apply '("+work") 'category))

(defun save-screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
  Saves to a chosen file and puts the filename in the kill ring."
  (interactive)
  (let* ((file-name (concat
                     (make-temp-name "Emacs-") ".svg"))
         (path "~/Notes/html/agenda/")
         (full-file-name (concat path file-name))
         (data (x-export-frames nil 'svg))
         (index-file-template "~/.dotfiles/doom/org-agenda.html.template")
         (index-file (concat path "index.html")))
    (dolist
        (var (directory-files path t "Emacs.*svg"))
      (delete-file var))
    (with-temp-file full-file-name
      (insert data))
    (with-temp-file index-file
      (progn
        (insert-file-contents index-file-template)
        (goto-char (point-min))
        (while (search-forward "{{ FILENAME }}" nil t)
          (replace-match file-name t))))
    (message (concat "Saved screenshot to " file-name))))

(defun psamim-sync-agenda-svg ()
  "Save a screenshot of the current frame as an SVG image.
  Saves to a chosen file and puts the filename in the kill ring."
  (interactive)
  (progn
    (org-agenda nil "mo")
    (hl-line-mode -1)
    (org-agenda-redo-all)
    (goto-char 100000)
    (setq cursor-type nil)
    (save-screenshot-svg)
    (setq cursor-type 'box)))

(defun psamim-sync ()
  (interactive)
  (progn
    ;; (load-theme 'doom-one-light)
    ;; (global-hl-line-mode -1)
    (run-at-time
     "5 sec"
     nil
     '(lambda () (progn
                   ;; (psamim-sync-agenda-svg)
                   ;; (psamim-sync-calendars)
                   ;; (psamim-org-ical-export)
                   (org-caldav-sync)
                   (kill-emacs))))))

(map! :localleader
      (:map org-mode-map
            "c e" #'export-clock))

(map! :localleader
      (:map ledger-mode-map
            "c" #'ledger-mode-clean-buffer))

(map! :localleader (:map org-agenda-mode-map "f p" #'do-not-display-work))
(map! :localleader (:map org-agenda-mode-map "o" #'org-agenda-set-property))
(map! :localleader (:map org-agenda-mode-map "c" #'org-agenda-columns))
(map! :leader :desc "Org clock context" :nvg "n c" #'counsel-org-clock-context)
(map! :leader :desc "Dired" :nvg "d" #'ranger)
(map! :leader :desc "my-org-agenda" :nvg "na" 'my-org-agenda)
(map! :leader :desc "my-org-index" :nvg "ni" 'my-org-index)
(map! :leader :desc "sync-calendar" :nvg "rc" 'psamim-sync-calendars)
(map! :leader :desc "sync-agenda-svg" :nvg "ra" 'psamim-sync-agenda-svg)
(map! :leader :desc "open-org-journal" :nvg "njo" 'org-journal-open-current-journal-file)
(map! :localleader (:map org-mode-map :desc "roam-refile-to-today" :nvg "rt" 'psamim/org-roam-refile-to-today))

(custom-theme-set-faces!
  'doom-one-light
  '((org-agenda-date org-agenda-date-weekend)
    ;; :foreground "#586e75"
    :weight semibold
    :slant normal
    :box (:line-width 7 :color "#fafafa" :style nil))
  '((header-line-highlight highlight) :background "#fafafa")
  '((org-habit-ready-face
     org-habit-clear-future-face
     org-habit-overdue-face
     org-habit-alert-future-face
     org-habit-overdue-future-face
     org-habit-alert-face) :color "#fafafa" )
  '((org-habit-ready-face) :foreground "#18ac4d")
  '((org-habit-overdue-future-face) :color "#ffaa7f")
  '((org-habit-alert-future-face) :foreground "#ffaa7f")
  '((org-habit-overdue-face org-habit-overdue-future-face) :foreground "#ff007f")
  '((org-agenda-date-today)
    ;; :foreground "#073642"
    :weight semibold
    :slant normal
    :box (:line-width 7 :color "#fafafa" :style nil))
  '((org-agenda-calendar-event
     org-scheduled
     org-scheduled-today
     org-agenda-calendar-sexp
     org-scheduled-previously)  :weight light)
  '((org-agenda-done) :foreground "#91a6ad" :weight light :strike-through "dark gray")
  '((org-agenda-structure) :family "pacifico" :height 220
    :box (:line-width 2 :color "#fafafa" :style nil))
  '((org-document-title) :family "pacifico")
  '((org-ellipsis) :height 1.0)
  '((org-level-1) :weight normal :height 1.1 :inherit outline-1)
  '((org-level-2) :weight normal :foreground "#211221" :inherit outline-2)
  '((org-level-3) :weight normal :inherit outline-3 :foreground "#424242")
  '((org-level-4) :weight normal :inherit outline-4 :foreground "#616161")
  '((org-level-5) :weight normal :inherit outline-5 :foreground "#616161")
  '((org-level-6) :weight normal :inherit outline-6 :foreground "#616161")
  '((org-link) :weight normal :inherit link)
  '((org-document-title) :height 1.6)
  '(org-tag :foreground "#fafafa")
  '((org-drawer org-meta-line org-headline-done) :foreground "dark gray")
  '((org-block-begin-line org-block-end-line) :foreground "dark gray" :background "#f7edd0" :extend t)
  ;; '((org-table) :background "#f7edd0")
  )

;; (use-package! org-roam-server
;;   :ensure t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8181
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  :hook (org-roam . org-roam-ui-mode)
  :config
  )

(defun org--create-inline-image (file width)
  "Create image located at FILE, or return nil.
WIDTH is the width of the image.  The image may not be created
according to the value of `org-display-remote-inline-images'."
  (let* ((remote? (file-remote-p file))
	 (file-or-data
	  (pcase org-display-remote-inline-images
	    ((guard (not remote?)) file)
	    (`download (with-temp-buffer
			 (set-buffer-multibyte nil)
			 (insert-file-contents-literally file)
			 (buffer-string)))
	    (`cache (let ((revert-without-query '(".")))
		      (with-current-buffer (find-file-noselect file)
			(buffer-string))))
	    (`skip nil)
	    (other
	     (message "Invalid value of `org-display-remote-inline-images': %S"
		      other)
	     nil))))
    (when file-or-data
      (create-image file-or-data
		    (and (image-type-available-p 'imagemagick)
			 width
			 'imagemagick)
		    remote?
		    :width width :mask 'heuristic :ascent 'center))))

(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer.
Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-show-properties)
    (org-hide-properties)))

(setq ivy-use-selectable-prompt t)

(setq
 org-icalendar-combined-agenda-file (or (getenv "ICALENDAR_FILE") "~/org.ics")
 org-icalendar-honor-noexport-tag t
 org-icalendar-timezone "Europe/Amsterdam"
 org-icalendar-include-todo nil
 org-icalendar-include-sexps t
 org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
 org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
 org-icalendar-with-timestamps 'active)

;;; define categories that should be excluded
;; (setq org-export-exclude-category (list "sample"))

;;; define filter. The filter is called on each entry in the agenda.
;;; It defines a regexp to search for two timestamps, gets the start
;;; and end point of the entry and does a regexp search. It also
;;; checks if the category of the entry is in an exclude list and
;;; returns either t or nil to skip or include the entry.

;; (defun org-mycal-export-limit ()
;;   "Limit the export to items that have a date, time and a range. Also exclude certain categories."
;;   (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\
;; \)>")
;;   (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
;;   (save-excursion
;;                                         ; get categories
;;     (setq mycategory (org-get-category))
;;                                         ; get start and end of tree
;;     (org-back-to-heading t)
;;     (setq mystart    (point))
;;     (org-end-of-subtree)
;;     (setq myend      (point))
;;     (goto-char mystart)
;;                                         ; search for timerange
;;     (setq myresult (re-search-forward org-tstr-regexp myend t))
;;                                         ; search for categories to exclude
;;     (setq mycatp (member mycategory org-export-exclude-category))
;;                                         ; return t if ok, nil when not ok
;;     (if (and myresult (not mycatp)) t nil)))

;;; activate filter and call export function
;; (defun psamim-org-ical-export ()
;;   (interactive)
;;   (save-excursion
;;     (org-icalendar-combine-agenda-files)))
;; (let ((org-icalendar-verify-function 'org-mycal-export-limit))
;;   (org-icalendar-combine-agenda-files))))

;; (use-package! calibredb
;;   :defer t
;;   :config
;;   (setq calibredb-format-all-the-icons t)
;;   (setq calibredb-root-dir "~/Calibre/calibre-web/")
;;   (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
;;   (setq calibredb-library-alist '(("~/Calibre/calibre-web/")
;;                                   ("~/Calibre/fidibo")))
;;   (map! :map calibredb-show-mode-map
;;         :ne "?" #'calibredb-entry-dispatch
;;         :ne "o" #'calibredb-find-file
;;         :ne "O" #'calibredb-find-file-other-frame
;;         :ne "V" #'calibredb-open-file-with-default-tool
;;         :ne "s" #'calibredb-set-metadata-dispatch
;;         :ne "e" #'calibredb-export-dispatch
;;         :ne "q" #'calibredb-entry-quit
;;         :ne "." #'calibredb-open-dired
;;         :ne [tab] #'calibredb-toggle-view-at-point
;;         :ne "M-t" #'calibredb-set-metadata--tags
;;         :ne "M-a" #'calibredb-set-metadata--author_sort
;;         :ne "M-A" #'calibredb-set-metadata--authors
;;         :ne "M-T" #'calibredb-set-metadata--title
;;         :ne "M-c" #'calibredb-set-metadata--comments)
;;   (map! :map calibredb-search-mode-map
;;         :ne [mouse-3] #'calibredb-search-mouse
;;         :ne "RET" #'calibredb-find-file
;;         :ne "?" #'calibredb-dispatch
;;         :ne "a" #'calibredb-add
;;         :ne "A" #'calibredb-add-dir
;;         :ne "c" #'calibredb-clone
;;         :ne "d" #'calibredb-remove
;;         :ne "D" #'calibredb-remove-marked-items
;;         :ne "j" #'calibredb-next-entry
;;         :ne "k" #'calibredb-previous-entry
;;         :ne "l" #'calibredb-virtual-library-list
;;         :ne "L" #'calibredb-library-list
;;         :ne "n" #'calibredb-virtual-library-next
;;         :ne "N" #'calibredb-library-next
;;         :ne "p" #'calibredb-virtual-library-previous
;;         :ne "P" #'calibredb-library-previous
;;         :ne "s" #'calibredb-set-metadata-dispatch
;;         :ne "S" #'calibredb-switch-library
;;         :ne "o" #'calibredb-find-file
;;         :ne "O" #'calibredb-find-file-other-frame
;;         :ne "v" #'calibredb-view
;;         :ne "V" #'calibredb-open-file-with-default-tool
;;         :ne "." #'calibredb-open-dired
;;         :ne "b" #'calibredb-catalog-bib-dispatch
;;         :ne "e" #'calibredb-export-dispatch
;;         :ne "r" #'calibredb-search-refresh-and-clear-filter
;;         :ne "R" #'calibredb-search-clear-filter
;;         :ne "q" #'calibredb-search-quit
;;         :ne "m" #'calibredb-mark-and-forward
;;         :ne "f" #'calibredb-toggle-favorite-at-point
;;         :ne "x" #'calibredb-toggle-archive-at-point
;;         :ne "h" #'calibredb-toggle-highlight-at-point
;;         :ne "u" #'calibredb-unmark-and-forward
;;         :ne "i" #'calibredb-edit-annotation
;;         :ne "DEL" #'calibredb-unmark-and-backward
;;         :ne [backtab] #'calibredb-toggle-view
;;         :ne [tab] #'calibredb-toggle-view-at-point
;;         :ne "M-n" #'calibredb-show-next-entry
;;         :ne "M-p" #'calibredb-show-previous-entry
;;         :ne "/" #'calibredb-search-live-filter
;;         :ne "M-t" #'calibredb-set-metadata--tags
;;         :ne "M-a" #'calibredb-set-metadata--author_sort
;;         :ne "M-A" #'calibredb-set-metadata--authors
;;         :ne "M-T" #'calibredb-set-metadata--title
;;         :ne "M-c" #'calibredb-set-metadata--comments))

(setq org-re-reveal-theme "white"
      org-re-reveal-transition "slide"
      org-re-reveal-plugins '(markdown notes math search zoom))

(setq org-caldav-url "https://next.psam.im/remote.php/dav/calendars/psamim"
      org-caldav-sync-direction 'twoway
      org-caldav-delete-org-entries 'always
      org-caldav-delete-calendar-entries 'always
      ;; org-caldav-sync-changes-to-org 'all
      )

(setq org-caldav-calendars
      '(
        (:calendar-id "org-2"
         ;; :sync-direction 'org->cal
         :files ("~/Notes/projects/projects.org"
                 "~/Notes/projects/misc.org"
                 "~/Notes/events.org")
         :inbox "~/Notes/calendar-org-inbox.org")
        (:calendar-id "people"
         :sync-direction 'org->cal
         :files ("~/Notes/roam/20210507181408-people.gpg.org")
         :inbox "~/Notes/calendar-people-inbox.org")
        (:calendar-id "personal-1_shared_by_raffi"
         ;; :sync-direction 'cal->org
         :files ("~/Notes/org-inbox.org")
         :inbox "~/Notes/calendar-inbox.org")
        ))


;; (use-package! org-xournalpp
;;   :config
;;   (add-hook 'org-mode-hook 'org-xournalpp-mode))

;; (after! citar
;;   (setq! citar-bibliography '("/home/samim/workspace/thesis/References.bib"))
;;   (map! :localleader
;;         :map LaTeX-mode-map
;;         :desc "Insert cite"  "t" #'citar-insert-citation)
;;   (map! :localleader
;;         :map latex-mode-map
;;         :desc "Insert cite"  "t" #'citar-insert-citation)
;;   (setq citar-symbols
;;         `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
;;           (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
;;           (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
;;   (setq citar-symbol-separator "  "))

;; https://www.reddit.com/r/orgmode/comments/ub1fuk/new_workflow_for_reading_epubs_and_taking_notes/
;; (org-link-set-parameters
;;  "calibre"
;;  :follow 'my/calibre-follow)

;; (defun my/calibre-follow (path)
;;   (call-process "xdg-open" nil 0 nil (concat "calibre:" path)))

;; Open phone numbers with kde-connect
(defun org-tel-open (number _)
  (start-process "Phone Call"
                 "*call*"
                 "kdeconnect-handler"
                 (concat "tel:" number)))

;; https://github.com/doomemacs/doomemacs/issues/6478
;; Configuration A
(setq org-fold-core-style 'overlays)
(evil-select-search-module 'evil-search-module 'evil-search)

;; Configuration B
;; (setq org-fold-core-style 'text-properties)
;; (evil-select-search-module 'evil-search-module 'isearch)


(pixel-scroll-precision-mode)


(after! org-roam
  (require 'org-roam-dailies)
  (setq
   org-roam-directory "~/Notes/roam"
   org-roam-dailies-directory "journal"
   org-roam-dailies-capture-templates
   '(
     ("b" "archive-to-today" entry "* %?" :target
      (file+datetree "%<%Y>.org" "week"))
     )

   ;; https://github.com/org-roam/org-roam/issues/2143#issuecomment-1357558467
   org-roam-node-display-template
   #("${doom-hierarchy:*} ${doom-type:10} ${doom-tags:10}" 20 35
     (face font-lock-keyword-face)
     36 51
     (face org-tag))
   )
  )

;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/#automatically-copy-or-move-completed-tasks-to-dailies
(defun psamim/org-roam-refile-to-today ()
  (interactive)
  (let ((org-refile-keep nil) ;; Set this to nil to delete the original!
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "" today-file nil pos)))))

(after! git-gutter
  (setq git-gutter:disabled-modes '(org-mode image-mode)))

