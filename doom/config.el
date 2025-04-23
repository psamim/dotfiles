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
      doom-symbol-font (font-spec :family "Iosevka")
      ;; doom-big-font (font-spec :family "Fira Mono" :size 19)
      )

(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(custom-set-variables '(emojify-display-style 'unicode))

(+bidi-global-mode 1)
(setq +bidi-want-smart-fontify nil)
(setq +bidi-arabic-font (font-spec :family "Mikhak"))

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
     '("~/Notes/calendar/shared-inbox.org"
       "~/Notes/study.org"
       "~/Notes/roam/people.org.gpg"
       "~/Notes/events.org"))))


(setq mixed-pitch-fixed-pitch-faces
      (quote (line-number-current-line line-number font-lock-comment-face org-done org-todo org-todo-keyword-outd org-todo-keyword-kill org-todo-keyword-wait org-todo-keyword-done org-todo-keyword-habt org-todo-keyword-todo org-tag org-ref-cite-face org-property-value org-special-keyword org-date diff-added org-drawer diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-table org-verbatim)))


;; Compute agenda files more frequently
(setq-hook! org-agenda-mode
  org-agenda-files
  (org-agenda--calculate-files-for-regex org-agenda--todo-keyword-regex))

(setq org-attach-id-dir "attachments")

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
   ("chat" "~/.dotfiles/icons/chat.svg" nil nil :ascent center :mask heuristic)
   ("letter" "~/.dotfiles/icons/letter.svg" nil nil :ascent center :mask heuristic)
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


(defun psamim/insert-persian-date ()
  (interactive)
  (insert
   (shell-command-to-string "~/.bin/persian-date")))

(defun psamim/insert-time ()
  (interactive)
  (insert (format-time-string " [%I:%M]" (current-time))))

(add-hook! '(org-clock-out-hook org-clock-in-hook) #'org-save-all-org-buffers)
(advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)

(defun psamim/my-org-agenda ()
  (interactive)
  (if (eq system-type 'darwin)  ;; Work laptop
      (org-agenda nil "w")
    (org-agenda nil "a")))

(defun psamim/my-org-index ()
  (interactive)
  (find-file "~/Notes/roam/20210625-index.org"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(after! company
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1
        company-dabbrev-code-everywhere t
        company-dabbrev-code-other-buffers 'all))

(set-company-backend!
 '(text-mode
   markdown-mode
   org-mode
   gfm-mode)
 '(:seperate
   company-ispell
   company-files
   company-dabbrev
   company-yasnippet))

(add-hook 'text-mode-hook
          (lambda ()
            (setq olivetti-body-width 0.91)
            (olivetti-mode)
            (setq header-line-format " ")
            (mixed-pitch-mode 1)))

;; Transparency
(set-frame-parameter nil 'alpha-background 94)
(add-to-list 'default-frame-alist '(alpha-background . 94))
(set-frame-parameter (selected-frame) 'alpha '(94 94))

(defun psamim/toggle-transparency ()
  "Toggle transparency"
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (eq
         (if (numberp alpha)
             alpha
           (cdr alpha)) ; may also be nil
         100)
        (set-frame-parameter nil 'alpha '(94 . 94))
      (set-frame-parameter nil 'alpha '(100 . 100)))))

(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; macos
(if (eq system-type 'darwin)
    (progn
      (setq-hook! org-mode
        epa-file-encrypt-to "psamim+personio@gmail.com"
        org-crypt-key "psamim+personio@gmail.com"
        ns-use-proxy-icon nil
        frame-title-format nil)

      (add-to-list 'default-frame-alist '(undecorated-round . t))))

(defun my-org-mode-autosave-settings ()
  (add-hook 'auto-save-hook 'org-save-all-org-buffers nil nil))

(add-hook 'org-agenda-finalize-hook #'psamim/set-agenda-window-clean)
(add-hook! 'org-mode-hook #'my-org-mode-autosave-settings)

(add-hook 'org-mode-local-vars-hook #'(lambda () (eldoc-mode -1)))

(defun psamim/agenda-color-char ()
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

(defun psamim/set-agenda-window-clean ()
  (setq line-spacing 2)
  (psamim/agenda-color-char)
  (+bidi-mode -1)
  (setq mode-line-format nil)
  ;; (mixed-pitch-mode 1)
  ;; (set-frame-parameter nil 'font "Iosevka-18")
  (setq header-line-format " ")
  (set-face-attribute 'header-line nil :background "#00000000")
  (set-window-margins (frame-selected-window) 4))


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

(doom-modeline-mode 0)
(use-package! doom-modeline
  :init
  (setq doom-modeline-check-simple-format t)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode org-mode)))

;; Define your custom doom-modeline
(doom-modeline-def-modeline 'my-simple-line
  '(buffer-info selection-info)
  '(misc-info input-method process))

;; Add to `doom-modeline-mode-hook` or other hooks
(defun psamim/setup-custom-doom-modeline ()
  (doom-modeline-set-modeline 'my-simple-line 'default))
(add-hook 'doom-modeline-mode-hook 'psamim/setup-custom-doom-modeline)

;; Use visual mode and "gt" to make Arabic numbers into Latin
(use-package! unidecode
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
   "gu" 'evil-unidecode))


(setq evil-normal-state-cursor '(box "light blue")
      evil-insert-state-cursor '(bar "medium sea green")
      evil-visual-state-cursor '(hollow "orange"))

(defun psamim/add-property-with-date-captured ()
  "Add DATE_CAPTURED property to the current item."
  (interactive)
  (org-set-property "CREATED" (format-time-string "%F")))

(add-hook 'org-capture-before-finalize-hook 'psamim/add-property-with-date-captured)

(after! org
  (custom-set-variables
   '(org-capture-templates
     (quote (
             ("t" "todo" entry
              (file "~/Notes/roam/20240521-focus_board.org") "* TODO %?\n%a\n" :clock-keep t)
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
   epa-file-encrypt-to "psamim@gmail.com"
   org-clock-persist t
   org-tags-exclude-from-inheritance '("project" "crypt")
   ;; org-duration-format 'h:mm
   org-tag-alist '(("crypt" . ?c) ("project" . ?p) ("weekly" . ?w) ("monthly" . ?m))
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
     ("\\.jpe?g\\'" . default)
     (auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "okular %s"))
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
                        (org-agenda-span 5)
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
                        (org-agenda-span 5)
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

;; (use-package! org-fancy-priorities ; priority icons
;;   :hook (org-mode . org-fancy-priorities-mode)
;;   :config (setq org-fancy-priorities-list '("⚑" "⬆" "⬇")))




(setq ispell-dictionary "en_US")


(defun load-secrets () (interactive) (load "~/.doom.d/secrets.el.gpg"))

;; https://orgmode.org/manual/Filtering_002flimiting-agenda-items.html
(defun my-auto-exclude-fn (tag)
  (when (member tag '("work" "global"))
    (concat "-" tag)))

(setq org-agenda-auto-exclude-function #'my-auto-exclude-fn)

(defun psamim/do-not-display-work ()
  (interactive)
  (org-agenda-filter-apply '("-work") 'category))

(defun psamim/save-screenshot-svg ()
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

(defun psamim/sync-agenda-svg ()
  "Save a screenshot of the current frame as an SVG image.
  Saves to a chosen file and puts the filename in the kill ring."
  (interactive)
  (progn
    (org-agenda nil "mo")
    (hl-line-mode -1)
    (org-agenda-redo-all)
    (goto-char 100000)
    (setq cursor-type nil)
    (psamim/save-screenshot-svg)
    (setq cursor-type 'box)))

(defun psamim/sync ()
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

(map! :map magit-status-mode-map :n "<tab>" 'magit-section-toggle)
(map! "M-q" #'kill-current-buffer)
(map! "M-n" #'vterm)
(map! :localleader
      (:map ledger-mode-map
            "c" #'ledger-mode-clean-buffer))
(map! :localleader (:map org-agenda-mode-map "f p" #'psamim/do-not-display-work))
(map! :localleader (:map org-agenda-mode-map "o" #'org-agenda-set-property))
(map! :localleader (:map org-agenda-mode-map "c" #'org-agenda-columns))
(map! :leader :desc "Org clock context" :nvg "n c" #'counsel-org-clock-context)
(map! :leader :desc "Dired" :nvg "d" #'dirvish-dwim)
(map! :leader :desc "my-org-agenda" :nvg "na" 'psamim/my-org-agenda)
(map! :leader :desc "my-org-index" :nvg "ni" 'psamim/my-org-index)
(map! :leader :desc "sync-agenda-svg" :nvg "ra" 'psamim/sync-agenda-svg)
(map! :localleader (:map org-mode-map :desc "roam-refile-to-date" :nvg "rt" 'psamim/org-roam-refile-to-date))
(map! :localleader (:map org-mode-map :desc "insert-created-property" :nvg "dc" 'psamim/insert-created-property))
(map! :localleader (:map org-mode-map :desc "insert Persian date" :nvg "dp" 'psamim/insert-persian-date))
(map! :localleader (:map org-mode-map :desc "insert time" :nvg "dn" 'psamim/insert-time))
(map! :map typescript-mode-map :nv "gh" 'lsp-ui-doc-glance)

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
      '((:calendar-id "org-2"
         ;; :sync-direction 'org->cal
         :files ("~/Notes/roam/20240521-focus_board.org"
                 "~/Notes/events.org")
         :inbox "~/Notes/calendar/org-inbox.org")
        (:calendar-id "people"
         ;; :sync-direction 'org->cal
         :files ("~/Notes/roam/people.org.gpg")
         :inbox "~/Notes/calendar/people-inbox.org")
        (:calendar-id "personal-1_shared_by_raffi"
         ;; :sync-direction 'cal->org
         :files ()
         :inbox "~/Notes/calendar/shared-inbox.org")))


;; (use-package! org-xournalpp
;;   :config
;;   (add-hook 'org-mode-hook 'org-xournalpp-mode))

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

(setq +org-roam-open-buffer-on-find-file nil)
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  :hook (org-roam . org-roam-ui-mode)
  :config
  )

(after! org-roam
  (require 'org-roam-dailies)
  (setq
   org-roam-db-location "~/Notes/roam/db.sqlite"
   org-roam-directory "~/Notes/roam"
   org-roam-dailies-directory "journal"
   org-roam-capture-templates
   '(("d" "default" plain "%?" :target (file+head "%<%Y%m%d>-${slug}.org" "#+title: ${title}\n")))

   org-roam-dailies-capture-templates
   '(("b" "archive-to-today" entry "* %?" :target
      (file+datetree "%<%Y>.org.gpg" "week")))

   ;; https://github.com/org-roam/org-roam/issues/2143#issuecomment-1357558467
   org-roam-node-display-template
   #("${doom-hierarchy:*} ${doom-type:10} ${doom-tags:10}" 20 35
     (face font-lock-keyword-face)
     36 51
     (face org-tag))
   ))

;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/#automatically-copy-or-move-completed-tasks-to-dailies
(defun psamim/org-roam-refile-to-date ()
  (interactive)
  (let ((org-refile-keep nil) ;; Set this to nil to delete the original!
        (org-after-refile-insert-hook #'save-buffer)
        (date
         (time-convert
          (org-time-string-to-seconds
           (org-read-date t nil (org-entry-get nil "CREATED" nil)))
          1))
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture date t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "" today-file nil pos)))))

(after! git-gutter
  (setq git-gutter:disabled-modes '(org-mode image-mode)))

(require 'org-expiry)
(setq
 org-expiry-created-property-name "CREATED" ; Name of property when an item is created
 org-expiry-inactive-timestamps   t         ; Don't have everything in the agenda view
 )

(defun psamim/insert-created-property()
  "Insert a CREATED property using org-expiry.el"
  (interactive)
  (org-expiry-insert-created)
  (org-back-to-heading)
  (org-end-of-line)
  (insert " "))

;; (use-package! org-tidy
;;   :config (setq
;;            org-tidy-properties-style 'invisible
;;            org-tidy-protect-overlay nil)
;;   :hook
;;   (org-mode . org-tidy-mode))

(setq +ligatures-in-modes '())
(setq +ligatures-extras-in-modes '(org-mode))

(use-package! jest-test-mode
  :ensure t
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))
