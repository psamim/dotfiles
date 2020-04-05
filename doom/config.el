;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
(setq doom-font (font-spec :family "Iosevka" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq
  org-agenda-files (quote ("~/Notes/todo.org" "~/Notes/appointments.org"))
  org-directory "~/Notes")

(setq-hook! org-mode
  org-log-done t
  org-image-actual-width '(700)
  org-clock-into-drawer t
  org-clock-persist t
  org-columns-default-format "%60ITEM(Task) %20TODO %10Effort(Effort){:} %10CLOCKSUM"
  org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                ("STYLE_ALL" . "habit")))
  org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar")
  ;; org-export-babel-evaluate nil
  org-confirm-babel-evaluate nil
  org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE"))
  org-archive-location "~/Notes/archive/todo.org::"
  org-duration-format 'h:mm
  org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  bidi-paragraph-direction t
  org-icalendar-timezone "Asia/Tehran"
  ;; org-caldav-url 'google
  ;; org-caldav-calendar-id "ovuticv96133cisuc0pm8f7d6g@group.calendar.google.com"
  ;; org-caldav-files '("~/Notes/appointments.org")
  ;; org-caldav-oauth2-client-id "279358326453-ar2bfnerndjnnie90e59i9otuif9ut84.apps.googleusercontent.com"
  ;; org-caldav-oauth2-client-secret "SECRET"
  ;; org-caldav-inbox "~/Notes/calendar-inbox.org"
  )



(defun psamim-journal-prefix ()
  (let*
      (
       (time (format-time-string "%s"))
       (decodedTime (decode-time time))
       (now (list (nth 4 decodedTime) (nth 3 decodedTime) (nth 5 decodedTime))))
    (concat
     ;; (format-time-string "%B %e, %Y" time) "\n"
     (format-time-string "%A" time)
     (calendar-persian-date-string now)
     ;; (calendar-bahai-date-string now) "\n\n")
    )))

(customize-set-variable 'org-journal-file-type `weekly)
(setq!
 org-journal-dir "~/Notes/journal"
 org-journal-encrypt-journal t)
 ; org-journal-date-prefix (psamim-journal-prefix))

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(setq org-capture-templates '(("j" "Journal entry" entry (function org-journal-find-location)
                               "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))


(add-hook! '(org-clock-out-hook org-clock-in-hook) #'org-save-all-org-buffers)
(advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)
(defun my-org-mode-autosave-settings ()
  (add-hook 'auto-save-hook 'org-save-all-org-buffers nil nil))
(add-hook 'org-mode-hook 'my-org-mode-autosave-settings)


(defun export-clock ()
  (interactive)
  (org-clock-csv-to-file "~/clock.csv" "~/Notes/clock.org"))

(map! :localleader
      (:map org-mode-map
        "c e" #'export-clock))

(map! :localleader
      (:map ledger-mode-map
        "c" #'ledger-mode-clean-buffer))

(after! ledger-mode
  (set-company-backend! 'ledger-mode 'ledger-mode))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(set-fontset-font
 "fontset-default"
 (cons (decode-char 'ucs #x0600) (decode-char 'ucs #x06ff)) ; arabic
 ;; "Vazir Code-13")
 "Shabnam-12")

;; (after! org-mode
;;   (set-company-backend! 'company-dabbrev)
;;   )
