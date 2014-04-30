(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


(defun my-add-semicolon-at-the-end-of-line ()
  "Add a semicolon to the end of line and go to next"
  (interactive) ; Do the following interactively
  (end-of-line) ; Move to the end of line
  (insert ";") ; Add the semicolon
  (evil-normal-state))

(setq path-to-ctags "TAGS")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags-exuberant --exclude=.git -f %s -e -R %s" path-to-ctags (directory-file-name dir-name))))

(defun my-window-killer ()
  "closes the window, and deletes the buffer if it's the last window open."
  (interactive)
  (if (> buffer-display-count 1)
      (if (= (length (window-list)) 1)
          (kill-buffer)
        (delete-window))
    (kill-buffer-and-window)))


(defun my-minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))


(defun set-transparency (alpha)
  "Sets the transparency of the current frame."
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha alpha))


(defun my-google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search Google: "))))))

(defun my-ddg ()
  "DuckDuckGo the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "https://duckduckgo.com/?q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search DDG: "))))))

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun my-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))


(defun my-delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))


;; make sure $PATH is set correctly
(require-package 'exec-path-from-shell)
(ignore-errors ;; windows
  (exec-path-from-shell-initialize))


(defun switch-full-screen ()
 (interactive)
 (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(defun my-insert-today-date ()
  (interactive)
   (insert
          (format-time-string "%Y-%m-%d")))

(provide 'init-util)
