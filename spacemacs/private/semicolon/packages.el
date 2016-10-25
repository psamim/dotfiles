(setq semicolon-packages
      '(
        key-chord
        ))

(defun semicolon/init-key-chord ()
  """Forces Spacemacs to install key-chord-mode.""")
(defun semicolon/post-init-key-chord ()
  """Add the proper shortcuts."""
  (add-hook 'prog-mode-hook
            (lambda ()
              (key-chord-mode 1)
              (key-chord-define evil-insert-state-map "jj"
                                'add-semicolon-at-end-of-line))))
