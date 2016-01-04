(defun add-semicolon-at-end-of-line ()
  """Add a semicolon at the end of  the current line."""
  (interactive)
  (end-of-line)
  (insert ";"))
