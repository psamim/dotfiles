(setq keybindings-psamim-packages
      '(ranger
        ))

(defun keybindings-psamim/post-init-ranger ()
  (progn
    (spacemacs/set-leader-keys
      "d" 'ranger)
    ))
