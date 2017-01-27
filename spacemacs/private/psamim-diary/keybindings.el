(spacemacs|define-transient-state psamim-diary
  :title "Diary Transient State"
  :doc "
[_p_] [_N_] previous day [_n_] next day [_d_] previous years today [_t_] today [_q_] quit"
  :bindings
  ("n" psamim-diary-show-next)
  ("p" psamim-diary-show-prev)
  ("N" psamim-diary-show-prev)
  ("d" psamim-diary-dired-day)
  ("t" psamim-diary-open-today)
  ("q" nil :exit t))

(spacemacs/set-leader-keys
  "aa" 'spacemacs/psamim-diary-transient-state/body)

