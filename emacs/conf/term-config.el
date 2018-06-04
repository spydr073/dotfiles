
(use-package eshell
  :config

  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
		       (file-name-directory (buffer-file-name))
		     default-directory))
	   (height (/ (window-total-height) 3))
	   (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))
      (insert (concat "ls"))
      (eshell-send-input)))

  (defun eshell/x ()
    (insert "exit")
    (eshell-send-input)
    (delete-window))

  ) ;;-- end eshell

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-copy-env "PATH"))

(use-package multi-term
  :init
  (setq multi-term-program "/run/current-system/sw/bin/zsh"))

(provide 'term-config)
