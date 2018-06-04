
(use-package evil
  :ensure t
  :init
  :config

  (use-package powerline-evil :ensure t :requires evil)

  (use-package evil-leader
    :ensure t
    :requires evil
    :init
    :config
    (setq evil-leader/in-all-states t)
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>")

    (setq evil-motion-state-modes
	  (append evil-emacs-state-modes
		  evil-motion-state-modes))
    (setq evil-emacs-state-modes nil)

    (evil-leader/set-key
      "w" 'save-buffer
      "e" 'find-file

      "b" 'switch-to-buffer
      "d" 'kill-this-buffer

      "u" 'undo-tree-visualize

      "t" 'neotree-toggle

      "q" 'delete-window
      "=" 'split-window-right
      "-" 'split-window-below

      "h" 'evil-window-left
      "j" 'evil-window-down
      "k" 'evil-window-up
      "l" 'evil-window-right

      "H" 'shrink-window-horizontally
      "J" 'shrink-window
      "K" 'enlarge-window
      "L" 'enlarge-window-horizontally
      )

    (evil-leader/set-key-for-mode 'emacs-lisp-mode
      "x" 'eval-region
      )

    )

  (evil-mode)

  (setq evil-normal-state-tag   "NORMAL"
	evil-emacs-state-tag    "EMACS"
	evil-insert-state-tag   "INSERT"
	evil-replace-state-tag  "REPLACE"
	evil-motion-state-tag   "MOTION"
	evil-visual-state-tag   "VISUAL"
	evil-operator-state-tag "OPERATOR"
	)

  (setq evil-emacs-state-cursor    '("red"    box)
	evil-normal-state-cursor   '("green"  box)
	evil-visual-state-cursor   '("orange" box)
	evil-insert-state-cursor   '("red"    bar)
	evil-replace-state-cursor  '("red"    (bar 0.2))
	evil-operator-state-cursor '("red"    hollow)
	)

  (define-key evil-motion-state-map (kbd "\\") 'evil-ex)

  (define-key evil-normal-state-map (kbd "C-k")
    (lambda ()
      (interactive)
      (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-j")
    (lambda ()
      (interactive)
      (evil-scroll-down nil)))

  );;-- end evil config


;; esc quits
(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


(provide 'evil-config)
