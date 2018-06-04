
(use-package all-the-icons
  :ensure t
  :config
  (setq inhibit-compacting-font-caches t))

(use-package multi-term
  :init
  (setq multi-term-program "/run/current-system/sw/bin/zsh"))

(use-package nlinum-relative
  :ensure t
  :requires evil
  :config
  (nlinum-relative-setup-evil)
  (setq nlinum-relative-redisplay-delay 0.15)
  (setq nlinum-relative-current-symbol "")
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

(use-package whitespace
  :ensure t
  :config
  (unless (member 'whitespace-mode prog-mode-hook)
    (add-hook 'prog-mode-hook 'whitespace-mode))
  (global-set-key (kbd "C-c w") 'whitespace-cleanup)
  (set-default 'indicate-empty-lines t)
  (set-default 'indent-tabs-mode nil)
  (setq whitespace-style '(face trailing lines-tail tabs)
	whitespace-line-column 99)

  (defun untabify-buffer ()
    (interactive)
    (untabify (point-min) (point-max)))

  (defun indent-buffer ()
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a buffer."
    (interactive)
    (indent-buffer)
    (untabify-buffer)
    (delete-trailing-whitespace))
  )

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode:
  :config (global-undo-tree-mode t))

(use-package undohist
  :ensure t
  :config (undohist-initialize))

(use-package use-package-chords
    :ensure t
    :config
    (key-chord-mode t))

(use-package ace-window
    :ensure t
    :chords ("jk" . ace-window)
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package neotree
    :ensure t
    :config
    (setq neo-smart-open t)
    (setq neo-theme 'arrow))

(use-package magit :ensure t :defer t)

(use-package fzf :ensure t)

(use-package git-gutter
    :ensure t
    :diminish git-gutter-mode
    :config
    (global-git-gutter-mode t))

(use-package autopair
  :ensure t
  :config
  (autopair-global-mode t))

(use-package smartparens
    :ensure t
    :diminish smartparens-mode
    :config
    (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package rainbow-delimiters
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-mode
    :ensure t
    :config
    (setq rainbow-x-colors nil)
    (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package aggressive-indent :ensure t)


(provide 'packages-config)
