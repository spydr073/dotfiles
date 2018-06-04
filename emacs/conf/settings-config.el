
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode 1)
(minibuffer-complete)
(setq require-final-newline t)

(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

(global-visual-line-mode 1)
(global-prettify-symbols-mode)
(show-paren-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'focus-out-hook #'garbage-collect)
(global-auto-revert-mode t)

(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq scroll-margin 10
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

(set-face-attribute 'default nil
		    :family "Inconsolata"
		    :height 120
		    :weight 'normal
		    :width  'normal)
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
		    'unicode
		    (font-spec :family "DejaVu Sans Mono"
			       :width 'normal
			       :size 15.5
			       :weight 'normal)))

(use-package custom-theme
  :load-path "~/dotfiles/emacs/themes"
  :defer t
  :init
  (load-theme 'custom t)
  (modify-frame-parameters nil (list (cons 'alpha 90)))
  )

(require 'server)
(if (not (server-running-p)) (server-start))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)



(provide 'settings-config)
