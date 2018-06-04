
;;-- Bootstrap Config
(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)


;;-- Init Paths
(add-to-list 'load-path (expand-file-name "~/dotfiles/emacs/conf/"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/emacs/conf/langs/"))
(add-to-list 'custom-theme-load-path "~/dotfiles/emacs/conf/themes")
(use-package exec-path-from-shell
    :ensure t
    :config
    (setenv "SHELL" "/run/current-system/sw/bin/zsh")
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-copy-env "TERM")
    (exec-path-from-shell-initialize))


;;-- Local Info
(setq user-full-name "spydr073")
(setq user-mail-address "spydr073@gmail.com")

;;-- Load Settings
(require 'settings-config )
(require 'packages-config )
(require 'func-config)
(require 'term-config)
(require 'evil-config)
(require 'modeline-config)

;;-- Load Langs


;; Seperate the generated customizations
(setq custom-file "~/dotfiles/emacs/custom.el")
(load custom-file 'noerror)




;;-- REFACTOR LANGS --;;

;; web langs
(use-package web-mode
    :ensure t
    :mode ("\\.html\\'")
    :config
    (setq web-mode-markup-indent-offset 2)
    )

;; haskell
(use-package haskell-mode :ensure t)
(use-package hindent :ensure t)

;; idris
(use-package idris-mode :ensure t)
