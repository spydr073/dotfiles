
;; Author: Tom Willemsen <tom@ryuslash.org>

;;; Code:

(deftheme tron
  "Create 2012-01-01")

(custom-theme-set-faces
 'tron
 '(bold ((t (:foreground "#24c6e0" :weight bold))))
 '(bold-italic ((t (:inherit bold :slant italic))))
 '(cursor ((t (:background "#15abc3"))))
 '(default ((t (:background "#000000" :foreground "#15abc3"))))
 '(fringe ((t (:background "#000000" :foreground "#15abc3"))))
 '(header-line ((t (:inherit mode-line))))
 '(linum ((t (:inherit default))))
 '(mouse ((t (:foreground "#e0c625"))))
 '(region ((t (:background "#e0c625" :foreground "#000000"))))
 '(show-paren-match-face ((t (:weight bold))))

 ;; column-marker
 '(column-marker-1 ((t (:background "#a3e8ef" :foreground "#000000"))))
 '(column-marker-2 ((t (:background "#55c3f8" :foreground "#000000"))))
 '(column-marker-3 ((t (:background "#f6faf9" :foreground "#000000"))))

 ;; css
 '(css-property ((t (:inherit font-lock-variable-name-face))))
 '(css-proprietary-property ((t (:inherit mouse))))

 ;; custom
 '(custom-button ((t (:inherit default :box (:line-width 1 :color "#15abc3")))))
 '(custom-button-mouse ((t (:inherit custom-button :box (:color "#e0c624")))))
 '(custom-button-pressed ((t (:inherit custom-button :box (:color "#05e4a5")))))

 ;; Eshell
 '(eshell-ls-archive-face ((t (:foreground "#029cdc" :bold t))))
 '(eshell-ls-backup-face ((t (:inherit font-lock-builtin-face :italic t))))
 '(eshell-ls-clutter-face ((t (:foreground "OrangeRed" :bold t))))
 '(eshell-ls-directory-face ((t (:foreground "#e0c625" :bold t))))
 '(eshell-ls-executable-face ((t (:foreground "#00815b"))))
 '(eshell-ls-missing-face ((t (:foreground "#e0c624" :bold t))))
 '(eshell-ls-product-face ((t (:foreground "OrangeRed"))))
 '(eshell-ls-readonly-face ((t (:foreground "#808080" :italic t))))
 '(eshell-ls-special-face ((t (:foreground "Mauve" :bold t))))
 '(eshell-ls-symlink-face ((t (:foreground "#035390" :bold t))))
 '(eshell-ls-unreadable-face ((t (:foreground "Grey30"))))
 '(eshell-prompt-face ((t (:foreground "#e0c625"))))
 '(eshell-test-failed-face ((t (:foreground "OrangeRed" :bold t))))
 '(eshell-test-ok-face ((t (:foreground "#05e4a5" :bold t))))

 ;; font-lock
 '(font-lock-builtin-face ((t (:foreground "#029cdc"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#2e3436" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "#808080" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#029cdc"))))
 '(font-lock-doc-face ((t (:foreground "#00815b"))))
 '(font-lock-function-name-face ((t (:foreground "#e0c624" :bold t))))
 '(font-lock-keyword-face ((t (:inherit font-lock-builtin-face :weight bold))))
 '(font-lock-negation-char-face ((t (:foreground "#c3ab15"))))
 '(font-lock-preprocessor-face ((t (:foreground "#c3ab15" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#05e4a5"))))
 '(font-lock-type-face ((t (:foreground "#029cdc" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#00815b"))))
 '(font-lock-warning-face ((t (:foreground "OrangeRed"))))

 ;; info
 '(info-title-1 ((t (:inherit org-level-1))))
 '(info-title-2 ((t (:inherit org-level-2))))
 '(info-title-3 ((t (:inherit org-level-3))))
 '(info-title-4 ((t (:inherit org-level-4))))
 '(info-xref ((t (:inherit default :underline t))))
 '(info-xref-visited ((t (:inherit mouse :underline t))))

 ;; magit
 '(magit-header ((t (:foreground "#e0c624" :weight bold))))
 '(magit-diff-add ((t (:foreground "#00815b"))))
 '(magit-diff-del ((t (:foreground "OrangeRed"))))
 '(magit-diff-hunk-header ((t (:background "#101010"))))
 '(magit-diff-file-header ((t (:weight bold :inherit magit-diff-hunk-header))))
 '(magit-item-highlight ((t (:background "#101010"))))
 '(magit-item-mark ((t (:background "#808080"))))

 ;; mode-line
 '(mode-line ((t (:foreground "#e0c625" :background nil :box nil))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-inactive ((t (:foreground "#15abc3" :background nil :box nil))))

 ;; rainbow-delimiters
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#a3e8ef"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#55c3f8"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#f6faf9"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#4ed8f5"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#004a88"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#f0ffff"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#51d7f0"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#be9194"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#94949c"))))

 ;; rainbow-delimiters
 '(org-level-1 ((t (:inherit rainbow-delimiters-depth-1-face :weight bold))))
 '(org-level-2 ((t (:inherit rainbow-delimiters-depth-2-face :weight bold))))
 '(org-level-3 ((t (:inherit rainbow-delimiters-depth-3-face :weight bold))))
 '(org-level-4 ((t (:inherit rainbow-delimiters-depth-4-face :weight bold))))
 '(org-level-5 ((t (:inherit rainbow-delimiters-depth-5-face :weight bold))))
 '(org-level-6 ((t (:inherit rainbow-delimiters-depth-6-face :weight bold))))
 '(org-level-7 ((t (:inherit rainbow-delimiters-depth-7-face :weight bold))))
 '(org-level-8 ((t (:inherit rainbow-delimiters-depth-8-face :weight bold))))

 ;; rst
 '(rst-level-1-face ((t (:inherit rainbow-delimiters-depth-1-face :weight bold))))
 '(rst-level-2-face ((t (:inherit rainbow-delimiters-depth-2-face :weight bold))))
 '(rst-level-3-face ((t (:inherit rainbow-delimiters-depth-3-face :weight bold))))
 '(rst-level-4-face ((t (:inherit rainbow-delimiters-depth-4-face :weight bold))))
 '(rst-level-5-face ((t (:inherit rainbow-delimiters-depth-5-face :weight bold))))
 '(rst-level-6-face ((t (:inherit rainbow-delimiters-depth-6-face :weight bold))))

 ;; outline
 '(outline-1 ((t (:inherit org-level-1))))
 '(outline-2 ((t (:inherit org-level-2))))
 '(outline-3 ((t (:inherit org-level-3))))
 '(outline-4 ((t (:inherit org-level-4))))
 '(outline-5 ((t (:inherit org-level-5))))
 '(outline-6 ((t (:inherit org-level-6))))
 '(outline-7 ((t (:inherit org-level-7))))
 '(outline-8 ((t (:inherit org-level-8))))

 ;; markdown
 '(markdown-header-face-1 ((t (:inherit org-level-1))))
 '(markdown-header-face-2 ((t (:inherit org-level-2))))
 '(markdown-header-face-3 ((t (:inherit org-level-3))))
 '(markdown-header-face-4 ((t (:inherit org-level-4))))
 '(markdown-header-face-5 ((t (:inherit org-level-5))))
 '(markdown-header-face-6 ((t (:inherit org-level-6))))

 ;; jabber
 '(jabber-chat-prompt-local ((t (:foreground "#15abc3"  :weight bold))))
 '(jabber-chat-prompt-foreign ((t (:inherit font-lock-function-name-face))))
 '(jabber-chat-prompt-system ((t (:inherit font-lock-doc-face))))
 '(jabber-chat-text-local ((t (:foreground "#15abc3"))))
 '(jabber-chat-text-foreign ((t (:inherit font-lock-function-name-face :weight normal))))
 '(jabber-activity-face ((t (:foreground "#00815b"))))
 '(jabber-activity-personal-face ((t (:foreground "OrangeRed"))))

 ;; erc
 '(erc-default-face ((t (:inherit jabber-chat-text-local))))
 '(erc-input-face ((t (:inherit jabber-chat-text-foreign))))
 '(erc-nick-default-face ((t (:inherit jabber-chat-prompt-system)))))

(provide-theme 'tron)

;; Local Variables:
;; eval: (rainbow-mode 1)
;; End:

;;; tron-theme.el ends here
