
(deftheme custom "Custom Emacs Theme")

(let ((bg             "#191919")
      (fg             "#87875f")

      (Boolean        "#e6fff3")
      (Character      "#e6fff3")
      (Comment        "#43705a")
      (Conditional    "#e6fff3")
      (Constant       "#e6fff3")
      (Cursor         "#43705a")
      (Debug          "#61a181")
      (Define         "#e6fff3")
      (Delimiter      "#61a181")
      (DiffAdd        "#e6fff3")
      (DiffChange     "#e6fff3")
      (DiffDelete     "#e6fff3")
      (DiffText       "#000000")
      (Directory      "#e6fff3")
      (Error          "#e6fff3")
      (ErrorMsg       "#e6fff3")
      (Exception      "#e6fff3")
      (Float          "#e6fff3")
      (FoldColumn     "#aec6cf")
      (Folded         "#aec6cf")
      (Function       "#e6fff3")
      (Identifier     "#e6fff3")
      (Ignore         "#000000")
      (Include        "#e6fff3")
      (IncSearch      "#1d3026")
      (Keyword        "#e6fff3")
      (Label          "#e6fff3")
      (lCursor        "#43705a")
      (Macro          "#e6fff3")
      (ModeMsg        "#b19cd9")
      (MoreMsg        "#aec6cf")
      (NonText        "#332233")
      (Normal         "#9bcfb5")
      (Number         "#e6fff3")
      (Operator       "#e6fff3")
      (PreCondit      "#e6fff3")
      (PreProc        "#61a181")
      (Question       "#9bcfb5")
      (Repeat         "#e6fff3")
      (Search         "#61a181")
      (Special        "#61a181")
      (SpecialChar    "#61a181")
      (SpecialComment "#61a181")
      (SpecialKey     "#332233")
      (Statement      "#e6fff3")
      (StatusLineNC   "#1d3026")
      (StorageClass   "#f070a0")
      (String         "#e6fff3")
      (Structure      "#f070a0")
      (Tag            "#61a181")
      (Title          "#e6fff3")
      (Todo           "#884433")
      (Type           "#e6fff3")
      (Typedef        "#f070a0")
      (Underlined     "#e6fff3")
      (VertSplit      "#61a181")
      (Visual         "#e6fff3")
      (VisualNOS      "#9bcfb5")
      (WarningMsg     "#1d3026")
      (WildMenu       "#43705a")
      )

  (custom-theme-set-faces
   'custom

   ;; defaults
   `(default     ((t (:foreground ,fg
		      :background ,bg))))
   `(bold        ((t (:weight bold))))
   `(bold-italic ((t (:weight bold
		      :slant italic))))
   `(italic      ((t (:slant italic))))
   `(underline   ((t (:underline t))))
   `(cursor      ((t (:background ,Cursor))))

   `(linum       ((t (:foreground ,ModeMsg
	       	      :height 0.6))))

   ;; highlight
   `(fringe                 ((t (:background ,bg))))
   `(highlight              ((t (:background "#222222"))))
   `(region                 ((t (:background ,lCursor))))
   `(secondary-selection    ((t (:background "#333333"))))
   `(isearch                ((t (:foreground nil
				 :background "#FDBD33"
		 		 :weight bold
				 :underline  "#FF9632" ))))
   `(isearch-fail           ((t (:foreground "black"
				 :background "#FF9999"
				 :weight bold))))
   `(lazy-highlight         ((t (:background "#FFFF00"
				 :underline  "#FF9632"))))
   `(trailing-whitespace    ((t (:background ,NonText))))
   `(query-replace          ((t (:inherit isearch))))
   `(whitespace-hspace      ((t (:foreground ,NonText))))
   `(whitespace-indentation ((t (:foreground ,NonText))))
   `(whitespace-line        ((t (:foreground ,NonText))))
   `(whitespace-tab         ((t (:foreground ,NonText))))
   `(whitespace-trailing    ((t (:foreground ,NonText))))


   ;; mode line
   `(mode-line           ((t (:foreground "#599cab"
   			      :background "#091f2e"
			      :font "DejaVu Sans Mono-10"
   			      ))))
   ;`(mode-line-inactive  ((t (:foreground "#F0F0EF"
   ;			      :background "#9B9C97"
   ;			      :box (:line-width 1
   ;				    :color "#4E4E4C")))))
   ;`(mode-line-buffer-id ((t (:foreground "white"
   ;			      :weight bold))))
   ;`(mode-line-emphasis  ((t (:foreground "white"
   ;			      :weight bold))))
   ;`(mode-line-highlight ((t (:foreground "yellow"))))

   `(my-evil-state-emacs-face  ((t (:background "#9B9C97"))))
   `(my-evil-state-normal-face ((t (:background "red"))))
   `(my-evil-state-insert-face ((t (:background "blue"))))

   ;; Escape and prompt faces.
   `(minibuffer-prompt            ((t (:foreground "black"
				       :background "gold"
				       :weight bold))))
   `(minibuffer-noticeable-prompt ((t (:foreground "black"
				       :background "gold"
				       :weight bold))))
   `(escape-glyph                 ((t (:foreground "#008ED1"))))
   `(error                        ((t (:foreground "red"))))
   `(warning                      ((t (:foreground "orange"
				       :weight bold))))
   `(success                      ((t (:foreground "green"))))

   ;; font lock
   `(font-lock-builtin-face              ((t (:foreground ,Boolean
					      :weight bold))))
   `(font-lock-comment-delimiter-face    ((t (:foreground ,Comment))))
   `(font-lock-comment-face              ((t (:foreground ,Comment
					      :slant italic))))
   `(font-lock-constant-face             ((t (:foreground ,Boolean))))
   `(font-lock-doc-face                  ((t (:foreground ,Comment))))
   `(font-lock-function-name-face        ((t (:foreground ,Function
					      :weight normal))))
   `(font-lock-keyword-face              ((t (:foreground ,Keyword
					      :weight bold))))
   `(font-lock-preprocessor-face         ((t (:foreground "#808080"))))
   `(font-lock-regexp-grouping-backslash ((t (:weight bold
					      :inherit nil))))
   `(font-lock-regexp-grouping-construct ((t (:weight bold
					      :inherit nil))))
   `(font-lock-string-face               ((t (:foreground ,String))))
   `(font-lock-type-face                 ((t (:foreground ,Type
					      :weight bold))))
   `(font-lock-variable-name-face        ((t (:foreground ,Boolean
					      :weight normal))))
   `(font-lock-warning-face              ((t (:foreground "red"
					      :weight bold))))


   )

  (provide-theme 'custom)

  );; end custom theme
