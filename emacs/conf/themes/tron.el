
;;; Code
(deftheme tron
  "Based on tron legacy")

(custom-theme-set-faces
 `tron
 `(default ((t (:background "#000000" :foreground "#6FC3DF" ))))
 `(bold ((t (:bold t))))
 `(bold-italic ((t (:bold t))))
 `(border-glyph ((t (nil))))
 `(fringe ((t (:background "#ABD6FE"))))
 `(mode-line ((t (:foreground "#000000" :background "#ABD6FE"))))
 `(mode-line-inactive ((t (:foreground "#7E8080" :background "#ABD6FE"))))
 `(region ((t (:background "#356a9c"))))
 `(font-lock-builtin-face ((t (:foreground "#6FC3DF" :bold t))))
 `(font-lock-comment-face ((t (:foreground "#575b5b"))))
 `(font-lock-function-name-face ((t (:foreground "#6FC3DF" :bold t))))
 `(font-lock-keyword-face ((t (:foreground "#ABD6FE" :bold t))))
 `(font-lock-string-face ((t (:foreground "#356A9C"))))
 `(font-lock-type-face ((t (:foreground"#6FC3DF" :bold t))))
 `(font-lock-constant-face ((t (:foreground "#ABD6FE" :bold t))))
 `(font-lock-variable-name-face ((t (:foreground "#6FC3DF"))))
 `(minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
 `(font-lock-warning-face ((t (:foreground "#DF740C" :bold t))))
 `(font-lock-doc-face ((t (:foreground "#356A9C"))))
 `(font-lock-preprocessor-face ((t (:foreground "#ABD6FE" :bold t))))
)

(set-mouse-color "#6FC3DF")

(set-face-foreground 'highlight "#000000")
(set-face-background 'highlight "#6FC3DF")
(set-face-foreground 'region "#000000")
(set-face-background 'region "#6FC3DF")

(provide-theme 'tron)

;;eof
