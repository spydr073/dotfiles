
(use-package unicode-fonts
  :ensure t
  :init (unicode-fonts-setup))

;; evil state
(defface emacs-state
  '((t :foreground "Black"
       :background "#b19050"
       :weight bold))
  "Emacs Mode")

(defface normal-state
  '((t :foreground "Black"
       :background "#7da3a1"
       :weight bold))
  "Normal Mode")

(defface insert-state
  '((t :foreground "Black"
       :background "#ea9b67"
       :weight bold))
  "Insert Mode")

(defface visual-state
  '((t :foreground "Black"
       :background "#c7dc70"
       :weight bold))
  "Visual Mode")

;;-- Render modeline as left and right segments
(defun simple-mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left))))
    (format (format "%%s %%%ds" available-width) left right)))

;;-- Colorize evil mode tag
(defun evil-generate-mode-line-tag (&optional state)
  (let ((tag (evil-state-property state :tag t)))
    (if (stringp tag)
        (propertize (concat " " tag "  ")
                    'face (cond
                           ((string= "normal" state)
                            'normal-state)
                           ((string= "insert" state)
                            'insert-state)
                           ((string= "visual" state)
                            'visual-state)
                           ((string= "emacs" state)
                            'emacs-state))
                    'help-echo (evil-state-property state :name)
                    'mouse-face 'mode-line-highlight)
      tag)))



;; Define faces.

(defface my/mode:vc-added
  `(
     (  ((class color))
        (:background "#FFAA55"  :foreground "black")  )
     (  t
        (:weight bold :underline t)  )
   )
  "VC status tag face for files that have just been added to
version-control."
  :group 'MY/mode)

(defface my/mode:vc-edited
  `(
     (  ((class color))
        (:background "#F05B80"  :foreground "black")  )   ; "#F04040" maybe?
     (  t
        (:weight bold :underline t)  )
   )
  "VC status tag face for files that are under version control
but which have been edited."
  :group 'MY/mode)

(defface my/mode:vc-in-sync
  `(
     (  ((class color))
        (:background "#60CC60"  :foreground "black")  )
     (  t
        (:weight bold :underline t)  )
   )
  "VC status tag face for files that are under version control
and which are in sync with the respository."
  :group 'MY/mode)

(defface my/mode:vc-none
  `(
     (  ((class color))
        (:background "#70A0D0"  :foreground "black")  )
     (  t
        (:weight bold :underline t)  )
   )
  "VC status tag face for files that are not under version
control"
  :group 'MY/mode)

(defface my/mode:vc-unknown
  `(
     (  ((class color))
        (:background "#FF0000"  :foreground "white")  )
     (  t
        (:weight bold :underline t)  )
   )
  "VC status tag face for files whose version-control status
cannot be determined."
  :group 'MY/mode)

(defvar my-vc-mode-attrs
  '((""  . (" NoVC "  my/mode:vc-none))
    ("-" . (" VC = "  my/mode:vc-in-sync))
    (":" . (" VC > "  my/mode:vc-edited))
    ("@" . (" VC + "  my/mode:vc-added))
    ("?" . (" ?VC? "  my/mode:vc-unknown))
    )
  "Lookup table to translate vc-mode character into another string/face."
)


;; This function helps me understand the version-control status.
(defun my-mode-line-vc-info ()
  "Return version-control status information about the file in
the current buffer, as a fontified string.

The mode-line variable `vc-mode' is nil if the file is not under
version control, and displays a hyphen or a colon depending on whether
the file has been modified since check-in.  I can never keep those
straight.

This function returns \"NoVC\" if the file is not under version
control.  It displays a string with an = sign if the file is in sync
with its version control, and a string with a > sign if the file has
been modified since its last check-in."
  (let* ((class
          (cond
           ;; If not under version-control
           ((not vc-mode)
            "")

           ;; If under version-control decode the -:@ character
           ((string-match "\\` ?\\(?:CVS\\|Git\\)\\([-:@]\\)\\([^^:~ \x00-\x1F\\\\/]+\\)?" vc-mode)
            (match-string-no-properties 1 vc-mode))

           ;; Otherwise, indicate confusion
           (t
            "?")
           ))

         (branch
          (if (any class '("-" ":" "@"))
              (concat " " (match-string-no-properties 2 vc-mode))
            ""))

         ;; Fetch properties list for the class character above
         (props (cdr (assoc class my-vc-mode-attrs)))
         )

    (concat (propertize (car props) 'face (cadr props))
            branch)))



(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render

     ;; left
     (format-mode-line
      (quote
       (""
	evil-mode-line-tag
	(vc-mode vc-mode)
	" ❱ "
        "%b"
	"%*"
	" ❱ "
	"❲%m❳"
	" ❱ "
	)))

     ;; right
     (format-mode-line
      (quote
       ("❲%l:%c❳"
	" ❮ "
	"❲%p:%I❳ "
	)))))))


(provide 'modeline-config)
