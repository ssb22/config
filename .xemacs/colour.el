;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.

;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.

;; Time-stamp: <2015-11-23>

;; ***********************************************
;; Turn on syntax highlighting:
;; ***********************************************
(setq-default font-lock-auto-fontify t)
(setq-default font-lock-use-fonts nil)
(setq-default font-lock-use-colors t)
(setq-default font-lock-maximum-decoration t)
(setq-default font-lock-maximum-size 256000)
(setq-default font-lock-mode-enable-list nil)
(setq-default font-lock-mode-disable-list nil)
(require 'font-lock)
(remove-hook 'font-lock-mode-hook 'turn-on-fast-lock)
(remove-hook 'font-lock-mode-hook 'turn-on-lazy-shot)

;; ***********************************************
;; Colour settings:
;; ***********************************************
(condition-case nil
    (progn
      (set-face-property 'default 'foreground "yellow")
      (set-face-property 'default 'background "black")
      )
  (error (progn
	   (set-face-foreground 'default "yellow")
	   (set-face-background 'default "black"))))
(condition-case nil (if (not (display-graphic-p)) (progn (set-face-bold-p 'default t) (menu-bar-mode -1))) (error nil)) ;; we want at least the yellow to be intense on text-only terminals; note "bold nil" exceptions below

(setq display-time-display-time-foreground "yellow")
(setq display-time-display-time-background "darkblue")
(custom-set-faces
 ;; Note: make sure there's a 't' on the end of these lines
 '(info-xref ((t (:underline t :foreground "green" :bold nil))) t)
 '(info-node ((t (:underline t :foreground "cyan"))) t)
 '(secondary-selection ((t (:foreground "black" :background "paleturquoise"))) t)
 '(forms-label-face ((((class color)) (:foreground "pink"))) t)
 '(gnus-header-subject-face ((((class color) (background dark)) (:foreground "cyan"))) t)
 '(gnus-header-from-face ((((class color) (background dark)) (:foreground "spring green"))) t)
 '(font-lock-string-face ((t (:foreground "yellow" :background "blue3"))) t)
 '(shell-option-face ((t (:foreground "red"))) t)
 '(message-headers ((t (:bold nil :foreground "purple"))) t)
 '(shell-prompt-face ((t (:foreground "cyan"))) t)
 '(gnus-header-newsgroups-face ((((class color) (background dark)) (:foreground "green" :bold nil))) t)
 '(message-header-subject-face ((((class color) (background dark)) (:foreground "cyan"))) t)
 '(font-lock-doc-string-face ((t (:foreground "blue"))) t)
 '(font-lock-preprocessor-face ((t (:foreground "cyan3" :background "black"))) t)
 '(font-lock-variable-name-face ((t (:foreground "cyan"))) t)
 '(paren-match ((t (:foreground "black" :background "darkseagreen2"))) t)
 '(shell-output-3-face ((t (:foreground "green" :bold nil))) t)
 '(gnus-header-content-face ((((class color) (background dark)) (:foreground "forest green"))) t)
 '(font-lock-keyword-face ((t (:foreground "white" :background "black"))) t)
 '(message-cited-text-face ((((class color) (background dark)) (:foreground "green" :bold nil))) t)
 '(shell-output-2-face ((t (:foreground "white"))) t)
 '(gnus-signature-face ((((type x)) (:foreground "orange"))) t)
 '(font-lock-type-face ((t (:foreground "purple"))) t)
 '(message-cited-text ((t (:italic t :foreground "green" :bold nil))) t)
 '(shell-output-face ((t nil)) t)
 '(primary-selection ((t (:background "darkblue"))) t) ; XEmacs
 '(region ((t (:background "darkblue"))) t) ; FSF Emacs
 '(message-header-cc-face ((((class color) (background dark)) (:foreground "green4"))) t)
 '(message-header-other-face ((((class color) (background dark)) (:foreground "red1"))) t)
 '(message-header-newsgroups-face ((((class color) (background dark)) (:foreground "yellow"))) t)
 '(message-separator-face ((((class color) (background dark)) (:foreground "blue"))) t)
 '(list-mode-item-selected ((t (:foreground "black" :background "gray68"))) t)
 '(message-header-contents ((t (:italic nil :foreground "cyan"))) t)
 '(font-lock-comment-face ((t (:foreground "red" :bold nil))) t)
 '(x-face ((t (:foreground "black" :background "grey"))) t)
 '(font-lock-function-name-face ((t (:foreground "green" :bold nil))) t)
 '(widget-button-face ((t (:bold t :foreground "green" :bold nil))) t)
 '(isearch ((t (:foreground "black" :background "paleturquoise"))) t)
 '(highlight ((t (:foreground "darkred" :background "darkseagreen2"))) t)
 '(modeline ((t (:foreground "Black" :background "Gray75"))) t)
 '(message-header-to-face ((((class color) (background dark)) (:foreground "green2"))) t)
 '(zmacs-region ((t (:background "darkblue"))) t)
 '(forms-field-face ((((class color)) (:foreground "cyan"))) t)
 '(message-highlighted-header-contents ((t (:foreground "green" :bold nil))) t))

;; ***********************************************
;; SGML/XML stuff (must be after colour settings)
;; ***********************************************
(add-hook
 'sgml-mode-hook
 '(lambda ()
    (if (and
         (or
          (string= mode-name "XML")
          (string= mode-name "SGML"))   ; Not HTML etc
         (not (string-match "\.html?$" buffer-file-name))
         )
        (progn
          (condition-case nil
              (sgml-parse-prolog)
            ;; Needed for set-face to work
            (error nil))
          ;; so it doesn't abort if it's HTML etc
          (setq sgml-set-face t)
          (setq sgml-markup-faces
                '((start-tag . font-lock-function-name-face)
                  (end-tag . font-lock-function-name-face)
                  (comment . font-lock-comment-face)
                  (pi . font-lock-preprocessor-face)
                  (sgml . font-lock-variable-name-face)
                  (doctype . font-lock-keyword-face)
                  (entity . font-lock-string-face)
                  (shortref . font-lock-reference-face)))
          ;; Fontifying is only done when the SGML
          ;; is actually parsed, so parse it:
          (save-excursion
            (condition-case nil
                (sgml-end-of-element)
              (error nil))
            )
          (clear-message)
          ))))

;; Some versions of XEmacs segfault when you go into XML
;; mode if the frame is split.  This is a work-around.
(if (and (string-match "XEmacs" emacs-version)
         (boundp 'emacs-major-version)
         (<= emacs-major-version 21)
         (boundp 'emacs-minor-version)
         (<= emacs-minor-version 1))
    (add-hook
     'sgml-mode-hook
     '(lambda ()
        (delete-other-windows)
        ))
  )

;; (XSL not done here: psgml doesn't yet work with XSL
;; because it doesn't support ':'s in tags - indentation
;; doesn't work etc - even if set it to HTML mode and (setq
;; sgml-default-doctype-name "html") to avoid it trying to
;; load a non-existent DTD for XSL)

;; (I think there is a mode for XSL files; you can install
;; it separately)

;; Go into XML mode when loading a .xml file
(setq auto-mode-alist (append '(
                                ("\\.xml\\'" . xml-mode)
                                ;; ("\\.xsl\\'" . xml-mode)
                                ) auto-mode-alist))

(condition-case nil
    (progn
      (add-to-list 'default-frame-alist '(cursor-color . "grey"))
      (set-cursor-color "grey")
      (blink-cursor-mode -1)) (error nil))
