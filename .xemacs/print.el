;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.


;; Time-stamp: <2002-08-28>

;; ***********************************************
;; Printing stuff:
;; ***********************************************
(setq-default lpr-switches nil)
(setq-default ps-print-color-p t)
(setq-default ps-paper-type 'a4)
(custom-set-variables
 '(ps-header-font-size 20)
 '(ps-print-color-p nil)
 '(ps-header-title-font-size 24)
 '(ps-font-size 16)
 )

(defun make-region-printable ()
  (interactive)
  (setq fill-column 50)
  (turn-on-filladapt-mode)
  (fill-region (region-beginning) (region-end))
  )

(condition-case nil (progn
(add-menu-button '("File") "Remember M-x " "Pretty-Print")
(add-menu-button '("File") "make-region-printable" "Pretty-Print")
) (error nil))
