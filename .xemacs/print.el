;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.


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
  (let ((inhibit-message t)) (package-install 'filladapt))
  (require 'filladapt)
  (turn-on-filladapt-mode)
  (fill-region (region-beginning) (region-end))
  )
(defun print-region-to-pdf ()
  ;; Does not require lpr setup.  This one uses 20 (could change to 16, 18 etc as appropriate); long lines wrap w/out word break
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "enscript -p - -f Courier@20 -B -b --margins=72:72:72:72 /dev/stdin | ps2pdf - /tmp/output.pdf")
  (message "PDF written to /tmp/output.pdf"))
(defun print-buffer-to-pdf ()
  (interactive)
  (shell-command-on-region (buffer-end 0) (buffer-end 1) "enscript -p - -f Courier@20 -B -b --margins=72:72:72:72 /dev/stdin | ps2pdf - /tmp/output.pdf")
  (message "PDF written to /tmp/output.pdf"))

(condition-case nil (progn
(add-menu-button '("File") "Remember M-x " "Pretty-Print")
(add-menu-button '("File") "make-region-printable" "Pretty-Print")
) (error nil))
