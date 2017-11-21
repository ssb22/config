;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.


;; Time-stamp: <2007-02-06>
;; (other files might have a later time stamp)

(defun myload (file)
  (condition-case err (load file)
    (error (warn "Warning: file %s: %s" file (cdr err)))))

(myload "~/.xemacs/utils.el")
(myload "~/.xemacs/sys.el")

(myload "~/.xemacs/colour.el") ;; must be before 'large'
(myload "~/.xemacs/large.el")
(myload "~/.xemacs/cjk.el") ;; after 'large'

(myload "~/.xemacs/speech.el")

(myload "~/.xemacs/email.el")
(myload "~/.xemacs/cjkmail.el")
(myload "~/.xemacs/print.el")
(myload "~/.xemacs/misc.el")
(myload "~/.xemacs/desktop.el")

;; ***********************************************
;; Clock and system load / mail indicator:
;; (must be after colour settings)
;; ***********************************************
(condition-case nil (display-time) (error nil))
(setq display-time-compatible t)
(setq display-time-string-forms
  '((format "%s: %s" 12-hours minutes)))
;; Uncomment this if you want a load indicator and/or mail
;; indicator.  This uses glyphs and may not be large enough
;; if you're in high resolution.
;; (setq display-time-compatible f)
;; (setq display-time-no-mail-sign nil)
;; Uncomment the next line if you don't want to be told about mail at all
;; (setq display-time-mail-sign nil)
(condition-case nil
(set-face-font my-global-modeline-face (face-font (make-face 'modeline))) ;; seems to be needed here as well, just in case
(error nil)) ;; (may not work on FSF Emacs)
(condition-case nil (display-time) (error nil))

;; *********************************************
;; Terminal coding system
;; (for gnuclient within putty; for some reason doesn't
;; work when put it in cjk.el so we're trying it here
;; at the end of everything)
;; *********************************************

(set-terminal-coding-system 'utf-8)
;; "currently broken"?
;; putty can accept UTF-8

;; STILL doesn't work on startup - need to
;; interactively do it from the gnuclient session
