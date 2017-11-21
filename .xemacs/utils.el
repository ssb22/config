;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.


;; utils.el - service routines needed by other things

;; Time-stamp: <2004-01-08>

(defun my-replace-regexp (REGEXP TO-STRING)
  (save-excursion
    (while (re-search-forward REGEXP nil t)
      (replace-match TO-STRING nil nil))
    )
  ;; Return whether or not there are any more
  (save-excursion (re-search-forward REGEXP nil t))
  )

(defun my-replace-regexp-region (REGEXP TO-STRING)
  (save-excursion
    (goto-char (min (region-beginning) (region-end)))
    (while (re-search-forward REGEXP (max (region-beginning) (region-end)) t)
      (replace-match TO-STRING nil nil))
    )
  )

(setq running-on-console
      (or (string= (getenv "DISPLAY") ":0.0")
          (string= (getenv "DISPLAY") ":0")
          (string= (getenv "DISPLAY") "localhost:2.0")
          ))
(setq running-on-console t) ;; temporarily while gnuserv not working
