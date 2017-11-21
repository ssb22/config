;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.


;; Time-stamp: <2004-11-04>

;; ***********************************************
;; System-specific stuff (don't copy it):
;; ***********************************************
(custom-set-variables
 '(traceroute-program "/usr/sbin/traceroute")
 '(nnmail-spool-file "/var/spool/mail/$user")
 )
(setq load-path (cons "/usr/share/xemacs20/site-lisp/bbdb/" load-path))
(condition-case nil (load "~/.xemacs/cedict.el") (error ""))

(custom-set-variables
 '(message-required-mail-headers (quote (From Subject Date Message-ID Lines)))
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 )

(setq-default ispell-program-name "ispell")
