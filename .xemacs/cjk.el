;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.


;; Time-stamp: <2013-03-04>

;; ***********************************************
;; Nice CJK fonts (and larger versions) :
;; ***********************************************

(defun nice-cjk-fonts ()
  "Set some nice fonts for CJK characters"
  (interactive)
  (set-face-font
   (make-face 'default)
   (append '(
     "-isas-song ti-medium-r-normal-*-*-240-*-*-c-*-gb2312.1980-0"
     "-*-ming-medium-r-normal-*-*-240-*-*-c-*-big5-0" ;; bitmap
     "-arphic-ar pl mingti2l big5-medium-r-normal-*-*-240-*-*-c-*-big5-0" ;; nicer but slow loading
     "-eten-fixed-medium-r-normal-*-*-230-*-*-c-*-big5-0"
     "-jis-fixed-*-r-*--24-*-*-*-*-*-jisx0208.1983-*"
     "-baekmuk-gulim-medium-r-normal-*-*-240-*-*-m-*-ksc5601.1987-0") (face-font 'default 'global))))

(condition-case nil
(nice-cjk-fonts)
(error nil)) ;; (goes wrong in FSF Emacs)

(defun nice-large-cjk-fonts ()
  "Set some nice fonts for CJK characters (larger versions)"
  ;; (don't have a scalable ko font - stuck at 24pt)
  ;; (don't have scalable big5 - doing scaled-bitmap)
  ;; $A5DH7(B simp
  ;; $(00|O}(B trad
  ;; $BF|K\(B jp
  ;; $(CGQ1[(B ko
  (interactive)
  (set-face-font
   (make-face 'default)
   (append '(
     "-cc-song-medium-r-normal-*-*-480-*-*-c-*-gb2312.1980-0"
     "-arphic-ar pl mingti2l big5-medium-r-normal-*-*-480-*-*-c-*-big5-0" ;; nice but slow loading
     "-*-ming-medium-r-normal-*-*-480-*-*-c-*-big5-0" ;; scaled bitmap
     "-eten-fixed-medium-r-normal-*-*-230-*-*-c-*-big5-0" ;; not big
     "-watanabe-fixed-*-r-*--48-*-*-*-*-*-jisx0208.1983-*"
     "-baekmuk-gulim-medium-r-normal-*-*-240-*-*-m-*-ksc5601.1987-0") (face-font 'default 'global))))

(condition-case nil
(set-face-font (make-face 'bold) (make-font-instance
                                  "-watanabe-*-*-r-*--24-*-*-*-*-*-jisx0208.1983-*"))
(error nil))
;; (this bold font is not really satisfactory - it's a
;; scaled bitmap.  At least it's better than the default
;; for viewing Japanese subject lines in a VM summary
;; buffer where the message is selected.)
;; (condition cased so still works in tty)

;; ***********************************************
;; Japanese stuff:
;; ***********************************************
;; Use SKK (but this might need setting up -
;; I've installed a local server and put a
;; /etc/emacs/site-start.d/50skk2.el with the
;; necessary variables)
(custom-set-variables
 '(default-input-method (quote japanese-skk))
 )

;; Use EDICT dictionary
;; The following is correct if you install the Debian edict package
(setq edict-dictionaries
      '("/usr/share/edict/edict"))
;; so you can type "M-x edict-search-english" by a word

;; Use English version of SKK Jp input method tutorial:
(setq skk-tut-file "/usr/share/skk/SKK.tut.E")

;; Functions to display an XEmacs CJK buffer properly:

(defun my-decode-buffer (CODING-SYSTEM)
  (let ((old-read-only buffer-read-only)
        (old-modified (buffer-modified-p)))
    (setq buffer-read-only nil)
    (decode-coding-region (point-min) (point-max) CODING-SYSTEM)
    (set-buffer-file-coding-system CODING-SYSTEM)
    (setq buffer-read-only old-read-only)
    (if old-modified t (not-modified))
    )
  )

(defun decode-buffer-autodetect ()
  (interactive)
  (my-decode-buffer (coding-system-name (car (detect-coding-region (point-min) (point-max)))))
  )

(defun decode-buffer-euc-jp ()
  (interactive)
  (my-decode-buffer 'euc-jp)
  )

(defun decode-buffer-iso-2022-jp ()
  (interactive)
  (my-decode-buffer 'iso-2022-jp)
  )

(defun decode-region-iso-2022-jp ()
  ;; (useful for copy/paste from "less Copyself")
  (interactive)
  (decode-coding-region (region-beginning) (region-end) 'iso-2022-jp)
  )

;; (make-coding-system 'sjis 'shift-jis)
(defun decode-buffer-sjis ()
  (interactive)
  (my-decode-buffer 'shift_jis)
  )

;; ***********************************************
;; Chinese stuff:
;; ***********************************************

(condition-case nil
  (require 'un-define) ; Mule UCS package, for utf-8 etc
  (error nil))

(defun decode-buffer-big5 ()
  (interactive) (my-decode-buffer 'big5))

(defun decode-buffer-gb2312 ()
  (interactive) (my-decode-buffer 'gb2312))

(defun decode-buffer-utf8 ()
  (interactive)
  (my-decode-buffer 'utf-8)
  ;; (Note that UTF-encoded Chinese might use a Japanese mule charset for most of the characters)
  )

(defun decode-region-gb2312 ()
  (interactive)
  (decode-coding-region (region-beginning) (region-end) 'gb2312)
  )

(defun decode-region-big5 ()
  (interactive)
  (decode-coding-region (region-beginning) (region-end) 'big5)
  )

(defun decode-region-utf8 ()
  (interactive)
  (decode-coding-region (region-beginning) (region-end) 'utf-8)
  )
