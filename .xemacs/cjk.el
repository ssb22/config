;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.


;; Time-stamp: <2020-07-07>

;; ***********************************************
;; XEmacs nice CJK fonts (and larger versions) :
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
  ;; 的确 simp
  ;; 的確 trad
  ;; 日本 jp
  ;; 한글 ko
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
;; Input methods:
;; ***********************************************

(condition-case nil (require 'pyim) (error nil))
(custom-set-variables
 '(default-input-method
;; If SKK has been set up, assume we want to use it.
;; This may need installing a local server and adding
;; /etc/emacs/site-start.d/50skk2.el or similar with
;; the necessary variables.
    (if (condition-case nil (assoc "japanese-skk" input-method-alist) (error nil))
        (quote japanese-skk)
      ;; pyim might have been installed as a package:
      (if (condition-case nil (assoc "pyim" input-method-alist) (error nil))
          (quote pyim)
        ;; many systems have quail chinese-py:
        (if (condition-case nil (assoc "chinese-py" input-method-alist) (error nil))
            (quote chinese-py)
          ;; fall back to no input method:
          nil
        )))))

;; If using SKK, use English version of tutorial:
(setq skk-tut-file "/usr/share/skk/SKK.tut.E")

;; Use EDICT dictionary, if edict package is installed,
;; so you can type "M-x edict-search-english" by a word
;; The following is correct if you install the Debian edict package
(setq edict-dictionaries
      '("/usr/share/edict/edict"))

;; Search CEDICT (or a derivative), and can use it as a
;; rudimentary 'input method' (search by English etc) if
;; all else fails.  For maximum backward-compatibility,
;; this code uses the old-style GB2312 / Big5 cedict files
;; but can also be set to use the modern UTF-8 file.
(defun cedict-search-setup-big5 ()
  (interactive)
  (setq cedict-dictionary "~/.xemacs/cedict.b5")
  (setq cedict-search-omit-first-word nil))
(defun cedict-search-setup-gb ()
  (interactive)
  (setq cedict-dictionary "~/.xemacs/cedict.GB")
  (setq cedict-search-omit-first-word nil))
(defun cedict-search-setup-utf8-trad ()
  (interactive)
  (setq cedict-dictionary "~/.xemacs/cedict.u8")
  (setq cedict-search-omit-first-word nil))
(defun cedict-search-setup-utf8-simp ()
  (interactive)
  (setq cedict-dictionary "~/.xemacs/cedict.u8")
  (setq cedict-search-omit-first-word t))
(cedict-search-setup-utf8-simp) ;; by default
(defun cedict-search ()
  (interactive)
  (find-file cedict-dictionary)
  (deactivate-mark)
  (goto-char (point-min))
  (isearch-forward)
  (local-set-key [return] 'cedict-search-finished)
  )
(defun cedict-search-finished ()
  (interactive)
  (isearch-exit)
  (beginning-of-line)
  (if cedict-search-omit-first-word
      (search-forward " "))
  (set-mark-command nil)
  (if cedict-search-take-whole-entry
      (next-line 1)
    (search-forward " ") (backward-char))
  (copy-region-as-kill (region-beginning) (region-end))
  (switch-to-buffer nil)
  (yank)
  (local-set-key [return] 'newline-and-indent) ;; TODO: fix it to be buffer-local!  "local-set-key" affects ALL buffers with that major mode
  )
(setq cedict-search-take-whole-entry nil)
(defun cedict-search-setup-whole-entry ()
  (interactive)
  (setq cedict-search-take-whole-entry t))
(defun cedict-search-setup-chars-only ()
  (interactive)
  (setq cedict-search-take-whole-entry nil))
(global-set-key [(control ?=)] 'cedict-search)

;; ***********************************************
;; Functions to display XEmacs CJK buffer properly:
;; ***********************************************

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
