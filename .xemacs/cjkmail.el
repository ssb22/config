;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.


;; cjkmail.el - East Asian languages & letters / email

;; Stuff for inserting Japanese subject lines in emails
(defun japanese-subject-region ()
  (interactive)
  (vm-encode-coding-region (region-beginning) (region-end) 'iso-2022-jp)
  (vm-mime-base64-encode-region (region-beginning) (region-end))
  (let (x)
    (setq x (region-beginning))
    (goto-char (region-end))
    (insert "?=")
    (goto-char x)
    (insert "=?ISO-2022-JP?B?")
    )
  )

(defun chinese-gb-subject-region ()
  (interactive)
  (vm-encode-coding-region (region-beginning) (region-end) 'gb2312)
  (vm-mime-base64-encode-region (region-beginning) (region-end))
  (let (x)
    (setq x (region-beginning))
    (goto-char (region-end))
    (insert "?=")
    (goto-char x)
    (insert "=?gb2312?B?")
    )
  )

(defun chinese-big5-subject-region ()
  (interactive)
  (vm-encode-coding-region (region-beginning) (region-end) 'big5)
  (vm-mime-base64-encode-region (region-beginning) (region-end))
  (let (x)
    (setq x (region-beginning))
    (goto-char (region-end))
    (insert "?=")
    (goto-char x)
    (insert "=?big5?B?")
    )
  )

;; (Note: The following assumes that ANY non-ASCII text in
;; the subject line is ISO-2022-JP.  Needs fixing!)
(defun catch-japanese-subject ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Subject:.*[^\n-~]" nil t)
        (progn
          (beginning-of-line nil)
          (re-search-forward "[^\n-~].*$" nil t)
          (set-mark (match-beginning 0))
          (goto-char (match-end 0))
          (japanese-subject-region)
          ))
    ))
(add-hook 'mail-send-hook 'catch-japanese-subject)

;; Get Chinese to work properly in VM - some versions don't
;; have it set properly in the variables like
;; vm-mime-mule-charset-to-coding-alist
(condition-case nil (progn
(require 'vm)
(remassoc 'chinese-gb2312 vm-mime-mule-charset-to-charset-alist)
(add-to-list 'vm-mime-mule-charset-to-charset-alist '(chinese-gb2312 "gb2312"));; for encoding messages (please don't say they're iso-2022-jp)
(add-to-list 'vm-mime-mule-coding-to-charset-alist '(chinese-gb2312 "gb2312"));; for encoding messages (please don't say they're iso-2022-jp)

(remassoc 'korean-ksc5601 vm-mime-mule-charset-to-charset-alist)
(add-to-list 'vm-mime-mule-charset-to-charset-alist '(korean-ksc5601 "euc-kr"));; similar (please say euc-kr not iso-2022-kr)
(add-to-list 'vm-mime-mule-coding-to-charset-alist '(korean-ksc5601 "euc-kr"))

(add-to-list 'vm-mime-mule-charset-to-charset-alist '(chinese-big5-1 "big5"));; ditto
(add-to-list 'vm-mime-mule-coding-to-charset-alist '(chinese-big5-1 "big5"))
(add-to-list 'vm-mime-mule-charset-to-charset-alist '(chinese-big5-2 "big5"));; ditto (although this one probably won't be used)
(add-to-list 'vm-mime-mule-coding-to-charset-alist '(chinese-big5-2 "big5"))
(add-to-list 'vm-mime-mule-charset-to-charset-alist '(big5-unix "big5"));; ditto
(add-to-list 'vm-mime-mule-coding-to-charset-alist '(big5-unix "big5"))
(add-to-list 'vm-mime-mule-charset-to-charset-alist '(cn-big5 "big5"));; ditto (needed for some versions?)
(add-to-list 'vm-mime-mule-coding-to-charset-alist '(cn-big5 "big5"))
(add-to-list 'vm-mime-mule-charset-to-charset-alist '(cn-gb-2312 "gb2312"));; just in case it's needed
(add-to-list 'vm-mime-mule-coding-to-charset-alist '(cn-gb-2312 "gb2312"))
;; If need to do more of this, see mm-mime-mule-charset-alist (some of it should probably be in VM e.g. Thai stuff).  Note the symbols in the stuff above are not necessarily the same as those in the stuff below!
(add-to-list 'vm-mime-mule-charset-to-coding-alist '("gb2312" gb2312));; for decoding messages
;; (big5 should already be there)

;; ---------------------------------------------------

;; An attempt to help with UTF-8 stuff on various platforms:

(if (find-coding-system 'unicode-utf8)
    (add-to-list 'vm-mime-mule-charset-to-coding-alist '("utf-8" unicode-utf8))
  (add-to-list 'vm-mime-mule-charset-to-coding-alist '("utf-8" utf-8))
  ) ;; for viewing utf-8 messages

;; (careful about copying from it to a GB/Big5 document though (**** sort this out?))

;; Stuff for sending in UTF-8:

(add-to-list 'vm-mime-mule-charset-to-charset-alist '(unicode-utf8 "utf-8")) ;; for sending in UTF-8
(add-to-list 'vm-mime-mule-coding-to-charset-alist '(unicode-utf8 "utf-8"))
(add-to-list 'vm-mime-mule-charset-to-charset-alist '(utf-8 "utf-8")) ;; for sending in UTF-8
(add-to-list 'vm-mime-mule-coding-to-charset-alist '(utf-8 "utf-8"))

(add-to-list 'vm-mime-mule-charset-to-charset-alist '(keyboard "utf-8"))
(add-to-list 'vm-mime-mule-charset-to-charset-alist '(utf-8-ws-unix "utf-8"))
(add-to-list 'vm-mime-mule-charset-to-charset-alist '(utf-8-ws "utf-8"))

)(error nil)) ;; (VM may not be there on some Emacs installations)

(defun set-buffer-to-utf8 () (condition-case nil (set-buffer-file-coding-system 'utf-8) (error nil)))
(add-hook 'mail-mode-hook 'set-buffer-to-utf8)
