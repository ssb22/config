;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.

;; email.el - mail & news, including workarounds, fill, &c

;; ***********************************************
;; Composition stuff:
;; ***********************************************

;; Load abbrev table on startup
;; (mail mode seems to go into abbrev anyway)
;; (To define new abbrev, type text & do C-x a g)
;; (also list-abbrevs or edit-abbrevs)
(condition-case nil (read-abbrev-file) (error nil))
(setq abbrev-table-name-list '(global-abbrev-table))
;; global only, so edit-abbrevs gets straight to the content

;; This makes the "--text follows this line--" in mail
;; composition buffers read-only, to reduce accidents
;; (while we're at it, highlight it in a different colour;
;; useful if first paragraph of message says "Dear ...")
(defun protect-mail-separator ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^--text follows this line--$" nil t)
    (let ((protectExtent (make-extent (+ 1 (match-beginning 0)) (match-end 0)))
          (highlightExtent (make-extent (match-beginning 0) (match-end 0)))
          )
      (set-extent-property protectExtent 'read-only t)
      (set-extent-face highlightExtent 'font-lock-keyword-face)
      )))
;; (defun protect-mail-separator () nil)
(defun unprotect-mail-separator ()
  (interactive)
  (let ((temp (extent-list)))
    (while (consp temp)
      (progn
        (set-extent-property (car temp) 'read-only nil)
        (setq temp (cdr temp))))))
;; (add-hook 'mail-setup-hook 'protect-mail-separator)
;; above doesn't work with resend or retry bounce
;; mail-mode-hook doesn't work (args out of range)
(if (fboundp 'extent-list)
    (progn
      (add-hook 'vm-mail-mode-hook 'protect-mail-separator)
      (add-hook 'mail-send-hook 'unprotect-mail-separator)
      (add-hook 'vm-mail-send-hook 'unprotect-mail-separator)
      (add-hook 'vm-resend-message-hook 'unprotect-mail-separator)
      ))

;; ***********************************************
;; Gnus stuff:
;; ***********************************************
(custom-set-variables
 '(gnus-inhibit-startup-message t)
 '(gnus-large-newsgroup 2000)
 '(gnus-group-mode-line-format "Gnus: %%b")
 '(gnus-treat-fill-article t);; fill the article
 '(gnus-treat-fill-long-lines t);; fill long lines
 )

;; ***********************************************
;; BBDB stuff:
;; ***********************************************
(condition-case nil
(if (< (frame-height) 20)
    ;; too small for bbdb
    (require 'vm)
  (progn
    (require 'bbdb)
    (bbdb-initialize)
    (bbdb-insinuate-vm)
    (setq bbdb-quiet-about-name-mismatches t)))
(error nil)) ;; may go wrong on FSF Emacs

;; ***********************************************
;; VM stuff:
;; ***********************************************

(setq vm-folder-directory "~/.vm")      ; Don't clutter the home directory
(setq vm-use-toolbar nil)               ; More screen space
(setq vm-tale-is-an-idiot t)            ; Just in case
(setq vm-auto-decode-mime-messages t)   ; Of course
(setq vm-auto-get-new-mail t)           ; seconds or t (may or may not interrupt edit)
;; NB have t, not seconds, otherwise kbiff etc won't work
(setq vm-mail-check-interval nil)       ; (NOT the same as above)
(setq vm-keep-sent-messages nil)        ; Tidy up the buffers
(setq vm-auto-folder-case-fold-search t) ; not case sensitive
(setq vm-visit-when-saving 0)           ; Keep buffers tidy
(setq vm-delete-after-saving t)
(setq vm-preview-lines nil)             ; Go straight into the message
(setq vm-delete-empty-folders t)
(setq vm-move-after-deleting t)
(setq vm-move-after-killing t)
(setq vm-move-after-undeleting t)
(setq vm-inhibit-startup-message t)
(setq vm-reply-subject-prefix "Re: ")
(setq vm-included-text-prefix "> ")
(setq vm-in-reply-to-format nil)        ; No "In-reply-to" lines
(setq vm-group-by "subject")            ; or "author" ?
(setq sc-nested-citation-p t)
;; (autoload 'sc-cite-original     "supercite" "Supercite 3.1" t)
;; (autoload 'sc-submit-bug-report "supercite" "Supercite 3.1" t)
;; mail-yank-hooks add sc-cite-original

;; Different summary format:
(setq vm-summary-format "%*%a %-17.17F %-25.25s %4l/%-5c %-3.3m %2d\n")

(setq vm-flush-interval nil) ;; don't worry about
			     ;; auto-saving message attributes (was 90)
;; (emacs autosave would take a while to get there anyway
;; with large buffers)

;; VM please don't quote computer games on startup:
(defun erase-vm-startup ()
  (setq vm-startup-message-displayed t)
  (setq vm-startup-message-lines '(""))
  )
(add-hook 'vm-mode-hook 'erase-vm-startup)
(add-hook 'vm-summary-mode-hook 'erase-vm-startup)
(add-hook 'vm-presentation-mode-hook 'erase-vm-startup)
(erase-vm-startup)

;; Put the signature on
;; (add-hook 'mail-mode-hook 'mail-signature) ;; No - it interferes with send-pr
(setq-default mail-signature t)

;; Write a copy to Copyself
(setq mail-archive-file-name "~/.vm/Copyself")
;; (add-hook 'mail-send-hook '(lambda () (write-region (point-min) (point-max) "~/Copyself" t)))

;; Filling stuff (some people send messages with rather
;; long lines)
(defun fill-quoted-paragraph ()
  (interactive)
  (setq old-fill-prefix fill-prefix)
  (setq fill-prefix vm-included-text-prefix)
  (call-interactively 'fill-paragraph)
  (setq fill-prefix old-fill-prefix)
  )

;; Actually should use mail-archive-file-name
(add-hook 'vm-arrived-messages-hook 'vm-auto-archive-messages)

;; Don't do W3 stuff for HTML (just use lynx -dump)
(setq vm-mime-internal-content-type-exceptions '("text/html"))
(condition-case nil
(add-to-list 'vm-mime-type-converter-alist
             '("text/html" "text/plain" "lynx -force_html -dump -stdin"))
(error nil)) ;; (nothing if vm not present)
;; (setq vm-mime-internal-content-type-exceptions nil)

;; Sort the fonts out
(add-hook 'vm-mode-hook 'fix-fonts)
(add-hook 'vm-summary-mode-hook 'fix-fonts)
(add-hook 'vm-presentation-mode-hook 'fix-fonts)
;; 2000-08-29: Need to put it in vm-presentation-mode-hook
;; because that gets run on startup (vm-select-message-hook
;; might be worth doing as well)
;; 2000-09-13: It doesn't seem to work (if there are any
;; strange fonts in the initial message that is displayed, a
;; manual fix-fonts is necessary) NEEDATTENTION
(add-hook 'vm-select-message-hook 'fix-fonts)
;; Don't worry about unrecognised character sets (Windows-1251/1252/1257; gb2312 where the text is actually English; etc)
;; (setq vm-mime-default-face-charsets t)
;; 2001-09-08: Actually the above doesn't work - some
;; ISO-2022 messages don't get displayed correctly.  For
;; getting gb2312 to display properly, see under "Chinese
;; stuff".
;; 2001-10-11: Putting "utf-8" in this list until we get VM
;; to handle it properly (removed 2002-10-03 - see
;; cjkmail.el)
;; 2001-11-30: Temporarily putting "windows-874" for Panit's messages (really need to sort out Thai stuff though)
;; 2002-11-01: Got windows-1250 message so added that
(setq vm-mime-default-face-charsets '("us-ascii" "iso-8859-1" "windows-1252" "windows-1251" "windows-1257" "windows-874" "windows-1250"))

;; If someone sends an attachment of type
;; application/octet-stream, try to work out what it is
;; from the filename:
(setq vm-infer-mime-types t)
;; MIME-encode lines beginning "From " before sending
;; (this avoids their having a ">" prefixed, which can
;; break some syntax highlighting systems that treat it
;; as quoted)
(setq vm-mime-composition-armor-from-lines t)
;; If you need to support all the Latin charsets, you can uncomment and modify this:
;; (need to set vm-mime-default-face-charset-exceptions as well)
;;(setq vm-mime-charset-font-alist
;;'(
;;("iso-8859-9" . "-*-*-*-r-normal-*-20-*-*-*-*-*-iso8859-9")
;;("iso-8859-8" . "-*-*-*-r-normal-*-20-*-*-*-*-*-iso8859-8")
;;("iso-8859-7" . "-*-*-*-r-normal-*-20-*-*-*-*-*-iso8859-7")
;;("iso-8859-6" . "-*-*-*-r-normal-*-20-*-*-*-*-*-iso8859-6")
;;("iso-8859-5" . "-*-*-*-r-normal-*-20-*-*-*-*-*-iso8859-5")
;;("iso-8859-4" . "-*-*-*-r-normal-*-20-*-*-*-*-*-iso8859-4")
;;("iso-8859-3" . "-*-*-*-r-normal-*-20-*-*-*-*-*-iso8859-3")
;;("iso-8859-2" . "-*-*-*-r-normal-*-20-*-*-*-*-*-iso8859-2")
;;))

(defun html-message ()
  "Set a VM compose buffer to HTML"
  (interactive)
  (save-excursion
    (unprotect-mail-separator)
    (vm-mime-encode-composition)
    (goto-char (point-min))
    (if (re-search-forward "^Content-Type: .*$" nil t)
        (replace-match "Content-Type: text/html" nil nil)
      )
    )
  )

;; VM 6.89 requires you to press '#' three times to expunge
;; messages (rather than 6.72's once).  The following gets
;; the old behaviour back.
(add-hook 'vm-mode-hook
          '(lambda ()
             (progn
               (local-unset-key ?#)
               (local-set-key ?# 'vm-expunge-folder))))

;; XEmacs 21.4.1 has removed support of a couple of obsolete
;; functions that VM/BBDB use (so VM doesn't work).  We'll
;; include them here.
;; (taken from /usr/share/xemacs-21.1.13/lisp/obsolete.el
;; on the lab's machine)

(defun set-extent-data (extent data)
  "Obsolete.  Set the `data' property of EXTENT."
  (set-extent-property extent 'data data))
;;(make-obsolete 'set-extent-data 'set-extent-property) ;; warns on startup in Emacs 28.2
(defun set-extent-attribute (extent attr &optional clearp)
  (cond ((eq attr 'write-protected)
         (set-extent-property extent 'read-only t))
        ((eq attr 'unhighlight)
         (set-extent-property extent 'mouse-face nil))
        ((eq attr 'writable)
         (set-extent-property extent 'read-only nil))
        ((eq attr 'visible)
         (set-extent-property extent 'invisible nil))
        (t
         (set-extent-property extent attr t))))
;;(make-obsolete 'set-extent-attribute 'set-extent-property) ;; warns on startup in Emacs 28.2
(defun extent-data (extent)
  "Obsolete.  Return the `data' property of EXTENT."
  (extent-property extent 'data))

;; This is a workaround for some versions of VM to make them display
;; images of type "image/pjpeg" as well as image/jpeg
(if (boundp 'vm-mime-image-type-converter-alist)
    (add-to-list 'vm-mime-image-type-converter-alist '("image" "image/pjpeg" "/usr/bin/convert - jpeg:-")))

;; Some versions of VM display images by default and they
;; can be unweildy.  Disable this.
(setq-default vm-auto-displayed-mime-content-types '("text" "multipart"))

;; Bug reporting is sometimes sent to wrong address
;; (by some versions of xemacs)
(defun fix-bug-address ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^To: xemacs-beta@cvs.xemacs.org$" nil t)
        (replace-match "To: xemacs-beta@xemacs.org")
      )))
(add-hook 'mail-send-hook 'fix-bug-address)
(add-hook 'vm-mail-send-hook 'fix-bug-address)

;; Another fix - Big5 messages are sometimes sent out as
;; iso-2022-jp
(defun vm-workaround-big5-bug ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (and (fboundp 'get-coding-system) (eql buffer-file-coding-system (get-coding-system 'big5-unix)))
        (if (re-search-forward "^Content-Type: text/plain; charset=iso-2022-jp$" nil t)
            (replace-match "Content-Type: text/plain; charset=big5")
          ))
    ;; and while we're at it:
    (if (and (fboundp 'find-coding-system) (eql buffer-file-coding-system (find-coding-system 'utf-8)))
        (if (re-search-forward "^Content-Type: text/plain; charset=gb2312$" nil t)
            (replace-match "Content-Type: text/plain; charset=utf-8")
          ))
    ))
(add-hook 'mail-send-hook 'vm-workaround-big5-bug)
(add-hook 'vm-mail-send-hook 'vm-workaround-big5-bug)

;; When I say "followup" I don't want it to go to me as well
(condition-case nil
(setq vm-reply-ignored-addresses (cons user-mail-address vm-reply-ignored-addresses))
(error nil))

;; This might make things faster:
(setq vm-mime-base64-decoder-program "mmencode")
(setq vm-mime-base64-decoder-switches '("-b" "-u"))
(setq vm-mime-base64-encoder-program "mmencode")
(setq vm-mime-base64-encoder-switches '("-b"))
