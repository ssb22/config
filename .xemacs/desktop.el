;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.

;; desktop.el - initial desktop etc, including how to handle
;; scratch & wiki (emacs will be running all the time so can
;; use it for notes etc).  Includes launch of small-frame,
;; gnuserv, &c

;; ***********************************************
;; Scratch stuff:
;; ***********************************************
;; Get rid of the scratch message
;; (would be nice to get rid of the splash screen also)
(setq initial-scratch-message nil)

;; Turn auto-save on in scratch buffer if we're on the
;; console
(if running-on-console
    (setq initial-major-mode #'(lambda () (fundamental-mode) (auto-save-mode t)))
  )

;; Now using "emacs wiki" to keep notes instead of scratch
;; (on machines where it is available).  However, still want
;; C-x C-f to default to the home directory (not the wiki
;; directory) so:
(defun my-wiki-find-file ()
    (interactive)
    (if (emacs-wiki-directories-member)
        (find-file (read-file-name "Find file: " "~/"))
      (call-interactively 'find-file)))
(add-hook 'emacs-wiki-mode-hook
          #'(lambda ()
             (progn
               (local-set-key
                [(control x) (control f)]
                'my-wiki-find-file)
               ;; Also add emacs-wiki-highlight-buffer
               ;; because not all versions seem to do it by
               ;; default
               (emacs-wiki-highlight-buffer)
               (emacs-wiki-use-font-lock)
               ;; And fix-fonts just in case
               (fix-fonts)
               )))
(require 'font-lock)

(setq-default emacs-wiki-directories '("~/.xemacs/wiki"))
(make-face 'emacs-wiki-header-1)
(make-face 'emacs-wiki-header-2)
(make-face 'emacs-wiki-header-3)

;; Start the wiki if available (and we're on the console)
(if running-on-console
 (progn
    (condition-case nil
          (emacs-wiki-find-file "WelcomePage")
      (error (if (file-exists-p "~/.xemacs/wiki/WelcomePage")
                 (find-file "~/.xemacs/wiki/WelcomePage")
               ;; otherwise start with *scratch*, but at least save some horizontal space on its modeline if possible ("Fundamental" is a long word and may push the clock off screen)
               (when (eq system-type 'darwin)
                 (add-hook 'emacs-startup-hook #'do-face-remaps)
                 (defun do-face-remaps ()
                   (face-remap-add-relative 'mode-line :family "Arial")
                   (face-remap-add-relative 'mode-line-inactive :family "Arial")
                   )))))
  (fix-fonts)
  ))

;; On some versions of JDE there's a conflict between JDE
;; and emacs-wiki.  Use java-mode instead of jde-mode.
(defun jde-mode () (java-mode))
;; Some versions of lilypond mode wreak havoc with kill/yank
;; in other buffers - best turn it off to be safe
(defun LilyPond-mode () (fundamental-mode))

;; ***********************************************
;; Initial desktop:
;; ***********************************************
;; Load a shell if we're in XEmacs 21
(cond
 ((and (string-match "XEmacs" emacs-version)
       (boundp 'emacs-major-version)
       (>= emacs-major-version 21))
  (progn
    ;; (new-frame) (other-frame 1)
    ;; (shell)
    ;; ;; (font-lock-mode)
    ;; ;; (other-window 1)
    ;; (other-frame 1)
    (setq auto-save-interval 0);; don't save based
    ;; on keystrokes (too slow in xemacs21) - only on
    ;; idle time
    (setq auto-save-timeout 100) ;; a bit less than 960, to
    ;; make up for the 0 auto-save-interval and occasional
    ;; xemacs crashes (NB the logarithmic thing, so don't
    ;; worry about large buffer holdup)
    )))

;; Add an appropriately-sized frame
;; (if running-on-console (small-frame))
;; this is better (and copes with small TTYs) :
(if (eq (mytty-type) nil)
    (condition-case nil ; (condition-case added for FSF Emacs 22)
(general-small-frame
 (min 60 (- (frame-width) 1))
 (min
  (if (eq window-system 'mac) 18 20)
  (frame-height)))
(error nil))
)
;; and for MS Windows maximise:
(condition-case nil (w32-send-sys-command #xf030) (error nil))

;; Start gnuserv if we're on the console
(condition-case nil
(if running-on-console (gnuserv-start)) (error nil))

;; If you want to browse Gemini space with Elpher
;; and have msttcorefonts installed, you can try:
(add-hook 'elpher-mode-hook
          (lambda ()
            (toggle-word-wrap t) ;; for proportional wrapping
            (face-remap-add-relative 'default :family "Arial")))
(defun window-width (&optional WINDOW PIXELWISE)
    (if (equal mode-name "elpher")
        32767 ;; because we're using toggle-word-wrap instead
      (window-body-width WINDOW PIXELWISE)))
(custom-set-variables
 '(elpher-default-url-type "gemini")
 '(elpher-gemini-max-fill-width 32767) ;; use toggle-word-wrap instead
 '(elpher-start-page-url "gemini://gemini.ctrl-c.club/~ssb22/"))
