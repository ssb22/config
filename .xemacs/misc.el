;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.

;; ***********************************************
;; Protect against sloppy keypresses:
;; ***********************************************
;; Confirm exit (Ctrl-C is right next to Ctrl-X on QWERTY-like layouts, OK on Dvorak)
(defun queryExit ()
  "Ask for confirmation on exit."
  (interactive)
  (switch-to-buffer (get-buffer "*scratch*"))
  (if (eq (buffer-size) 0)
      t
      (y-or-n-p "(Note scratch) Really exit emacs? "))
  )
(setq kill-emacs-query-functions (cons 'queryExit kill-emacs-query-functions))

(if (or (eq window-system 'mac) (eq window-system 'ns))
;; http://superuser.com/questions/308045/disallow-closing-last-emacs-window-via-window-manager-close-button
;; (too easy to click there by mistake on the Mac)
(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
      "Ask for confirmation before deleting the last frame"
      (let ((frame   (posn-window (event-start event)))
            (numfrs  (length (visible-frame-list))))
        (when (or (> numfrs 1) (y-or-n-p "Really exit Emacs? "))
          ad-do-it))))

;; Avoid accidentally creating new buffers if Ctrl-X B's tab completion
;; doesn't complete uniquely and you don't notice
(defadvice switch-to-buffer
  (before exising-buffer activate compile)
  "Avoid accidentally creating new buffers"
  (interactive
   (list (read-buffer "Switch to buffer: "
                      (other-buffer) (null current-prefix-arg)))))

;; Please don't beep for long (do need non-0 volume for
;; e.g. i-search wrapping)
(setq bell-volume 10)
(setq sound-alist nil)

;; Get rid of M-z keybinding (hardly ever used, destructive,
;; and sometimes pressed accidentally, so don't want it)
(global-unset-key [(meta ?z)]) ;; zap-to-char

;; ***********************************************
;; Miscellaneous user functions:
;; ***********************************************

;; Function to copy a URL
(defun copy-url-at-point ()
  "Copy URL at point"
  (interactive)
  (kill-new (url-get-url-at-point))     ; emacs clipboard
  (own-selection (url-get-url-at-point)) ; X clipboard
  )

;; Ctrl-` macro for answering "please proofread" emails
(defun insert-comment ()
  "Insert a proof-reading comment into reply"
  (interactive)
  (let ((myComment (read-from-minibuffer "Comment: ")))
    (insert (concat "\n\n" myComment "\n\n> "))
    ))
(global-set-key [(control ?`)] 'insert-comment)

;; Functions for launching commonly-used programs

(defun run-in-background (PROGRAM &optional ARGS)
  (interactive "FRun: ")
  (shell-command
   (concat "/sbin/start-stop-daemon --start --exec `which "
           PROGRAM
           "` --name @@rubbish@@ -bq -- "
           ARGS))
  (clear-message)
  )

(defun crontab () (interactive)
  (run-in-background "crontab" "-e"));; which makes a
;; gnuserv connection to this xemacs process anyway
(defun galeon () (interactive)
  (run-in-background "galeon"))
(defun dillo () (interactive)
  (run-in-background "dillo"))
;; +sb = no scrollbar.  Change rxvt to xterm if you don't have rxvt on your system.
(defun top () (interactive)
  (run-in-background "rxvt" "+sb -n top -T top -e top"))
(defun rxvt () (interactive)
  (run-in-background "rxvt" "-e bash -c \"cd;exec bash\""))
(defun su () (interactive)
  (run-in-background "rxvt" "-e su root -c \"cd;exec bash\""))
;; (not "su -" because still want the environment, e.g. X display)

;;(defun mozilla () (interactive)
;;  (run-in-background "mozilla"))
(defun mozilla () (interactive)
  (run-in-background "firefox"))

(defun lynx () (interactive)
  (run-in-background "rxvt" "+sb -n lynx -T lynx -e lynx"))
;; or use M-x w3m

(if (fboundp 'browse-url-default-macosx-browser)
    (progn
    (setq-default browse-url-browser-function 'browse-url-default-macosx-browser)
    (setq-default vm-url-browser 'browse-url-default-macosx-browser)
      )
  (progn
    (setq-default browse-url-browser-function 'browse-url-mozilla)
    (setq-default vm-url-browser 'browse-url-mozilla) ;; so VM's
;; mouse-2 also starts mozilla, although it might not appear
;; on the mouse-3 menu
    ))
(setq-default browse-url-new-window-flag t)
;; (not using w3 as default now mozilla css is working
;; properly.  w3 is still fairly slow on some pages but good
;; on others.)

;; ***********************************************
;; W3 stuff:
;; ***********************************************
;;(setq-default browse-url-browser-function 'browse-url-w3)
(custom-set-variables
 '(w3-user-fonts-take-precedence t)
 '(w3-user-colors-take-precedence t)
 '(w3-honor-stylesheets nil)
 ;; Setting w3-horizontal-rule-char to '-' because the
 ;; default (use terminal graphics characters) doesn't
 ;; always work very well under X
 '(w3-horizontal-rule-char 45)
 '(w3-use-terminal-characters-on-tty nil)
 '(w3-default-homepage (quote "http://localhost/cgi-bin/access?Aem=3&Aesu=on&Aes=on"))
 '(w3-use-terminal-characters t)
 '(w3-latex-print-links (quote footnote))
 '(w3-max-menu-length 20)
 '(w3-notify (quote quiet))
 ;; '(w3-debug-html (quote style))
 '(w3-debug-html nil)
 '(w3-do-incremental-display t)
 '(url-mail-command (quote mail))
 '(url-privacy-level (quote (email cookie)))
 '(url-be-asynchronous t)
 )

;; ***********************************************
;; Shell stuff:
;; ***********************************************
(custom-set-variables
 '(shell-option-face (quote shell-option-face))
 '(shell-multiple-shells t)
 '(shell-output-face (quote shell-output-face))
 '(comint-scroll-to-bottom-on-input (quote this))
 )
;; Ensure we go into font lock
(add-hook 'shell-mode-hook 'font-lock-mode)

(defun python () (interactive) (py-shell))
;; because "M-x python" completes faster than "M-x py-shell"

;; ***********************************************
;; Info stuff:
;; ***********************************************
(custom-set-variables
 '(Info-auto-advance t)
 '(Info-enable-edit t)
 '(Info-button1-follows-hyperlink t)
 )

;; ***********************************************
;; Miscellaneous "features":
;; ***********************************************

;; Parenthesis highlighting (please don't blink):
(condition-case nil (require 'paren) (error nil))
(setq-default paren-mode 'paren)
(condition-case nil (paren-set-mode 'paren) (error nil))
(condition-case nil (show-paren-mode t) (error nil))
;; (different versions do it in different ways)

;; Use pathnames instead of <n> to uniquify buffer names
(condition-case nil (require 'uniquify) (error nil))
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Enable visiting compressed files
(condition-case nil (progn (require 'jka-compr) (jka-compr-install)) (error nil))

(condition-case nil (require 'tramp) (error nil))
;; Don't use tramp-util for tramp-compile - xemacs21.4.17 it's not asynchronous (use "compile" with the ssh commands instead)
;; Define a tramp-quit for use if the connection goes down during the session (call tramp-quit before opening any more files over tramp) :
(defun tramp-quit () (interactive)
  (setq tmp (buffer-list))
  (while tmp
    (if (eq (string-match "^\*tramp" (buffer-name (car tmp))) 0) (kill-buffer (car tmp)))
    (setq tmp (cdr tmp))
    ))

;; Set auto save file locations to home directory
;; - useful if you're using curlftpfs, sshfs, TRAMP, etc
;; and don't want emacs to autosave over the remote link
(setq auto-save-file-name-transforms `((".*" "~/" t)))
;;(setq backup-directory-alist `((".*" . "~")))
(setq ange-ftp-auto-save 1) ;; (in case ange-ftp instead of TRAMP is used for the FTP files)

;; On some Emacs if Tramp visits a file over FTP it will
;; try to change shell-command to do rsh (which is less
;; likely to be available and is no good if the only
;; command you want to run is to check the date or
;; something)
;; The following should help.  It makes sure shell
;; commands run from your home directory if the current
;; buffer is FTP (SSH is OK), and it raises an error if
;; you try to M-x grep from an FTP directory.
(defadvice shell-command (around no-rsh)
(let ((default-directory (cond ((and (>= (length default-directory) 5) (string-equal (substring default-directory 0 5) "/ftp:")) "/") (t default-directory)))) ; ("/" works better than "~" on FSF Emacs 22)
    ad-do-it))
(ad-activate 'shell-command)
(defadvice grep (around no-rsh)
  (if (and (>= (length default-directory) 5) (string-equal (substring default-directory 0 5) "/ftp:")) (progn (beep) (message "Cannot grep over FTP")) ad-do-it))
(ad-activate 'grep)

;; Put "redo" on the menus, if available
(condition-case nil (require 'redo) (error nil))
;; If not available, note this paragraph from the Emacs manual:
;; Any command other than an undo command breaks the sequence of undo
;; commands.  Starting at this moment, the previous undo commands are
;; considered ordinary changes that can themselves be undone.  Thus, you
;; can redo changes you have undone by typing `C-f' or any other command
;; that will have no important effect, and then using more undo commands.

;; Enable "Functions" menu in C and C++ code, if available
(condition-case nil
    (progn
      (require 'func-menu)
      (add-hook 'c-mode-hook 'fume-add-menubar-entry)
      (add-hook 'c++-mode-hook 'fume-add-menubar-entry)
      (setq-default fume-mode-line-string nil)
      ) (error nil))
;; and in case that doesn't work, do this:
(if (fboundp 'imenu-add-to-menubar)
    (progn
      (add-hook 'c-mode-hook #'(lambda () (imenu-add-to-menubar "IM-C")))
      (add-hook 'c++-mode-hook #'(lambda () (imenu-add-to-menubar "IM-C++")))
      ))

;; Enable recursive minibuffers by default (if in a
;; different window)
(setq minibuffer-max-depth nil)

;; Load time stamp (if present)
(condition-case nil 
(progn (require 'time-stamp) (add-hook 'write-file-hooks 'time-stamp))
(error nil))
;; (setq time-stamp-format "%04y-%02m-%02d");; ISO 8601
;; above doesn't seem to be supported anymore so:
(defun my-time-string () (format-time-string "%Y-%m-%d"))
(setq time-stamp-format '(my-time-string))
(setq time-stamp-old-format-warn nil) ;; (needed on newer versions of Emacs)

;; Initialise x-symbol if available
;; (and we're on the console - this doesn't work so well
;; from across the network because it tries to mess with
;; font paths)
;; 2001-11-08: COMMENTED OUT - it interferes with multi-language CJK stuff
;;(if running-on-console
;;    (condition-case nil (x-symbol-initialize) (error nil))
;;  )

;; M-. and M-, to go to definition and back WITHOUT ctags
;; (if dumb-jump installed on Emacs 24.3+)
(if
    (or
     (> emacs-major-version 24)
     (and (= emacs-major-version 24) (> emacs-minor-version 2)))
    (condition-case nil
        (progn
          (require 'dumb-jump)
          (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
      (error nil)))

;; ***********************************************
;; Miscellaneous editing options:
;; ***********************************************

;; Enable auto-fill by default in all text modes:
(add-hook 'text-mode-hook #'(lambda () (auto-fill-mode 1)))

;; "filladapt" is slightly better at filling bulleted lists
;; and so forth.
(condition-case nil (progn (require 'filladapt) (add-hook 'text-mode-hook 'turn-on-filladapt-mode)) (error nil))

;; Might want it on in fundamental mode etc so that M-q uses
;; it, but it's not good with noninteractive use on text
;; that begins paragraphs with indentation (and some email
;; and news articles are like that) - it keeps the linebreak
;; after the first line of such paragraphs.
;; setq-default filladapt-mode (or custom-set-variables)
;; (add-hook 'first-change-hook 'turn-on-filladapt-mode)
;; (no, still used by VM)
(setq-default filladapt-mode-line-string nil)

;; Another fill option: optimal fill (run through "fmt")
(defun optimal-fill () (interactive)
  (shell-command-on-region
   (if (region-active-p) (region-beginning) (car (bounds-of-thing-at-point 'paragraph)))
   (if (region-active-p) (region-end) (cdr (bounds-of-thing-at-point 'paragraph)))
 (format "fmt -%d" fill-column) t t)
  )
(global-set-key [(control meta ?q)] 'optimal-fill)

;; Get return to do auto-indent (not just in Python)
(condition-case nil
(define-key global-map (kbd "RET") 'newline-and-indent) ;; FSF Emacs
(error (global-set-key [(return)] 'newline-and-indent))) ;; xemacs
(global-set-key [(linefeed)] 'newline)

(setq-default overwrite-mode nil)
(setq-default case-fold-search t) ;; ignore case in searches
(setq-default case-replace t)
(setq-default zmacs-regions t) ;; region activates/deactives

;; When pasting text, paste at the cursor position (so that
;; the mouse doesn't have to be aimed too accurately) :
(setq-default mouse-yank-at-point t)

(setq-default require-final-newline t)
(setq-default next-line-add-newlines nil) ;; stop at end
(setq-default teach-extended-commands-p t)
(setq-default teach-extended-commands-timeout 4)
(setq-default debug-on-error nil)
(setq-default debug-on-quit nil)

;; Don't pop up new windows:
(setq-default get-frame-for-buffer-default-instance-limit nil)
(if (fboundp 'show-temp-buffer-in-current-frame)
    (setq-default temp-buffer-show-function 'show-temp-buffer-in-current-frame))

(condition-case nil (progn
(if (featurep 'scrollbar) (progn (add-spec-list-to-specifier scrollbar-width 'nil) (add-spec-list-to-specifier scrollbar-height 'nil)))
(add-spec-list-to-specifier modeline-shadow-thickness '((global (nil . 2))))
) (error nil))

(setq-default bar-cursor nil) ;; a block is more visible
(setq-default buffers-menu-max-size 25)
(setq-default complex-buffers-menu-p nil)
(setq-default buffers-menu-sort-function 'sort-buffers-menu-by-mode-then-alphabetically)
(setq-default buffers-menu-grouping-function 'group-buffers-menu-by-mode-then-alphabetically)
(setq-default buffers-menu-submenus-for-groups-p 15)
;; (if more than 15 buffers, have submenus)

(setq-default font-menu-ignore-scaled-fonts nil)
(setq-default font-menu-this-frame-only-p nil)
(setq-default mouse-avoidance-mode nil)
(if (featurep 'mule) (set-language-environment '"English"))
(custom-set-variables
 '(widget-choice-toggle t)
 '(outline-glyphs-on-left t)
 '(save-place t)
 '(widget-mouse-face (quote highlight))
 '(column-number-mode t)
 '(save-place-limit 50)
 '(kill-ring-max 100)
 '(tab-width 4)
 '(whitespace-chars (quote tabs))
 '(indent-tabs-mode nil)
 '(query-replace-highlight t)
 '(mark-ring-max 100)
 '(line-number-mode t)
 '(kill-whole-line t)
 '(global-mark-ring-max 100)
 '(auto-save-offer-delete t)
 '(disable-auto-save-when-buffer-shrinks nil)
 ;; (above: it's usually INBOX)
 '(line-move-ignore-invisible t)
 )
(condition-case nil (require 'whitespace-mode) (error nil))
(setq-default whitespace-mode t)
;; NEEDATTENTION The following has no effect
(setq-default whitespace-mode-line-string nil)
(setq-default whitespace-incremental-mode-line-string nil)

(setq inhibit-startup-message t) ;; FSF Emacs

(setq line-move-visual nil) ;; for FSF Emacs 23+
(global-set-key (kbd "M-g") 'goto-line)
(setq x-select-enable-clipboard t)
(global-set-key (kbd "M-C-l") #'(lambda () (interactive) (switch-to-buffer (other-buffer))))
(condition-case nil
    (progn
      ;; needed on Mac OS X Lion:
      (global-set-key '[(kp-delete)] 'delete-char)
      ;; some other Mac versions (Emacs in a Terminal):
      (global-set-key (kbd "M-[ 5 d") 'backward-word)
      (global-set-key (kbd "M-[ 5 c") 'forward-word)
      ;; in some Mac versions (e.g. Lion) you have to go to
      ;; System Preferences / Keyboard / Keyboard shortcuts / Mission Control
      ;; and turn off all 4 of the ctrl-arrow shortcuts
      ;; so that ctrl-arrow can be used with applications.
      ;; Also, please do Home and End by line:
      (global-set-key '[(home)] 'beginning-of-line)
      (global-set-key '[(end)] 'end-of-line)
      )
  (error nil))
(if (fboundp 'hyper-apropos) t (defun hyper-apropos () (interactive) (apropos (read-from-minibuffer "Apropos: "))))
(setq exec-path (cons "/usr/local/bin" (cons "/sw/bin" exec-path))) ;; for Mac
(if (fboundp 'xemacs-splash-buffer) t
  (progn
    ;; based on http://emacswiki.org/emacs/minibuf-electric-gnuemacs.el
    (defvar my-electric-file-mode nil)
      (defvar my-electric-file-mode-map (make-sparse-keymap))
      (make-local-variable 'my-electric-file-mode)
      (if (not (fboundp 'minibuffer-prompt-end))
          (defun minibuffer-prompt-end ()
            (point-min)))
      (defun minibuffer-electric-slash ()
        (interactive)
        (and (eq ?/ (preceding-char))
             (not (eq (point) (1+ (minibuffer-prompt-end))))
             (not (eq ?: (char-after (- (point) 2))))
             (delete-region (minibuffer-prompt-end) (point)))
        (insert ?/))
      (defun minibuffer-electric-tilde ()
        (interactive)
        (if (eq ?/ (preceding-char))
            (delete-region (minibuffer-prompt-end) (point)))
        (insert ?~))
      (define-key my-electric-file-mode-map "/" 'minibuffer-electric-slash)
      (define-key my-electric-file-mode-map "~" 'minibuffer-electric-tilde)
      (setq minor-mode-map-alist (cons (cons 'my-electric-file-mode
                                             my-electric-file-mode-map)
                                       minor-mode-map-alist))
      (defun my-electric-minibuffer-setup ()
        (setq my-electric-file-mode
              (and (boundp 'minibuffer-completion-table)
                   (eq minibuffer-completion-table
        'read-file-name-internal))))
      (defun my-electric-minibuffer-exit ()
        (setq my-electric-file-mode nil))
      (add-hook 'minibuffer-setup-hook 'my-electric-minibuffer-setup)
      (add-hook 'minibuffer-exit-hook  'my-electric-minibuffer-exit)
    ))

(put 'upcase-region 'disabled nil) ;; don't disable M-x upcase-region
(setq vc-handled-backends ()) ;; don't try version control if using sshfs
;; (it can go very wrong if there's lots of git branches and the link is bad)

;; for FSF Emacs 22; happens anyway in 23+ (and xemacs) :
(condition-case nil (transient-mark-mode 1) (error nil))
(setq frame-title-format "%b")
(setq mac-option-modifier 'meta)
(if (or (boundp 'shift-select-mode) (fboundp 'xemacs-splash-buffer))
    t
  (condition-case nil
      (progn
        ;; if shift-select-mode is not available and not xemacs (i.e. FSF Emacs 22), we have to make do with pc-select
        (require 'pc-select)
        (pc-selection-mode)
        ;; make pc-selection-mode more like shift-select-mode by also allowing C-Space + arrows to mark visibly, and by turning off delete-selection-mode:
        (defun exchange-point-and-mark-nomark () (interactive) (exchange-point-and-mark))
        (defun forward-char-nomark (&optional arg) (interactive) (forward-char arg))
        (defun forward-word-nomark (&optional arg) (interactive) (forward-word arg))
        (defun forward-line-nomark (&optional arg) (interactive) (forward-line arg))
        (defun forward-sexp-nomark (&optional arg) (interactive) (forward-sexp arg))
        (defun forward-paragraph-nomark (&optional arg) (interactive) (forward-paragraph arg))
        (defun next-line-nomark (&optional arg) (interactive) (next-line arg))
        (defun end-of-line-nomark (&optional arg) (interactive) (end-of-line arg))
        (defun backward-line-nomark (&optional arg) (interactive) (backward-line arg))
        (defun scroll-down-nomark (&optional arg) (interactive) (scroll-down arg))
        (defun end-of-buffer-nomark (&optional arg) (interactive) (end-of-buffer arg))
        (defun backward-char-nomark (&optional arg) (interactive) (backward-char arg))
        (defun backward-word-nomark (&optional arg) (interactive) (backward-word arg))
        (defun backward-sexp-nomark (&optional arg) (interactive) (backward-sexp arg))
        (defun backward-paragraph-nomark (&optional arg) (interactive) (backward-paragraph arg))
        (defun previous-line-nomark (&optional arg) (interactive) (previous-line arg))
        (defun beginning-of-line-nomark (&optional arg) (interactive) (beginning-of-line arg))
        (defun scroll-up-nomark (&optional arg) (interactive) (scroll-up arg))
        (defun beginning-of-buffer-nomark (&optional arg) (interactive) (beginning-of-buffer arg))
        (delete-selection-mode nil)
        ) (error nil)))
(setq default-buffer-file-coding-system 'utf-8)
(condition-case nil
    (prefer-coding-system 'utf-8) ;; for filesystem etc
  (error nil))

;; Need to do the following to make M-x xterm-mouse-mode
;; work, at least on some versions of Emacs 22.  (We don't
;; turn on M-x xterm-mouse-mode by default, because it
;; disables the terminal's own copy/paste functions, which
;; might involve a different clipboard if it's not a real
;; xterm, e.g. iTerm2 etc; can't always tell with getenv)
(condition-case nil (progn
(require 'mouse)
(if (not (fboundp 'track-mouse)) (defun track-mouse (e)))
(global-set-key [mouse-4] #'(lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] #'(lambda () (interactive) (scroll-up 1)))
) (error nil))

;; If using Aquamacs, make it a bit more Emacs-like
;; (tested on Aquamacs 24 based on GNU Emacs 23,
;;  and Aquamacs 3.3 based on GNU Emacs 25.1.1)
(condition-case nil (run-at-time "1 sec" nil (lambda nil (setq select-enable-clipboard t))) (error nil)) ;; needed on 3.3 (1-second delay to work around something that sets it back to nil on startup after init.el has finished)
(condition-case nil (run-at-time "1 sec" nil (lambda nil (condition-case nil (cua-mode -1) (error nil)))) (error nil)) ;; ditto on 3.4: we want C-x C-x 'swap' to work
(defun savehist-save (x) nil) ;; (otherwise Aquamacs 2.4 can't quit if it's running on a filesystem that won't let it chmod the minibuffer-history file)
(setq-default cursor-type 'box) ;; (might be useful on other emacsen too)
(condition-case nil (aquamacs-autoface-mode -1) (error nil))
(condition-case nil (global-smart-spacing-mode -1) (error nil))
(condition-case nil (global-visual-line-mode -1) (error nil))
(condition-case nil (one-buffer-one-frame-mode -1) (error nil))
(condition-case nil (osx-key-mode -1) (error nil))
(condition-case nil (set-face-attribute 'echo-area nil :family 'unspecified) (error nil)) ;; (otherwise dances up and down whenever used)
(setq special-display-regexps nil)
(setq-default aquamacs-ring-bell-on-error-flag t)
(setq ring-bell-function nil)
(setq ns-command-modifier 'meta) ;; NextStep (Mac) Command key is meta not alt
(setq ns-use-mac-modifier-symbols nil)
(condition-case nil (smart-frame-positioning-mode -1) (error nil))
(condition-case nil (tabbar-mode -1) (error nil))
(condition-case nil (remove-hook 'text-mode-hook 'smart-spacing-mode) (error nil))
(condition-case nil (remove-hook 'text-mode-hook 'auto-detect-wrap) (error nil))
(condition-case nil (remove-hook 'text-mode-hook 'visual-line-mode) (error nil))
(condition-case nil (add-hook 'html-helper-mode-hook #'(lambda () (condition-case nil (html-mode) (error nil)))) (error nil))
(condition-case nil (fringe-mode 'default) (error nil))
(condition-case nil (let ((el (assoc 'empty-line default-fringe-indicator-alist)) (co (assoc 'continuation default-fringe-indicator-alist))) (if el (setcdr el nil)) (if co (setcdr co '(left-curly-arrow right-curly-arrow)))) (error nil)) ;; Aquamacs' ellipses are hard for low vision, + the empty-line thing can be too stripey for nystagmus
(setq-default ispell-program-name "aspell") ;; you'll need to set up cocoAspell or something
;; TODO: make the modeline font a bit larger! (changing faces here doesn't seem to take effect)
;; TODO: rename the Window menu back to Buffers

;; make MELPA packages available
(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("elpa" . 2) ("melpa-stable" . 1) ("melpa" . 0))) ;; especially if not (> emacs-major-version 25), e.g. markdown-mode non-stable requires Emacs 26 as of 2022-06
(package-initialize)
