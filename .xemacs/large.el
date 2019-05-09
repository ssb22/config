;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.

;; (large print, increasing real-estate, user functions
;; (reflow) &c)

;; Time-stamp: <2019-05-08>

;; ***********************************************
;; Large fonts
;; ***********************************************

;; service routine
(defun mysubstring (STRING FROM &optional TO)
  (condition-case nil
      (substring STRING FROM TO)
    (error ""))
  )

(defun mytty-type ()
  (condition-case nil
      (tty-type)
    (error nil))
  )

;; fix-fonts: This function seems to have no effect on
;; faces that are not loaded until after the event, hence
;; interactive (so you can run it with M-x if necessary).
;; Its effect is to ensure that all faces that are bold,
;; italic, or bold-italic, use the fonts that you have
;; specified for the "bold", "italic" and "bold-italic"
;; faces, rather than other fonts that emacs might guess at
;; using make-face-bold etc.
(defun fix-fonts ()
  (interactive)
  (condition-case nil (progn
  (let (temp global props weight slant is-bold is-italic)
    (setq temp (face-list))
    (while (consp temp)
      (progn
        (setq global (window-frame (get-buffer-window (current-buffer))))
        ;; (because we don't want to apply buffer local fonts to all buffers)
         (setq props
               (if (or (string= (mysubstring (format "%s" (car temp)) 0 8) "modeline") (string= (mysubstring (format "%s" (car temp)) 0 18) "my-global-modeline") (string= (mysubstring (format "%s" (car temp)) 0 11) "buffers-tab")) nil ; hack for some xemacs 21.5 beta versions (seen in FC6) which segfaults when you call the following font-properties on any modeline or buffers-tab face
               (font-properties (face-property (car temp) 'font) global)
)
)
        (if props ;; if it's nil then it's a tty frame
            (progn
              (setq weight (downcase (cdr (assoc 'WEIGHT_NAME props))))
              (setq slant (downcase (cdr (assoc 'SLANT props))))
              (setq is-bold (not (or (string= weight "normal") (string= weight "regular") (string= weight "medium") (string= weight "light") (string= weight "thin"))))
              ;; (setq is-bold (face-property (car temp) 'bold))
              (setq is-italic (or (string= slant "o") (string= slant "i")))
              ;; (setq is-italic (face-property (car temp) 'italic))
              ;; Only touch it if the encoding is the same
              ;;(setq defaultProps (font-properties (face-property (make-face 'default) 'font) global))
              ;;(if
              ;;(and
              ;;(string= (cdr (assoc 'CHARSET_REGISTRY props)) (cdr (assoc 'CHARSET_REGISTRY defaultProps)))
              ;;(string= (cdr (assoc 'CHARSET_ENCODING props)) (cdr (assoc 'CHARSET_ENCODING defaultProps))))
              
              ;; Don't touch "default", "italic" etc
              (if (not (or (string= (car temp) "default")
                           (string= (car temp) "bold")
                           (string= (car temp) "italic")
                           (string= (car temp) "message-cited-text") ;; hack - see below
                           (string= (car temp) "bold-italic")
                           (string= (mysubstring (format "%s" (car temp)) 0 8) "modeline")
                           ;; One last condition - don't
                           ;; touch faces that don't
                           ;; actually specify a font
                           ;; themselves (e.g. highlighting
                           ;; etc)
                           (not (specifier-specs
                                 (face-property (car temp)
                                                'font)))))
                  ;; OK to touch it
                  (set-face-font
                   (make-face (car temp))
                   ;;(specifier-instance
                    (face-property
                     (make-face
                      (cond
                       ((and is-bold is-italic)
                        'bold-italic)
                       (is-bold 'bold)
                       (is-italic 'italic)
                       (t 'default)
                       )
                      ) 'font);; global)
                    )
                )
              ))
        (setq temp (cdr temp))
        )
      )
    )
  (set-face-font my-global-modeline-face (face-font (make-face 'modeline))) ;; here as well, just in case
  ) (error nil) ;; (may get here if FSF Emacs; see pc-default-font)
  ))

(add-hook 'font-lock-after-fontify-buffer-hook 'fix-fonts)
(add-hook 'hyper-apropos-mode-hook 'fix-fonts)
(add-hook 'Manual-mode-hook 'fix-fonts)

;; Optional: zooming keys for FSF Emacs, using files from
;;    http://www.emacswiki.org/emacs/download/zoom-frm.el
;;    http://www.emacswiki.org/emacs/download/frame-cmds.el
;;    http://www.emacswiki.org/emacs/download/frame-fns.el
;; useful if you have variable sight (especially with Freetype fonts)
;; (WARNING - if that wiki is open to vandalism, verify the files first!)
(condition-case nil
    (require 'zoom-frm)
  (error
   (condition-case nil
       (progn
         (load "~/.xemacs/frame-fns.el")
         (load "~/.xemacs/frame-cmds.el")
         (load "~/.xemacs/zoom-frm.el"))
     (error nil))
   ))
(if (fboundp 'zoom-in)
    (progn
      (define-key global-map (kbd "C-+") 'zoom-in)
      (define-key global-map (kbd "C--") 'zoom-out)
      ))

;; You can start with a larger font by doing e.g.:
;; (dotimes (_ 15 nil) (call-interactively 'zoom-in))
;; but this applies only to the first frame.  It's
;; probably best to set DPI accordingly for all frames.

(condition-case nil
(if xemacs-betaname (defun mouse-drag-modeline (arg) t)) ;; desperate hack because some FC6 ship with beta versions where this is broken (xemacs segfaults when click the modeline)
(error nil))

;; Font settings:
(defun pc-default-font ()
  (interactive)
  (condition-case nil
  (if (and (eq (mytty-type) nil) (eq system-type 'darwin))
  ;; FSF Emacs 23 on Mac - can't assume DPI is set, but screen zoom is available
  (set-face-attribute 'default nil :font "Monospace-18")
  ;; else
  (if (and (eq (mytty-type) nil) (eq system-type 'windows-nt))
  ;; this seems to work on double-DPI Vista:
  ;; (might need to be adjusted on some systems)
  (set-face-attribute 'default nil :font "Monospace-16")
  ;; else
  ;; This works in FSF Emacs 23 in high-res high-DPI screens (assumes DPI is set high)
  (set-face-attribute 'default nil :font "Monospace-12")
  ;; If DPI is not set high, you might need to set this to something like
  ;; (set-face-attribute 'default nil :font "Monospace-26")
  ))
  ;; Fallback for older bitmap-only systems (see .Xresources for notes on why the 20)
  (error (condition-case nil (progn
  (set-face-font (make-face 'default) (make-font-instance "-*-fixed-medium-r-normal-*-20-*-100-*-c-*-*-*"))
  ;; Need a proportional font for the modeline so that more
  ;; can fit on it; needs to be legible but without giving
  ;; the modeline too much space.  Trying this for now:
  ;; (set-face-font 'modeline "-*-aplos-medium-r-normal-*-18-*-*-*-p-*-*-*")
  (condition-case nil
      (set-face-font (make-face 'modeline) (make-font-instance "-*-aplos-bold-r-normal-*-*-160-*-*-p-*-*-*"))
    ;; That doesn't always work - try another.  Must be
    ;; sans-serif, preferably 160 (and preferably not
    ;; a scaled bitmap)
    (error
     (set-face-font (make-face 'modeline) (make-font-instance "-*-helvetica-bold-r-*-*-17-*-*-*-p-*-*-*"))
     ;; (set-face-font (make-face 'modeline) (make-font-instance "-adobe-helvetica-bold-r-normal-*-*-180-*-*-p-*-iso8859-1"))
     ))
  ;; If we let emacs make up a bold font itself, it will
  ;; probably look awful.  Here's one.  Bitstream
  ;; courier/charter is not so tiring.
  (set-face-font (make-face 'bold) (make-font-instance "-bitstream-courier-bold-r-normal-*-20-*-*-*-m-*-*-*"))
  ;; Italic is the most difficult - so many italic fonts
  ;; are unreadable.  However, it doesn't really matter if
  ;; it's proportional.  (could even use proportionality
  ;; instead of italicity)
  ;; (set-face-font (make-face 'italic) (make-font-instance "-*-utopia-regular-i-normal-*-19-*-*-*-p-*-*-*"))
  (set-face-font (make-face 'italic) (make-font-instance "-adobe-utopia-regular-r-normal-*-19-*-*-*-p-*-iso8859-1"))
  ;; Hack: message-cited-text sometimes gets left out (need
  ;; to fix this)
  (set-face-font (make-face 'message-cited-text) (make-font-instance "-adobe-utopia-regular-r-normal-*-19-*-*-*-p-*-iso8859-1"))
  ;; (set-face-font (make-face 'bold-italic) (make-font-instance "-*-utopia-medium-i-normal-*-19-*-*-*-p-*-*-*"))
  (condition-case nil
      (set-face-font (make-face 'bold-italic) (make-font-instance "-*-lucida-bold-r-normal-*-19-*-*-*-p-*-iso8859-1"))
    (error
     (set-face-font (make-face 'bold-italic) (make-font-instance "-adobe-utopia-bold-r-normal-*-19-*-*-*-p-*-iso8859-1"))
     ))
  (condition-case nil (nice-cjk-fonts) (error nil))
  (fix-fonts) ) (error (condition-case nil (progn (set-face-attribute 'default nil :family "Courier New") (set-face-attribute 'default nil :height 180) (error nil)))))))) ;; (for FSF Emacs 22, assumes zoom, value is in 1/10-point; Courier New seems to work a bit better than "Monospace" (apple-monaco/default) which for some reason leaves the size not propagating to new frames and not set at all unless this function is run interactively)

;; Set the default font here as well as in the Xresources, in case need to
;; change the stuff below.
;; NEEDATTENTION We don't want to override command-line fonts here (search command-line-args for "-fn" ?)
(condition-case nil
    (pc-default-font)
  (error nil))

;; Double-sized font settings, in case you have to use a
;; high-resolution display (do M-x pc-default-font-double)
(defun pc-default-font-double ()
  (interactive)
  (set-face-font (make-face 'default) (make-font-instance "-misc-fixed-medium-r-normal-*-*-400-*-*-c-*-iso8859-1"))
  (condition-case nil
      (set-face-font (make-face 'modeline) (make-font-instance "-*-aplos-bold-r-normal-*-*-320-*-*-p-*-*-*"))
    (error
     (set-face-font (make-face 'modeline) (make-font-instance "-*-helvetica-bold-r-*-*-34-*-*-*-p-*-*-*"))
     ))
  (set-face-font (make-face 'bold) (make-font-instance "-bitstream-courier-bold-r-normal-*-40-*-*-*-m-*-*-*"))
  ;; for the following, using 33.  38 would be ideal, but X
  ;; server doesn't figure out that this is double 19 and
  ;; complains that it can't find the font.
  (set-face-font (make-face 'italic) (make-font-instance "-adobe-utopia-regular-r-normal-*-33-*-*-*-p-*-iso8859-1"))
  (set-face-font (make-face 'message-cited-text) (make-font-instance "-adobe-utopia-regular-r-normal-*-33-*-*-*-p-*-iso8859-1"))
  (condition-case nil
      (set-face-font (make-face 'bold-italic) (make-font-instance "-*-lucida-bold-r-normal-*-33-*-*-*-p-*-iso8859-1"))
    (error
     (set-face-font (make-face 'bold-italic) (make-font-instance "-adobe-utopia-bold-r-normal-*-33-*-*-*-p-*-iso8859-1"))
     ))
  (condition-case nil (nice-large-cjk-fonts) (error nil))
  (fix-fonts)
  )

;; ***********************************************
;; Miscellaneous things that help in large print
;; ***********************************************

;; Line-at-a-time scrolling
(setq scroll-step 1)

;; Expand the minibuffer if you type a lot
(condition-case nil
(resize-minibuffer-mode)
(error nil)) ; happens anyway on FSF Emacs 23

;; Re-arrange the modeline (save some space)
;; First need a face for global-mode-string (clock etc) to
;; make it stand out, especially if not using the glyphs
;; (because they're too small or whatever)
(condition-case nil (progn
(setq my-global-modeline-extent (make-extent nil nil nil))
(setq my-global-modeline-face (make-face 'my-global-modeline-face))
(copy-face (make-face 'modeline) my-global-modeline-face)
(set-face-foreground 'my-global-modeline-face "yellow")
(set-face-background 'my-global-modeline-face "darkblue")
(set-face-font my-global-modeline-face (face-font (make-face 'modeline))) ;; just in case
(set-extent-face my-global-modeline-extent 'my-global-modeline-face)
;; Now can set the modeline itself
(setq-default
 modeline-format
 (list
  ""
  (if (boundp 'modeline-multibyte-status) 'modeline-multibyte-status "")
  (cons modeline-modified-extent 'modeline-modified)
  (cons modeline-buffer-id-extent
        (list (cons modeline-buffer-id-left-extent
                    ;;(cons 15 (list
                    ;; (list 'line-number-mode "L%l ")
                    ;; (list 'column-number-mode "C%c ")
                    ;;
                    ;;))
                    (cons modeline-buffer-id-right-extent "%17b")
                    )
              ))
  " " (cons -3 "%p");; percentage (or "Top", "All", "Bot") (3chars)
  " "
  (cons my-global-modeline-extent 'global-mode-string) ;; clock etc
  " "
  (list 'line-number-mode "%l")
  (list 'column-number-mode ":%c")
  " %[("
  (cons modeline-minor-mode-extent
        (list "" 'mode-name 'minor-mode-alist))
  (cons modeline-narrowed-extent "%n")
  'modeline-process
  ")%]"
  "%-"
  ))
;; Need to mess with the VM modeline as well, to put the
;; global-mode-string in the right colours.  Hope future
;; versions of VM don't break this.  (Ideally we should just
;; load it and modify the last item on the list.)
(setq-default vm-mode-line-format
  (list
   "" "  %&%& "
   '("VM: "
     (vm-folder-read-only "read-only ")
     (vm-virtual-folder-definition
      (vm-virtual-mirror "mirrored "))
     "%b" (vm-mail-buffer (vm-ml-sort-keys
                           ("" " by " vm-ml-sort-keys)))
     (vm-message-list ("   " vm-ml-message-number
                       " (of " vm-ml-highest-message-number
                       ")")
                      (vm-folder-type
                       "   (unrecognized folder type)"
                       "   (no messages)")))
   '(vm-spooled-mail-waiting " Mail")
   '(vm-message-list ("  %[ "
                      vm-ml-message-attributes-alist
                      (vm-ml-labels ("; " vm-ml-labels))
                      " %]    ") ("  %[%]   ")) "%p" "   "
  (cons my-global-modeline-extent 'global-mode-string)
   ))
) (error nil)) ;; (error on FSF Emacs, TODO FSF Emacs modeline rearrange)

;; don't truncate lines anywhere, except in dired:
(setq-default truncate-lines nil)
(add-hook 'dired-mode-hook 'toggle-truncate-lines)
(setq-default dired-listing-switches "-aGhl")
;; (inhibit group info - slightly less bulky listing)

;; ***********************************************
;; More screen "real estate"
;; ***********************************************

;; Get rid of the toolbar:
(if (featurep 'toolbar) (progn (set-default-toolbar-position 'top) (add-spec-list-to-specifier default-toolbar-visible-p '((global (nil)))) (add-spec-list-to-specifier toolbar-buttons-captioned-p 'nil)))
(condition-case nil (tool-bar-mode -1) (error nil)) ;; FSF Emacs

;; XEmacs 21.4+ please don't do a "gutter"
(if (and (string-match "XEmacs" emacs-version)
         (boundp 'emacs-major-version)
         (>= emacs-major-version 21)
         (boundp 'emacs-minor-version)
         (>= emacs-minor-version 4))
    (set-specifier default-gutter-visible-p nil))

;; ***********************************************
;; Utility functions for user functions below
;; ***********************************************

(defun my-set-fill-column (WIDTH)
  (setq-default fill-column WIDTH)
  (setq fill-column WIDTH) ;; just in case
  (setq-default vm-fill-paragraphs-containing-long-lines WIDTH)
  (setq-default vm-paragraph-fill-column WIDTH)
  (setq process-environment (cons (format "COLUMNS=%d" WIDTH) process-environment)) ;; for manual-entry etc (also cld set MANWIDTH etc)
;; NEEDATTENTION Also info pages and hyper-apropos if possible
  )

;; ***********************************************
;; User functions
;; ***********************************************

(defun menus-off ()
  "Switch off menus and modeline"
  (interactive)
  (set-specifier menubar-visible-p nil)
  (set-specifier has-modeline-p nil)
  )

(defun menus-on ()
  "Switch on menus and modeline"
  (interactive)
  (set-specifier menubar-visible-p t)
  (set-specifier has-modeline-p t)
  )

(defun general-small-frame (w h)
  "Service routine for small-frame and related functions"
  (if window-system (progn
  (condition-case nil
      (progn
        ;; needed e.g. on Mac: change CURRENT frame:
        (set-frame-size (selected-frame) (+ w 1) h)
        (set-frame-position (selected-frame) 0 0)
        ) (error
           ;; if Emacs version can't do that, create a
           ;; NEW frame and delete all other frames.
           ;; This crashes Emacs 25.1 on OS X (but the
           ;; above should work with that).  Emacs 24 on
           ;; Mac doesn't crash with the below but would
           ;; need the above AS WELL (or instead, so we'd
           ;; better just leave this in the error clause)
           (delete-other-frames
            (make-frame (list 'left 0 'top 0 'width (+ w 1) 'height h)))))
  (setq default-x-frame-plist (list 'width (+ w 1) 'height h)) ;; X11
  (setq window-system-default-frame-alist ;; Mac OS (FSF Emacs 23)
        (list (list 'ns (cons 'width (+ w 1)) (cons 'height h))))
  ;; and for Mac OS (FSF Emacs 22) :
  (if (boundp 'default-frame-alist) (progn
    (setq default-frame-alist (assq-delete-all 'width (assq-delete-all 'height default-frame-alist)))
    (add-to-list 'default-frame-alist (cons 'width (+ w 1)))
    (add-to-list 'default-frame-alist (cons 'height h))))
  (my-set-fill-column w)
  (condition-case nil (delete-file "~/.vm.windows") (error nil))
  (find-file "~/.vm.windows")
  (insert (format "((default (nil (((- (0 0 %d %d) (0 %d %d %d)) ((nil summary) (nil message)) ((nil nil nil t) (nil nil nil nil)))))) (composing-message ((((top . 70) (left . 70))) (((0 0 %d %d) ((nil composition)) ((nil nil nil t)))))) (editing-message ((((top . 70) (left . 70))) (((0 0 %d %d) ((nil edit)) ((nil nil nil t)))))))" (+ w 2) (/ h 3) (/ h 3) (+ w 2) (- h 1) w h w h))
  (save-buffer) (kill-buffer nil)
  )))

(defun small-frame ()
  "Set a frame size that will fit in 640x480 at 20px font (so don't have to scroll around with mouse)"
  (interactive) (general-small-frame 60 20))

(defun small-frame-flwm ()
  "as small-frame but flwm allows 1 extra line"
  (interactive) (general-small-frame 60 21))

(defun small-frame-1152-40px ()
  "as small-frame but for 1152x864 and 40px fonts"
  (interactive) (general-small-frame 55 20))

(defun small-frame-1024-40px ()
  "as small-frame but for 1024x768 and 40px fonts"
  (interactive) (general-small-frame 48 17))

(defun small-frame-800-40px ()
  "as small-frame but for 800x600 and 40px fonts"
  (interactive) (general-small-frame 37 13))

(defun small-frame-512-20px-flwm ()
  "as small-frame but for 512x384 and and 20px fonts"
  (interactive) (general-small-frame 48 17))

(defun small-frame-320-20px ()
  "as small-frame but for 320x200 and and 20px fonts"
  (interactive) (general-small-frame 28 9) (menus-off))

;; Function to fill a (text) buffer to the window width
;; Can be slow on large files
;; Not very good with code, HTML etc
(defun fill-buffer ()
  "Fill a buffer to the window width"
  (interactive)
  (setq fill-column (- (window-width) 1))
  (fill-region (point-min) (point-max))
  (goto-line 1)
  ;; (setq fill-column default-fill-column)
  )

;; fill-quoted-paragraph is under VM options

;; This function needs fixing!
(defun fill-buffer-keep-original ()
  "Create a read-only version of the buffer, filled to widow width"
  (interactive)
  (copy-region-as-kill (point-min) (point-max))
  (setq temp major-mode)
  ;; (if (string= temp "Info-mode") (setq temp '"text-mode")) ; hack for info
  (switch-to-buffer (generate-new-buffer "*Fill-Buffer*"))
  (setq major-mode temp)
  (yank)
  (fill-buffer)
  ;; (font-lock-mode)
  (if (not (current-local-map)) (use-local-map (make-keymap)))
  (define-key (current-local-map) [space] 'scroll-up-command)
  (setq buffer-read-only t)
  )

;; Function to properly indent a buffer (of code etc) - in
;; large.el because sometimes needed if someone sends code
;; that was written with a wider width in mind
(defun indent-buffer ()
  "Indent a buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil)
  )
