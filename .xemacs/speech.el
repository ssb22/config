;; This file is part of Silas S. Brown's Emacs/XEmacs
;; configuration, which is split across several Lisp files.

;; I wouldn't recommend anyone copies it exactly, but
;; you might be able to copy and paste parts you like.

;; See my website for details on tested Emacs setups.
;; No warranty.

;; functions for speech synthesis using festival
;; (since emacspeak is awkward to set up on some versions of
;; XEmacs)

;; reads words at cursor as go along, sometimes useful
(defun festival-start-reading-words () (interactive)
  (require 'festival)
  (add-hook 'post-command-hook 'festival-say-symbol-if-moved))
;; Can remove: (remove-hook 'post-command-hook 'festival-say-symbol-if-moved)

;; Can already use festival-say-region, festival-say-buffer

;; M-p for read paragraph and M-s for read sentence:

(defun festival-read-paragraph () (interactive)
  (require 'festival)
  (festival-send-command '(audio_mode 'shutup))
  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
    (festival-say-string
     (buffer-substring (car bounds) (cdr bounds)
                       ))))

(defun festival-read-sentence () (interactive)
  (require 'festival)
  (festival-send-command '(audio_mode 'shutup))
  (let ((bounds (bounds-of-thing-at-point 'sentence)))
    (festival-say-string
     (buffer-substring (car bounds) (cdr bounds)
                       ))))

(global-set-key [(meta ?p)] 'festival-read-paragraph)
(global-set-key [(meta ?s)] 'festival-read-sentence)

;; ------------

;; Used by the above:
(setq festival-say-symbol-bounds nil)
(defun festival-say-symbol-if-moved () (interactive)
  (if (not (or (eq this-command 'self-insert-command)
               (eq this-command 'delete-backward-char)
               ))
      (let ((lastBounds festival-say-symbol-bounds)
            (bounds (bounds-of-thing-at-point 'symbol)))
        (if (and (not (eq bounds nil))
                 (not (equal bounds lastBounds)))
            (progn
              (setq festival-say-symbol-bounds bounds)
              (festival-send-command '(audio_mode 'shutup))
              (festival-say-string
               (buffer-substring (car bounds) (cdr bounds)
                                 )))))))
