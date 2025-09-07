;;; lisp/init-ui.el -*- lexical-binding: t -*-

;;; Font Settings
;; Base font: Noto Serif (handles Latin punctuation automatically)
(set-face-attribute 'default nil :family "Noto Serif")

;; PUA area only: CMUO Serif
(dolist (range '((#xE000 . #xE00D)  ; Uppercase consonants
                 (#xE00E . #xE017)  ; Uppercase vowels
                 (#xE018 . #xE01C)  ; Uppercase special
                 (#xE01D . #xE02A)  ; Lowercase consonants
                 (#xE02B . #xE034)  ; Lowercase vowels
                 (#xE035 . #xE039))) ; Lowercase special
  (set-fontset-font t range "CMUO Serif"))

;; CJK area: Noto Serif TC
(set-fontset-font t 'han "Noto Serif TC")

;; Enable wrapping by visual lines instead of logical lines
(global-visual-line-mode t)

(provide 'init-ui)

;;; lisp/init-ui.el ends here
