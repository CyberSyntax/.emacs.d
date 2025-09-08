;;; lisp/init-ui.el -*- lexical-binding: t; -*-

(defun my/font-file (name)
  "Return absolute path to fonts/NAME under `user-emacs-directory` if readable."
  (let ((f (expand-file-name (concat "fonts/" name) user-emacs-directory)))
    (when (file-readable-p f) f)))

(cond
 ;; macOS: your original setup (families are installed system-wide)
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Noto Serif")
  ;; PUA area: CMUO Serif family
  (set-fontset-font t '(#xE000 . #xE039) "CMUO Serif" nil 'prepend)
  ;; CJK Han: Noto Serif TC family
  (set-fontset-font t 'han "Noto Serif TC" nil 'prepend))

 ;; Native Android: load fonts directly from files (no Termux)
 ((eq system-type 'android)
  ;; Keep the system default for Latin unless you want a full-serif UI.
  ;; If you prefer a serif default on Android, uncomment the block below.
  ;;
  ;; (when-let ((noto-tc (my/font-file "NotoSerifTC-Regular.ttf")))
  ;;   (set-face-attribute 'default nil :font (font-spec :file noto-tc)))
  ;;
  ;; PUA range (E000..E039): CMUO Serif
  (when-let ((cmuo (my/font-file "CMUOSerif-Roman.otf")))
    (set-fontset-font t '(#xE000 . #xE039) (font-spec :file cmuo) nil 'prepend))
  ;; Han script (CJK ideographs, includes Hanja)
  (when-let ((noto-tc (my/font-file "NotoSerifTC-Regular.ttf")))
    (set-fontset-font t 'han (font-spec :file noto-tc) nil 'prepend))))

;; Wrap by visual lines
(global-visual-line-mode t)

(provide 'init-ui)
;;; lisp/init-ui.el ends here