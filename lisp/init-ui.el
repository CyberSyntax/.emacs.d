;;; lisp/init-ui.el -*- lexical-binding: t; -*-

(defun my/font-file (name)
  (let ((f (expand-file-name (concat "fonts/" name) user-emacs-directory)))
    (when (file-readable-p f) f)))

(let ((can-font (fboundp 'set-fontset-font)))
  (cond
   ;; macOS
   ((eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Noto Serif")
    (when can-font
      (set-fontset-font t '(#xE000 . #xE039) "CMUO Serif" nil 'prepend)
      (set-fontset-font t 'han "Noto Serif TC" nil 'prepend)))

   ;; Android
   ((eq system-type 'android)
    ;; Keep default UI font; only set specific ranges if possible
    (when (and can-font (my/font-file "CMUOSerif-Roman.otf"))
      (set-fontset-font t '(#xE000 . #xE039)
                        (font-spec :file (my/font-file "CMUOSerif-Roman.otf"))
                        nil 'prepend))
    (when (and can-font (my/font-file "NotoSerifTC-Regular.ttf"))
      (set-fontset-font t 'han
                        (font-spec :file (my/font-file "NotoSerifTC-Regular.ttf"))
                        nil 'prepend)))))

(global-visual-line-mode t)

(provide 'init-ui)

;;; lisp/init-ui.el ends here
