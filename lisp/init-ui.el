;;; lisp/init-ui.el -*- lexical-binding: t; -*-

(defun my/font (family filename)
  "Return a FONT usable by set-fontset-font.
Order:
  1) Prefer FAMILY if installed.
  2) Use a file font:
     - Android: copy once from shared to internal, then use internal.
     - Others: use shared file directly if readable.
Returns either FAMILY (string), a (font-spec :file ...), or nil."
  (let* ((family-installed (and family (find-font (font-spec :family family))))
         (internal-dir (expand-file-name "~/.emacs.d/fonts/"))
         (internal-file (expand-file-name filename internal-dir))
         (shared-file (expand-file-name (concat "fonts/" filename) user-emacs-directory)))
    (cond
     ;; 1) Family first
     (family-installed family)
     ;; 2) File-based (internal first)
     ((file-readable-p internal-file)
      (font-spec :file internal-file))
     ;; 3) Shared file: Android → copy once to internal; others → use shared directly
     ((file-readable-p shared-file)
      (if (eq system-type 'android)
          (progn
            (make-directory internal-dir t)
            ;; Copy only if it does not exist; do not overwrite
            (unless (file-exists-p internal-file)
              (ignore-errors (copy-file shared-file internal-file)))
            (when (file-readable-p internal-file)
              (font-spec :file internal-file)))
        ;; Non-Android can use the shared file directly
        (font-spec :file shared-file)))
     ;; Nothing found
     (t nil))))


(let ((can-font (fboundp 'set-fontset-font)))
  (cond
   ;; macOS
   ((eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Noto Serif")
    (when can-font
      ;; PUA range: apply CMUO Serif first
      (set-fontset-font t '(#xE000 . #xF8FF) "CMUO Serif" nil 'prepend)
      ;; Han glyphs: apply Noto Serif TC next
      (set-fontset-font t 'han "Noto Serif TC" nil 'prepend)))

   ;; Android
   ((eq system-type 'android)
    (when can-font
      ;; PUA
      (let ((f (my/font "CMUO Serif" "CMUOSerif-Roman.otf")))
        (when f
          (set-fontset-font t '(#xE000 . #xF8FF) f nil 'prepend)))
      ;; Han
      (let ((f (my/font "Noto Serif TC" "NotoSerifTC-Regular.ttf")))
        (when f
          (set-fontset-font t 'han f nil 'prepend)))))))

(global-visual-line-mode t)

(provide 'init-ui)

;;; lisp/init-ui.el ends here
