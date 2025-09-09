;;; lisp/init-ui.el -*- lexical-binding: t; -*-

(defun my/font (family filename)
  "Return a FONT usable by `set-fontset-font`.
Order:
1. Prefer FAMILY if installed.
2. Use a file font:
   - Android: copy once from shared to internal, then use internal.
   - Others: use shared file directly if readable.
Returns either FAMILY (string), a (font-spec :file ...), or nil."
  (let* ((family-installed (and family (find-font (font-spec :family family))))
         (internal-dir (expand-file-name "~/.emacs.d/fonts/"))
         (internal-file (expand-file-name filename internal-dir))
         (shared-file (expand-file-name (concat "fonts/" filename) user-emacs-directory)))
    (cond
     ;; 1) Use installed family first
     (family-installed family)
     ;; 2) Internal file
     ((file-readable-p internal-file)
      (font-spec :file internal-file))
     ;; 3) Shared file: Android → copy once to internal, others → use shared directly
     ((file-readable-p shared-file)
      (if (eq system-type 'android)
          (progn
            (make-directory internal-dir t)
            (unless (file-exists-p internal-file)
              (ignore-errors (copy-file shared-file internal-file)))
            (when (file-readable-p internal-file)
              (font-spec :file internal-file)))
        (font-spec :file shared-file)))
     ;; Nothing found
     (t nil))))

(when (fboundp 'set-fontset-font)

  ;; -------------------------------
  ;; macOS: create fontset without custom name to avoid XLFD errors
  ;; -------------------------------
  (when (eq system-type 'darwin)
    (let ((fontset (create-fontset-from-ascii-font "Noto Serif-12")))
      ;; PUA range
      (set-fontset-font fontset '(#xE000 . #xF8FF) "CMUO Serif" nil 'prepend)
      ;; Han glyphs
      (set-fontset-font fontset 'han "Noto Serif TC" nil 'prepend)
      ;; Apply to default frame
      (add-to-list 'default-frame-alist `(font . ,fontset))))

  ;; -------------------------------
  ;; Android: use file-based fonts
  ;; -------------------------------
  (when (eq system-type 'android)
    ;; PUA font
    (let ((f (my/font "CMUO Serif" "CMUOSerif-Roman.otf")))
      (when f
        (set-fontset-font t '(#xE000 . #xF8FF) f nil 'prepend)))
    ;; Han font
    (let ((f (my/font "Noto Serif TC" "NotoSerifTC-Regular.ttf")))
      (when f
        (set-fontset-font t 'han f nil 'prepend)))))

;; Enable visual line mode globally
(global-visual-line-mode t)

(provide 'init-ui)

;;; lisp/init-ui.el ends here