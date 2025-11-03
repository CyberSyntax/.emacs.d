;;; init-grammarly.el --- Flycheck + Grammarly setup -*- lexical-binding: t; coding: utf-8 -*-

;; Plain-ASCII, no reader macros. Integrates:
;; - grammarly.el (auth via auth-source)
;; - flycheck-grammarly (generic checker)
;; - Chains after proselint-json so both run.

(require 'cl-lib)

;; Try to load Grammarly core; if present, pull credentials from authinfo.
(when (require 'grammarly nil t)
  ;; Use auth-source: ~/.authinfo(.gpg)
  ;;   machine grammarly.com login YOUR_EMAIL pass YOUR_PASSWORD
  (ignore-errors
    (grammarly-load-from-authinfo))
  ;; Quiet by default
  (defvar grammarly--show-debug-message nil)
  (defvar grammarly-on-open-function-list nil)
  (defvar grammarly-on-message-function-list nil)
  (defvar grammarly-on-close-function-list nil))

;; Flycheck integration
(when (and (require 'flycheck nil t)
           (require 'flycheck-grammarly nil t))
  ;; Base setup from the package
  (with-eval-after-load 'flycheck
    (flycheck-grammarly-setup)

    ;; So Grammarly does not spam while you type too fast
    (defvar flycheck-grammarly-check-time 1.0)

    ;; Prefer to run Grammarly AFTER proselint-json in writing modes.
    ;; This keeps your current proselint-json as the primary checker.
    (when (fboundp 'flycheck-add-next-checker)
      ;; Only add if proselint-json exists; harmless otherwise.
      (flycheck-add-next-checker 'proselint-json '(warning . grammarly) 'append))

    ;; Optional: enable the chain automatically in common writing modes.
    (dolist (hook '(text-mode-hook markdown-mode-hook gfm-mode-hook org-mode-hook))
      (add-hook hook
                (lambda ()
                  ;; Ensure Flycheck is on; your init-flycheck already does this, but harmless if repeated.
                  (flycheck-mode 1)))))

  ;; Handy toggle if you want to disable Grammarly in a noisy buffer
  (defun my/toggle-grammarly-checker ()
    "Toggle the Grammarly checker in the current buffer."
    (interactive)
    (if (member 'grammarly flycheck-disabled-checkers)
        (progn
          (setq-local flycheck-disabled-checkers
                      (remove 'grammarly flycheck-disabled-checkers))
          (message "Grammarly checker ENABLED"))
      (add-to-list 'flycheck-disabled-checkers 'grammarly)
      (message "Grammarly checker DISABLED")))
  )

(provide 'init-grammarly)
;;; init-grammarly.el ends here