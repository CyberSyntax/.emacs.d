;;; init-flycheck.el --- Global Flycheck + Proselint integration -*- lexical-binding: t; coding: utf-8 -*-

;; Plain-ASCII, no reader macros (#' or #()), robust JSON parser for proselint.

(require 'cl-lib)   ;; cl-loop
(require 'json)     ;; json-read-from-string

(when (require 'flycheck nil t)
  ;; -------------------------
  ;; Global defaults
  ;; -------------------------
  (setq flycheck-global-modes t
        flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save idle-change)
        flycheck-idle-change-delay 0.8
        flycheck-emacs-lisp-load-path 'inherit)

  ;; Enable Flycheck globally after init (no #' reader macro)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; -------------------------
  ;; Proselint checker (JSON)
  ;; -------------------------
  (when (executable-find "proselint")
    (defun my/flycheck-proselint-json-parse (output checker buffer)
      "Parse Proselint --json OUTPUT into Flycheck errors."
      (when (and output (string-match-p "[^[:space:]]" output))
        (let* ((json-object-type 'alist)
               (json-array-type  'vector)
               (json-key-type    'symbol)
               (data (ignore-errors (json-read-from-string output)))
               (errs (and (alist-get 'data data)
                          (alist-get 'errors (alist-get 'data data)))))
          (cl-loop
           for e across (or errs [])
           for line = (or (alist-get 'line e) 1)
           for col  = (max 1 (or (alist-get 'column e) 1))
           for code = (or (alist-get 'check e) "proselint")
           for msg  = (or (alist-get 'message e) "proselint issue")
           collect (flycheck-error-new-at
                    line col 'warning
                    (format "%s (%s)" msg code)
                    :checker checker
                    :buffer buffer)))))

    (flycheck-define-checker proselint-json
      "Proselint JSON checker for prose."
      :command ("proselint" "--json" source)
      :error-parser my/flycheck-proselint-json-parse
      :modes (text-mode markdown-mode gfm-mode org-mode))

    ;; Register and prefer it in writing modes
    (add-to-list 'flycheck-checkers 'proselint-json)
    (dolist (hook '(text-mode-hook markdown-mode-hook gfm-mode-hook org-mode-hook))
      (add-hook hook
                (lambda ()
                  (setq-local flycheck-checker 'proselint-json)
                  (flycheck-mode 1)))))

  ;; -------------------------
  ;; Helper
  ;; -------------------------
  (defun my/flycheck-status ()
    "Echo current Flycheck checker and status."
    (interactive)
    (if (bound-and-true-p flycheck-mode)
        (message "Flycheck: %s %s"
                 (or flycheck-checker "none")
                 (flycheck-mode-line-status-text))
      (message "Flycheck is disabled in this buffer."))))

(provide 'init-flycheck)
;;; init-flycheck.el ends here