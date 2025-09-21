;;; init-deps.el --- One-shot dependency setup (fast follow-up runs) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'init-deps-config)

(defun my-deps-all-present-p ()
  (cl-every #'locate-library my-required-libraries))

(defun my-deps-install-if-needed ()
  "If deps are not yet complete, install only the missing ones and record completion."
  (unless my-deps-complete
    ;; 1) Install missing ELPA packages only.
    (require 'init-packages)  ;; uses guard to avoid heavy work if not needed
    (let ((pkgs '(use-package gptel org org-roam org-roam-ui fsrs org-srs yasnippet org-web-tools transient)))
      (dolist (p pkgs)
        (unless (locate-library (symbol-name p))
          (ignore-errors (package-install p)))))

    ;; 2) Install missing vendor repos only (no updates of those already present).
    (require 'init-vendor)
    (my-vendor-autonomous-setup)

    ;; 3) Verify and record success.
    (when (my-deps-all-present-p)
      (my-deps--record-success))))

(provide 'init-deps)
;;; init-deps.el ends here
