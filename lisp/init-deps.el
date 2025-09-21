;;; init-deps.el --- One-shot dependency setup (fast follow-up runs) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'init-deps-config)

(defun my-deps-all-present-p ()
  (cl-every #'locate-library my-required-libraries))

(defun my-deps-install-if-needed ()
  "If deps are not yet complete, install only the missing ones and record completion."
  (unless my-deps-complete
    ;; 1) Install missing ELPA packages only, derived from `my-required-libraries`.
    (require 'init-packages)  ;; handles package init, mirrors, stubs, etc.
    (dolist (entry my-required-libraries)
      (let* ((lib (if (symbolp entry) (symbol-name entry) entry))
             (pkg (intern lib)))
        (unless (locate-library lib)
          ;; Try to install from ELPA; vendor-only libs will just error out (ignored).
          (ignore-errors (package-install pkg)))))

    ;; 2) Install missing vendor repos only (no updates of those already present).
    (require 'init-vendor)
    (my-vendor-autonomous-setup)

    ;; 3) Verify and record success.
    (when (my-deps-all-present-p)
      (my-deps--record-success))))

(provide 'init-deps)
;;; init-deps.el ends here
