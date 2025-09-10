;;; init-deps.el --- One-shot dependency setup (fast follow-up runs) -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst my-deps-record-file
  (expand-file-name "var/deps.done" user-emacs-directory))

(defvar my-deps-complete
  (and (file-exists-p my-deps-record-file)
       (ignore-errors
         (string-match-p
          "\\bok\\b"
          (with-temp-buffer
            (insert-file-contents my-deps-record-file)
            (buffer-string)))))
  "Non-nil means all required deps were installed previously; skip checks/installs now.")

(defun my-deps--record-success ()
  (make-directory (file-name-directory my-deps-record-file) t)
  (with-temp-file my-deps-record-file
    (insert "ok\n"))
  (setq my-deps-complete t))

(defconst my-required-libraries
  '("gptel" "org" "org-roam" "org-roam-ui" "fsrs" "org-srs"
    "yasnippet" "org-web-tools" "transient"
    "org-queue" "org-story" "hanja-reading" "org-headline-manager" "android-support" "cnfonts")
  "Libraries that must be locatable (ELPA or vendor).")

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
