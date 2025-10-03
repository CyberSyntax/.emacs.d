;;; init.el --- Main Emacs configuration entry point -*- lexical-binding: t; -*-

;; ===================================================================
;; Initial Setup
;; ===================================================================

;; Keep all modules under ./lisp on the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Ensure any future Customize output never pollutes this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror :nomessage))

(require 'init-deps)
;; If not yet complete, perform the one-time installs now.
(unless my-deps-complete
  (my-deps-install-if-needed))

(defvar my-var-directory (expand-file-name "var/" user-emacs-directory)
  "Directory for storing volatile data like caches, history, etc.")
(unless (file-directory-p my-var-directory)
  (make-directory my-var-directory t))

;; Pre-create commonly used subdirectories under var/
(dolist (sub '("cache/" "auto-save-list/sessions/" "auto-save-list/backups/"))
  (make-directory (expand-file-name sub my-var-directory) t))

(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/sessions/" my-var-directory)
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/backups/" my-var-directory) t)))

;; ===================================================================
;; Global Settings (Set Before Loading Modules)
;; ===================================================================

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(setq debug-on-error t)
(setq completion-styles '(substring partial-completion flex))

;; Root directory for Org files (Android vs. others)
(defvar org-agenda-directory
  (if (eq system-type 'android)
      "/storage/emulated/0/Documents/org"
    ;; macOS/Linux/Windows:
    (expand-file-name "Documents/org" (getenv "HOME")))
  "Directory containing all Org files.")

;; Build org-agenda-files programmatically (no Customize, no giant literal list)
(setq org-agenda-files
      (when (file-directory-p org-agenda-directory)
        (directory-files-recursively org-agenda-directory "\\.org\\'")))

;; Cache directory used by various modules (e.g., org-queue)
(setq cache-dir (expand-file-name "cache/" my-var-directory))
(unless (file-directory-p cache-dir) (make-directory cache-dir t))

;; Keep bookmarks under var/
(setq bookmark-default-file (expand-file-name "bookmarks" my-var-directory))

;; ===================================================================
;; Load Modules
;; ===================================================================

;; Package/bootstrap first (sets mirrors, ensures use-package, etc.)
(require 'init-packages)

;; Vendor manager (GitHub repos). Then RUN it now so vendor libs are present.
(require 'init-vendor)
(my-vendor-autonomous-setup)

;; Load authinfo support early (before modules that need it)
(require 'init-authinfo)

;; Load machine-specific settings (optional, ignored if not present)
(load (expand-file-name "lisp/init-local.el" user-emacs-directory) 'noerror)

;; Personal modules
(require 'init-gptel)
(require 'init-cnfonts)
(require 'init-ui)
(require 'init-tabs)
(require 'init-org)
(require 'init-anki-editor)
(require 'init-gt)
(require 'init-android)

;; ===================================================================
;; Load Vendor Packages (optional; only if present)
;; ===================================================================

(defun require-if-available (feature &optional filename)
  "Require FEATURE if its library is found; else log and return nil.
FEATURE may be a symbol or a string. FILENAME, if non-nil, is the library name to locate."
  (let* ((feat (cond
                ((symbolp feature) feature)
                ((stringp feature) (intern feature))
                (t (error "FEATURE must be symbol or string, got: %S" feature))))
         (lib  (or filename (symbol-name feat))))
    (if (locate-library lib)
        (require feat nil t)
      (message "Skipped require %s (not installed yet)" feat)
      nil)))

(require-if-available 'org-headline-manager)
(require-if-available 'hanja-reading)

;; org-queue uses the same root as my agenda; set this BEFORE loading org-queue
(setq org-queue-directory org-agenda-directory)

(require-if-available 'org-queue)
(require-if-available 'org-story)

;; ===================================================================
;; Final Steps
;; ===================================================================

(message "Emacs configuration successfully loaded.")

;;; init.el ends here