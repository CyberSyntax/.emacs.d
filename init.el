;;; init.el --- Main Emacs configuration entry point -*- lexical-binding: t; -*-

;; ===================================================================
;; Initial Setup
;; ===================================================================

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defvar my-var-directory (expand-file-name "var/" user-emacs-directory)
  "Directory for storing volatile data like caches, history, etc.")
(unless (file-directory-p my-var-directory)
  (make-directory my-var-directory t))

(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/sessions/" my-var-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list/backups/" my-var-directory) t)))

;; ===================================================================
;; Global Settings (Set Before Loading Modules)
;; ===================================================================

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq debug-on-error t)
(setq completion-styles '(substring partial-completion flex))

(defvar org-agenda-directory (expand-file-name "org" user-emacs-directory)
  "Default directory for Org files. Can be overridden in init-local.el.")
(setq cache-dir (expand-file-name "cache/" my-var-directory))
(unless (file-directory-p cache-dir) (make-directory cache-dir t))
(setq bookmark-default-file (expand-file-name "bookmarks" my-var-directory))

;; ===================================================================
;; Load Modules
;; ===================================================================

;; 1. Load and execute the vendor package manager first.
(require 'init-vendor)
(my-vendor-autonomous-setup)

;; 2. Load the main package management system.
(require 'init-packages)

;; 3. Load personal configuration modules.
(require 'init-gptel)
(require 'init-ui)
(require 'init-org)
(require 'init-android)

;; ===================================================================
;; Load Vendor Packages
;; ===================================================================

(require 'org-headline-manager)
(require 'hanja-reading)
(require 'org-queue)
(require 'org-story)

;; ===================================================================
;; Final Steps
;; ===================================================================

;; Load machine-specific settings, if any.
(load (expand-file-name "lisp/init-local.el" user-emacs-directory) 'noerror)

(message "Emacs configuration successfully loaded.")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
