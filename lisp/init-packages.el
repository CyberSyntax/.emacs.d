;;; lisp/init-packages.el --- Package management setup -*- lexical-binding: t; -*-

;; ===================================================================
;; 1. Configure Package System Behavior FIRST
;; ===================================================================

(setq byte-compile-warnings '(not free-vars obsolete))
(setq load-prefer-newer t)
(setq package-enable-at-startup nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")))
(setq package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 5)
        ("MELPA Stable" . 5)))

;; ===================================================================
;; 2. Initialize the Package System
;; ===================================================================

(require 'package)
(package-initialize)

;; ===================================================================
;; 3. Bootstrap and Configure `use-package`
;; ===================================================================

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; ===================================================================
;; 4. Declare Packages
;; ===================================================================

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (defcustom my-yas-snippet-dir (expand-file-name "snippets/" user-emacs-directory)
    "Directory for storing YASnippet snippets."
    :type 'directory
    :group 'yasnippet)
  (unless (file-directory-p my-yas-snippet-dir)
    (make-directory my-yas-snippet-dir t))
  (setq yas-snippet-dirs (list my-yas-snippet-dir))
  (yas-reload-all))

(use-package org-web-tools)

(use-package transient
  :config
  ;; Tell transient to save its history file inside our clean var/ directory.
  (setq transient-history-file (expand-file-name "transient-history.el" my-var-directory)))

(provide 'init-packages)

;;; lisp/init-packages.el ends here
