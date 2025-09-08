;;; lisp/init-packages.el --- Robust package management (offline-safe) -*- lexical-binding: t; -*-

;; This file hardens package initialization against network timeouts and adds
;; an offline-safe bootstrap for `use-package` so your init never fails when
;; ELPA/MELPA are unreachable.

;; ===================================================================
;; 0. Core requires and small helpers
;; ===================================================================

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-parse)
(require 'package)

(defvar my-var-directory (expand-file-name "var/" user-emacs-directory)
  "Directory for variable state files (history, caches, etc).")

(unless (file-directory-p my-var-directory)
  (make-directory my-var-directory t))

;; Prefer newer compiled elisp
(setq load-prefer-newer t)

;; Do not auto-enable packages before init
(setq package-enable-at-startup nil)

;; Sometimes TLS 1.3 and some GnuTLS builds cause handshake issues; prefer <= 1.2.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Shorter network timeouts so init doesn't hang for long
(defvar my/url-request-timeout 6
  "Seconds to wait on a single HTTP(S) request before timing out.")

(defvar my/allow-insecure-http-mirrors nil
  "If non-nil, allow HTTP fallbacks to ELPA/MELPA mirrors when HTTPS fails.")

(defun my/setup-proxy-from-env ()
  "Configure `url-proxy-services` from standard *_proxy environment variables."
  (let* ((hp (or (getenv "http_proxy") (getenv "HTTP_PROXY")))
         (sp (or (getenv "https_proxy") (getenv "HTTPS_PROXY")))
         (no (or (getenv "no_proxy") (getenv "NO_PROXY")))
         (parse (lambda (p default-port)
                  (when (and p (stringp p) (not (string-empty-p p)))
                    (let* ((u (url-generic-parse-url
                               (if (string-match-p "\\`https?://" p) p (concat "http://" p))))
                           (host (url-host u))
                           (port (or (url-port u) default-port)))
                      (when host (format "%s:%d" host port)))))))
    (setq url-proxy-services nil)
    (when hp
      (let ((h (funcall parse hp 80)))
        (when h (push (cons "http" h) url-proxy-services))))
    (when sp
      (let ((h (funcall parse sp 443)))
        (when h (push (cons "https" h) url-proxy-services))))
    (when (and no (stringp no) (not (string-empty-p no)))
      (push (cons "no_proxy" no) url-proxy-services))))

(defun my/network-online-p ()
  "Quickly check if we can reach at least one ELPA endpoint."
  (let ((url-request-timeout my/url-request-timeout)
        (targets '("https://elpa.gnu.org/packages/"
                   "https://melpa.org/packages/"
                   "https://stable.melpa.org/packages/"))
        (ok nil))
    (catch 'online
      (dolist (tgt targets)
        (condition-case nil
            (let ((buf (url-retrieve-synchronously tgt t t)))
              (when buf
                (kill-buffer buf)
                (setq ok t)
                (throw 'online t)))
          (error nil))))
    ok))

;; ===================================================================
;; 1. Package archives and initialization
;; ===================================================================

(my/setup-proxy-from-env)

(setq package-archives
      (append
       '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
         ("MELPA Stable" . "https://stable.melpa.org/packages/")
         ("MELPA"        . "https://melpa.org/packages/"))
       ;; Optional mirrors with lower priority (comment/uncomment as needed)
       (when my/allow-insecure-http-mirrors
         '(("GNU ELPA (USTC)" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
           ("MELPA (USTC)"    . "http://mirrors.ustc.edu.cn/elpa/melpa/")
           ("MELPA Stable (USTC)" . "http://mirrors.ustc.edu.cn/elpa/stable-melpa/")))))

(setq package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 5)
        ("MELPA Stable" . 5)))

(package-initialize)

;; ===================================================================
;; 2. Safer refresh with retries and per-archive tolerance
;; ===================================================================

(defun my/package-refresh-archives-resilient (&optional retries)
  "Refresh package archives, tolerating individual archive failures.
Retries up to RETRIES (default 2). Returns non-nil on success."
  (let* ((retries (or retries 2))
         (url-request-timeout my/url-request-timeout)
         (failures '())
         (succeeded nil))

    (cl-labels ((refresh-once ()
                  ;; Use the internal per-archive function so a single failure doesn't abort all.
                  (setq failures '())
                  (dolist (archive package-archives)
                    (condition-case err
                        (package--with-response-buffer-1
                         (cdr archive)
                         (lambda () (package--download-one-archive archive "archive-contents"))
                         :file "archive-contents"
                         :async nil
                         :error-function (lambda (&rest _) nil)
                         :noerror t)
                      (error
                       (push (car archive) failures))))
                  ;; Rebuild the index from whatever succeeded
                  (condition-case nil
                      (package-read-all-archive-contents)
                    (error nil))
                  (setq succeeded (null failures))
                  succeeded))

      (catch 'done
        (dotimes (i (max 1 retries))
          (when (refresh-once)
            (throw 'done t))
          (sleep-for 0.7))
        succeeded))))

;; ===================================================================
;; 3. Offline-safe bootstrap for `use-package`
;; ===================================================================

;; Define a minimal stub if the real use-package isn't available yet.
(defun my/define-use-package-stub ()
  "Define a minimal `use-package` stub that won't error offline."
  (defvar use-package-always-ensure nil
    "When non-nil, `use-package` ensures package is installed. Stub ignores it offline.")
  (defmacro use-package (name &rest plist)
    "Very small subset of `use-package`:
- Evaluates :init immediately (if provided)
- Tries (require NAME) without error
- Evaluates :config immediately if NAME was loaded, or installs `with-eval-after-load` if not
All other keywords are ignored by this stub."
    (declare (indent defun))
    (let* ((init   (plist-get plist :init))
           (config (plist-get plist :config)))
      `(progn
         ,@(when init `((progn ,@init)))
         (let ((loaded (require ',name nil 'noerror)))
           (if loaded
               (progn ,@config)
             (with-eval-after-load ',name
               ,@config)))
         t))))

(defun my/ensure-use-package ()
  "Ensure `use-package` is available. If offline, install a stub and try again later."
  (or (require 'use-package nil 'noerror)
      (progn
        (when (my/network-online-p)
          (let ((url-request-timeout my/url-request-timeout))
            (ignore-errors
              (unless package-archive-contents
                (my/package-refresh-archives-resilient 2))
              (package-install 'use-package))))
        (or (require 'use-package nil 'noerror)
            (progn
              (my/define-use-package-stub)
              ;; Try to replace the stub with the real package a bit later if we come online.
              (run-with-idle-timer
               10 nil
               (lambda ()
                 (when (and (not (featurep 'use-package))
                            (my/network-online-p))
                   (let ((url-request-timeout my/url-request-timeout))
                     (condition-case _err
                         (progn
                           (unless package-archive-contents
                             (my/package-refresh-archives-resilient 2))
                           (package-install 'use-package)
                           (message "Installed use-package; restart Emacs to switch from stub."))
                       (error nil))))))
              t)))))

;; Bootstrap
(my/ensure-use-package)

;; We can safely set this even if the stub is active; it will be ignored.
(setq use-package-always-ensure t)

;; ===================================================================
;; 4. Declare Packages (safe even when offline)
;; ===================================================================

(use-package yasnippet
  :init
  ;; Ensure our snippet dir exists before loading yasnippet
  (defcustom my-yas-snippet-dir (expand-file-name "snippets/" user-emacs-directory)
    "Directory for storing YASnippet snippets."
    :type 'directory
    :group 'yasnippet)
  (unless (file-directory-p my-yas-snippet-dir)
    (make-directory my-yas-snippet-dir t))
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs (list my-yas-snippet-dir))
  (yas-reload-all))

(use-package org-web-tools)

(use-package transient
  :config
  ;; Keep transient history under our `var/` directory
  (setq transient-history-file (expand-file-name "transient-history.el" my-var-directory)))

(provide 'init-packages)

;;; lisp/init-packages.el ends here