;;; lisp/init-packages.el --- Robust package management (offline-safe) -*- lexical-binding: t; -*-

;; This hardens package initialization against network timeouts and prevents
;; startup failures when ELPA/MELPA are unreachable.

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-parse)
(require 'package)

;; ===================================================================
;; 0) Core directories, TLS and timeouts
;; ===================================================================

(defvar my-var-directory (expand-file-name "var/" user-emacs-directory)
  "Directory for variable state files (history, caches, etc).")
(unless (file-directory-p my-var-directory)
  (make-directory my-var-directory t))

;; Prefer newer compiled elisp
(setq load-prefer-newer t)

;; Do not auto-enable packages before init
(setq package-enable-at-startup nil)

;; Some older GnuTLS builds + servers can have TLS1.3 handshake issues.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Keep network waits short so init won't block for long.
(defvar my/url-request-timeout 6
  "Seconds to wait on a single HTTP(S) request before timing out.")
(setq url-request-timeout my/url-request-timeout)

;; Optional: set EMACS_OFFLINE=1 in your environment to force offline mode.
(defvar my/offline-env
  (let ((v (or (getenv "EMACS_OFFLINE") "")))
    (member (downcase v) '("1" "true" "yes" "on")))
  "Non-nil means force offline mode regardless of connectivity check.")

;; ===================================================================
;; 1) Proxy support from environment
;; ===================================================================

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
(my/setup-proxy-from-env)

;; ===================================================================
;; 2) Connectivity detection (non-fatal, fast)
;; ===================================================================

(defun my/network-online-p ()
  "Quickly check if at least one ELPA endpoint is reachable. Non-fatal."
  (let ((url-request-timeout my/url-request-timeout)
        (targets '("https://elpa.gnu.org/packages/"
                   "https://melpa.org/packages/"
                   "https://stable.melpa.org/packages/")))
    (catch 'online
      (dolist (tgt targets)
        (condition-case nil
            (let ((buf (url-retrieve-synchronously tgt t t)))
              (when buf
                (kill-buffer buf)
                (throw 'online t)))
          (error nil)))
      nil)))

(defconst my/network-allowed
  (and (not my/offline-env)
       (my/network-online-p))
  "Non-nil when we believe network is available and allowed.")

;; ===================================================================
;; 3) Package archives and initialization
;; ===================================================================

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 5)
        ("MELPA Stable" . 5)))

(package-initialize)

;; ===================================================================
;; 4) Suppress hard failures from package-refresh-contents when offline
;; ===================================================================

(defun my/pkg-refresh-contents-suppress-errors (orig-fn &rest args)
  "Call ORIG-FN but suppress network errors, returning nil instead of error."
  (let ((url-request-timeout my/url-request-timeout))
    (condition-case err
        (apply orig-fn args)
      (file-error
       (message "package-refresh-contents suppressed: %s" (error-message-string err))
       nil)
      (error
       (message "package-refresh-contents suppressed (other): %s" (error-message-string err))
       nil))))

(advice-add 'package-refresh-contents :around #'my/pkg-refresh-contents-suppress-errors)

;; ===================================================================
;; 5) Resilient per-archive refresh (used only when we do refresh explicitly)
;; ===================================================================

(defun my/package-refresh-archives-resilient (&optional retries)
  "Refresh package archives, tolerating individual archive failures.
Retries up to RETRIES (default 1). Returns non-nil on success."
  (let* ((retries (or retries 1))
         (url-request-timeout my/url-request-timeout))
    (cl-labels ((refresh-once ()
                  (let ((failures '()))
                    (dolist (archive package-archives)
                      (condition-case _
                          (package--with-response-buffer-1
                           (cdr archive)
                           (lambda () (package--download-one-archive archive "archive-contents"))
                           :file "archive-contents"
                           :async nil
                           :error-function (lambda (&rest _) nil)
                           :noerror t)
                        (error (push (car archive) failures))))
                    (ignore-errors (package-read-all-archive-contents))
                    (null failures))))
      (catch 'done
        (dotimes (_ (max 1 retries))
          (when (refresh-once)
            (throw 'done t))
          (sleep-for 0.5))
        nil))))

;; ===================================================================
;; 6) Offline-safe bootstrap for `use-package`
;; ===================================================================

(defun my/define-use-package-stub ()
  "Define a minimal `use-package` stub that won't error offline."
  (defvar use-package-always-ensure nil
    "When non-nil, `use-package` ensures package is installed. Stub ignores offline.")
  (defmacro use-package (name &rest plist)
    "Minimal subset of `use-package` for offline safety:
- Evaluates :init immediately (if provided)
- Tries (require NAME) without error
- Evaluates :config immediately if NAME was loaded, else via `with-eval-after-load`."
    (declare (indent defun))
    (let ((init   (plist-get plist :init))
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
  "Ensure `use-package` is available. If offline, install a stub."
  (or (require 'use-package nil 'noerror)
      (progn
        (when my/network-allowed
          (let ((url-request-timeout my/url-request-timeout))
            (ignore-errors
              (unless package-archive-contents
                (my/package-refresh-archives-resilient 1))
              (package-install 'use-package))))
        (or (require 'use-package nil 'noerror)
            (progn
              (my/define-use-package-stub)
              ;; Try installing the real package later if network becomes available.
              (run-with-idle-timer
               10 nil
               (lambda ()
                 (when (and (not (featurep 'use-package))
                            (my/network-online-p))
                   (let ((url-request-timeout my/url-request-timeout))
                     (condition-case _
                         (progn
                           (unless package-archive-contents
                             (my/package-refresh-archives-resilient 1))
                           (package-install 'use-package)
                           (message "Installed use-package; restart Emacs to switch from stub."))
                       (error nil))))))
              t)))))

(my/ensure-use-package)

;; Always set this BEFORE any `use-package` forms expand.
;; If offline, this prevents `use-package` from trying to install packages and calling ELPA.
(setq use-package-always-ensure (and my/network-allowed t))

;; As an extra safety net: if the real `use-package` is present, ensure network errors are suppressed.
(with-eval-after-load 'use-package
  (when (fboundp 'use-package-ensure-elpa)
    (defun my/around-use-package-ensure-elpa (orig name ensure state)
      (if (not my/network-allowed)
          (progn
            (message "use-package: skipping ensure for %s (offline)" name)
            t)
        (condition-case err
            (funcall orig name ensure state)
          (error
           (message "use-package ensure failed for %s: %s (suppressed)" name (error-message-string err))
           t))))
    (advice-add 'use-package-ensure-elpa :around #'my/around-use-package-ensure-elpa)))

;; ===================================================================
;; 7) Declare Packages (safe even when offline)
;; ===================================================================

(use-package yasnippet
  :init
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