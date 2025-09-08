;;; lisp/init-packages.el --- Robust package management with mirror fallback -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-parse)
(require 'package)

;; ===================================================================
;; Core behavior and directories
;; ===================================================================

(setq byte-compile-warnings '(not free-vars obsolete))
(setq load-prefer-newer t)
(setq package-enable-at-startup nil)

(defvar my-var-directory (expand-file-name "var/" user-emacs-directory)
  "Directory for variable state files (history, caches, etc).")
(unless (file-directory-p my-var-directory)
  (make-directory my-var-directory t))

;; TLS tweaks (some networks/servers have TLS1.3 handshake quirks)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Keep network waits short so init won't block too long.
(defvar my/url-request-timeout 8
  "Seconds to wait on a single HTTP(S) request before timing out.")
(setq url-request-timeout my/url-request-timeout)

;; Optional: set EMACS_OFFLINE=1 in your environment to force offline mode.
(defvar my/offline-env
  (let ((v (or (getenv "EMACS_OFFLINE") "")))
    (member (downcase v) '("1" "true" "yes" "on")))
  "Non-nil means force offline mode regardless of connectivity check.")

;; ===================================================================
;; Proxy support from environment
;; ===================================================================

(defun my/setup-proxy-from-env ()
  "Configure `url-proxy-services` from *_proxy environment variables."
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
;; Connectivity and ELPA mirror selection
;; ===================================================================

(defun my/network-online-p ()
  "Quickly check if the network is reachable."
  (let ((url-request-timeout my/url-request-timeout))
    (condition-case nil
        (let ((buf (url-retrieve-synchronously "https://elpa.gnu.org/packages/" t t)))
          (when buf (kill-buffer buf) t))
      (error nil))))

(defconst my/network-allowed
  (and (not my/offline-env) (my/network-online-p))
  "Non-nil when we believe network is available and allowed.")

(defvar my/elpa-mirror-sets
  '((gnu "https://elpa.gnu.org/packages/"
         "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"
         "https://mirrors.ustc.edu.cn/elpa/gnu/"
         "https://mirrors.bfsu.edu.cn/elpa/gnu/")
    (nongnu "https://elpa.nongnu.org/nongnu/"
            "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"
            "https://mirrors.ustc.edu.cn/elpa/nongnu/"
            "https://mirrors.bfsu.edu.cn/elpa/nongnu/")
    (melpa "https://melpa.org/packages/"
           "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"
           "https://mirrors.ustc.edu.cn/elpa/melpa/"
           "https://mirrors.bfsu.edu.cn/elpa/melpa/")
    (melpa-stable "https://stable.melpa.org/packages/"
                  "https://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/"
                  "https://mirrors.ustc.edu.cn/elpa/stable-melpa/"
                  "https://mirrors.bfsu.edu.cn/elpa/stable-melpa/"))
  "Per-archive fallback URLs (tries each until one works).")

(defun my/check-elpa-url (base)
  "Return non-nil if BASE (an ELPA base URL) responds for archive-contents."
  (let* ((url-request-timeout my/url-request-timeout)
         (url (concat (file-name-as-directory base) "archive-contents"))
         (buf (condition-case nil
                  (url-retrieve-synchronously url t t)
                (error nil))))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (if (re-search-forward "^HTTP/1\\.[01] \\([0-9]+\\)" nil t)
                (let ((code (string-to-number (match-string 1))))
                  (and (>= code 200) (< code 400)))
              t))
        (kill-buffer buf)))))

(defun my/select-elpa-mirrors ()
  "Pick one working mirror per archive defined in `my/elpa-mirror-sets`."
  (let (chosen)
    (dolist (spec my/elpa-mirror-sets)
      (let* ((name (symbol-name (car spec)))
             (urls (cdr spec))
             (ok-url nil))
        (dolist (u urls)
          (when (and (not ok-url) (my/check-elpa-url u))
            (setq ok-url u)))
        (when ok-url
          (push (cons name ok-url) chosen))))
    (nreverse chosen)))

(defun my/bootstrap-package-archives ()
  "Set `package-archives` to first working mirrors and set priorities."
  (if (not my/network-allowed)
      (setq package-archives nil)
    (let ((selected (my/select-elpa-mirrors)))
      (setq package-archives selected)
      (setq package-archive-priorities
            (let (prio)
              (dolist (p '(("gnu" . 20)
                           ("nongnu" . 15)
                           ("melpa" . 10)
                           ("melpa-stable" . 8)))
                (when (assoc (car p) selected)
                  (push p prio)))
              (nreverse prio))))))

(defun my/package-refresh-archives-with-fallback (&optional timeout)
  "Refresh archives once using selected mirrors."
  (let ((url-request-timeout (or timeout my/url-request-timeout)))
    (condition-case err
        (progn
          (when (and my/network-allowed (null package-archives))
            (my/bootstrap-package-archives))
          (when package-archives
            (package-refresh-contents)))
      (error
       (message "package-refresh-contents suppressed: %s" (error-message-string err))
       nil))))

;; Also suppress any hard error from other code calling `package-refresh-contents`.
(defun my/pkg-refresh-contents-suppress-errors (orig-fn &rest args)
  (let ((url-request-timeout my/url-request-timeout))
    (condition-case err
        (apply orig-fn args)
      (error
       (message "package-refresh-contents suppressed: %s" (error-message-string err))
       nil))))
(advice-add 'package-refresh-contents :around #'my/pkg-refresh-contents-suppress-errors)

;; ===================================================================
;; package.el initialization and mirror bootstrap
;; ===================================================================

(package-initialize)
(my/bootstrap-package-archives)
(my/package-refresh-archives-with-fallback)

;; ===================================================================
;; Resilient `use-package` bootstrap (with offline stub)
;; ===================================================================

(defun my/define-use-package-stub ()
  "Define a minimal `use-package` stub that won't error offline."
  (defvar use-package-always-ensure nil
    "When non-nil, `use-package` ensures package is installed.")
  (defmacro use-package (name &rest plist)
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
  "Ensure `use-package` is available. Install or stub."
  (or (require 'use-package nil 'noerror)
      (progn
        (when (and my/network-allowed package-archives)
          (let ((url-request-timeout my/url-request-timeout))
            (ignore-errors
              (unless package-archive-contents
                (my/package-refresh-archives-with-fallback))
              (package-install 'use-package))))
        (or (require 'use-package nil 'noerror)
            (progn
              (my/define-use-package-stub)
              (run-with-idle-timer
               10 nil
               (lambda ()
                 (when (and (not (featurep 'use-package))
                            (my/network-online-p))
                   (let ((url-request-timeout my/url-request-timeout))
                     (condition-case _
                         (progn
                           (unless package-archive-contents
                             (my/package-refresh-archives-with-fallback))
                           (package-install 'use-package)
                           (message "Installed use-package; restart Emacs to use the real package."))
                       (error nil))))))
              t)))))
(my/ensure-use-package)

(setq use-package-always-ensure (and my/network-allowed t))

(with-eval-after-load 'use-package
  (when (fboundp 'use-package-ensure-elpa)
    (defun my/around-use-package-ensure-elpa (orig name ensure state &optional no-refresh)
      (if (not my/network-allowed)
          (progn
            (message "use-package: skipping ensure for %s (offline)" name)
            t)
        (condition-case err
            (funcall orig name ensure state no-refresh)
          (error
           (message "use-package ensure failed for %s: %s (suppressed)" name (error-message-string err))
           t))))
    (advice-add 'use-package-ensure-elpa :around #'my/around-use-package-ensure-elpa)))

;; ===================================================================
;; Packages
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
  (setq transient-history-file (expand-file-name "transient-history.el" my-var-directory)))

(provide 'init-packages)

;;; lisp/init-packages.el ends here