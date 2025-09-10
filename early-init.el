;;; early-init.el --- Cross-platform early setup (no copy on Android) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'timer)  ;; avoid (void-function timerp) on some builds

;; 0) Disable VC on Android when git isn't available (prevents git ls-files calls on open)
(when (eq system-type 'android)
  (unless (executable-find "git")
    (setq vc-handled-backends nil)
    (remove-hook 'find-file-hook #'vc-refresh-state)
    (setq auto-revert-check-vc-info nil)
    (with-eval-after-load 'vc
      (setq vc-handled-backends nil)
      (remove-hook 'find-file-hook #'vc-refresh-state))))

;; 1) Make GITHUB_TOKEN available early for init-vendor
(let* ((token-env (getenv "GITHUB_TOKEN"))
       (token-file (expand-file-name ".github-token" user-emacs-directory))
       (token-file-val (when (and (not token-env) (file-readable-p token-file))
                         (string-trim (with-temp-buffer
                                        (insert-file-contents token-file)
                                        (buffer-string)))))
       (token (or token-env token-file-val)))
  (when (and token (not (string-empty-p token)))
    (setenv "GITHUB_TOKEN" token)))

;; 2) Optional: try to bridge Termux bin into PATH/exec-path if accessible (usually sandboxed)
(when (eq system-type 'android)
  (let* ((prefix "/data/data/com.termux/files/")
         (bin    (expand-file-name "usr/bin" prefix)))
    (when (file-accessible-directory-p bin)
      (setenv "PREFIX" prefix)
      (let* ((current-path (or (getenv "PATH") ""))
             (parts (split-string current-path path-separator t))
             (new-path (mapconcat #'identity
                                  (delete-dups (append parts (list bin)))
                                  path-separator)))
        (setenv "PATH" new-path))
      (setq exec-path (cl-remove-if-not #'file-accessible-directory-p exec-path))
      (add-to-list 'exec-path bin t))))

;;; early-init.el ends here
