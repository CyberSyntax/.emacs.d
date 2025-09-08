;;; early-init.el --- Android environment tweaks (no Termux dependency) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

;; Make a GitHub token available as early as possible so init-vendor sees it.
;; Priority:
;;   1) Respect existing env GITHUB_TOKEN (if already set)
;;   2) Else, read ~/.emacs.d/.github-token if present (single line with the token)
(let* ((token-env (getenv "GITHUB_TOKEN"))
       (token-file (expand-file-name ".github-token" user-emacs-directory))
       (token-file-val (when (and (not token-env) (file-readable-p token-file))
                         (string-trim (with-temp-buffer
                                        (insert-file-contents token-file)
                                        (buffer-string)))))
       (token (or token-env token-file-val)))
  (when (and token (not (string-empty-p token)))
    (setenv "GITHUB_TOKEN" token)))

;; On native Android Emacs, avoid assuming Termux exists.
;; If Termux is actually installed and its directories are accessible,
;; we optionally bridge its bin into PATH/exec-path. Otherwise, do nothing.
(when (eq system-type 'android)
  (let* ((prefix "/data/data/com.termux/files/")
         (bin    (expand-file-name "usr/bin" prefix)))
    (when (file-accessible-directory-p bin)
      ;; Export PREFIX only if Termux is present
      (setenv "PREFIX" prefix)
      ;; Append Termux bin to PATH if not already present
      (let* ((current-path (or (getenv "PATH") ""))
             (parts (split-string current-path path-separator t))
             (new-path (mapconcat #'identity
                                  (delete-dups (append parts (list bin)))
                                  path-separator)))
        (setenv "PATH" new-path))
      ;; Clean up exec-path (remove non-existent dirs), then append Termux bin
      (setq exec-path (cl-remove-if-not #'file-accessible-directory-p exec-path))
      (add-to-list 'exec-path bin t))))

;;; early-init.el ends here