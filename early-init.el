;;; early-init.el --- Android environment tweaks (no Termux dependency) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

;; 0) Disable VC on Android when git isn't available
;; Prevents Emacs from calling external VCS tools (e.g. git ls-files) on file open.
(when (eq system-type 'android)
  (unless (executable-find "git")
    ;; Don’t try any VCS backends
    (setq vc-handled-backends nil)
    ;; Stop VC from probing files at open
    (remove-hook 'find-file-hook #'vc-refresh-state)
    ;; Avoid VC checks in auto-revert
    (setq auto-revert-check-vc-info nil)
    ;; Belt-and-suspenders if vc.el is loaded later
    (with-eval-after-load 'vc
      (setq vc-handled-backends nil)
      (remove-hook 'find-file-hook #'vc-refresh-state))))

;; 1) Make a GitHub token available early so init-vendor sees it.
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

;; 2) Optional: If Termux is installed and readable, bridge its bin to PATH/exec-path.
;; Note: Native Android Emacs usually cannot read another app’s private dir,
;; so this typically won’t change anything; it’s best-effort and harmless.
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