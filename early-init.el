;;; early-init.el --- Android environment tweaks (no Termux dependency) -*- lexical-binding: t; -*-

(require 'cl-lib)

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