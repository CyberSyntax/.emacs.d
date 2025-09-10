;;; early-init.el --- Android environment tweaks (no Termux dependency) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

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

;; 3) Android: auto-sync fonts from repo → Emacs internal ~/fonts
(when (eq system-type 'android)
  (defvar my/android-fonts-src-dir (expand-file-name "fonts" user-emacs-directory))
  (defvar my/android-legacy-fonts-dir (expand-file-name "~/.emacs.d/fonts"))
  (defvar my/android-fonts-dst-dir (expand-file-name "~/fonts"))
  (defvar my/android-fonts-extensions '("ttf" "otf" "ttc" "otc"))

  (defun my/android--font-file-p (path)
    (and (file-regular-p path)
         (member (downcase (file-name-extension path ""))
                 my/android-fonts-extensions)))

  (defun my/android--newer-or-missing-p (src dst)
    (or (not (file-exists-p dst))
        (let* ((as (file-attributes src))
               (ad (file-attributes dst))
               (ms (file-attribute-modification-time as))
               (md (file-attribute-modification-time ad))
               (ss (file-attribute-size as))
               (sd (file-attribute-size ad)))
          (or (time-less-p md ms) (not (= ss sd))))))

  (defun my/android--collect-fonts (dir)
    (when (file-directory-p dir)
      (directory-files-recursively dir "\\.\\(ttf\\|otf\\|ttc\\|otc\\)\\'")))

  (defun my/android-sync-fonts (&optional quiet)
    "Copy all fonts from repo fonts/ and legacy ~/.emacs.d/fonts/ into ~/fonts.
- Safe copy flags for Android (preserve uid/gid = nil).
- If both TTF and OTF exist for same basename, TTF wins.
Returns number of files copied."
    (interactive)
    (let* ((dst my/android-fonts-dst-dir)
           (srcs (append (my/android--collect-fonts my/android-fonts-src-dir)
                         (my/android--collect-fonts my/android-legacy-fonts-dir)))
           (by-base (make-hash-table :test 'equal))
           (copied 0))
      ;; Index by base name without extension; prefer TTF if seen.
      (dolist (f srcs)
        (let* ((bn (file-name-base f))
               (ext (downcase (file-name-extension f "")))
               (key bn)
               (cur (gethash key by-base)))
          (cond
           ((and (string= ext "ttf"))
            (puthash key f by-base))                  ;; TTF always overrides prior
           ((null cur))                               ;; no prior, accept
           ((and cur (not (string= (downcase (file-name-extension cur "")) "ttf")))
            (puthash key f by-base)))))               ;; overwrite only if not TTF
      (make-directory dst t)
      (maphash
       (lambda (_k src)
         (let* ((rel (file-name-nondirectory src))
                (out (expand-file-name rel dst)))
           (make-directory (file-name-directory out) t)
           (when (my/android--newer-or-missing-p src out)
             (condition-case e
                 (progn
                   ;; OK-IF-EXISTS=t, KEEP-TIME=t, PRESERVE-UID/GID=nil (Android-safe)
                   (copy-file src out t t nil)
                   (cl-incf copied))
               (error (unless quiet
                        (message "[fonts] copy failed %s -> %s: %s"
                                 src out (error-message-string e))))))))
       by-base)
      (unless quiet
        (message "[fonts] synced %d file(s) → %s" copied dst))
      copied))

  ;; Run very early so families exist before cnfonts/init-ui touch fonts
  (let ((n (my/android-sync-fonts t)))
    (when (and n (> n 0))
      (ignore-errors (font-family-list))))) ;; nudge font discovery

;;; early-init.el ends here