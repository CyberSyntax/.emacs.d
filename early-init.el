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
  (defvar my/android-fonts-src-dir (expand-file-name "fonts" user-emacs-directory)
    "Source dir inside repo that holds font files.")
  (defvar my/android-fonts-dst-dir (expand-file-name "~/fonts")
    "Destination dir Emacs-Android scans for fonts.")
  (defvar my/android-fonts-extensions '("ttf" "otf" "ttc" "otc")
    "Font file extensions to sync.")

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

  (defun my/android-sync-fonts (&optional quiet)
    "Copy fonts from repo fonts to Emacs internal ~/fonts if newer or missing.
Returns the number of files copied."
    (interactive)
    (let* ((src my/android-fonts-src-dir)
           (dst my/android-fonts-dst-dir)
           (copied 0))
      (when (file-directory-p src)
        (make-directory dst t)
        (dolist (f (directory-files-recursively src "."))
          (when (my/android--font-file-p f)
            (let* ((rel (file-relative-name f src))
                   (out (expand-file-name rel dst)))
              (make-directory (file-name-directory out) t)
              (when (my/android--newer-or-missing-p f out)
                (condition-case e
                    (progn
                      (copy-file f out t t t) ;; overwrite, preserve times/modes
                      (cl-incf copied))
                  (error (unless quiet
                           (message "[fonts] copy failed %s -> %s: %s"
                                    f out (error-message-string e)))))))))
        (unless quiet
          (message "[fonts] synced %d file(s) from %s → %s" copied src dst)))
      copied))

  ;; Run very early so families exist before cnfonts/init-ui touch fonts
  (let ((n (my/android-sync-fonts t)))
    (when (and n (> n 0))
      (ignore-errors (font-family-list))))) ;; nudge font discovery

;;; early-init.el ends here