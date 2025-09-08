;;; lisp/init-vendor.el --- Enhanced vendor manager (offline-aware) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'url)
(require 'url-parse)

;; Keep network waits short
(defvar my/vendor-url-timeout 6
  "Seconds to wait on each GitHub API/raw request.")

(defvar my-vendor-autonomous-config
  '((cache-file . "vendor-cache.json")
    (vendor-subdir . "lisp/vendor")
    (compile-cache-subdir . "compiled-cache")
    (repositories . (;; Third-party vendor packages
                     ("https://github.com/bohonghuang/org-srs.git" . "org-srs")
                     ("https://github.com/open-spaced-repetition/lisp-fsrs.git" . "fsrs")
                     ;; My GitHub packages
                     ("https://github.com/CyberSyntax/org-queue.git" . "org-queue")
                     ("https://github.com/CyberSyntax/org-story.git" . "org-story")
                     ("https://github.com/CyberSyntax/hanja-reading.git" . "hanja-reading")
                     ("https://github.com/CyberSyntax/org-headline-manager.git" . "org-headline-manager")
                     ("https://github.com/CyberSyntax/emacs-android-support-module.git" . "android-support-module")))
    (update-frequency . daily)
    (auto-compile . t)
    (compile-strategy . smart)
    (fix-lexical-binding . t)
    (handle-dependencies . t)
    (compilation-warnings . moderate))
  "Enhanced vendor configuration with dependency handling.")

(defvar my-vendor-runtime-data nil
  "Runtime data discovered about the environment.")

(defun my-vendor--json-parse-string (str)
  "Parse JSON STR, returning alist."
  (if (fboundp 'json-parse-string)
      (json-parse-string str :object-type 'alist :array-type 'list)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-false nil)
          (json-null nil))
      (json-read-from-string str))))

(defun my-vendor-get-system-fingerprint ()
  "Generate a cross-platform system fingerprint."
  (let* ((emacs-version (format "%d.%d" emacs-major-version emacs-minor-version))
         (system-info (list
                       system-type
                       system-configuration
                       (if (boundp 'system-configuration-options)
                           system-configuration-options
                         "unknown")
                       (if (featurep 'native-compile) "native" "bytecode")
                       (if (featurep 'json) "json" "no-json")))
         (fingerprint-string (format "%s-%s" emacs-version (string-join (mapcar #'prin1-to-string system-info) "-"))))
    (let ((hash (secure-hash 'sha256 fingerprint-string)))
      (format "%s-%s" emacs-version (substring hash 0 12)))))

(defun my-vendor-ensure-lexical-binding (file-path)
  "Ensure FILE-PATH has proper lexical-binding header."
  (when (and (cdr (assoc 'fix-lexical-binding my-vendor-autonomous-config))
             (string-suffix-p ".el" file-path)
             (file-writable-p file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      (unless (and (looking-at "^;;")
                   (save-excursion
                     (re-search-forward "lexical-binding:" (line-end-position) t)))
        (goto-char (point-min))
        (if (looking-at "^;;")
            (progn
              (end-of-line)
              (insert "\n;;; -*- lexical-binding: t; -*-"))
          (insert ";;; -*- lexical-binding: t; -*-\n"))
        (write-region (point-min) (point-max) file-path nil 'silent)
        t))))

(defun my-vendor-analyze-dependencies (el-file)
  "Analyze dependencies in EL-FILE and return list of required features."
  (let ((deps '()))
    (with-temp-buffer
      (condition-case nil
          (progn
            (insert-file-contents el-file nil 0 (* 10 1024))
            (goto-char (point-min))
            (while (re-search-forward "(\\s-*require\\s-+'\\([^)\\s-]+\\)" nil t)
              (let ((feature (match-string 1)))
                (unless (or (string-prefix-p "cl-" feature)
                            (member feature '("org" "emacs")))
                  (push feature deps))))
            (goto-char (point-min))
            (while (re-search-forward ";;;###autoload\\|;;; Code:\\|;;; Commentary:" nil t)
              (forward-line 1)
              (when (re-search-forward "(\\s-*require\\s-+'\\([^)\\s-]+\\)" (line-end-position 3) t)
                (let ((feature (match-string 1)))
                  (unless (member feature deps)
                    (push feature deps))))))
        (error nil)))
    deps))

(defun my-vendor-get-package-files (repo-dir)
  "Get all .el files in REPO-DIR with dependency analysis."
  (let ((files '()))
    (when (file-directory-p repo-dir)
      (dolist (file (directory-files-recursively repo-dir "\\.el\\'"))
        (unless (string-match-p "/\\." (file-name-nondirectory file))
          (let ((deps (my-vendor-analyze-dependencies file)))
            (push (list file deps) files)))))
    (sort files (lambda (a b) (< (length (cadr a)) (length (cadr b)))))))

(defun my-vendor-get-compile-cache-dir (repo-name)
  "Get compilation cache directory for REPO-NAME."
  (let* ((config my-vendor-autonomous-config)
         (vendor-dir (expand-file-name 
                      (cdr (assoc 'vendor-subdir config))
                      user-emacs-directory))
         (cache-subdir (cdr (assoc 'compile-cache-subdir config)))
         (fingerprint (my-vendor-get-system-fingerprint)))
    (expand-file-name 
     (format "%s/%s/%s" cache-subdir fingerprint repo-name)
     vendor-dir)))

(defun my-vendor-setup-compilation-environment (repo-dir cache-dir)
  "Set up proper compilation environment for the package."
  (let ((load-path-additions '()))
    (push repo-dir load-path-additions)
    (push cache-dir load-path-additions)
    (let* ((config my-vendor-autonomous-config)
           (vendor-dir (expand-file-name 
                        (cdr (assoc 'vendor-subdir config))
                        user-emacs-directory)))
      (dolist (repo (cdr (assoc 'repositories config)))
        (let ((other-repo-dir (expand-file-name (cdr repo) vendor-dir)))
          (when (and (file-directory-p other-repo-dir)
                     (not (string= other-repo-dir repo-dir)))
            (push other-repo-dir load-path-additions))))
      (dolist (repo (cdr (assoc 'repositories config)))
        (let ((other-cache-dir (my-vendor-get-compile-cache-dir (cdr repo))))
          (when (and (file-directory-p other-cache-dir)
                     (not (string= other-cache-dir cache-dir)))
            (push other-cache-dir load-path-additions)))))
    load-path-additions))

(defun my-vendor-needs-compilation-p (source-dir cache-dir)
  "Check if SOURCE-DIR needs compilation compared to CACHE-DIR."
  (or (not (file-directory-p cache-dir))
      (let ((source-newest 0)
            (cache-newest 0)
            (cache-files-exist nil))
        (dolist (file (directory-files-recursively source-dir "\\.el\\'"))
          (let ((mtime (float-time (nth 5 (file-attributes file)))))
            (when (> mtime source-newest)
              (setq source-newest mtime))))
        (when (file-directory-p cache-dir)
          (dolist (file (directory-files-recursively cache-dir "\\.elc\\'"))
            (setq cache-files-exist t)
            (let ((mtime (float-time (nth 5 (file-attributes file)))))
              (when (> mtime cache-newest)
                (setq cache-newest mtime)))))
        (or (not cache-files-exist)
            (> source-newest cache-newest)))))

(defun my-vendor-compile-package-smart (repo-name source-dir)
  "Smart compilation with dependency resolution."
  (let* ((cache-dir (my-vendor-get-compile-cache-dir repo-name))
         (config my-vendor-autonomous-config)
         (auto-compile (cdr (assoc 'auto-compile config)))
         (strategy (cdr (assoc 'compile-strategy config)))
         (warning-level (cdr (assoc 'compilation-warnings config))))
    (unless auto-compile
      (message "Auto-compilation disabled, using source files for %s" repo-name)
      (cl-return-from my-vendor-compile-package-smart source-dir))
    (message "Smart compilation for %s..." repo-name)
    (message "  Source: %s" source-dir)
    (message "  Cache:  %s" cache-dir)
    (let ((needs-compile (pcase strategy
                           ('always t)
                           ('never nil)
                           ('smart (my-vendor-needs-compilation-p source-dir cache-dir))
                           (_ t))))
      (if (not needs-compile)
          (progn
            (message "  ✓ Using cached compilation")
            cache-dir)
        (condition-case err
            (let ((original-load-path load-path)
                  (compiled-count 0)
                  (error-count 0)
                  (warning-count 0))
              (unless (file-directory-p cache-dir)
                (make-directory cache-dir t))
              (let ((load-path-additions (my-vendor-setup-compilation-environment source-dir cache-dir)))
                (dolist (addition load-path-additions)
                  (add-to-list 'load-path addition)))
              (unwind-protect
                  (let ((file-info-list (my-vendor-get-package-files source-dir)))
                    (message "    Found %d files to compile" (length file-info-list))
                    (dolist (file-info file-info-list)
                      (let* ((el-file (car file-info))
                             (relative-path (file-relative-name el-file source-dir))
                             (target-el (expand-file-name relative-path cache-dir))
                             (target-dir (file-name-directory target-el)))
                        (unless (file-directory-p target-dir)
                          (make-directory target-dir t))
                        (copy-file el-file target-el t)
                        (when (my-vendor-ensure-lexical-binding target-el)
                          (message "    Fixed lexical-binding in %s" relative-path))
                        (condition-case compile-err
                            (let ((byte-compile-error-on-warn (eq warning-level 'strict))
                                  (byte-compile-warnings 
                                   (pcase warning-level
                                     ('none nil)
                                     ('moderate '(obsolete))
                                     ('strict t)
                                     (_ '(obsolete))))
                                  (byte-compile-log-buffer-name "*Vendor Compile Log*"))
                              (if (byte-compile-file target-el)
                                  (progn
                                    (setq compiled-count (1+ compiled-count))
                                    (message "    ✓ %s" relative-path))
                                (progn
                                  (setq warning-count (1+ warning-count))
                                  (message "    ⚠ %s (warnings)" relative-path))))
                          (error
                           (setq error-count (1+ error-count))
                           (message "    ✗ %s: %s" relative-path (error-message-string compile-err)))))))
                (setq load-path original-load-path))
              (message "  ✓ Compilation complete: %d compiled, %d warnings, %d errors" 
                       compiled-count warning-count error-count)
              (if (> compiled-count 0) cache-dir source-dir))
          (error
           (message "  ✗ Smart compilation failed: %s" (error-message-string err))
           (message "  → Falling back to source directory")
           source-dir))))))

(defun my-vendor-probe-capability (test-name test-function)
  "Probe a capability using TEST-FUNCTION, cache result under TEST-NAME."
  (or (cdr (assoc test-name my-vendor-runtime-data))
      (let ((result (condition-case nil
                        (funcall test-function)
                      (error nil))))
        (push (cons test-name result) my-vendor-runtime-data)
        result)))

(defun my-vendor-discover-environment ()
  "Discover all environment capabilities dynamically."
  (list
   (cons 'git-functional
         (my-vendor-probe-capability 'git-functional
           (lambda ()
             (and (executable-find "git")
                  (zerop (with-temp-buffer
                           (call-process "git" nil t nil "--version")))))))
   (cons 'network-available
         (my-vendor-probe-capability 'network-available
           (lambda ()
             (condition-case nil
                 (with-temp-buffer
                   (let ((url-request-timeout my/vendor-url-timeout))
                     (url-insert-file-contents "https://api.github.com" nil 0 64)
                     t))
               (error nil)))))
   (cons 'filesystem-writable
         (my-vendor-probe-capability 'filesystem-writable
           (lambda ()
             (let ((test-dir (make-temp-file "emacs-fs-test" t)))
               (unwind-protect
                   (and (file-directory-p test-dir)
                        (file-writable-p test-dir))
                 (ignore-errors
                   (when (file-exists-p test-dir)
                     (delete-directory test-dir t))))))))
   (cons 'compilation-available
         (my-vendor-probe-capability 'compilation-available
           (lambda () (fboundp 'byte-compile-file))))))

(defun my-vendor-select-strategy ()
  "Dynamically select the best strategy based on discovered capabilities."
  (let ((env-data (my-vendor-discover-environment)))
    (let ((git-ok (cdr (assoc 'git-functional env-data)))
          (network-ok (cdr (assoc 'network-available env-data)))
          (fs-writable (cdr (assoc 'filesystem-writable env-data))))
      (cond
       ((and git-ok network-ok fs-writable) 'git-update)
       ((and (eq system-type 'android) network-ok fs-writable) 'download-raw)
       ((not fs-writable) 'readonly)
       (t 'fallback)))))

(defun my-vendor-should-update-p ()
  "Check if we should update today."
  (let* ((config my-vendor-autonomous-config)
         (cache-file (expand-file-name 
                      (cdr (assoc 'cache-file config))
                      temporary-file-directory))
         (today (format-time-string "%Y-%m-%d")))
    (if (file-exists-p cache-file)
        (condition-case nil
            (let ((last-update (with-temp-buffer
                                 (insert-file-contents cache-file)
                                 (string-trim (buffer-string)))))
              (not (string= last-update today)))
          (error t))
      t)))

(defun my-vendor-mark-updated ()
  "Mark that we updated today."
  (let* ((config my-vendor-autonomous-config)
         (cache-file (expand-file-name 
                      (cdr (assoc 'cache-file config))
                      temporary-file-directory))
         (today (format-time-string "%Y-%m-%d")))
  (condition-case nil
      (with-temp-file cache-file
        (insert today))
    (error nil))))

(defun my-vendor-execute-git-update (repo-url repo-dir)
  "Execute git operation for REPO-URL into REPO-DIR."
  (let ((success nil))
    (condition-case err
        (progn
          (if (file-directory-p repo-dir)
              (let ((default-directory repo-dir))
                (message "Updating repo in %s..." repo-dir)
                (setq success (zerop (call-process "git" nil nil nil "pull" "--ff-only")))
                (when success
                  (message "  ✓ Successfully updated %s" repo-dir)))
            (message "Cloning %s to %s..." repo-url repo-dir)
            (setq success (zerop (call-process "git" nil nil nil "clone" repo-url repo-dir)))
            (when success
              (message "  ✓ Successfully cloned to %s" repo-dir)))
          success)
      (error 
       (message "  ✗ Git operation failed for %s: %s" repo-url (error-message-string err))
       nil))))

(defun my-vendor-extract-owner-repo (repo-url)
  "Extract 'owner/repo' from REPO-URL."
  (let ((clean-url (replace-regexp-in-string "\\.git$" "" repo-url)))
    (if (string-match "github\\.com/\\(.*\\)" clean-url)
        (match-string 1 clean-url)
      (error "Invalid GitHub URL: %s" repo-url))))

(defun my-vendor-fetch-latest-commit-sha (owner-repo)
  "Fetch the latest commit SHA for OWNER-REPO's main branch from GitHub API."
  (let* ((api-url (format "https://api.github.com/repos/%s/commits/main" owner-repo))
         (buf (let ((url-request-timeout my/vendor-url-timeout))
                (url-retrieve-synchronously api-url t t)))
         (json nil))
    (when buf
      (with-current-buffer buf
        (goto-char (point-min))
        (when (re-search-forward "^\r?\n\r?\n" nil t)
          (setq json (my-vendor--json-parse-string
                      (buffer-substring-no-properties (point) (point-max))))))
      (kill-buffer buf))
    (if json
        (alist-get 'sha json)
      (error "Failed to fetch commit SHA for %s (rate limit or network issue?)" owner-repo))))

(defun my-vendor-get-local-sha (repo-dir)
  "Get the stored local SHA from REPO-DIR's last-sha.txt, or nil if missing."
  (let ((sha-file (expand-file-name "last-sha.txt" repo-dir)))
    (when (file-exists-p sha-file)
      (with-temp-buffer
        (insert-file-contents sha-file)
        (string-trim (buffer-string))))))

(defun my-vendor-update-local-sha (repo-dir new-sha)
  "Update the last-sha.txt in REPO-DIR with NEW-SHA."
  (let ((sha-file (expand-file-name "last-sha.txt" repo-dir)))
    (with-temp-file sha-file
      (insert new-sha))))

(defun my-vendor-fetch-github-tree (owner-repo)
  "Fetch recursive file tree from GitHub API for OWNER-REPO's main branch."
  (let* ((api-url (format "https://api.github.com/repos/%s/git/trees/main?recursive=1" owner-repo))
         (buf (let ((url-request-timeout my/vendor-url-timeout))
                (url-retrieve-synchronously api-url t t)))
         (json nil))
    (when buf
      (with-current-buffer buf
        (goto-char (point-min))
        (when (re-search-forward "^\r?\n\r?\n" nil t)
          (setq json (my-vendor--json-parse-string
                      (buffer-substring-no-properties (point) (point-max))))))
      (kill-buffer buf))
    (if (and json (alist-get 'tree json))
        (alist-get 'tree json)
      (error "Failed to fetch tree for %s (rate limit?)" owner-repo))))

(defun my-vendor-download-raw-file (owner-repo path local-file)
  "Download raw file content from GitHub for OWNER-REPO's PATH to LOCAL-FILE."
  (let ((raw-url (format "https://raw.githubusercontent.com/%s/main/%s" owner-repo path)))
    (with-temp-buffer
      (let ((url-request-timeout my/vendor-url-timeout))
        (condition-case err
            (progn
              (url-insert-file-contents raw-url)
              (write-region (point-min) (point-max) local-file nil 'silent)
              t)
          (error
           (message "  ✗ Failed to download %s: %s" path (error-message-string err))
           nil))))))

(defun my-vendor-execute-raw-download (repo-url repo-dir)
  "Download raw files for REPO-URL into REPO-DIR if validation shows an update is needed."
  (let* ((owner-repo (my-vendor-extract-owner-repo repo-url))
         (local-sha (my-vendor-get-local-sha repo-dir))
         (remote-sha (condition-case err
                         (my-vendor-fetch-latest-commit-sha owner-repo)
                       (error
                        (message "  ✗ SHA fetch failed for %s: %s - Skipping update" owner-repo (error-message-string err))
                        nil))))
    (if (and remote-sha
             (or (null local-sha) (not (string= local-sha remote-sha))))
        (condition-case err
            (progn
              (when (file-directory-p repo-dir)
                (delete-directory repo-dir t t)
                (message "  Deleted old repo: %s" repo-dir))
              (message "Fetching file tree for %s..." owner-repo)
              (let ((tree (my-vendor-fetch-github-tree owner-repo))
                    (file-count 0))
                (dolist (item tree)
                  (let ((path (alist-get 'path item))
                        (type (alist-get 'type item)))
                    (cond
                     ((string= type "tree")
                      (make-directory (expand-file-name path repo-dir) t))
                     ((string= type "blob")
                      (let ((local-file (expand-file-name path repo-dir)))
                        (when (my-vendor-download-raw-file owner-repo path local-file)
                          (setq file-count (1+ file-count))))))))
                (if (> file-count 0)
                    (progn
                      (my-vendor-update-local-sha repo-dir remote-sha)
                      (message "  ✓ Downloaded %d files to %s (updated to SHA %s)" file-count repo-dir (substring remote-sha 0 7))
                      t)
                  (message "  ✗ No files downloaded for %s" owner-repo)
                  nil)))
          (error 
           (message "  ✗ Raw download failed for %s: %s" owner-repo (error-message-string err))
           nil))
      (progn
        (message "  ✓ Skipping download for %s—already up to date (SHA %s)" owner-repo (if local-sha (substring local-sha 0 7) "none"))
        t))))

(defun my-vendor-autonomous-setup ()
  "Enhanced autonomous vendor package setup with smart compilation."
  (let* ((config my-vendor-autonomous-config)
         (strategy (my-vendor-select-strategy))
         (vendor-dir (expand-file-name 
                      (cdr (assoc 'vendor-subdir config))
                      user-emacs-directory))
         (repositories (cdr (assoc 'repositories config)))
         (should-update (my-vendor-should-update-p))
         (updated-any nil)
         (fingerprint (my-vendor-get-system-fingerprint)))
    
    (message "=== Enhanced Cross-Platform Vendor Setup ===")
    (message "Platform fingerprint: %s" fingerprint)
    (message "Vendor directory: %s" vendor-dir)
    (message "Strategy: %s" strategy)
    (message "Should update: %s" (if should-update "YES" "NO"))
    
    (condition-case nil
        (unless (file-directory-p vendor-dir)
          (make-directory vendor-dir t))
      (error 
       (setq strategy 'readonly)))
    
    (pcase strategy
      ('git-update
       (if should-update
           (progn
             (dolist (repo repositories)
               (let* ((url (car repo))
                      (name (cdr repo))
                      (repo-dir (expand-file-name name vendor-dir)))
                 (when (my-vendor-execute-git-update url repo-dir)
                   (setq updated-any t))))
             (when updated-any (my-vendor-mark-updated)))
         (message "Skipping update - already done today")))
      ('download-raw
       (if should-update
           (progn
             (dolist (repo repositories)
               (let* ((url (car repo))
                      (name (cdr repo))
                      (repo-dir (expand-file-name name vendor-dir)))
                 (when (my-vendor-execute-raw-download url repo-dir)
                   (setq updated-any t))))
             (when updated-any (my-vendor-mark-updated)))
         (message "Skipping update - already done today")))
      ('readonly (message "Read-only mode"))
      (_ (message "Fallback mode")))
    
    (let ((added-paths 0))
      (message "Smart compilation and load-path setup:")
      (dolist (repo repositories)
        (let* ((name (cdr repo))
               (source-dir (expand-file-name name vendor-dir)))
          (if (file-directory-p source-dir)
              (let ((final-dir (my-vendor-compile-package-smart name source-dir)))
                (add-to-list 'load-path final-dir)
                (setq added-paths (1+ added-paths))
                (message "  ✓ %s -> %s" name 
                         (if (string= final-dir source-dir) "SOURCE" "COMPILED")))
            (message "  ✗ %s (missing)" name))))
      (message "=== Cross-Platform Setup Complete: %d packages ===" added-paths))))

;; Execute setup (safe even when offline; will pick readonly/fallback as needed)
(my-vendor-autonomous-setup)

(defun my-vendor-status ()
  "Show detailed cross-platform status."
  (interactive)
  (let* ((config my-vendor-autonomous-config)
         (fingerprint (my-vendor-get-system-fingerprint)))
    (message "=== Cross-Platform Vendor Status ===")
    (message "Platform: %s" fingerprint)
    (message "Lexical binding fix: %s" (if (cdr (assoc 'fix-lexical-binding config)) "ON" "OFF"))
    (message "Dependency handling: %s" (if (cdr (assoc 'handle-dependencies config)) "ON" "OFF"))
    (message "Warning level: %s" (cdr (assoc 'compilation-warnings config)))
    (my-vendor-discover-environment)
    (message "Capabilities: %S" my-vendor-runtime-data)))

(defun my-vendor-toggle-compilation ()
  "Toggle auto-compilation on/off."
  (interactive)
  (let ((current (cdr (assoc 'auto-compile my-vendor-autonomous-config))))
    (setcdr (assoc 'auto-compile my-vendor-autonomous-config) (not current))
    (message "Auto-compilation: %s" (if (not current) "ENABLED" "DISABLED"))))

(defun my-vendor-force-recompile ()
  "Force complete recompilation."
  (interactive)
  (my-vendor-clean-cache)
  (my-vendor-autonomous-setup))

(defun my-vendor-clean-cache ()
  "Clean compilation cache."
  (interactive)
  (let* ((config my-vendor-autonomous-config)
         (vendor-dir (expand-file-name 
                      (cdr (assoc 'vendor-subdir config))
                      user-emacs-directory))
         (cache-base-dir (expand-file-name 
                          (cdr (assoc 'compile-cache-subdir config))
                          vendor-dir)))
    (when (file-directory-p cache-base-dir)
      (delete-directory cache-base-dir t)
      (message "Cache cleaned: %s" cache-base-dir))))

(provide 'init-vendor)

;;; lisp/init-vendor.el ends here