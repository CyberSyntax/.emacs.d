;;; lisp/init-vendor.el --- Strict branch, codeload-first archives, no-SHA raw fallback -*- lexical-binding: t; -*-
;;
;; Cross-platform vendor manager:
;; - Prefers archive downloads (pure Emacs; no external tools) on platforms without git.
;; - Tries codeload first for explicit branches (public repos often succeed there fast).
;; - Strict branch selection: if override present, do NOT guess master.
;; - Raw fallback does not require commit SHA (downloads by API tree if available).
;;
;; Commands:
;;   M-x my-vendor-update-now
;;   M-x my-vendor-status
;;   M-x my-vendor-clean-cache
;;   M-x my-vendor-reset-daily-throttle
;;
;; It honors $GITHUB_TOKEN for api.github.com (not for github.com/codeload).

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'url)
(require 'url-parse)

(unless (bound-and-true-p auto-compression-mode)
  (auto-compression-mode 1))

(defvar my-vendor-user-agent
  (format "Emacs/%s (%s) my-vendor" emacs-version system-type))
(defvar my-vendor-gh-token
  (or (getenv "GITHUB_TOKEN") (getenv "GH_TOKEN"))
  "Personal token for api.github.com to raise rate limits and access private repos.")

(defvar my/vendor-url-timeout 120
  "Seconds to wait on HTTP(S) requests.")
(setq url-request-timeout my/vendor-url-timeout)

;; --------------------------------------------------------------------
;; Main configuration (your exact lists kept)
;; --------------------------------------------------------------------

(defvar my-vendor-autonomous-config
  '((cache-file . "vendor-cache.json")
    (vendor-subdir . "lisp/vendor")
    (compile-cache-subdir . "compiled-cache")
    (repositories .
                  (;; Third-party vendor packages
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
  "Vendor configuration.")

(defvar my-vendor-branch-overrides
  '(("bohonghuang/org-srs" . "master")
    ("open-spaced-repetition/lisp-fsrs" . "master")
    ("CyberSyntax/org-queue" . "main")
    ("CyberSyntax/org-story" . "main")
    ("CyberSyntax/hanja-reading" . "main")
    ("CyberSyntax/org-headline-manager" . "main")
    ("CyberSyntax/emacs-android-support-module" . "main"))
  "Owner/repo -> preferred branch to try first.")

;; Diagnostics verbosity
(defvar my-vendor-log-branches t)

;; --------------------------------------------------------------------
;; Utilities
;; --------------------------------------------------------------------

(defun my-vendor--json-parse-string (str)
  (if (fboundp 'json-parse-string)
      (json-parse-string str :object-type 'alist :array-type 'list)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-false nil)
          (json-null nil))
      (json-read-from-string str))))

(defun my-vendor-get-system-fingerprint ()
  (let* ((em (format "%d.%d" emacs-major-version emacs-minor-version))
         (info (list system-type system-configuration
                     (if (boundp 'system-configuration-options)
                         system-configuration-options "unknown")
                     (if (featurep 'native-compile) "native" "bytecode")
                     (if (featurep 'json) "json" "no-json")))
         (s (format "%s-%s" em (string-join (mapcar #'prin1-to-string info) "-"))))
    (format "%s-%s" em (substring (secure-hash 'sha256 s) 0 12))))

(defun my-vendor-extract-owner-repo (repo-url)
  (let* ((clean (replace-regexp-in-string "\\.git\\'" "" repo-url))
         (m (and (string-match "github\\.com/\\([^?#]+\\)" clean)
                 (match-string 1 clean))))
    (unless m (error "Invalid GitHub URL: %s" repo-url))
    (replace-regexp-in-string "/\\'" "" m)))

(defun my-vendor--http-get-json (url with-auth)
  (let* ((url-request-extra-headers
          (append
           `(("User-Agent" . ,my-vendor-user-agent)
             ("Accept" . "application/vnd.github+json"))
           (when (and with-auth my-vendor-gh-token (not (string-empty-p my-vendor-gh-token)))
             (list (cons "Authorization" (format "Bearer %s" my-vendor-gh-token))))
           url-request-extra-headers))
         (url-request-timeout my/vendor-url-timeout)
         (buf (url-retrieve-synchronously url t t))
         (result nil))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (let ((code (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                          (string-to-number (match-string 1)))))
              (when (re-search-forward "^\r?\n\r?\n" nil t)
                (setq result
                      (cons code
                            (ignore-errors
                              (my-vendor--json-parse-string
                               (buffer-substring-no-properties (point) (point-max)))))))))
        (kill-buffer buf)))
    result))

(defun my-vendor--github-api-get-json (url)
  (let ((resp (my-vendor--http-get-json url t)))
    (when (and resp (memq (car resp) '(401 403)))
      (setq resp (my-vendor--http-get-json url nil)))
    resp))

(defun my-vendor-fetch-default-branch (owner-repo)
  (let* ((api-url (format "https://api.github.com/repos/%s" owner-repo))
         (resp (my-vendor--github-api-get-json api-url))
         (json (cdr-safe resp)))
    (or (and json (alist-get 'default_branch json))
        nil)))

;; Strict branch selection:
;; - If override exists -> only that.
;; - Else, if API default exists -> only that.
;; - Else -> guesses "main", "master".
(defun my-vendor--candidate-branches (owner-repo)
  (let* ((override   (alist-get owner-repo my-vendor-branch-overrides nil nil #'string=))
         (def-branch (ignore-errors (my-vendor-fetch-default-branch owner-repo))))
    (cond
     (override   (list override))
     (def-branch (list def-branch))
     (t          '("main" "master")))))

(defun my-vendor--log-branches (owner-repo branches)
  (when my-vendor-log-branches
    (message "Branches to try for %s: %s"
             owner-repo (mapconcat #'identity branches ", "))))

(defun my-vendor-get-local-sha (repo-dir)
  (let ((sha-file (expand-file-name "last-sha.txt" repo-dir)))
    (when (file-exists-p sha-file)
      (with-temp-buffer
        (insert-file-contents sha-file)
        (string-trim (buffer-string))))))

(defun my-vendor-update-local-sha (repo-dir new-sha)
  (let ((sha-file (expand-file-name "last-sha.txt" repo-dir)))
    (with-temp-file sha-file
      (insert new-sha))))

;; --------------------------------------------------------------------
;; Download helpers
;; --------------------------------------------------------------------

(defun my-vendor--download-to-file (url file &optional redirects-left)
  "Download URL to FILE; follow redirects. Auth only for api.github.com."
  (let* ((redirects-left (or redirects-left 4))
         (uobj (ignore-errors (url-generic-parse-url url)))
         (host (downcase (or (and uobj (url-host uobj)) "")))
         (is-api-host (string= host "api.github.com"))
         (url-request-extra-headers
          (append
           '(("User-Agent" . "Emacs my-vendor"))
           (when (and is-api-host my-vendor-gh-token (not (string-empty-p my-vendor-gh-token)))
             (list (cons "Authorization" (format "Bearer %s" my-vendor-gh-token))))
           url-request-extra-headers))
         (url-request-timeout my/vendor-url-timeout)
         (result nil))
    (condition-case e
        (let ((buf (url-retrieve-synchronously url t t)))
          (unless buf
            (message "  ✗ download failed: no response from %s" url)
            (setq result nil))
          (when buf
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (let ((case-fold-search t))
                    (if (not (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t))
                        (setq result nil)
                      (let* ((code (string-to-number (match-string 1)))
                             (loc  (progn
                                     (goto-char (point-min))
                                     (when (re-search-forward "^Location: \\(.*\\)$" nil t)
                                       (string-trim (match-string 1))))))
                        (cond
                         ((and loc (memq code '(301 302 303 307 308)))
                          (if (<= redirects-left 0)
                              (message "  ✗ redirect loop from %s" url)
                            (setq result (my-vendor--download-to-file loc file (1- redirects-left)))))
                         ((/= code 200)
                          (message "  ✗ HTTP %s from %s" code url))
                         (t
                          (when (re-search-forward "^\r?\n\r?\n" nil t)
                            (write-region (point) (point-max) file nil 'silent)
                            (setq result t))))))))
              (kill-buffer buf))))
      (error
       (message "  ✗ download failed: %s" (error-message-string e))
       (setq result nil)))
    result))

(defun my-vendor--trim-nul (s)
  (if (string-match "\0" s) (substring s 0 (match-beginning 0)) s))

(defun my-vendor--parse-octal (s)
  (let* ((t0 (replace-regexp-in-string "[\0 ].*$" "" s)))
    (if (string-empty-p t0) 0 (string-to-number t0 8))))

(defun my-vendor--tar-header-valid-p (file)
  "Quick sanity: does FILE look like a tar after decompression?"
  (with-temp-buffer
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents file nil 0 512))
    (let* ((blk (buffer-substring-no-properties (point-min)
                                                (min (point-max) (+ (point-min) 512))))
           (magic (and (>= (length blk) 262) (substring blk 257 262))))
      (string= magic "ustar"))))

(defun my-vendor--tar-peek-topdir (tarfile)
  "Return top-level dir inside TARFILE (string without trailing slash), or nil."
  (with-temp-buffer
    (setq buffer-file-coding-system 'no-conversion)
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents tarfile))
    (let ((pos (point-min))
          (end (point-max)))
      (when (<= (+ pos 512) end)
        (let ((block (buffer-substring-no-properties pos (+ pos 512))))
          (unless (string-match-p "\\`\\(\0\\{512\\}\\)\\'" block)
            (let* ((name (my-vendor--trim-nul (substring block 0 100)))
                   (prefix (my-vendor--trim-nul (substring block 345 500)))
                   (full (if (string-empty-p prefix) name (concat prefix "/" name))))
              (car (split-string full "/" t)))))))))

(defun my-vendor--tar-extract (tarfile dest &optional strip-components only-under)
  "Extract TARFILE into DEST (pure elisp)."
  (setq dest (file-name-as-directory dest))
  (make-directory dest t)
  (with-temp-buffer
    (setq buffer-file-coding-system 'no-conversion)
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents tarfile))
    (let ((pos (point-min))
          (end (point-max)))
      (cl-labels
          ((blk (o l) (buffer-substring-no-properties o (+ o l)))
           (nul-p (s) (string-match-p "\\`\\(\0+\\)\\'" s))
           (field (b off len) (my-vendor--trim-nul (substring b off (+ off len))))
           (oct (b off len) (my-vendor--parse-octal (substring b off (+ off len)))))
        (while (<= (+ pos 512) end)
          (let ((block (blk pos 512)))
            (if (nul-p block)
                (setq pos (+ pos 512))
              (let* ((name   (field block 0 100))
                     (size   (oct   block 124 12))
                     (type   (let ((c (aref block 156))) (if (= c 0) ?0 c)))
                     (prefix (field block 345 155))
                     (full   (if (string-empty-p prefix) name (concat prefix "/" name))))
                (when (and only-under (not (string-prefix-p (concat only-under "/") full)))
                  (setq full nil))
                (when full
                  (let* ((parts (split-string full "/" t))
                         (parts* (nthcdr (or strip-components 0) parts))
                         (rel (mapconcat #'identity parts* "/")))
                    (when (not (string-empty-p rel))
                      (let ((out (expand-file-name rel dest)))
                        (pcase type
                          (?5 (make-directory (file-name-as-directory out) t))
                          ((or ?0 48)
                           (let* ((start (+ pos 512))
                                  (endd  (+ start size)))
                             (make-directory (file-name-directory out) t)
                             (write-region start endd out nil 'silent)))
                          (_ nil))))))
                (let* ((data-size size)
                       (pad (mod (- 512 (mod data-size 512)) 512)))
                  (setq pos (+ pos 512 data-size pad))))))))))

(defun my-vendor--sha-from-topdir (topdir)
  (when (and topdir (string-match "-\\([0-9a-f]\\{7,40\\}\\)\\'" topdir))
    (match-string 1 topdir)))

(defun my-vendor--extract-archive (tarfile repo-dir owner-repo)
  (condition-case err
      (progn
        (unless (my-vendor--tar-header-valid-p tarfile)
          (error "Downloaded file is not a tar archive (server returned HTML/JSON)."))
        (let* ((top (my-vendor--tar-peek-topdir tarfile))
               (sha (or (my-vendor--sha-from-topdir top)
                        (secure-hash
                         'sha256
                         (with-temp-buffer
                           (let ((coding-system-for-read 'no-conversion))
                             (insert-file-contents tarfile))
                           (buffer-string))))))
          (when (file-directory-p repo-dir)
            (delete-directory repo-dir t t))
          (make-directory repo-dir t)
          (message "Extracting to %s..." repo-dir)
          (my-vendor--tar-extract tarfile repo-dir 1 top)
          (when sha (my-vendor-update-local-sha repo-dir sha))
          (message "  ✓ Installed %s (%s)"
                   (file-name-nondirectory repo-dir)
                   (substring sha 0 7))
          t))
    (error
     (message "  ✗ Extract failed for %s: %s" owner-repo (error-message-string err))
     nil)))

;; --------------------------------------------------------------------
;; Archive path (codeload-first for explicit branches)
;; --------------------------------------------------------------------

(defun my-vendor-execute-archive-download (repo-url repo-dir)
  "Download GitHub archive tarball and extract into REPO-DIR."
  (let* ((owner-repo (my-vendor-extract-owner-repo repo-url))
         (branches   (my-vendor--candidate-branches owner-repo))
         (ok nil))
    ;; Try API tarball without specifying a branch (default branch tarball).
    (let* ((no-branch-url (format "https://api.github.com/repos/%s/tarball" owner-repo))
           (tmp (make-temp-file "vendor-" nil ".tar.gz")))
      (message "Downloading tarball (default) from api.github.com...")
      (when (my-vendor--download-to-file no-branch-url tmp)
        (setq ok (my-vendor--extract-archive tmp repo-dir owner-repo)))
      (ignore-errors (delete-file tmp)))
    ;; Explicit branches: codeload -> github.com -> API
    (unless ok
      (my-vendor--log-branches owner-repo branches)
      (dolist (br branches)
        (unless ok
          (let* ((candidates
                  (list
                   (format "https://codeload.github.com/%s/tar.gz/refs/heads/%s" owner-repo br)
                   (format "https://github.com/%s/archive/refs/heads/%s.tar.gz" owner-repo br)
                   (format "https://api.github.com/repos/%s/tarball/%s" owner-repo br)))
                 (tmp (make-temp-file "vendor-" nil ".tar.gz"))
                 (downloaded nil))
            (dolist (u candidates)
              (unless downloaded
                (let ((host (condition-case nil
                                (url-host (url-generic-parse-url u))
                              (error u))))
                  (message "Downloading tarball (%s) from %s..." br host))
                (setq downloaded (my-vendor--download-to-file u tmp))))
            (when downloaded
              (setq ok (my-vendor--extract-archive tmp repo-dir owner-repo)))
            (ignore-errors (delete-file tmp))))))
    ok))

;; --------------------------------------------------------------------
;; Raw-tree path (no SHA requirement)
;; --------------------------------------------------------------------

(defun my-vendor-fetch-github-tree (owner-repo branch)
  "Fetch recursive file tree (alist) for OWNER-REPO@BRANCH via GitHub API."
  (let* ((api-url (format "https://api.github.com/repos/%s/git/trees/%s?recursive=1"
                          owner-repo branch))
         (resp (my-vendor--github-api-get-json api-url))
         (json (cdr-safe resp)))
    (and json (alist-get 'tree json))))

(defun my-vendor-download-raw-file (owner-repo branch path local-file)
  "Download raw file from GitHub OWNER-REPO BRANCH PATH to LOCAL-FILE."
  (let ((raw-url (format "https://raw.githubusercontent.com/%s/%s/%s" owner-repo branch path)))
    (with-temp-buffer
      (let ((url-request-extra-headers '(("User-Agent" . "Emacs my-vendor")))
            (url-request-timeout my/vendor-url-timeout))
        (condition-case err
            (progn
              (url-insert-file-contents raw-url)
              (make-directory (file-name-directory local-file) t)
              (write-region (point-min) (point-max) local-file nil 'silent)
              t)
          (error
           (message "  ✗ Failed to download %s: %s" path (error-message-string err))
           nil))))))

(defun my-vendor-execute-raw-download (repo-url repo-dir)
  "Download files via API tree + raw blobs. Does not require commit SHA."
  (let* ((owner-repo (my-vendor-extract-owner-repo repo-url))
         (branches   (my-vendor--candidate-branches owner-repo))
         (ok nil))
    (my-vendor--log-branches owner-repo branches)
    (dolist (br branches)
      (unless ok
        (message "Fetching file tree for %s (%s)..." owner-repo br)
        (let ((tree (ignore-errors (my-vendor-fetch-github-tree owner-repo br))))
          (if (not (and tree (listp tree)))
              (message "  ✗ Could not fetch tree for %s (%s)" owner-repo br)
            (when (file-directory-p repo-dir)
              (delete-directory repo-dir t t)
              (message "  Deleted old repo: %s" repo-dir))
            (make-directory repo-dir t)
            (let ((file-count 0))
              (dolist (item tree)
                (let ((path (alist-get 'path item))
                      (type (alist-get 'type item)))
                  (cond
                   ((string= type "tree")
                    (make-directory (expand-file-name path repo-dir) t))
                   ((string= type "blob")
                    (let ((local-file (expand-file-name path repo-dir)))
                      (when (my-vendor-download-raw-file owner-repo br path local-file)
                        (setq file-count (1+ file-count))))))))
              (if (> file-count 0)
                  (progn
                    (my-vendor-update-local-sha repo-dir (format "%s-%s" br (format-time-string "%Y%m%d%H%M%S")))
                    (message "  ✓ Downloaded %d files to %s" file-count repo-dir)
                    (setq ok t))
                (message "  ✗ No files downloaded for %s (%s)" owner-repo br)))))))
    ok))

;; --------------------------------------------------------------------
;; Compile/cache helpers
;; --------------------------------------------------------------------

(defun my-vendor-ensure-lexical-binding (file-path)
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
            (progn (end-of-line) (insert "\n;;; -*- lexical-binding: t; -*-"))
          (insert ";;; -*- lexical-binding: t; -*-\n"))
        (write-region (point-min) (point-max) file-path nil 'silent)
        t))))

(defun my-vendor-analyze-dependencies (el-file)
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
                  (push feature deps)))))
        (error nil)))
    deps))

(defun my-vendor-get-package-files (repo-dir)
  (let ((files '()))
    (when (file-directory-p repo-dir)
      (dolist (file (directory-files-recursively repo-dir "\\.el\\'"))
        (unless (string-match-p "/\\." (file-name-nondirectory file))
          (let ((deps (my-vendor-analyze-dependencies file)))
            (push (list file deps) files)))))
    (sort files (lambda (a b) (< (length (cadr a)) (length (cadr b)))))))

(defun my-vendor-get-compile-cache-dir (repo-name)
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
  (let* ((cache-dir (my-vendor-get-compile-cache-dir repo-name))
         (config my-vendor-autonomous-config)
         (auto-compile (cdr (assoc 'auto-compile config)))
         (warning-level (cdr (assoc 'compilation-warnings config))))
    (unless auto-compile
      (message "Auto-compilation disabled, using source files for %s" repo-name)
      (cl-return-from my-vendor-compile-package-smart source-dir))
    (message "Smart compilation for %s..." repo-name)
    (message "  Source: %s" source-dir)
    (message "  Cache:  %s" cache-dir)
    (let ((needs-compile (my-vendor-needs-compilation-p source-dir cache-dir)))
      (if (not needs-compile)
          (progn
            (message "  ✓ Using cached compilation")
            cache-dir)
        (condition-case err
            (let ((original-load-path load-path)
                  (compiled-count 0)
                  (warning-count 0)
                  (error-count 0))
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

;; --------------------------------------------------------------------
;; Strategy and orchestration
;; --------------------------------------------------------------------

(defvar my-vendor-runtime-data nil)
(defvar my-vendor-force-update nil)

(defun my-vendor-probe-capability (test-name test-function)
  (or (cdr (assoc test-name my-vendor-runtime-data))
      (let ((result (condition-case nil (funcall test-function) (error nil))))
        (push (cons test-name result) my-vendor-runtime-data)
        result)))

(defun my-vendor-discover-environment ()
  (list
   (cons 'git-functional
         (my-vendor-probe-capability 'git-functional
           (lambda ()
             (and (executable-find "git")
                  (zerop (with-temp-buffer (call-process "git" nil t nil "--version")))))))
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
  (let ((env-data (my-vendor-discover-environment)))
    (let ((git-ok      (cdr (assoc 'git-functional env-data)))
          (network-ok  (cdr (assoc 'network-available env-data)))
          (fs-writable (cdr (assoc 'filesystem-writable env-data))))
      (cond
       ((and (eq system-type 'android) network-ok fs-writable) 'download-archive)
       ((and git-ok network-ok fs-writable) 'git-update)
       ((and network-ok fs-writable) 'download-archive)
       ((not fs-writable) 'readonly)
       (t 'fallback)))))

(defun my-vendor-repo-present-p (repo-dir)
  (and (file-directory-p repo-dir)
       (or (file-directory-p (expand-file-name ".git" repo-dir))
           (file-exists-p (expand-file-name "last-sha.txt" repo-dir))
           (let ((els (directory-files-recursively repo-dir "\\.el\\'")))
             (and els (not (null els)))))))

(defun my-vendor-missing-repos ()
  (let* ((config my-vendor-autonomous-config)
         (vendor-dir (expand-file-name (cdr (assoc 'vendor-subdir config)) user-emacs-directory))
         (repos (cdr (assoc 'repositories config)))
         (missing '()))
    (dolist (repo repos)
      (let* ((name (cdr repo))
             (dir (expand-file-name name vendor-dir)))
        (unless (my-vendor-repo-present-p dir)
          (push repo missing))))
    (nreverse missing)))

(defun my-vendor-should-update-p ()
  (let* ((config my-vendor-autonomous-config)
         (cache-file (expand-file-name (cdr (assoc 'cache-file config))
                                       temporary-file-directory))
         (today (format-time-string "%Y-%m-%d"))
         (missing (my-vendor-missing-repos)))
    (or my-vendor-force-update
        missing
        (if (file-exists-p cache-file)
            (condition-case nil
                (let ((last-update (with-temp-buffer
                                     (insert-file-contents cache-file)
                                     (string-trim (buffer-string)))))
                  (not (string= last-update today)))
              (error t))
          t))))

(defun my-vendor-mark-updated ()
  (let* ((config my-vendor-autonomous-config)
         (cache-file (expand-file-name (cdr (assoc 'cache-file config))
                                       temporary-file-directory))
         (today (format-time-string "%Y-%m-%d")))
    (condition-case nil
        (with-temp-file cache-file
          (insert today))
      (error nil))))

(defun my-vendor-execute-git-update (repo-url repo-dir)
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

(defun my-vendor-autonomous-setup ()
  (let* ((config my-vendor-autonomous-config)
         (strategy (my-vendor-select-strategy))
         (vendor-dir (expand-file-name (cdr (assoc 'vendor-subdir config)) user-emacs-directory))
         (repositories (cdr (assoc 'repositories config)))
         (missing (my-vendor-missing-repos))
         (should-update (my-vendor-should-update-p))
         (updated-any nil)
         (fingerprint (my-vendor-get-system-fingerprint)))
    (message "=== Enhanced Cross-Platform Vendor Setup ===")
    (message "Platform fingerprint: %s" fingerprint)
    (message "Vendor directory: %s" vendor-dir)
    (message "Strategy: %s" strategy)
    (message "Should update: %s%s"
             (if should-update "YES" "NO")
             (if missing " (missing repos present)" ""))
    (condition-case nil
        (unless (file-directory-p vendor-dir)
          (make-directory vendor-dir t))
      (error (setq strategy 'readonly)))
    (pcase strategy
      ('readonly (message "Read-only mode"))
      (_
       (dolist (repo repositories)
         (let* ((url (car repo))
                (name (cdr repo))
                (repo-dir (expand-file-name name vendor-dir))
                (repo-missing (not (my-vendor-repo-present-p repo-dir)))
                (need-update (or repo-missing should-update)))
           (when need-update
             (let ((ok nil))
               (pcase strategy
                 ('git-update
                  (setq ok (or (my-vendor-execute-git-update url repo-dir)
                               (my-vendor-execute-archive-download url repo-dir)
                               (my-vendor-execute-raw-download url repo-dir))))
                 ('download-archive
                  (setq ok (or (my-vendor-execute-archive-download url repo-dir)
                               (my-vendor-execute-raw-download url repo-dir))))
                 ('download-raw
                  (setq ok (or (my-vendor-execute-raw-download url repo-dir)
                               (my-vendor-execute-archive-download url repo-dir))))
                 (_ (setq ok nil)))
               (when ok (setq updated-any t))))))))
    (when updated-any (my-vendor-mark-updated))
    (let ((added-paths 0))
      (message "Smart compilation and load-path setup:")
      (dolist (repo (cdr (assoc 'repositories my-vendor-autonomous-config)))
        (let* ((name (cdr repo))
               (source-dir (expand-file-name name (expand-file-name (cdr (assoc 'vendor-subdir my-vendor-autonomous-config))
                                                                    user-emacs-directory))))
          (if (file-directory-p source-dir)
              (let ((final-dir (my-vendor-compile-package-smart name source-dir)))
                (add-to-list 'load-path final-dir)
                (setq added-paths (1+ added-paths))
                (message "  ✓ %s -> %s" name
                         (if (string= final-dir source-dir) "SOURCE" "COMPILED")))
            (message "  ✗ %s (missing)" name))))
      (message "=== Cross-Platform Setup Complete: %d packages ===" added-paths))))

;; --------------------------------------------------------------------
;; Commands
;; --------------------------------------------------------------------

(defun my-vendor-status ()
  (interactive)
  (let* ((config my-vendor-autonomous-config)
         (fingerprint (my-vendor-get-system-fingerprint)))
    (message "=== Cross-Platform Vendor Status ===")
    (message "Platform: %s" fingerprint)
    (message "Lexical binding fix: %s" (if (cdr (assoc 'fix-lexical-binding config)) "ON" "OFF"))
    (message "Dependency handling: %s" (if (cdr (assoc 'handle-dependencies config)) "ON" "OFF"))
    (message "Warning level: %s" (cdr (assoc 'compilation-warnings config)))
    (my-vendor-discover-environment)
    (message "Capabilities: %S" my-vendor-runtime-data))
  (let* ((config my-vendor-autonomous-config)
         (vendor-dir (expand-file-name (cdr (assoc 'vendor-subdir config)) user-emacs-directory)))
    (dolist (repo (cdr (assoc 'repositories config)))
      (let* ((url (car repo))
             (name (cdr repo))
             (dir (expand-file-name name vendor-dir))
             (owner-repo (my-vendor-extract-owner-repo url))
             (override (alist-get owner-repo my-vendor-branch-overrides nil nil #'string=)))
        (message "%-28s %-8s %s"
                 name
                 (if override (format "[%s]" override) "")
                 (if (my-vendor-repo-present-p dir) "OK" "MISSING"))))))

(defun my-vendor-toggle-compilation ()
  (interactive)
  (let ((current (cdr (assoc 'auto-compile my-vendor-autonomous-config))))
    (setcdr (assoc 'auto-compile my-vendor-autonomous-config) (not current))
    (message "Auto-compilation: %s" (if (not current) "ENABLED" "DISABLED"))))

(defun my-vendor-force-recompile ()
  (interactive)
  (my-vendor-clean-cache)
  (my-vendor-autonomous-setup))

(defun my-vendor-clean-cache ()
  (interactive)
  (let* ((config my-vendor-autonomous-config)
         (vendor-dir (expand-file-name (cdr (assoc 'vendor-subdir config)) user-emacs-directory))
         (cache-base-dir (expand-file-name (cdr (assoc 'compile-cache-subdir config)) vendor-dir)))
    (when (file-directory-p cache-base-dir)
      (delete-directory cache-base-dir t))
    (message "Cache cleaned: %s" cache-base-dir)))

(defun my-vendor-reset-daily-throttle ()
  (interactive)
  (let* ((config my-vendor-autonomous-config)
         (cache-file (expand-file-name (cdr (assoc 'cache-file config))
                                       temporary-file-directory)))
    (when (file-exists-p cache-file)
      (delete-file cache-file))
    (message "Removed throttle file: %s" cache-file)))

(defun my-vendor-update-now ()
  (interactive)
  (let ((my-vendor-force-update t))
    (my-vendor-autonomous-setup)))

(provide 'init-vendor)

;;; lisp/init-vendor.el ends here