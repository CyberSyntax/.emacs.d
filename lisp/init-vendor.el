;;; lisp/init-vendor.el --- Cross-platform vendor (strict branches, daily update) -*- lexical-binding: t; -*-

;; Goals
;; - Strict branches: only the exact branch listed is used. No guessing.
;; - Cross-platform:
;;     * Non-Android: use git if available (clone/pull).
;;     * Android or no git: download GitHub tarball and extract.
;; - Daily throttle:
;;     * If a repo was updated today, skip.
;;     * If missing, install now.
;;     * If present but last update was before today, update now.
;; - Adds vendor directories to `load-path`.
;; - No prompts; no destructive “delete-all” command required.

(require 'cl-lib)
(require 'subr-x)
(require 'url)        ;; url-copy-file
(require 'url-parse)  ;; url-generic-parse-url, url-host

;; Ensure gzip decompression of .tar.gz is transparent when reading.
(auto-compression-mode 1)

;; -----------------------------------------------------------------------------
;; Configuration
;; -----------------------------------------------------------------------------

(defvar my-vendor-directory
  (expand-file-name "lisp/vendor" user-emacs-directory)
  "Directory where vendor packages are unpacked.")

(defvar my-vendor-url-timeout 120
  "Timeout in seconds for HTTP(S) downloads.")

;; Repository list: (URL-OR-OWNER/REPO . LOCAL-DIR-NAME)
(defvar my-vendor-repositories
  '(("https://github.com/bohonghuang/org-srs.git"                . "org-srs")
    ("https://github.com/open-spaced-repetition/lisp-fsrs.git"   . "fsrs")
    ("https://github.com/CyberSyntax/org-queue.git"              . "org-queue")
    ("https://github.com/CyberSyntax/org-story.git"              . "org-story")
    ("https://github.com/CyberSyntax/hanja-reading.git"          . "hanja-reading")
    ("https://github.com/CyberSyntax/org-headline-manager.git"   . "org-headline-manager")
    ("https://github.com/CyberSyntax/emacs-android-support-module.git" . "android-support-module"))
  "Alist of GitHub repos to fetch and their local directory names.")

;; STRICT branch overrides: every repo here is used as-is; if missing, we skip it.
(defvar my-vendor-branch-overrides
  '(("bohonghuang/org-srs"                      . "master")
    ("open-spaced-repetition/lisp-fsrs"         . "master")
    ("CyberSyntax/org-queue"                    . "main")
    ("CyberSyntax/org-story"                    . "main")
    ("CyberSyntax/hanja-reading"                . "main")
    ("CyberSyntax/org-headline-manager"         . "main")
    ("CyberSyntax/emacs-android-support-module" . "main"))
  "Owner/repo -> required branch. Only these branches are used (strict mode).")

;; Cache/state file (per-repo last update date)
(defvar my-vendor-cache-dir
  (if (boundp 'cache-dir)
      cache-dir
    (expand-file-name "var/cache/" user-emacs-directory)))

(defvar my-vendor-state-file
  (expand-file-name "vendor-state.el" my-vendor-cache-dir)
  "File storing an alist of (\"owner/repo\" . \"YYYY-MM-DD\").")

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun my-vendor--ensure-dir (dir)
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun my-vendor--today ()
  (format-time-string "%Y-%m-%d"))

(defun my-vendor--normalize-owner-repo (spec)
  "Turn SPEC into \"owner/repo\".
SPEC may be \"owner/repo\" or a GitHub URL ending in .git."
  (cond
   ((and (stringp spec)
         (string-match-p "\\`[[:alnum:]-]+/[[:alnum:]._+-]+\\'" spec))
    spec)
   ((and (stringp spec)
         (string-match "\\`https://github\\.com/\\([^?#]+?\\)\\(?:\\.git\\)?\\'" spec))
    (match-string 1 spec))
   (t
    (error "Unrecognized repo spec: %S" spec))))

(defun my-vendor--branch-for (owner-repo)
  (alist-get owner-repo my-vendor-branch-overrides nil nil #'string=))

(defun my-vendor--use-git-p ()
  "Return non-nil when we should use git (non-Android and git is available)."
  (and (not (eq system-type 'android))
       (executable-find "git")))

(defun my-vendor--repo-dir (local-dir)
  (expand-file-name local-dir my-vendor-directory))

(defun my-vendor--download-to-file (url file)
  "Download URL into FILE using url-copy-file. Return t on success."
  (let ((url-request-timeout my-vendor-url-timeout))
    (condition-case err
        (progn
          (when (file-exists-p file) (delete-file file))
          (url-copy-file url file t)
          (file-exists-p file))
      (error
       (message "    ✗ Download failed from %s: %s" url (error-message-string err))
       nil))))

(defun my-vendor--codeload-url (owner-repo branch)
  (format "https://codeload.github.com/%s/tar.gz/refs/heads/%s" owner-repo branch))

(defun my-vendor--github-archive-url (owner-repo branch)
  (format "https://github.com/%s/archive/refs/heads/%s.tar.gz" owner-repo branch))

;; -----------------------------------------------------------------------------
;; Minimal tar extractor (pure elisp)
;; -----------------------------------------------------------------------------

(defun my-vendor--trim-nul (s)
  (if (not s) "" (if (string-match "\0" s) (substring s 0 (match-beginning 0)) s)))

(defun my-vendor--parse-octal (s)
  (let ((s1 (and s (replace-regexp-in-string "[\0 ].*$" "" s))))
    (if (or (null s1) (string-empty-p s1)) 0 (string-to-number s1 8))))

(defun my-vendor--tar-extract (tarfile dest &optional strip-components)
  "Extract TARFILE to DEST, removing STRIP-COMPONENTS leading path parts."
  (setq dest (file-name-as-directory dest))
  (my-vendor--ensure-dir dest)
  (with-temp-buffer
    (setq buffer-file-coding-system 'no-conversion)
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents tarfile))
    (let ((pos (point-min))
          (end (point-max)))
      (cl-labels
          ((blk (o l) (buffer-substring-no-properties o (min (+ o l) end)))
           (field (b off len) (my-vendor--trim-nul (substring b off (+ off len))))
           (oct (b off len) (my-vendor--parse-octal (substring b off (+ off len)))))
        (while (<= (+ pos 512) end)
          (let ((block (blk pos 512)))
            ;; End-of-archive: NUL block
            (if (string-match-p "\\`\\(?:\\x00\\{512\\}\\)+\\'" block)
                (setq pos (+ pos 512))
              (let* ((name   (field block 0 100))
                     (size   (oct   block 124 12))
                     (type   (let ((c (aref block 156))) (if (= c 0) ?0 c)))
                     (prefix (field block 345 155))
                     (full   (if (string-empty-p prefix) name (concat prefix "/" name))))
                (when (and full (not (string-empty-p full)))
                  (let* ((parts (split-string full "/" t))
                         (parts* (nthcdr (or strip-components 0) parts))
                         (rel   (mapconcat #'identity parts* "/")))
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
                  (setq pos (+ pos 512 data-size pad)))))))))))

;; -----------------------------------------------------------------------------
;; State (daily throttle)
;; -----------------------------------------------------------------------------

(defvar my-vendor--state nil
  "Alist (\"owner/repo\" . \"YYYY-MM-DD\"). Loaded from `my-vendor-state-file`.")

(defun my-vendor--load-state ()
  (setq my-vendor--state nil)
  (when (file-exists-p my-vendor-state-file)
    (condition-case _err
        (with-temp-buffer
          (insert-file-contents my-vendor-state-file)
          (setq my-vendor--state (read (current-buffer))))
      (error (setq my-vendor--state nil))))
  (unless (listp my-vendor--state)
    (setq my-vendor--state nil))
  my-vendor--state)

(defun my-vendor--save-state ()
  (my-vendor--ensure-dir (file-name-directory my-vendor-state-file))
  (with-temp-file my-vendor-state-file
    (insert (prin1-to-string my-vendor--state))))

(defun my-vendor--state-get (owner-repo)
  (alist-get owner-repo my-vendor--state nil nil #'string=))

(defun my-vendor--state-put-today (owner-repo)
  (let ((cell (assoc owner-repo my-vendor--state)))
    (if cell
        (setcdr cell (my-vendor--today))
      (push (cons owner-repo (my-vendor--today)) my-vendor--state))))

;; -----------------------------------------------------------------------------
;; Git path
;; -----------------------------------------------------------------------------

(defun my-vendor--call-git (&rest args)
  "Call git with ARGS; return t on exit code 0."
  (let ((status (apply #'call-process "git" nil nil nil args)))
    (zerop status)))

(defun my-vendor--git-update (url owner-repo branch repo-dir)
  "Clone or update using git. Return t on success."
  (if (file-directory-p (expand-file-name ".git" repo-dir))
      ;; Update existing clone
      (let ((default-directory repo-dir))
        (message "  → git: updating %s (%s)" owner-repo branch)
        (and
         (my-vendor--call-git "fetch" "origin" branch "--depth" "1")
         ;; Ensure we're on the desired branch and aligned to origin/branch
         (or (my-vendor--call-git "checkout" branch)
             (my-vendor--call-git "checkout" "-B" branch))
         (my-vendor--call-git "reset" "--hard" (format "origin/%s" branch))))
    ;; Fresh clone
    (progn
      (when (file-directory-p repo-dir)
        (delete-directory repo-dir t))
      (message "  → git: cloning %s (%s)" owner-repo branch)
      (my-vendor--call-git "clone" "--depth" "1" "--branch" branch url repo-dir))))

;; -----------------------------------------------------------------------------
;; Tarball path
;; -----------------------------------------------------------------------------

(defun my-vendor--tarball-update (owner-repo branch repo-dir)
  "Download and extract tarball for OWNER-REPO@BRANCH into REPO-DIR. Return t on success."
  (let ((tmp (make-temp-file "vendor-" nil ".tar.gz"))
        (ok nil))
    (unwind-protect
        (let ((candidates (list (my-vendor--codeload-url owner-repo branch)
                                (my-vendor--github-archive-url owner-repo branch))))
          (dolist (u candidates)
            (unless ok
              (message "  → tarball: %s (%s)" owner-repo branch)
              (message "    downloading from %s" (condition-case nil
                                                    (url-host (url-generic-parse-url u))
                                                  (error u)))
              (when (my-vendor--download-to-file u tmp)
                ;; Replace contents atomically: delete dir then extract
                (when (file-directory-p repo-dir)
                  (delete-directory repo-dir t))
                (make-directory repo-dir t)
                (my-vendor--tar-extract tmp repo-dir 1)
                (setq ok t)
                (message "    ✓ installed to %s" repo-dir)))))
      (ignore-errors (delete-file tmp)))
    ok))

;; -----------------------------------------------------------------------------
;; Orchestration
;; -----------------------------------------------------------------------------

(defun my-vendor--ensure-one (entry force)
  "Ensure one repo ENTRY is installed/updated respecting daily throttle.
ENTRY is a cons (SPEC . LOCAL-DIR). Return non-nil if present at end."
  (let* ((spec (car entry))
         (local-dir (cdr entry))
         (owner-repo (my-vendor--normalize-owner-repo spec))
         (branch (my-vendor--branch-for owner-repo))
         (repo-dir (my-vendor--repo-dir local-dir))
         (present (file-directory-p repo-dir))
         (last (my-vendor--state-get owner-repo))
         (today (my-vendor--today))
         (do-update (or force (not present) (not (string= last today)))))
    (cond
     ((not branch)
      (message "  ✗ %s has no strict branch override; skipping" owner-repo)
      present)
     ((not do-update)
      (message "  ✓ %s up-to-date (today)" owner-repo)
      present)
     (t
      (let ((ok (if (my-vendor--use-git-p)
                    (my-vendor--git-update (if (string-match-p "\\`https://github\\.com/" spec)
                                               spec
                                             (format "https://github.com/%s.git" owner-repo))
                                           owner-repo branch repo-dir)
                  (my-vendor--tarball-update owner-repo branch repo-dir))))
        (when ok
          (my-vendor--state-put-today owner-repo))
        ok)))))

(defun my-vendor-autonomous-setup (&optional force)
  "Install/update vendor repositories, add them to `load-path`.
When FORCE is non-nil, ignore the daily throttle and update anyway."
  (message "=== Vendor Setup (strict branches, daily) ===")
  (message "Vendor directory: %s" my-vendor-directory)
  (my-vendor--ensure-dir my-vendor-directory)
  (my-vendor--ensure-dir my-vendor-cache-dir)
  (my-vendor--load-state)
  (let ((installed 0))
    (dolist (entry my-vendor-repositories)
      (when (my-vendor--ensure-one entry force)
        (setq installed (1+ installed))))
    (my-vendor--save-state)
    (message "Adding vendor packages to load-path...")
    (dolist (entry my-vendor-repositories)
      (let* ((local-dir (cdr entry))
             (dir (my-vendor--repo-dir local-dir)))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))
    (message "=== Vendor Setup Complete: %d packages present ===" installed)))

(defun my-vendor-status ()
  "Show status of vendor repos (branch and last update date)."
  (interactive)
  (my-vendor--load-state)
  (message "=== Vendor Status ===")
  (dolist (entry my-vendor-repositories)
    (let* ((spec (car entry))
           (local-dir (cdr entry))
           (owner-repo (my-vendor--normalize-owner-repo spec))
           (branch (or (my-vendor--branch-for owner-repo) "[none]"))
           (repo-dir (my-vendor--repo-dir local-dir))
           (last (or (my-vendor--state-get owner-repo) "[never]"))
           (ok (file-directory-p repo-dir)))
      (message "%-36s %-8s %-12s %s"
               owner-repo branch last (if ok "OK" "MISSING")))))

(defun my-vendor-update-now ()
  "Force update all repos now (ignoring daily throttle)."
  (interactive)
  (my-vendor-autonomous-setup t))

(defun my-vendor-reset-daily-throttle ()
  "Clear the daily throttle so next run will update all repos."
  (interactive)
  (when (file-exists-p my-vendor-state-file)
    (delete-file my-vendor-state-file))
  (setq my-vendor--state nil)
  (message "Vendor throttle reset: %s removed" my-vendor-state-file))

(provide 'init-vendor)

;;; lisp/init-vendor.el ends here
