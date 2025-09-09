;;; lisp/init-vendor.el --- One-shot vendor install; zero work after success -*- lexical-binding: t; -*-

;; Behavior
;; - First run (no deps record): install only missing vendor repos, then add paths.
;; - Subsequent runs (deps record present): add paths only. No network, no per-repo checks, no updates.

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-parse)

;; -------------------------------------------------------------------
;; Completion record (shared with init-deps / init-packages)
;; -------------------------------------------------------------------

(defconst my-deps-record-file
  (expand-file-name "var/deps.done" user-emacs-directory)
  "If this file contains \"ok\", dependency setup is considered complete (skip all work).")

(defvar my-deps-complete
  (and (file-exists-p my-deps-record-file)
       (ignore-errors
         (with-temp-buffer
           (insert-file-contents my-deps-record-file)
           (goto-char (point-min))
           (re-search-forward "\\bok\\b" nil t))))
  "Non-nil means deps were previously installed; vendor should perform no installs/updates.")

;; -------------------------------------------------------------------
;; Config
;; -------------------------------------------------------------------

(defvar my-vendor-directory
  (expand-file-name "lisp/vendor" user-emacs-directory)
  "Directory where vendor packages are unpacked.")

(defvar my-vendor-url-timeout 120
  "Timeout in seconds for HTTP(S) downloads.")

;; (URL-OR-OWNER/REPO . LOCAL-DIR-NAME)
(defvar my-vendor-repositories
  '(("https://github.com/bohonghuang/org-srs.git"                . "org-srs")
    ("https://github.com/open-spaced-repetition/lisp-fsrs.git"   . "fsrs")
    ("https://github.com/CyberSyntax/org-queue.git"              . "org-queue")
    ("https://github.com/CyberSyntax/org-story.git"              . "org-story")
    ("https://github.com/CyberSyntax/hanja-reading.git"          . "hanja-reading")
    ("https://github.com/CyberSyntax/org-headline-manager.git"   . "org-headline-manager")
    ("https://github.com/CyberSyntax/emacs-android-support-module.git" . "android-support-module"))
  "Alist of GitHub repos to fetch and their local directory names.")

;; Strict branch for each repo
(defvar my-vendor-branch-overrides
  '(("bohonghuang/org-srs"                      . "master")
    ("open-spaced-repetition/lisp-fsrs"         . "master")
    ("CyberSyntax/org-queue"                    . "main")
    ("CyberSyntax/org-story"                    . "main")
    ("CyberSyntax/hanja-reading"                . "main")
    ("CyberSyntax/org-headline-manager"         . "main")
    ("CyberSyntax/emacs-android-support-module" . "main"))
  "Owner/repo -> fixed branch to use.")

;; -------------------------------------------------------------------
;; Helpers
;; -------------------------------------------------------------------

(defun my-vendor--ensure-dir (dir)
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun my-vendor--normalize-owner-repo (spec)
  "Turn SPEC into \"owner/repo\"."
  (cond
   ((and (stringp spec)
         (string-match-p "\\`[[:alnum:]-]+/[[:alnum:]._+-]+\\'" spec))
    spec)
   ((and (stringp spec)
         (string-match "\\`https://github\\.com/\$$[^/]+/[^/]+\$$\$$?:\\.git\$$?\\'" spec))
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
    (condition-case _err
        (progn
          (when (file-exists-p file) (delete-file file))
          (url-copy-file url file t)
          (file-exists-p file))
      (error nil))))

(defun my-vendor--codeload-url (owner-repo branch)
  (format "https://codeload.github.com/%s/tar.gz/refs/heads/%s" owner-repo branch))

(defun my-vendor--github-archive-url (owner-repo branch)
  (format "https://github.com/%s/archive/refs/heads/%s.tar.gz" owner-repo branch))

;; Minimal .tar.gz extractor (no external tools)
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
            (if (string-match-p "\\`\$$?:\\x00\\{512\\}\$$+\\'" block)
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

;; Installers (missing-only, no updates)
(defun my-vendor--call-git (&rest args)
  "Call git with ARGS; return t on exit code 0."
  (let ((status (apply #'call-process "git" nil nil nil args)))
    (zerop status)))

(defun my-vendor--git-install-if-missing (url branch repo-dir)
  "Clone URL@BRANCH into REPO-DIR if missing. Return t if present after."
  (if (file-directory-p repo-dir)
      t
    (my-vendor--call-git "clone" "--depth" "1" "--branch" branch url repo-dir)))

(defun my-vendor--tarball-install-if-missing (owner-repo branch repo-dir)
  "Download and extract tarball for OWNER-REPO@BRANCH into REPO-DIR if missing.
Return t if present after."
  (if (file-directory-p repo-dir)
      t
    (let ((tmp (make-temp-file "vendor-" nil ".tar.gz"))
          (ok nil))
      (unwind-protect
          (let ((candidates (list (my-vendor--codeload-url owner-repo branch)
                                  (my-vendor--github-archive-url owner-repo branch))))
            (dolist (u candidates)
              (unless ok
                (when (my-vendor--download-to-file u tmp)
                  (make-directory repo-dir t)
                  (my-vendor--tar-extract tmp repo-dir 1)
                  (setq ok t)))))
        (ignore-errors (delete-file tmp)))
      (and ok (file-directory-p repo-dir)))))

;; Load-path integration
(defun my-vendor--add-paths ()
  "Add vendor directories to `load-path`."
  (dolist (entry my-vendor-repositories)
    (let ((dir (my-vendor--repo-dir (cdr entry))))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; -------------------------------------------------------------------
;; Orchestration
;; -------------------------------------------------------------------

(defun my-vendor-autonomous-setup ()
  "First run: install missing vendor repos only; then add paths.
Later runs (deps done): add paths only; no installs, no checks, no updates."
  (my-vendor--ensure-dir my-vendor-directory)
  (if my-deps-complete
      (progn
        (my-vendor--add-paths)
        t)
    ;; First run: install missing only (silent), then add paths.
    (dolist (entry my-vendor-repositories)
      (let* ((spec (car entry))
             (local-dir (cdr entry))
             (owner-repo (my-vendor--normalize-owner-repo spec))
             (branch (my-vendor--branch-for owner-repo))
             (repo-dir (my-vendor--repo-dir local-dir)))
        (when (and branch (not (file-directory-p repo-dir)))
          (let ((url (if (string-match-p "\\`https://github\\.com/" spec)
                         spec
                       (format "https://github.com/%s.git" owner-repo))))
            (or (and (my-vendor--use-git-p)
                     (my-vendor--git-install-if-missing url branch repo-dir))
                (my-vendor--tarball-install-if-missing owner-repo branch repo-dir))))))
    (my-vendor--add-paths)
    t))

(provide 'init-vendor)

;;; lisp/init-vendor.el ends here