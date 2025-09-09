;;; lisp/init-vendor.el --- One-shot vendor install; zero work after success -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-parse)

;; Completion record: if present with "ok", skip all vendor work except load-path.
(defconst my-deps-record-file
  (expand-file-name "var/deps.done" user-emacs-directory))
(defvar my-deps-complete
  (and (file-exists-p my-deps-record-file)
       (ignore-errors
         (with-temp-buffer
           (insert-file-contents my-deps-record-file)
           (goto-char (point-min))
           (re-search-forward "\\bok\\b" nil t)))))

(defun my-deps--record-success ()
  (make-directory (file-name-directory my-deps-record-file) t)
  (with-temp-file my-deps-record-file
    (insert "ok\n"))
  (setq my-deps-complete t))

;; Vendor config
(defvar my-vendor-directory
  (expand-file-name "lisp/vendor" user-emacs-directory))
(defvar my-vendor-url-timeout 120)

(defvar my-vendor-repositories
  '(("https://github.com/bohonghuang/org-srs.git"                . "org-srs")
    ("https://github.com/open-spaced-repetition/lisp-fsrs.git"   . "fsrs")
    ("https://github.com/CyberSyntax/org-queue.git"              . "org-queue")
    ("https://github.com/CyberSyntax/org-story.git"              . "org-story")
    ("https://github.com/CyberSyntax/hanja-reading.git"          . "hanja-reading")
    ("https://github.com/CyberSyntax/org-headline-manager.git"   . "org-headline-manager")
    ("https://github.com/CyberSyntax/emacs-android-support-module.git" . "android-support-module")))

(defvar my-vendor-branch-overrides
  '(("bohonghuang/org-srs"                      . "master")
    ("open-spaced-repetition/lisp-fsrs"         . "master")
    ("CyberSyntax/org-queue"                    . "main")
    ("CyberSyntax/org-story"                    . "main")
    ("CyberSyntax/hanja-reading"                . "main")
    ("CyberSyntax/org-headline-manager"         . "main")
    ("CyberSyntax/emacs-android-support-module" . "main")))

(defconst my-required-libraries
  '("gptel" "org" "org-roam" "org-roam-ui" "fsrs" "org-srs"
    "yasnippet" "org-web-tools" "transient"
    "org-queue" "org-story" "hanja-reading" "org-headline-manager" "android-support"))

;; Helpers
(defun my-vendor--ensure-dir (dir)
  (unless (file-directory-p dir) (make-directory dir t)))

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
  (and (not (eq system-type 'android))
       (executable-find "git")))

(defun my-vendor--repo-dir (local-dir)
  (expand-file-name local-dir my-vendor-directory))

(defun my-vendor--download-to-file (url file)
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

;; Minimal tar extractor (pure elisp)
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

;; Installers (missing-only)
(defun my-vendor--call-git (&rest args)
  (let ((status (apply #'call-process "git" nil nil nil args)))
    (zerop status)))
(defun my-vendor--git-install-if-missing (url branch repo-dir)
  (if (file-directory-p repo-dir)
      t
    (my-vendor--call-git "clone" "--depth" "1" "--branch" branch url repo-dir)))
(defun my-vendor--tarball-install-if-missing (owner-repo branch repo-dir)
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

;; Load-path
(defun my-vendor--add-paths ()
  (dolist (entry my-vendor-repositories)
    (let ((dir (my-vendor--repo-dir (cdr entry))))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; Presence check (ELPA + vendor)
(defun my-deps-all-present-p ()
  (cl-every #'locate-library my-required-libraries))

;; Main
(defun my-vendor-autonomous-setup ()
  "First run: install missing vendor repos only (if their libs aren’t already present).
Later runs: only add paths. No updates, no checks."
  (my-vendor--ensure-dir my-vendor-directory)
  (my-vendor--add-paths)
  (unless my-deps-complete
    (dolist (entry my-vendor-repositories)
      (let* ((spec (car entry))
             (local-dir (cdr entry))    ;; assume main library matches local-dir
             (owner-repo (my-vendor--normalize-owner-repo spec))
             (branch (my-vendor--branch-for owner-repo))
             (repo-dir (my-vendor--repo-dir local-dir)))
        ;; Install only if directory missing AND the library is not already available via ELPA.
        (when (and branch
                   (not (file-directory-p repo-dir))
                   (not (locate-library local-dir)))
          (let ((url (if (string-match-p "\\`https://github\\.com/" spec)
                         spec
                       (format "https://github.com/%s.git" owner-repo))))
            (or (and (my-vendor--use-git-p)
                     (my-vendor--git-install-if-missing url branch repo-dir))
                (my-vendor--tarball-install-if-missing owner-repo branch repo-dir))))))
    (my-vendor--add-paths)
    (when (my-deps-all-present-p)
      (my-deps--record-success)))
  t)

(provide 'init-vendor)

;;; lisp/init-vendor.el ends here