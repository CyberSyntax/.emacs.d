;;; lisp/init-org.el -*- lexical-binding: t -*-

;; Disable persistent caching for Org parser elements.
(setq org-element-cache-persistent nil)

;; Ensure org-element is loaded
(require 'org-element)

(with-eval-after-load 'org-element
  (defun my-org-element-at-point-advice (orig-fn &rest args)
    "Only run org-element-at-point in Org mode buffers."
    (when (derived-mode-p 'org-mode)
      (apply orig-fn args)))
  (advice-add 'org-element-at-point :around #'my-org-element-at-point-advice))

;; Configure Org Mode
(use-package org
  :ensure t
  :init
  (setq org-src-fontify-natively t)
  :config
  (define-key org-mode-map (kbd "M-p") 'org-metaup)
  (define-key org-mode-map (kbd "M-n") 'org-metadown)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-cycle-separator-lines -1)
  (setq org-return-follows-link t)
  (setq org-export-with-toc nil)
  (setq org-startup-folded 'content)
  ;; Disable backup files
  (setq make-backup-files nil)
  ;; Disable auto-save files
  (setq auto-save-default nil)
  ;; On Windows, set the browser function appropriately:
  (when (eq system-type 'windows-nt)
    (setq browse-url-browser-function 'browse-url-default-windows-browser))
  (require 'org-tempo))  ;; For easy code block expansion

;; Set the default LaTeX preview process to dvisvgm
(setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-preview-latex-process-alist
      '((dvisvgm :programs ("latex" "dvisvgm")
                 :description "dvi > svg"
                 :message "you need to install the programs: latex and dvisvgm."
                 :image-input-type "dvi"
                 :image-output-type "svg"
                 :image-size-adjust (1.6 . 1.6)
                 :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
                 :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))))

;; Make Org scale the inline previews larger, too
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.8))
(setq org-format-latex-options
      (plist-put org-format-latex-options :html-scale 1.8))

;; Customize the LaTeX header for rendering TikZ and pgfplots
(setq org-format-latex-header
      "\\documentclass[preview]{standalone}
       \\usepackage{amsmath}
       \\usepackage{tikz}
       \\usepackage{pgfplots}
       \\pgfplotsset{compat=1.17}
       \\usepackage[T1]{fontenc}
       \\usepackage{lmodern}")

;; Ensure org-id is loaded
(require 'org-id)

(setq org-id-uuid-program (expand-file-name "bin/uuidgenlc" user-emacs-directory))

;; Fallback on platforms (e.g. Android) if the program doesn’t exist
(when (and (boundp 'org-id-uuid-program)
           (not (file-exists-p org-id-uuid-program)))
  (setq org-id-uuid-program nil))

;; This allows Org mode to store and retrieve unique identifiers
;; across all your Org files.
(setq org-id-track-globally t)

;; The file path is constructed by expanding the relative path
;; "org-id-locations" based on the value of `cache-dir`.
(setq org-id-locations-file (expand-file-name "org-id-locations" cache-dir))

;; Set Org-mode to open PDF links within Emacs instead of an external application
(with-eval-after-load 'org
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)))

(defun my/wiki-file-handler (operation &rest args)
  "Intercept /wiki/... files and open them as Wikipedia URLs."
  (let ((file (car args)))
    (if (and (stringp file)
             (string-prefix-p "/wiki/" file))
        (let ((url (concat "https://en.wikipedia.org" file)))
          (browse-url url)
          ;; Return nil or something harmless to suppress error
          nil)
      ;; For everything else, fallback to the original handler
      (let ((inhibit-file-name-handlers
             (cons 'my/wiki-file-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args)))))

;; Register the handler for "/wiki/" paths
(add-to-list 'file-name-handler-alist '("^/wiki/" . my/wiki-file-handler))

;; Define a customizable variable for the org-agenda cache file.
(defcustom my-org-agenda-cache-file (expand-file-name "org-agenda.cache" cache-dir)
  "File path to store the cached org-agenda-files list with a date stamp.
For each file in org-agenda-files, if the file lies under `org-agenda-directory`,
its path is saved relative to that directory, ensuring cross‑platform compatibility."
  :type 'string
  :group 'org-agenda)

(defun my--ensure-parent-dir (file)
  "Ensure the parent directory of FILE exists."
  (let ((dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun my-save-org-agenda-files-to-cache ()
  "Save `org-agenda-files` to `my-org-agenda-cache-file` with a date stamp.
For each file in org-agenda-files, if the file is inside `org-agenda-directory`,
its path is saved relative to that directory."
  (my--ensure-parent-dir my-org-agenda-cache-file)
  (with-temp-file my-org-agenda-cache-file
    (let ((today (format-time-string "%Y-%m-%d"))
          (files-saved
           (delq nil
                 (mapcar
                  (lambda (file)
                    (when (file-exists-p file)
                      (let ((full (file-truename file)))
                        (if (file-in-directory-p full org-agenda-directory)
                            (file-relative-name full org-agenda-directory)
                          full))))
                  org-agenda-files))))
      (insert (prin1-to-string (list :date today :agenda-files files-saved))))))

(defun my-load-org-agenda-files-from-cache ()
  "Load cached org agenda files from `my-org-agenda-cache-file`.
If the saved date matches today, convert any relative paths into absolute
paths using `org-agenda-directory` and update `org-agenda-files` accordingly."
  (my--ensure-parent-dir my-org-agenda-cache-file)
  (if (file-exists-p my-org-agenda-cache-file)
      (let* ((data (condition-case _
                       (with-temp-buffer
                         (insert-file-contents my-org-agenda-cache-file)
                         (read (buffer-string)))
                     (error nil))))
        (when (and (listp data))
          (let* ((saved-date (plist-get data :date))
                 (saved-files (plist-get data :agenda-files))
                 (today (format-time-string "%Y-%m-%d")))
            (when (and (stringp saved-date)
                       (equal saved-date today)
                       (listp saved-files))
              (setq org-agenda-files
                    (mapcar
                     (lambda (path)
                       (if (or (file-name-absolute-p path)
                               ;; Windows drive-letter absolute path
                               (string-match-p "^[A-Za-z]:[\\/]" path))
                           path
                         (expand-file-name path org-agenda-directory)))
                     saved-files))
              t))))
    nil))

(defun my-auto-setup-org-agenda-files ()
  "Automatically set up `org-agenda-files`."
  (unless (file-directory-p org-agenda-directory)
    (make-directory org-agenda-directory t)
    (message "Created missing org-agenda-directory: %s" org-agenda-directory))
  ;; Ensure cache file’s parent dir exists even if init.el didn’t create it
  (my--ensure-parent-dir my-org-agenda-cache-file)
  (if (my-load-org-agenda-files-from-cache)
      (message "Loaded org-agenda-files from cache.")
    (progn
      (setq org-agenda-files
            (delete-dups
             (mapcar #'file-truename
                     (directory-files-recursively org-agenda-directory "\\.org$"))))
      (my-save-org-agenda-files-to-cache)
      (message "Scanned org-agenda-directory and updated org-agenda-files cache."))))

;; Execute auto-setup of org-agenda files.
(my-auto-setup-org-agenda-files)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory org-agenda-directory)
  (setq org-roam-db-location (expand-file-name "org-roam.db" cache-dir))
  (setq org-roam-database-connector (if (sqlite-available-p) 'sqlite 'sqlite3))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (if (eq system-type 'android)
      (org-roam-db-autosync-mode 0)
    ;; macOS/Linux/Windows:
    (org-roam-db-autosync-mode 1))
  ;; Don’t let org-roam warnings pop the *Warnings* window
  (if (boundp 'warning-suppress-log-types)
      (add-to-list 'warning-suppress-log-types '(org-roam))  ; keep echo-area, no *Warnings*
    (add-to-list 'warning-suppress-types '(org-roam)))       ; older Emacs: fully suppress
  )

(use-package org-roam-ui
  :after org-roam
  :ensure t
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

;; Configure FSRS (Free Spaced Repetition Scheduler)
(use-package fsrs
  :ensure t
  :init
  ;; This code runs BEFORE fsrs is loaded.
  ;; This is the correct place to set configuration variables.
  (setq my-fsrs-weights
        [0.1328, 1.8668, 11.7173, 56.0247, 6.7091, 0.8260, 2.6668, 0.0010, 1.5693, 0.1708, 0.6159, 1.3165, 0.0831, 0.4285, 1.1685, 0.6014, 1.8729, 0.8229, 0.2622, 0.1653, 0.4227]))

;; Configure Org-SRS, ensuring it loads after FSRS
(use-package org-srs
  :ensure t
  :after fsrs ;; This keyword ensures fsrs is loaded first.
  :config
  ;; Configuration that runs AFTER org-srs is loaded can go here.
  (require 'org-srs)) ;; Explicitly require to be safe

;; -------------------------------
;; Org Content Cleaning for LLM
;; -------------------------------

(defun my-org-clean-for-llm (content)
  "Clean org CONTENT for LLM consumption.
Removes PROPERTIES blocks, SCHEDULED/DEADLINE lines, priorities,
timestamps, and tags while preserving the actual content."
  (let ((in-properties nil)
        (lines (split-string content "\n"))
        result)
    (dolist (line lines)
      (cond
       ((string-match "^:PROPERTIES:" line) (setq in-properties t))
       ((string-match "^:END:" line) (setq in-properties nil))
       ((not in-properties)
        (unless (or (string-match "^SCHEDULED:" line)
                    (string-match "^DEADLINE:" line)
                    (string-match "^#\\+title:" line))
          (setq line (replace-regexp-in-string "\\[#[0-9A-Z]+\\][ \t]*" "" line))
          (setq line (replace-regexp-in-string "\\[[0-9]+-[0-9]+-[0-9]+ [A-Za-z]+ [0-9:]+\\]" "" line))
          (setq line (replace-regexp-in-string "[ \t]+:[a-zA-Z0-9_@:]+:[ \t]*$" "" line))
          (push line result)))))
    (string-join (nreverse result) "\n")))

(defun my-org-clean-buffer-for-llm ()
  "Clean current org buffer for LLM and return as string."
  (interactive)
  (my-org-clean-for-llm (buffer-string)))

(provide 'init-org)

;;; lisp/init-org.el ends here
