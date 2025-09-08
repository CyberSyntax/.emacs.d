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
  (setq org-ellipsis "⇣")
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
		   :image-size-adjust (1.0 . 1.0)
		   :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
		   :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))))

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

;; This allows Org mode to store and retrieve unique identifiers
;; across all your Org files.
(setq org-id-track-globally t)

;; The file path is constructed by expanding the relative path
;; "org-id-locations" based on the value of `cache-dir`.
(setq org-id-locations-file (expand-file-name "org-id-locations" cache-dir))

;; Set the location of the org-id-locations file (moved after org-queue loading)

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
When a file in org-agenda-files lies under `cache-dir`,
its path is saved relative to that directory, ensuring cross‑platform compatibility."
  :type 'string
  :group 'org-agenda)

(defun my-save-org-agenda-files-to-cache ()
  "Save `org-agenda-files` to `my-org-agenda-cache-file` with a date stamp.
For each file in org-agenda-files, if the file is inside `org-agenda-directory`,
its path is saved relative to that directory."
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
  (if (file-exists-p my-org-agenda-cache-file)
	(let* ((data (with-temp-buffer
		       (insert-file-contents my-org-agenda-cache-file)
		       (read (buffer-string))))
	       (saved-date (plist-get data :date))
	       (saved-files (plist-get data :agenda-files))
	       (today (format-time-string "%Y-%m-%d")))
	  (if (string= saved-date today)
	      (progn
		(setq org-agenda-files
		      (mapcar
		       (lambda (path)
			 (if (or (file-name-absolute-p path)
				 (string-match-p "^[A-Za-z]:[/\\\\]" path))
			     path
			   (expand-file-name path org-agenda-directory)))
		       saved-files))
		t)
	    nil))
    nil))

(defun my-auto-setup-org-agenda-files ()
  "Automatically set up `org-agenda-files`."
  (unless (file-directory-p org-agenda-directory)
    (make-directory org-agenda-directory t)
    (message "Created missing org-agenda-directory: %s" org-agenda-directory))
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
  :bind (("C-c n l" . org-roam-buffer-toggle)
	   ("C-c n f" . org-roam-node-find)
	   ("C-c n i" . org-roam-node-insert))
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
  :ensure t  ;; Good practice, though vendor also handles it
  :init
  ;; This code runs BEFORE fsrs is loaded.
  ;; This is the correct place to set configuration variables.
  (setq my-fsrs-weights
        [0.2187 2.1756 10.7365 41.8256 7.1638 0.5166 2.2582 0.0015 1.2522 0.1048 0.8643 1.8184 0.1412 0.2909 2.3755 0.2315 2.9898 0.3743 0.6460]))

;; Configure Org-SRS, ensuring it loads after FSRS
(use-package org-srs
  :ensure t
  :after fsrs ;; This keyword ensures fsrs is loaded first.
  :config
  ;; Configuration that runs AFTER org-srs is loaded can go here.
  (require 'org-srs)) ;; Explicitly require to be safe

(provide 'init-org)

;;; lisp/init-org.el ends here