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

;; ═══════════════════════════════════════════════════════════════════════════
;; Org→LLM Projection System
;; ═══════════════════════════════════════════════════════════════════════════
;;
;; THEORETICAL FOUNDATION
;; ──────────────────────
;; This implements a projection P: OrgTree → Text optimized for LLM consumption.
;;
;; From information theory, this is a rate-distortion problem:
;;   - Source X = org document (structured, hierarchical)
;;   - Channel capacity C = LLM context window (finite tokens)
;;   - Goal: minimize distortion d(X, X̂) subject to rate ≤ C
;;
;; The distortion measure is task-dependent, so we expose primitive operators
;; that compose to handle any task.
;;
;; DECOMPOSITION THEOREM
;; ─────────────────────
;; Any useful projection decomposes as: P = ω ∘ π ∘ σ
;;
;;   σ (Selection)  : OrgTree → OrgTree'   ; which nodes to include
;;   π (Projection) : OrgTree' → OrgTree'' ; which attributes per node
;;   ω (Ordering)   : OrgTree'' → Text     ; serialization (fixed: DFS)
;;
;; This decomposition is COMPLETE (any projection expressible) and
;; MINIMAL (no redundant parameters).
;;
;; ORTHOGONALITY
;; ─────────────
;; The three parameters (scope, depth, detail) are orthogonal:
;;   - Changing scope doesn't constrain valid depth values
;;   - Changing depth doesn't constrain valid detail values
;;   - No parameter is derivable from the others
;;
;; INFORMATION CHANNELS
;; ────────────────────
;; Org documents contain independent information channels:
;;
;;   Channel          │ Entropy │ Typical Task Relevance
;;   ─────────────────┼─────────┼────────────────────────
;;   Hierarchy        │ Low     │ Structure analysis
;;   Heading text     │ Medium  │ Navigation, TOC
;;   Body content     │ High    │ Most tasks
;;   Metadata         │ Low     │ Scheduling only
;;   Properties       │ Low     │ Technical only
;;
;; The 'content detail level removes metadata/properties (~25% token reduction)
;; with near-zero semantic loss for most LLM tasks—this is Pareto-optimal.
;;
;; SCALE SPACE
;; ───────────
;; Documents exist in a scale space (from wavelet theory):
;;   - Coarse scale (depth=1): Document theme, main sections
;;   - Medium scale (depth=3): Subsection topics
;;   - Fine scale (depth=∞):   All details
;;
;; Information density typically decreases with depth (Zipf-like).
;; Top levels carry more "aboutness" per token than deep levels.
;;
;; ═══════════════════════════════════════════════════════════════════════════

(defgroup my-org-llm nil
  "Org-mode content projection for LLM consumption."
  :group 'org
  :prefix "my-org-llm-")

(defcustom my-org-llm-default-depth nil
  "Default depth limit for `my-org-view-for-llm'.
nil means unlimited depth."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Max level"))
  :group 'my-org-llm)

(defcustom my-org-llm-default-detail 'content
  "Default detail level for `my-org-view-for-llm'.
- `structure': Headings only (coarsest, ~90% compression)
- `content':   Headings + body, no metadata (~25% compression, Pareto-optimal)
- `full':      Everything (no compression)"
  :type '(choice (const :tag "Structure only" structure)
                 (const :tag "Content (recommended)" content)
                 (const :tag "Full document" full))
  :group 'my-org-llm)

(defun my-org-llm--drawer-marker-p (line)
  "Return non-nil if LINE is a drawer boundary (:DRAWER: or :END:)."
  (string-match "^[ \t]*:[A-Z_]+:[ \t]*$" line))

(defun my-org-llm--update-drawer-stack (line drawers-stack)
  "Update DRAWERS-STACK based on LINE. Returns new stack."
  (cond
   ;; Drawer start (any drawer, not just PROPERTIES)
   ((string-match "^[ \t]*:\\([A-Z_]+\\):[ \t]*$" line)
    (let ((drawer-name (match-string 1 line)))
      (if (string= drawer-name "END")
          (cdr drawers-stack)  ; Pop on :END:
        (cons drawer-name drawers-stack))))  ; Push on drawer start
   (t drawers-stack)))

(defun my-org-llm--clean-heading (line)
  "Remove priorities and tags from heading LINE, preserving TODO state."
  ;; Remove priority [#A], [#B], etc.
  (setq line (replace-regexp-in-string "\\[#[A-Z0-9]\\][ \t]*" "" line))
  ;; Remove tags at end of line
  (setq line (replace-regexp-in-string "[ \t]+:[a-zA-Z0-9_@#%:]+:[ \t]*$" "" line))
  line)

(defun my-org-llm--clean-line (line)
  "Remove metadata artifacts from content LINE."
  ;; Remove active timestamps [2024-01-15 Mon 10:00]
  (setq line (replace-regexp-in-string
              "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^]]*\\]" "" line))
  ;; Remove inactive timestamps <2024-01-15 Mon 10:00>
  (setq line (replace-regexp-in-string
              "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^>]*>" "" line))
  ;; Remove CLOSED: SCHEDULED: DEADLINE: prefixes
  (setq line (replace-regexp-in-string
              "^[ \t]*\\(CLOSED\\|SCHEDULED\\|DEADLINE\\):.*$" "" line))
  line)

(defun my-org-llm--heading-level (line)
  "Return heading level of LINE, or nil if not a heading."
  (when (string-match "^\\(\\*+\\)[ \t]" line)
    (length (match-string 1 line))))

(defun my-org-llm--process-content (content detail depth)
  "Process CONTENT string according to DETAIL and DEPTH parameters.

DETAIL controls attribute projection:
  `structure' - headings only
  `content'   - headings + body, no metadata
  `full'      - everything

DEPTH controls level limit (nil = unlimited)."
  (let ((lines (split-string content "\n"))
        (drawers-stack nil)
        (current-heading-level 0)
        (include-body (memq detail '(content full)))
        (include-metadata (eq detail 'full))
        result)
    (dolist (line lines)
      (let ((heading-level (my-org-llm--heading-level line))
            (is-drawer-marker (my-org-llm--drawer-marker-p line))
            (was-in-drawer drawers-stack))
        ;; Update drawer state BEFORE processing
        (setq drawers-stack (my-org-llm--update-drawer-stack line drawers-stack))
        (cond
         ;; It's a heading
         (heading-level
          (setq current-heading-level heading-level)
          ;; Check depth limit
          (when (or (null depth) (<= heading-level depth))
            (push (my-org-llm--clean-heading line) result)))

         ;; Not a heading - process body content
         (include-body
          ;; Skip if we're beyond depth limit
          (when (or (null depth) (<= current-heading-level depth))
            (cond
             ;; Drawer markers: include only in full mode
             (is-drawer-marker
              (when include-metadata
                (push line result)))

             ;; Inside a drawer (was-in-drawer checks state BEFORE :END:)
             (was-in-drawer
              (when include-metadata
                (push line result)))

             ;; File-level keywords (#+title:, #+author:, etc.)
             ((string-match "^#\\+" line)
              (when include-metadata
                (push line result)))

             ;; Regular content line
             (t
              (if include-metadata
                  ;; Full mode: keep everything as-is
                  (push line result)
                ;; Content mode: clean metadata from lines
                (let ((cleaned (my-org-llm--clean-line line)))
                  ;; Skip lines that became empty after cleaning
                  (unless (and (string-match "^[ \t]*$" cleaned)
                               (not (string-match "^[ \t]*$" line)))
                    (push cleaned result)))))))))))

    ;; Join and clean up excessive blank lines
    (let ((text (string-join (nreverse result) "\n")))
      ;; Collapse 3+ consecutive newlines to 2
      (replace-regexp-in-string "\n\\{3,\\}" "\n\n" text))))

(defun my-org-llm--get-subtree-content (heading)
  "Get content of subtree under HEADING (string match)."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^\\*+[ \t]+" (regexp-quote heading) "\\([ \t]\\|$\\)") nil t)
      (let ((start (line-beginning-position))
            (level (my-org-llm--heading-level
                    (buffer-substring (line-beginning-position)
                                      (line-end-position)))))
        (forward-line 1)
        (while (and (not (eobp))
                    (let ((l (my-org-llm--heading-level
                              (buffer-substring (line-beginning-position)
                                                (line-end-position)))))
                      (or (null l) (> l level))))
          (forward-line 1))
        (buffer-substring-no-properties start (point))))))

(defun my-org-llm--get-region-content (beg end)
  "Get content between BEG and END positions."
  (buffer-substring-no-properties beg end))

(defun my-org-llm--get-buffer-content ()
  "Get entire buffer content."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun my-org-llm--get-subtree-at-point-content ()
  "Get content of subtree at point."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (point)))
      (org-end-of-subtree t t)
      (buffer-substring-no-properties start (point)))))

;;;###autoload
(defun my-org-view-for-llm (&rest args)
  "Project org content for LLM consumption.

This implements a projection P = ω ∘ π ∘ σ where:
  σ = selection (determined by :scope)
  π = projection (determined by :detail)
  ω = ordering (fixed: document order / DFS)

ARGS are keyword arguments:

:scope SCOPE
  What part of the document to include (selection operator σ).
  - `buffer'          : Entire buffer (default)
  - `subtree'         : Subtree at point
  - \"Heading text\"  : Subtree under matching heading
  - (BEG . END)       : Region between positions

:depth DEPTH
  Maximum heading level to include (integer, nil = unlimited).
  Implements scale-space selection:
  - 1: Document theme only (coarsest)
  - 2: Major sections
  - 3: Subsections (good default for TOC)
  - nil: All levels (finest)

:detail DETAIL
  What attributes to include per node (projection operator π).
  - `structure' : Headings only (~90% compression)
  - `content'   : Headings + body, no metadata (~25% compression)
  - `full'      : Everything (no compression)

COMMON USAGE PATTERNS:

  ;; TOC: structure at depth 3
  (my-org-view-for-llm :detail \\='structure :depth 3)

  ;; Section focus: specific subtree, full content
  (my-org-view-for-llm :scope \"Methods\" :detail \\='content)

  ;; Clean full buffer (Pareto-optimal default)
  (my-org-view-for-llm)

  ;; Full document with metadata (rare)
  (my-org-view-for-llm :detail \\='full)

Returns the processed string. When called interactively, also
copies result to kill ring."
  (interactive)
  (let* ((scope (or (plist-get args :scope) 'buffer))
         (depth (or (plist-get args :depth) my-org-llm-default-depth))
         (detail (or (plist-get args :detail) my-org-llm-default-detail))
         ;; Get raw content based on scope
         (content
          (cond
           ((eq scope 'buffer)
            (my-org-llm--get-buffer-content))
           ((eq scope 'subtree)
            (my-org-llm--get-subtree-at-point-content))
           ((stringp scope)
            (or (my-org-llm--get-subtree-content scope)
                (error "Heading not found: %s" scope)))
           ((and (consp scope) (integerp (car scope)) (integerp (cdr scope)))
            (my-org-llm--get-region-content (car scope) (cdr scope)))
           (t
            (error "Invalid scope: %S (expected buffer, subtree, string, or (beg . end))"
                   scope))))
         ;; Process content
         (result (my-org-llm--process-content content detail depth)))
    ;; Interactive: copy to kill ring
    (when (called-interactively-p 'any)
      (kill-new result)
      (message "Copied %d chars to kill ring (:scope %S :depth %S :detail %S)"
               (length result) scope depth detail))
    result))

;; ═══════════════════════════════════════════════════════════════════════════
;; Convenience Wrappers (for backward compatibility and ease of use)
;; ═══════════════════════════════════════════════════════════════════════════

;;;###autoload
(defun my-org-toc-for-llm (&optional max-level)
  "Extract TOC from current buffer up to MAX-LEVEL (default 3).
Convenience wrapper for `my-org-view-for-llm' with :detail \\='structure."
  (interactive "P")
  (my-org-view-for-llm :detail 'structure :depth (or max-level 3)))

;;;###autoload
(defun my-org-section-for-llm (heading)
  "Extract section content under HEADING.
Convenience wrapper for `my-org-view-for-llm' with :scope HEADING."
  (interactive "sHeading: ")
  (my-org-view-for-llm :scope heading :detail 'content))

;;;###autoload
(defun my-org-clean-buffer-for-llm ()
  "Clean entire buffer for LLM consumption.
Convenience wrapper for `my-org-view-for-llm' with default parameters."
  (interactive)
  (my-org-view-for-llm))

(provide 'init-org)

;;; lisp/init-org.el ends here
