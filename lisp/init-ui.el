;;; init-ui.el --- Cross-platform UI/font setup with Android diagnostics -*- lexical-binding: t; -*-
;;
;; This file configures fonts for macOS and Android, and documents everything we learned
;; while debugging font rendering on Emacs-Android (sfnt-android backend). It keeps the
;; macOS setup fully functional and adds robust, probe-driven logic on Android, with
;; detailed comments and diagnostic helpers so we can continue the investigation later.
;;
;; Summary of key findings (captured here for future reference):
;; - Backend differences matter:
;;   * macOS (Cocoa) + Fontconfig: file-based fonts and families work as expected.
;;   * Android (sfnt-android backend): Emacs can only use Typeface families the OS
;;     explicitly exposes to the app. It does not reliably follow the system's font
;;     fallback chain, and generally ignores :file fonts (font-spec :file ...).
;;
;; - On Android, :file fonts are typically ignored:
;;   * Even when (find-font (font-spec :file "…")) returns a font-entity, the backend
;;     often will not render glyphs from such file fonts. The mapping appears to set,
;;     but (fontset-font …) resolves to (nil . "…") and char-displayable-p is nil.
;;
;; - What worked on the tested Samsung device:
;;   * Hangul rendered once we mapped a system family the backend could actually use:
;;     "One UI Sans KR VF". We made this deterministic by probing families and then
;;     pinning the first one verified by char-displayable-p.
;;   * Han (CJK Unified Ideographs) did not render because no Han-capable Typeface was
;;     visible to Emacs (font-family-list). Although other apps display Han via Android's
;;     fallback, sfnt-android did not provide a family Emacs can map to.
;;
;; - What did not work (as of testing):
;;   * "SEC CJK Regular Extra", "SECFallback" and various CJK buckets/encodings did not
;;     render Han when bound with set-fontset-font.
;;   * Probing plausible One UI regional families (TC/HK/JP/SC, serif/sans) returned nil
;;     (not installed / not exported as Typeface).
;;   * xwidget-webkit was present symbolically but the build was not compiled with xwidgets.
;;
;; - Practical paths forward if Han still fails on your device:
;;   1) If your firmware ever exposes a Han-capable Typeface (e.g., Noto/Source Han CJK
;;      TC/KR/JP) so it appears in (font-family-list), you can map 'han to it and it will
;;      work immediately.
;;   2) Emacs with Fontconfig (e.g., Termux + X11 or other builds linking Fontconfig) lets
;;      you install Source Han/Noto Serif CJK files and they Just Work (including PUA).
;;
;; - This file therefore:
;;   * Preserves a working macOS configuration (unchanged semantics).
;;   * On Android, tries to:
;;     - Probe and pin a working Hangul family.
;;     - Probe and pin a working Han family from a curated candidate list, verifying
;;       that it actually renders common ideographs before binding.
;;     - Falls back gracefully with clear Messages if no Han-capable family is visible.
;;   * Provides diagnostics and helper commands for continued investigation.

;;; Code

(require 'cl-lib)

;;;; Utilities

(defgroup init-ui nil
  "Cross-platform UI/font setup and Android diagnostics."
  :group 'faces
  :prefix "init-ui-")

(defun init-ui/log (fmt &rest args)
  "Log a formatted message to *Messages*."
  (apply #'message (concat "[init-ui] " fmt) args))

;;;; File-based font helper (works on macOS/Linux; usually ignored by Android backend)

(defun my/font (family filename)
  "Return a font for `set-fontset-font`.

Order:
1) Prefer FAMILY if installed.
2) Use a file font:
   - Android: copy once from ~/.emacs.d/fonts/shared to internal ~/.emacs.d/fonts/, then use internal.
   - Others: use shared file directly if readable.

Return FAMILY (string), (font-spec :file ...), or nil.

Note: On Android's sfnt-android backend, :file fonts are typically ignored at render time,
even if (find-font (font-spec :file …)) is non-nil. See the Android section below."
  (let* ((family-installed (and family (find-font (font-spec :family family))))
         (internal-dir (expand-file-name "~/.emacs.d/fonts/"))
         (internal-file (expand-file-name filename internal-dir))
         (shared-file (expand-file-name (concat "fonts/" filename) user-emacs-directory)))
    (cond
     ;; 1) System family
     (family-installed family)
     ;; 2) Internal copy
     ((file-readable-p internal-file)
      (font-spec :file internal-file))
     ;; 3) Copy shared to internal on Android; otherwise use shared directly
     ((file-readable-p shared-file)
      (if (eq system-type 'android)
          (progn
            (make-directory internal-dir t)
            (unless (file-exists-p internal-file)
              (ignore-errors (copy-file shared-file internal-file)))
            (when (file-readable-p internal-file)
              (font-spec :file internal-file)))
        (font-spec :file shared-file)))
     (t nil))))

;;;; Common helpers for mapping and verification

(defun init-ui/set-font-for (target script-or-range font &optional where priority)
  "Wrapper for `set-fontset-font` with logging.

TARGET is a fontset or nil (current frame).
SCRIPT-OR-RANGE is a script symbol or cons range.
FONT is a family string or (font-spec ...).
WHERE is nil (default), 'prepend or 'append.
PRIORITY is ignored; kept for API clarity."
  (condition-case err
      (progn
        (set-fontset-font target script-or-range font nil (or where 'prepend))
        (init-ui/log "Mapped %S → %S (target=%s where=%s)"
                     script-or-range font (if target target 'current) (or where 'prepend)))
    (error
     (init-ui/log "Failed mapping %S → %S: %s" script-or-range font (error-message-string err)))))

(defun init-ui/displayable-p (ch)
  "Convenience predicate."
  (char-displayable-p ch))

(defun init-ui/verify-mapping (label chars &optional fontset)
  "Report if CHARS display under FONTSET (or current frame) with LABEL."
  (let ((results (mapcar (lambda (c) (list c (char-displayable-p c))) chars)))
    (init-ui/log "%s displayable? %S" label results)
    results))

;;;; macOS (preserve existing, known-good setup)

(when (and (fboundp 'set-fontset-font)
           (eq system-type 'darwin)
           (not (bound-and-true-p cnfonts-mode)))
  ;; Rationale for macOS:
  ;; - Use a simple fontset built from an ASCII base to avoid XLFD issues.
  ;; - PUA → CMUO Serif (contains PUA glyphs).
  ;; - Han → Noto Serif TC (traditional forms; good for classical texts).
  (let ((fontset (create-fontset-from-ascii-font "Noto Serif-12")))
    ;; Private Use Area
    (init-ui/set-font-for fontset '(#xE000 . #xF8FF) "CMUO Serif" 'prepend)
    ;; Han (CJK Unified Ideographs)
    (init-ui/set-font-for fontset 'han "Noto Serif TC" 'prepend)
    ;; Apply to default frames (no custom XLFD name here)
    (add-to-list 'default-frame-alist `(font . ,fontset))
    (init-ui/log "macOS fontset applied: base=Noto Serif-12; PUA=CMUO Serif; Han=Noto Serif TC")))

;;;; Android: probe-driven setup with diagnostics
;;
;; Notes consolidated from experiments (do not delete):
;; - Backend: (frame-parameter nil 'font-backend) → (sfnt-android).
;; - :file fonts are generally ignored by this backend.
;; - set-fontset-font with nil (current frame) and with "fontset-default" both tested.
;; - Hangul became reliable when using an installed family Emacs can see (e.g., "One UI Sans KR VF").
;; - Han failed because no Han-capable Typeface was visible in (font-family-list).
;; - Mapping 'han to SEC/SECFallback/various ranges didn't help on the tested device.
;; - xwidget-webkit not compiled in this build; browser fallback function provided below.

(when (and (fboundp 'set-fontset-font)
           (eq system-type 'android)
           (not (bound-and-true-p cnfonts-mode)))   ;; defer to cnfonts if active

  ;; User-tunable lists of candidate families to probe.
  (defcustom init-ui/android-hangul-candidates
    '("Noto Sans KR" "Noto Serif KR" "One UI Sans KR VF" "SamsungOneKorean" "NanumGothic" "Roboto")
    "Preferred Hangul families to probe on Android, in order."
    :type '(repeat string) :group 'init-ui)

  (defcustom init-ui/android-han-candidates
    '(
      ;; Ideal serif families when available:
      "Noto Serif CJK TC" "Noto Serif CJK KR" "Noto Serif CJK JP" "Source Han Serif TC" "Source Han Serif KR" "Source Han Serif JP"
      ;; Sans fallbacks if serif is not present:
      "Noto Sans CJK TC" "Noto Sans CJK KR" "Noto Sans CJK JP" "Source Han Sans TC" "Source Han Sans KR" "Source Han Sans JP"
      ;; OEM guesses (rarely exported as Typeface names to apps):
      "One UI Sans TC VF" "One UI Sans HK VF" "One UI Sans JP VF" "SEC CJK Regular Extra" "SECFallback"
      )
    "Preferred Han families to probe on Android, in order. The code verifies actual rendering before binding."
    :type '(repeat string) :group 'init-ui)

  ;; Characters used to verify real rendering (don't "trust" just find-font).
  (defcustom init-ui/android-hangul-probe-char ?한
    "Hangul codepoint used to verify mapping."
    :type 'character :group 'init-ui)

  (defcustom init-ui/android-han-probe-chars
    '(?一 ?中 ?國 ?漢)
    "A few very common CJK ideographs to verify Han mapping."
    :type '(repeat character) :group 'init-ui)

  (defun init-ui/android--family-visible-p (family)
    "Return non-nil if FAMILY is visible as a Typeface to Emacs (font-family-list/find-font)."
    (and (member family (font-family-list))
         (find-font (font-spec :family family))))

  (defun init-ui/android--try-bind-and-check (script family &optional probe-chars)
    "Bind SCRIPT to FAMILY, then verify PROBE-CHARS display. Return t if all displayable."
    (when (init-ui/android--family-visible-p family)
      (set-fontset-font nil script family nil 'replace)
      (let* ((chars (or probe-chars (list init-ui/android-hangul-probe-char)))
             (ok-all (cl-every #'char-displayable-p chars)))
        (when ok-all
          ;; Also bind into fontset-default for subsequent frames.
          (set-fontset-font "fontset-default" script family nil 'replace)
          (init-ui/log "Confirmed %s → %s renders %S" script family chars)
          t))))

  (defun init-ui/android--first-working-family (script candidates probe-chars)
    "Return the first FAMILY in CANDIDATES that actually renders PROBE-CHARS for SCRIPT."
    (catch 'found
      (dolist (fam candidates nil)
        (when (init-ui/android--try-bind-and-check script fam probe-chars)
          (throw 'found fam)))))

  (defun init-ui/android-apply-hangul (&optional quiet)
    "Probe and bind a usable Hangul family; persist for new frames."
    (let ((fam (init-ui/android--first-working-family 'hangul
                                                      init-ui/android-hangul-candidates
                                                      (list init-ui/android-hangul-probe-char))))
      (if fam
          (progn
            ;; Ensure new frames inherit the mapping
            (add-hook 'after-make-frame-functions
                      (lambda (f)
                        (with-selected-frame f
                          (ignore-errors
                            (set-fontset-font nil 'hangul fam nil 'prepend))))))
            (unless quiet
              (init-ui/log "Hangul mapped to %s; %c displayable: %s"
                           fam init-ui/android-hangul-probe-char
                           (char-displayable-p init-ui/android-hangul-probe-char)))
            fam)
        (unless quiet
          (init-ui/log "No working Hangul family found among: %S"
                       init-ui/android-hangul-candidates))
        nil)))

  (defun init-ui/android-apply-han (&optional quiet)
    "Probe and bind a usable Han family; persist for new frames. Logs diagnostics on failure."
    (let ((fam (init-ui/android--first-working-family 'han
                                                      init-ui/android-han-candidates
                                                      init-ui/android-han-probe-chars)))
      (if fam
          (progn
            (add-hook 'after-make-frame-functions
                      (lambda (f)
                        (with-selected-frame f
                          (ignore-errors
                            (set-fontset-font nil 'han fam nil 'prepend))))))
            (unless quiet
              (init-ui/log "Han mapped to %s; probe %S displayable: %s"
                           fam init-ui/android-han-probe-chars
                           (cl-every #'char-displayable-p init-ui/android-han-probe-chars)))
            fam)
        (unless quiet
          (init-ui/log "No Han-capable family visible to Emacs. Candidates tried: %S"
                       init-ui/android-han-candidates)
          (init-ui/log "This usually means the sfnt-android backend can't see a CJK ideograph Typeface on this firmware."))
        nil))

  (defun init-ui/android-apply-fonts ()
    "Main entry point for Android. Probe/pin Hangul, then Han; log backend and results.
Guards against ordering issues where helpers might not be defined yet."
    (init-ui/log "Android font-backend: %S" (frame-parameter nil 'font-backend))
    ;; Always try to establish Hangul first (works on more devices).
    (when (fboundp 'init-ui/android-apply-hangul)
      (ignore-errors (init-ui/android-apply-hangul)))
    ;; Then try Han; logs if none found.
    (when (fboundp 'init-ui/android-apply-han)
      (ignore-errors (init-ui/android-apply-han))))

  ;; Run once at startup; also keep a frame hook to inherit mappings.
  (add-hook 'emacs-startup-hook #'init-ui/android-apply-fonts)
  
  ;; Frame hook with guards against void-function errors
  (add-hook 'after-make-frame-functions
            (lambda (f)
              (with-selected-frame f
                (when (fboundp 'init-ui/android-apply-hangul)
                  (ignore-errors (init-ui/android-apply-hangul t)))
                (when (fboundp 'init-ui/android-apply-han)
                  (ignore-errors (init-ui/android-apply-han t))))))

  ;; Diagnostics helpers (M-x friendly)

  (defun init-ui/android-show-cjk-candidates ()
    "Show families that look CJK-like to help manual testing."
    (interactive)
    (let* ((re (regexp-opt '("One UI" "Noto" "Source" "CJK" "Chinese" "SEC" "Samsung" "Nanum")))
           (fams (cl-remove-if-not (lambda (s) (string-match-p re s)) (font-family-list))))
      (message "CJK-ish families: %S" fams)
      fams))

  (defun init-ui/android-probe-hangul-now ()
    "Probe Hangul now and report results."
    (interactive)
    (let ((fam (init-ui/android-apply-hangul)))
      (if fam
          (message "Hangul OK via %s; %c displayable=%s"
                   fam init-ui/android-hangul-probe-char
                   (char-displayable-p init-ui/android-hangul-probe-char))
        (message "No working Hangul family found"))))

  (defun init-ui/android-probe-han-now ()
    "Probe Han now and report results."
    (interactive)
    (let ((fam (init-ui/android-apply-han)))
      (if fam
          (message "Han OK via %s; %S displayable=%s"
                   fam init-ui/android-han-probe-chars
                   (cl-every #'char-displayable-p init-ui/android-han-probe-chars))
        (message "No Han-capable family visible to Emacs"))))

  ;; In-Emacs browser preview (uses Android's system fallback; no xwidgets required).
  (defun init-ui/preview-in-browser (&optional beg end)
    "Preview the region (or whole buffer) in the default browser with UTF-8."
    (interactive)
    (let* ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
           (end (or end (if (use-region-p) (region-end) (point-max))))
           (html (concat "<meta charset='utf-8'>"
                         "<style>body{font-size:160%;line-height:1.6;} blockquote{margin-left:1em}</style>"
                         "<body><blockquote>"
                         (buffer-substring-no-properties beg end)
                         "</blockquote></body>"))
           (file (make-temp-file "han-preview" nil ".html")))
      (with-temp-file file (insert html))
      (browse-url-of-file file)
      (init-ui/log "Opened preview in browser: %s" file)))

;;;; Platform-agnostic UI tweaks

;; Enable visual line wrapping globally (soft-wrap at word boundaries).
(global-visual-line-mode 1)

(provide 'init-ui)
;;; init-ui.el ends here
