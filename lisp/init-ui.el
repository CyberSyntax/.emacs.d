;;; init-ui.el --- Cross-platform UI/font setup (Android: families only) -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup init-ui nil
  "Cross-platform UI/font setup."
  :group 'faces
  :prefix "init-ui-")

(defun init-ui/log (fmt &rest args)
  (apply #'message (concat "[init-ui] " fmt) args))

;;;; File-based font helper (no copying; Android relies on ~/fonts families)

(defun my/font (family filename)
  "Return a font for set-fontset-font without copying anything.
Order:
1) If FAMILY is installed, return the family name.
2) Else, use a shared file font from repo (.emacs.d/fonts/FILENAME) if readable (non-Android).
3) Else, if a staged file exists at ~/.emacs.d/fonts/FILENAME (non-Android), use that.
On Android, return nil (file-based mapping is usually ignored by the text backend)."
  (cond
   ((and family (find-font (font-spec :family family))) family)
   ((eq system-type 'android) nil)
   (t
    (let* ((shared-file (expand-file-name (concat "fonts/" filename) user-emacs-directory))
           (internal-file (expand-file-name filename (expand-file-name "~/.emacs.d/fonts/"))))
      (cond
       ((file-readable-p shared-file)  (font-spec :file shared-file))
       ((file-readable-p internal-file) (font-spec :file internal-file))
       (t nil))))))

(defun init-ui/set-font-for (target script-or-range font &optional where priority)
  (condition-case err
      (progn
        (set-fontset-font target script-or-range font nil (or where 'prepend))
        (init-ui/log "Mapped %S → %S (target=%s where=%s)"
                     script-or-range font (if target target 'current) (or where 'prepend)))
    (error
     (init-ui/log "Failed mapping %S → %S: %s" script-or-range font (error-message-string err)))))

(defun init-ui/displayable-p (ch) (char-displayable-p ch))

(defun init-ui/verify-mapping (label chars &optional fontset)
  (let ((results (mapcar (lambda (c) (list c (char-displayable-p c))) chars)))
    (init-ui/log "%s displayable? %S" label results)
    results))

;;;; macOS (preserve existing, known-good setup)
(when (and (fboundp 'set-fontset-font)
           (eq system-type 'darwin)
           (not (bound-and-true-p cnfonts-mode)))
  (let ((fontset (create-fontset-from-ascii-font "Noto Serif-12")))
    (init-ui/set-font-for fontset '(#xE000 . #xF8FF) "CMUO Serif" 'prepend)
    (init-ui/set-font-for fontset 'han "Noto Serif TC" 'prepend)
    (add-to-list 'default-frame-alist `(font . ,fontset))
    (init-ui/log "macOS fontset applied: base=Noto Serif-12; PUA=CMUO Serif; Han=Noto Serif TC")))

;;;; Android: families only (no :file mapping)
(when (and (fboundp 'set-fontset-font)
           (eq system-type 'android)
           (not (bound-and-true-p cnfonts-mode)))

  (defcustom init-ui/android-hangul-candidates
    '("Noto Sans KR" "Noto Serif KR" "One UI Sans KR VF" "SamsungOneKorean" "NanumGothic" "Roboto")
    "Preferred Hangul families to probe on Android, in order."
    :type '(repeat string) :group 'init-ui)

  (defcustom init-ui/android-han-candidates
    '("Noto Serif CJK TC" "Noto Serif CJK KR" "Noto Serif CJK JP"
      "Source Han Serif TC" "Source Han Serif KR" "Source Han Serif JP"
      "Noto Sans CJK TC" "Noto Sans CJK KR" "Noto Sans CJK JP"
      "Source Han Sans TC" "Source Han Sans KR" "Source Han Sans JP"
      "One UI Sans TC VF" "One UI Sans HK VF" "One UI Sans JP VF"
      "SEC CJK Regular Extra" "SECFallback")
    "Preferred Han families to probe on Android, in order."
    :type '(repeat string) :group 'init-ui)

  (defcustom init-ui/android-hangul-probe-char ?한
    "Hangul codepoint used to verify mapping."
    :type 'character :group 'init-ui)

  (defcustom init-ui/android-han-probe-chars '(?一 ?中 ?國 ?漢)
    "Common CJK ideographs to verify Han mapping."
    :type '(repeat character) :group 'init-ui)

  (defun init-ui/android--family-visible-p (family)
    (and (member family (font-family-list))
         (find-font (font-spec :family family))))

  (defun init-ui/android--try-bind-and-check (script family &optional probe-chars)
    (when (init-ui/android--family-visible-p family)
      (set-fontset-font nil script family nil 'replace)
      (let* ((chars (or probe-chars (list init-ui/android-hangul-probe-char)))
             (ok-all (cl-every #'char-displayable-p chars)))
        (when ok-all
          (set-fontset-font "fontset-default" script family nil 'replace)
          (init-ui/log "Confirmed %s → %s renders %S" script family chars)
          t))))

  (defun init-ui/android--first-working-family (script candidates probe-chars)
    (catch 'found
      (dolist (fam candidates nil)
        (when (init-ui/android--try-bind-and-check script fam probe-chars)
          (throw 'found fam)))))

  (defun init-ui/android-apply-hangul (&optional quiet)
    (let ((fam (init-ui/android--first-working-family 'hangul
                                                      init-ui/android-hangul-candidates
                                                      (list init-ui/android-hangul-probe-char))))
      (if fam
          (progn
            (add-hook 'after-make-frame-functions
                      (lambda (f)
                        (with-selected-frame f
                          (ignore-errors (set-fontset-font nil 'hangul fam nil 'prepend))))))
            (unless quiet (init-ui/log "Hangul mapped to %s" fam))
            fam)
        (unless quiet (init-ui/log "No working Hangul family found"))
        nil)))

  (defun init-ui/android-apply-han (&optional quiet)
    (let ((fam (init-ui/android--first-working-family 'han
                                                      init-ui/android-han-candidates
                                                      init-ui/android-han-probe-chars)))
      (if fam
          (progn
            (add-hook 'after-make-frame-functions
                      (lambda (f)
                        (with-selected-frame f
                          (ignore-errors (set-fontset-font nil 'han fam nil 'prepend))))))
            (unless quiet (init-ui/log "Han mapped to %s" fam))
            fam)
        (unless quiet (init-ui/log "No Han-capable family visible to Emacs"))
        nil)))

  (defun init-ui/android-apply-fonts ()
    (init-ui/log "Android font-backend: %S" (frame-parameter nil 'font-backend))
    (when (fboundp 'init-ui/android-apply-hangul) (ignore-errors (init-ui/android-apply-hangul)))
    (when (fboundp 'init-ui/android-apply-han)    (ignore-errors (init-ui/android-apply-han))))
  (add-hook 'emacs-startup-hook #'init-ui/android-apply-fonts)

  (add-hook 'after-make-frame-functions
            (lambda (f)
              (with-selected-frame f
                (ignore-errors (init-ui/android-apply-hangul t))
                (ignore-errors (init-ui/android-apply-han t)))))

  (defun init-ui/android-show-cjk-candidates ()
    (interactive)
    (let* ((re (regexp-opt '("One UI" "Noto" "Source" "CJK" "SEC" "Samsung" "Nanum")))
           (fams (cl-remove-if-not (lambda (s) (string-match-p re s)) (font-family-list))))
      (message "CJK-ish families: %S" fams)
      fams)))

;; Platform-agnostic UI tweaks
(global-visual-line-mode 1)

(provide 'init-ui)
;;; init-ui.el ends here