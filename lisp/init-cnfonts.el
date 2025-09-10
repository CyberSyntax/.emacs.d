;;; init-cnfonts.el -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Loaded after init-vendor; cnfonts is on load-path if vendored.
(when (and (display-graphic-p)
           (require 'cnfonts nil t))

  ;; Keys
  (global-set-key (kbd "C-M-_") #'cnfonts-decrease-fontsize)
  (global-set-key (kbd "C-M-+") #'cnfonts-increase-fontsize)
  (global-set-key (kbd "C-M-)") #'cnfonts-reset-fontsize)

  ;; Minimal defaults; adjust names to match your (font-family-list)
  ;; 1st list: ASCII, 2nd: CJK (prefer TC), 3rd/4th: optional (ExtB/Symbol)
  (setq cnfonts-personal-fontnames
        '(("JetBrains Mono" "Fira Code" "Menlo" "Monaco" "Hack")
          ("Noto Serif TC" "Noto Serif CJK TC" "Noto Sans CJK TC" "Source Han Serif TC" "Source Han Sans TC"
           ;; fallbacks if TC missing
           "Noto Serif CJK JP" "Noto Sans CJK JP")
          nil nil))
  (setq cnfonts-profiles '("program" "document")
        cnfonts-use-face-font-rescale t
        cnfonts-use-cache t)

  (cnfonts-mode 1)

  ;; Helpers
  (defun my/font-present-p (fam)
    (and (stringp fam) (member fam (font-family-list))))
  (defun my/first-present (&rest names)
    (cl-find-if #'my/font-present-p names))
  (defun my/readable-first (&rest files)
    (cl-find-if #'file-readable-p files))

  (defun my/android-font (rel)
    (expand-file-name rel (expand-file-name "~/fonts")))

  ;; Prefer TC for Han and ensure Hangul + PUA mappings (mac + android)
  (defun my/apply-cjk-overrides (&rest _)
    (when (display-graphic-p)
      ;; 1) Han → Traditional Chinese family (prefer family, else file on Android/mac)
      (let* ((tc-fam (or
                      (my/first-present
                       "Noto Serif TC" "Noto Serif CJK TC" "Noto Sans CJK TC"
                       "Source Han Serif TC" "Source Han Sans TC"
                       "PingFang TC" "Heiti TC" "Hiragino Sans CNS" "Hiragino Mincho ProN")
                      ;; macOS: allow :file fallback from repo if needed
                      (and (eq system-type 'darwin)
                           (let ((f (expand-file-name "fonts/NotoSerifTC-Regular.ttf" user-emacs-directory)))
                             (when (file-readable-p f) (font-spec :file f))))
                      ;; Android: fallback to files in ~/fonts
                      (and (eq system-type 'android)
                           (let ((p (my/readable-first
                                     (my/android-font "NotoSerifTC-Regular.ttf")
                                     (my/android-font "NotoSerifTC-Medium.ttf")
                                     (my/android-font "NotoSerifTC-Light.ttf")
                                     (my/android-font "NotoSerifTC-VariableFont_wght.ttf"))))
                             (when p (font-spec :file p))))))
             (han-ok nil))
        (when tc-fam
          ;; Bind script 'han (covers U+4E00..9FFF)
          (set-fontset-font "fontset-default" 'han tc-fam nil 'prepend)
          ;; Also bind core CJK ranges explicitly as extra safety
          (set-fontset-font "fontset-default" '(#x4E00 . #x9FFF) tc-fam nil 'prepend)  ; CJK Unified
          (set-fontset-font "fontset-default" '(#x3400 . #x4DBF) tc-fam nil 'prepend)  ; Ext-A
          ;; quick probe for a common char (法)
          (setq han-ok (char-displayable-p ?法))
          (message "[cnfonts] han → %s (法 displayable: %s)"
                   (if (stringp tc-fam) tc-fam (plist-get (cdr tc-fam) :file))
                   han-ok))

        ;; 2) Hangul → Korean family (prefer family; Android already has One UI Sans KR VF)
        (let ((kr-fam (or
                       (my/first-present
                        "Noto Sans KR" "Noto Serif KR" "Apple SD Gothic Neo" "NanumGothic"
                        "One UI Sans KR VF" "SamsungOneKorean" "Roboto")
                       ;; Android file fallback (rarely needed since One UI family is present)
                       (and (eq system-type 'android)
                            (let ((p (my/readable-first
                                      (my/android-font "NotoSansKR-Regular.ttf")
                                      (my/android-font "NotoSansKR-Medium.ttf")
                                      (my/android-font "NotoSansKR-VariableFont_wght.ttf"))))
                              (when p (font-spec :file p)))))))
          (when kr-fam
            (set-fontset-font "fontset-default" 'hangul kr-fam nil 'prepend)
            (message "[cnfonts] hangul → %s"
                     (if (stringp kr-fam) kr-fam (plist-get (cdr kr-fam) :file)))))

        ;; 3) PUA → CMUO Serif (contains PUA glyphs); file fallback on Android
        (let ((pua-fam (or
                        (my/first-present "CMUO Serif" "CMU Serif" "Symbola")
                        (and (eq system-type 'android)
                             (let ((p (my/readable-first
                                       (my/android-font "CMUOSerif-Roman.ttf")
                                       (my/android-font "CMUOSerif-Roman.otf"))))
                               (when p (font-spec :file p)))))))
          (when pua-fam
            (set-fontset-font "fontset-default" '(#xE000 . #xF8FF) pua-fam nil 'prepend)
            (message "[cnfonts] PUA → %s"
                     (if (stringp pua-fam) pua-fam (plist-get (cdr pua-fam) :file)))))))

  ;; Run after cnfonts has set its fonts, and once more at startup (Android timing)
  (with-eval-after-load 'cnfonts
    (add-hook 'cnfonts-set-font-finish-hook #'my/apply-cjk-overrides)
    (add-hook 'emacs-startup-hook (lambda () (run-with-timer 0.3 nil #'my/apply-cjk-overrides))))

  ;; If cnfonts is active on Android, let it own font mapping (don't fight init-ui).
  (with-eval-after-load 'init-ui
    (when (eq system-type 'android)
      (dolist (fn '(init-ui/android-apply-fonts
                    init-ui/android-apply-hangul
                    init-ui/android-apply-han
                    init-ui/android-apply-pua))
        (when (fboundp fn)
          (advice-add fn :around
                      (let ((name (symbol-name fn)))
                        (lambda (orig &rest args)
                          (if (bound-and-true-p cnfonts-mode)
                              (ignore (message "[cnfonts] skipping %s" name))
                            (apply orig args))))))))))

;; Quick diagnostic: which TC/KR families are visible
(defun my/cjk-font-diag ()
  "Show which TC/KR families Emacs sees."
  (interactive)
  (let ((want '("Noto Serif TC" "Noto Serif CJK TC" "Noto Sans CJK TC"
                "Source Han Serif TC" "Source Han Sans TC"
                "PingFang TC" "Heiti TC" "Hiragino Sans CNS" "Hiragino Mincho ProN"
                "Noto Sans KR" "Noto Serif KR" "Apple SD Gothic Neo" "NanumGothic"
                "One UI Sans KR VF" "SamsungOneKorean" "Roboto"
                "CMUO Serif" "CMU Serif" "Symbola")))
    (message "Present: %S"
             (cl-remove-if-not (lambda (x) (member x (font-family-list))) want))))

(provide 'init-cnfonts)
;;; init-cnfonts.el ends here