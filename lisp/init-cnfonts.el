;;; init-cnfonts.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(when (and (display-graphic-p)
           (require 'cnfonts nil t))

  ;; Keys
  (global-set-key (kbd "C-M-_") #'cnfonts-decrease-fontsize)
  (global-set-key (kbd "C-M-+") #'cnfonts-increase-fontsize)
  (global-set-key (kbd "C-M-)") #'cnfonts-reset-fontsize)

  ;; Personal font names
  (setq cnfonts-personal-fontnames
        '(("JetBrains Mono" "Fira Code" "Menlo" "Monaco" "Hack")
          ("Noto Serif TC" "Noto Serif CJK TC" "Noto Sans CJK TC"
           "Source Han Serif TC" "Source Han Sans TC"
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

  ;; Apply mappings after cnfonts base fonts
  (defun my/apply-cjk-overrides (&rest _)
    (when (display-graphic-p)

      ;; 1) Han → Traditional Chinese
      (let* ((tc-fam (or
                      (my/first-present
                       "Noto Serif TC" "Noto Serif CJK TC" "Noto Sans CJK TC"
                       "Source Han Serif TC" "Source Han Sans TC"
                       "PingFang TC" "Heiti TC" "Hiragino Sans CNS" "Hiragino Mincho ProN")
                      (and (eq system-type 'darwin)
                           (let ((f (expand-file-name "fonts/NotoSerifTC-Regular.ttf" user-emacs-directory)))
                             (when (file-readable-p f) (font-spec :file f))))))
             (ok nil))
        (when tc-fam
          (set-fontset-font "fontset-default" 'han tc-fam nil 'prepend)
          (set-fontset-font "fontset-default" '(#x4E00 . #x9FFF) tc-fam nil 'prepend)
          (set-fontset-font "fontset-default" '(#x3400 . #x4DBF) tc-fam nil 'prepend)
          (setq ok (char-displayable-p ?法))
          (message "[cnfonts] han → %s (法 displayable: %s)"
                   (if (stringp tc-fam) tc-fam (plist-get (cdr tc-fam) :file)) ok)))

      ;; 2) Hangul → Korean
      (let ((kr-fam (my/first-present
                     "Noto Sans KR" "Noto Serif KR" "One UI Sans KR VF"
                     "SamsungOneKorean" "NanumGothic" "Roboto")))
        (when kr-fam
          (set-fontset-font "fontset-default" 'hangul kr-fam nil 'prepend)
          (message "[cnfonts] hangul → %s" kr-fam)))

      ;; 3) PUA → CMUO (mac: family or file; Android: family-only)
      (let* ((pua-range '(#xE000 . #xF8FF))
             (fams (font-family-list))
             (fam (or (cl-find-if (lambda (s) (string-match-p "\\bCMUO\\b" s)) fams)
                      (cl-find-if (lambda (s) (string-match-p "\\bCMU\\b.*\\bSerif\\b" s)) fams))))
        (cond
         (fam
          ;; Highest precedence for PUA; map both current and default; disable symbol fallback on mac
          (set-fontset-font nil               pua-range fam nil 'replace)
          (set-fontset-font "fontset-default" pua-range fam nil 'replace)
          (when (eq system-type 'darwin)
            (setq use-default-font-for-symbols nil))
          (message "[cnfonts] PUA → family %s (replace)%s"
                   fam (if (eq system-type 'darwin) ", symbols=nil" "")))
         ((eq system-type 'darwin)
          ;; File fallback only on mac
          (let* ((try (list
                       (expand-file-name "fonts/CMUOSerif-Roman.ttf" user-emacs-directory)
                       (expand-file-name "fonts/CMUOSerif-Roman.otf" user-emacs-directory)))
                 (file (cl-find-if #'file-readable-p try)))
            (when file
              (set-fontset-font nil               pua-range (font-spec :file file) nil 'replace)
              (set-fontset-font "fontset-default" pua-range (font-spec :file file) nil 'replace)
              (setq use-default-font-for-symbols nil)
              (message "[cnfonts] PUA → file %s (replace), symbols=nil"
                       (file-name-nondirectory file)))))))))

  ;; Run after cnfonts finishes, and once more at startup (Android timing)
  (with-eval-after-load 'cnfonts
    (add-hook 'cnfonts-set-font-finish-hook #'my/apply-cjk-overrides)
    (add-hook 'emacs-startup-hook (lambda () (run-with-timer 0.3 nil #'my/apply-cjk-overrides))))

  ;; Defer to cnfonts on Android (don't fight init-ui)
  (with-eval-after-load 'init-ui
    (when (eq system-type 'android)
      (dolist (fn '(init-ui/android-apply-fonts
                    init-ui/android-apply-hangul
                    init-ui/android-apply-han))
        (when (fboundp fn)
          (advice-add fn :around
                      (let ((name (symbol-name fn)))
                        (lambda (orig &rest args)
                          (if (bound-and-true-p cnfonts-mode)
                              (ignore (message "[cnfonts] skipping %s" name))
                            (apply orig args))))))))))

;; Quick diagnostic
(defun my/cjk-font-diag ()
  "Show which TC/KR/CMUO families Emacs sees."
  (interactive)
  (let ((want '("Noto Serif TC" "Noto Serif CJK TC" "Noto Sans CJK TC"
                "Source Han Serif TC" "Source Han Sans TC"
                "Noto Sans KR" "Noto Serif KR" "One UI Sans KR VF"
                "SamsungOneKorean" "NanumGothic" "Roboto"))
        (cmuo (cl-remove-if-not (lambda (s) (string-match-p "CMUO" s))
                                (font-family-list))))
    (message "TC/KR present: %S | CMUO present: %S"
             (cl-remove-if-not (lambda (x) (member x (font-family-list))) want)
             cmuo)))

(provide 'init-cnfonts)
;;; init-cnfonts.el ends here