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
           "Noto Serif CJK JP" "Noto Sans CJK JP" "MiSans VF")
          nil nil))
  (setq cnfonts-profiles '("program" "document")
        cnfonts-use-face-font-rescale t
        cnfonts-use-cache t)

  (cnfonts-mode 1)

  ;; Prefer TC for Han and ensure Hangul has a font (mac + android)
  (with-eval-after-load 'cnfonts
    (defun my/font-present-p (fam)
      (and (stringp fam) (member fam (font-family-list))))
    (defun my/first-present (&rest names)
      (cl-find-if #'my/font-present-p names))

    (defun my/apply-cjk-overrides (&rest _)
      (when (display-graphic-p)
        ;; Han → Traditional Chinese family
        (let* ((tc-fam (or
                        (my/first-present
                         "Noto Serif TC" "Noto Serif CJK TC" "Noto Sans CJK TC"
                         "Source Han Serif TC" "Source Han Sans TC"
                         "PingFang TC" "Heiti TC" "Hiragino Sans CNS" "Hiragino Mincho ProN")
                        ;; macOS: fallback to repo font file if the family isn't system-installed
                        (and (eq system-type 'darwin)
                             (let ((f (expand-file-name "fonts/NotoSerifTC-Regular.ttf" user-emacs-directory)))
                               (when (file-readable-p f)
                                 (font-spec :file f))))))
               (kr-fam (my/first-present
                        "Noto Sans KR" "Noto Serif KR" "Apple SD Gothic Neo" "NanumGothic"
                        "One UI Sans KR VF" "SamsungOneKorean" "Roboto")))
          (when tc-fam
            (set-fontset-font "fontset-default" 'han tc-fam nil 'prepend)
            (message "[cnfonts] han → %s"
                     (if (stringp tc-fam) tc-fam (plist-get (cdr tc-fam) :file))))
          (when kr-fam
            (set-fontset-font "fontset-default" 'hangul kr-fam nil 'prepend)
            (message "[cnfonts] hangul → %s" kr-fam)))))

    ;; Run after cnfonts has set its fonts (works on mac and android)
    (add-hook 'cnfonts-set-font-finish-hook #'my/apply-cjk-overrides)

    ;; Extra safety: run again at startup (helps Android timing)
    (add-hook 'emacs-startup-hook
              (lambda () (run-with-timer 0.3 nil #'my/apply-cjk-overrides))))

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
                "One UI Sans KR VF" "SamsungOneKorean" "Roboto")))
    (message "Present: %S"
             (cl-remove-if-not (lambda (x) (member x (font-family-list))) want))))

(provide 'init-cnfonts)
;;; init-cnfonts.el ends here