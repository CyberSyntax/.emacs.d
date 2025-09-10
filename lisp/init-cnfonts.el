;;; init-cnfonts.el -*- lexical-binding: t; -*-

;; Loaded after init-vendor; cnfonts is on load-path if vendored.

(when (and (display-graphic-p)
           (require 'cnfonts nil t))
  ;; Keys
  (global-set-key (kbd "C-M-_") #'cnfonts-decrease-fontsize)
  (global-set-key (kbd "C-M-+") #'cnfonts-increase-fontsize)
  (global-set-key (kbd "C-M-)") #'cnfonts-reset-fontsize)

  ;; Minimal defaults; adjust names to match your (font-family-list)
  (setq cnfonts-personal-fontnames
        '(("JetBrains Mono" "Fira Code" "Menlo" "Monaco" "Hack")
          ("Noto Serif CJK JP" "Noto Serif CJK TC" "Noto Sans CJK JP" "Source Han Serif JP" "MiSans VF")
          nil nil))
  (setq cnfonts-profiles '("program" "document")
        cnfonts-use-face-font-rescale t
        cnfonts-use-cache t)

  (cnfonts-mode 1)

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

(provide 'init-cnfonts)
;;; init-cnfonts.el ends here
