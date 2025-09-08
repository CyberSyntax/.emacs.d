;;; lisp/init-android.el --- Android-specific support -*- lexical-binding: t; -*-

;; This module is only active when running in native Android Emacs.
;; It does NOT assume Termux is present. The vendor system will fetch
;; the optional 'android-support' package if network allows.
(when (eq system-type 'android)
  (let ((lib "android-support"))
    (if (locate-library lib)
        (require 'android-support)
      (message "init-android: '%s' not found yet; skipping for now." lib))))

(provide 'init-android)

;;; lisp/init-android.el ends here