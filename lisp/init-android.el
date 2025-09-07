;;; lisp/init-android.el --- Android-specific support -*- lexical-binding: t; -*-

;; This entire module is only active when running on Android via Termux.
(when (eq system-type 'android)
  (require 'android-support))

(provide 'init-android)
;;; init-android.el ends here