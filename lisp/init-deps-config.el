;;; lisp/init-deps-config.el --- Shared dependency configuration -*- lexical-binding: t; -*-

(defconst my-required-libraries
  '("gptel" "org" "org-roam" "org-roam-ui" "fsrs" "org-srs"
    "yasnippet" "org-web-tools" "transient" "gt"
    "org-queue" "org-story" "hanja-reading" "org-headline-manager"
    "android-support" "cnfonts" "anki-editor"))

(defconst my-deps-record-file
  (expand-file-name "var/deps.done" user-emacs-directory)
  "Completion marker file for dependency installation.")

(defvar my-deps-complete
  (and (file-exists-p my-deps-record-file)
       (ignore-errors
         (with-temp-buffer
           (insert-file-contents my-deps-record-file)
           (goto-char (point-min))
           (re-search-forward "\\bok\\b" nil t)))))

(defun my-deps--record-success ()
  (make-directory (file-name-directory my-deps-record-file) t)
  (with-temp-file my-deps-record-file
    (insert "ok\n"))
  (setq my-deps-complete t))

(provide 'init-deps-config)
;;; lisp/init-deps-config.el ends here
