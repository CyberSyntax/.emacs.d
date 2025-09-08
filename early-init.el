;;; early-init.el --- Bridge to Termux environment for Native Android Emacs -*- lexical-binding: t; -*-

(when (string= system-type "android")
  (let* ((prefix "/data/data/com.termux/files/")
         (path (expand-file-name "usr/bin" prefix)))
    (setenv "PREFIX" prefix)
    (setenv "PATH" (format "%s:%s" (getenv "PATH") path))
    (setf exec-path (cl-delete-if-not #'file-readable-p exec-path)
          exec-path (nconc (butlast exec-path) (cons path (last exec-path))))))

;;; early-init.el ends here
