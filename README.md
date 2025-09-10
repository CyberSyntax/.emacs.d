## Setup for Native Android Emacs

This guide outlines the essential steps to make the Native Android Emacs app use this configuration, which is managed via Git inside Termux.

### Prerequisites

*   Termux and the Native Android Emacs app are installed.
*   `git` is installed in Termux (`pkg install git`).

### Step 1: Prepare Termux and Clone the Configuration

First, grant Termux access to shared storage, then navigate to your `Documents` folder and clone this repository.

1.  In Termux, run the one-time setup command:
    ```bash
    termux-setup-storage
    ```
    (A system dialog will pop up asking for permission. You must **Allow** it.)

2.  Navigate into your shared `Documents` folder:
    ```bash
    cd ~/storage/shared/Documents
    ```

3.  Clone the repository. Git will automatically create the `.emacs.d` directory here.
    ```bash
    git clone https://github.com/CyberSyntax/.emacs.d.git
    ```

### Step 2: Create the Native Emacs Bootstrap File

File: /data/data/org.gnu.emacs/files/.emacs.d/early-init.el
```elisp
;;; internal early-init bootstrap -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'timer)

(defvar my/internal-shared-emacs-dir
  "/storage/emulated/0/Documents/.emacs.d/"
  "Path to shared .emacs.d on external storage.")

(defun my/int-log (fmt &rest args)
  (apply #'message (concat "[internal-early] " fmt) args))

;; Point Emacs at your shared repo early
(setq user-emacs-directory my/internal-shared-emacs-dir)

;; Android pre-flight: copy fonts from shared repo → ~/fonts (TTF preferred)
(when (eq system-type 'android)
  (let* ((src (expand-file-name "fonts" user-emacs-directory))
         (legacy (expand-file-name "~/.emacs.d/fonts"))
         (dst (expand-file-name "~/fonts")))
    (my/int-log "pre-flight: src=%s legacy=%s dst=%s" src legacy dst)
    (condition-case err
        (progn
          (make-directory dst t)
          (cl-labels
              ((collect (dir)
                        (when (file-directory-p dir)
                          (directory-files dir t "\\`[^.]\\|\\`\\.[^.]"
                                           t)))
               (copy-one (file)
                         (when (and (file-regular-p file)
                                    (member (downcase (file-name-extension file ""))
                                            '("ttf" "otf" "ttc" "otc")))
                           (let* ((out (expand-file-name (file-name-nondirectory file) dst)))
                             (copy-file file out t t nil)
                             1))))
            (let ((list-src (collect src))
                  (list-legacy (collect legacy))
                  (copied 0))
              (when list-src
                ;; TTF first
                (dolist (f list-src)
                  (when (string-equal (downcase (file-name-extension f "")) "ttf")
                    (cl-incf copied (or (copy-one f) 0))))
                ;; Non-TTF after
                (dolist (f list-src)
                  (unless (string-equal (downcase (file-name-extension f "")) "ttf")
                    (cl-incf copied (or (copy-one f) 0)))))
              (when list-legacy
                (dolist (f list-legacy)
                  (cl-incf copied (or (copy-one f) 0))))
              (my/int-log "pre-flight: copied %d file(s) to ~/fonts" copied)))
          ;; Nudge registration
          (ignore-errors (font-family-list)))
      (error
       (my/int-log "pre-flight copy failed: %s" (error-message-string err))))))

;; Load the shared early-init now (repo takes over)
(let ((shared (expand-file-name "early-init.el" user-emacs-directory)))
  (if (file-readable-p shared)
      (progn
        (my/int-log "loading shared early-init: %s" shared)
        (load shared nil 'nomessage)
        (my/int-log "loaded shared early-init"))
    (my/int-log "shared early-init not found/readable: %s" shared)))

;;; early-init.el ends here
```

File: /data/data/org.gnu.emacs/files/.emacs.d/init.el
```elisp
;;; internal init bootstrap -*- lexical-binding: t; -*-

;; Redirect init to the shared repo. Do NOT load early-init here.

(setq user-emacs-directory "/storage/emulated/0/Documents/.emacs.d/")

(let ((shared (expand-file-name "init.el" user-emacs-directory)))
  (if (file-readable-p shared)
      (progn
        (message "[internal-init] loading shared init: %s" shared)
        (load shared nil 'nomessage)
        (message "[internal-init] loaded shared init"))
    (message "[internal-init] shared init not found/readable: %s" shared)))

;; Optional: keep any Custom block you want here
(custom-set-variables
 '(package-selected-packages nil))
(custom-set-faces)

;;; init.el ends here
```

### Step 3: Local and private configuration

**File:** `/data/data/com.termux/files/home/storage/shared/Documents/.emacs.d/lisp/init-local.el`
```elisp
;;; init-local.el --- Local and private configuration -*- lexical-binding: t; -*-

;; This file is for personal settings that should not be committed to Git.

(provide 'init-local)
;;; init-local.el ends here
```
