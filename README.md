# Emacs Configuration

## General Setup

1.  In Emacs, create the file `~/.authinfo` by pressing `C-x C-f`, typing the path, and adding the following line:

    ```
    machine openrouter.ai login apikey password YOUR_API_KEY_HERE
    ```
    *(Replace `YOUR_API_KEY_HERE` with your actual key.)*


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
          ;; Collect only font files (recursive), not directories.
          (cl-labels
              ((collect (dir)
                        (when (file-directory-p dir)
                          (directory-files-recursively dir "\\.\\(ttf\\|otf\\|ttc\\|otc\\)\\'")))
               (ext (f) (downcase (or (file-name-extension f) ""))))
            (let* ((src-files    (collect src))
                   (legacy-files (collect legacy))
                   (all (append src-files legacy-files))
                   (by-base (make-hash-table :test 'equal))
                   (copied 0))
              ;; Prefer TTF for the same basename.
              (dolist (f all)
                (let* ((base (file-name-base f))
                       (e    (ext f))
                       (cur  (gethash base by-base)))
                  (cond
                   ((string= e "ttf") (puthash base f by-base))      ;; TTF overrides any prior
                   ((null cur)        (puthash base f by-base)))))    ;; first non-TTF wins if no TTF

              ;; Copy resolved files
              (maphash
               (lambda (_base f)
                 (let* ((out (expand-file-name (file-name-nondirectory f) dst)))
                   (condition-case e2
                       (progn
                         ;; OK-IF-EXISTS=t, KEEP-TIME=t, PRESERVE-UID/GID=nil (Android-safe)
                         (copy-file f out t t nil)
                         (cl-incf copied))
                     (error
                      (my/int-log "copy failed %s -> %s: %s" f out (error-message-string e2))))))
               by-base)

              (my/int-log "pre-flight: copied %d file(s) to ~/fonts" copied))))
      (error
       (my/int-log "pre-flight copy failed: %s" (error-message-string err))))))
;; Optional: nudge registration
(ignore-errors (font-family-list))

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
;;; lisp/init-local.el --- Local and private configuration -*- lexical-binding: t; -*-

;; This file is for local, machine-specific settings that should not be committed to Git.
;; It extends the standard Emacs authentication mechanism for this specific environment.

;; Add our private, untracked authentication file to the list of sources
;; that Emacs will check for credentials.
(add-to-list 'auth-sources (expand-file-name ".authinfo" (user-emacs-directory load-file-name)))

(provide 'init-local)
;;; init-local.el ends here
```