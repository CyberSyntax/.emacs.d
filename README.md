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

**File:** `/data/data/org.gnu.emacs/files/.emacs.d/init.el`
```elisp
;;; Minimal Bootstrap for Native Android Emacs -*- lexical-binding: t; -*-
;;; This file redirects Emacs to the main configuration in shared storage.

(setq user-emacs-directory "/storage/emulated/0/Documents/.emacs.d/")

(load-file (expand-file-name "early-init.el" user-emacs-directory))
(load-file (expand-file-name "init.el" user-emacs-directory))
```

### Step 3: Local and private configuration

File: /data/data/org.gnu.emacs/files/.emacs.d/early-init.el
```elisp
;;; internal early-init bootstrap -*- lexical-binding: t; -*-
;; Redirect early-init to the shared repo, then run it.

(setq user-emacs-directory "/storage/emulated/0/Documents/.emacs.d/")

(load (expand-file-name "early-init.el" user-emacs-directory) nil 'nomessage)
```

File: /data/data/org.gnu.emacs/files/.emacs.d/init.el
```elisp
;;; internal init bootstrap -*- lexical-binding: t; -*-
;; Redirect init to the shared repo. Do NOT load early-init here.

(setq user-emacs-directory "/storage/emulated/0/Documents/.emacs.d/")

(load (expand-file-name "init.el" user-emacs-directory) nil 'nomessage)

(custom-set-variables
 '(package-selected-packages nil))
(custom-set-faces)
```
