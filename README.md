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

This minimal `init.el` file redirects the native app to load your main configuration. This is a one-time setup. You will need a file manager that can access `/data/data/`.

1.  Navigate to `/data/data/org.gnu.emacs/files/`.
2.  Create a folder named `.emacs.d`.
3.  Inside that folder, create a file named `init.el` with the exact content below.

**File:** `/data/data/org.gnu.emacs/files/.emacs.d/init.el`
```elisp
;;; Minimal Bootstrap for Native Android Emacs -*- lexical-binding: t; -*-
;;; This file redirects Emacs to the main configuration in shared storage.

(setq user-emacs-directory "/storage/emulated/0/Documents/.emacs.d/")

(load-file (expand-file-name "early-init.el" user-emacs-directory))
(load-file (expand-file-name "init.el" user-emacs-directory))
```
