## Setup for Termux Android Emacs

This guide outlines the essential steps to configure the Termux Android Emacs app using this setup.

### Prerequisites

* Termux and the native Android Emacs app are installed.
* `git` is installed in Termux (`pkg install git`).

### Step 1: Prepare Termux and Clone the Configuration

First, grant Termux access to shared storage, then navigate to your `Documents` folder and clone this repository.

1. In Termux, run the one-time setup command:
    ```bash
    termux-setup-storage
    ```
    A system dialog will appear asking for permission. Make sure to **allow** it.

2. Clone the repository. Git will automatically create the `.emacs.d` directory:
    ```bash
    git clone https://github.com/CyberSyntax/.emacs.d.git
    ```
