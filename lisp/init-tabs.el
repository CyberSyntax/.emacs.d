;; lisp/init-tabs.el -*- lexical-binding: t; -*-
(require 'tab-bar)

;; Optional: enable tabs if you don't already do so
;; (tab-bar-mode 1)

(defun my/tab-new-scratch ()
  "Open a new tab and show a scratch buffer."
  (interactive)
  (tab-bar-new-tab)
  (switch-to-buffer (scratch-buffer)))

;; Pick a key that doesn’t clash; adjust to taste
(global-set-key (kbd "C-x t s") #'my/tab-new-scratch)

;; New tab + gptel chat
(with-eval-after-load 'init-gptel
  (defun my/tab-new-gptel ()
    (interactive)
    (tab-bar-new-tab)
    (my-gptel-new-chat)
    (delete-other-windows))
  (global-set-key (kbd "C-x t g") #'my/tab-new-gptel))

(provide 'init-tabs)