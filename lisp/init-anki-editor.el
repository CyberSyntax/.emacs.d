;;; lisp/init-anki-editor.el -*- lexical-binding: t; -*-

(use-package anki-editor
  :ensure t
  :after org
  ;; Global shortcut you asked for:
  :bind (("C-c A" . anki-editor-gui-add-cards)))

(provide 'init-anki-editor)