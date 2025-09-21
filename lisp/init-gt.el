;;; lisp/init-gt.el --- Minimal, robust DeepL EN<->KO setup for gt.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(use-package gt
  :ensure t
  :config
  ;; Make DeepL accept any language symbol by uppercasing its name when not mapped.
  ;; This avoids maintaining a per-language table (e.g., 'ko → "KO").
  (with-eval-after-load 'gt-engine-deepl
    (advice-add
     'gt-deepl-lang :around
     (lambda (orig lang)
       (or (ignore-errors (funcall orig lang)) ; mapped? use it
           (upcase (symbol-name lang))))))     ; fallback: 'ko → "KO", 'en → "EN", etc.

  ;; Default translator:
  ;; - Language pair: EN <-> KO (auto-detected from text)
  ;; - Engine: DeepL (Pro endpoint api.deepl.com, via authinfo)
  ;;   Put your key in ~/.authinfo(.gpg):
  ;;   machine api.deepl.com login auth-key password YOUR_DEEPL_KEY
  ;; - Render: dedicated buffer with key hints and cycling
  (setq gt-default-translator
        (gt-translator
         :taker   (gt-taker :langs '(en ko))
         :engines (gt-deepl-engine :pro t)
         :render  (gt-buffer-render)))

  ;; Hotkey to translate current selection/thing-at-point
  :bind (("C-c t" . gt-translate)))

(provide 'init-gt)

;;; lisp/init-gt.el ends here