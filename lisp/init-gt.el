;;; lisp/init-gt.el --- Minimal, robust DeepL EN<->KO setup for gt.el -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Send no extra DeepL params (let DeepL defaults apply).
(setq gt-deepl-extra-params nil)

(use-package gt
  :ensure t
  :config
  ;; Make DeepL accept any language symbol by uppercasing its name when not mapped.
  ;; Avoids maintaining a static mapping
  (with-eval-after-load 'gt-engine-deepl
    (advice-add
     'gt-deepl-lang :around
     (lambda (orig lang)
       (or (ignore-errors (funcall orig lang)) ; mapped? use it
           (upcase (symbol-name lang))))))     ; fallback: 'ko → "KO", 'en → "EN", etc.

  ;; Default translator:
  ;; - Language pair: EN <-> KO (auto-detected from text)
  ;; - Engine: DeepL Pro (api.deepl.com, via authinfo)
  ;;   ~/.authinfo(.gpg):
  ;;   machine api.deepl.com login auth-key password YOUR_DEEPL_KEY
  ;; - Render: dedicated buffer
  (setq gt-default-translator
        (gt-translator
         :taker   (gt-taker :langs '(en ko))
         :engines (gt-deepl-engine :pro t)
         :render  (gt-buffer-render)))

  ;; Enable Visual Line mode only in the *gt-result* buffer.
  (with-eval-after-load 'gt-render-buffer
    (add-hook 'gt-buffer-render-init-hook
              (lambda ()
                (visual-line-mode 1))))

  ;; Hotkey to translate current selection/thing-at-point
  :bind (("C-c t" . gt-translate)))

(provide 'init-gt)

;;; lisp/init-gt.el ends here
