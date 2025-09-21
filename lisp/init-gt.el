;;; lisp/init-gt.el --- Minimal translation setup -*- lexical-binding: t; -*-

(use-package gt
  :ensure t
  :init
  ;; Set the API key BEFORE the package is configured
  (when (my-authinfo-get-token "api.deepl.com")
    (setq gt-deepl-api-key (my-authinfo-get-token "api.deepl.com")))
  :config
  ;; Now create the translator (the API key is already set)
  (setq gt-default-translator
        (gt-translator
         :taker (gt-taker :langs '(en ko))
         :engines (gt-deepl-engine)
         :render (gt-buffer-render)))
  
  ;; Key binding for translation
  :bind ("C-c t" . gt-translate))

(provide 'init-gt)

;;; lisp/init-gt.el ends here
