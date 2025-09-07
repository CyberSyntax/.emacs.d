;;; lisp/init-gptel.el -*- lexical-binding: t -*-

(use-package gptel
  :ensure t
  :init
  ;; Robust streaming + media support
  (setq gptel-use-curl t
        gptel-track-media t)
  ;; Include reasoning text (optional, leave default if your gptel is older)
  (when (boundp 'gptel-include-reasoning)
    (setq gptel-include-reasoning 'include))
  :config
  ;; Read OpenRouter key from env or authinfo
  (defun my/openrouter-api-key ()
    (or (getenv "OPENROUTER_API_KEY")
        (ignore-errors
          (let ((auth-sources '("~/.authinfo.gpg" "~/.authinfo")))
            (gptel-api-key-from-auth-source "openrouter.ai")))
        (user-error "OpenRouter key missing. Set OPENROUTER_API_KEY or add to ~/.authinfo(.gpg)")))

  ;; Optional referral headers (disabled by default).
  ;; If you want them, uncomment both the defvar and :header line below.
  ;; (defvar my/openrouter-headers
  ;;   '(("HTTP-Referer" . "https://your.site")
  ;;     ("X-Title"      . "Your Site Name")))

  ;; Main backend: OpenRouter Chat Completions
  (setq my/gptel-openrouter
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key #'my/openrouter-api-key
          ;; :header my/openrouter-headers        ;; uncomment if you set headers above
          :models '(openai/gpt-5
                    openai/gpt-5-mini
                    openai/gpt-oss-120b
                    anthropic/claude-opus-4.1
                    anthropic/claude-sonnet-4
                    google/gemini-2.5-pro
                    google/gemini-2.5-flash
                    google/gemini-2.5-flash-lite
		    deepseek/deepseek-r1-0528
		    deepseek/deepseek-r1-0528-qwen3-8b
		    deepseek/deepseek-prover-v2
		    deepseek/deepseek-v3.1-base
		    deepseek/deepseek-chat-v3.1)
          ;; High reasoning effort, no hard max_tokens cap from your side
          :request-params '(:reasoning (:effort "high"))))

  (setq gptel-backend my/gptel-openrouter
        gptel-model  'openai/gpt-5))

(provide 'init-gptel)

;;; lisp/init-gptel.el ends here
