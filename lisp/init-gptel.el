;;; lisp/init-gptel.el -*- lexical-binding: t -*-

(use-package gptel
  :ensure t
  :init
  ;; Robust streaming + media support
  (setq gptel-use-curl (and (executable-find "curl") t)
        gptel-track-media t)
  ;; Include reasoning text (optional, leave default if your gptel is older)
  (when (boundp 'gptel-include-reasoning)
    (setq gptel-include-reasoning 'include))
  :config
  ;; Conditionally define the key function.
  ;; If a function named 'my/openrouter-api-key' already exists
  ;; (because it was loaded from init-local.el on Android),
  ;; this definition will be skipped. Otherwise (on PC), it is defined here.
  (unless (fboundp 'my/openrouter-api-key)
    (defun my/openrouter-api-key ()
      "Finds the key using .authinfo or env var for PC environments."
      (or (getenv "OPENROUTER_API_KEY")
          (ignore-errors
            (let ((auth-sources '("~/.authinfo.gpg" "~/.authinfo")))
              (gptel-api-key-from-auth-source "openrouter.ai")))
          (user-error "OpenRouter key missing. Set OPENROUTER_API_KEY or add to ~/.authinfo(.gpg)"))))

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
;; Custom command to start a new gptel chat without a prompt
(defun my-gptel-new-chat ()
  "Start a new gptel session without the interactive buffer name prompt.
This convenience function programmatically calls the original `gptel`
command with an auto-generated, unique buffer name, allowing for an
immediate chat session."
  (interactive)
  (let* (;; Get the current time with high precision (including microseconds) to ensure uniqueness.
         (time (current-time))
         (microseconds (nth 2 time))
         ;; Create a unique buffer name using a timestamp format, e.g., "*gptel 20250911-223015.123456*".
         ;; This prevents any naming conflicts, even when called multiple times in the same second.
         (buffer-name (format "*gptel %s.%06d*"
                              (format-time-string "%Y%m%d-%H%M%S")
                              microseconds)))

    ;; Programmatically invoke the original `gptel` function.
    ;; We use `funcall` to explicitly pass all necessary arguments, matching `gptel`'s function signature:
    ;; `(defun gptel (name &optional _ initial interactivep))`
    (funcall #'gptel
             buffer-name  ; 1. name: The unique buffer name we just created.
             nil          ; 2. _: A placeholder for the second, unused optional argument.
             nil          ; 3. initial: A placeholder for the initial text; we want an empty buffer.
             t)))         ; 4. interactivep: The crucial argument. Setting this to `t` ensures that
					;    `gptel` will call `display-buffer` to actually show the buffer to the user
					;    after creating it. Without this, the buffer is created silently.

;; Assign our new, convenient command to the global keybinding "C-c C-g".
;; This replaces the standard workflow of `M-x gptel` followed by typing a buffer name.
(global-set-key (kbd "C-c C-g") #'my-gptel-new-chat)

(provide 'init-gptel)

;;; lisp/init-gptel.el ends here
