;;; init-deepl-write.el --- DeepL Write (Improve text) integration  -*- lexical-binding: t; -*-
;;
;; This module integrates DeepL API for Write (v2) “Improve text” endpoint:
;;   POST https://api.deepl.com/v2/write/rephrase
;;
;; Highlights
;; - JSON requests with either curl or url.el (auto-picks curl if available)
;; - Enforces 10 KiB request-body budget with safe, paragraph/line/word chunking
;; - Supports target_lang + optional writing_style OR tone (mutually exclusive)
;; - Reads auth key from $DEEPL_AUTH_KEY or auth-source (~/.authinfo(.gpg))
;; - Region command: C-c w  (use defaults)   |   C-c W  (prompt)
;;
;; Requirements
;; - Emacs 27+ recommended (json, url)
;; - Optional: curl(1) for robust HTTP/2 handling and compressed transfer
;;
;; Notes
;; - Write is available to DeepL API *Pro* only; Free host may return 403/404.
;; - The API requires all submitted texts to be in the same language.

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'subr-x)

(defgroup my-deepl-write nil
  "DeepL Write (text improvement) integration."
  :group 'external
  :prefix "my/deepl-write-")

;; ---------- User defaults ----------
(defcustom my/deepl-write-default-target "en-US"
  "Default target language for improvements (e.g., \"en-US\", \"en-GB\", \"de\")."
  :type 'string)

(defcustom my/deepl-write-default-style "default"
  "Default writing style. One of:
 default, academic, business, casual, simple,
 prefer_academic, prefer_business, prefer_casual, prefer_simple."
  :type 'string)

(defcustom my/deepl-write-default-tone "default"
  "Default tone. One of:
 default, confident, diplomatic, enthusiastic, friendly,
 prefer_confident, prefer_diplomatic, prefer_enthusiastic, prefer_friendly."
  :type 'string)

(defcustom my/deepl-write-host "api.deepl.com"
  "DeepL host. Write is Pro-only; Free host may reject (/v2 only)."
  :type 'string)

(defcustom my/deepl-write-transport 'auto
  "HTTP transport to use: 'auto, 'curl, or 'url.
'auto prefers curl if available, else falls back to url.el."
  :type '(choice (const auto) (const curl) (const url)))

(defcustom my/deepl-write-use-prefer-prefix t
  "If non-nil, automatically prefix style/tone with `prefer_` for graceful fallback."
  :type 'boolean)

;; ---------- Constants (spec-aligned) ----------
(defconst my/deepl-write--endpoint-path "/v2/write/rephrase")
(defconst my/deepl-write--limit-bytes 10240)       ; 10 KiB hard limit (spec)
(defconst my/deepl-write--safe-payload-bytes 7000) ; conservative per-call budget

(defconst my/deepl-write--styles
  '("default" "academic" "business" "casual" "simple"
    "prefer_academic" "prefer_business" "prefer_casual" "prefer_simple"))

(defconst my/deepl-write--tones
  '("default" "confident" "diplomatic" "enthusiastic" "friendly"
    "prefer_confident" "prefer_diplomatic" "prefer_enthusiastic" "prefer_friendly"))

(defconst my/deepl-write--targets
  ;; Superset from spec (docs list plus OpenAPI includes 'en' and 'pt')
  '("de" "en" "en-GB" "en-US" "es" "fr" "it" "pt" "pt-BR" "pt-PT"))

;; ---------- Helpers ----------
(defun my/deepl-write--endpoint ()
  (concat "https://" my/deepl-write-host my/deepl-write--endpoint-path))

(defun my/deepl-write--env-key ()
  (let ((v (getenv "DEEPL_AUTH_KEY")))
    (and (stringp v) (string-match-p "\\S-" v) v)))

(defun my/deepl-write--authinfo-key ()
  "Look up a key via auth-source if available."
  (when (require 'auth-source nil t)
    (let* ((auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
           (cands (list
                   (list :host "api.deepl.com"     :user "auth-key")
                   (list :host "api.deepl.com"     :user "apikey")
                   (list :host "api-free.deepl.com":user "auth-key")
                   (list :host "api-free.deepl.com":user "apikey")))
           key)
      (catch 'done
        (dolist (spec cands)
          (let* ((res (car (apply #'auth-source-search (append spec '(:max 1))))))
            (when res
              (let ((secret (plist-get res :secret)))
                (setq key (if (functionp secret) (funcall secret) secret)))
              (when (and (stringp key) (string-match-p "\\S-" key))
                (throw 'done key)))))
        nil))))

(defun my/deepl-write--auth-key ()
  "Find DeepL auth key from env or authinfo."
  (or (my/deepl-write--env-key)
      (and (fboundp 'my-authinfo-get-password)
           (or (my-authinfo-get-password "api.deepl.com" "auth-key")
               (my-authinfo-get-password "api.deepl.com" "apikey")
               (my-authinfo-get-password "api-free.deepl.com" "auth-key")
               (my-authinfo-get-password "api-free.deepl.com" "apikey")))
      (my/deepl-write--authinfo-key)
      (user-error
       "DeepL auth key missing. Set $DEEPL_AUTH_KEY or add to ~/.authinfo(.gpg):
  machine api.deepl.com login auth-key password <YOUR_KEY>")))

(defun my/deepl-write--utf8-bytes (s)
  (length (encode-coding-string (or s "") 'utf-8)))

(defun my/deepl-write--fits-p (s)
  (< (my/deepl-write--utf8-bytes s) my/deepl-write--safe-payload-bytes))

(defun my/deepl-write--split-into-chunks (text)
  "Split TEXT into chunks that fit comfortably under server size limits.
Preserves paragraph and line boundaries where possible."
  (let* ((paras (split-string (or text "") "\\(?:\r?\n\\)\\{2,\\}" t))
         (chunks '())
         (current ""))
    (cl-labels
        ((fits-p (s) (my/deepl-write--fits-p s))
         (flush-current () (unless (string-empty-p current) (push current chunks) (setq current ""))))
      (dolist (p paras)
        (let ((p1 (if (string-empty-p current) p (concat current "\n\n" p))))
          (cond
           ((fits-p p1) (setq current p1))
           ((fits-p p)  (flush-current) (setq current p))
           (t
            ;; split by lines, then words
            (dolist (ln (split-string p "\n" t))
              (let ((cand (if (string-empty-p current) ln (concat current "\n" ln))))
                (cond
                 ((fits-p cand) (setq current cand))
                 ((fits-p ln)   (flush-current) (setq current ln))
                 (t
                  (let ((acc ""))
                    (dolist (w (split-string ln "\\s-+" t))
                      (let ((cand2 (if (string-empty-p acc) w (concat acc " " w))))
                        (if (fits-p cand2)
                            (setq acc cand2)
                          (let ((cand3 (if (string-empty-p current) acc (concat current "\n" acc))))
                            (if (fits-p cand3)
                                (setq current cand3 acc "")
                              (flush-current) (setq current acc acc ""))))))
                    (when (not (string-empty-p acc))
                      (let ((cand4 (if (string-empty-p current) acc (concat current "\n" acc))))
                        (if (fits-p cand4) (setq current cand4)
                          (flush-current) (setq current acc)))))))))))))
      (flush-current)
      (nreverse chunks))))

(defun my/deepl-write--maybe-prefer (choice)
  (if (and my/deepl-write-use-prefer-prefix
           choice
           (not (string-prefix-p "prefer_" choice))
           (not (string= choice "default")))
      (concat "prefer_" choice)
    choice))

;; ---------- HTTP (curl first; url.el fallback) ----------
(defun my/deepl-write--http-curl (payload)
  "POST PAYLOAD (alist) via curl. Return (CODE . BODY-STRING)."
  (unless (executable-find "curl")
    (user-error "curl not found; set my/deepl-write-transport to 'url or install curl"))
  (let* ((data   (encode-coding-string (json-encode payload) 'utf-8))
         (url    (my/deepl-write--endpoint))
         (auth   (concat "DeepL-Auth-Key " (my/deepl-write--auth-key)))
         (tmp    (generate-new-buffer " *deepl-curl*"))
         (status 0))
    (unwind-protect
        (with-temp-buffer
          (insert data)
          ;; IMPORTANT: do not use `apply` here; pass args directly.
          (setq status
                (call-process-region
                 (point-min) (point-max) "curl" nil tmp nil
                 "-sS" "--compressed" "-i"
                 "-X" "POST" url
                 "-H" (concat "Authorization: " auth)
                 "-H" "Content-Type: application/json"
                 "--data-binary" "@-")))
      )
    (with-current-buffer tmp
      (goto-char (point-min))
      (let (code body)
        ;; Accept HTTP/1.1 or HTTP/2 status lines
        (when (re-search-forward "^HTTP/\\(?:1\\.[01]\\|2\\(?:\\.0\\)?\\) \\([0-9]+\\)" nil t)
          (setq code (string-to-number (match-string 1))))
        (when (re-search-forward "\r?\n\r?\n" nil t)
          (setq body (buffer-substring-no-properties (point) (point-max))))
        (kill-buffer tmp)
        (cons (or code (if (numberp status) status 0)) (or body ""))))))

(defun my/deepl-write--http-url (payload)
  "POST PAYLOAD (alist) via url.el. Return (CODE . BODY-STRING)."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "DeepL-Auth-Key " (my/deepl-write--auth-key)))
            ("Content-Type"  . "application/json")
            ("Accept"        . "application/json")))
         (url-request-coding-system 'binary)
         (url-mime-charset-string nil)
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (buf (url-retrieve-synchronously (my/deepl-write--endpoint) t t)))
    (unwind-protect
        (with-current-buffer (or buf (user-error "No response from DeepL Write")))
          (goto-char (point-min))
          (let* ((status-var (and (boundp 'url-http-response-status) url-http-response-status))
                 code body)
            (unless (numberp status-var)
              (when (re-search-forward "^HTTP/\$begin:math:text$?:1\\\\.[01]\\\\|2\\\\(?:\\\\.0\\$end:math:text$?\\) \$begin:math:text$[0-9]+\\$end:math:text$" nil t)
                (setq status-var (string-to-number (match-string 1)))))
            (setq code status-var)
            (when (re-search-forward "\r?\n\r?\n" nil t)
              (setq body (buffer-substring-no-properties (point) (point-max))))
            (cons (or code 0) (or body ""))))
      (when (buffer-live-p buf) (kill-buffer buf))))

(defun my/deepl-write--http-improvements (texts target style tone)
  "Make one HTTP call for list TEXTS; return list of improved strings.
TEXTS must be a list of strings. STYLE and TONE are mutually exclusive."
  (unless (and (listp texts) (cl-every #'stringp texts))
    (user-error "texts must be a list of strings"))
  (let* ((style* (and style (not (string= style "default")) style))
         (tone*  (and tone  (not (string= tone  "default")) tone)))
    (when (and style* tone*)
      (user-error "Choose writing_style OR tone (not both)"))
    (let* ((payload `((text . ,texts) (target_lang . ,target)))
           (payload (if style* (cons (cons 'writing_style style*) payload) payload))
           (payload (if tone*  (cons (cons 'tone          tone*) payload) payload))
           (resp (pcase my/deepl-write-transport
                   ('curl (my/deepl-write--http-curl payload))
                   ('url  (my/deepl-write--http-url  payload))
                   (_     (if (executable-find "curl")
                              (my/deepl-write--http-curl payload)
                            (my/deepl-write--http-url  payload)))))
           (code (car resp))
           (body (cdr resp)))
      (let ((json-object-type 'alist)
            (json-array-type  'list)
            (json-key-type    'symbol))
        (cond
         ;; Success
         ((and (integerp code) (= code 200))
          (let* ((data (ignore-errors (json-read-from-string body)))
                 (imps (and (listp data) (alist-get 'improvements data))))
            (unless (and (listp imps) imps)
              (user-error "DeepL: success but empty `improvements`"))
            (mapcar (lambda (o) (or (alist-get 'text o) "")) imps)))
         ;; Common auth/host errors
         ((member code '(401 403 404 413 415 429 503))
          (let* ((data (ignore-errors (json-read-from-string body)))
                 (msg  (and (listp data) (alist-get 'message data)))
                 (fallback (pcase code
                             (401 "invalid/expired auth key")
                             (403 "forbidden: Write requires API Pro or Cost Control limit reached")
                             (404 "endpoint/host not available (Free) or path wrong")
                             (413 "payload too large (>10 KiB)")
                             (415 "unsupported media type")
                             (429 "rate limit / cost control")
                             (503 "service unavailable")
                             (_   "request rejected"))))
            (user-error "DeepL Write HTTP %s: %s" code (or msg fallback))))
         (t
          (user-error "DeepL Write error (HTTP %s): %s"
                      code (substring body 0 (min 300 (length body))))))))))

;; ---------- Public API ----------
(defun my/deepl-write--interactive-options (arg)
  "Return plist of :target :style :tone. ARG non-nil -> prompt, nil -> defaults."
  (if (not arg)
      (list :target my/deepl-write-default-target
            :style  (my/deepl-write--maybe-prefer my/deepl-write-default-style)
            :tone   (my/deepl-write--maybe-prefer my/deepl-write-default-tone))
    (let* ((target (completing-read "Target language: " my/deepl-write--targets
                                    nil t nil nil my/deepl-write-default-target))
           (which  (completing-read "Adjust (style/tone/neither): "
                                    '("style" "tone" "neither") nil t nil nil "neither"))
           (style  (and (string= which "style")
                        (my/deepl-write--maybe-prefer
                         (completing-read "Style: " my/deepl-write--styles nil t nil nil "default"))))
           (tone   (and (string= which "tone")
                        (my/deepl-write--maybe-prefer
                         (completing-read "Tone: " my/deepl-write--tones nil t nil nil "default")))))
      (list :target target :style style :tone tone))))

(defun my/deepl-write--improve (text &optional target style tone)
  "Improve TEXT, splitting into safe chunks; return single combined string."
  (let* ((target (or target my/deepl-write-default-target))
         (style  (my/deepl-write--maybe-prefer (or style my/deepl-write-default-style)))
         (tone   (my/deepl-write--maybe-prefer (or tone  my/deepl-write-default-tone)))
         (chunks (my/deepl-write--split-into-chunks (or text "")))
         (out '()))
    (when (and style tone (not (string= style "default")) (not (string= tone "default")))
      (user-error "Choose writing_style OR tone (not both)"))
    (dolist (ck chunks)
      ;; Send one chunk per request to respect budget and maintain order.
      (setq out (append out (my/deepl-write--http-improvements (list ck) target style tone))))
    (mapconcat #'identity out "\n\n")))

;;;###autoload
(defun my/deepl-write-rephrase-region (&optional arg)
  "Improve the active region only. With C-u ARG, prompt for target/style/tone."
  (interactive "P")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((opts   (my/deepl-write--interactive-options arg))
         (target (plist-get opts :target))
         (style  (plist-get opts :style))
         (tone   (plist-get opts :tone))
         (beg    (region-beginning))
         (end    (region-end))
         (orig   (buffer-substring-no-properties beg end))
         (impr   (my/deepl-write--improve orig target style tone)))
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (insert impr))
    (message "DeepL Write: improved region (%s%s%s)"
             target
             (if style (format ", style=%s" style) "")
             (if tone  (format ", tone=%s"  tone)  ""))))

;;; Convenience: uppercase W always prompts (even without C-u).
(defun my/deepl-write-rephrase-region-ask ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'my/deepl-write-rephrase-region)))

;; ---------- Keybindings ----------
;; Single key that improves the region.
(global-unset-key (kbd "C-c w"))
(global-set-key   (kbd "C-c w") #'my/deepl-write-rephrase-region)
(global-set-key   (kbd "C-c W") #'my/deepl-write-rephrase-region-ask)

(provide 'init-deepl-write)
;;; init-deepl-write.el ends here
