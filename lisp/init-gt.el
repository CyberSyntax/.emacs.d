;;; lisp/init-gt.el --- Minimal, robust DeepL EN<->KO setup for gt.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

;; Send no extra DeepL params (let DeepL defaults apply).
(setq gt-deepl-extra-params nil)

(use-package gt
  :ensure t
  :bind (("C-c t" . gt-translate))
  :config
  ;; Make DeepL accept any language symbol by uppercasing its name when not mapped.
  ;; Avoids maintaining a static mapping.
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

  ;; Never fold the source text in *gt-result* (show full input, no "...").
  (with-eval-after-load 'gt-render-buffer
    (setq gt-buffer-render-source-text-limit nil))

  ;; Track current pair and rebuild the default translator when it changes.
  (defvar my/gt-langs '(en ko)
    "Current language pair used by the default translator. Order matters.")

  (defun my/gt-apply-langs (langs)
    "Apply LANGS as the current pair and rebuild `gt-default-translator`."
    (setq my/gt-langs langs)
    (setq gt-default-translator
          (gt-translator
           :taker   (gt-taker :langs langs)
           :engines (gt-deepl-engine :pro t)
           :render  (gt-buffer-render)))
    (message "gt.el: langs => %S" langs))

  (defun my/gt-swap-langs ()
    "Swap EN<->KO (or, in general, reverse the current pair)."
    (interactive)
    (my/gt-apply-langs (reverse my/gt-langs)))

  ;; Collect the last translation's output text from *gt-result*.
  (defun my/gt--collect-result-from-buffer ()
    "Return the visible translation result text in current *gt-result* buffer."
    (let ((tr (and (boundp 'gt-buffer-render-translator) gt-buffer-render-translator)))
      (cond
       ;; Prefer task results directly
       ((and tr (slot-boundp tr 'tasks))
        (let* ((tasks (oref tr tasks))
               ;; pick the first task that has a result
               (tgood (cl-find-if (lambda (tk) (oref tk res)) tasks))
               (res (and tgood (oref tgood res))))
          (cond
           ((stringp res) (string-trim res))
           ((listp res)   (string-trim (string-join (mapcar #'identity res) "\n\n")))
           (t nil))))
       ;; Fallback: read all regions marked as gt-result in buffer
       (t
        (save-excursion
          (goto-char (point-min))
          (let (chunks prop)
            (while (setq prop (text-property-search-forward 'gt-result))
              (push (buffer-substring-no-properties
                     (prop-match-beginning prop) (prop-match-end prop)) chunks))
            (when chunks (string-trim (string-join (nreverse chunks) "\n\n")))))))))

  ;; Swap and retranslate using the previous output as new input.
  (defun my/gt-swap-using-buffer-result ()
    "In *gt-result*, use the previous output as new input, swap langs, and retranslate."
    (interactive)
    (unless (and (eq (current-buffer) (get-buffer "*gt-result*"))
                 (boundp 'gt-buffer-render-translator))
      (user-error "Run this in the *gt-result* buffer"))
    (let ((newtext (my/gt--collect-result-from-buffer)))
      (if (or (null newtext) (string-empty-p newtext))
          (message "gt.el: no result to reuse.")
        ;; 1) swap langs
        (my/gt-swap-langs)
        ;; 2) run a translator that feeds NEWTEXT directly as source, no picking
        (let ((translator
               (gt-translator
                :taker   (gt-taker :langs my/gt-langs
                                   :text (lambda (_tr) newtext)
                                   :pick nil)
                :engines (gt-deepl-engine :pro t)
                :render  (gt-buffer-render))))
          (gt-start translator)))))

  ;; Bind "s" only in the *gt-result* buffer and enable visual-line-mode there.
  (with-eval-after-load 'gt-render-buffer
    (add-hook 'gt-buffer-render-init-hook
              (lambda ()
                (visual-line-mode 1)
                (local-set-key (kbd "s") #'my/gt-swap-using-buffer-result)))))

(provide 'init-gt)

;;; lisp/init-gt.el ends here