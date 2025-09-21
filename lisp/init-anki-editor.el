;;; lisp/init-anki-editor.el -*- lexical-binding: t; -*-

(require 'subr-x)

(use-package anki-editor
  :ensure t
  :after org
  :init
  ;; Repo-wide defaults (tracked, not private).
  ;; If you delete these setq lines, code will fall back to "Basic" / "Default".
  (defgroup my-anki-editor nil
    "Global defaults and helpers for anki-editor."
    :group 'anki-editor)

  (defcustom my/anki-editor-default-deck nil
    "Default deck when ANKI_DECK is not present (or not in Org).
If nil, fall back to \"Default\"."
    :type '(choice (const :tag "Use Anki's \"Default\"" nil)
                   (string :tag "Deck name (use \"::\" for subdecks)")))

  (defcustom my/anki-editor-default-note-type nil
    "Default note type when ANKI_NOTE_TYPE is not present (or not in Org).
If nil, fall back to \"Basic\"."
    :type '(choice (const :tag "Use Anki's \"Basic\"" nil)
                   (string :tag "Note type name")))

  ;; Set your repo defaults here.
  (setq my/anki-editor-default-note-type "Bilingual Cloze"
        my/anki-editor-default-deck
        "Root::Neural Pathways")
  :config
  ;; Helpers
  (defun my/anki--default-deck ()
    (or my/anki-editor-default-deck "Default"))

  (defun my/anki--default-model ()
    (or my/anki-editor-default-note-type "Basic"))

  (defun my/anki--safe-model-fields (model)
    "Return field names for MODEL, falling back to Basic if needed."
    (condition-case _
        (anki-editor-api-call-result 'modelFieldNames :modelName model)
      (error
       (condition-case _
           (progn
             (message "anki-editor: falling back to note type \"Basic\"")
             (setq model "Basic")
             (anki-editor-api-call-result 'modelFieldNames :modelName model))
         (error '("Front" "Back"))))))

  (defun my/anki--main-text ()
    "Prefill text: active region, else current line, else empty."
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (or (thing-at-point 'line t) "")))

  (defun my/anki--prefill-fields (model main)
    "Build full fields alist for MODEL, placing MAIN into a sensible field."
    (let* ((fields (my/anki--safe-model-fields model))
           (alist  (mapcar (lambda (nm) (cons nm "")) fields))
           (target (cond
                    ((member "Text" fields) "Text")
                    ((member "Front" fields) "Front")
                    (t (car fields)))))
      (when (and target (stringp main))
        (setcdr (assoc target alist) (string-trim-right main)))
      alist))

  (defun my/anki--ensure-deck-exists (deck)
    "Create DECK if it doesn't exist."
    (condition-case _ (anki-editor-api-call-result 'createDeck :deck deck)
      (error nil)))

  ;; Works anywhere:
  ;; - On an Org Anki note: use anki-editor's native mapping.
  ;; - Elsewhere: open Add dialog with defaults and context text.
  (defun my/anki-editor-gui-add-cards-anywhere (&optional prompt)
    "Open Anki Add dialog from anywhere.
C-u to prompt for deck and note type."
    (interactive "P")
    (anki-editor-api-check)
    (let* ((org-context
            (and (derived-mode-p 'org-mode)
                 (save-excursion
                   (condition-case _ (progn (anki-editor--goto-nearest-note-type) (org-at-heading-p))
                     (error nil))))))
      (if org-context
          (condition-case _
              (anki-editor-gui-add-cards)
            (error (my/anki--gui-add-cards-adhoc prompt)))
        (my/anki--gui-add-cards-adhoc prompt))))

  (defun my/anki--gui-add-cards-adhoc (prompt)
    "Add dialog using global/default deck/model, prefilled from context.
PROMPT non-nil prompts for deck/model."
    (let* ((all-decks  (condition-case _ (anki-editor-deck-names) (error nil)))
           (all-models (condition-case _ (anki-editor-note-types) (error nil)))
           (default-deck (my/anki--default-deck))
           (default-model (my/anki--default-model))
           (deck  (if (and prompt all-decks)
                      (completing-read "Deck: " all-decks nil nil default-deck)
                    default-deck))
           (model (if (and prompt all-models)
                      (completing-read "Note type: " all-models nil nil default-model)
                    default-model))
           (fields (my/anki--prefill-fields model (my/anki--main-text))))
      (my/anki--ensure-deck-exists deck)
      (anki-editor-api-call-result
       'guiAddCards
       :note (list :deckName  deck
                   :modelName model
                   :fields    fields
                   :options   '(:closeAfterAdding t)))))

  ;; Global keybinding: available in all modes
  (global-set-key (kbd "C-c A") #'my/anki-editor-gui-add-cards-anywhere))

(provide 'init-anki-editor)
;;; lisp/init-anki-editor.el ends here
