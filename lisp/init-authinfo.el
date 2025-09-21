;;; init-authinfo.el -*- lexical-binding: t; -*-

(defun my-authinfo-get-password (machine &optional login-field)
  "Get password from authinfo for MACHINE with optional LOGIN-FIELD."
  (require 'auth-source)
  (let* ((login-field (or login-field "apikey"))
         (auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
         (found (car (auth-source-search
                     :host machine
                     :user login-field
                     :max 1))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun my-authinfo-get-token (service)
  "Get API token for SERVICE from authinfo.
Tries common patterns for API tokens."
  (or (my-authinfo-get-password service "apikey")
      (my-authinfo-get-password service "token")
      (my-authinfo-get-password service "api_key")))

(provide 'init-authinfo)

;;; init-authinfo.el ends here
