(defun inf-ipy-output-comint-filter (str)
  (if (string-match "^<image \\(.*\\)>" str)
      (progn
        (insert "\n")
        (insert-image (create-image (match-string 1 str)))
        (insert "\n")
        (comint-send-input nil
                           t)  ;; artificial
        "")
    str))

(defun inf-ipy-send-string (proc string)
  (comint-simple-send proc (concat string "\ninf-ipy-eoe\n")))

(defun inf-ipy ()
  (interactive)
  (let* ((buffer (comint-check-proc "*inf-ipy*")))
    (pop-to-buffer-same-window
     (get-buffer-create "*inf-ipy*"))
    (unless buffer
      (apply 'make-comint "inf-ipy" "inf-ipy" nil '("--comint"))
      (inf-ipy-mode)
      (add-hook 'comint-preoutput-filter-functions 'inf-ipy-output-comint-filter t t))))

(define-derived-mode inf-ipy-mode comint-mode "inf-ipy"
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp (rx bol ">" space))
  (setq-local comint-input-sender 'inf-ipy-send-string))

(defvar org-babel-default-header-args:inf-ipy
  '((:results . "silent")))

(defun org-babel-execute:inf-ipy (body params)
  (comint-send-string "*inf-ipy*" (concat body "\n")))

(defmacro inf-ipy-configure-kernel (kernel)
  `(progn
     (defvar ,(intern
               (concat "org-babel-default-header-args:inf-ipy-"
                       (symbol-name kernel)))
       '((:results . "silent")))

     (defun ,(intern
              (concat "org-babel-execute:inf-ipy-"
                      (symbol-name kernel)))
         (body params)
       (comint-send-string "*inf-ipy*" (concat body "\n")))

     (add-to-list 'org-src-lang-modes
                  '(,(concat "inf-ipy-" (symbol-name kernel)) . ,kernel))))
