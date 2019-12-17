(define-derived-mode inf-ipy-mode comint-mode "inf-ipy"
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp (rx bol ">" space)))

(defun inf-ipy-comint-filter (str)
  (if (string-match "^<image \\(.*\\)>" str)
      (progn
        (insert "\n")
        (insert-image (create-image (match-string 1 str)))
        (insert "\n")
        (comint-send-input nil
                           t  ;; artificial
                           )
        "")
    str))

(defun inf-ipy ()
  (interactive)
  (let* ((buffer (comint-check-proc "*inf-ipy*")))
    (pop-to-buffer-same-window
     (get-buffer-create "*inf-ipy*"))
    (unless buffer
      (apply 'make-comint "inf-ipy" "inf-ipy" nil '("--repl"))
      (inf-ipy-mode)
      (add-hook 'comint-preoutput-filter-functions 'inf-ipy-comint-filter t t))))

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

;; (macroexpand '(inf-ipy-configure-kernel python))
;; (load-file "~/source/inf-ipy/inf-ipy.el")
;; (inf-ipy-configure-kernel python)
;; (inf-ipy-configure-kernel matlab)
