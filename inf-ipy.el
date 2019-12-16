(define-derived-mode inf-ipy-mode comint-mode "inf-ipy"
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp (rx bol ">" space)))

(defun inf-ipy ()
  (interactive)
  (let* ((buffer (comint-check-proc "*inf-ipy*")))
    (pop-to-buffer-same-window
     (get-buffer-create "*inf-ipy*"))
    (unless buffer
      (apply 'make-comint "inf-ipy" "inf-ipy" nil '("--repl"))
      (inf-ipy-mode))))

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
;; (inf-ipy-configure-kernel python)
