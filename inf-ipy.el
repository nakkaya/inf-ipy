(define-derived-mode inf-ipy-mode comint-mode "inf-ipy"
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp (rx bol ">" space)))

(defun run-inf-ipy ()
  (interactive)
  (let* ((buffer (comint-check-proc "*inf-ipy*")))
    (pop-to-buffer-same-window
     (get-buffer-create "*inf-ipy*"))
    (unless buffer
      (apply 'make-comint "inf-ipy" "inf-ipy" nil '("--repl"))
      (inf-ipy-mode))))
