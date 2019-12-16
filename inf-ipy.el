(define-derived-mode inf-ipy-mode comint-mode "inf-ipy"
  ;; This disables editing and traversing the "=>" prompts
  (setq-local comint-prompt-read-only t)
  ;; Lets comint mode recognize the prompt
  (setq-local comint-prompt-regexp (rx bol ">" space)))

(defun run-inf-ipy ()
  (interactive)
  (let* ((buffer (comint-check-proc "inf-ipy")))
    ;; pop to the "*inf-ipy*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*inf-ipy*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint "inf-ipy" "inf-ipy" nil '("--repl"))))
  (inf-ipy-mode))
