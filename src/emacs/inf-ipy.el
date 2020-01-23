;;; inf-ipy.el --- A REPL interface to communicate with Jupyter kernels in Emacs -*- lexical-binding: t -*-

;; Author: Nurullah Akkaya <nurullah at nakkaya dot com>
;; Version: 0.1
;; URL: https://github.com/nakkaya/inf-ipy/
;; Package-Requires: ((emacs "25.1"))

;; BSD 2-Clause License

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;; * Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer.

;; * Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; Commentary:
;; A REPL interface to communicate with Jupyter kernels in Emacs.

(require 'comint)
(require 'org)
(require 'ob)
(require 'org-id)

;;; Code:

(defcustom inf-ipy-program "inf-ipy"
  "Program name for invoking inf-ipy."
  :type 'string
  :group 'inf-ipy)

(defcustom inf-ipy-prompt (rx bol "inf-ipy>" space)
  "Regexp to recognize prompts in the inf-ipy mode."
  :type 'regexp
  :group 'inf-ipy)

(defcustom inf-ipy-buffer "*inf-ipy*"
  "inf-ipy process buffer name."
  :type 'string
  :group 'inf-ipy)

(defun inf-ipy-ob-insert-result (buffer uuid result)
  "Replace result UUID with the result of the execution."
  (save-window-excursion
    (save-excursion
      (save-restriction
        (with-current-buffer (find-file-noselect buffer)
          (goto-char (point-min))
          (when (re-search-forward uuid nil t)
            (beginning-of-line)
            (kill-line)
            (insert (mapconcat
                     (lambda (x)
                       (format "%s" x))
                     (butlast
                      (split-string
                       (ansi-color-filter-apply result) "\n"))
                     "\n"))))))))

(let ((output nil)
      (que    '()))

  (defun inf-ipy-output-comint-que (buffer uuid)
    (setq que (append que (list (list uuid buffer)))))

  (defun inf-ipy-output-comint-deque ()
    (let* ((next   (car que))
           (uuid   (car next))
           (buffer (car (cdr next))))
      (setq que  (cdr que))
      (list uuid buffer)))

  (defun inf-ipy-output-comint-filter (str)
    (setq output (concat output str))

    (when (string-match inf-ipy-prompt str)
      (let* ((next   (inf-ipy-output-comint-deque))
             (uuid   (car next))
             (buffer (car (cdr next))))
        (when uuid
          (inf-ipy-ob-insert-result buffer uuid output)))
      (setq output ""))
    str)

  (defun inf-ipy-comint-output ()
    "Last output received from the process."
    output))

(defun inf-ipy-send-string (proc string)
  "Multi line input sender for the process."
  (comint-simple-send proc (concat string "\ninf-ipy-eoe")))

(defun inf-ipy-opts ()
  "Default inf-ipy options. If working directory does not contain
   a config.ini file prompt for one."
    (if (not (file-exists-p "./config.ini"))
              (let ((config (read-file-name "Kernel Config: ")))
                        (list "--comint" "--config" config))
          (list "--comint")))

(defun inf-ipy-repl (&optional arg)
  "Run an inf-ipy process, input and output via buffer ‘*inf-ipy*’.
   With argument, switches to ‘*inf-ipy*’. If there is a process
   already running in ‘*inf-ipy*’, just switch to that buffer. "
  (interactive "p")
  (with-current-buffer (get-buffer-create inf-ipy-buffer)
    (let* ((buffer (comint-check-proc inf-ipy-buffer)))
      (unless buffer
        (message "inf-ipy starting")
        (apply 'make-comint inf-ipy-program inf-ipy-program nil (inf-ipy-opts))
        (inf-ipy-mode)
        (add-hook 'comint-preoutput-filter-functions 'inf-ipy-output-comint-filter t t)
        (while
            (progn
              (accept-process-output (get-process inf-ipy-buffer) 10)
              (not (inf-ipy-comint-output)))))))
  (when (and arg (not (eq arg 1)))
    (pop-to-buffer-same-window
     (get-buffer-create inf-ipy-buffer))))

(defun inf-ipy-clear ()
  "Clear ‘*inf-ipy*’ buffer."
  (interactive)
  (pop-to-buffer-same-window
   (get-buffer-create inf-ipy-buffer))
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun inf-ipy-quit ()
  "Quit ‘*inf-ipy*’ process."
  (interactive)
  (pop-to-buffer-same-window
   (get-buffer-create inf-ipy-buffer))
  (kill-process))

(defvar inf-ipy-map nil "Keymap for `inf-ipy-mode'")

(progn
  (setq inf-ipy-map (make-sparse-keymap))
  (define-key inf-ipy-map (kbd "C-c C-k") 'inf-ipy-clear)
  (define-key inf-ipy-map [C-up] 	  'comint-previous-input)
  (define-key inf-ipy-map [C-down] 	  'comint-next-input)
  (define-key inf-ipy-map "\C-m" 	  'comint-send-input)
  (define-key inf-ipy-map (kbd "C-c C-q") 'inf-ipy-quit))

(define-derived-mode inf-ipy-mode comint-mode "inf-ipy"
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp inf-ipy-prompt)
  (setq-local comint-input-sender 'inf-ipy-send-string)
  (use-local-map inf-ipy-map))

;;; org-babel support

(defun inf-ipy-ob-execute-send (body)
  "Send body to comint buffer for processing."
  (with-current-buffer
      (get-buffer-create inf-ipy-buffer)
    (comint-send-string inf-ipy-buffer (concat body "\n"))
    (comint-send-input nil t)))

(defun inf-ipy-ob-execute (body params)
  "Launch inf-ipy process if not running and execute
   org-mode source block."
  (inf-ipy-repl)
  (if (or (eq (cdr (assq :result-type params)) 'output)
          (string= (cdr (assq :results params)) "replace drawer"))
      (let ((uuid (org-id-uuid)))
        (org-babel-remove-result)
        (save-excursion
          (re-search-forward "#\\+END_SRC")
          (insert (format
                   "\n\n#+RESULTS: %s\n: %s"
                   (or (org-element-property :name (org-element-context))
                       "")
                   uuid)))
        (inf-ipy-output-comint-que (buffer-file-name) uuid)
        (inf-ipy-ob-execute-send body)
        uuid)
    (inf-ipy-ob-execute-send body)))

;; register inf-ipy with org babel.
(defun org-babel-execute:inf-ipy (body params)
  (inf-ipy-ob-execute body params))

(add-to-list 'org-src-lang-modes '("inf-ipy" . python))

;; register inf-ipy-* with org babel.
(defmacro inf-ipy-configure-kernel (kernel)
  `(progn
     (defun ,(intern
              (concat "org-babel-execute:inf-ipy-"
                      (symbol-name kernel)))
         (body params)
       (inf-ipy-ob-execute body params))

     (add-to-list 'org-src-lang-modes
                  '(,(concat "inf-ipy-" (symbol-name kernel)) . ,kernel))))

(provide 'inf-ipy)

;;; inf-ipy.el ends here
