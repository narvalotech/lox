;; Interpreter for Part II: A tree-walk interpreter

(defparameter *had-error* nil)

(defun read-file (path)
  (with-open-file
      (stream (uiop:parse-unix-namestring path))
    (loop for line = (read-line stream nil)
          until (eq line nil)
          unless (string-equal line "")
            collect line)))

(defun run (str)
  ;; (let ((tokens (scan-tokens str)))
  ;;   (loop for token in tokens do
  ;;         (format t "Token: ~A" token)))

  (if (equal str "err")
      (lox-error 1 "ohno"))

  (format t "Running script: ~A~%" str))

(defun run-file (path)
  (run (read-file path))
  (if *had-error*                       ; return codes
      65
      0))

(defun run-prompt ()
  (loop for line = (read-line *standard-input* nil)
        until (eq line nil) do
              (progn (run line)
                     (setq *had-error* nil))))

(defun main (&optional filepath)
  (if filepath
      (run-file filepath)
      (run-prompt)))

(defun lox-report (line where message)
  (format t "[line ~A] Error~A: ~A~%"
          line where message)
  (setq *had-error* t))

(defun lox-error (line message)
  (lox-report line "" message))

(main)
