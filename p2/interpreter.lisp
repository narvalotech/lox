;; Interpreter for Part II: A tree-walk interpreter

(defpackage :com.craftinginterpreters.lox
  (:use :common-lisp))

(in-package :com.craftinginterpreters.lox)

(deftype token-type ()
  '(member
    ;; Single-char tokens
    'LEFT-PAREN 'RIGHT-PAREN 'LEFT-BRACE 'RIGHT-BRACE
    'COMMA 'DOT 'MINUS 'PLUS 'SEMICOLON 'SLASH 'STAR

    ;; One or two char tokens
    'BANG 'BANG-EQUAL
    'EQUAL 'EQUAL-EQUAL
    'GREATER 'GREATER-EQUAL
    'LESS 'LESS-EQUAL

    ;; Literals
    'IDENTIFIER 'STRING 'NUMBER

    ;; Keywords
    'AND 'CLASS 'ELSE 'FALSE 'FUN 'FOR 'IF 'NIL 'OR
    'PRINT 'RETURN 'SUPER 'THIS 'TRUE 'VAR 'WHILE

    'EOF
    ))

(defclass token ()
  ((type
    :initarg :type
    :type 'token-type)
   (lexeme :initarg :lexeme)
   (literal :initarg :literal)
   (line :initarg :line)))

;; TODO: generic if necessary
(defun ->string (token)
  (format nil "~A ~A ~A"
          (slot-value token 'type)
          (slot-value token 'lexeme)
          (slot-value token 'literal)))

(->string
  (make-instance 'token
                :type 'left-paren
                :lexeme "("
                :literal 10))

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
