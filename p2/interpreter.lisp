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

;; Scanner.java start

(defclass scanner ()
  ((tokens :type 'list :initarg :tokens) ; list of 'token
   (source :initarg :source)
   ;; start and current are offsets that index into the string
   (start :initform 0)                  ; first char in the lexeme being scanned
   (current :initform 0)                ; char currently being considered
   (line :initform 1)))                 ; what (source) line current is on

(defun make-eof (line)
  (make-instance 'token
                 :type 'EOF
                 :lexeme ""
                 :literal nil
                 :line line))

(defun is-at-end (scn)
  (with-slots (start current source line) scn
    (>= current (length source))))

(defun advance (scn)
  (with-slots (start current source line) scn
    (char source (inc current))))

(defun peek (scn)
  (with-slots (current source) scn
    (if (is-at-end scn)
        #\Nul
        (char source current))))

(defun scan-token (c scn)
  (with-slots (start current source line) scn
    (flet ((match (expected)
             (if (and
                  (not (is-at-end scn))
                  (equal expected (char source current)))

                 (progn
                   (inc current) t)))

           (consume-until (c)
             (loop while
                   (not (or (equal c (peek scn))
                            (is-at-end scn)))
                   do (advance scn))))

      (case c
        (#\( 'LEFT-PAREN)
        (#\) 'RIGHT-PAREN)
        (#\{ 'LEFT-BRACE)
        (#\} 'RIGHT-BRACE)
        (#\, 'COMMA)
        (#\. 'DOT)
        (#\- 'MINUS)
        (#\+ 'PLUS)
        (#\; 'SEMICOLON)
        (#\* 'STAR)

        ;; two-characters lexemes
        (#\! (if (match #\=) 'BANG-EQUAL 'BANG))
        (#\= (if (match #\=) 'EQUAL-EQUAL 'EQUAL))
        (#\< (if (match #\=) 'LESS-EQUAL 'LESS))
        (#\> (if (match #\=) 'GREATER-EQUAL 'GREATER))

        ;; comments or division
        (#\/ (if (match #\/)
                 (consume-until #\Newline)
                 'SLASH))

        ;; skip over whitespace
        (#\Space nil)
        (#\Return nil)
        (#\Tab nil)
        (#\Newline (progn (inc line) nil))

        (t (lox-error line "Unexpected character."))
        ))))

(defun inc (place)
  (setq place (+ 1 place)))

;; does Scanner really need to be a class?
(defmethod scan-tokens ((scn scanner))
  (with-slots (start current source line) scn
    (flet (;; this is rather a "make-token"
           (add-token (type &optional literal)
             (if type
                 (make-instance
                  'token
                  :type type
                  :lexeme (subseq source start current)
                  :literal literal
                  :line line))))

      (append
       (loop until (is-at-end scn)
             collect
             ;; We are at the beginning of the next lexeme
             (progn
               (setq start current)
               (add-token (scan-token (advance scn) scn))))
       (make-eof line)))))

;; Scanner.java end

(main)
