;; Interpreter for Part II: A tree-walk interpreter
(declaim (optimize (speed 0) (space 0) (debug 3)))

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
  (let* ((scanner (make-instance 'scanner :source str))
         (tokens (scan-tokens scanner)))
    (break)
    (loop for token in tokens do
      (format t "Token: ~A" token))))

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
    (sleep .1)
    (char source (- (incf current) 1))))

(defun peek (scn)
  (with-slots (current source) scn
    (if (is-at-end scn)
        #\Nul
        (char source current))))

(defun peek-next (scn)
  (with-slots (current source) scn
    (if (>= (+ 1 current) (length source))
        #\Nul
        (char source (+ 1 current)))))

(defun consume-until (scn c &optional extra)
  (loop while
        (not (or (equal c (peek scn))
                 (is-at-end scn)))
        do (progn
             (if extra (funcall extra))
             (advance scn))))

(defun scan-string (scn)
  (with-slots (source line start current) scn
    (consume-until scn #\"
                   (lambda ()
                     (if (equal #\Newline (peek scn))
                         (incf line))))

    (if (is-at-end scn)
        ;; we're missing the closing "
        (progn (lox-error line "Unterminated string.")
               (return-from scan-string nil)))

    ;; consume the closing "
    (advance scn)

    ;; Return the token + without-quotes string value
    (list
     'STRING
     (subseq source (- start 1) (- current 1)))))

(defun is-digit (c)
  (and
   (>= (char-code c) (char-code #\0))
   (<= (char-code c) (char-code #\9))))

(defun parse-double (str)
  ;; We should only get a number literal here.
  ;; Something like: 420.69
  ;; READ-FROM-STRING will do the conversion.
  (read-from-string str))

(defun scan-number (scn)
  (loop while (is-digit (peek scn))
        do (advance scn))

  (if (and (equal (peek scn) #\.)
           (is-digit (peek-next scn)))

      (progn (advance scn)
             (loop while (is-digit (peek scn))
                   do (advance scn))))

  (with-slots (source start current) scn
      (list 'NUMBER
            (parse-double (subseq source start current)))))

(defun is-alpha (c)
  (or
   (and
    (>= (char-code c) (char-code #\a))
    (<= (char-code c) (char-code #\z)))
   (and
    (>= (char-code c) (char-code #\A))
    (<= (char-code c) (char-code #\Z)))
   (equal (char-code c) (char-code #\_))))

(defun is-alphanumeric (c)
  (or (is-alpha c) (is-digit c)))

(defun make-dict (kvs)
  (let ((hash (make-hash-table :test 'equalp)))
    (dolist (kv kvs)
      (setf (gethash (car kv) hash) (cadr kv)))
    hash))

(defparameter *reserved-keywords*
  (make-dict
   '(("and" AND)
     ("class" CLASS)
     ("else" ELSE)
     ("false" FALSE)
     ("for" FOR)
     ("fun" FUN)
     ("if" IF)
     ("nil" NIL)
     ("or" OR)
     ("print" PRINT)
     ("return" RETURN)
     ("super" SUPER)
     ("this" THIS)
     ("true" TRUE)
     ("var" VAR)
     ("while" WHILE)
     )))

(defun dict-getval (dict key)
  (gethash key dict))

(defun scan-identifier (scn)
  ;; consume identifier from source
  (loop while (is-alphanumeric (peek scn))
        do (advance scn))

  (with-slots (start current source) scn
    (let* ((text (subseq source start current))
           (type (dict-getval *reserved-keywords* text)))
      (if type
          type
          'IDENTIFIER))))

(defun scan-token (c scn)
  (if (is-digit c)
      ;; nope out if it's a digit, instead of having
      ;; a case for each possible digit
      (return-from scan-token (scan-number scn)))

  (if (is-alpha c)
      ;; do the same for identifiers
      (return-from scan-token (scan-identifier scn)))

  (with-slots (start current source line) scn
    (flet ((match (expected)
             (if (and
                  (not (is-at-end scn))
                  (equal expected (char source current)))

                 (progn
                   (incf current) t))))

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
                 (consume-until scn #\Newline)
                 'SLASH))

        ;; skip over whitespace
        (#\Space nil)
        (#\Return nil)
        (#\Tab nil)
        (#\Newline (progn (incf line) nil))

        ;; string literals
        ;; SCAN-STRING returns a tuple ('STRING value)
        ;; `value' is then used as the `literal' param
        ;; of ADD-TOKEN
        (#\" (scan-string scn))

        (t (lox-error line "Unexpected character."))
        ))))

;; does Scanner really need to be a class?
(defmethod scan-tokens ((scn scanner))
  (with-slots (start current source line) scn
    (flet (;; this is rather a "make-token"
           (add-token (&optional type literal)
             (format t "add-token: type ~A literal ~A~%" type literal)
             (if type
                 (make-instance
                  'token
                  :type type
                  :lexeme (subseq source start current)
                  :literal literal
                  :line line))))

      (append
       (loop until (is-at-end scn)
             with token do
               (setf token
                     (progn
                       ;; We are at the beginning of the next lexeme
                       (setq start current)
                       (let ((scanned (scan-token (advance scn) scn)))
                         (if (listp scanned)
                             (add-token (car scanned) (cadr scanned))
                             (add-token scanned)))))
             when token collect token)
       (list (make-eof line))))))

;; Scanner.java end

(trace scan-token)
(trace is-at-end)
(trace advance)
(run "1 + (2 * 3)")
(run "print")
;; FIXME: fix the quote escaping
(run "print \"Hello Lox\";")

;; Parser start

;; Classes based on `expr' are to enable scanner and
;; parser to communicate.
(defclass expr () ()
  (:documentation "Superclass for all possible LOX expressions"))

(defun as-keyword (sym) (intern (string sym) :keyword))
(defun slot->defclass-slot (spec)
  (let ((name (first spec))
        (class (second spec)))
    (if class
        `(,name
          :initarg ,(as-keyword name)
          :initform (make-instance (quote ,class)))
        `(,name :initarg ,(as-keyword name)))))

(defun def-expr (name slots)
  `(defclass ,name (expr)
     ,(mapcar #'slot->defclass-slot slots)))

(defparameter *test2*
  '(binary
    ((left expr)
     (operator token)
     (right expr)
     (i-have-no-class))))
(apply #'def-expr *test2*)

(defmacro def-exprs (typelist)
  "Create `expr' subclasses from a list"
  `(progn
     ,@(mapcar
        (lambda (l) (apply #'def-expr l))
        typelist)))

;; Generate the classes
(def-exprs
    ((binary
      ((left expr)
       (operator token)
       (right expr)))

     (grouping
      ((expression expr)))

     (literal ((value)))

     (unary
      ((operator token)
       (expr right)))))

;; `C-c I' to test if it works
;; (make-instance 'grouping)




;; Parser end
