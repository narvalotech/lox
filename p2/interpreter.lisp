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
    'AND 'CLASS 'ELSE 'FALSE 'FUN 'FOR 'IF 'C-NIL 'OR
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
(defparameter *had-runtime-error* nil)

(defun read-file (path)
  (with-open-file
      (stream (uiop:parse-unix-namestring path))
    (loop for line = (read-line stream nil)
          until (eq line nil)
          unless (string-equal line "")
            collect line)))

(defun run (str)
  (let* ((scanner (make-instance 'scanner :source str))
         (tokens (scan-tokens scanner))
         (parser (make-instance 'parser :tokens tokens))
         (statements (parser-parse parser)))
    ;; do we need to handle *had-runtime-error*?
    ;; TODO: also make an `interpreter' instance like the java impl.
    (if (not *had-error*)
        (format t "~%~A~%"
                (interpret statements)))))

(defun run-file (path)
  (run (read-file path))
  (cond
    (*had-error* 65)
    (*had-runtime-error* 70)
    (t 0)))

(defun run-prompt ()
  (loop for line = (read-line *standard-input* nil)
        until (eq line nil) do
              (progn (run line)
                     (setq *had-error* nil)
                     (setq *had-runtime-error* nil))))

(defun main (&optional filepath)
  (if filepath
      (run-file filepath)
      (run-prompt)))

(defun lox-report (type line where message)
  (format t "[line ~A] Error ~A: ~A~%"
          line where message)
  (case type
    ('LOX-RUNTIME-ERROR (setq *had-runtime-error* t))
    ('LOX-ERROR (setq *had-error* t))))

(defun lox-error (line message)
  (lox-report 'LOX-ERROR line "" message))

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
    (sleep .001)
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
     (subseq source (+ start 1) (- current 1)))))

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
     ("nil" C-NIL)
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

;; Expr.java start

;; Classes based on `expr' are to enable scanner and
;; parser to communicate.
(defclass expr () ()
  (:documentation "Superclass for all possible LOX expressions"))

(defclass stmt () ()
  (:documentation "Superclass for all possible LOX statements"))

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

(defun def-stmt (name slots)
  `(defclass ,name (stmt)
     ,(mapcar #'slot->defclass-slot slots)))

(defparameter *test2*
  '(binary
    ((left expr)
     (operator token)
     (right expr)
     (i-have-no-class))))
(apply #'def-expr *test2*)

(defmacro def-subclasses (defclass-fn typelist)
  "Create subclasses from a list and the class-making function."
  `(progn
     ,@(mapcar
        (lambda (l) (apply defclass-fn l))
        typelist)))

;; Generate the expression classes
(def-subclasses def-expr
    ((assign
      ((name token)
       (value expr)))

     (binary
      ((left expr)
       (operator token)
       (right expr)))

     (grouping
      ((expression expr)))

     (literal ((value)))

     (logical ((left expr)
               (operator token)
               (right expr)))

     (unary
      ((operator token)
       (right expr)))

     ;; just `variable' violates the SBCL package lock
     ;; TODO: decide & apply naming scheme to all expr classes
     (expr-variable
      ((name token)))))

;; Generate the statement classes
(def-subclasses def-stmt
    ((stmt-block
      ((statements)))

     (stmt-expression
      ((expression expr)))

     (stmt-if
      ((condition expr)
       (then-branch stmt)
       (else-branch stmt)))

     ;; just `print' violates the SBCL package lock
     (stmt-print
      ((expression expr)))

     (stmt-var
      ((name token)
       (initializer expr)))

     (stmt-while
      ((condition expr)
       (body stmt)))))

;; `C-c I' to test if it works
;; (make-instance 'grouping)

;; I need to implement the visitor interface b/c book is written for java
;; I don't know enough of the impl to take a decision right now..
;; My gut tells me I won't need this pattern at all and I can just use
;; generic functions. I guess we'll see soon enough.

(defgeneric print-ast (expr)
  (:documentation "Print the Abstract Syntax Tree of an expression."))

(defun parenthesize (name &rest exprs)
  (with-output-to-string (builder)
    (format builder "(~A" name)
    (loop for expr in exprs do
      (progn
        (write-string " " builder)
        (write-string (print-ast expr) builder)))
    (write-string ")" builder)
    builder))

(defmethod print-ast ((expr binary))
  (with-slots (left operator right) expr
    (parenthesize (slot-value operator 'lexeme) left right)))

(defmethod print-ast ((expr grouping))
  (parenthesize "group" (slot-value expr 'expression)))

(defmethod print-ast ((expr literal))
  (with-slots (value) expr
    ;; Do I really need this?
    (if (equal value nil)
        "nil"
        (format nil "~A" value))))

(defmethod print-ast ((expr unary))
  (with-slots (operator right) expr
    (parenthesize (slot-value operator 'lexeme) right)))

;; Typing is hard, okay
(defmacro mi (&rest args)
  `(make-instance ,@args))

(format nil "~A"
        (print-ast (mi 'binary
                       :left (mi 'unary
                                 :operator (mi 'token :type 'MINUS :lexeme "-" :literal nil :line 1)
                                 :right (mi 'literal :value 123))
                       :operator (mi 'token :type 'STAR :lexeme "*" :literal nil :line 1)
                       :right (mi 'grouping :expression (mi 'literal :value 45.67)))))

;; Expr.java end
;; (also AstPrinter.java)

;; Parser.java start

(defclass parser ()
  ((tokens :initarg :tokens
           :initform '())
   (current :initarg :current
            :initform 0)))

;; TODO: put all that stuff in its own package, remove the
;; `parser-' prefixes.

;; Each method for parsing a grammar rule returns a syntax
;; tree for that rule. When the body contains a nonterminal
;; (ref to another rule), we call that other rule's method.

(defun parser-peek (prs)
  (with-slots (tokens current) prs
    (nth current tokens)))

(defun parser-previous (prs)
  (with-slots (tokens current) prs
    (nth (- current 1) tokens)))

(defun parser-is-at-end (prs)
  (equal 'EOF (slot-value (parser-peek prs) 'type)))

(defun parser-advance (prs)
  (if (not (parser-is-at-end prs))
      (incf (slot-value prs 'current)))
  ;; why can't we just save the prev value before incf?
  (parser-previous prs))

(defun parser-check (type prs)
  ;; `if' returns `nil' when cond not true
  (if (not (parser-is-at-end prs))
      (equal type (slot-value (parser-peek prs) 'type))))

;; TODO: standardize on passing 'self' as first params
;; -> allows `types' to be &rest instead of list
(defun parser-match (types prs)
  (loop for type in types do
    (if (parser-check type prs)
        (progn
          (parser-advance prs)
          (return-from parser-match t))))
  nil)

(defun rule-and (prs)
  (let ((expression (rule-equality prs)))
    (loop while (parser-match '(AND) prs) do
      (setq expression
            (make-instance 'logical
                           :left expression
                           :operator (parser-previous prs)
                           :right (rule-equality prs))))
    expression))

(defun rule-or (prs)
  (let ((expression (rule-and prs)))
    (loop while (parser-match '(OR) prs) do
      (setq expression
            (make-instance 'logical
                           :left expression
                           :operator (parser-previous prs)
                           :right (rule-and prs))))
    expression))

(defun rule-assignment (prs)
  (let ((expr (rule-or prs)))

    (if (parser-match '(EQUAL) prs)

        (let ((equals (parser-previous prs))
              (value (rule-assignment prs)))

          (if (typep expr 'expr-variable)

              (let ((name (slot-value expr 'name)))
                (return-from rule-assignment
                  (make-instance 'assign :name name :value value))))

          (parser-error equals "Invalid assignment target.")))
    expr))

(defun rule-expression (prs)
  (rule-assignment prs))

(defun parser-synchronize (prs)
  (parser-advance prs)

  ;; Discard tokens until the next statement boundary
  (loop while (not (parser-is-at-end prs)) do
    ;; Wait until we hit a semicolon
    (if (equal (slot-value (parser-previous prs) 'type) 'SEMICOLON)
        (return-from parser-synchronize))

    ;; Wait until the next token is a statement
    (if (member (slot-value (parser-peek prs) 'type)
                '(CLASS FUN VAR FOR IF WHILE PRINT RETURN))
        (return-from parser-synchronize))

    (parser-advance prs)))

(define-condition parser-error-condition (error)
  ((token :initarg :token :reader text)
   (message :initarg :message :reader text)))

(defun parser-error (token message)
  (with-slots (type line lexeme) token
    (if (equal type 'EOF)
        (lox-report 'LOX-ERROR line " at end" message)
        (lox-report 'LOX-ERROR line (format nil "at '~A'" lexeme) message)))

  ;; We throw the error when we want to synchronize. We
  ;; catch it higher in the call-stack, at the level of the
  ;; grammar rule we are synchronizing to.
  (error 'parser-error-condition
         :token token
         :message message))

(defun parser-consume (type err-str prs)
  (if (parser-check type prs)
      (parser-advance prs)
      (parser-error (parser-peek prs) err-str)))

(defun print-statement (prs)
  (let ((value (rule-expression prs)))
    (parser-consume 'SEMICOLON "Expect ';' after value." prs)
    (make-instance 'stmt-print :expression value)))

(defun expression-statement (prs)
  (let ((expr (rule-expression prs)))
    (parser-consume 'SEMICOLON "Expect ';' after value." prs)
    (make-instance 'stmt-expression :expression expr)))

(defun make-block (prs)
  (let ((statements
          (loop while
                (and (not (parser-check 'RIGHT-BRACE prs))
                     (not (parser-is-at-end prs)))
                collect
                (parser-declaration prs))))

    (parser-consume 'RIGHT-BRACE "Expect '}' after block." prs)

    statements))

(defun if-statement (prs)
  (parser-consume 'LEFT-PAREN "Expect '(' after 'if'." prs)

  (let ((condition (expression-statement prs)))
    (parser-consume 'RIGHT-PAREN "Expect ')' after if condition." prs)

    (let ((then-branch (statement prs))
          (else-branch nil))

      (if (parser-match '(ELSE) prs)
          (setq else-branch (statement prs)))

      (make-instance 'stmt-if
                     :condition condition
                     :then-branch then-branch
                     :else-branch else-branch))))

(defun while-statement (prs)
  (parser-consume 'LEFT-PAREN "Expect '(' after 'while'." prs)

  (let ((condition (rule-expression prs)))
    (parser-consume 'RIGHT-PAREN "Expect ')' after condition." prs)

    (make-instance 'stmt-while
                   :condition condition
                   :body (statement prs))))

(defun for-statement (prs)
  ;; very imperative, such wow
  (parser-consume 'LEFT-PAREN "Expect '(' after 'for'." prs)

  (let ((initializer)
        (condition)
        (increment)
        (body))

    (setq initializer
          (cond ((parser-match '(SEMICOLON) prs) nil)
                ((parser-match '(VAR) prs) (var-declaration prs))
                (t (expression-statement prs))))

    (setq condition
          (if (not (parser-check 'SEMICOLON prs))
              (rule-expression prs)
              nil))

    (parser-consume 'SEMICOLON "Expect ';' after loop condition." prs)

    (setq increment
          (if (not (parser-check 'RIGHT-PAREN prs))
              (rule-expression prs)
              nil))

    (parser-consume 'RIGHT-PAREN "Expect ')' after for clauses." prs)

    (setq body (statement prs))

    (if increment
        (setq body (make-instance 'stmt-block
                                  :statements
                                  (list body
                                        (make-instance 'stmt-expression
                                                       :expression increment)))))

    (if (not condition)
        (setq condition (make-instance 'literal :value t)))

    (setq body (make-instance 'stmt-while
                              :condition condition
                              :body body))

    (if initializer
        (setq body (make-instance 'stmt-block
                                  :statements (list initializer body))))

    body))

(defun statement (prs)
  (cond ((parser-match '(FOR) prs) (for-statement prs))
        ((parser-match '(IF) prs) (if-statement prs))
        ((parser-match '(PRINT) prs) (print-statement prs))
        ((parser-match '(WHILE) prs) (while-statement prs))
        ((parser-match '(LEFT-BRACE) prs)
         (make-instance 'stmt-block :statements (make-block prs)))
        (t (expression-statement prs))))

(defun var-declaration (prs)
  (let ((name (parser-consume 'IDENTIFIER "Expect variable name." prs))
        (initializer nil))

    (if (parser-match '(EQUAL) prs)
        (setq initializer (expression-statement prs))
        (parser-consume 'SEMICOLON "Expect ';' after variable declaration." prs))

    (make-instance 'stmt-var :name name :initializer initializer)))

(defun parser-declaration (prs)
  (handler-case
      (if (parser-match '(VAR) prs)
          (var-declaration prs)
          (statement prs))
    (parser-error-condition ()
      (progn (parser-synchronize prs) nil))))

(defun parser-parse (prs)
  (loop until (parser-is-at-end prs)
        collect (parser-declaration prs)))

(defun rule-primary (prs)
  (flet ((match (&rest types) (parser-match types prs))
         (literal (value) (make-instance 'literal :value value)))
    (cond
      ;; TODO: do I need to distinguish between false and nil?
      ((match 'FALSE) (literal nil))
      ((match 'TRUE) (literal t))
      ((match 'C-NIL) (literal nil))
      ((match 'NUMBER 'STRING)
       (literal (slot-value (parser-previous prs) 'literal)))
      ((match 'IDENTIFIER)
       (make-instance 'expr-variable :name (parser-previous prs)))
      ((match 'LEFT-PAREN)
       (let ((expression (rule-expression prs)))
         (parser-consume 'RIGHT-PAREN "Expect ')' after expression." prs)
         (make-instance 'grouping :expression expression)))
      (t (parser-error (parser-peek prs) "Expect expression.")))))

(defun rule-unary (prs)
  (if (parser-match (list 'BANG 'MINUS) prs)
      (make-instance 'unary
                     :operator (parser-previous prs)
                     :right (rule-unary prs)))
  (rule-primary prs))

;; TODO: make a helper method for all this redundant code in
;; the binary rules/operators.

(defun rule-factor (prs)
  (let ((expression (rule-unary prs)))
    (loop while
          (parser-match (list 'SLASH 'STAR) prs) do
            (setq expression
                  (make-instance 'binary
                                 :left expression
                                 :operator (parser-previous prs)
                                 :right (rule-unary prs))))
    expression))

(defun rule-term (prs)
  (let ((expression (rule-factor prs)))
    (loop while
          (parser-match (list 'MINUS 'PLUS) prs) do
            (setq expression
                  (make-instance 'binary
                                 :left expression
                                 :operator (parser-previous prs)
                                 :right (rule-factor prs))))
    expression))

(defun rule-comparison (prs)
  (let ((expression (rule-term prs)))
    (loop while
          (parser-match (list 'GREATER 'GREATER-EQUAL 'LESS 'LESS-EQUAL) prs) do
            (setq expression
                  (make-instance 'binary
                                 :left expression
                                 :operator (parser-previous prs)
                                 :right (rule-term prs))))
    expression))

(defun rule-equality (prs)
  (let ((expression (rule-comparison prs)))
    (loop while (parser-match (list 'BANG-EQUAL 'EQUAL-EQUAL) prs) do
      (setq expression
            (make-instance 'binary
                           :left expression
                           :operator (parser-previous prs)
                           :right (rule-comparison prs))))
    expression))

;; Parser.java end

;; Interpreter.java start
(defun is-truthy (obj)
  ;; We don't have a difference between `nil' and `null'.
  (if obj t nil))

(defgeneric evaluate (stmt)
  (:documentation "Execute a statement or expression."))

(defmethod evaluate ((expr literal))
  (slot-value expr 'value))

(defmethod evaluate ((expr grouping))
  (evaluate (slot-value expr 'expression)))

(define-condition lox-runtime-error-condition (error)
  ((token :initarg :token :reader text)
   (message :initarg :message :reader text)))

(defun lox-runtime-error (token message)
  (with-slots (type line) token
    (lox-report 'LOX-RUNTIME-ERROR line "" (format nil "runtime-error: ~A" message)))

  (error 'lox-runtime-error-condition
         :token token
         :message message))

(defun check-number-operand (operator operand)
  (if (not (numberp operand))
      (lox-runtime-error operator "Operand must be a number.")))

(defmethod evaluate ((expr unary))
  (let* ((operator (slot-value expr 'operator))
         (right (evaluate (slot-value expr 'right)))
         (type (slot-value operator 'type)))

    (case type
      ('MINUS (progn
                (check-number-operand operator right)
                (- right)))
      ('BANG (is-truthy right))

      (t (error "unreachable")))))

(defun is-equal (a b)
  ;; we'll see if this is enough for lox
  (equal a b))

(defun check-number-operands (operator left right)
  (if (not (and (numberp left) (numberp right)))
      (lox-runtime-error operator "Operands must be numbers.")))

(defmethod evaluate ((expr binary))
  ;; Evaluate `left' first
  (let* ((left (evaluate (slot-value expr 'left)))
         (right (evaluate (slot-value expr 'right)))
         (operator (slot-value expr 'operator))
         (type (slot-value operator 'type)))

    ;; DRY (better than wet)
    (if (not (equal type 'PLUS))
        (check-number-operands
         operator left right))

    (case type
      ('MINUS (- left right))
      ('SLASH (/ left right))
      ('STAR  (* left right))
      ('GREATER (> left right))
      ('GREATER_EQUAL (>= left right))
      ('LESS (< left right))
      ('LESS_EQUAL (<= left right))
      ('BANG_EQUAL (not (is-equal left right)))
      ('EQUAL_EQUAL (is-equal left right))

      ;; (over)loaded term
      ('PLUS
       (cond
         ((and (stringp left) (stringp right))
          (concatenate 'string left right))
         ((and (numberp left) (numberp right))
          (+ left right))
         ((t) (lox-runtime-error operator "Y U no string? (or number)"))))

      (t (error "unreachable")))))

(defmethod evaluate ((expr logical))
  (with-slots (left operator right) expr
    (let ((obj-left (evaluate left))
          (op-type (slot-value operator 'type)))

      (if (equal op-type 'OR)
          (if (is-truthy obj-left)
              (return-from evaluate obj-left))
          ;; other op type (ie AND)
          (if (not (is-truthy obj-left))
              (return-from evaluate obj-left)))

      (evaluate right))))


(defmethod execute ((stmt stmt-expression))
  (evaluate (slot-value stmt 'expression)))

(defmethod execute ((stmt stmt-print))
  (let ((value (evaluate (slot-value stmt 'expression))))
    (format t "~A~%" value)))

(defmethod execute ((stmt stmt-var))
  (let ((value nil)
        (init (slot-value stmt 'initializer))
        (lexeme (slot-value (slot-value stmt 'name) 'lexeme)))

    (if init
        (setq value (execute init)))

    (env-define lexeme value *env*)
    nil))

(defmethod execute ((stmt stmt-if))
  (with-slots (condition then-branch else-branch) stmt
    (cond
      ((is-truthy (evaluate condition))
       (execute then-branch))
      (else-branch
       (execute else-branch))
      (t nil))))

(defmethod execute ((stmt stmt-while))
  (with-slots (condition body) stmt
    (loop while (is-truthy (evaluate condition))
          do (execute body)))
  nil)

;; TODO: don't edit the *env* dynamic binding
(defun execute-block (statements env)
  (let ((previous *env*))
    (unwind-protect
         (progn
           (setf *env* env)
           (dolist (statement statements)
             (execute statement)))
      (setf *env* previous))))

(defmethod execute ((stmt stmt-block))
  (execute-block (slot-value stmt 'statements)
                 (make-instance 'environment :enclosing *env*))
  nil)

(defun stringify (obj)
  ;; lox seems to match pretty closely `format' output
  (format nil "~A" obj))

;; AST in -> interpreted result out
(defun interpret (statements)
  (handler-case
      (loop for statement in statements do
        (execute statement))
    ;; we could use RESTART-CASE instead
    (lox-runtime-error-condition () nil)))

;; Interpreter.java end

;; Environment.java start

(defclass environment ()
  ((values :initform (make-hash-table :test 'equalp))
   (enclosing :initform nil :initarg :enclosing))
  (:documentation "Stores variable bindings."))

(defun env-define (name value env)
  ;; here, `name' is a string
  (setf (gethash name (slot-value env 'values)) value))

(defun env-get (name env)
  ;; Here name is a 'token instance
  ;; TODO: use `declare' to enforce this
  (let* ((parent-env (slot-value env 'enclosing))
         (lexeme (slot-value name 'lexeme))
         (values (slot-value env 'values))
         (value (gethash lexeme values)))

    (if (and (not value) parent-env)
        (setf value (env-get name parent-env)))

    (if value value
        (lox-runtime-error
         name
         (format nil "Undefined variable '~A'." lexeme)))))

(defun env-assign (name value env)
  ;; Here name is a 'token instance
  ;; TODO: use `declare' to enforce this
  (let ((lexeme (slot-value name 'lexeme))
        (parent-env (slot-value env 'enclosing))
        (values (slot-value env 'values)))

    (multiple-value-bind (current-val key-exists) (gethash lexeme values)
      (declare (ignore current-val))

      (if (and (not key-exists) parent-env)
          (return-from env-assign
            (env-assign name value parent-env)))

      (if key-exists
          (env-define lexeme value env)   ; returns the new value of `name'. i.e. `value'.
          (lox-runtime-error name (format nil "Undefined variable '~A'." lexeme))))))

(defmethod evaluate ((expr expr-variable))
  (env-get (slot-value expr 'name) *env*))

(defmethod evaluate ((expr assign))
  (with-slots (name value) expr
    (env-assign name (evaluate value) *env*)))

(defparameter *env* (make-instance 'environment))

;; Environment.java end

;; tests
;; (trace scan-token)
;; (trace is-at-end)
;; (trace advance)
(run "print 1 + (2 * 3);")
(run "2 / 5 + 2 * 3")                   ; missing semicolon
(run "2 +/ 3")                          ; illegal expression
(run "print")                           ; missing expr after print
(run (format nil "print \"Hello Lox\" ~%; ~%print (1 + 2 / 3); 3+3;"))
;; test from the book
(run (format nil "print \"one\";~%print true;~%print 2 + 1;"))
(run "var a = 1; var b = 2; print a + b;")
(run "var a = 1; var b = 2; b = 44; print a + b;")
(run "b = 20; print a + b;")
(run "c = 3;")                          ; undefined var
(run "
var a = \"global a\";
var b = \"global b\";
var c = \"global c\";
{
  var a = \"outer a\";
  var b = \"outer b\";
  {
    var a = \"inner a\";
    print a;
    print b;
    print c;
  }
  print a;
  print b;
  print c;
}
print a;
print b;
print c;
")

(progn (setq *had-runtime-error* nil)
       (setq *had-error* nil))
(run "print \"hi\" or 2;")
(run "print nil or \"yes\";")
(run "
{
var z = 0;

while (z < 10) {
  print z;
  z = z + 1;
}
}
")

(run "
for (var i = 0; i < 10; i = i + 1) {
  print i;
}
")

(run "
{
  var i = 0;
  while (i < 10) {
    print i;
    i = i + 1;
  }
}
")

;; Fibonacci example from end of `control-flow' in book
(run "
var a = 0;
var temp;

for (var b = 1; a < 10000; b = temp + b) {
  print a;
  temp = a;
  a = b;
}
")
