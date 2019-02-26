(defpackage :lem-ruby-mode
  (:use :cl :lem :lem.language-mode)
  (:export :*ruby-mode-hook*))
(in-package :lem-ruby-mode)

(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tm-string-region (separator)
  (make-tm-region `(:sequence ,separator)
                  `(:sequence ,separator)
                  :name 'syntax-string-attribute
                  :patterns (make-tm-patterns (make-tm-match "\\\\."))))

(defun floating-point-literals ()
  (let* ((digitpart "([0-9](_?[0-9])*)")
         (fraction (format nil "\\.(~a)" digitpart))
         (exponent (format nil "((e|E)(\\+|\\-)?(~a))" digitpart))
         (pointfloat (format nil "(((~a)?(~a))|((~a)\\.))" digitpart fraction digitpart))
         (exponentfloat (format nil "(((~a)|(~a))(~a))" digitpart pointfloat exponent)))
    (format nil "\\b((~a)|(~a))\\b" pointfloat exponentfloat)))

(defun make-tmlanguage-ruby ()
  (let* ((integer-literals "\\b(([1-9](_?[0-9])*)|(0(_?0)*)|(0(b|B)(_?[01])+)|(0(o|O)(_?[0-7])+)|(0(x|X)(_?[0-9a-fA-F])+))\\b")
         (floating-point-literals (floating-point-literals))
         (patterns (make-tm-patterns
                    ;; functions
                    (make-tm-match "^\\s*def\\s+\(?:[^(\\s\\n.]*\\.\)?\([^(\\s\\n]+\)"
                                   :captures (vector (make-tm-name 'syntax-keyword-attribute)
                                                     (make-tm-name 'syntax-function-name-attribute)))
                    ;; keywords
                    (make-tm-match (tokens :word-boundary
                                           '("alias" "and" "begin" "break" "case" "class"
                                             "def" "defined?" "do" "elsif" "else" "fail"
                                             "ensure" "for" "end" "if" "in" "module" "next"
                                             "not" "or" "redo" "rescue" "retry" "return"
                                             "self" "super" "then" "unless" "undef" "until"
                                             "when" "while" "yield"))
                                   :name 'syntax-keyword-attribute)
                    ;; built-in methods on Kernel
                    (make-tm-match (tokens :word-boundary
                                           '("at-exit" "autoload" "autoload?" "callcc" "catch"
                                             "eval" "exec" "format" "lambda" "load" "loop" "open"
                                             "p" "print" "printf" "proc" "putc" "puts" "require"
                                             "require_relative" "spawn" "sprintf" "syscall"
                                             "system" "throw" "trace_var" "trap" "untrace_var"
                                             "warn"))
                                   :name 'syntax-builtin-attribute)
                    ;; keyword-like private methods on Module
                    (make-tm-match (tokens :word-boundary
                                           '("alias_method" "attr" "attr_accessor" "attr_reader"
                                             "attr_writer" "define_method" "extend" "include"
                                             "module_function" "prepend" "private_class_method"
                                             "private_constant" "public_class_method"
                                             "public_constant" "refine" "using"))
                                   :name 'syntax-builtin-attribute)
                    ;; Kernel methods that have no required arguments
                    (make-tm-match (tokens :word-boundary
                                           '("__callee__" "__dir__" "__method__" "abort" "binding"
                                             "block_given?" "caller" "exit" "exit!" "fail" "fork"
                                             "global_variables" "local_variables" "private"
                                             "protected" "public" "raise" "rand" "readline"
                                             "readlines" "sleep" "srand"))
                                   :name 'syntax-builtin-attribute)
                    ;;
                    (make-tm-match (tokens :word-boundary
                                           '("nil" "true" "false"))
                                   :name 'syntax-constant-attribute)
                    (make-tm-match (tokens :word-boundary
                                           '("__LINE__" "__ENCODING__" "__FILE__"))
                                   :name 'syntax-builtin-attribute)
                    (make-tm-match "\(^|[^:]\) \(:@\{0,2\} \(?:\sw|\s_\)+\)"
                    (make-tm-region "#" "$" :name 'syntax-comment-attribute)
                    (make-tm-region "=begin" "=end" :name 'syntax-comment-attribute)
                    (make-tm-match (tokens :word-boundary
                                           '("self" "nil" "true" "false" "__FILE__" "__LINE__" "__ENCODING__"))
                                   :name 'syntax-constant-attribute)
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    (make-tm-match integer-literals
                                   :name 'syntax-constant-attribute)
                    (make-tm-match floating-point-literals
                                   :name 'syntax-constant-attribute)
                    #+nil
                    (make-tm-match (tokens nil '("+" "-" "*" "**" "/" "//" "%" "@"
                                                 "<<" ">>" "&" "|" "^" "~"
                                                 "<" ">" "<=" ">=" "==" "!="))
                                   :name 'syntax-keyword-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *ruby-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :block-string-pairs '(("=begin" . "=end"))
                :line-comment-string "#"))
        (tmlanguage (make-tmlanguage-ruby)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode ruby-mode language-mode
    (:name "ruby"
     :keymap *ruby-mode-keymap*
     :syntax-table *ruby-syntax-table*
     :mode-hook *ruby-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'ruby-calc-indent
        (variable-value 'line-comment) "#"
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun))

(defun ruby-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (point-column point)))
      (+ column (- tab-width (rem column tab-width))))))

(defun beginning-of-defun (point n)
  (loop :repeat n :do (search-backward-regexp point "^\\w")))

(defun end-of-defun (point n)
  (with-point ((p point))
    (loop :repeat n
          :do (line-offset p 1)
              (unless (search-forward-regexp p "^\\w") (return)))
    (line-start p)
    (move-point point p)))

(pushnew (cons "\\.rb$" 'ruby-mode) *auto-mode-alist* :test #'equal)
