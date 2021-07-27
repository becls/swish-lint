;;; Copyright 2021 Beckman Coulter, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(import (indent))

;; This is handy for debugging failing cases.
(define write-files? #f)

(define mat-name (make-process-parameter #f))

(define-syntax indent-mat
  (syntax-rules ()
    [(_ name settings e1 e2 ...)
     (isolate-mat name settings
       (mat-name 'name)
       (let () e1 e2 ...))]))

(define-syntax code
  (syntax-rules ()
    [(_ str ...)
     (ct:join #\newline str ...)]))

(define (spaces n str)
  (string-append (make-string n #\space) str))

(define file-counter (make-process-parameter 0))

(define (pretty-token x)
  (match x
    [`(<token> ,name ,props)
     (format "<token ~s ~a>" name (ht:keys props))]
    [,_
     (guard (list? x))
     (map pretty-token x)]
    [,_ (format "~s" x)]))

(define (print-details expected actual)
  (newline)
  (printf "expected:\n")
  (printf "~a\n" (pretty-token expected))
  (printf "actual:\n")
  (printf "~a\n" (pretty-token actual))
  (newline))

(define (check-tokens* pred expected input)
  (let ([actual (tokenize input)])
    (unless (pred expected actual)
      (print-details expected actual)
      (throw `(parse-failed
               (expected ,expected)
               (actual ,actual))))))

(define (check-tokens expected input)
  (check-tokens*
   (lambda (expected actual)
     (equal? expected (map (<token> name) actual)))
   expected
   input))

(define (check-indent expected input)
  (define (maybe-write-file prefix text)
    (when write-files?
      (let* ([path (format "~a/~a-~a.ss" prefix (mat-name) (file-counter))]
             [op (open-file-to-replace (make-directory-path path))])
        (on-exit (close-port op)
          (display text op)
          (newline op)))))

  (file-counter (+ (file-counter) 1))
  (maybe-write-file "mat-expected" expected)
  (match (try (indent input))
    [`(catch ,_ ,err)
     (maybe-write-file "mat-actual" input)
     (throw err)]
    [,actual
     (maybe-write-file "mat-actual" actual)
     (unless (equal? expected actual)
       (print-details expected actual)
       (throw `(indentation-failed
                (expected ,expected)
                (actual ,actual))))]))

(indent-mat non-keywords ()
  (check-indent
   (code
    "(function arg1"
    "  arg2"
    "  ...)")
   (code
    "(function arg1"
    "arg2"
    "...)"))

  (check-indent
   (code
    "(function"
    " arg1"
    " arg2"
    " ...)")
   (code
    "(function"
    "arg1"
    "arg2"
    "...)"))

  (check-indent
   (code
    "((function s-arg1 ...)"
    " arg1"
    " arg2"
    " ...)")
   (code
    "((function s-arg1 ...)"
    "arg1"
    "arg2"
    "...)"))
  )

(indent-mat data ()
  (check-indent
   (code "")
   (code "     "))

  (check-indent
   (code
    "(1 2 3"
    " 4 5 6)")
   (code
    "(1 2 3"
    "4 5 6)"))

  (check-indent
   (code
    "#(1 2 3"
    "  4 5 6)")
   (code
    "#(1 2 3"
    "4 5 6)"))

  (check-indent
   (code
    "#vu8(#x01 #x02 #x03"
    "     #x04 #x05 #x06)")
   (code
    "#vu8(#x01 #x02 #x03"
    "#x04 #x05 #x06)"))

  (check-indent
   (code
    "#vfx(#x01 #x02 #x03"
    "     #x04 #x05 #x06)")
   (code
    "#vfx(#x01 #x02 #x03"
    "#x04 #x05 #x06)"))

  (check-indent
   (code
    "(#\\1 #\\2 #\\3"
    " #\\4 #\\5 #\\6)")
   (code
    "(#\\1 #\\2 #\\3"
    "#\\4 #\\5 #\\6)"))

  (check-indent
   (code
    "'(alpha"
    "  beta"
    "  gamma)")
   (code
    "'(alpha"
    "beta"
    "gamma)"))
  )

(indent-mat definitions ()
  (check-indent
   (code
    "(define foo"
    "  value")
   (code
    "(define foo"
    "value"))

  (check-indent
   (code
    "(define (foo x)"
    "  body"
    "  ...")
   (code
    "(define (foo x)"
    "body"
    "..."))

  (check-indent
   (code
    "(define (foo"
    "         x)"
    "  body"
    "  ...")
   (code
    "(define (foo"
    "x)"
    "body"
    "..."))

  (check-indent
   (code
    "(define (foo"
    "         x"
    "         y)"
    "  body"
    "  ...")
   (code
    "(define (foo"
    "x"
    "y)"
    "body"
    "..."))
  )

(indent-mat 0-keywords ()
  (check-indent
   (code
    "(dynamic-wind"
    "  void"
    "  void"
    "  void)")
   (code
    "(dynamic-wind"
    "void"
    "void"
    "void)"))

  (check-indent
   (code
    "(dynamic-wind void"
    "              void"
    "              void)")
   (code
    "(dynamic-wind void"
    "void"
    "void)"))

  (check-indent
   (code
    "(begin"
    "  exp1"
    "  exp2"
    "  exp3)")
   (code
    "(begin"
    "exp1"
    "exp2"
    "exp3)"))

  (check-indent
   (code
    "(begin exp1"
    "       exp2"
    "       exp3)")
   (code
    "(begin exp1"
    "exp2"
    "exp3)"))
  )

(indent-mat 1-keywords ()
  (check-indent
   (code
    "(lambda args"
    "  (list args))")
   (code
    "(lambda args"
    "(list args))"))

  (check-indent
   (code
    "(lambda (args)"
    "  args)")
   (code
    "(lambda (args)"
    "args)"))
  )

(indent-mat 2-keywords ()
  (check-indent
   (code
    "(do ([i 0 (fx+ i 1)])"
    "    ((= i 12)))")
   (code
    "(do ([i 0 (fx+ i 1)])"
    "((= i 12)))"))

  (check-indent
   (code
    "(do ([i 0 (fx+ i 1)])"
    "    ((= i 12))"
    "  body"
    "  ...)")
   (code
    "(do ([i 0 (fx+ i 1)])"
    "((= i 12))"
    "body"
    "...)"))

  (check-indent
   (code
    "(do"
    "    ([i 0 (fx+ i 1)])"
    "    ((= i 12))"
    "  body"
    "  ...)")
   (code
    "(do"
    "([i 0 (fx+ i 1)])"
    "((= i 12))"
    "body"
    "...)"))
  )

(indent-mat normal-keywords ()
  (for-each
   (lambda (kw)
     (let ([len (string-length kw)])
       (check-indent
        (code
         (format "(~a" kw)
         " test1"
         " test2"
         " test3)")
        (code
         (format "(~a" kw)
         "test1"
         "test2"
         "test3)"))
       (check-indent
        (code
         (format "(~a test1" kw)
         (spaces (+ len 2) "test2")
         (spaces (+ len 2) "test3)"))
        (code
         (format "(~a test1" kw)
         "test2"
         "test3)"))))
   '("and" "or" "fx+" "#2%fx+" "#3%fx+"))
  )

(indent-mat let-special ()
  (check-indent
   (code
    "(let ()"
    "  body"
    "  ...)")
   (code
    "(let ()"
    "body"
    "...)"))

  (check-indent
   (code
    "(let ([x 12]"
    "      [y 13])"
    "  body"
    "  ...)")
   (code
    "(let ([x 12]"
    "[y 13])"
    "body"
    "...)"))

  (check-indent
   (code
    "(let lp ([x 12]"
    "         [y 13])"
    "  body"
    "  ...)")
   (code
    "(let lp ([x 12]"
    "[y 13])"
    "body"
    "...)"))

  ;; Emacs lines up the open square paren under the first character of
  ;; the named-let. This code currently does not handle this case.
  (check-indent
   (code
    "(let lp"
    "  ([x 12]"
    "   [y 13])"
    "  body"
    "  ...)")
   (code
    "(let lp"
    "([x 12]"
    "[y 13])"
    "body"
    "...)"))
  )

(indent-mat unbalanced ()
  (check-indent
   (code ")")
   (code ")"))

  ;; Emacs punts on this case.
  (check-indent
   (code
    ")))))(begin"
    "       123)")
   (code
    ")))))(begin"
    "123)"))

  ;; Emacs punts on this case.
  (check-indent
   (code
    ")))))(let ([x 12]"
    "           [y 13])"
    "       body"
    "       ...)")
   (code
    ")))))(let ([x 12]"
    "[y 13])"
    "body"
    "...)"))

  (check-indent
   (code
    "(let ([x 12]"
    "      [y 13)]"
    "  body"
    "  ...)")
   (code
    "(let ([x 12]"
    "[y 13)]"
    "body"
    "...)"))
  )

(indent-mat other-cases ()
  (check-indent
   (code
    "(cond"
    " [test1  "
    "  1])")
   (code
    "(cond"
    "[test1  "
    "1])"))

  (check-indent
   (code
    "(cond"
    " [(foo? x)"
    "  (foo-conseq)]"
    " [else"
    "  (else-conseq)])")
   (code
    "(cond"
    "[(foo? x)"
    "(foo-conseq)]"
    "[else"
    "(else-conseq)])"))

  (check-indent
   (code
    "(cond"
    " [(foo? x)"
    "  (foo-conseq)]"
    " [else                                  ; line comment"
    "  (else-conseq)])")
   (code
    "(cond"
    "[(foo? x)"
    "(foo-conseq)]"
    "[else ; line comment"
    "(else-conseq)])"))

  (check-indent
   (code
    "(cond"
    " [test? 1]"
    " [else                                  ; line comment"
    "  4])")
   (code
    "(cond"
    "[test? 1]"
    "[else ; line comment"
    "4])"))

  (check-indent
   (code
    "(match x"
    "  [\"string\""
    "   body"
    "   ...]"
    "  [#\\newline"
    "   body"
    "   ...]"
    "  [#(vector ,thing)"
    "   body"
    "   ...]"
    "  ['quoted"
    "   body"
    "   ...]"
    "  [`quasiquoted"
    "   body"
    "   ...]"
    "  [,x"
    "   body"
    "   ...]"
    "  [,@x"
    "   body"
    "   ...]"
    "  [#'x"
    "   body"
    "   ...]"
    "  [#`x"
    "   body"
    "   ...]"
    "  [#,x"
    "   body"
    "   ...]"
    "  [#,@x"
    "   body"
    "   ...]"
    "  [#vu8(1 2 3)"
    "   body"
    "   ...]"
    "  [#vfx(1 2 3)"
    "   body"
    "   ...]"
    "  )")
   (code
    "(match x"
    "[\"string\""
    "body"
    "...]"
    "[#\\newline"
    " body"
    "...]"
    "[#(vector ,thing)"
    " body"
    "...]"
    "['quoted"
    " body"
    "...]"
    "[`quasiquoted"
    " body"
    "...]"
    "[,x"
    " body"
    "...]"
    "[,@x"
    " body"
    "...]"
    "[#'x"
    "body"
    "...]"
    "[#`x"
    "body"
    "...]"
    "[#,x"
    "body"
    "...]"
    "[#,@x"
    "body"
    "...]"
    "[#vu8(1 2 3)"
    "body"
    "...]"
    "[#vfx(1 2 3)"
    "body"
    "...]"
    ")"))
  )

;; This header is handy in Emacs with column-number-mode to look
;; closely at alignment.
(define align-header
  (ct:string-append "#|"
    "23456789"
    "0123456789"
    "0123456789"
    "0123456789"
    "0123456789"
    "0123456789"
    "0123456789"
    "0123456"
    "|#"))

(indent-mat standalone-comments ()
  (check-indent
   ";;; stay here"
   ";;; stay here")

  (check-indent
   (spaces 4 ";;; stay here")
   (spaces 4 ";;; stay here"))

  (check-indent
   ";; align to left indent"
   (spaces 4 ";; align to left indent"))

  (for-each
   (lambda (n)
     (check-indent
      (spaces 40 "; align middle")
      (spaces n "; align middle")))
   '(0 10 20 30 40 50 60 70 80))

  (check-indent
   (code
    "(let ()"
    "  ;; (open a paren and"
    "  ;; close it on the next line)"
    "  (void))")
   (code
    "(let ()"
    ";; (open a paren and"
    ";; close it on the next line)"
    "(void))"))

  (check-indent
   (code
    "(let ()"
    "  ;; (open a paren and"
    "  ;; never close it"
    "  (void))")
   (code
    "(let ()"
    ";; (open a paren and"
    ";; never close it"
    "(void))"))

  ;; Growing length identifiers
  (check-indent
   (let ([op (open-output-string)])
     (display align-header op)
     (newline op)
     ;; add a let-contour to help manually reindent in Emacs
     (display "(let ()" op)
     (newline op)
     (let ([anchor (- 40 2)])
       (do ([n 0 (+ n 1)]) ((= n 90))
         (display "  " op)
         (display (make-string n #\a) op)
         (display (make-string (max 1 (- anchor n)) #\space) op)
         (display (if (< n anchor) "; align middle" "; align against code") op)
         (newline op)
         (newline op)))
     (display "  )" op)
     (newline op)
     (get-output-string op))
   (let ([op (open-output-string)])
     (display align-header op)
     (newline op)
     (display "(let ()" op)
     (newline op)
     (let ([anchor (- 40 2)])
       (do ([n 0 (+ n 1)]) ((= n 90))
         (display (make-string n #\a) op)
         (display " " op)
         (display (if (< n anchor) "; align middle" "; align against code") op)
         (newline op)
         (newline op)))
     (display "  )" op)
     (newline op)
     (get-output-string op)))

  ;; Growing length comments
  (check-indent
   (let ([op (open-output-string)])
     (display align-header op)
     (newline op)
     ;; add a let-contour to help manually reindent in Emacs
     (display "(let ()" op)
     (newline op)
     (let ([anchor (- 40 10 2)])
       (do ([n 0 (+ n 1)]) ((= n 90))
         (display "  a123456789" op)
         (display (make-string (if (< n anchor)
                                   anchor
                                   (max 1 (- anchor (- n anchor))))
                    #\space) op)
         (display "; " op)
         (display (make-string n #\a) op)
         (newline op)
         (newline op)))
     (display "  )" op)
     (newline op)
     (get-output-string op))
   (let ([op (open-output-string)])
     (display align-header op)
     (newline op)
     (display "(let ()" op)
     (newline op)
     (do ([n 0 (+ n 1)]) ((= n 90))
       (display "  a123456789" op)
       (display " " op)
       (display "; " op)
       (display (make-string n #\a) op)
       (newline op)
       (newline op))
     (display "  )" op)
     (newline op)
     (get-output-string op)))
  )

(indent-mat align-comments ()
  (check-indent
   (code
    (spaces 40 "; align middle")
    (spaces 40 "; align prior"))
   (code
    "; align middle"
    "; align prior"))

  (for-each
   (lambda (n m)
     (check-indent
      (code
       (spaces 40 "; align middle")
       (spaces 40 "; align prior"))
      (code
       (spaces n "; align middle")
       (spaces m "; align prior"))))
   '(0 10 20 30 40 50 60 70 80)
   '(80 70 60 50 40 30 20 10 0))
  )

(indent-mat block-comments ()
  (check-indent
   (code "#| Early eof")
   (code "#| Early eof"))

  (check-indent
   (code "#| On a single line |#")
   (code "#| On a single line |#"))

  (check-indent
   (code
    "#| On multiple"
    "   lines |#")
   (code
    "#| On multiple"
    "   lines |#"))

  (check-indent
   (code
    "(let ()"
    "  (let ()"
    "    (extract-block-comment #|"
    "Possibly embedded code"
    "     | <= or ASCII art => |"
    "  --- keep existing indent ---"
    "|#"
    "     )"
    "    )"
    "  )")
   (code
    "(let ()"
    "(let ()"
    "(extract-block-comment #|"
    "Possibly embedded code"
    "     | <= or ASCII art => |"
    "  --- keep existing indent ---"
    "|#"
    ")"
    ")"
    ")"))
  )

(indent-mat datum-comments ()

  (check-indent
   (code "#; identifier")
   (code "#; identifier"))

  (check-indent
   (code "#; 'quoted")
   (code "#; 'quoted"))

  (check-indent
   (code "#; (balanced)")
   (code "#; (balanced)"))

  (check-indent
   (code "#; (unbalanced expr")
   (code "#; (unbalanced expr"))

  (check-indent
   (code
    "#;; line comment"
    "identifier")
   (code
    "#;; line comment"
    "identifier"
    ))

  (check-indent
   (code
    "#;;; line comment"
    "identifier")
   (code
    "#;;; line comment"
    "identifier"
    ))

  (check-indent
   (code
    "#;;;; line comment"
    "identifier")
   (code
    "#;;;; line comment"
    "identifier"
    ))

  (check-indent
   (code
    "#; \"string\""
    "(let ()"
    "  (+ x y))")
   (code
    "#; \"string\""
    "(let ()"
    "(+ x y))"))

  (check-indent
   (code
    "#; \"incomplete string"
    "(let ()"
    "(+ x y))")
   (code
    "#; \"incomplete string"
    "(let ()"
    "(+ x y))"))

  (check-indent
   (code
    "#;(let ()"
    "    (+ x y))")
   (code
    "#;(let ()"
    "(+ x y))"))

  (check-indent
   (code
    "#;"
    "(define (foo x)"
    "  12)")
   (code
    "#;"
    "(define (foo x)"
    "12)"))

  (check-indent
   (code
    "#;; Mash in a line comment"
    "(let ()"
    "  (+ x y))")
   (code
    "#;; Mash in a line comment"
    "(let ()"
    "(+ x y))"))

  (check-indent
   (code
    "#;(check-indent"
    "   (code"
    "    \"(let lp\""
    "    \"    ([x 12]\""
    "    \"     [y 13])\""
    "    \"  body\""
    "    \"  ...)\")"
    "   (code"
    "    \"(let lp\""
    "    \"([x 12]\""
    "    \"[y 13])\""
    "    \"body\""
    "    \"...)\"))")
   (code
    "#;(check-indent"
    "(code"
    "\"(let lp\""
    "\"    ([x 12]\""
    "\"     [y 13])\""
    "\"  body\""
    "\"  ...)\")"
    "(code"
    "\"(let lp\""
    "\"([x 12]\""
    "\"[y 13])\""
    "\"body\""
    "\"...)\"))"))
  )

(indent-mat advanced-comments ()
  ;; Example expression from Chez Scheme's mats. Check our ability to
  ;; read a variety of comment ineractions.
  (check-indent
   (code
    "(let ()"
    "                                        ; this is the first comment"
    "  (a                                    ; second comment"
    "   #;(third                             ; comment in comment"
    "      comment #;(comment #1=e in"
    "                  . #;(comment in comment in comment)"
    "                  comment)) b           ; fourth comment"
    "   c #| fifth comment #| more"
    "nesting here |# |# d"
    "                                        ; sixth and final comment"
    "   #1#)")
   (code
    "(let ()"
    "; this is the first comment"
    "(a ; second comment"
    "#;(third ; comment in comment"
    "comment #;(comment #1=e in"
    ". #;(comment in comment in comment)"
    "comment)) b ; fourth comment"
    "c #| fifth comment #| more"
    "nesting here |# |# d"
    "; sixth and final comment"
    "#1#)"))

  (check-indent
   (code
    "(#|##|# |#|#1"
    "  #||#2"
    "  #|||#3"
    "  #|#||#|#4"
    "  #|| hello ||#5"
    "  #| ; rats |#)")
   (code
    "(#|##|# |#|#1"
    "#||#2"
    "#|||#3"
    "#|#||#|#4"
    "#|| hello ||#5"
    "#| ; rats |#)"))
  )

(indent-mat assemble-tokens ()
  (check-tokens
   '()
   (code ""))

  (for-each
   (lambda (p)
     (match p
       [(,token ,text)
        (check-tokens (list token) text)]))
   '( ;; fold-tokens
     (#\( "(")
     (#\) ")")
     (#\[ "[")
     (#\] "]")
     (#\space " ")
     ("  " "  ")
     (#\tab "\t")
     ("#'" "#'")
     ("#`" "#`")
     ("#," "#,")
     ("#,@" "#,@")
     (#\' "'")
     (#\` "`")
     (#\, ",")
     (",@" ",@")
     ("\"" "\"")
     (#\\ "\\")
     (#\; ";")
     (";;" ";;")
     (";;;" ";;;")
     (#\# "#")
     ("#;" "#;")
     ("#\\" "#\\")
     ("#|" "#|")
     ("|#" "|#")
     ;; assemble-tokens
     ("()" "()")
     ("[]" "[]")
     ("#\\newline" "#\\newline")
     ("\"string\"" "\"string\"")
     ))

  ;; Cases to cover string-tokens
  (check-tokens
   (list "\"incomplete string")
   (code "\"incomplete string"))

  (check-tokens
   (list "\"incomplete special character \\")
   (code "\"incomplete special character \\"))

  (check-tokens
   (list "\"complete special character \\n\"")
   (code "\"complete special character \\n\""))

  (check-tokens
   (list "\"incomplete named character #\\")
   (code "\"incomplete named character #\\"))

  (check-tokens
   (list "\"complete named character #\\newline\"")
   (code "\"complete named character #\\newline\""))

  (check-tokens
   (list "\"string1\"" #\space "\"string2\"")
   (code "\"string1\" \"string2\""))

  (check-tokens
   (list #\(
     "define" #\space "x" #\space
     "\"a string constant; with semicolon and embedded \\\"quotes\\\".\""
     #\))
   (code "(define x \"a string constant; with semicolon and embedded \\\"quotes\\\".\")"))

  (check-tokens*
   (lambda (expected actual)
     (and (equal? expected (map (<token> name) actual))
          (for-all (lambda (x) (has-prop? x 'comment)) actual)
          (for-all (lambda (x) (has-prop? x 'datum-comment)) actual)
          (match actual [(,_ ,str) (has-prop? str 'string)])))
   (list "#;" "\"string1\"")
   (code "#;\"string1\""))

  (check-tokens*
   (lambda (expected actual)
     (and (equal? expected (map (<token> name) actual))
          (match actual
            [(,_ ,str ,_ ,ident)
             (and (has-prop? str 'string)
                  (has-prop? str 'comment)
                  (has-prop? str 'datum-comment)
                  (not (has-prop? ident 'comment)))])))
   (list "#;" "\"string1\"" #\space "abc")
   (code "#;\"string1\" abc"))

  (let ()
    ;; The presence of a line comment after a datum comment should
    ;; have no impact on the properties.
    (check-tokens*
     (lambda (expected actual)
       (and (equal? expected (map (<token> name) actual))
            (for-all (lambda (x) (has-prop? x 'comment)) actual)
            (for-all (lambda (x) (has-prop? x 'datum-comment)) actual)))
     (list "#;" #\space ";;" #\space "comment" #\newline "identifier")
     (code
      "#; ;; comment"
      "identifier"))

    (check-tokens*
     (lambda (expected actual)
       (and (equal? expected (map (<token> name) actual))
            (for-all (lambda (x) (has-prop? x 'comment)) actual)
            (for-all (lambda (x) (has-prop? x 'datum-comment)) actual)))
     (list "#;" #\newline "identifier")
     (code
      "#;"
      "identifier")))
  )

(indent-mat fold-indent ()
  (match-let*
   ([((1 "(let ()" "(let ()")
      (2 "body)" "  body)"))
     (reverse
      (fold-indent
       (code
        "(let ()"
        "body)")
       '()
       (lambda (line old new acc)
         (cons (list line old new) acc))))]
    ;; Strings that cross lines can be tricky.
    [((1 "(define str" "(define str")
      (2 "\"" "  \"")
      (3 "some text" "some text")
      (4 "\")" "\")"))
     (reverse
      (fold-indent
       (code
        "(define str"
        "\""
        "some text"
        "\")")
       '()
       (lambda (line old new acc)
         (cons (list line old new) acc))))])
   'ok))
