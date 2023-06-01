;;; Copyright 2022 Beckman Coulter, Inc.
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

(import
 (checkers)
 (config-params)
 (read)
 (software-info)
 (testing common)
 (testing pipe)
 )

(software-info:install)

(define-syntax code
  (syntax-rules ()
    [(_ str ...)
     (ct:join #\newline str ...)]))

(isolate-mat import-export ()
  (match-let*
   ([,results '()]
    [,text (code
            "(library (lib)"
            "(export (b) (a))"
            "(import (d) (c))"
            "  body)")]
    [,_
     (check-import/export (read-code text)
       (lambda (x type fmt . args)
         (set! results (cons (list x type (apply format fmt args)) results))))]
    [,results (reverse results)]
    [((`(annotation [stripped export]) warning "should sort exports")
      (`(annotation [stripped (b)]) info "incorrectly sorted: (b)")
      (`(annotation [stripped (a)]) info "incorrectly sorted: (a)")
      (`(annotation [stripped import]) warning "should sort imports")
      (`(annotation [stripped (d)]) info "incorrectly sorted: (d)")
      (`(annotation [stripped (c)]) info "incorrectly sorted: (c)"))
     results])
   'ok))

(isolate-mat line-whitespace ()
  (match-let*
   ([,text (code
            "(let ()   "
            "\tbody\r"
            "\tbody   "
            "...)\r")]
    [,results '()]
    [,_
     (check-line-whitespace text #f
       (lambda (x type fmt . args)
         (set! results (cons (list x type (apply format fmt args)) results))))]
    [,results (reverse results)]
    [((2 error "undesirable tab character")
      (3 error "undesirable tab character")
      (2 error "undesirable DOS line ending")
      (4 error "undesirable DOS line ending")
      (1 error "undesirable trailing whitespace")
      (3 error "undesirable trailing whitespace"))
     results]
    [,results '()]
    [,_
     (check-line-whitespace text #t
       (lambda (x type fmt . args)
         (set! results (cons (list x type (apply format fmt args)) results))))]
    [,results (reverse results)]
    [((2 error "undesirable tab character (2 times)")
      (2 error "undesirable DOS line ending (2 times)")
      (1 error "undesirable trailing whitespace (2 times)"))
     results])
   'ok))

(isolate-mat regexp ()
  (match-let*
   ([,check (make-regexp-checker 'info "TEST.*")]
    [,results '()]
    [,_
     (check "file:///tmp/foo.ss" #t #f
       (code
        "(let ()"
        "  body   ; TEST: simple"
        "  body   ; TEST: (printf \"~a\" 12)"
        "  body   ; TEST: ~a"
        "  body)")
       (lambda (x type fmt . args)
         (set! results (cons (list x type (apply format fmt args)) results))))]
    [,results (reverse results)]
    [((#(range 2 12 2 24) info "TEST: simple")
      (#(range 3 12 3 34) info "TEST: (printf \"~a\" 12)")
      (#(range 4 12 4 20) info "TEST: ~a"))
     results])
   'ok))

(isolate-mat optional ()
  (parameterize ([optional-checkers
                  (list
                   (make-regexp-checker 'info "INFO.*")
                   (make-regexp-checker 'warning "WARNING.*")
                   (make-regexp-checker 'error "ERROR.*"))])
    (match-let*
     ([,results '()]
      [,_
       (run-optional-checkers "file:///tmp/foo.ss" #t #f
         (code
          "(let ()"
          "  body   ; INFO: informative"
          "  body   ; WARNING: be careful"
          "  body   ; ERROR: this is broken"
          "  body)")
         (lambda (x type fmt . args)
           (set! results (cons (list x type (apply format fmt args)) results))))]
      [,results (reverse results)]
      [((#(range 2 12 2 29) info "INFO: informative")
        (#(range 3 12 3 31) warning "WARNING: be careful")
        (#(range 4 12 4 33) error "ERROR: this is broken"))
       results])
     'ok)))

(isolate-mat external ()
  (with-tmp-dir
   ;; checker called with mismatched (filename regexp); executable is
   ;; not started
   (let ([fn (path-combine (tmp-dir) "dne")])
     (match-let*
      ([,check (make-external-checker (list fn '(filename "\\.ms$")))]
       [,results '()]
       [,_ (check "file:///tmp/foo.ss" #t #f ""
             (lambda (x type fmt . args)
               (set! results (cons (list x type (apply format fmt args)) results))))]
       [() results])
      'ok))
   ;; checker should report its command-line as a string
   (let ([fn (path-combine (tmp-dir) "echo-args-string")])
     (write-script fn
       '((printf "args: ~s\n" (command-line-arguments))))
     (match-let*
      ([,check (make-external-checker
                (list fn
                  "--option"
                  'filename
                  '(filename "\\.ss$")))]
       [,results '()]
       [,_ (check "file:///tmp/foo.ss" #t #f ""
             (lambda (x type fmt . args)
               (set! results (cons (list x type (apply format fmt args)) results))))]
       [,results (reverse results)]
       ;; strings report as errors
       [((1 error "args: (\"--option\" \"/tmp/foo.ss\" \"/tmp/foo.ss\")")) results])
      (remove-file fn)))
   ;; checker should report its command-line as JSON
   (let ([fn (path-combine (tmp-dir) "echo-args-json")])
     (write-script fn
       '((json:pretty
          (json:make-object
           [type "info"]
           [message (format "args: ~s" (command-line-arguments))]))))
     (match-let*
      ([,check (make-external-checker
                (list fn
                  "--option"
                  'filename
                  '(filename "\\.ss$")))]
       [,results '()]
       [,_ (check "file:///tmp/foo.ss" #t #f ""
             (lambda (x type fmt . args)
               (set! results (cons (list x type (apply format fmt args)) results))))]
       [,results (reverse results)]
       ;; JSON can configure its severity type
       [((#(near ,_ #f #f) info "args: (\"--option\" \"/tmp/foo.ss\" \"/tmp/foo.ss\")")) results])
      (remove-file fn)))
   ;; checker reports multiple messages in JSON
   (let ([fn (path-combine (tmp-dir) "report-multiple-json")]
         [msg1 "A hint on line 2"]
         [msg2 "A warning on line 3"]
         [msg3 "An error on line 10, column 5"])
     (write-script fn
       `((json:pretty
          (json:make-object
           [type "hint"]
           [line 2]
           [message ,msg1]))
         (json:pretty
          (json:make-object
           [type "warning"]
           [line 3]
           [message ,msg2]))
         (json:pretty
          (json:make-object
           ;; type defaults to error
           [column 5]
           [line 10]
           [message ,msg3]))))
     (match-let*
      ([,check (make-external-checker (list fn))]
       [,results '()]
       [,_ (check "file:///tmp/foo.ss" #t #f ""
             (lambda (x type fmt . args)
               (set! results (cons (list x type (apply format fmt args)) results))))]
       [,results (reverse results)]
       [((#(near ,_ 2 #f) hint ,@msg1)
         (#(near ,_ 3 #f) warning ,@msg2)
         (#(near ,_ 10 5) error ,@msg3))
        results])
      (remove-file fn)))
   ;; checker emits diagnostics on stderr, swish-lint sends that to
   ;; the trace-output-port.
   (let ([fn (path-combine (tmp-dir) "trace-output")]
         [diag-msg "this is a diagnostic message"]
         [err-msg "this is a message about the code"])
     (write-script fn
       `((display ,diag-msg (current-error-port))
         (newline (current-error-port))
         (display ,err-msg (current-output-port))
         (newline (current-output-port))
         ))
     (match-let*
      ([,check (make-external-checker (list fn))]
       [,results '()]
       [(,tp ,get-trace)
        (let-values ([(p get) (open-string-output-port)])
          (list p get))]
       [,_
        (parameterize ([trace-output-port tp])
          (check "file:///tmp/foo.ss" #t #f ""
            (lambda (x type fmt . args)
              (set! results (cons (list x type (apply format fmt args)) results)))))]
       [,results (reverse results)]
       [((1 error ,@err-msg)) results]
       ;; stderr is captured by the trace-output-port.
       ;; Use regexp here to ignore possible additional trace output
       ;; from SWISH_LINT_TRACE=yes.
       [#t (and (pregexp-match (pregexp-quote diag-msg) (get-trace)) #t)])
      (remove-file fn)))))
