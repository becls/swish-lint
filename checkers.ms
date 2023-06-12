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
 (read))

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
     (check #f
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
       (run-optional-checkers #f
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
