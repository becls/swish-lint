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
 (checkers))

(define-syntax code
  (syntax-rules ()
    [(_ str ...)
     (ct:join #\newline str ...)]))

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
    [((#(range 2 11 2 23) info "TEST: simple")
      (#(range 3 11 3 33) info "TEST: (printf \"~a\" 12)")
      (#(range 4 11 4 19) info "TEST: ~a"))
     results])
   'ok))
