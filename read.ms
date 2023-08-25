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

(import
 (read)
 (software-info))

(software-info:install)

(isolate-mat definitions ()
  (define (extract-name str)
    (let ([result #f])
      (walk-defns-re str 'no-table
        (lambda (table name source)
          (set! result name)))
      result))

  (match-let*
   (["f" (extract-name "(define (f x) x)")]
    ["f" (extract-name "(define f (lambda (x) x))")]
    ["f" (extract-name "(define-syntax f (syntax-rules () ...))")]
    ["f" (extract-name "(define-syntax (f x) (syntax-case x () ...))")]
    ["opt" (extract-name "(define-options opt ...)")]
    [#f (extract-name "(define- f x)")]
    [#f (extract-name "(definegarbage f x)")]
    [#f (extract-name "(define-inline 2 x ...)")]
    ["x" (extract-name "(set! x 5)")]
    ["x" (extract-name "(set-who! x 'me)")]
    [#f (extract-name "(setgarbage! x 'me)")]
    [#f (extract-name "(set-! x 'me)")]
    [#f (extract-name "(set! (x) 5)")]
    [#f (extract-name "(set-who! (x) 'me)")]
    [#f (extract-name "(set!bad x 5)")]
    ["f" (extract-name "(trace-define (f x) x)")]
    ["f" (extract-name "(trace-define-syntax (f x) x)")]
    [#f (extract-name "(trace-trace-define (f x) x)")]
    [#f (extract-name "(tracedefine (f x) x)")]
    ["f" (extract-name "(define-who f x)")]
    ["f" (extract-name "(meta define (f x) x)")]
    ["f" (extract-name "(meta define f (lambda (x) x)")]
    ["f" (extract-name "(meta define-record f (x))")]
    [#f (extract-name "(meta meta define (f x) x)")]
    [#f (extract-name "(meta set! x y)")]
    [#f (extract-name "(field-set! x y)")]
    [#f (extract-name "(set-field! x y)")]
    [#f (extract-name "(set-top-level-value! 'x y)")]
    )
   ;; Check that all (scheme) exports except set! return #f
   (for-each
    (lambda (x)
      (when (and (not (eq? x 'set!))
                 (pregexp-match "set.*!" (symbol->string x)))
        (let ([expr (format "(~a x y)" x)])
          (match (extract-name expr)
            [#f 'ok]
            [,result
             (printf "~a: ~a\n" result expr)
             (throw `#(failed ,result ,expr))]))))
    (library-exports '(scheme)))
   'ok))
