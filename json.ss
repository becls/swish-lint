;;; Copyright 2020 Beckman Coulter, Inc.
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

#!chezscheme
(library (json)
  (export
   json:get
   json:merge
   json:write-flat
   )
  (import
   (chezscheme)
   (swish imports)
   )

  (define json:get
    (case-lambda
     [(ht path default)
      (json:ref ht path default)]
     [(ht path)
      (let ([val (json:ref ht path #!bwp)])
        (when (eq? val #!bwp)
          (throw `#(json-path-not-found ,ht ,path)))
        val)]))

  (define (json:merge old new)
    (let ([r (hashtable-copy old #t)])
      (let-values ([(keys vals) (hashtable-entries new)])
        (vector-for-each
         (lambda (k v)
           (json:set! r k
             (if (not (json:object? v))
                 v
                 (let ([old (json:ref old k #f)])
                   (if (json:object? old)
                       (json:merge old v)
                       (hashtable-copy v #t))))))
         keys vals))
      r))

  (define (json:write-flat op x)
    (define (symbol<? x y)
      (string<? (symbol->string x) (symbol->string y)))
    (let lp ([x x] [path '()])
      (cond
       [(json:object? x)
        (vector-for-each
         (lambda (k)
           (lp (hashtable-ref x k #f) (cons k path)))
         (vector-sort symbol<? (hashtable-keys x)))]
       [(list? x)
        (do ([ls x (cdr ls)] [i 0 (+ i 1)]) ((null? ls))
          (lp (car ls) (cons i path)))]
       [else
        (fprintf op "~s => " (reverse path))
        (json:write op x #f)
        (newline op)])))
  )
