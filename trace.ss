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

(library (trace)
  (export
   trace-expr
   trace-init
   trace-msg
   trace-time
   trace-versions
   )
  (import
   (chezscheme)
   (json)
   (software-info)
   (swish imports)
   )
  (define (trace-init)
    (trace-output-port (console-error-port)))

  (define (trace-expr expr)
    (pretty-print expr (trace-output-port))
    (flush-output-port (trace-output-port))
    expr)

  (define (trace-msg msg)
    (json:write-flat (trace-output-port) msg)
    (newline (trace-output-port))
    (flush-output-port (trace-output-port))
    msg)

  (define-syntax trace-time
    (syntax-rules ()
      [(_ $who e1 e2 ...)
       (let ([who $who]
             [start (erlang:now)])
         (call-with-values
           (lambda () e1 e2 ...)
           (lambda result
             (let ([end (erlang:now)])
               (pretty-print `(time ,who ,(- end start) ms)
                 (trace-output-port))
               (newline (trace-output-port))
               (flush-output-port (trace-output-port))
               (apply values result)))))]))

  (define (trace-versions)
    (fprintf (trace-output-port) "~a\n" (versions->string))
    (flush-output-port (trace-output-port)))
  )
