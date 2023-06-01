;;; Copyright 2024 Beckman Coulter, Inc.
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

(library (testing common)
  (export
   with-tmp-dir
   write-script
   )
  (import
   (chezscheme)
   (swish imports)
   )
  (define-syntax with-tmp-dir
    (syntax-rules ()
      [(_ e0 e1 ...)
       (parameterize ([tmp-dir (path-combine (base-dir) "tmp")])
         e0 e1 ...
         (remove-directory (tmp-dir)))]))

  (define (write-script fn exprs)
    (let ([op (open-file-to-replace (make-directory-path fn))])
      (on-exit (close-port op)
        (fprintf op "#!/usr/bin/env swish\n")
        (for-each (lambda (x) (write x op) (newline op)) exprs)))
    (set-file-mode fn #o777))
  )
