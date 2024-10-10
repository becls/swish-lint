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

(library (software-info)
  (export
   software-info:install
   versions->string
   )
  (import
   (chezscheme)
   (swish imports)
   )
  (define (software-info:install)
    (define key 'swish-lint)
    (software-product-name key "Swish Lint")
    (software-revision key
      (include-line "git.revision"
        (lambda (fn)
          (warningf 'software-info.ss "file ~s not found at compile time" fn)
          #f)))
    (software-version key "1.3.0"))

  (define (output-version op key)
    (fprintf op "~11@a~@[ ~6@a~]~@[ (~a)~]\n"
      (software-product-name key)
      (software-version key)
      (software-revision key)))

  (define (versions->string)
    (let ([op (open-output-string)])
      (output-version op 'swish-lint)
      (output-version op 'swish)
      (output-version op 'chezscheme)
      (get-output-string op)))
  )
