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

(library (testing pipe)
  (export
   make-pipe
   )
  (import
   (chezscheme)
   (swish imports)
   )
  (define (await-read)
    (receive
     [close (void)]
     [#(read ,rbv ,rstart ,rn ,reader)
      (receive
       [close (void)]
       [#(write ,wbv ,wstart ,wn ,writer)
        (let ([count (min rn wn)])
          (bytevector-copy! wbv wstart rbv rstart count)
          (let ([msg (cons self count)])
            (send writer msg)
            (send reader msg))
          (await-read))])]))

  (define (make-pipe name)
    (let ([pid (spawn await-read)])
      (define (close) (send pid 'close))
      (values
       (make-custom-binary-input-port name
         (lambda (bv start n)
           (let ([m (monitor pid)])
             (send pid `#(read ,bv ,start ,n ,self))
             (receive
              [(,@pid . ,count)
               (demonitor&flush m)
               count]
              [`(DOWN ,@m ,_ ,_) 0])))
         #f #f close)
       (make-custom-binary-output-port name
         (lambda (bv start n)
           (let ([m (monitor pid)])
             (send pid `#(write ,bv ,start ,n ,self))
             (receive
              [(,@pid . ,count)
               (demonitor&flush m)
               count]
              [`(DOWN ,@m ,_ ,_) 0])))
         #f #f close))))
  )
