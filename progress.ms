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

(import (progress))

(isolate-mat progress ()
  (define (get-one) (receive (after 200 #f) [,msg msg]))
  (define prev-ts #f)
  (define (check title kind percentage message min-delta)
    (match-let*
     ([#(send ,timestamp ,msg) (get-one)]
      [,@title (json:ref msg 'title #f)]
      [,@kind (json:ref msg 'kind #f)]
      [,@percentage (json:ref msg 'percentage #f)]
      [,@message (json:ref msg 'message #f)]
      [,delta-ts (if prev-ts
                     (- timestamp prev-ts)
                     0)])
     (when min-delta
       (assert (<= min-delta delta-ts (+ min-delta 50))))
     (set! prev-ts timestamp)
     msg))

  (match-let*
   ([,title "My Progress"]
    [,me self]
    [#(ok ,pid)
     (progress:start title
       (lambda (done total) (format "~a/~a cases" done total))
       (lambda (msg) (send me `#(send ,(erlang:now) ,msg)))
       (lambda (expr) (void)))]
    [#f (get-one)]
    [ok (progress:inc-total pid)]
    [,msg (check title "begin" 0 #f 0)]
    [ok (progress:inc-total pid)]
    [ok (progress:inc-total pid)]
    [,msg (check #f "report" 0 "0/3 cases" 100)]
    [ok (progress:inc-done pid)]
    [,msg (check #f "report" 33 "1/3 cases" 100)]
    [ok (progress:inc-done pid)]
    [,msg (check #f "report" 66 "2/3 cases" 100)]
    [ok (progress:inc-total pid)]
    [,msg (check #f "report" 66 "2/4 cases" 100)] ; percentage does not go down
    [ok (progress:inc-done pid)]
    [,msg (check #f "report" 75 "3/4 cases" 100)]
    [ok (progress:inc-done pid)]
    [ok (progress:inc-total pid)]
    [ok (progress:inc-total pid)]
    [ok (progress:inc-done pid)]
    [,msg (check #f "report" 83 "5/6 cases" 100)] ; only one update
    [ok (receive (after 150 'ok))]
    [#f (get-one)]                   ; no progress yields no messages.
    [ok (progress:inc-done pid)]
    [,msg (check #f "report" 100 "6/6 cases" #f)]
    [,msg (check #f "end" #f #f 0)]
    [ok (progress:inc-done pid)]        ; No messages after complete
    [#f (get-one)]
    ;; Coverage: Make sure we don't divide by zero
    [,_ (set! prev-ts #f)]
    [#(ok ,pid)
     (progress:start title
       (lambda (done total) (format "~a/~a cases" done total))
       (lambda (msg) (send me `#(send ,(erlang:now) ,msg)))
       (lambda (expr) (void)))]
    [#f (get-one)]
    [ok (progress:inc-done pid)]
    [ok (progress:inc-done pid)]
    [ok (progress:inc-done pid)]
    [,msg (check title "begin" 0 #f 0)]
    [,msg (check #f "report" 0 "3/0 cases" 100)]
    [,msg (check #f "end" #f #f 0)])
   'ok))
